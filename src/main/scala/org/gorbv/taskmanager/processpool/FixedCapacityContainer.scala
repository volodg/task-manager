package org.gorbv.taskmanager.processpool

import org.gorbv.taskmanager.containers.{DoubleLinkedList, Node}
import org.gorbv.taskmanager.process.Process.Priority

import scala.collection.mutable

sealed trait ContainerStrategy[Key, Value]
case class DefaultStrategy[Key, Value]() extends ContainerStrategy[Key, Value]
case class FifoStrategy[Key, Value](onDelete: (Key, Value) => Unit) extends ContainerStrategy[Key, Value]
case class PriorityStrategy[Key, Value](onDelete: (Key, Value) => Unit, priority: Priority) extends ContainerStrategy[Key, Value]

/*
  Implements "Default,Fifo&Priority" storing strategies.
  Caution: Don't use containers methods in callbacks like valueFactory, keyFactory & onDelete
  Re-usage: after deletion, "value" object is moved to "pendings" and will be reused for next "put" methods calls
 */
final case class FixedCapacityContainer[Key, Value](maxSize: Int) {

  private val pendingValues: mutable.Queue[Value] = mutable.Queue[Value]()
  private def storePending(value: Value): Unit = pendingValues.enqueue(value)
  private def getPendingValue(key: Key, valueFactory: Key => Value): Value =
    pendingValues.dequeueFirst(_ => true).getOrElse(valueFactory(key))

  private case class KeyValue(key: Key, value: Value, priority: Option[Priority] = None)
  private type KVNode = Node[KeyValue]
  //Store pointers to nodes to have efficient O(1) remove methods
  private case class ValueWithNodes(keyValue: KeyValue, fifo: KVNode, priority: Option[KVNode] = None)

  private val valuesByKey = scala.collection.mutable.HashMap[Key, ValueWithNodes]()
  private val doubleLinkedList = DoubleLinkedList[KeyValue]()
  private val valuesByPriority = scala.collection.mutable.SortedMap[Priority, DoubleLinkedList[KeyValue]]()

  def size: Int = valuesByKey.size

  def put(
           strategy: ContainerStrategy[Key, Value],
           keyFactory: () => Key,
           valueFactory: Key => Value
         ): Option[(Key, Value)] =
    Option.when(hasFreeSpace || tryReleaseSpace(strategy)) {
      val priority = strategy match {
        case PriorityStrategy(_, priority) => Some(priority)
        case DefaultStrategy() => None
        case FifoStrategy(_) => None
      }

      val key = keyFactory()
      val value = getPendingValue(key, valueFactory)

      val fifoNode = doubleLinkedList.enqueue(KeyValue(key, value))
      val priorityNode = priority.map(priority => addPriorityNode(key, value, priority))

      valuesByKey.put(key, ValueWithNodes(fifoNode.value, fifoNode, priorityNode))

      (key, value)
    }

  private def addPriorityNode(key: Key, value: Value, priority: Priority): Node[KeyValue] = {
    val jobs = valuesByPriority.getOrElse(priority, DoubleLinkedList[KeyValue]())
    val result = jobs.enqueue(KeyValue(key, value, Some(priority)))
    valuesByPriority.put(priority, jobs)
    result
  }

  private def hasFreeSpace: Boolean =
    valuesByKey.size < maxSize

  def tryReleaseSpace(strategy: ContainerStrategy[Key, Value]): Boolean = {
    lazy val needsReleaseSpace = valuesByKey.nonEmpty && size == maxSize
    strategy match {
      case DefaultStrategy() => false
      case FifoStrategy(onDelete) =>
        if (needsReleaseSpace) {
          val element = doubleLinkedList.dequeue()

          remove(element.key, priorities = true, fifo = false)
          onDelete(element.key, element.value)
        }
        true
      case PriorityStrategy(onDelete, priority) =>
        if (needsReleaseSpace) {
          val lowestKey = valuesByPriority.firstKey
          if (lowestKey < priority) {
            val jobs = valuesByPriority(lowestKey)
            val element = jobs.dequeue()

            if (jobs.size == 0)
              valuesByPriority.remove(lowestKey)

            remove(element.key, priorities = false, fifo = true)
            onDelete(element.key, element.value)
            true
          } else
            false
        } else true
    }
  }

  def valueForKey(key: Key): Option[Value] =
    valuesByKey.get(key).map(_.keyValue.value)

  def remove(key: Key): Unit =
    remove(key, priorities = true, fifo = true)

  private def remove(key: Key, priorities: Boolean, fifo: Boolean): Unit =
    valuesByKey.get(key).foreach { nodes =>
      storePending(nodes.keyValue.value)
      valuesByKey.remove(key)
      if (fifo)
        doubleLinkedList.remove(nodes.fifo)
      if (priorities)
        nodes.priority.foreach(priorityNode => {
          val key = priorityNode.value.priority.get //must have priority field
          val jobs = valuesByPriority(key) //value must exist for given key
          jobs.remove(priorityNode)
          if (jobs.size == 0)
            valuesByPriority.remove(key)
        })
    }

  private def elementsForPriority(priority: Priority): Iterable[KeyValue] =
    valuesByPriority.get(priority).map(_.allValues).getOrElse(Iterable.empty[KeyValue])

  def valuesForPriority(priority: Priority): Iterable[Value] =
    elementsForPriority(priority).map(_.value)

  def removeValuesForPriority(priority: Priority): Unit =  {
    val values = elementsForPriority(priority)
    valuesByPriority.remove(priority)
    values.map(_.key).foreach(x => remove(x, priorities = false, fifo = true))
  }

  def allValues: Iterable[Value] =
    doubleLinkedList.allValues.map(_.value)

  def prioritySortedJobs: Iterable[Value] =
    valuesByPriority.flatMap(value => value._2.allValues.map(_.value))

  def clear(): Unit = {
    valuesByKey.values.map(_.keyValue.value).foreach(storePending)
    valuesByKey.clear()
    valuesByPriority.clear()
    doubleLinkedList.clear()
  }

}
