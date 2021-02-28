package org.gorbv.taskmanager.containers

case class Node[Value](
  value: Value,
  private[containers] var previous: Option[Node[Value]] = None,
  private[containers] var next: Option[Node[Value]] = None
)

/*
  Was developer to have O(1) remove element method
 */
case class DoubleLinkedList[Value]() {

  private[containers] var first: Option[Node[Value]] = None
  private[containers] var last: Option[Node[Value]] = None
  private var cachedSize: Int = 0

  def size: Int = cachedSize

  def dequeue(): Value = {
    val element = last.get
    last = last.flatMap(_.previous)
    last.foreach(last => last.next = None)
    if (last.isEmpty)
      first = None

    cachedSize -= 1

    element.value
  }

  def enqueue(value: Value): Node[Value] = {
    val newNode = Node(value, None, first)
    first.foreach(_.previous = Some(newNode))
    first = Some(newNode)

    if (last.isEmpty)
      last = first

    cachedSize += 1

    newNode
  }

  def remove(node: Node[Value]): Unit = {
    val isFirst = first.exists(_.eq(node))
    val isLast = last.exists(_.eq(node))

    if (isFirst || isLast || (node.previous.isDefined && node.next.isDefined))
      cachedSize -= 1

    if (isFirst)
      first = node.next

    if (isLast)
      last = node.previous

    node.previous.foreach(previous => previous.next = node.next)
    node.next.foreach(next => next.previous = node.previous)
  }

  def allValues: Iterable[Value] = new Iterable[Value] {
    override def iterator: Iterator[Value] = new Iterator[Value] {
      private var current: Option[Node[Value]] = DoubleLinkedList.this.last

      override def hasNext: Boolean = current.nonEmpty

      override def next(): Value = {
        val result = current
        current = result.flatMap(_.previous)
        result.map(_.value).get
      }
    }
  }

  def clear(): Unit = {
    var head = first
    while (head.isDefined) {
      val next = head.get.next
      head.get.next = None
      head.get.previous = None
      head = next
    }
    first = None
    last = None
    cachedSize = 0
  }

}
