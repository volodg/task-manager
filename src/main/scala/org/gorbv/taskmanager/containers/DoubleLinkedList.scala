package org.gorbv.taskmanager.containers

case class Node[Value] private (
  value: Value,
  private[containers] var previous: Option[Node[Value]],
  private[containers] var next: Option[Node[Value]],
  private[containers] val parent: Option[DoubleLinkedList[Value]]
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
    val newNode = Node(value, None, first, Some(this))
    first.foreach(_.previous = Some(newNode))
    first = Some(newNode)

    if (last.isEmpty)
      last = first

    cachedSize += 1

    newNode
  }

  def remove(node: Node[Value]): Unit =
    if (node.parent.exists(_.eq(this))) {
      cachedSize -= 1

      if (first.exists(_.eq(node)))
        first = node.next

      if (last.exists(_.eq(node)))
        last = node.previous

      node.previous.foreach(previous => previous.next = node.next)
      node.next.foreach(next => next.previous = node.previous)
    } else sys.error("Given does not belong to this container")

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
