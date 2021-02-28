package org.gorbv.taskmanager.containers

import org.specs2.mutable.Specification

final class DoubleLinkedListSpec extends Specification {

  "DoubleLinkedList" should {

    "beEmpty" in {
      val list = DoubleLinkedList[Int]()
      list.size must_== 0
      list.allValues.toSeq must_== Seq.empty[Int]
    }

    "has one element" in {
      val list = DoubleLinkedList[Int]()
      list.enqueue(1)
      list.size must_== 1

      list.allValues.toSeq === Seq(1)
      list.first.map(_.value) must beSome(1)
      list.last.map(_.value)  must beSome(1)
    }

    "has two element" in {
      val list = DoubleLinkedList[Int]()
      list.enqueue(2)
      list.enqueue(1)
      list.size must_== 2

      list.allValues.toSeq must_== Seq(2, 1)
      list.first.map(_.value) must beSome(1)
      list.last.map(_.value)  must beSome(2)
    }

    "has three element & dequeue" in {
      val list = DoubleLinkedList[Int]()
      list.enqueue(3)
      list.enqueue(2)
      list.enqueue(1)
      list.size must_== 3

      list.allValues.toSeq must_== Seq(3, 2, 1)
      list.first.map(_.value) must beSome(1)
      list.last.map(_.value)  must beSome(3)

      list.dequeue() must_== 3
      list.size must_== 2
      list.allValues.toSeq must_== Seq(2, 1)
      list.first.map(_.value) must beSome(1)
      list.last.map(_.value)  must beSome(2)

      list.dequeue() must_== 2
      list.size must_== 1
      list.allValues.toSeq must_== Seq(1)
      list.first.map(_.value) must beSome(1)

      list.dequeue() must_== 1
      list.size must_== 0
      list.allValues.toSeq must_== Seq.empty
      list.first.map(_.value) must beNone
    }

    "remove nodes" in {
      val list = DoubleLinkedList[Int]()
      val lastNode = list.enqueue(3)
      val middleNode = list.enqueue(2)
      val firstNode = list.enqueue(1)

      list.remove(middleNode)
      list.size must_== 2
      list.allValues.toSeq must_== Seq(3, 1)
      list.first.map(_.value) must beSome(firstNode.value)
      list.last.map(_.value) must beSome(lastNode.value)

      list.remove(lastNode)
      list.size must_== 1
      list.allValues.toSeq must_== Seq(1)
      list.first.map(_.value) must beSome(firstNode.value)
      list.last.map(_.value) must beSome(firstNode.value)

      list.remove(firstNode)
      list.size must_== 0
      list.allValues.toSeq must_== Seq.empty
      list.first must beNone
      list.last must beNone
    }


  }

}
