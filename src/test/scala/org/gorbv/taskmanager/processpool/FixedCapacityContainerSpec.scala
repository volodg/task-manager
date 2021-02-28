package org.gorbv.taskmanager.processpool

import org.specs2.mutable.Specification

final class FixedCapacityContainerSpec extends Specification {

  "FixedCapacityContainer" should {

    "be empty" in {
      val container = FixedCapacityContainer[Int, String](1)
      container.size must_== 0
      container.allValues.toSeq must_== Seq.empty
    }

    "one element" in {
      val container = FixedCapacityContainer[Int, String](1)
      var factoryCallsCount = 0
      val factory = (key: Int) => {
        factoryCallsCount += 1
        key.toString
      }
      val result = container.put(PriorityStrategy((_, _) => sys.error("should not be used"), 0), () => 0, factory)

      container.size must_== 1
      result must beSome((0, "0"))
      container.allValues.toSeq must_== Seq("0")
      factoryCallsCount must_== 1
      container.valuesForPriority(0).toSeq must_== Seq("0")
      container.valuesForPriority(1).toSeq must_== Seq.empty

      container.removeValuesForPriority(0)
      container.size must_== 0
      container.allValues.toSeq must_== Seq.empty
      container.valuesForPriority(0).toSeq must_== Seq.empty

      val result1 = container.put(PriorityStrategy((_, _) => sys.error("should not be used"), 0), () => 1, factory)
      container.size must_== 1
      result1 must beSome((1, "0"))
      container.allValues.toSeq must_== Seq("0")
      factoryCallsCount must_== 1
      container.valuesForPriority(0).toSeq must_== Seq("0")
      container.valuesForPriority(1).toSeq must_== Seq.empty
    }

    "not be able to add element for DefaultStrategy" in {
      val container = FixedCapacityContainer[Int, String](1)
      var factoryCallsCount = 0
      val factory = (key: Int) => {
        factoryCallsCount += 1
        key.toString
      }
      val result0 = container.put(DefaultStrategy(), () => 0, factory)

      result0 must beSome((0, "0"))
      container.size must_== 1
      container.allValues.toSeq must_== Seq("0")
      factoryCallsCount must_== 1

      val result1 = container.put(DefaultStrategy(), () => 1, factory)
      container.size must_== 1
      result1 must beNone
      factoryCallsCount must_== 1
    }

    "reuse created element after deletion" in {
      val container = FixedCapacityContainer[Int, String](1)
      var factoryCallsCount = 0
      val factory = (key: Int) => {
        factoryCallsCount += 1
        key.toString
      }
      val result0 = container.put(DefaultStrategy(), () => 0, factory)

      result0 must beSome((0, "0"))
      container.size must_== 1
      container.allValues.toSeq must_== Seq("0")
      factoryCallsCount must_== 1

      container.remove(0)
      container.size must_== 0
      container.allValues.toSeq must_== Seq.empty
      factoryCallsCount must_== 1

      val result1 = container.put(DefaultStrategy(), () => 1, factory)
      result1 must beSome((1, "0"))
      container.size must_== 1
      container.allValues.toSeq must_== Seq("0")
      factoryCallsCount must_== 1
    }

    "fifo" in {
      val container = FixedCapacityContainer[Int, String](2)
      var factoryCallsCount = 0
      val factory = (key: Int) => {
        factoryCallsCount += 1
        key.toString
      }
      val result0 = container.put(FifoStrategy((_, _) => sys.error("should not be used")), () => 0, factory)

      result0 must beSome((0, "0"))
      container.size must_== 1
      container.allValues.toSeq must_== Seq("0")
      factoryCallsCount must_== 1

      val result1 = container.put(FifoStrategy((_, _) => sys.error("should not be used")), () => 1, factory)

      result1 must beSome((1, "1"))
      container.size must_== 2
      container.allValues.toSeq must_== Seq("0", "1")
      factoryCallsCount must_== 2

      var onDeleteCallsCount = 0
      var deletedValue = Option.empty[(Int, String)]
      val onDelete = (key: Int, value: String) => {
        onDeleteCallsCount += 1
        deletedValue = Some((key, value))
      }

      val result3 = container.put(FifoStrategy(onDelete), () => 2, factory)

      result3 must beSome((2, "0"))
      container.size must_== 2
      container.allValues.toSeq must_== Seq("1", "0")
      factoryCallsCount must_== 2
      onDeleteCallsCount must_== 1
      deletedValue must beSome((0, "0"))

      val result4 = container.put(FifoStrategy(onDelete), () => 3, factory)

      result4 must beSome((3, "1"))
      container.allValues.toSeq must_== Seq("0", "1")
      container.size must_== 2
      factoryCallsCount must_== 2
      onDeleteCallsCount must_== 2
      deletedValue must beSome((1, "1"))
    }

    "priority" in {
      val container = FixedCapacityContainer[Int, String](2)
      var factoryCallsCount = 0
      val factory = (key: Int) => {
        factoryCallsCount += 1
        key.toString
      }
      val result0 = container.put(PriorityStrategy((_, _) => sys.error("should not be used"), 0), () => 0, factory)

      result0 must beSome((0, "0"))
      container.size must_== 1
      container.allValues.toSeq must_== Seq("0")
      factoryCallsCount must_== 1

      val result1 = container.put(PriorityStrategy((_, _) => sys.error("should not be used"), 1), () => 1, factory)

      result1 must beSome((1, "1"))
      container.size must_== 2
      container.allValues.toSet must_== Set("0", "1")
      factoryCallsCount must_== 2

      var onDeleteCallsCount = 0
      var deletedValue = Option.empty[(Int, String)]
      val onDelete = (key: Int, value: String) => {
        onDeleteCallsCount += 1
        deletedValue = Some((key, value))
      }

      val result3 = container.put(PriorityStrategy(onDelete, 2), () => 2, factory)

      result3 must beSome((2, "0"))
      container.size must_== 2
      container.allValues.toSet must_== Set("0", "1")
      factoryCallsCount must_== 2
      onDeleteCallsCount must_== 1
      deletedValue must beSome((0, "0"))

      val result4 = container.put(PriorityStrategy(onDelete, 2), () => 3, factory)

      result4 must beSome((3, "1"))
      container.allValues.toSet must_== Set("0", "1")
      container.size must_== 2
      factoryCallsCount must_== 2
      onDeleteCallsCount must_== 2
      deletedValue must beSome((1, "1"))

      container.valuesForPriority(2).toSet must_== Set("0", "1")
      container.valuesForPriority(1).toSeq must_== Seq.empty

      val result5 = container.put(PriorityStrategy(onDelete, 3), () => 4, factory)

      result5 must beSome((4, "0"))
      container.allValues.toSet must_== Set("0", "1")
      container.size must_== 2
      factoryCallsCount must_== 2
      onDeleteCallsCount must_== 3
      deletedValue must beSome((2, "0"))

      container.valuesForPriority(2).toSet must_== Set("1")
      container.valuesForPriority(3).toSet must_== Set("0")
    }

    "prioritySortedJobs & allValues" in {
      val container = FixedCapacityContainer[Int, String](200)
      val factory = (key: Int) => key.toString

      def addEl(index: Int, priority: Int) =
        container.put(PriorityStrategy((_, _) => sys.error("onDelete should not be called"), priority), () => index, factory)

      addEl(0, 2)
      addEl(1, 2)
      addEl(2, 1)
      addEl(3, 1)
      addEl(4, 0)
      addEl(5, 0)

      container.prioritySortedJobs must_== Seq("4", "5", "2", "3", "0", "1")
      container.allValues must_== Seq("0", "1", "2", "3", "4", "5")
    }

  }

}
