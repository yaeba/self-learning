package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // add minimal element, and findMin should return the element added
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // minimum of singleton heap should be the only element
  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // add two elements, findMin should return smallest of the two elements
  property("min of 2") = forAll{ (a: Int, b: Int) =>
    val smallest = a min b
    val h = insert(a, insert(b, empty))
    findMin(h) == smallest
  }

  // deleteMin from singleton heap should return empty heap
  property("deleteMin singleton") = forAll{ (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  // continually deleteMin yield sorted sequence
  property("keep deleteMin and get sorted") = forAll{ (h: H) =>
    def deleteMinR(heap: H, acc: List[Int]): List[Int] =
      if (isEmpty(heap)) acc.reverse
      else deleteMinR(deleteMin(heap), findMin(heap) :: acc)

    val l = deleteMinR(h, List())
    l == l.sorted
  }

  // findMin on melding of two heaps should return minimum of one or the other
  property("findMin two melded and return minimum of oneof") = forAll{ (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val h = meld(h1, h2)
      val minOfTwo = findMin(h1) min findMin(h2)
      findMin(h) == minOfTwo
    }
  }

  // meld empty with any heap should not change anything
  property("meld with empty and get back itself") = forAll{ (h: H) =>
    meld(h, empty) == h
  }

  // insert min, deletemin should get back itself
  property("insert min and deleteMin and get back itself") = forAll{ (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    deleteMin(insert(m, h)) == h
  }

  // insert values a, a-1 and a+1, findMin after deleteMin should return a
  property("findMin after deleteMin and get back a") = forAll{ (a: Int) =>
    (a > Int.MinValue && a < Int.MaxValue) ==> {
      val h = insert(a, insert(a - 1, insert(a + 1, empty)))
      findMin(deleteMin(h)) == a
    }
  }

}
