package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("sorted insert should succeed") = 
    forAll { (l: List[A]) =>
      val heap = l.foldRight(empty)(insert)
      asList(heap) == l.sorted
    }
  
  property("asList_should_be_sorted") = forAll { (h: H) =>
    asList(h).sorted == asList(h) 
  }

  def asList(h: H): List[A] = // the sorted list resulting from extracting all elements of h
    if (isEmpty(h)) List() else findMin(h) :: asList(deleteMin(h))
  
  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    h <- oneOf(empty, genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
