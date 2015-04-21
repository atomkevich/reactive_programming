package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min") = forAll { (a: Int, b:Int) =>
    val heap1 = insert(a, insert(b, empty))
    findMin(heap1) == ord.min(a, b)
  }

  property("deleteMin") = forAll { (n: A) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("deleteMin2") = forAll { (a: A, b: A, c: A) =>
    val heap1 = insert(c, insert(a, insert(b, empty)))
    findMin(deleteMin(deleteMin(heap1))) == max(c, max(a, b))
  }


  property("meld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    m <- oneOf(Gen.const(empty), genHeap)
  } yield insert(v, m)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
