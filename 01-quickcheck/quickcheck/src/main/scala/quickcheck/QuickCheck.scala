package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h: H): List[A] =
    if( isEmpty(h) ) Nil else findMin(h) :: toList(deleteMin(h))

  property("myTest1") = forAll { (h1:H, h2:H) =>
    (toList(h1):::toList(h2)).sorted == toList(meld(h1, h2)).sorted
  }

  property("myTest2") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }




  property("empty1") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("empty2") = forAll { (a: Int, b:Int) =>
    isEmpty(deleteMin(deleteMin(insert(b, insert(a, empty)))))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("gen2") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, deleteMin(h)))==m
  }

  property("gen3") = forAll { (h: H) =>
    val m = if (isEmpty(h)) Int.MinValue else findMin(h)
    val h2 = deleteMin(h)
    val m2 = if (isEmpty(h2)) m else findMin(h2)
    m <= m2
  }

  property("gen4") = forAll { (a: Int, h:H) =>
    val m = if (isEmpty(h)) a else findMin(h)
    val h2 = insert(a, insert(m, deleteMin(h)))
    val m2 = findMin(h2)
    if( m<a ) m2==m else m2==a
  }

  property("gen5") = forAll { (h:H) =>
    if(isEmpty(h)) true else minToAll(findMin(h), deleteMin(h))
  }

  property("insert1") =  {
    val h = insert(1, insert(2, empty))
    isMin(1, h) &&
    isMin(2, deleteMin(h)) &&
    isEmpty(deleteMin(deleteMin(h)))
  }

  property("insert2") =  {
    val h = insert(2, insert(1, empty))
    isMin(1, h) &&
    isMin(2, deleteMin(h)) &&
    isEmpty(deleteMin(deleteMin(h)))
  }

  property("insert3") =  {
    val h = insert(1, insert(2, insert(3, insert(4, empty))))
    isMin(1, h) &&
    isMin(2, deleteMin(h)) &&
    isMin(3, deleteMin(deleteMin(h))) &&
    isMin(4, deleteMin(deleteMin(deleteMin(h)))) &&
    isEmpty(deleteMin(deleteMin(deleteMin(deleteMin(h)))))
  }

  property("insert4") = forAll { (a:Int, b:Int) =>
    findMin(insert(a, insert(b, empty))) == (if (a<b) a else b)
  }

  property("meld1") = forAll { (h: H) =>
    isEmpty(h) || findMin(h)==findMin(meld(h, insert(findMin(h), deleteMin(h))))
  }

  property("meld2") = forAll { (a:Int, h1: H, h2:H) =>
    findMin(insert(a, meld(h1,h2)))==findMin(insert(a, h1)) || findMin(insert(a, meld(h1,h2)))==findMin(insert(a, h2))
  }

  property("list1") = forAll { h:H =>
    isSorted(toList(h))
  }

  property("list2") = forAll { h:H =>
    if (isEmpty(h)) true else isSorted(toList(deleteMin(h)))
  }

  property("melt1") = forAll { (h1:H, h2:H) =>
    isSorted(toList(meld(h1, h2)))
  }

  property("xxx1") =
    toList(deleteMin(fromList(List(1,2,3,4)))).equals(List(2,3,4))

  property("xxx2") =
    toList(deleteMin(fromList(List(4,3,2,1)))).equals(List(2,3,4))

  property("xxx3") =
    toList(deleteMin(fromList(List(7,3,5,2,6,4,8,1)))).equals(List(2,3,4,5,6,7,8))

  property("xxx4") =
    toList(deleteMin(fromList(List(-4,-3,-2,-1, 0)))).equals(List(-3,-2,-1, 0))

  def isMin(a:Int, h:H): Boolean = !isEmpty(h) && findMin(h)==a

  def minToAll(m:Int, h:H): Boolean = {
    if(isEmpty(h)) true else (m<=findMin(h) & minToAll(m, deleteMin(h)))
  }

  def fromList(l: List[A]): H = l match {
    case Nil => empty
    case h::ts => insert(h, fromList(ts))
  }

  def isSorted(l: List[A]): Boolean = l match {
    case Nil => true
    case h::ts => ts.forall( h <= _ ) && isSorted(ts)
  }

}
