package functional

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaz.Scalaz._

/*
* User: catayl2
* Date: 7/2/13
* Time: 7:59 PM
*/

class HelloScalaZTest extends FlatSpec with ShouldMatchers {
  import Peep._
  it should ("act like a proper monoid") in {
    val lst = List(2,3,4).∗ (List(7, _))
    assert (lst == List(7,2,7,3,7,4))

    val mAbc = (9, "abc") |+| (10, "def")
    mAbc assert_===  (19, "abcdef")

    val mXyz = (11, "uvw") |+| (12, "xyz")
    mXyz assert_===  (23, "uvwxyz")

    (mAbc |+| mXyz) assert_=== (42, "abcdefuvwxyz")

    val p1 = Peep( "abc", 9 )
    val p2 = Peep( "def", 21 )

    (p1 |+| p2) assert_=== Peep("abcdef",30)
    (p1 |+| p2) should equal(Peep("abcdef",30))

    val m = Map(3 -> Set(CaseKey(1), CaseKey(2), CaseKey(3))) |+| Map(3 -> Set(CaseKey(2), CaseKey(4)), 4 -> Set(CaseKey(9), CaseKey(11)))
    assert( m.sameElements( Map(3 -> Set(CaseKey(1), CaseKey(2), CaseKey(3), CaseKey(4)), 4 -> Set(CaseKey(9), CaseKey(11))) ) )

    val mOpt:Map[Int, Option[Int]] = Map(3 -> None, 4 -> Some(9)) |+| Map(3 -> Some(8), 4 -> Some(18))
    assert ( mOpt.sameElements(Map(3 -> Some(8), 4 -> Some(27))) )

    val peepStringTuple = (p1, "abc") |+| (p2, "def")
    peepStringTuple assert_=== (Peep("abcdef",30), "abcdef")

    Peeper(p1, true) |+| Peeper(p1 |+| p2, false) assert_=== (Peeper(Peep("abcabcdef",39), true |+| false))
  }

  it should("Show off Scalaz like a champ") in {

    /** Monoid Combinators **/
    val composedMonoids = some(some((1, "OH", 1 + (_:Int)))) |+| some(some((4, "HAI", 2 * (_:Int))))
    assert (composedMonoids.get.get._1 == 5)
    assert (composedMonoids.get.get._2 == "OHHAI")
    assert (composedMonoids.get.get._3(3) == 10)
    assert (List(1,2,3,4).sumr == 10)
    assert(
      List(some(Peep("matt", 15)), some(Peep(" ", 0)), some(Peep("carpenter", 8))).sumr ==
        some(Peep("matt carpenter", 23))
    )

    List(some(Peep("matt", 15)), none[Peep], some(Peep("carpenter", 8))).sumr.getOrElse(peep.zero) assert_===(Peep("mattcarpenter", 23))

    List(none[Peep], none[Peep]).sumr.getOrElse(peep.zero) assert_=== peep.zero

    /** Monad Combinators **/
    List(some(Peep("matt", 15)), some(Peep(" ", 0)), some(Peep("carpenter", 8))).sequence should equal (some(List(Peep("matt", 15), Peep(" ", 0), Peep("carpenter", 8))))

    List(some(Peep("matt", 15)), none[Peep], some(Peep("carpenter", 8))).sequence should equal (none[Peep])

    List(some(Peep("matt", 15)), some(Peep(" ", 0)), some(Peep("carpenter", 8)))
      .traverse(p => some(Peeper(p.get, p.get.age > 7))).get.sumr should equal ( Peeper(Peep("matt carpenter", 23), true) )

     // true if any Peep age > 7
      List(some(Peep("matt", 15)), some(Peep(" ", 0)), some(Peep("carpenter", 8)))
        .traverse(p => some(Peeper(p.get, p.get.age > 15))).get.sumr should equal ( Peeper(Peep("matt carpenter", 23), false) )

     // Note: false since no Peep age > 15

    /** Applicative Functors **/
    case class Dude(name:String, age:Int, iQ:Double)

    val applicativeTest = (some("Einstein") |@| some(75) |@| some(70.0d)){Dude.apply _}

    assert( applicativeTest ==
      some(Dude("Einstein", 75, 70.0d))
    )
    assert ( (some(9) |@| some(11) |@| some(10)){_ + _ + _} == some(30) )
    assert ( (some(9) |@| some(11) |@| none[Int]){_ + _ + _} == none )
  }
}