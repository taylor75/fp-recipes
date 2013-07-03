package functional

import scalaz._
import Scalaz._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec


/*
* User: catayl2
* Date: 1/6/13
* Time: 6:38 PM
*/


case class Peep(name:String, age:Int)
case class Peeper(peep:Peep, offender:Boolean)
case class CaseKey(id:Int)

object Peep {

  implicit def peepSemi:Semigroup[Peep] = semigroup ((f1, f2) => Peep(f1.name |+| f2.name, f1.age |+| f2.age))
  implicit def peep:Zero[Peep] = zero(Peep("",0))

  implicit def peeperSemi:Semigroup[Peeper] = semigroup((f1, f2) => Peeper(f1.peep |+| f2.peep, f1.offender |+| f2.offender))
  implicit def peeperZero:Zero[Peeper] = zero (Peeper( Peep("",0), false ))

  /** To be able to use scalaz's assert_=== operator **/
  implicit def PeepShow:Show[Peep] = showA
  implicit def PeepEqual:Equal[Peep] = equalA
  implicit def PeeperShow:Show[Peeper] = showA
  implicit def PeeperEqual:Equal[Peeper] = equalA

}
