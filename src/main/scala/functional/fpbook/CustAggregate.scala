package practice.fpbook

/*
* User: catayl2
* Date: 10/28/12
* Time: 3:35 PM
*/

class RDoc(val id:String)

abstract class CustAggregate[R <: RDoc]() {

  def rOp(left:R, right:R):R

  def perishFolded(z:List[R]) {
    z.foreach {e => println("Perishing " + e.id)}
  }

  def run(d:R) {

  }

  def reReduce(z:List[R])(rOp:(R,R) => R):Either[IllegalStateException, R] = {
    z match {
      case Nil => Left(new IllegalStateException("You should never ask me to reReduce an empty list!"))
      case h :: Nil => Right(h)
      case h :: tl => Right(z.reduce(rOp))
    }
  }
}

case class Person(name:String, age:Int)

case class CoolR(myId:String, people:List[Person], reductionHistory:List[String]) extends RDoc(myId)

class MyAggregation() extends CustAggregate[CoolR]{

  def rOp(left: CoolR, right: CoolR) =
    CoolR(IdGen.makeId(),
      (left.people ::: right.people).distinct,
      (left.reductionHistory ::: right.reductionHistory).distinct
    )
}


object IdGen {

  lazy val rand:java.util.Random = new java.util.Random()

  def makeId():String = {
    (0 to 9).map{ b => rand.nextInt(9).toString }.mkString("")
  }
}

object TestApp extends App {

  Range(0,20).foreach {i =>
    println(IdGen.makeId())
  }

}