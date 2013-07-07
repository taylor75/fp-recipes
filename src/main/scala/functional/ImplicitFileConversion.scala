package functional

import java.io.File
import practice.fpbook.{Sum, Ms, M}
import scala.reflect.ClassTag
import scala.beans.BeanProperty

/*
* User: catayl2
* Date: 7/2/13
* Time: 8:21 PM
*/

trait FileLike {
  def getName:String
  def length:Long
}

class LegacyFile(fPath:String, val user:String, val pwd:String) {
  def getName:String = fPath
  def length:Long = {
    if(connect){
      new File(fPath).length()
    } else 0L
  }

  private def connect:Boolean = user=="ctaylor" && pwd==""
}

object ImplicitFileConversion {

  implicit def FileToFileLikeConverter(fL:java.io.File):FileLike = {
    new File(fL.getName) with FileLike
  }

  implicit def LegacyFileToFileLikeConverter(file:LegacyFile):FileLike = {
    new LegacyFile(file.getName, file.user, file.pwd) with FileLike
  }
}

object TestApp extends App {
  import ImplicitFileConversion._

  def fileLikeOp(fLike:FileLike) {
    println(s"${fLike.getName}, ${fLike.length}")
  }

  val curDir = new File(".")
  println(curDir.getAbsolutePath)

  fileLikeOp(curDir)

  fileLikeOp(new LegacyFile(".", "ctaylor", ""))
}


object ImplicitClassConverters {
  import practice.fpbook.Prod

  implicit class MMInt[T](value:T)(implicit T:M[T]) {
    def +(b:T):T = T.op(value, b)
  }

  implicit class MMult[T](value:Option[practice.fpbook.Prod[T]])(implicit T:M[Option[practice.fpbook.Prod[T]]]) {
    def *(b:Option[practice.fpbook.Prod[T]]):Option[practice.fpbook.Prod[T]] = T.op(value, b)
  }

}

object ImplicitClassTest extends App {
  import ImplicitClassConverters._
  import Ms.intAddition
  import Ms.sumMonoid

  implicit val longAddition = Ms.dMonoid(0L,(a:Long,b:Long) => a+b)
  implicit val sumMInt = sumMonoid[Int]
  implicit val sumMLong = sumMonoid[Long]

  println(8 + 33)
  val sInt = Sum(2) + Sum(3)
  val sLong = Sum(2L) ~+ 3L

  val identTest = Sum(3L) ~+ 9

  println(identTest)

  def doMatch[X](s:Sum[X]) {
    s match {
      case Sum(_:Long) => println("Sum[Long] => " + s)
      case Sum(_:Int) => println("Sum[Int] => " + s)
      case dunno => println(dunno)
    }
  }

  doMatch(sInt)
  doMatch(Sum(sLong))
}
