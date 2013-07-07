package functional

import practice.fpbook.M
import util.Random
import practice.fpbook.Ms.listMonoid

/*
* User: catayl2 => c.f. http://blog.tmorris.net/posts/the-writer-monad-using-scala-example/
* Date: 1/4/13
* Time: 8:07 PM
*/

case class Logger[LOG, A](log: LOG, value: A) {
  def map[B](f: A => B) = Logger(log, f(value))

  def flatMap[B](f: A => Logger[LOG, B])(implicit m: M[LOG]) = {
    val x = f(value)
    println("appending: log "+log + ", x.log: " + x.log + " with value=" + value)
    Logger(m.op(log, x.log), x.value)
  }
}

object Logger {
  def unital[LOG, A](value: A)(implicit m: M[LOG]) = {
    Logger(m.zero, value)
  }

  def noLog[A](a: A) = {
    val logger = unital[List[String], A](a)
    logger
  }
}

object Main extends App {
  lazy val rand = new Random()
  val x = rand.nextInt( args(0).toInt )

  val r =
    for(a <- addOne(x);
        b <- intString(a);
        c <- lengthIsEven(b);
        d <- Logger.noLog(hundredOrThousand(c));
        e <- times7(d)
    ) yield e

  println("LOG")
  println("---")
  r.log foreach println

  def hundredOrThousand(b: Boolean) = if(b) 100 else 1000

  /** Logger Combinators **/
  def addOne(n: Int) = Logger(List("adding one to " + n), n+1)

  def intString(n: Int) = Logger(List("converting int to string " + n), n.toString)

  def lengthIsEven(s: String) = {
    val randomMod = s.toInt
    Logger(List("checking randomMod " + randomMod + " for evenness"), randomMod % 2 == 0 )
  }

  def times7(n: Int) = Logger(List("multiplying " + n + " by 7 to produce " + 7*n), n * 7 )
}
