package functional

import util.Random

/*
* User: catayl2
* Date: 1/4/13
* Time: 8:07 PM
*/

trait Monoid[A] {
  def append(a1: A, a2: A): A
  def empty: A
}

object Monoid {
  implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def append(a1: List[A], a2: List[A]) = a1 ::: a2
    def empty = Nil
  }
}

case class Logger[LOG, A](log: LOG, value: A) {
  def map[B](f: A => B) = Logger(log, f(value))

  def flatMap[B](f: A => Logger[LOG, B])(implicit m: Monoid[LOG]) = {
    val x = f(value)
    Logger(m.append(log, x.log), x.value)
  }
}

object Logger {
  def unital[LOG, A](value: A)(implicit m: Monoid[LOG]) =
    Logger(m.empty, value)

  def noLog[A](a: A) =
    unital[List[String], A](a)
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
