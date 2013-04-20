package practice.fpbook
import System.out.{println => say}

/*
* User: catayl2
* Date: 9/16/12
* Time: 11:12 AM
*/


trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def take(n:Int):Stream[A] = {
    if (n > 0) {
      uncons match {
        case None => Stream.empty
        case Some((hd,tl)) => Stream.cons(hd, tl.take(n-1))
      }
    } else Stream.empty
  }

  def toList:List[A] = {
    uncons match {
      case None => List.empty
      case Some((hd, tl)) => hd :: tl.toList
    }
  }

  def takeWhile(p:(A => Boolean)):Stream[A] = {
    uncons match {
      case None => Stream.empty
      case Some((hd,tl)) =>
        if (p(hd)) Stream.cons(hd, tl.takeWhile(p))
        else Stream.empty
  	}
  }

  def foldRight[B](z: => B)(implicit f: (A, => B) => B): B = {
    uncons match {
      case Some((h,tl)) =>
        val r:B = f(h, tl.foldRight(z))
        r
      case None => z
    }
  }

  def append[T >: A](other: => Stream[T]):Stream[T] = {
    this.foldRight(other){(a, b) =>
      Stream.cons(a, b)
    }
  }

  def filter(p:(A => Boolean)): Stream[A] = {
    foldRight(Stream.empty[A]){(a,b) =>
      if (p(a)) {
        say(a)
        Stream.cons(a, b)
      } else b
    }
  }

  def forall(p:(A => Boolean)):Boolean = foldRight(false){(a,b) => p(a) && b }

  def exists(p:(A => Boolean)):Boolean = foldRight(false){(a,b) => say("p(" + a + ") = "+p(a)); p(a) || b}

  def headOption:Option[A] = uncons match { case None => None; case Some((h,tl)) => Some(h) }

  def map[B](f:(A => B)) : Stream[B] = {
    val mappedStream:Stream[B] = Stream.empty
    foldRight(mappedStream){(a, b) =>
      Stream.cons(f(a), b)
    }
  }


  def flatMap[B](f:(A => Stream[B])) : Stream[B] = {
    val accumulatorStream:Stream[B] = Stream.empty
    foldRight(accumulatorStream){ (a, b) =>
      f(a).append(b)
    }
  }


}

object Stream {
  def empty[A] :Stream[A] = new Stream[A] {
    def uncons = None
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
    def uncons = Some((hd, tl))
  }

  def apply[A] (as: A*): Stream[A] = {
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail:_*))
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def unfold[A,S](z:S)(implicit f:S => Option[(S,A)]):Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((s, a)) => Stream.cons(a, unfold(s))
    }
  }
}

object LangTester extends App {
  val eList = List(List("abc", "ef|gh|ij", "exy"), List("val|var|def", "Int|Long|String"))

  say("eList: %s\n".format(eList.mkString(",")))

  val xformed = eList.flatMap {entry =>
    val folded = entry.foldLeft(List.empty[List[String]]) {(b, a) =>
      val eWordToks = a.split("\\|").toList
      if (b.isEmpty) eWordToks.map {w => List(w) }
      else {
        b.flatMap {accd => eWordToks.toList.map{wTok =>  wTok :: accd} }
      }
    }

    folded.map{_.reverse}
  }

  say(xformed)
}

object StreamApp extends App {
  val s: Stream[Int] = Stream((1 to 10).toList:_*)
  val emptyS:Stream[Int] = Stream.empty
  val s2:Stream[Int] = Stream(80,90,100,500,250)

  say(collection.immutable.Stream.iterate(1,10){i => i+2}.toList.mkString(", "))

  say(s.headOption)
  say(emptyS.headOption)
  say(s.take(2).toList)
  say(s.takeWhile {_ < 8}.toList)
  say(s.takeWhile {_ < 8}.foldRight(0){_ + _})
  say( s.foldRight(0){ (a,b) => a+b } )
  say( "Exists? " + s.exists{_ == 7} )
  say( s.forall{_ <= 5} )
  say( s.take(6).filter{_ % 2 == 0}.toList.mkString(", ") )
  say( s.map {a => (a * 10).toString}.toList )
  say( s.append(s2).toList )
  say( s.flatMap{ i => Stream((i, i)) }.toList )
  say( Stream.constant(5).take(15).toList )
  say( Stream.from(5).takeWhile(_ <= 15).toList )

  val unfolded = Stream.unfold((0, 1)){tup =>
    val nextS = (tup._2, tup._2 + tup._1)
    Option((nextS, tup._1))
  }.take(30).toList

  say ( unfolded )
}


