package practice.fpbook

/*
* User: catayl2
* Date: 2/2/13
* Time: 6:41 PM
*/

trait Identity[A] {
  def value: A
  def ~+(a: => A)(implicit s: M[A]): A = s op (value, a)
}

trait M[A] {
  def op(a1:A, a2:A):A
  def zero:A
}

trait NAD[A] extends Identity[A] {
  def value:A
  def fMap[B](f:(A) => NAD[B]):NAD[B] = f(value)
}

object NAD {
  def apply[A](a:A):NAD[A] = new NAD[A] with Identity[A]{
    def value = a
  }

}

object Test extends App {
  val sNad = NAD(Sum(9))
  import Ms.intAddition
  println(sNad.fMap{s => NAD(Sum(Sum(Sum(s ~+ 8) ~+ 13) ~+ (30 + s.value)))}.value)
}

case class Sum[A](value: A) extends Identity[A]
case class Prod[A](value: A) extends Identity[A]

object Ms {

	def dMonoid[X](zeroX:X, f:(X, X) => X) = new M[X]{
    def op(a1: X, a2: X) = f(a1,a2)
    def zero = zeroX
  }

  def ident[X](x:X) = new Identity[X] {def value:X = x}

  implicit def monoid[A](implicit A:M[A]): M[A] = A

  implicit val intAddition: M[Int] = dMonoid(0, {(a:Int, b:Int) => a + b})
	implicit val strAddition = dMonoid("", {(a:String, b:String) => a + b})
  implicit val booleanOr: M[Boolean] = dMonoid(false, {(a:Boolean, b:Boolean) => a || b})
  implicit val booleanAnd: M[Boolean] = dMonoid(true, {(a:Boolean, b:Boolean) => a && b})

  implicit def listMonoid[A] = dMonoid(Nil, {(a:List[A], b:List[A]) => a++b})
  implicit def sumMonoid[A](implicit A:M[A]) = dMonoid(Sum(A.zero), {(a:Sum[A], b:Sum[A]) => Sum(A.op(a.value, b.value))})
  implicit def optionMonoid[A](implicit A:M[A]) = dMonoid(None, {(a:Option[A], b:Option[A]) => a.map {anA => A.op(anA, b.getOrElse(A.zero))}})
}

object PMs {
  import Ms.dMonoid

  implicit val intMultiplication: M[Int] = dMonoid(1, {(a,b) => a*b})
  implicit val strMultiplication = Ms.dMonoid("",{(a:String, b:String) =>
    val cat = a+b
    (0 until cat.length).toList.foldLeft(cat){(acc,x) => s"$acc|$cat"}
  })

  implicit def productMonoid[A](implicit m:M[A]) = new M[Prod[A]] {
    def op(a: Prod[A], b: Prod[A]) = Prod(m.op(a.value, b.value))
    def zero = Prod(m.zero)
  }
}

