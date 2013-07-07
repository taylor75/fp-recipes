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

trait M[A] { self: M[A] =>
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
  import Ms.sumMonoid
  println(sNad.fMap{s => NAD(Sum(Sum(Sum(s ~+ 8) ~+ 13) ~+ (30 + s.value)))}.value)
}

case class Sum[A](value: A) extends Identity[A]
case class MM[A](value:A) extends Identity[A]

case class Product[A](value: A) extends Identity[A]

object Ms {

	def dMonoid[X](zeroX:X, f:(X, X) => X) = new M[X]{
    def op(a1: X, a2: X) = f(a1,a2)
    def zero = zeroX
  }

  def ident[X](x:X) = new Identity[X] {def value:X = x}

  implicit def monoid[A](implicit A:M[A]): M[A] = A

  implicit val intAddition: M[Int] = new M[Int] {
    def op(a1: Int, a2: Int) = a1+a2
    def zero = 0
  }

	implicit val strAddition = new M[String] {
		def op(a1:String, a2:String) = a1 + a2
		def zero = ""
	}

  val booleanOr: M[Boolean] = new M[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd: M[Boolean] = new M[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  implicit def listMonoid[A] = new M[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  implicit def sumMonoid[A](implicit A:M[A]) = new M[Sum[A]] {
    def op(a: Sum[A], b: Sum[A]) = Sum( A.op(a.value, b.value))
    def zero = Sum(A.zero)
  }

  implicit def optionMonoid[A](implicit A:M[A]) = new M[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.map {anA => A.op(anA, a2.getOrElse(A.zero))}
    def zero = None
  }
}

object PMs {

  implicit val intMultiplication: M[Int] = new M[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 0
  }

  implicit val strMultiplication = Ms.dMonoid("",{(a:String, b:String) =>
    val cat = a+b
    (0 until cat.length).toList.foldLeft(cat){(acc,x) => s"$acc|$cat"}
  })

  implicit def productMonoid[A](implicit m:M[A]) = new M[Product[A]] {
    def op(a: Product[A], b: Product[A]) = Product(m.op(a.value, b.value))
    def zero = Product(m.zero)
  }
}

object TestSum extends App {

  import Ms.intAddition
  import Ms.sumMonoid
  import functional.ImplicitClassConverters._
  implicit val sm = sumMonoid[Int]
  println(Sum(8) ~+ 9)
  println(Sum(8) + Sum(17))
}

object TesterApp extends App {
  import Ms._
  import functional.ImplicitClassConverters._
	val sumStr = sumMonoid(strAddition)

  def sumSum[T](s1:Sum[T], s2:Sum[T])(implicit T:M[T]):Sum[T] = {
		s1 + s2  // Sum(19)
  }

  println(sumSum(Sum("abc"), Sum("xyz")))
  println(sumSum(Sum(8), Sum(9)))
	val osm = optionMonoid(sumMonoid(intAddition))
  println(
		osm.op( Some(Sum(9)), Some(Sum(10)) ) // Some(Sum(19))
	)

	println(
		osm.op( Some(Sum(9)), None ) // Some(Sum(9))
	)

	val osmStr = optionMonoid(sumStr)
	println(
		osmStr.op( Some(Sum("9")), Some(Sum("11")) ) // Some(Sum(911))
	)
}

case class Deez[T](argument:Option[Product[Option[T]]])

object ProductTesterApp extends App {
  import PMs.intMultiplication
  import functional.ImplicitClassConverters._
  import PMs.strMultiplication
  import Ms.optionMonoid

  implicit val prodInt = PMs.productMonoid[Int]
  implicit val prodStr = PMs.productMonoid[String]
  implicit val productOpt = PMs.productMonoid[Option[Int]]
  implicit val productOptStr = PMs.productMonoid[Option[String]]
  implicit val optMult = Ms.optionMonoid[Product[Option[Int]]]
  implicit val optMultStr = Ms.optionMonoid[Product[Option[String]]]
  implicit val deezAddition = Ms.dMonoid(Deez[Int](None), {(a:Deez[Int], b:Deez[Int]) => Deez(a.argument + b.argument)})
  implicit val deezAdditionStr = Ms.dMonoid(Deez[String](None), {(a:Deez[String], b:Deez[String]) => Deez(a.argument + b.argument)})

  println( Some(Product(33)) * Some(Product(3)) )

  println( Some(Product(33)) * None )

  println(Some(Product(9)) * Some(Product(8)))

  println(Some(Product("abc")) * Some(Product("xyz")))

	println( Some(Product(9)) * None )

  println( optMult.op(Some(Product(Some(9))), Some(Product(Some(88)))) )

  println( optMult.op( Some(Product(Some(9))), Some(Product(None))) )

  println(Deez[Int](Some(Product(Some(8)))) + Deez[Int](Some(Product(Some(7)))))

  println(Deez[String](Some(Product(Some("8")))) + Deez[String](Some(Product(Some("7")))))
}


//  implicit def boxM[A](implicit A:M[A]) = new M[MM[A]] {
//    def op(a: MM[A], b: MM[A]) = MM( A.op(a.value, b.value))
//    def zero = MM(A.zero)
//  }
