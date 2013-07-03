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


case class Sum[A](value: A) extends Identity[A]

case class Product[A](value: A)

object Ms {

	def monoid[A](implicit A:M[A]): M[A] = A

  implicit val intAddition: M[Int] = new M[Int] {
    def op(a1: Int, a2: Int) = a1+a2

    def zero = 0
  }

  implicit val intMultiplication: M[Int] = new M[Int] {
    def op(a1: Int, a2: Int) = a1 * a2

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

  def listMonoid[A] = new M[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  implicit def sumMonoid[A](implicit A:M[A]) = new M[Sum[A]] {
    def op(a: Sum[A], b: Sum[A]) = Sum( A.op(a.value, b.value))
    def zero = Sum(A.zero)
  }

  implicit def productMonoid[A](A:M[A]) = new M[Product[A]] {
    def op(a: Product[A], b: Product[A]) = Product(A.op (a.value, b.value))
    def zero = Product(A.zero)
  }

  implicit def optionMonoid[A](A:M[A]) = new M[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = {
			a1.map {anA => A.op(anA, a2.getOrElse(A.zero))}
    }

    def zero = None
  }
}

object TestSum extends App {

  import Ms.intAddition
  import Ms.intAddition._
  import Ms.sumMonoid


  implicit val sm = sumMonoid[Int]
  println(Sum(8) ~+ 9)
}

object TesterApp extends App {
  import Ms._

	val sumStr = sumMonoid(strAddition)

  println(
		sumMonoid(intAddition).op(Sum(9),Sum(10))  // Sum(19)
	)

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

	val opm = optionMonoid(productMonoid(intMultiplication))
	println(
		opm.op( None, Some(Product(9)) ) // None
	)

	val opm2 = optionMonoid(productMonoid(intMultiplication))
	println(
		opm.op( Some(Product(9)), None ) // Some(Product(0))
	)

	val pom = productMonoid(optionMonoid(intMultiplication))
	println(
		pom.op( Product(Some(9)), Product(None) ) // Product(Some(0))
	)

	val som = productMonoid(optionMonoid(intAddition))
	println(
		som.op( Product(Some(9)), Product(None) ) // Product(Some(9))
	)
}
