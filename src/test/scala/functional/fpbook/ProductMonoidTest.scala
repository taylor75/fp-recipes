package functional.fpbook

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import practice.fpbook.{Prod, PMs, Ms}

/*
* User: catayl2
* Date: 7/7/13
* Time: 3:39 PM
*/
import PMs.{intMultiplication,strMultiplication,productMonoid}
import functional.ImplicitClassConverters._
import Ms.{optionMonoid,dMonoid}

case class Deez[T](argument:Option[practice.fpbook.Prod[Option[T]]])

object ProdMonoids {
  implicit val prodInt = productMonoid[Int]
  implicit val prodStr = productMonoid[String]
  implicit val productOpt = productMonoid[Option[Int]]
  implicit val productOptStr = productMonoid[Option[String]]
  implicit val optMult = optionMonoid[practice.fpbook.Prod[Option[Int]]]
  implicit val optMultStr = optionMonoid[practice.fpbook.Prod[Option[String]]]
  implicit val deezAddition = dMonoid(Deez[Int](None), {(a:Deez[Int], b:Deez[Int]) => Deez(a.argument + b.argument)})
  implicit val deezAdditionStr = dMonoid(Deez[String](None), {(a:Deez[String], b:Deez[String]) => Deez(a.argument + b.argument)})
}

class ProductMonoidTest extends FlatSpec with ShouldMatchers {

  it should "monoidically combine according to expectations" in {
    import ProdMonoids._

    Some(Prod(33)) * Some(Prod(3)) should equal(Some(Prod(99)))

    Some(Prod(33)) * None should equal (Some(Prod(33)))

    Some(Prod(9)) * Some(Prod(8)) should equal ( Some(Prod(72)) )

    Some(Prod("abc")) * Some(Prod("xyz")) should equal ( Some(Prod("abcxyz|abcxyz|abcxyz|abcxyz|abcxyz|abcxyz|abcxyz")) )

    Some(Prod(9)) * None should equal ( Some(Prod(9)) )

    Option(Prod(Option(9))) * Option(Prod(Option(88))) should equal (Some(Prod(Some(792))))

    Option(Prod(Option(9))) * Option(Prod(None)) should equal ( Some(Prod(Some(9))) )

    Deez[Int](Option(Prod(Option(8)))) + Deez[Int](Option(Prod(Option(7)))) should equal ( Deez(Option(Prod(Option(56)))) )

    Deez[String](Option(Prod(Option("8")))) + Deez[String](Option(Prod(Option("7")))) should equal ( Deez(Option(Prod(Option("87|87|87")))) )
  }

}
