package functional.fpbook

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import practice.fpbook.{Ms, Sum}

/*
* User: catayl2
* Date: 7/7/13
* Time: 3:57 PM
*/
class SumMonoidsTest extends FlatSpec with ShouldMatchers {

  it should "sum properly" in {
    import functional.ImplicitClassConverters._
    import Ms.{strAddition,intAddition,sumMonoid,optionMonoid}
    implicit val (sumStrM,sumIntM,optSumStrM,optSumInt)=(sumMonoid[String],sumMonoid[Int],optionMonoid[Sum[String]],optionMonoid[Sum[Int]])

    Sum(8) + Sum(1) should equal ( Sum(9) )
    Sum("4") + Sum("2") should equal ( Sum("42") )
    Option(Sum("a")) + Option(Sum("b")) should equal ( Some(Sum("ab")) )
    Option(Sum(9)) + Option(Sum(10)) should equal ( Some(Sum(19)) )
    Option(Sum(9)) + None should equal ( Some(Sum(9)) )
    Option(Sum("9")) + Option(Sum("11")) should equal ( Some(Sum("911")) )
    Sum(8) ~+ 9 should equal( 17 )
  }
}
