package functional.fpbook

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import practice.fpbook.Genotypes.{Gap, RefAllele, Genotype}
import practice.fpbook.{GtBundle, HOFuncs}
import language.implicitConversions

/*
* User: catayl2
* Date: 7/2/13
* Time: 8:04 PM
*/

class FPStreamTest extends FlatSpec with ShouldMatchers {

  it should "return None for flatMap over List(Option[Int]) if any option element is None" in {
    val optList = List(Option(5), None)

    HOFuncs.sequence(optList) should equal(None)

    val o2 = List(Option(7), None, Option(9))

    val duh = for {e <- o2; o <- e} yield o // Shows how flatMap can be used as a filter

    duh should equal( List(7,9) )

    // Shows flatMap as an all or nothing sequence success determiner.
    HOFuncs.sequence(o2.updated(1, Option(11))) should equal (Some(List(7,11,9)))
  }

  it should "Work with type aliases" in {

    val bndlA = new GtBundle(1, 500, "Zm_B73_CR01", Vector("A","A","T","G", "-\t4", "C", "G", "-\t489", "T"))
    val gtList = bndlA.dnaSequence

    println( gtList.collect(refAllele.orElse(gapCall)).flatten.map {_.genotypeCall}.mkString("") )

    println( gtList.map{_.genotypeCall}.mkString )

    println(gtList.collect(refAllele).flatten.mkString(", "))
  }

  val refAllele = new PartialFunction[Genotype,List[Genotype]] {

    def apply(v1: Genotype):List[Genotype] =
      v1 match {
        case g:RefAllele => List(g)
      }
    def isDefinedAt(x:Genotype) = {
      x match {
        case a:RefAllele => true
        case b => false
      }
    }
  }

  val gapCall = new PartialFunction[Genotype,List[Genotype]] {
    def apply(v1: Genotype):List[Genotype] =
      v1 match {
        case g:Gap => List(g)
      }
    def isDefinedAt(x:Genotype) = {
      x match {
        case a:Gap => true
        case b => false
      }
    }
  }

}
