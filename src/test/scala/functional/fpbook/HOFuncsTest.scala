package functional.fpbook

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import practice.fpbook.Genotypes.{Genotype, Gap, RefAllele}
import practice.fpbook.{HOFuncs, GtBundle}
import HOFuncs._
import System.out.{println => say}

/*
* User: catayl2
* Date: 7/2/13
* Time: 8:09 PM
*/


class HOFuncsTest extends FlatSpec with ShouldMatchers {

  it should "not fail any assertions" in {
    def divBy(i:Int) = (j:Int) => (j % i) == 0
    val lifted = lift[Int](_:Function2[Boolean,Boolean,Boolean], divBy(3), divBy(5))
    assert(!lifted { _ && _ }(99))
    assert(lifted { _ || _ }(99))

    val curried = curry ((a:Int, b:Int) => a+b)
    val cur5 = curried(5)

    def g(n:Double) = 2*n.toInt
    def f(n:Int):String = {
      (1 to n).toList.foldLeft(List.empty[String]) {(b,a) => n.toString :: b}.reverse.mkString(", ")
    }

    def fg:(Double) => String = compose(f, g)

    say( fg(2.0) )

    assert(cur5(8) == 13)
  }

  it should " work with a Stream of Genotypes from a Bundle" in {
    val bndlA = new GtBundle(1, 500, "Zm_B73_CR01", Vector("A","A","T","G", "-\t4", "C", "G", "-\t489", "T"))
    val gtList = bndlA.dnaSequence
    println(gtList)
    assert( Vector(RefAllele("A",1), RefAllele("A",2), RefAllele("T",3), RefAllele("G",4), Gap("-",5,8),
      RefAllele("C", 9), RefAllele("G", 10), Gap("-", 11, 499), RefAllele("T", 500)) == gtList )

    println(gtList.collect {case g:Genotype if(!g.isInstanceOf[Gap]) => g}.mkString(", "))
  }

  it should " work as a converted Stream[Genotype] " in {
    val bndlA = new GtBundle(1, 500, "Zm_B73_CR01", Vector("A","A","T","G", "-\t4", "C", "G", "-\t489", "T"))
    val gtList = bndlA.dnaSequence
    val gtStream = gtList.toStream

    assert(gtStream.takeRight(4).toList == List(RefAllele("C", 9), RefAllele("G", 10), Gap("-", 11, 499), RefAllele("T", 500)))
  }
}
