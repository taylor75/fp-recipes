package practice.fpbook

import System.out.{println => say}

/*
* User: catayl2
* Date: 9/19/12
* Time: 9:28 PM
*/
object HOFuncs {

  type Pred[A] = A => Boolean

  def sequence[T](opts:List[Option[T]]):Option[List[T]] = {
    val initial:Option[List[T]] = Option(List.empty)
    opts.foldLeft(initial){(b,a) => a.flatMap{i => b.flatMap{i2 => Option(i :: i2)}}}
      .flatMap {oLst => Option(oLst.reverse)}
  }

  def lift[A](f:(Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] = { b:A => f(g(b), h(b)) }

  def curry[A,B,C](f: (A,B) => C): A => B => C = (a:A) => f(a, _:B)

  def compose[A,B,C](f:B => C, g: A => B): A => C = { (a:A) => f(g(a)) }

}

import org.scalatest._
import matchers.ShouldMatchers
import practice.fpbook.Genotypes.RefAllele
import practice.fpbook.Genotypes.Gap
import practice.fpbook.Genotypes.Genotype
import HOFuncs._

class HOFuncsExercise extends FlatSpec with ShouldMatchers {

  it should "not fail any assertions" in {
    def divBy(i:Int) = (j:Int) => (j % i) == 0
    val lifted = lift[Int](_:Function2[Boolean,Boolean,Boolean], divBy(3), divBy(5))
    assert(lifted{_ && _}(99) == false)
    assert(lifted{_ || _}(99) == true)
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

package Genotypes {
  abstract class Genotype(val genotypeCall:String, val physicalPosition:Int)

  case class GenotypeBlockStart(bStart:Int) extends Genotype("", bStart)
  case class RefAllele(pseudoBase:String, position:Int) extends Genotype(pseudoBase, position)
  case class Gap(gapSymbol:String, gapStart:Int, gapEnd:Int)
    extends Genotype("("+gapSymbol + (gapEnd - gapStart + 1).toString+")", gapEnd)
  case class Insertion(nucleotides:String, position:Int) extends Genotype(nucleotides, position)
  case class Deletion(symbol:String, position:Int, numBases:Int) extends Genotype(symbol, position)
  case class AlignedGenotype(call:String, position:Int, pValue:Double) extends Genotype(call, position)
}

import Genotypes._
class GtBundle(start:Int, end:Int, seqName:String, attData:Vector[String]) {

  def dnaSequence:Seq[Genotype] = {
    val initGt = GenotypeBlockStart(start - 1).asInstanceOf[Genotype]
    attData.headOption match {
      case Some(head:String) =>
        attData.tail.foldLeft(Vector(deriveGtFunc(initGt, head))){ foldFunc }.reverse
      case None =>
        Seq.empty
    }
  }

  def deriveGtFunc(sGt:Genotype, attHead:String):Genotype = {
    attHead.split("\t") match {
      case Array(e1) =>
        RefAllele(e1, sGt.physicalPosition + 1)
      case Array(e1, e2) =>
        Gap("-", sGt.physicalPosition + 1, sGt.physicalPosition + e2.toInt)
    }
  }

  def foldFunc(acc:Vector[Genotype], a:String) = {
    deriveGtFunc(acc.head, a) +: acc
  }
}
