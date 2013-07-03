package practice.fpbook

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
