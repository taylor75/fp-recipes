package functional

import xml.NodeSeq

/*
* User: ctaylor
* Date: 3/18/13
* Time: 4:03 PM
*/

/* Here is the first piece of the Scala typeclass puzzle, its just a trait.
 * The trait defines a concept which in this case is a transformer
 * that transforms a type T into a R.
 */
trait Transformer[T, R] {
  def transform(t: T): R
}

/*
* This is a companion object for the typeclass giving default implementations for the typeclass.
* These implementations are found after local implicits, so you can still override the default
* behaviour. For more about the search order see [3].
*/
object Transformer {

  implicit object IntToStringTransformer extends Transformer[Int, String] { def transform(t: Int) = t.toString }

  /* This one is interesting. It transforms a List[T] into a String, but to do that it
  * needs a transformer for T to String. So it requires such a transformer implicitly.
  */
  implicit def ListToStringTransformer[T](implicit tToString: Transformer[T, String]) = new Transformer[List[T], String] {
    def transform(t: List[T]) = t.map(tToString.transform(_)).mkString(",")
  }
}

// This is something that makes use of the typeclass
trait Transform {

  // The implicit Transformer, transformer, supplies an appropriate transformer for the method
  def transform[T, R](t: T)(implicit transformer: Transformer[T, R]): R = transformer.transform(t)
}

// These examples will drop back to the default implementations in the Transformer's companion object
object ExampleWithDefaults extends App with Transform {
  val transformationA = transform(2)
  val transformationB = transform(List(1,2,3))

  println(transformationA) // prints 2
  assert(transformationA.isInstanceOf[String])

  println(transformationB) // prints 1,2,3
  assert(transformationB.isInstanceOf[String])
}

// I want my own special transformers!
object ExampleWithMyTransformers extends App with Transform {

  implicit object MyIntToXmlTransformer extends Transformer[Int, xml.Node] { def transform(t: Int) = <aNumber>{ t.toString }</aNumber> }

  // As with the default List transformer, this one needs a transformer of T to Node so that it can perform the transform for lists.
  implicit def MyListToXmlTransformer[T](implicit transformer: Transformer[T, xml.Node]) = new Transformer[List[T], xml.NodeSeq] {
    import xml._
    def transform(t: List[T]): NodeSeq = t.foldLeft(NodeSeq.Empty)((accumulator, next) => accumulator ++ transformer.transform(next))
  }

	val transformationA = transform(2)
  val transformationB = transform(List(1,2,3))

  println(transform(2)) // prints <aNumber>2</aNumber>
  assert(transformationA.isInstanceOf[NodeSeq])

  println(transform(List(1, 2, 3))) // prints <aNumber>1</aNumber><aNumber>2</aNumber><aNumber>3</aNumber>
  assert(transformationB.isInstanceOf[NodeSeq])

  // Here is a transformer from Boolean to String
  implicit object BooleanToStringTransformer extends Transformer[Boolean, String] { def transform(b: Boolean) = b.toString }

  // This is the clever bit.
  // Now we can now transform List[Boolean] to String. The List transformer defined in the default Transformer trait
  // will be used but with the new BooleanToStringTransformer. So the default transformers and the new ones
  // work together.
  println(transform(List(true, false))) // prints true,false

  // Finally, if there are no implicits available then the code fails to compile.
  // transform("3")
}