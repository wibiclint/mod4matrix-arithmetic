import com.twitter.algebird._
import com.twitter.scalding._
import com.twitter.scalding.mathematics._

/** Simple case class for mod-4 arithmetic. */
case class Mod4Int(x: Int) {
  override def toString: String = x.toString
}

/**
 * Create a ring for mod-4 numbers.  The ring is a set of operations that we can perform on mod-4
 * numbers.  For a ring, we need the following:
 * - To define addition (addition is closed)
 * - To define an additive identity (zero)
 * - To define multiplication (multiplication should be closed and associative)
 * - To ensure that addition is distributive over multiplication
 *      e.g.,
 *          a * (b + c) = a * b + a * c
 *      and
 *          (a + b) * c = a * c + b * c
 *
 * We implement our ring using a Scalding "type class."  See http://tinyurl.com/b6sfh9l for an
 * excellent introduction to type classes.  In brief, a type class allows us to define a ring for a
 * class (such as `Mod4Int`) without modifying the underlying class itself.
 *
 * We put the implicit type class within another object and then immediately import it because Scala
 * does not allow us to define an implicit object at the top level of our file.
 */
object Mod4RingImplicits {
  implicit object Mod4Ring extends Ring[Mod4Int] {
    override def zero = Mod4Int(0)
    override def one = Mod4Int(1)
    override def plus(l : Mod4Int, r : Mod4Int) = Mod4Int((l.x + r.x) % 4)
    override def times(l : Mod4Int, r : Mod4Int) = Mod4Int((l.x * r.x) % 4)
  }
}

import Mod4RingImplicits._

/**
 * A silly test case that actually uses the mod-4 integers.  We multiply the matrix:
 *    1   2
 *    1   2
 * by the identity matrix. The output TSV file should look like:
 *  0   0   1
 *  0   1   2
 *  1   0   1
 *  1   1   2
 */
class Mod4Test(args: Args) extends Job(args) {

  import Matrix._

  val matrixA = IterableSource(List(
      // Row, col, val
      (0, 0, 1),
      (0, 1, 2),
      (1, 0, 1),
      (1, 1, 2)), ('row, 'col, 'val))
      .map('val -> 'val) { v: Int => Mod4Int(v) }
      .toMatrix[Int, Int, Mod4Int]('row, 'col, 'val)

  // Identity matrix
  val matrixI = IterableSource(List(
      (0, 0, 1),
      (1, 1, 1)), ('row, 'col, 'val))
      .map('val -> 'val) { v: Int => Mod4Int(v) }
      .toMatrix[Int, Int, Mod4Int]('row, 'col, 'val)

  (matrixA * matrixI)
    .write( Tsv( args("output") ) )
}

