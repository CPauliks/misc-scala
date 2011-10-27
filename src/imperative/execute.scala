package imperative

import scala.collection.immutable.Map

/**
 * Something that can be used on the right-hand side of an assignment.
 */
trait RValue[T] {
  def get: T
}

/**
 * Something that can be used on the left-hand side of an assignment.
 */
trait LValue[T] extends RValue[T] {
  def set(value: T): LValue[T]
}

/**
 * A cell for storing a value.
 */
case class Cell[T](var value: T) extends LValue[T] {
  override def get = value
  override def set(value: T) = { this.value = value ; this }
}

/**
 * A companion object defining a useful Cell instance.
 */
object Cell {
  val NULL = Cell(0)
}

/**
 * An interpreter for expressions and statements.
 */
object Execute {

  type Store = Map[String, LValue[Int]]

  def apply(store: Store)(s: Statement): LValue[Int] = s match {
    case Constant(value) => Cell(value)
    case Plus(left, right) => Cell(apply(store)(left).get + apply(store)(right).get)
    case Minus(left, right) => Cell(apply(store)(left).get - apply(store)(right).get)
    case Times(left, right) => Cell(apply(store)(left).get * apply(store)(right).get)
    case Div(left, right) => Cell(apply(store)(left).get / apply(store)(right).get)
    case Variable(name) => store(name)
    case Assignment(left, right) => {
      val rvalue = apply(store)(right)
      val lvalue = apply(store)(left)
      lvalue.set(rvalue.get)
    }
    case Sequence(statements @ _*) =>
      statements.foldLeft(Cell.NULL.asInstanceOf[LValue[Int]])((c, s) => apply(store)(s))
    case While(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue.get != 0) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Cell.NULL
    }
  }
}
