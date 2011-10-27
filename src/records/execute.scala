package records

import Execute.{Store,Value}

/**
 * A cell for storing a value (either a number or an object).
 */
case class Cell(var value: Value){
  def get = value
  def set(value: Value) = { this.value = value ; this }
}

/**
 * A companion object defining a useful Cell instance.
 */
object Cell {
  def apply(i: Int): Cell = Cell(Left(i)) // Left -> number, Right -> object
  val NULL = Cell(0)
}

/**
 * An interpreter for expressions and statements.
 */
object Execute {

  /**
   * A memory store is a mapping from variable names to storage cells.
   */
  type Store = Map[String, Cell]

  /**
   * An object (instance) is the same as a memory store.
   */
  type Instance = Store

  /**
   * A run-time value is either a number or an object.
   */
  type Value = Either[Int,Instance]

  def apply(store: Store)(s: Statement): Cell = s match {
    case Constant(value) => Cell(Left(value))
    case Plus(left, right) => binaryOperation(store, left, right, _+_)
    case Minus(left, right) => binaryOperation(store, left, right, _-_)
    case Times(left, right) => binaryOperation(store, left, right, _*_)
    case Div(left, right) => binaryOperation(store, left, right, _/_)
    case Variable(name) => store(name)
    case Assignment(left, right) => {
      val rvalue = apply(store)(right)
      val lvalue = apply(store)(left)
      lvalue.set(rvalue.get)
    }
    case Sequence(statements @ _*) =>
      statements.foldLeft(Cell.NULL)((c, s) => apply(store)(s))
    case While(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue.get.isRight || gvalue.get.left.get != 0) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Cell.NULL
    }
    case New(Clazz(fields @ _*)) => 
      // create an object based on the list of field names in the clazz
      Cell(Right(Map(fields.map(field => (field, Cell(0))): _*)))
    case Selection(record, field) => {
      // assume the expression evaluates to a record (.right)
      // and choose the desired field
      apply(store)(record).get.right.get.apply(field)
    }
  }

  def binaryOperation(store: Store, left: Statement, right: Statement, operator: (Int, Int) => Int): Cell = {
    val l: Int = apply(store)(left).get.left.get
    val r: Int = apply(store)(right).get.left.get
    Cell(Left(operator(l, r)))
  }
}
