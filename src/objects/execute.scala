package objects

import Execute.{Store,Value}

/**
 * A cell for storing a value (either a number or an object).
 */
case class Cell(var value: Value) {
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
   * A memory store is now a pair consisting of
   * 1) a mapping from variable names to storage Cells and
   * 2) a mapping from method names to methods.
   */
  type Store = (Map[String, Cell], Map[String, (Seq[String], Statement)])

  /**
   * An object (instance) is still the same as a memory store.
   */
  type Instance = Store

  /**
   * A run-time value is either a number or an object.
   */
  type Value = Either[Int, Instance]

  def apply(store: Store)(s: Statement): Cell = s match {
    case Constant(value) => Cell(Left(value))
    case Plus(left, right) => binaryOperation(store, left, right, _+_)
    case Minus(left, right) => binaryOperation(store, left, right, _-_)
    case Times(left, right) => binaryOperation(store, left, right, _*_)
    case Div(left, right) => binaryOperation(store, left, right, _/_)
    case Variable(name) => store._1(name)
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
    case New(fClazz) => {
      // apply the fClazz to obtain a Clazz instance
      // and use pattern matching to obtain the constituents
      val Clazz(fields, methods) = fClazz()
      // create an object based on the list of field names and methods
      val fs = Map(fields.map(field => (field, Cell(0))): _*)
      val ms = Map(methods: _*)
      Cell(Right((fs, ms)))
    }
    case Selection(receiver, field) => {
      // assume the expression evaluates to a record (.right)
      // and choose the desired field
      apply(store)(receiver).get.right.get._1(field)
    }
    case Message(receiver, method, arguments @ _*) => {
      // evaluate receiver expression to a Cell containing an Instance
      val rec = apply(store)(receiver)
      // look up method in the Instance's second component (method table)
      val meth = rec.get.right.get._2(method)
      // evaluate the arguments
      val args = arguments.map(apply(store))
      // create argument bindings "0" -> arg(0), "1" -> arg(1), etc.
      val argBindings = (0 until args.length) map (_.toString) zip args
      // create bindings for the local variables
      val localBindings = meth._1 map (field => (field, Cell(0)))
      // augment the store with these new bindings
      val storeWithBindings =
        (store._1 + ("this" -> rec) ++ argBindings ++ localBindings, store._2)
      // finally execute the resulting Statement in the augmented store
      // (note that this automatically returns the result if there is one)
      apply(storeWithBindings)(meth._2)
    }
  }

  def binaryOperation(store: Store, left: Statement, right: Statement, operator: (Int, Int) => Int): Cell = {
    val l: Int = apply(store)(left).get.left.get
    val r: Int = apply(store)(right).get.left.get
    Cell(Left(operator(l, r)))
  }
}
