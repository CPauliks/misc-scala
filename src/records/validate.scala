package records

/**
 * A simple static sanity checker for Statements.
 * Misses some things, but prevents problems like performing operations on
 * Loops, Sequences, and Assignments
 * and Loops with Units as guard conditions
 */

object Validate {

  def apply(s: Statement): Boolean = s match {
    case Constant(_) => true
    case Plus(left, right) => validateBinaryStatement(left, right)
    case Minus(left, right) => validateBinaryStatement(left, right)
    case Times(left, right) =>  validateBinaryStatement(left, right)
    case Div(left, right) => validateBinaryStatement(left, right)
    case Variable(_) => true
    case Assignment(left, right) => canBeLValue(left) && apply(right)
    case Sequence(statements @ _*) => statements.foldLeft(true)((a , b) => a && apply(b))
    case While(guard, body) => guard match {
        case Constant(_) => apply(body)
        case Variable(_) => apply(body)
        case Selection(_, _) => apply(body)
        case Assignment(left, right) => apply(Assignment(left, right)) && apply(body)
        case Plus(left, right) => apply(Plus(left, right)) && apply(body)
        case Minus(left, right) => apply(Minus(left, right)) && apply(body)
        case Times(left, right) => apply(Times(left, right)) && apply(body)
        case Div(left, right) => apply(Div(left, right)) && apply(body)
        case Sequence(statements @ _*) => apply(statements.last) && apply(body)
        case _ => false
    }
    case New(Clazz(fields @ _*)) => true
    case Selection(record, field) => true
    case _ => false
  }
  
  def canBeLValue(s: Statement): Boolean = s match {
    case Variable(_) => true
    case Selection(_ , _) => true
    case Sequence(statements @ _*) => canBeLValue(statements.last)
    case _ => false
  }
  
  def validateBinaryStatement(left: Statement, right: Statement): Boolean = {
      if (canBeInABinaryStatement(left) && canBeInABinaryStatement(right)) {
        apply(left) && apply(right)
      }
      else {
        false
      }
  }
  
  def canBeInABinaryStatement(s: Statement): Boolean = s match {
    case Assignment(_, _) => false
    case While(_, _) => false
    case Sequence(statements @ _*) => false
    case New(_) => false
    case _ => true
  }
  
}