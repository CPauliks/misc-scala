package records
object mainImperative {

  val store = Map[String, Cell](
    "x" -> Cell(0),
    "y" -> Cell(0),
    "r" -> Cell(0)
  )

  val s =
    While(Variable("y"),
      Sequence(
        Assignment(Variable("r"), Plus(Variable("r"), Variable("x"))),
        Assignment(Variable("y"), Minus(Variable("y"), Constant(1)))
      )
    )
    
  val t = "while (y) {\nr = r + x,\ny=y-1\n};".stripSuffix(";")

  def main(args: Array[String]) {
    println(store)
    Execute(store)(s)
    println(store)
  }
}
