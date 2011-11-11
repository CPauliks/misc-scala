package imperative

object mainWithParser {

  val store = Map[String, LValue[Int]](
    "x" -> Cell(0),
    "y" -> Cell(0),
    "r" -> Cell(0)
  )

  val s = "{x = 2, y = 3, while (y) { r = r + x, y = y - 1 }}"

  def main(args: Array[String]) {
    println(s)
    val p = StatementParser.parseAll(StatementParser.statement, s)
    println(p)
    println(store)
    Execute(store)(p.get)
    println(store)
  }
}
