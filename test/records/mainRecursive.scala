package records

object mainRecursive {

  val store = Map[String, Cell](
    "n" -> Cell(0),
    "h" -> Cell(0),
    "s" -> Cell(0)
  )

  val listNode = Clazz("value", "next")

  /*
   * n = new ListNode
   * h = n
   * n.value = 2
   * n.next = new ListNode
   * n = n.next
   * ...
   * n.value = 7
   * n.next = null
   * n = h
   * while (n != null) {
   *   s = s + n.value
   *   n = n.next
   * }
   */


  val s =
    Sequence(
      Assignment(Variable("n"), New(listNode)),
      Assignment(Variable("h"), Variable("n")),
      Assignment(Selection(Variable("n"), "value"), Constant(2)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(3)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(5)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(7)),
      Assignment(Selection(Variable("n"), "next"), Constant(0)),
      Assignment(Variable("n"), Variable("h")),
      While(Variable("n"),
        Sequence(
          Assignment(Variable("s"), Plus(Variable("s"), Selection(Variable("n"), "value"))),
          Assignment(Variable("n"), Selection(Variable("n"), "next"))
        )
      )
    )

  def main(args: Array[String]) {
    println(store)
    Execute(store)(s)
    println(store)
  }
}
