package records

object mainSimple {

  val studentCourseRecord = Clazz("firstExamScore", "secondExamScore", "totalScore")
  val studentSemRecord = Clazz("course1", "course2")
  
  val store = Map[String, Cell](
    "q" -> Cell(0),
    "r" -> Cell(0)
  )

  val s =
      Sequence(
        Assignment(Variable("r"), New(studentSemRecord)),
        Assignment(Selection(Variable("r"), "course1"), New(studentCourseRecord)),
        Assignment(Selection(Selection(Variable("r"), "course1"), "firstExamScore"), Constant(25)),
        Assignment(Selection(Selection(Variable("r"), "course1"), "secondExamScore"), Constant(35)),
        Assignment(Selection(Selection(Variable("r"), "course1"), "totalScore"),
                           Plus(Selection(Selection(Variable("r"), "course1"), "firstExamScore"),
                                    Selection(Selection(Variable("r"), "course1"), "secondExamScore"))),
        Assignment(Selection(Variable("r"), "course2"), Selection(Variable("r"), "course1")),
        Assignment(Variable("q"), Selection(Selection(Variable("r"), "course2"), "totalScore")),
        Assignment(Selection(Selection(Variable("r"), "course1"), "firstExamScore"), Constant(45))
      )

  def main(args: Array[String]) {
    println(store)
    Execute(store)(s)
    println(store)
  }
}
