package records

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class TestParser extends FunSuite {
  
  /**
   * Fails due to parser creating Lists instead of WrappedArrays
   * No problem with actual parser.
   */
  test("Parse while loop") {
    val s =
	    While(Variable("y"),
	      Sequence(
	        Assignment(Variable("r"), Plus(Variable("r"), Variable("x"))),
	        Assignment(Variable("y"), Minus(Variable("y"), Constant(1)))
	      )
	    ).toString()
	    
    val t = StatementParser.parseAll(StatementParser.statement , "while (y) {\nr = r + x,\ny=y-1\n};".stripSuffix(";"))
    assertEquals(s, t.get.toString)
  }

  test("Parse field selection and assignment") {
    val s = 
      Assignment(Selection(Selection(Variable("r"), "course1") , "totalScore"),
          Plus(Selection(Selection(Variable("r"), "course1"), "firstExamScore"),
              Selection(Selection(Variable("r"), "course1"), "secondExamScore")
          )
      ).toString();
    
    val t = StatementParser.parseAll(StatementParser.statement, "r.course1.totalScore = r.course1.firstExamScore + r.course1.secondExamScore;".stripSuffix(";"))
    assertEquals(s, t.get.toString)
  }
  
  test("Parse assignment") {
    val s = Assignment(Variable("n"),Constant(2)).toString()
      
    val t = StatementParser.parseAll(StatementParser.statement, "n = 2;".stripSuffix(";"))
    
    assertEquals(s, t.get.toString)
  }
  
}