package records

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class TestValidator extends FunSuite {
  
  test("Test binary expression") {
    val s = 
      Assignment(Selection(Selection(Variable("r"), "course1") , "totalScore"),
          Plus(Selection(Selection(Variable("r"), "course1"), "firstExamScore"),
              Selection(Selection(Variable("r"), "course1"), "secondExamScore")
          )
      )
    assertTrue(Validate(s))
  }
  
  test("Test assignments in binary expression") {
    val s = Plus(Assignment(Variable("x"), Constant(2)), Constant(2))
    assertFalse(Validate(s))
  }
  
    test("Test assignments in binary expression 2") {
    val s = Div(Assignment(Variable("x"), Constant(2)), Assignment(Variable("y"), Constant(2)))
    assertFalse(Validate(s))
  }
    
    test("Test Assignment") {
      val s = Assignment(Variable("x"), Variable("y"))
      val t = Assignment(Variable("x"), Constant(2))
      val u = Assignment(Constant(2), Constant(3))
      assertTrue(Validate(s))
      assertTrue(Validate(t))
      assertFalse(Validate(u))
    }
    
    test ("Test Assignment 2") {
      val s = Assignment(Selection(
          Sequence(
	          Assignment(Selection(Variable("n"), "value"), Constant(7)),Variable("n")
	      ), 
	      "next"), Constant(12))
	  assertTrue(Validate(s))
	 }
}