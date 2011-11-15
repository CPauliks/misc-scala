package records

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    term ~ "+" ~ term ^^ { case l ~ _ ~ r => Plus(l, r) }
  | term ~ "-" ~ term ^^ { case l ~ _ ~ r => Minus(l, r) }
  | term
  | factor
  | statement
  )
  def term: Parser[Statement] = (
    factor ~ "*" ~ factor ^^ { case l ~ _ ~ r => Times(l, r) }
  | factor ~ "/" ~ factor ^^ { case l ~ _ ~ r => Div(l, r) }
  | factor
  )
  def factor: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
  | "new List(" ~> repsep(ident, ",") <~ ")" ^^ {case fields => New(Clazz(fields: _*)) }  
  | field
  | ident ^^ { case s => Variable(s) }
  | "(" ~> expr <~ ")" ^^ { case e => e }
  )
  def statement: Parser[Statement] = (
    ident ~ "=" ~ expr ^^ { case s ~ _ ~ r => Assignment(Variable(s), r) }
  | "while" ~ "(" ~> expr ~ ")" ~ statement ^^ { case g ~ _ ~ b => While(g, b) }
  | "{" ~> repsep(statement, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
  | field ~ "=" ~ expr ^^ { case f ~ _ ~ r => Assignment(f, r) }
  )
  /**
   * Allows for nested fields up to one level to be displayed as a single assignment.
   */
  def field: Parser[Statement] = (
    ident ~ "." ~ field ^^ { case v ~ _ ~ Selection(Variable(r),f) => Selection(Selection(Variable(v), r),f) }
  | ident ~ "." ~ ident ^^ { case v ~ _ ~ f => Selection(Variable(v), f) }
  )
  
  /**
   * Parser for typedef (struct) lines
   */
  def struct: Parser[(String, Clazz)] = (
    "struct" ~> ident ~ "{" ~ repsep(ident, ",") <~ "}" ^^ { case record ~ _ ~ fields => Tuple2(record, Clazz(fields: _*))}    
  )
  
  /**
   * Parser for new variable (var) lines
   */
  def newVar: Parser[String] = (
    "var" ~> ident ^^  {case v => v}
  )
  }
