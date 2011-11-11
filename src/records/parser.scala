package records

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    term ~ "+" ~ term ^^ { case l ~ _ ~ r => Plus(l, r) }
  | term ~ "-" ~ term ^^ { case l ~ _ ~ r => Minus(l, r) }
  | term
  | factor
  )
  def term: Parser[Statement] = (
    factor ~ "*" ~ factor ^^ { case l ~ _ ~ r => Times(l, r) }
  | factor ~ "/" ~ factor ^^ { case l ~ _ ~ r => Div(l, r) }
  | factor
  )
  def factor: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
  | "new" ~> ident ^^ {case c => New(Clazz(c)) }  
  | field
  | ident ^^ { case s => Variable(s) }
  | "(" ~> expr <~ ")" ^^ { case e => e }
  )
  def statement: Parser[Statement] = (
    ident ~ "=" ~ expr ^^ { case s ~ _ ~ r => Assignment(Variable(s), r) }
  | "while" ~ "(" ~> expr ~ ")" ~ statement ^^ { case g ~ _ ~ b => While(g, b) }
  | "{" ~> repsep(statement, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
  | field ~ "=" ~ expr ^^ {case f ~ _ ~ r => Assignment(f, r) }
  )
  def field: Parser[Statement] = (
    ident ~ "." ~ ident ^^ {case v ~ _ ~ f => Selection(Variable(v), f) }
  )
  }
