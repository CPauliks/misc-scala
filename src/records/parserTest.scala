package records

object ParserTest {
	
	val s = "n = n.next.last"
	
    def main(args: Array[String]) {
	    println(s)
	    val p = StatementParser.parseAll(StatementParser.statement, s)
	    println(p)
	  }
}