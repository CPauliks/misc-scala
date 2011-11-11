package records

object ParserTest {
	
	val s = "while (n) {s = s + n.value, n = n.next}"
	
    def main(args: Array[String]) {
	    println(s)
	    val p = StatementParser.parseAll(StatementParser.statement, s)
	    println(p)
	  }
}