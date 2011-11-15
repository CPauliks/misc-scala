package records
import scala.collection.immutable.StringOps;

/**
 * Testing different parser requirements for the REPL.
 * Specifically dependency injection for new structs
 */
object ParserTest {
  
	val c = "struct ListNode { value, next }"
	val v = "var n"
	
    def main(args: Array[String]) {
	    val q = StatementParser.parseAll(StatementParser.newVar, v)
	    val r = StatementParser.parseAll(StatementParser.struct, c)
	    val b = "n = new " + r.get._2.fields
	    val a = StatementParser.parseAll(StatementParser.statement, b)
	    println(q.get)
	    println(r.get)
	    println(a.get)
	  }
}