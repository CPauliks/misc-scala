package records
import scala.collection.immutable.StringOps;
object ParserTest {
	
	val s = "while(y){n = n.next.last,\ny = 0};\n".stripLineEnd.stripSuffix(";")
	
    def main(args: Array[String]) {
	    println(s)
	    val p = StatementParser.parseAll(StatementParser.statement, s)
	    println(p)
	  }
}