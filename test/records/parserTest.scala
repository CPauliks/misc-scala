package records
import scala.collection.immutable.StringOps;
object ParserTest {
	
	val s = "while(y){n = n.next.last,\ny = 0};\n".stripLineEnd.stripSuffix(";")
	val c = "struct ListNode { value, next }"
	val v = "var n"
	
    def main(args: Array[String]) {
	    //println(s)
	    val p = StatementParser.parseAll(StatementParser.statement, s)
	    val q = StatementParser.parseAll(StatementParser.newVar, v)
	    val r = StatementParser.parseAll(StatementParser.struct, c)
	    if (p.successful){
	      println(p.get)
	      println(q.get)
	      println(r.get)
	      //println(execute(store)(p.get))
	    }
	    else {
	    	println(p) 
	    }
	  }
}