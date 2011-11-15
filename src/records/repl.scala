package records

import scala.collection.mutable.Map
import scala.collection.mutable.StringBuilder

object REPL {
  
  /**
   * Map of variables and their values
   */
  var store = Map[String, Cell]()
  
  /**
   * A Map of all defined records from "struct" lines,
   * used to replace Clazz names with field names
   */
  var structs = Map[String, Clazz]()
  
  def startREPL() {
    println("Enter commands separated by a semicolon.  Enter help; for more information.")
    var currentCommand = new StringBuilder()
    var currentLine = ""
    while(true) {
      println("Awaiting next command...")
      currentLine = readLine().stripLineEnd
      if (currentLine.endsWith(";")){
        currentCommand.append(currentLine.stripSuffix(";"))
        process(currentCommand.toString())
        currentCommand.clear()
      }
      else {
        println("Incomplete command")
        currentCommand.append(currentLine)
      }
    }
    
    def process(command: String) {
      command match {
        case "help" => printHelp
        case "quit" => exit(0)
        case "clear" => {
          store.clear()
          structs.clear()
          println("Store has been emptied!")
        }
        case "dump" => println(store)
        case _ => parseCommand(command)
      }
    }
    
    def printHelp {
      println ("Enter all commands in C-style syntax ending with a semicolon.  Special commands:")
      println ("Type 'help;' to see this menu.  Type 'dump;' to display the memory store")
      println ("Type 'clear;' to wipe the memory store.  Type 'quit;' to exit.") 
    }
    
    def parseCommand(command: String) {
      val struct = StatementParser.parseAll(StatementParser.struct, command)
      val variable = StatementParser.parseAll(StatementParser.newVar, command)
      val executable = StatementParser.parseAll(StatementParser.statement, command)
      if (struct.successful){
        structs += struct.get
      }
      else if (variable.successful) {
        store += (variable.get -> Cell(0))
      }
      else if (executable.successful) {
        val toExecute = executable.get
        if(Validate(toExecute)) {
          println(Execute(store)(toExecute))
        }
      }
      else {
        println(executable.get)
      }
      
    }
  }
  
  def main(args: Array[String]) {
    startREPL
  }
}