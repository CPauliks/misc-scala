package records

import scala.collection.mutable.StringBuilder

object REPL {
  
  /**
   * Map of variables and their values
   */
  var store = Map[String, Cell]()
  
  /**
   * A Map of all defined records from "struct" lines,
   * used for dependency injection for "new" lines
   */
  var structs = Map[String, Clazz]()
  
  def startREPL() {
    println("Enter commands separated by a semicolon.  Enter help; for more information.")
    var currentCommand = new StringBuilder()
    var currentLine = ""
    
    /**
     * Loop repeatedly waiting for a line to end with a semicolon.
     * Otherwise append the current line to the StringBuilder and ask again
     */
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
    
    /**
     * Look at the most recent command and check for non-executable commands
     * Otherwise parse it
     */
    def process(command: String) {
      command match {
        case "help" => printHelp
        case "quit" => exit(0)
        case "clear" => {
          store = store.empty
          structs = structs.empty
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
      
      /**
       * If the statement is a new record assignment
       * we need to pull out the typedef and and replace it with the actual fields
       * simple dependency injection 
       */
      val modifiedCommand =  {  
        if (command.contains("new")){
          val location = command.indexOf("new")
          val structname = command.substring(location + 3).trim()
          if (structs.contains(structname)) {
            command.replaceAll(structname, structs(structname).fields.toString());
          }
          else {
            ""
          }
	    }
        
	      else {
	    	 command;
	       }
	  }
      
      /**
       * Try parsing the command in all 3 ways. Do whichever one works, and print the result; 
       */
      val struct = StatementParser.parseAll(StatementParser.struct, modifiedCommand)
      val variable = StatementParser.parseAll(StatementParser.newVar, modifiedCommand)
      val executable = StatementParser.parseAll(StatementParser.statement, modifiedCommand)
      
      /**
       * New typedef
       */
      if (struct.successful){
        structs += struct.get
      }
      
      /**
       * New variable creation.
       */
      else if (variable.successful) {
        store += (variable.get -> Cell(0))
      }
      
      /**
       * An executable line.
       */
      else if (executable.successful) {
        val toExecute = executable.get
        if(Validate(toExecute)) {
          println(Execute(store)(toExecute))
        }
      }

      /**
       * None of the above.
       */
      else {
        println("Invalid command")
      }
      
    }
  }
  
  def main(args: Array[String]) {
    startREPL
  }
}