/*
Part of SWiFT game (c) Chide Groenouwe

### CONVENTIONS
None so far

*/

package org.ocbkc.swift.parser

import scala.collection.JavaConversions._
import scala.util.parsing.combinator._
import System._
import java.io._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.test.CLIwithFileInput

object Efe2FOLtheoryParserCLI extends CLIwithFileInput
{  def main(args: Array[String])
   {  applyFunctionToFile(Efe2FOLtheoryParser.parseAll(Efe2FOLtheoryParser.efeDocument, _).toString, args(0))
   }
}
// List((((B~'po')~akwasi)~'pc'), (((F~()~kjdkf)~)))
class Efe2FOLtheoryParser extends AlphaGroupParser
{  def efeDocument = repsep(sentence, rep(NL)) <~ rep(NL) ^^ { sentList => sentList.mkString("\n") }
   def sentence = (((((spaces ~> predId) <~ "(") ~ constantId) <~ ")") <~ spaces) ^^ {
      case predId ~ constantId =>
      {  val pred = Predicate(predId, 1)
         PredApp(pred, List(Constant(constantId)))
      }
   }

   def predId = "B" | "F"
   def constantId = id
}

object Efe2FOLtheoryParser extends Efe2FOLtheoryParser
{
}
