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
{  def efeDocument = repsep(sentence, rep(NL)) <~ rep(NL) ^^ { case sentList =>
      val ft = new FOLtheory
      ft.addStats(sentList) match
      {  case None => { println("efe2FOLtheoryParser returned:\n" + ft); ft } // successful
         case Some(Tuple2(s,i)) =>
         {  val m = "efe2FOLtheoryParser.efeDocument: my dear, dear beloved, friend, a fatal error occurred, couldn't add one or more statement(s) to the Scala FOLtheory class!"
            println(m)
            throw new RuntimeException(m)
         }
      }
   }

   def sentence = (((((spaces ~> predId) <~ "(") ~ constantId) <~ ")") <~ spaces) ^^ {
      case predId ~ constantId =>
      {  val pred = Predicate(predId, 1)
         PredApp_FOL(pred, List(Constant(constantId)))
      }
   }

   def predId = "B" | "F"
   def constantId = id
}

object Efe2FOLtheoryParser extends Efe2FOLtheoryParser
{
}
