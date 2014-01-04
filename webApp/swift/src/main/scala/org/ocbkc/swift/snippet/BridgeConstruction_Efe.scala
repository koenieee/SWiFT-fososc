// { BEGIN transform from Una to Efe (already transformed parts are indicated within this block with "tranformed2efe")

package org.ocbkc.swift 
{
package snippet 
{
import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.model.Player
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.LiftHelpers._
import Helpers._
import System.err.println
import org.ocbkc.swift.global.GlobalConstant._
import org.ocbkc.swift.global.TestSettings._
import org.ocbkc.swift.parser._
import scala.util.parsing.combinator.Parsers //{Success, Failure}
import org.ocbkc.swift.logilang.bridge.brone._

// BriCo = BridgeConstruction
package efe
{

object sesBriCo extends SessionVar(new BridgeConstructionSessionInfo)

class BridgeConstructionSessionInfo
{  var subjectTf:String = ""
   var predicateTf:String = ""
}

class BridgeConstruction
{  val sesBricoLR = sesBriCo.is

   val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.
   var errorTrans:String = ""
   var translationTAcontents:String = if(!TEST) "Enter translation here." else sesCoordLR.si.textCTLbyComputer

// { transformed2efe
   def processMenu4EntityBridgeSelect(entNLname:String) =
   {  //TODO
   }
   
   def generateMenus4EntityBridge(ns:NodeSeq):NodeSeq =
   {  val constants = sesCoordLR.si.constantsByPlayer match 
                                             {  case Some(consts) => consts
                                                case None         => println("   No constants found in translation player."); Nil
                                             }
      val entNLnames = sesCoordLR.si.bridgeCTL2NLcomputer.getOrElse{ logAndThrow("bridgeCTL2NLcomputer is None.") }.collect{ case eb:EntityBridge => eb.asInstanceOf[EntityBridge] }.map{ eb => eb.entNLname(0) }
      constants.flatMap
      {  constantIdentifier =>
         {
            val entBridgeConstructionTemplate = chooseTemplate("top", "entBridgeConstructionTemplate", ns)
            log("entBridgeConstructionTemplate = " + entBridgeConstructionTemplate)

            logp( { ns:NodeSeq => "Result of bind ebct with entBridgeConstructionTemplate: " + ns.toString },
                  bind( "ebct", entBridgeConstructionTemplate, 
                        "constantName"    -> Text(constantIdentifier),
                        "entNLselectBox"  -> SHtml.select(entNLnames.map(name => (name,name)), Empty, processMenu4EntityBridgeSelect(_))
               )
            )
         }
      }
   }
// }

   def render(ns: NodeSeq): NodeSeq =
   {  sesCoordLR.URstartBridgeConstruction
      
      def processSubmission() = 
      {  println("efe.BridgeConstruction.processSubmission called")
         
         // check errors on submission here
         
         // sesCoord

         println("   bridge = " + sesCoordLR.si.bridgeCTL2NLplayer)
         
         sesCoord.URstopBridgeConstruction
         
         S.redirectTo("questionAttackRound.html")
      }
// { transformed2efe
      bind( "top", ns,
            "translation"                 -> Text(sesCoordLR.si.textCTLbyPlayer),
            "entBridgeConstructionMenus"  -> generateMenus4EntityBridge(ns),
            "constructedBridgeCTL2NL"     -> Text(sesCoordLR.si.bridgeCTL2NLplayer),
            // "errorInInfo2ConstructBridge" -> errorBridgeWebText,
            "sourceText"                  -> Text(sesCoordLR.si.textNL),
            "submitBt"                    -> SHtml.submit("Submit", processSubmission),
            "entBridgeConstructionTemplate" ->  emptyNode
          )
// }
   }
}

}
}
}

// }
