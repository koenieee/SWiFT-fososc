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
import Helpers._
import System.err.println
import org.ocbkc.swift.global.GlobalConstant._
import org.ocbkc.swift.global.TestSettings._
import org.ocbkc.swift.parser._
import scala.util.parsing.combinator.Parsers //{Success, Failure}
import org.ocbkc.swift.logilang.bridge.brone

// BriCo = BridgeConstruction
object sesBriCo extends SessionVar(new BridgeConstructionSessionInfo)

class BridgeConstructionSessionInfo
{  var subjectTf:String = ""
   var predicateTf:String = ""
}

class BridgeConstruction
{  val sesBriCoLR = sesBriCo.is

   val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.
   var errorTrans:String = ""
   var translationTAcontents:String = if(!TEST) "Enter translation here." else sesCoordLR.si.textCTLbyComputer

// { transformed2efe
   def processMenu4EntityBridge(entNLname:String) =
   {  //TODO
   }
   
   def generateMenus4EntityBridge(ns:NodeSeq):NodeSeq =
   {  val constants = sesCoordLR.si.constantsByPlayer match 
                                             {  case Some(consts) => consts
                                                case None         => println("   No constants found in translation player."); Nil
                                             }
   
      constants.flatmap
      {  entNLnames = List("testLoxolop", "testMoekelPower") // <replace>
         val entBridgeConstructionTemplate = chooseTemplate("top", "entBridgeConstructionTemplate", ns)
         bind( "top", entBridgeConstructionTemplate, 
            "constantName"          -> Text(constantIdentifier),
            "naturalLanguageSelect" -> SHtml.select(entNLnames, Empty, processMenu4EntityBridgeSelect(_, constantIdentifier))
         )
      }
   }
// }
   def render(ns: NodeSeq): NodeSeq =
   {  sesCoordLR.URstartBridgeConstruction
      
      def processSubmission() = 
      {  println("BridgeConstruction.processSubmission called")
         
         // check errors on submission here
         
         // sesCoord

         println("   bridge = " + sesCoordLR.si.bridgeCTL2NLplayer)
         
         sesCoord.URstopBridgeConstruction
         
         S.redirectTo("questionAttackRound.html")
      }



*/



      bind( "top", ns, 
            "translation"                 -> Text(sesCoordLR.si.textCTLbyPlayer),
            "entBridgeConstructionMenus"  -> generateMenus4EntityBridge,
            "constructedBridgeCTL2NL"     -> Text(sesCoordLR.si.bridgeCTL2NLplayer),
            "errorInInfo2ConstructBridge" -> errorBridgeWebText,
            "sourceText"                  -> Text(sesCoordLR.si.textNL),
            "submitBt"                    -> SHtml.submit("Submit", processSubmission)
          )
   }
}

}
}

// }
