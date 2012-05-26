/* <&y2012.04.09.13:58:55& for all scala code: change appending to Lists, because this is highly inefficient (use a ListBuffer instead)>/
(  importance = 9
)
*/
package org.ocbkc.swift.coord
{  
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.model._
import System._
import org.ocbkc.swift.cores.{TraitGameCore, NotUna}
import org.ocbkc.swift.cores.gameCoreHelperTypes._
import net.liftweb.json._
import java.io._
import net.liftweb.common.{Box,Empty,Failure,Full}
//import scala.util.parsing.combinator.Parsers._
import org.ocbkc.swift.parser._

/* Conventions:
- Names of classes correspond with design $JN/...
- CTL = Computationally Transparent Language
- NL  = Natural Language
*/
package ses
{

import Round._
class Core(/* val player: User */var text: Text, var round: Round)
{  val gameCore: TraitGameCore = new NotUna()
   var cc: CoreContent = null
   val sesHis = new SessionHistory()
   initialise

   // ...Touched: the user has edited this field in this session at least once.

   // object for holding state of the session
   object State
   /* var translationTouched:Boolean, var bridgeTouched:Boolean) */

   private def initialise = {  // load sessionhistory data from disk for this user (persistency info).
      // Read output of the Clean command
      var prefix:String = "" // <&y2012.01.10.09:36:56& coulddo: refactor this, because it is also used when making things persistant>
      Player.currentUserId match
      {  case Full(id)  => { prefix = id }
         case _         => { throw new RuntimeException("  No user id found.") }
      }
      // cc stands for CoreContent

      val ccRootDir  = new File("users/userId" + prefix + "/CoreContent/")
      val ccFiles = ccRootDir.listFiles()
      if( ccFiles != null && ccFiles.length != 0 )
      {  implicit val formats = Serialization.formats(NoTypeHints) // <? &y2012.01.10.20:11:00& is this a 'closure' in action? It is namely used in the following function>
         def readCc(file:File):Unit =
         {  val in:BufferedReader   = new BufferedReader(new FileReader(file))
            var inStr:String        = in.readLine()
            val ccLoc:CoreContent   = Serialization.read[CoreContent](inStr)
            sesHis.coreContents ::= ccLoc
         }

         ccFiles map readCc
      }
   }
   // var sesHis:SessionHistory = new SessionHistory 
   // <&y2012.01.02.23:15:26& initialise SessionHistory object with data made persistant in the past>

   // Communication with User Interface
   // UR = User Request
   // user requests to prepare session
   def URprepare = 
   {  
   }

   def URtranslation:String =  
   {  round = Trans
      cc = gameCore.initialiseCoreContent
      cc.timingInfo.startTime = System.currentTimeMillis()
      cc.timingInfo.startTimeTranslation = cc.timingInfo.startTime
      cc.textNL
   }

   def URstopTranslation =
   {  cc.timingInfo.stopTimeTranslation  = System.currentTimeMillis()
   }

   def URstopBridgeConstruction =
   {
   }

   def URquestionAttack:QuestionAndCorrectAnswer = 
   {  gameCore.generateQuestionAndCorrectAnswer
   }

   def URalgorithmicDefenceStage1:FolnuminquaQuery =
   {  gameCore.algorithmicDefenceGenerator
   }

   def URalgorithmicDefenceStage2:(scala.Boolean, String, String, String) =
   {  val res = gameCore.doAlgorithmicDefence
      // Session completed: store this session for future analysis/score calculations
      // now:Calendar = System.currentTimeMillis()
      cc.timingInfo.stopTime = System.currentTimeMillis()
      sesHis.coreContents ::= cc
      // serialize cc
      implicit val formats = Serialization.formats(NoTypeHints)
      var ccSer:String = Serialization.write(cc)
      err.println("  corecontents serialised to: " + ccSer)
      // write session to file with unique name, e.g.: playerName/corecontent/
      var prefix:String = ""
      Player.currentUserId match
      {  case Full(id)  => { prefix = id }
         case _         => { throw new RuntimeException("  No user id found.") }
      }

      // <&y2012.01.07.17:59:19& MUSTDO: what happens with Player.CurrentUserId, if someone deletes his user account, will the number be reused for another, new, user, if so that would be a problem>
      var outFile = new File("users/userId" + prefix + "/CoreContent/" + cc.timingInfo.startTime)
      err.println("   creating file: " + outFile.getAbsolutePath)
      // <&y2012.01.07.18:15:09& in following I get runtime exception: couldn't find file. Perhaps applicatio doesn't have right? Or perhaps I may not use / in filenames>
      outFile.getParentFile().mkdirs()
      // outFile.createNewFile() // <&y2011.12.23.13:39:00& is this required, or is the file automatically created when trying to write to it?>
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.println(ccSer)
      out.close()

      val testDeSer:CoreContent = Serialization.read[CoreContent](ccSer)
      
      res
   }
// <&y2012.02.21.19:22:56& refactor by using built-in parser of CoreContent.?>
   def testSyntaxTranslation:String = 
   {  cc.ParseTextCTLbyPlayer
      val warn = cc.parseWarningMsgTxtCTLplayer
      if(!warn.equals("")) warn else cc.parseErrorMsgTextCTLplayer
   }

   def testSyntaxBridge = 
   {  import scala.util.parsing.combinator.Parsers
      if( cc.bridgeCTL2NLplayer == "" ) 
         None
      else
         Some(HurelanBridge.parseAll(HurelanBridge.bridge, cc.bridgeCTL2NLplayer))
   }

   object Test
   {  var initConstitutions:Boolean = true
   }
}


/* Assumptions and conventions regarding UI:
- UI is an abstract layer around the actual UI implementation. Or perhaps better: is a kind of API between the Session coordinator and the implementation of the UI. This allows the specific UI solution (web based, OS-based, etc.) to be changed when required. Only the definition of the methods in the UI object have to be changed, without having to make changes to the ses.Core class.
*/


}
}
