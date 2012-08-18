/* <&y2012.04.09.13:58:55& for all scala code: change appending to Lists, because this is highly inefficient (use a ListBuffer instead)>/
(  importance = 9
)
*/
package org.ocbkc.swift.coord
{  
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.model._
import org.ocbkc.swift.general._
import org.ocbkc.swift.global.TestSettings._
import org.ocbkc.swift.OCBKC._
import System._
import org.ocbkc.swift.cores.{TraitGameCore, NotUna}
import org.ocbkc.swift.cores.gameCoreHelperTypes._
import net.liftweb.json._
import java.io._
import net.liftweb.util.Mailer
import net.liftweb.util.Mailer._
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
class Core(/* val player: User, var text: Text,*/ var round: Round)
{  println("ses.Core.constructor called")
   val gameCore: TraitGameCore = new NotUna()
   var cc: CoreContent = null
   val sesHis = new SessionHistory()
   
   /* <&y2012.08.08.20:00:20& following MUST be refactored as soon as Mapper framework is understood (see the tryMapperPersistency gitbranch). Now things are only retained during a session, but not accross sessions...> */
   // BEGIN temporary solution for constiSelectionProcedure
   var isFirstTimePlayer:Boolean = true // <&y2012.08.04.19:43:17& set this to true after first session has been completed (or other conditions?)>
   val constiSelectionProcedure = OneToStartWith // embodies which procedure is followed to let people study the constitutions, e.g.: allow them to choose only one constitution and study it the first time they play, or allow any constitution to be consulted at any moment etc. etc.
   var firstChosenConstitution:Option[Constitution] = None // <&y2012.08.03.10:20:25& perhaps in future refactor, or supplement, with more generic, row of chosen constitutions>
   var timeFirstChosenConstitution:Option[Long] = None

   // for coming increment the following will not yet be used.
   var studyHistory:StudyHistory = new StudyHistory

   // END


   def currentPlayer = Player.currentUser match // <&y2012.08.04.20:16:59& refactor rest of code to use this currentPlayer, instead of doing this again and again....>
   {  case Full(player) => player
      case _            => 
      {  println("   ERROR: I'm afraid no player is logged in..."); throw new RuntimeException("   ERROR: I'm afraid no player is logged in...") // there should always be a player if a Coord object is being created.
      }
   } // <&y2012.08.04.19:33:00& perhaps make it so that also this rewrite URL becomes visible in the browser URL input line>

   initialise

   // ...Touched: the user has edited this field in this session at least once.

   // object for holding state of the session
   object State
   /* var translationTouched:Boolean, var bridgeTouched:Boolean) */

   private def initialise = {  // load sessionhistory data from disk for this user (persistency info).
      println("Core.initialise called")
      // Read output of the Clean command
      var prefix:String = "" // <&y2012.01.10.09:36:56& coulddo: refactor this, because it is also used when making things persistent>
      Player.currentUserId match // <&y2012.06.23.14:41:16& refactor: put currentuserid in session var, and use that throughout the session-code>
      {  case Full(id)  => { prefix = id }
         case _         => { throw new RuntimeException("  No user id found.") }
      }

      if( prefix.toInt != 1 || !CLEARSESHISPLAYER1AFTEREACHLOGIN ) // <&y2012.08.13.23:44:03& TODO remove this if, as soon as persistency of constiSelectionProcedure is realised. Now only intended for testing purposes: each time user 1 logs in again, his game-session history is erased so that the constiSelectionProcedure can be tested.>
      {
      // cc stands for CoreContent
      // <&y2012.06.03.00:54:10& SHOULDDO: refactor, move this to CoreContent singleton object in the same way as Constitution>
      val ccRootDir  = new File("users/userId" + prefix + "/CoreContent/")
      println("   directory to read in serialized corecontents: " +  ccRootDir.getAbsolutePath)
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

   // call this when const has been updated, and you want to notify all followers.
   def mailFollowersUpdate(const: Constitution, body:String ) =
   {  def sendupdatemail(followerId:Int) =
      {  println("sendupdatemail called")
         val follower = Player.find(followerId.toString) match
         {  case Full(player)  => player
            case _             => throw new RuntimeException("Player with id " + followerId + " not found.")
         }
         println("   follower id = " + followerId)
         println("   follower email = " + follower.email.get)
         Mailer.sendMail(From("cg@xs4all.nl"), Subject("Constitution " + const.id + " has been updated..."), To(follower.email.get), new PlainMailBodyType(body))
         println("   mail sent!")
      }

      const.followers.map( sendupdatemail )
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
