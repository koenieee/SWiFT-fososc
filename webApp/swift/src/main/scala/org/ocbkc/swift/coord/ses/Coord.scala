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
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.test._
import System._
import org.ocbkc.swift.cores.{TraitGameCore, NotUna}
import org.ocbkc.swift.cores.gameCoreHelperTypes._
import net.liftweb.json._
import java.io._
import net.liftweb.util.Mailer
import net.liftweb.util.Mailer._
import net.liftweb.common.{Box,Empty,Failure,Full}
import org.ocbkc.swift.model._
import _root_.net.liftweb.mapper.By
import org.ocbkc.swift.global.Types._

//import scala.util.parsing.combinator.Parsers._
import org.ocbkc.swift.parser._

/* Conventions:
- Names of classes correspond with design $JN/...
- CTL = Computationally Transparent Language
- NL  = Natural Language
*/
package ses
{

//import Round._


// in trait, make for easy reuse for creating test simulation sessions.
trait CoreTrait
{  var cc: CoreContent = null
   val sesHis = new SessionHistory()
   val gameCore: TraitGameCore = new NotUna(currentPlayer.id.get)

   def currentPlayer:Player

   def URchooseFirstConstitution(constiId:ConstiId) =
   {  val player = currentPlayer
      val consti = Constitution.getById(constiId).get // get is possible because the player is only presented constitutions which HAVE a last release.

      player.firstChosenConstitution(constiId).save
      player.releaseOfFirstChosenConstitution(consti.lastReleaseCommitId.get).save // get is possible because the player is only presented constitutions which HAVE a last release.
      player.timeFirstChosenConstitution(System.currentTimeMillis).save
   }

   def URtranslation:String =  
   {  //round = Trans
      cc = gameCore.initialiseCoreContent
      cc.startTime(SystemWithTesting.currentTimeMillis).save
      cc.startTimeTranslation(cc.startTime.is).save
      cc.textNL
   }

   def URstopTranslation =
   {  cc.stopTimeTranslation(SystemWithTesting.currentTimeMillis).save
      Unit
   }


   def URalgorithmicDefenceStage1:FolnuminquaQuery =
   {  gameCore.algorithmicDefenceGenerator
   }

   def URalgorithmicDefenceStage2:(scala.Boolean, String, String, String) =
   {  val res = gameCore.doAlgorithmicDefence
      // Session completed: store this session for future analysis/score calculations
      // now:Calendar = System.currentTimeMillis()
      cc.stopTime(System.currentTimeMillis).save
      sesHis.coreContents ::= cc      
      cc.serialize // serialize the JSON part
      PlayerCoreContent_join.create.player(currentPlayer).coreContent(cc).save
      res
   }

   def numOfSessionsAfterConstiAccess =
   {  sesHis.totalNumber - OneToStartWith.minSesionsB4access2allConstis
   }

   def accessToConstiGame:Boolean =
   {  numOfSessionsAfterConstiAccess >= 0
   }
}

class Core(/* val player: User, var text: Text,v ar round: Round */) extends CoreTrait
{  println("ses.Core.constructor called")
   
   /* <&y2012.08.08.20:00:20& following MUST be refactored as soon as Mapper framework is understood (see the tryMapperPersistency gitbranch). Now things are only retained during a session, but not accross sessions...> */
   // BEGIN temporary solution for constiSelectionProcedure
   var isFirstTimePlayer:Boolean = true // <&y2012.08.04.19:43:17& set this to true after first session has been completed (or other conditions?)>
   var timeFirstChosenConstitution:Option[Long] = None

   // for coming increment the following will not yet be used.
   var studyHistory:StudyHistory = new StudyHistory

   // END

   override def currentPlayer = Player.currentUser match // <&y2012.08.04.20:16:59& refactor rest of code to use this currentPlayer, instead of doing this again and again....>
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
      var prefix:String = "" // <&y2012.01.10.09:36:56& coulddo: refactor this, because it is also used when making things persistent>
      Player.currentUserId match // <&y2012.06.23.14:41:16& refactor: put currentuserid in session var, and use that throughout the session-code>
      {  case Full(id)  => { prefix = id }
         case _         => { throw new RuntimeException("  No user id found.") }
      }
      println("   reading corecontent objects from database...")
      val ccs = PlayerCoreContent_join.findAll(By(PlayerCoreContent_join.player, currentPlayer)).map{ join => join.coreContent.obj.open_! }

      sesHis.coreContents = ccs
      println("   found " + ccs.length + " CoreContent objects for this player")
   }
   // var sesHis:SessionHistory = new SessionHistory 
   // <&y2012.01.02.23:15:26& initialise SessionHistory object with data made persistant in the past>

   // Communication with User Interface
   // UR = User Request
   // user requests to prepare session
   def URprepare = 
   {  
   }

   override def URchooseFirstConstitution(constiId:ConstiId) =
   {  val player = currentPlayer
      player.firstChosenConstitution(constiId).save
      player.timeFirstChosenConstitution(System.currentTimeMillis).save
   }

   def URchooseFirstConstitution(player:Player, constiId:ConstiId) =
   {  //val player = currentPlayer
      player.firstChosenConstitution(constiId).save
      player.timeFirstChosenConstitution(System.currentTimeMillis).save
   }

/*
   def URtranslation:String =  
   {  round = Trans
      cc = gameCore.initialiseCoreContent
      cc.startTime(System.currentTimeMillis).save
      cc.startTimeTranslation(cc.startTime.is).save
      cc.textNL
   }
*/

   def URstopBridgeConstruction =
   {
   }

   def URquestionAttack:QuestionAndCorrectAnswer = 
   {  gameCore.generateQuestionAndCorrectAnswer
   }
/*
   def URalgorithmicDefenceStage1:FolnuminquaQuery =
   {  gameCore.algorithmicDefenceGenerator
   }

   def URalgorithmicDefenceStage2:(scala.Boolean, String, String, String) =
   {  val res = gameCore.doAlgorithmicDefence
      // Session completed: store this session for future analysis/score calculations
      // now:Calendar = System.currentTimeMillis()
      cc.stopTime(System.currentTimeMillis).save
      sesHis.coreContents ::= cc      
      cc.serialize // serialize the JSON part
      PlayerCoreContent_join.create.player(currentPlayer).coreContent(cc).save
      res
   }
*/
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

   def addFollower(p:Player, c:Constitution) =
   {  val userId = p.userIdAsString
      c.addFollower(p)
   }

   def removeFollower(p:Player, c:Constitution) =
   {  val userId = p.userIdAsString
      c.removeFollower(p)
   }



   object Test
   {  var initConstitutions:Boolean = true
   }
}

// simulation of Core for testing purposes
class CoreSimu(val currentPlayerVal:Player) extends CoreTrait
{  override def currentPlayer = currentPlayerVal

   // the following is a simplification: it skips playing an actual game, but just determines whether the player has succeeded or not.
   def URalgorithmicDefenceSimplified(winSession:Boolean, duration:DurationInMillis) =
   {  val cTM = SystemWithTesting.currentTimeMillis

      cc.startTime(cTM).save
      cc.startTimeTranslation(cTM).save
      cc.stopTime(cTM + duration).save
      cc.stopTimeTranslation(cTM + duration).save

      cc.answerPlayerCorrect(winSession).save
      cc.serialize
      PlayerCoreContent_join.create.player(currentPlayer).coreContent(cc).save
      sesHis.coreContents ::= cc
   }
}


/* Assumptions and conventions regarding UI:
- UI is an abstract layer around the actual UI implementation. Or perhaps better: is a kind of API between the Session coordinator and the implementation of the UI. This allows the specific UI solution (web based, OS-based, etc.) to be changed when required. Only the definition of the methods in the UI object have to be changed, without having to make changes to the ses.Core class.
*/


}
}
