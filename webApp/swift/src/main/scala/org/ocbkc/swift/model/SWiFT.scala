package org.ocbkc.swift.model
{
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.cores.EfeChallengeTypes._
import org.ocbkc.swift.parser._
import org.ocbkc.swift.logilang.query.folnuminqua._
import org.ocbkc.swift.logilang.query.plofofa._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.bridge.brone._
import System.err.println
import _root_.net.liftweb.mapper._
import net.liftweb.db.ConnectionIdentifier
import net.liftweb.common._
import java.sql.ResultSet
import org.ocbkc.swift.OCBKC._
import net.liftweb.json._
import java.io._
import org.ocbkc.swift.global.GlobalConstant._

/*
class Source extends Enumeration
{  type Source = Value
   val SOURCE, TRANSLATION = Value   
}
*/

// [&y2012.10.20.21:26:10& never used this up until now. It was intended to hold the current state of the session in Coord.scala. Remove it?]
object Round extends Enumeration
{  type Round = Value
   val NotStarted, Trans, Qattack, Qdefence = Value
}

/* NL  = natural language
   CTL = computationally transparent language
*/

/*
class Answer(var NL:String,  var CTL:String)
{
}

class Question(var NL:String, var CTL:String)
{
}
*/
// Because of the fact that the translation of the computer is the reference translation (the right one), its algorithmic defence yields the CORRECT answer. I.e.: don't read this as an algorithmic defence, but as the correct answer to the question.
/*
class QuestionAttack(var questionNL: String, var algoDefComputer: AlgoDef)
{  
}
*/
// BS: used the SemCons such that they share the same object if they refer to the same CTL.

// question in CTL and algorithm happen to be the same in the current system.
//  @BS: questionCTL must be stated according to the semantic conditions of the CTL used, which is not stored here, so keep track of the context!
/*
class AlgoDef(var semCons: SemCons, var questionCTL: String, var algorithm: String, var answer: Answer)
{
}
*/
/* <& &y2011.12.12.16:46:35& I am not sure: am I always obliged to create a question in CTL, even for a system in which the question and algorithm do not coincide?>
 <& &y2012.05.05.13:47:34& Hmm is this even possible? You always want to formalise the question somehow?>
 <& &y2012.05.05.13:48:51& yes, I think so, if people would for example apply a set of truth preservering transformation, without using an goal-oriented algorithm>

 <& &y2012.05.05.13:49:52& then a new question about this is: how do you show that these transformations lead to a valid answer to the question that was posed???>

 <& &y2012.05.05.13:50:43& also produce an example of this.>

class TextNL(var content:String)
{
}

class TextCTL(var semCons: SemCons, var content:String)

/* Only the sem cons that are added on top of the predefined semcons of the target language */
class SemCons(var bridge2NL:String)
{
}

// SessionBundle bundles the complete core playing material for playing a given session
class SessionBundle(var textNL:TextNL, var textCTL:TextCTL, var qa: QuestionAttack )
{
}

// #BS: only put inforeps that directly represents the subject matter, so no control structures, but only the content produced by the player, etc.
*/

/** @param startTime represents the starttime of the fluency game session.
  * @param startTimeTranslation contains the start time of the translation round. Note that its content may in many cases be equal to startTime.
  */

// <&y2011.12.12.22:11:13& Chide: I can do some refactoring later. Tried to be more generic previously, but this stage turned out to early for too much elegance.>
/** SessionInfo is general: it can hold data of any SWiFT challenge. Therefore, for example, specific parsers (for specific KR languages are not defined here, but in the GameCore.
@todo: &y2014.01.20.16:13:05& holding data for any challenge is currently a problem, because currently some datatypes are fixed (such as textCTLbyPlayer_). Refactor such that it becomes general again, or make this a superclass.
Note: _pf = pure format
  * @param textCTLbyPlayer_ This is a String, and not a RepresentationBundle, because a player can have entered a syntactically incorrect document.
  */

case class SessionInfo( var textNL: String,
                        var questionNL: String,
                        var questionCTLcomputer_rb: Option[EfeQuerySent_rb],
                        var textCTLbyComputer: Option[FOLtheory],
                        var bridgeCTL2NLcomputer: Option[BridgeDoc],
                        var algoDefComputer_rb: Option[EfeQuerySent_rb],
                        var answerComputerCTL: Option[EfeAnswerLangSent],
                        var answerComputerNL: String,
                        var textCTLbyPlayer_ : String, // don't change this one directly.
//                      var constantsByPlayer:Option[List[String]],
//                      var predsByPlayer:Option[List[String]],
                        var bridgeCTL2NLplayer: Option[BridgeDoc],
                        var algoDefPlayer: Option[EfeQuerySent_rb],
                        var answerPlayerCTL: Option[EfeAnswerLangSent],
                        var answerPlayerNL: String, // remove after changing answerPlayerCTL to _rb format
                        var questionRelatedBridgeStats: String,
                        var hurelanRole1NL:String,
                        var hurelanRole2NL:String,
                        var subjectNL:String
                      ) extends LongKeyedMapper[SessionInfo] with IdPK
{  def getSingleton = SessionInfoMetaMapperObj
   // object player extends MappedLongForeignKey(this, Player)
   object gameCoreName extends MappedString(this, 100)
   object startTime extends MappedLong(this)
   object stopTime extends MappedLong(this)
   object startTimeTranslation extends MappedLong(this)
   object stopTimeTranslation extends MappedLong(this)
   object answerPlayerCorrect extends MappedBoolean(this)
   object userId extends MappedLong(this)

   def this() = this("","",None,None,None,None,None,"","",None,None,None,"","","","","")

   var observers:List[TextCTLbyPlayerObserver] = Nil

   def durationTranslation:Option[Long] = // <&y2012.10.28.19:58:50& TODO: refactor with checking if startTimeTranslation is defined?>
   {  println("   durationTranslation called")
      println("   startTimeTranslation.is == " + startTimeTranslation.is)
      println("   stopTimeTranslation.is == " + stopTimeTranslation.is)
      if(startTimeTranslation.is == 0 || stopTimeTranslation.is == 0) None else
      {  val result = Some(stopTimeTranslation.is - startTimeTranslation.is)
         log("   durationTranslation = " + result)
         result
      }
   }

   // None is there is a parse error

   /* out: None: parse error, which can be found in the field parseErrorMsgTextCTLplayer
           Some(...) <&y2012.02.20.15:25:09& finish explanation>
      side effects: also updates constantsByPlayer and predsByPlayer
   */
   /*
   def textCTLbyPlayerCleanFormat:Option[String] =
   {  if(textCTLplayerUpdated4terParsing)
      {  textCTLplayerUpdated4terParsing = false
         if(!ParseTextCTLbyPlayer) None else textCTLbyPlayerCleanFormat_
      }
      else
         textCTLbyPlayerCleanFormat_
   }
   */




   // <&y2012.02.19.11:26:28& coulddo: also make getter and setter for constantsByPlayer predsByPlayer, in style of textCTLbyPlayerCleanFormat>
   
   /* side effects: when parse error, the field parseErrorMsgTextCTLplayer will contain the parse error message. If not, it the latter field will be equal to "".
      out: Boolean: true = parsing was succesful
   */

   /*
   def ParseTextCTLbyPlayerDeprecated:Boolean = 
   {  println("ParseTextCTLbyPlayer called")
      FolminquaParser.parseAll(FolminquaParser.folminquaTheory, textCTLbyPlayer) match 
      {  case FolminquaParser.Success(FolminquaParseResult(cleanform,cons,preds),_) => 
                                                                  {  textCTLbyPlayerCleanFormat_   = Some(cleanform)
                                                                     constantsByPlayer             = Some(cons)
                                                                     predsByPlayer                 = Some(preds)
                                                                     true
                                                                  }
         case f@FolminquaParser.Failure(_,_) => {  println("  parse error: " + f.toString)
                                                   textCTLbyPlayerCleanFormat_   = None
                                                   constantsByPlayer             = None
                                                   predsByPlayer                 = None
                                                   parseErrorMsgTextCTLplayer = f.toString
                                                   false
                                                }
      }
   }
   */


   def serialize =
   {  implicit val formats = Serialization.formats(NoTypeHints)
      var siSer:String = Serialization.write(this)
      println("  sessionInfos serialised to: " + siSer)
      // write session to file with unique name, e.g.: playerName/sessionInfo/

      var prefix:String = userId.toString

      // <&y2012.01.07.17:59:19& MUSTDO: what happens with Player.CurrentUserId, if someone deletes his user account, will the number be reused for another, new, user, if so that would be a problem>
      var outFile = new File(SESSIONINFOOBJECTDIR + "/si" + this.id )
      println("   creating file: " + outFile.getAbsolutePath)
      // <&y2012.01.07.18:15:09& in following I get runtime exception: couldn't find file. Perhaps applicatio doesn't have right? Or perhaps I may not use / in filenames>
      outFile.getParentFile().mkdirs()
      // outFile.createNewFile() // <&y2011.12.23.13:39:00& is this required, or is the file automatically created when trying to write to it?>
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.println(siSer)
      out.close

      val testDeSer:SessionInfo = Serialization.read[SessionInfo](siSer)
   }

   def textCTLbyPlayer_=(t:String) =
   {  observers.foreach{ _.textCTLbyPlayerChanged(textCTLbyPlayer) }
      textCTLbyPlayer_ = t
   }

   def textCTLbyPlayer = textCTLbyPlayer_

   def registerObserverTextCTLbyPlayer(observer:TextCTLbyPlayerObserver) =
   {  if(!observers.contains(observer)) observers ::= observer
   }

   def copyJsonSerializedFieldsFrom(si:SessionInfo) =
   {  this.textNL = si.textNL
      this.questionNL = si.questionNL
      this.questionCTLcomputer_rb = si.questionCTLcomputer_rb
      this.textCTLbyComputer = si.textCTLbyComputer
      this.bridgeCTL2NLcomputer = si.bridgeCTL2NLcomputer
      this.algoDefComputer_rb = si.algoDefComputer_rb
      this.answerComputerCTL = si.answerComputerCTL
      this.answerComputerNL = si.answerComputerNL
      this.textCTLbyPlayer_  = si.textCTLbyPlayer_ 
//    this.constantsByPlayer = si.constantsByPlayer
//    this.predsByPlayer = si.predsByPlayer
      this.bridgeCTL2NLplayer = si.bridgeCTL2NLplayer
      this.algoDefPlayer = si.algoDefPlayer
      this.answerPlayerCTL = si.answerPlayerCTL
      this.answerPlayerNL = si.answerPlayerNL
      this.questionRelatedBridgeStats = si.questionRelatedBridgeStats
      this.hurelanRole1NL = si.hurelanRole1NL
      this.hurelanRole2NL = si.hurelanRole2NL
      this.subjectNL = si.subjectNL
   }
}
/** @todo Move this one to a general lib
  */
trait Observer
{
}

trait TextCTLbyPlayerObserver extends Observer
{  def textCTLbyPlayerChanged(newTextCTL:String)
}

// "join" of player and sessionInfo
case class PlayerSessionInfo_join extends LongKeyedMapper[PlayerSessionInfo_join] with IdPK
{  def getSingleton = PlayerSessionInfo_join
   object player extends MappedLongForeignKey(this, Player)
   object sessionInfo extends MappedLongForeignKey(this, SessionInfoMetaMapperObj)
}

object PlayerSessionInfo_join extends PlayerSessionInfo_join with LongKeyedMetaMapper[PlayerSessionInfo_join]
{  def join(player:Player, si:SessionInfo)
   {  this.create.player(player).sessionInfo(si)
   }
}


object SessionInfoMetaMapperObj extends SessionInfo with LongKeyedMetaMapper[SessionInfo]
{  override def create =
   {  super.create
   }
   

   // possibly confusing: createInstance is used when READING info. that was made persistent... 
   override def createInstance(dbId: ConnectionIdentifier, rs : ResultSet, mapFuncs: List[Box[(ResultSet,Int,SessionInfo) => Unit]]) : SessionInfo =
   {  val si = super.createInstance(dbId, rs, mapFuncs)
      val siFileName = SESSIONINFOOBJECTDIR + "/si" + si.id
      val siFile  = new File( siFileName )
      implicit val formats = Serialization.formats(NoTypeHints) // <? &y2012.01.10.20:11:00& is this a 'closure' in action? It is namely used in the following function
      val in:BufferedReader   = new BufferedReader(new FileReader(siFile))
      var inStr:String        = in.readLine
      if( inStr == null) throw new RuntimeException("   Problems reading " + siFileName )
      val siLoc:SessionInfo   = Serialization.read[SessionInfo](inStr)
      si.copyJsonSerializedFieldsFrom(siLoc)

      in.close 

      si
   }
}

// <&y2012.01.08.18:25:04& or should this object belong to Coord?>
class SessionHistory
{  var sessionInfos:List[SessionInfo] = Nil
   def correctCcs = sessionInfos.filter( si => si.answerPlayerCorrect )

   def totalNumber = sessionInfos.length
   def numberCorrect = sessionInfos.count( si => si.answerPlayerCorrect )

   // import Ordering.LongOrdering
   object OrderingOnDurationTranslation extends Ordering[SessionInfo]
   {  def compare(si1:SessionInfo, si2:SessionInfo) = scala.math.Ordering.Long.compare( si1.durationTranslation.get, si2.durationTranslation.get  )
   }   

   // <&y2012.01.11.23:37:48& couldo refactor: calculate this directly using a map to duration first, and then selecting the min>
   def shortestTranslationTime:Option[Double] = sessionWithShortestDurationAndCorrectTranslation match 
      {  case None => None 
         case Some(si) => Some(si.durationTranslation.get.toDouble/1000)
      }

   def sessionWithShortestDurationAndCorrectTranslation:Option[SessionInfo] = correctCcs match 
      {  case Nil => None 
         case _   => Some(correctCcs.min( OrderingOnDurationTranslation  ))
      }

   def percentageCorrect:Option[Double] =
   {  if( totalNumber != 0) Some(numberCorrect.toDouble/totalNumber.toDouble * 100.0) else None
   }
}
}
