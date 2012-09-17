package org.ocbkc.swift.model
{
import org.ocbkc.swift.parser._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang._
import System.err.println
/*
class Source extends Enumeration
{  type Source = Value
   val SOURCE, TRANSLATION = Value   
}
*/
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

// <&y2011.12.12.22:11:13& Chide: I can do some refactoring later. Tried to be more generic previously, but this stage turned out to early for too much elegance.>
case class CoreContent( var textNL: String,
                        var questionNL: String,
                        var questionCTLcomputer: String,
                        var textCTLbyComputer: String,
                        var bridgeCTL2NLcomputer: String,
                        var algoDefComputer: String,
                        var answerComputerCTL: String,
                        var answerComputerNL: String,
                        var textCTLbyPlayer_ : String,
                        var constantsByPlayer:Option[List[String]],
                        var predsByPlayer:Option[List[String]],
                        var bridgeCTL2NLplayer: String,
                        var algoDefPlayer: Option[FolnuminquaQuery],
                        var answerPlayerCTL: String,
                        var answerPlayerNL: String,
                        var questionRelatedBridgeStats: String,
                        var hurelanRole1NL:String,
                        var hurelanRole2NL:String,
                        var subjectNL:String,
                      )

)
{  object startTime extends MappedLong(this)
   object stopTime extends MappedLong(this)
   object startTimeTranslation extends MappedLong(this)
   object stopTimeTranslation extends MappedLong(this)
   object answerPlayerCorrect extends MappedBoolean(this)

   // delete: def this() = this(new TimingInfo(),"","","","","","","","","",None,None,"",None,"","","","","","",false)
   def durationTranslation:Option[Long] = 
   {  if(startTimeTranslation.is == 0 || stopTimeTranslation.is == 0) None else Some(stopTimeTranslation.is - startTimeTranslation.is)
   }

   var textCTLplayerUpdated4terParsing = false
   def textCTLbyPlayer = textCTLbyPlayer_.is
   def textCTLbyPlayer_=(t:String) = { textCTLplayerUpdated4terParsing = true; textCTLbyPlayer_(t) }

   //var textCTLbyPlayerCleanFormat_ :Option[String] = None
   var textCTLbyPlayerScalaFormat_ :Option[FOLtheory] = None
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
   def textCTLbyPlayerScalaFormat:Option[FOLtheory] =
   {  if(textCTLplayerUpdated4terParsing)
      {  textCTLplayerUpdated4terParsing = false
         if(!ParseTextCTLbyPlayer) None else textCTLbyPlayerScalaFormat_
      }
      else
         textCTLbyPlayerScalaFormat_
   }


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
   def ParseTextCTLbyPlayer:Boolean = 
   {  println("ParseTextCTLbyPlayer called")
      textCTLplayerUpdated4terParsing = false
      if(textCTLbyPlayer.equals("")) parseWarningMsgTxtCTLplayer = "Warning: empty file." else parseWarningMsgTxtCTLplayer = ""  // <&y2012.05.19.20:27:13& replace with regex for visually empty file (thus file with only space characters, like space, newline, tab etc.>

      Folminqua2FOLtheoryParser.parseAll(Folminqua2FOLtheoryParser.folminquaTheory, textCTLbyPlayer) match
         {  case Folminqua2FOLtheoryParser.Success(ftl,_)         => {  textCTLbyPlayerScalaFormat_ = Some(ftl)
                                                                        constantsByPlayer           = Some(ftl.constants.map({ case Constant(id) => id }))
                                                                        predsByPlayer               = Some(ftl.predicates.map(pred => pred.name))
                                                                        parseErrorMsgTextCTLplayer = ""
                                                                        true
                                                                     }
            case failMsg@Folminqua2FOLtheoryParser.Failure(_,_)   => {  textCTLbyPlayerScalaFormat_   = None
                                                                        constantsByPlayer             = None
                                                                        predsByPlayer                 = None
                                                                        println("  parse error: " + failMsg.toString)
                                                                        parseErrorMsgTextCTLplayer = failMsg.toString
                                                                        false 
                                                                     }
         }
   }

   var parseErrorMsgTextCTLplayer:String = ""
   var parseWarningMsgTxtCTLplayer:String = ""
}

// <&y2012.01.08.18:25:04& or should this object belong to Coord?>
class SessionHistory
{  var coreContents:List[CoreContent] = Nil
   def correctCcs = coreContents.filter( cc => cc.answerPlayerCorrect )

   def totalNumber = coreContents.length
   def numberCorrect = coreContents.count( cc => cc.answerPlayerCorrect )

   // import Ordering.LongOrdering
   object OrderingOnDurationTranslation extends Ordering[CoreContent]
   {  def compare(cc1:CoreContent, cc2:CoreContent) = scala.math.Ordering.Long.compare( cc1.durationTranslation.get, cc2.durationTranslation.get  )
   }   

   // <&y2012.01.11.23:37:48& couldo refactor: calculate this directly using a map to duration first, and then selecting the min>
   def shortestTranslationTime:Option[Double] = sessionWithShortestDurationAndCorrectTranslation match 
      {  case None => None 
         case Some(cc) => Some(cc.durationTranslation.get.toDouble/1000)
      }

   def sessionWithShortestDurationAndCorrectTranslation:Option[CoreContent] = correctCcs match 
      {  case Nil => None 
         case _   => Some(correctCcs.min( OrderingOnDurationTranslation  ))
      }

   def percentageCorrect:Option[Double] =
   {  if( totalNumber != 0) Some(numberCorrect.toDouble/totalNumber.toDouble * 100.0) else None
   }
}
}
