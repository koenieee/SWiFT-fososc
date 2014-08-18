/**
  * Rename "core" to something as "fluencychallenge"
  */
package org.ocbkc.swift.cores
{
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.fofa._
import org.ocbkc.swift.logilang.fofa.translator._
import org.ocbkc.swift.logilang.fofa
import org.ocbkc.swift.logilang.efe._
import scala.util.Random
import org.ocbkc.generic.random.RandomExtras
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.Types._
import org.ocbkc.swift.natlang
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang.query.folnuminqua._
import org.ocbkc.swift.logilang.query.plofofa._
import org.ocbkc.swift.logilang.query.plofofa
import org.ocbkc.swift.logilang.query.plofofa.translator._
import org.ocbkc.swift.logilang.bridge.brone._
import org.ocbkc.swift.reas._
import org.ocbkc.swift.reas
import org.ocbkc.swift.model._
import org.ocbkc.swift.global.GlobalConstant._
import org.ocbkc.swift.coord.ses._
import System._
import scala.sys.process._
import scala.util.matching._
import scala.util.matching.Regex._
import java.io._
//import java.lang._
import org.ocbkc.swift.parser._
import org.ocbkc.swift.trans._
import net.liftweb.json._
import net.liftweb.json.ext._
import scala.util.parsing.combinator.Parsers
import org.ocbkc.swift.logilang.bridge.brone.translators._
import org.ocbkc.swift.test._



/* Conventions:
   - Names of classes correspond with design $JN/...

   BS:
   - Inforepresentations which form part of the *state* of the gamecore, are stored as property (and not passed as arguments to methods who are using them).
   - For convenience, however, methods do return the requested inforeps, even if they can also be read from the gamecore state. 
 */

import Round._

// specifically intended for 'ad hoc' return types.
package gameCoreHelperTypes
{  class QuestionAndCorrectAnswer(questionNL:String, questionCTLcomputer:String)
}

import gameCoreHelperTypes._

trait TraitGameCore[QuerySent__TP/* __TP = Type Parameter */ <: QuerySent, AnswerLangSent__TP <: CTLsent]
{  // SHOULDDO: how to initialize a val of this trait in a subclass of this trait? (would like to do that with playerId)
      val gameCoreName:String
      val playerId:Long
      var si:SessionInfo
      var parseWarningMsgTxtCTLplayer:String = ""

      var parseErrorMsgTextCTLplayer:String = ""

      def initialiseSessionInfo:SessionInfo = // <does this really belong here?>
      {  si = new SessionInfo
         si.gameCoreName(gameCoreName).save
            si.userId(playerId).save
            si
      }
      def generateText:String
      def algorithmicDefenceGenerator:QuerySent__TP
      def generateQuestionAndCorrectAnswer:QuestionAndCorrectAnswer

   case class AlgorithmicDefenceResult(answerCorrect:Boolean, answerPlayerNL:String, reasonerComment:String, answerPlayerCTL:AnswerLangSent__TP)
                                                     def doAlgorithmicDefence:AlgorithmicDefenceResult
                                                        // <&y2011.11.17.18:49:46& or should I change the type of text and trans to the Text class etc. see model package.>
}

/*
   <&y2011.12.12.16:16:58& refactor: either work with:
   - providing input info by setting properties of the gamecore class, and then calling the methodwhich uses them
   - provide all inputs through the parameters of the method...
   Don't mix, that is confusing.
   Or perhaps: find out a "design rule of thumb" which allows mixing them in a non-confusing way.
   >
 */

/** Naming conventions:
 * These aliases are intended to make the code more readable by providing the role the type plays in this challenge. E.g. the query language sentence of the Efe challenge is PlofofaPat.
 */
object EfeChallengeTypes
{  type EfeQuerySent       = PlofofaPat
   type EfeQuerySent_rb    = PlofofaPat_rb
   val EfeQuerySent_rb     = PlofofaPat_rb
   type EfeAnswerLangSent  = FofaSent // change to _rb version as soon as implemented.
   type EfeKRdoc           = FOLtheory
   type EfeKRdoc_rb        = EfeDoc_rb
   val EfeKRdoc_rb         = EfeDoc_rb
}


// helper class for return type of generateQuestionAndCorrectAnswer
/** Specifications can be found in the following documents in the same git repository:
 * TODOdefaultCommandForOpening ./specs/NotUna_FluencyGame_TODO_SCAN.pdf 
  */

import EfeChallengeTypes._

/** @todo &y2014.02.01.18:22:29& why not make EfeLang a singleton object?
  */

class EfeLang(val playerIdInit:Long) extends TraitGameCore[EfeQuerySent_rb, EfeAnswerLangSent/* change to _rb when available */]
{  log("Constructor EfeLang called")
   val gameCoreName = "efe"
   var si:SessionInfo = null
   val playerId = playerIdInit
   
   case class TranslationProblem(textCTL:FOLtheory, textNL: String, bridge:BridgeDoc, algoDef_rb:EfeQuerySent_rb, answerCTL:EfeAnswerLangSent)

   /** EfeLang is not equal to FOL, but FOL is just used, with some predefined predicates. The "right" solution would be to define a new language in a separate package. This is a quick solution. Just throw a FOLtheory through this method, and it will add the predefined predicates. Moreover it will return the BridgeDoc.
     */
   def initialiseEfeDoc(efeDoc:FOLtheory):(BridgeDoc, Predicate, Predicate) =
   {  val fastPredicate       = efeDoc.gocPredicate("F", 1).get
      val fastPredicateBridge = PredicateBridgeSent("F", List("fast"))
      val bigPredicate        = efeDoc.gocPredicate("B", 1).get
      val bigPredicateBridge  = PredicateBridgeSent("B", List("big"))
      val bd                  = new BridgeDoc
      bd.bridgeSents          ++= List(fastPredicateBridge, bigPredicateBridge)
      (bd, fastPredicate, bigPredicate)
   }

   def generateTranslationProblem:TranslationProblem =
   {  log("generateTranslationProblem started")
      import RandomExtras.pickRandomElementFromList
      val rg = new Random()
      val generatedEfeDoc = new FOLtheory
   // { refactor the following not using vars
 
      var textNL:String = ""  // refactor not using vars
      var algoDef_rb_option:Option[EfeQuerySent_rb] = None
      var answerCTL_option:Option[EfeAnswerLangSent] = None
      
   // }
      val (bridgeDoc, fastPredicate, bigPredicate) = initialiseEfeDoc(generatedEfeDoc)

      // in this increment: pick one random translation problem of efe (in future increment they will be combined into one TextNL document).

      val numberOfSubTranslationProblems = 2 // &y2014.02.23.12:56:50& Note: not needed in future increment with more questions in one algorithmic defence. Then SWiFT will work with an enumeration like datatype to indicate these.

      rg.nextInt(2) match
      {  case 0 =>
         {  // create translation problem 1 predicate applied to 1 constant. @todo: &y2014.02.22.16:15:33& use the refactored methods
     
            // @todo shoulddo: refactor using the new "outfactored" methods
            val randomPersonNLname = pickRandomElementFromList( natlang.info.Info.properNamesForPersons, rg ).get

            val pred = randomPredicate
            val randomPersonCTLname = "ctlName" + randomPersonNLname
            val randomPersonConstant = generatedEfeDoc.gocConstant(randomPersonCTLname)
            val entityBridge = EntityBridgeSent(randomPersonCTLname, List(randomPersonNLname))

            bridgeDoc.bridgeSents ++= List(entityBridge)
            generatedEfeDoc.addPredApp(PredApp_FOL(pred, List(randomPersonConstant)))      
            
            algoDef_rb_option = Some(EfeQuerySent_rb(MostInfo(PatVar("s"), plofofa.Forall(Var("x"), PatVar("s"), PredApp_Plofofa(pred, List(Var("x")))))))
            answerCTL_option = Some(fofa.Forall(Var("x"), List(randomPersonConstant), PredApp_Fofa(pred, List(Var("x")))))
            textNL = TranslateFOLtheory2NL.NLstyleStraight(generatedEfeDoc, bridgeDoc)(0)
         }
      // generate translation subproblem 1
         case 1 =>
         {  val (constants, pred) = addKR4distributedPredicateInNL(generatedEfeDoc, bridgeDoc)
            textNL = TranslateFOLtheory2NL.NLstyleDistributePredicateUnchecked(constants, pred, bridgeDoc)
            algoDef_rb_option = Some(EfeQuerySent_rb(MostInfo(PatVar("s"), plofofa.Forall(Var("x"), PatVar("s"), PredApp_Plofofa(pred, List(Var("x")))))))
            answerCTL_option = Some(fofa.Forall(Var("x"), constants, PredApp_Fofa(pred, List(Var("x")))))
         }
      }

      def randomPredicate =
      {  pickRandomElementFromList( List(bigPredicate, fastPredicate), rg ).get
      }
      
      /** Generates and adds a contant to efeDoc which does not yet exist in efeDoc.
        */
      def generateRandomEntity(efeDoc:EfeKRdoc, bridgeDoc:BridgeDoc):Constant =
      {  val randomPersonNLname     = pickRandomElementFromList( natlang.info.Info.properNamesForPersons diff bridgeDoc.entNLnames, rg ).get
         // By Mussie?:
         log("WARNING: exception can occur here when there are not sufficient properNamesForPersons left... Fix this.")

         val randomPersonCTLname    = "ctlName" + randomPersonNLname
         val randomPersonConstant   = efeDoc.gocConstant(randomPersonCTLname)
         val entityBridge = EntityBridgeSent(randomPersonCTLname, List(randomPersonNLname))

         bridgeDoc.bridgeSents ++= List(entityBridge)

         randomPersonConstant
      }
   
       /** Generates and adds contants to efeD  oc which does not yet exist in efeDoc.
        */    
      def generateRandomEntityList(efeDoc:EfeKRdoc, bridgeDoc:BridgeDoc, n:Int):List[Constant] =
      {  List.tabulate(n)(n => generateRandomEntity(efeDoc, bridgeDoc))
      }

      /** Extends efeDoc with some constants and a predicate application, which allow translation into a NL with a ditributed NL-predicate.
          @returns (the constants to which the predicate is applied, and the predicate itself)
          Assumes the predicate to be one-place, otherwise a runtime error will occur. @todo could build in check against this.
        */
      def addKR4distributedPredicateInNL(efeDoc:EfeKRdoc, bridgeDoc:BridgeDoc):(List[Constant], Predicate) =
      {  val minimalEntities = 3
         val maximumEntities = 5
         val numberOfEntities = RandomExtras.nextBetween(rg, minimalEntities, maximumEntities)
         val constants = generateRandomEntityList(efeDoc, bridgeDoc, numberOfEntities)
         val ranPred = randomPredicate

         constants.foreach{ c:Constant => efeDoc.addPredApp(PredApp_FOL(ranPred, List(c))) }

         (constants, ranPred)
      }

   logp( { edab:TranslationProblem => "   Generated TranslationProblem = " + edab } , TranslationProblem(generatedEfeDoc, textNL, bridgeDoc, algoDef_rb_option.get, answerCTL_option.get))
   }

   override def initialiseSessionInfo:SessionInfo =
   {  super.initialiseSessionInfo
      val tp = generateTranslationProblem
      si.textCTLbyComputer = Some(tp.textCTL)
      si.textNL = tp.textNL
      // move to generateTranslationProblem si.textNL = Translation.FOltheory2NL_straight(tp.textCTL, tp.bridge)(0)
      si.questionCTLcomputer_rb = Some(tp.algoDef_rb)
      si.questionNL = TranslatePlofofaSentToNL(tp.algoDef_rb, tp.bridge)
                                          /*
         - TODO replace with generated item
         - Moreover, initialise with the scalaFormat instead, because in this increment people do not need to enter the queries themselves. This prevents some extra work (writing parsers).*/
      si.algoDefComputer_rb = si.questionCTLcomputer_rb
      si.answerComputerCTL = Some(tp.answerCTL)
/*       - TODO replace with generated item
         - Moreover, perhaps for now use the scalaFormat instead, because in this increment people do not need to enter the queries themselves. This prevents some extra work (writing parsers).
*/

      si.answerComputerNL = TranslateFofaSentToNL(tp.answerCTL, tp.bridge)
      si.questionRelatedBridgeStats = "TODOquestionRelatedBridgeStats"
      si.subjectNL = "subjectNL" // <still applicable?>
      // <&y2012.02.17.09:43:47& perhaps replace the first identifier match with a regular expression drawn from the parser (so that if you make changes their, it automatically gets changed here...>
      si.bridgeCTL2NLcomputer = Some(tp.bridge)
      si
   }

   var textCTLplayerUpdated4terParsing = true // starts with true, because also includes situation that no parsing has taken place ever.

   def textCTLbyPlayerChanged(newTextCTL:String) =
   {  log("textCTLbyPlayerChanged called (call-back method from SessionInfo")
      textCTLplayerUpdated4terParsing = true
   }
   /** @todo It may be more elegant to put this intelligent setter and getter in the class SessionInfo instead, and then attach the GameCore to it using the observer pattern.
     */
   def textCTLbyPlayer_=(t:String) = { textCTLplayerUpdated4terParsing = true; /* WIW textCTLbyPlayer_rb = EfeDoc_rb(); */ si.textCTLbyPlayer_ =  t }
   def textCTLbyPlayer = si.textCTLbyPlayer_

   var textCTLbyPlayer_rb_cached:Option[EfeKRdoc_rb.FactoryResult] = None

   def textCTLbyPlayer_rb:Option[EfeDoc_rb] =
   {  textCTLbyPlayer_rb_withErrorInfo_and_store match
      {  case EfeKRdoc_rb.FactoryResult(Some(ctl_rb), _, _)   => Some(ctl_rb)
         case EfeKRdoc_rb.FactoryResult(None, _, _)      => None
      }
   }
      

   /** Parses the textCTLbyPlayer, and returns a representation bundle, including errors, moreover it makes the intermediate translation persistent in the database. The latter is only done when the translation changed since the last parsing + storage.
     */
   def textCTLbyPlayer_rb_withErrorInfo_and_store:EfeKRdoc_rb.FactoryResult =
   {  log("textCTLbyPlayer_rb_withErrorInfo_and_store called")

      if(textCTLplayerUpdated4terParsing)
      {  log("textCTLplayerUpdated4terParsing is true, so creating new representation bundle (which will also parse the text)")
         textCTLplayerUpdated4terParsing = false
         val e = EfeDoc_rb(textCTLbyPlayer)
         textCTLbyPlayer_rb_cached = Some(e)
         storeIntermediateTranslation(textCTLbyPlayer, e, SystemWithTesting.currentTimeMillis)
         e
      }else
      {  log("   !textCTLplayerUpdated4terParsing, so using cached value")
         textCTLbyPlayer_rb_cached.get
      }
   }

   /** Intended for use by textCTLbyPlayer_rb_withErrorInfo_and_store
     */
   def storeIntermediateTranslation(textCTLbyPlayer_loc: String, erfr: EfeKRdoc_rb.FactoryResult, timeOffered:TimeInMillis) =
   {  val it =
      erfr match
      {  case EfeKRdoc_rb.FactoryResult(Some(ctl_rb), errMsg, warnMsg)    =>
         {  val i = IntermediateTranslation.create.sessionInfo(si).timeOffered(timeOffered).parseErrorsAndWarnings(errMsg + warnMsg).textCTLbyPlayer(textCTLbyPlayer_loc).grammaticallyCorrect(true)
            i.save
            i
         }
         case EfeKRdoc_rb.FactoryResult(None, errMsg, warnMsg)            => 
         {  val i = IntermediateTranslation.create.sessionInfo(si).timeOffered(timeOffered).parseErrorsAndWarnings(errMsg + warnMsg).textCTLbyPlayer(textCTLbyPlayer_loc).grammaticallyCorrect(false)
            i.save
            i
         }
      }

      SessionInfo_IntermediateTranslation_join.create.intermediateTranslation(it).sessionInfo(si).save
      Unit
   }
   def generateText = "todo"

   /**
     */
   def algorithmicDefenceGenerator:EfeQuerySent_rb =
   {  val ret = BridgeBasedAutoPlofafaTranslator(si.algoDefComputer_rb.get, si.bridgeCTL2NLcomputer.get, si.bridgeCTL2NLplayer.get, null).get
      si.algoDefPlayer = Some(ret)
      ret
   }

   def generateQuestionAndCorrectAnswer:QuestionAndCorrectAnswer = null // <TODO>

   /** @todo check answer correct with  BridgeBasedAutoFofaTranslator, instead of comparing the translations to natural language. The latter may be prone to errors, because different translations may exist for the same CTL sentence. And do exist already: the order of the names of persons may be different.
     */
   def doAlgorithmicDefence:AlgorithmicDefenceResult =
   {  val answerPlayerCTL = reas.plofofa.Prover.query(si.algoDefPlayer.get, textCTLbyPlayer_rb.get.sf) // for now scala format is needed, because the prover works on a more expressive CTL than EfeDoc.
      si.answerPlayerCTL = Some(answerPlayerCTL)
      si.answerPlayerNL = TranslateFofaSentToNL(answerPlayerCTL, si.bridgeCTL2NLplayer.get)

      val answerPlayerCTLtranslated2answerLangComputer = BridgeBasedAutoFofaTranslator(answerPlayerCTL, si.bridgeCTL2NLplayer.get, si.bridgeCTL2NLcomputer.get, si.textCTLbyComputer.get)
      match
      {  case Some(answerPlayerCTLtranslated2answerLangComputer) =>
         {  si.answerPlayerCorrect( si.answerComputerCTL.get.equalsModuloVarNames(answerPlayerCTLtranslated2answerLangComputer) ).save
         }
         case None => si.answerPlayerCorrect( false ).save
      }

      AlgorithmicDefenceResult(si.answerPlayerCorrect.is, si.answerPlayerNL, "", answerPlayerCTL)
   }
   // <&y2011.11.17.18:49:46& or should I change the type of text and trans to the Text class etc. see model package.>

   def getOrCreatePlayerBridge:BridgeDoc =
   {  si.bridgeCTL2NLplayer match
      {  case None =>
         {  val (bridgeDoc, fastPredicate, bigPredicate) = initialiseEfeDoc(textCTLbyPlayer_rb.getOrElse(logAndThrow("Never call this method when textCTLbyPlayer has nto been defined yet")).sf)
            si.bridgeCTL2NLplayer = Some(bridgeDoc)
            bridgeDoc
         }
         case Some(b) => b
      }
   }

}
//} EUC

/* @todo temporarily switched of NotUna, put back during develop.refactor4addingAnyFluencyChallenge, which also requires some refactoring of things now put in Efe, while they should be put on a more general level (in the TraitGameCore)

class NotUna(val playerIdInit:Long) extends TraitGameCore
{  //var translation: String = ""
   val gameCoreName="NotUna"
   val playerId = playerIdInit
   var si:SessionInfo = null
   /* This doesn't only generate the text, but everything: the ctf text, the nl text, the question for the attack, and the answer based on the text. (Note that for the latter, the Clean program actually applies the reasoner to textCTLbyComputer, it is not "baked in".)      
   */

   /* <&y2012.09.26.12:38:17& COULDDO perhaps refactor: call the serialize method from SessionInfoMetaMapperObj.save (by overriding the latter method). Without additional checks, that will however be less efficient, because at each save invocation a lot will be written over and over to disk...> */

   override def initialiseSessionInfo:SessionInfo = 
   {  // regex must contain exactly 1 group which will be returned as a match.
      super.initialiseSessionInfo
      def extractRegExGroup(regexStr:String, sbc: String):String =
      {  val regex = new Regex(regexStr)
         val m     = regex.findFirstMatchIn(sbc)
         
         var errormsg = "NotUna.generateNewSessionBundle.extractRegExGroup: error: with regex = " + regex + ": "
         var result:String = ""
         if( m.isDefined )
         {  var group = m.get.group(1) // <&y2011.11.23.18:07:48& better: count groups first, must be one, other wise you can get exception>
            if( group != null ) 
               result = group
            else
               result = errormsg + " group not found"
         }
         else
            result = errormsg + " no match"

         err.println("NotUna.generateNewSessionBundle.extractRegExGroup: " + result)
         result
      }

      def extractNl(sbc: String):String = extractRegExGroup("""TextNL \[\(SentenceNL \"([^\"]+)\"""", sbc)
      // def extractNl(sbc: String):String = extractRegExGroup("""IADTstring \"TextNL ([^\"]*)\"""", sbc)

      def extractCTL(sbc: String):String = extractRegExGroup("""IADTstring \"TextCTL ([^\"]*)\"""", sbc)

      def extractNLquestion(sbc: String):String = extractRegExGroup("""QuestionAttackNL \[\(SentenceNL \"([^\"]*)""", sbc)
 
      def extractQRBS(sbc: String):String = extractRegExGroup("""IADTstring \"QuestionRelatedBridgeStats ([^\"]*)\"""", sbc)

/* gvim regex --> scala regex:
\( --> (
\) --> )
"  --> \"
(  --> \(
)  --> \)
[  --> \[ (only if intended to match a '[' symbol)
]  --> \[ (only if intended to match a ']' symbol)
\+ (for repitition) --> +
*/

//      def extractAnswerNL(sbc: String):String = extractRegExGroup("""AnswerNL \[\(SentenceNL \"([^\"]+)\"""", sbc)
      def extractAnswerNL(sbc: String):String = extractRegExGroup("""IADTstring \"AnswerNL ([^\"]*)\"""", sbc)

      def extractAnswerCTL(sbc: String):String = extractRegExGroup("""AnswerCTL ([^\]]*\][^0-9]*[0-9]*\)\))""", sbc)

      def extractBridgeCTL2NLcomputer(sbc: String):String = extractRegExGroup("""IADTstring \"BridgeComputer ([^\"]*)\"""", sbc)

      def extractQuestionCTLcomputer(sbc: String):String = extractRegExGroup("""IADTstring \"QuestionAttackCTL ([^\"]*)\"""", sbc)

      def extractSubjectNL(sbc: String):String = extractRegExGroup("""EntityStat \(_EntityStat_ \[[0-9]+\]""" + HurelanBridge.wordNLregexStr + """ \[[0-9]+\]([a-zA-Z\-]+)\)""", sbc)

      var sbClean:String = ""
      val ran:Int = currentTimeMillis().toInt
      // <&y2011.11.23.21:08:25& check whether scala Ints indeed fit in Clean ints>
      err.println("generateNewSessionBundle: use as random number = " + ran)
      sbClean = ( ( SWIFTBINARIES + "/textgenerator " + ran ) !!)
      
      si.textNL = extractNl(sbClean)
      si.textCTLbyComputer = extractCTL(sbClean)
      si.questionNL = extractNLquestion(sbClean)
      si.questionCTLcomputer = extractQuestionCTLcomputer(sbClean)
      si.algoDefComputer = si.questionCTLcomputer
      si.answerComputerCTL = extractAnswerCTL(sbClean)
      si.answerComputerNL = extractAnswerNL(sbClean)
      si.questionRelatedBridgeStats = extractQRBS(sbClean)
      si.subjectNL = extractSubjectNL(si.questionRelatedBridgeStats)
      // <&y2012.02.17.09:43:47& perhaps replace the first identifier match with a regular expression drawn from the parser (so that if you make changes their, it automatically gets changed here...>
      si.hurelanRole1NL = extractRegExGroup("""HurelanStat \(_HurelanStat_ \[[0-9]+\][a-zA-Z_]+ \[[0-9]+\](""" + HurelanBridge.wordNLregexStr + """)""", si.questionRelatedBridgeStats)
      si.hurelanRole2NL = extractRegExGroup("""HurelanStat \(_HurelanStat_ \[[0-9]+\][a-zA-Z_]+ \[[0-9]+\][a-zA-Z_]+ \[[0-9]+\](""" + HurelanBridge.wordNLregexStr +""")""", si.questionRelatedBridgeStats)
      si.bridgeCTL2NLcomputer = extractBridgeCTL2NLcomputer(sbClean)

      si
   }

   def parseTextCTLbyPlayer:Boolean = 
   {  println("ParseTextCTLbyPlayer called")
      textCTLplayerUpdated4terParsing = false
      parseWarningMsgTxtCTLplayer = if(si.textCTLbyPlayer.equals("")) "Warning: empty file." else ""  // <&y2012.05.19.20:27:13& replace with regex for visually empty file (thus file with only space characters, like space, newline, tab etc.>

      Folminqua2FOLtheoryParser.parseAll(Folminqua2FOLtheoryParser.folminquaTheory, si.textCTLbyPlayer) match
         {  case Folminqua2FOLtheoryParser.Success(ftl,_)         => {  si.textCTLbyPlayerScalaFormat_ = Some(ftl)
                                                                        si.constantsByPlayer           = Some(ftl.constants.map({ case Constant(id) => id }))
                                                                        si.predsByPlayer               = Some(ftl.predicates.map(pred => pred.name))
                                                                        parseErrorMsgTextCTLplayer = ""
                                                                        true
                                                                     }
            case failMsg@Folminqua2FOLtheoryParser.Failure(_,_)   => {  si.textCTLbyPlayerScalaFormat_      = None
                                                                        si.constantsByPlayer             = None
                                                                        si.predsByPlayer                 = None
                                                                        println("  parse error: " + failMsg.toString)
                                                                        parseErrorMsgTextCTLplayer = failMsg.toString
                                                                        false 
                                                                     }
         }
   }

   def algorithmicDefenceGenerator:FolnuminquaQuery = 
   {  // generate algo. defence (=, or better coincides with reasoning goal) for translation created by the player.     
   
      // <&y2011.12.11.19:40:54& make naming consistent (naming for algodef4player) throughout all source code>
      // <&y2011.12.12.16:27:40& Build in test whether all required GameCore properties are set>
      err.println("algorithmicDefenceGenerator: start")
      // <&y2012.05.07.18:49:04& rewrite in SWiFT format>
      val bridgeCTL2NLplayerCleanFormat = HurelanBridge.parseAll(HurelanBridge.bridge, si.bridgeCTL2NLplayer)  match { case HurelanBridge.Success(result,_) => result; case _ => throw new RuntimeException(" Error while parsing bridgeCTL2NLplayer") }
      // <&y2012.01.27.23:02:44& refactor this: put bridgeCTL2NLplayerCleanFormat in the SessionInfo model, and check there whether it needs updates or not.>
      val cmd_output = cleanBridge(SWIFTBINARIES + "/adGen", si.bridgeCTL2NLcomputer + NEWLINE + si.algoDefComputer + NEWLINE + bridgeCTL2NLplayerCleanFormat + NEWLINE) 
      /* Example in scala format (instance of FolnuminquaQuery): Sharpest(NumResPat(Geq(), PatVar(numpatvarname), Var(boundvarname), PredApp(p,consts)))
     */
      val algoDefPlayerSerializedWithLiftJson = cmd_output
      implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[Var], classOf[Constant]))) + (new EnumSerializer(ComparisonOperator))
      println("   trying to deserialize:" + algoDefPlayerSerializedWithLiftJson)
      val algoDefPlayerScalaFormat = Serialization.read[Sharpest](algoDefPlayerSerializedWithLiftJson) // <&y2012.05.16.22:35:10& is it possible to use the name of superclass of the case class Sharpest after the read?>
      si.algoDefPlayer = Some(algoDefPlayerScalaFormat)
      algoDefPlayerScalaFormat
   }
   // for now it is assumed that only ONE question is generated per session (which is never improved or changed.)
   def generateText:String = 
   {  initialiseSessionInfo
      si.textNL
   }


   def generateQuestionAndCorrectAnswer:QuestionAndCorrectAnswer =
   {  new QuestionAndCorrectAnswer(si.questionNL, si.questionCTLcomputer) // it has already been done in this increment, so no additional calculations are required.
   }


   // <&y2011.12.24.12:42:26& move this function to general library>
   // <&y2011.12.26.12:44:14& use this function for all clean calls (thus rewrite some code)>
   // input: newline separated. For newline DO NOT use \n, but System.getProperty("line.separator");   
   def cleanBridge(function:String, input:String):String =
   {  val (functionoutput, _, _) = cleanBridgeExtended(function:String, input:String)
      functionoutput
   }

   def cleanBridgeExtended(function:String, input:String):(String,String,String) = 
   {  // err.println("cleanBridge start")
      val DEBUGCLEANCALLS = true
      var outFile = new File(function + ".clean.in")

      err.println("  creating file: " + outFile.getAbsolutePath)
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.print(input)
      out.flush()
      out.close()

      // delete old output file of clean command
      var inFile = new File(function+".clean.out")
      err.println("  trying to delete old "+function+".clean.out file (if it exists) " + (if(inFile.delete()) "successful" else "failed"))   

      // Run external (Clean) command
      var errStr:String = ""
      var outStr:String = ""

      val pl = ProcessLogger( o => (outStr += o), e => (errStr += e) )
      val procBuilder = sys.process.Process(function, new java.io.File( SWIFTBINARIES ))
      val s:Int = procBuilder!(pl)
      // Now delete input file, to prevent reareading it in the future... This can be switched of temporarily for debugging purposes: you can then still read the file.
      err.println("  trying to run " + function + " on commandline...")
      err.println("  exit value (0 is good) = " + s)
      err.println("  errorstream: " + (if(errStr == "") "None" else errStr))
      err.println("  end errorstream")
      err.println("  outputstream: " + (if(outStr == "") "None" else outStr))
      err.println("  end outputstream")
      if(!DEBUGCLEANCALLS) err.println("  now trying to delete "+function+".clean.in: " + (if(outFile.delete()) "successful" else "failed"))
      //if(DEBUGCLEANCALLS && !(s == 0)) println("  errors during execution, so trying to delete output file "+function+".clean.out: " + (if(inFile.delete()) "successful" else "hmm, failed")) // normally preserve in and output files for debugging, but delete when there is an error otherwise an old output will be used leading to less transparent runtime bugs.

      // Read output of the Clean command
      inFile = new File(function+".clean.out")
      val in:BufferedReader = new BufferedReader(new FileReader(inFile))
      val output = in.readLine()
      
      // <&y2011.12.12.15:25:59& build in error check: was the file really created afresh, and doesn't it contain errors?>
      if(DEBUGCLEANCALLS && !(s == 0)) println("  errors during execution, so trying to delete output file "+function+".clean.out: " + (if(inFile.delete()) "successful" else "hmm, failed")) // normally preserve in and output files for debugging, but delete when there is an error otherwise an old output will be used leading to less transparent runtime bugs.
      in.close()
      // Now delete output file, to prevent reareading it in the future... This can be switched of temporarily for debugging purposes: you can then still read the file.
      if(!DEBUGCLEANCALLS) err.println("  now trying to delete "+function+".clean.out: " + (if(inFile.delete()) "successful" else "failed")) // <&y2011.12.21.15:45:38& BUG: this doesn't happen, while it returns "true". Strange...>
      (output, outStr, errStr)
   }
   def doAlgorithmicDefence:(scala.Boolean, String, String, String) =
   {  // 1. do algorithmic defence of player's translation  
      err.println("start doAlgorithmicDefence")
      // >>> SUC
      // <&y2012.05.18.17:00:18& perhaps better to do the parsing in the SessionInfos class at the moment the player's text ctl is provided. You have to parse it anyway to check for syntactic correctness.>

      si.textCTLbyPlayerScalaFormat match
      {  case Some(textCTLbyPlayerScalaFormat_Loc)  => 
         {  si.algoDefPlayer match
            {  case Some(algoDefPlayerLoc) => si.answerPlayerCTL = Folnuminqua.query(algoDefPlayerLoc, textCTLbyPlayerScalaFormat_Loc).toString // <&y2012.05.18.20:15:07& I shuld reteurn the result in the answer lang, not only a number (check this). For this rewrite tpfolnuminqua>
               case None                   => throw new RuntimeException(" no algorithmic defence available, should always be present in this stage of the game.")
            }
         }
         case None               => throw new RuntimeException(" Error happened during parsing textCTLbyPlayer")
      }

      // 2. translate the answer in CTL to NL as well <&y2012.05.19.18:26:13& SHOULDDO do this later, first a quick fix: simply return the answer. The problem is that the reasoner in scala currently doesn't return the answer in the answer lang, but it just returns a number.>
      /*
      err.println("   answerPlayerCTL = " + si.answerPlayerCTL)
      val bridgeCTL2NLplayerCleanFormat = HurelanBridge.parseAll(HurelanBridge.bridge, si.bridgeCTL2NLplayer) match { case HurelanBridge.Success(result,_) => result; case _ => throw new RuntimeException(" Error while parsing bridgeCTL2NLplayer") }
      if( !si.answerPlayerCTL.equals("Unknown") )
      {  si.answerPlayerNL = cleanBridge(SWIFTBINARIES + "/answerInCTL2NL_CI", si.answerPlayerCTL + NEWLINE + bridgeCTL2NLplayerCleanFormat + NEWLINE)
      }
      */

      // <&y2012.05.19.21:20:06& as soon as answerInCTL2NL is ported to scala, do the following differently (= translate answerPlayerCTL into answerPlayerNL> 
      si.answerPlayerNL = si.answerComputerNL.replaceAll("""(minimally )[0-9]+""", "$1" + si.answerPlayerCTL) // <_&y2012.05.19.21:11:46& dangerous, if there are other digits in the string...>

      si.answerPlayerCorrect(si.answerPlayerNL.equals(si.answerComputerNL)).save

      (si.answerPlayerCorrect.is, si.answerPlayerNL, "TODO: reasonercomment (needed?)", si.answerPlayerCTL)
      // <<< EUC
   }
}

*/

// Helper return types, allows returning a subset of the above things in a type

// return 

}
