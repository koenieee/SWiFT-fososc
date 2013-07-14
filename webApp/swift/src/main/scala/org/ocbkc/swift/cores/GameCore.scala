// change this when moving the project.// <&y2011.11.07.13:19:35& perhaps in future move gamecore to own package>
package org.ocbkc.swift.cores
{  
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.reas._
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
import net.liftweb.json._
import net.liftweb.json.ext._
//import scala.util.parsing.combinator.Parsers

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

trait TraitGameCore
{  // SHOULDDO: how to initialize a val of this trait in a subclass of this trait? (would like to do that with playerId)
   def initialiseCoreContent:CoreContent // <does this really belong here?>
   def generateText:String
   def algorithmicDefenceGenerator:FolnuminquaQuery
   def generateQuestionAndCorrectAnswer:QuestionAndCorrectAnswer
   def doAlgorithmicDefence:(scala.Boolean, String, String, String)
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

// helper class for return type of generateQuestionAndCorrectAnswer

class NotUna(val playerIdInit:Long) extends TraitGameCore
{  //var translation: String = ""
   val playerId = playerIdInit
   /* This doesn't only generate the text, but everything: the ctf text, the nl text, the question for the attack, and the answer based on the text. (Note that for the latter, the Clean program actually applies the reasoner to textCTLbyComputer, it is not "baked in".)      
   */
   var cc:CoreContent = null

   def initialiseCoreContent:CoreContent = 
   {  // regex must contain exactly 1 group which will be returned as a match.
      cc = new CoreContent
      cc.userId(playerId)
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
      
      cc.textNL = extractNl(sbClean)
      cc.textCTLbyComputer = extractCTL(sbClean)
      cc.questionNL = extractNLquestion(sbClean)
      cc.questionCTLcomputer = extractQuestionCTLcomputer(sbClean)
      cc.algoDefComputer = cc.questionCTLcomputer
      cc.answerComputerCTL = extractAnswerCTL(sbClean)
      cc.answerComputerNL = extractAnswerNL(sbClean)
      cc.questionRelatedBridgeStats = extractQRBS(sbClean)
      cc.subjectNL = extractSubjectNL(cc.questionRelatedBridgeStats)
      // <&y2012.02.17.09:43:47& perhaps replace the first identifier match with a regular expression drawn from the parser (so that if you make changes their, it automatically gets changed here...>
      cc.hurelanRole1NL = extractRegExGroup("""HurelanStat \(_HurelanStat_ \[[0-9]+\][a-zA-Z_]+ \[[0-9]+\](""" + HurelanBridge.wordNLregexStr + """)""", cc.questionRelatedBridgeStats)
      cc.hurelanRole2NL = extractRegExGroup("""HurelanStat \(_HurelanStat_ \[[0-9]+\][a-zA-Z_]+ \[[0-9]+\][a-zA-Z_]+ \[[0-9]+\](""" + HurelanBridge.wordNLregexStr +""")""", cc.questionRelatedBridgeStats)
      cc.bridgeCTL2NLcomputer = extractBridgeCTL2NLcomputer(sbClean)

      cc
   }

   def algorithmicDefenceGenerator:FolnuminquaQuery = 
   {  // generate algo. defence (=, or better coincides with reasoning goal) for translation created by the player.     
   
      // <&y2011.12.11.19:40:54& make naming consistent (naming for algodef4player) throughout all source code>
      // <&y2011.12.12.16:27:40& Build in test whether all required GameCore properties are set>
      err.println("algorithmicDefenceGenerator: start")
      // <&y2012.05.07.18:49:04& rewrite in SWiFT format>
      val bridgeCTL2NLplayerCleanFormat = HurelanBridge.parseAll(HurelanBridge.bridge, cc.bridgeCTL2NLplayer)  match { case HurelanBridge.Success(result,_) => result; case _ => throw new RuntimeException(" Error while parsing bridgeCTL2NLplayer") }
      // <&y2012.01.27.23:02:44& refactor this: put bridgeCTL2NLplayerCleanFormat in the CoreContent model, and check there whether it needs updates or not.>
      val cmd_output = cleanBridge(SWIFTBINARIES + "/adGen", cc.bridgeCTL2NLcomputer + NEWLINE + cc.algoDefComputer + NEWLINE + bridgeCTL2NLplayerCleanFormat + NEWLINE) 
      /* Example in scala format (instance of FolnuminquaQuery): Sharpest(NumResPat(Geq(), PatVar(numpatvarname), Var(boundvarname), PredApp(p,consts)))
     */
      val algoDefPlayerSerializedWithLiftJson = cmd_output
      implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[Var], classOf[Constant]))) + (new EnumSerializer(ComparisonOperator))
      println("   trying to deserialize:" + algoDefPlayerSerializedWithLiftJson)
      val algoDefPlayerScalaFormat = Serialization.read[Sharpest](algoDefPlayerSerializedWithLiftJson) // <&y2012.05.16.22:35:10& is it possible to use the name of superclass of the case class Sharpest after the read?>
      cc.algoDefPlayer = Some(algoDefPlayerScalaFormat)
      algoDefPlayerScalaFormat
   }
   // for now it is assumed that only ONE question is generated per session (which is never improved or changed.)
   def generateText:String = 
   {  initialiseCoreContent
      cc.textNL
   }


   def generateQuestionAndCorrectAnswer:QuestionAndCorrectAnswer =
   {  new QuestionAndCorrectAnswer(cc.questionNL, cc.questionCTLcomputer) // it has already been done in this increment, so no additional calculations are required.
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
      // <&y2012.05.18.17:00:18& perhaps better to do the parsing in the CoreContents class at the moment the player's text ctl is provided. You have to parse it anyway to check for syntactic correctness.>

      cc.textCTLbyPlayerScalaFormat match
      {  case Some(textCTLbyPlayerScalaFormat_Loc)  => 
         {  cc.algoDefPlayer match
            {  case Some(algoDefPlayerLoc) => cc.answerPlayerCTL = Folnuminqua.query(algoDefPlayerLoc, textCTLbyPlayerScalaFormat_Loc).toString // <&y2012.05.18.20:15:07& I shuld reteurn the result in the answer lang, not only a number (check this). For this rewrite tpfolnuminqua>
               case None                   => throw new RuntimeException(" no algorithmic defence available, should always be present in this stage of the game.")
            }
         }
         case None               => throw new RuntimeException(" Error happened during parsing textCTLbyPlayer")
      }

      // 2. translate the answer in CTL to NL as well <&y2012.05.19.18:26:13& SHOULDDO do this later, first a quick fix: simply return the answer. The problem is that the reasoner in scala currently doesn't return the answer in the answer lang, but it just returns a number.>
      /*
      err.println("   answerPlayerCTL = " + cc.answerPlayerCTL)
      val bridgeCTL2NLplayerCleanFormat = HurelanBridge.parseAll(HurelanBridge.bridge, cc.bridgeCTL2NLplayer) match { case HurelanBridge.Success(result,_) => result; case _ => throw new RuntimeException(" Error while parsing bridgeCTL2NLplayer") }
      if( !cc.answerPlayerCTL.equals("Unknown") )
      {  cc.answerPlayerNL = cleanBridge(SWIFTBINARIES + "/answerInCTL2NL_CI", cc.answerPlayerCTL + NEWLINE + bridgeCTL2NLplayerCleanFormat + NEWLINE)
      }
      */

      // <&y2012.05.19.21:20:06& as soon as answerInCTL2NL is ported to scala, do the following differently (= translate answerPlayerCTL into answerPlayerNL> 
      cc.answerPlayerNL = cc.answerComputerNL.replaceAll("""(minimally )[0-9]+""", "$1" + cc.answerPlayerCTL) // <_&y2012.05.19.21:11:46& dangerous, if there are other digits in the string...>

      cc.answerPlayerCorrect(cc.answerPlayerNL.equals(cc.answerComputerNL)).save

      (cc.answerPlayerCorrect.is, cc.answerPlayerNL, "TODO: reasonercomment (needed?)", cc.answerPlayerCTL)
      // <<< EUC
   }
}

// Helper return types, allows returning a subset of the above things in a type

// return 

}
