// <&y2011.11.07.13:19:35& perhaps in future move gamecore to own package>
package org.ocbkc.swift.cores
{  

import org.ocbkc.swift.model._
import org.ocbkc.swift.coord.ses._
import System._
import scala.sys.process._
import scala.util.matching._
import scala.util.matching.Regex._

/* Conventions:
- Names of classes correspond with design $JN/...
*/

import Round._

trait TraitGameCore
{  def generateText:Text
   def generateQuestionAndAnswers (text:String, trans:String):QuestionAttackAndDefence
   // <&y2011.11.17.18:49:46& or should I change the type of text and trans to the Text class etc. see model package.>
}

class NotUna extends TraitGameCore
{  //var translation: String = ""
   
   /* This doesn't only generate the text, but everything: the ctf text, the nl text, the question for the attack and the algorithm for the defence.
   */
   private var sb:SessionBundle = null

   def generateNewSessionBundle = 
   {  // regex must contain exactly 1 group which will be returned as a match.
      def extractRegExGroup(regexStr:String, sbc: String):String =
      {  val regex = new Regex(regexStr)
         val m     = regex.findFirstMatchIn(sbc);
         
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

      def extractCTL(sbc: String):String = extractRegExGroup("""TextCTL (\[[^\[]*\[\[[^\]]*\],\[[^\]]*\]\]\)\)\])""", sbc)

      def extractNLquestion(sbc: String):String = extractRegExGroup("""QuestionAttackNL \[\(SentenceNL \"([^\"]*)""", sbc)
 
/* gvim regex --> scala regex:
\( --> (
\) --> )
"  --> \"
(  --> \(
)  --> \)
[  --> \[ (only if intended to match a '[' symbol)
]  --> \[ (only if intended to match a ']' symbol)
*/

      def extractCTLquestion(sbc: String):String =  
      {  "dummy"
      }

      def extractAnswerNL(sbc: String):String =
      {  extractRegExGroup("""AnswerNL \[\(SentenceNL \"([^\"]+)\"""", sbc)
      }

      def extractAnswerCTL(sbc: String):String = extractRegExGroup("""AnswerCTL ([^\]]*\][^0-9]*[0-9]*\)\))""", sbc)

      var sbClean:String = ""
      val ran:Int = currentTimeMillis().toInt
      // <&y2011.11.23.21:08:25& check whether scala Ints indeed fit in Clean ints>
      err.println("generateNewSessionBundle: currentTimeMillis = " + ran)
      sbClean = ( ( "../../textgenerator " + ran ) !!)

      // extract text
      var ctlt = new CTLtranslation("", extractCTL(sbClean))
      val text = new Text(extractNl(sbClean), ctlt)

      // extract question attack and algo. defence
      val question = new Question(extractNLquestion(sbClean), extractCTLquestion(sbClean))

      // extract correct machine generated answers
      val answerFromSource = new Answer(extractAnswerNL(sbClean),extractAnswerCTL(sbClean)) // <&y2011.11.07.14:53:56& dummy>
      val answerFromTrans  = new Answer("", "")
      val qaad = new QuestionAttackAndDefence(question, answerFromSource, answerFromTrans)

      sb = new SessionBundle(text, qaad)         
      // err.println("NotUna: pwd = " + output)
   }

   // for now it is assumed that only ONE question is generated per session (which is never improved or changed.)
   def generateText:Text = 
   {  generateNewSessionBundle
      sb.text
   }

   def generateQuestionAndAnswers(text:String, trans:String):QuestionAttackAndDefence =
   {  
      // IMPORTANT: in this increment, the test and trans are ignored, because everything has already been created in generateSessionBundle. In the future it may be so that this must be changed, because the transalation may be used to influence the question attack. Now that is not the case.
      sb.qaad
   }
}

}
