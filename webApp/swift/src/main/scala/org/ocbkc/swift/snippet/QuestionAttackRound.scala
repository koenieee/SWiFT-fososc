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
import org.ocbkc.swift.model._
import org.ocbkc.swift.coord.ses.EfeCore

import Helpers._

class QuestionAttackRound
{  val sesCoordLR = SesCoord.is

   def render(ns: NodeSeq): NodeSeq =
   {  var playerAnswerTF = ""

      def processSubmission() = 
      {  println("processSubmission called")
         // <&y2011.11.08.18:54:41& check errors on submission here>
         S.redirectTo("algorithmicDefenceRound.html") 
      }  
      SesCoord.URstartQuestionAttack
      var boundForm = 
            bind( "form", ns, 
                  "playerAnswerFromSource" -> SHtml.text("Enter your answer here", playerAnswerTF = _),
                  "continue"      -> SHtml.submit("Continue", processSubmission)
               )
      bind( "qar", boundForm, 
            "questionNL" ->   Text(SesCoord.si.questionNL),
            "questionCTL" ->  Text(SesCoord.si.questionCTLcomputer_rb.toString),
            //"questionNo" ->   Text("TODO: questionNo"),
            "computerAnswerFromSource" -> Text(SesCoord.si.answerComputerNL)
          )
   }
}

}
}

