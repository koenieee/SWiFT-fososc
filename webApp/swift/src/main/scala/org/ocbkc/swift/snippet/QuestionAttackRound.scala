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
import org.ocbkc.swift.coord.ses.Core

import Helpers._

class QuestionAttackRound
{  val sesCoordLR = sesCoord.is

   def render(ns: NodeSeq): NodeSeq =
   {  var playerAnswerTF = ""

      def processSubmission() = 
      {  println("processSubmission called")
         // <&y2011.11.08.18:54:41& check errors on submission here>
         S.redirectTo("algorithmicDefenceRound.html") 
      }  
      sesCoord.URstartQuestionAttack
      var boundForm = 
            bind( "form", ns, 
                  "playerAnswerFromSource" -> SHtml.text("Enter your answer here", playerAnswerTF = _),
                  "continue"      -> SHtml.submit("Continue", processSubmission)
               )
      bind( "qar", boundForm, 
            "questionNL" ->   Text(sesCoord.cc.questionNL),
            "questionCTL" ->  Text(sesCoord.cc.questionCTLcomputer),
            //"questionNo" ->   Text("TODO: questionNo"),
            "computerAnswerFromSource" -> Text(sesCoord.cc.answerComputerNL)
          )
   }
}

}
}

