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
import Helpers._
import System.err.println

class AlgorithmicDefenceRoundStage2
{  val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.
   def render(ns: NodeSeq): NodeSeq =
   {  def processSubmission() = 
      {  println("processSubmission called")
         // check errors on submission here
         // <&y2011.10.23.17:49:39&>
         S.redirectTo("finaliseSession.html") 
      }  

      sesCoordLR.URstartAlgorithmicDefenceStage2

      bind(  
         "top", ns,
         "continue"              -> SHtml.submit("Continue", processSubmission), // <&y2011.11.09.13:39:03& change to "explain error" when applicable>
         "questionNL"            -> Text(sesCoordLR.si.questionNL), 
         "algoDefPlayer"         -> Text(sesCoordLR.si.algoDefPlayer.get.sf.toString), // @todo replace with pf as soon as implemented
         "answerFromSourceCTL"   -> Text(sesCoordLR.si.answerComputerCTL.get.toString),
         "answerFromSourceNL"    -> Text(sesCoordLR.si.answerComputerNL),
         "answerFromTransCTL"    -> Text(sesCoordLR.si.answerPlayerCTL.get.toString),
         "answerFromTransNL"     -> Text(sesCoordLR.si.answerPlayerNL),
         "conclusion"            -> Text( if (sesCoordLR.si.answerPlayerCorrect) "The answer derived from your translation is completely correct. You won!" else "The answer derived from your translation is wrong! Buzinga! Got ya'!")
         //"questionNo"      -> Text("TODO: questionNo")
      )
   }
}

}
}
