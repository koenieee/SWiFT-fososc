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

class AlgorithmicDefenceRound
{  val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.

   def render(ns: NodeSeq): NodeSeq =
   {  def processSubmission() = 
      {  println("processSubmission called")
         // check errors on submission here
         // <&y2011.10.23.17:49:39&>
         S.redirectTo("algorithmicDefenceRoundStage2.html") 
      }  

      val algoDefPlayer = sesCoordLR.URstartAlgorithmicDefenceStage1

      // recapitulate
      val answer   = bind(  
               "top", ns, 
               "startderivation"    -> SHtml.submit("Start Derivation!", processSubmission),
               "questionNL"         -> Text(sesCoordLR.si.questionNL), 
               "algoDefPlayer"      -> Text(sesCoordLR.si.algoDefPlayer.get.sf.toString),
               //"questionNo"       -> Text("TODO: questionNo"), 
               "computerAnswerFromSource" -> Text(sesCoordLR.si.answerComputerNL)
            )
      answer
   }
}

}
}
