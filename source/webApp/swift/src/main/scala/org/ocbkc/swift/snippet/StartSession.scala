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
import org.ocbkc.swift.coord._
import org.ocbkc.swift.model._
import System.err.println

object sesCoord extends SessionVar(new ses.Core(/* User, */null, Round.NotStarted))

class StartSession
{  val sesCoordLR = sesCoord.is; // Extract coord.ses.Core object from SessionVariable LR = Local Reference

   def render(ns: NodeSeq): NodeSeq =
   {  //var playerAnswerTF = ""

      def processSubmission() = 
      {  println("processSubmission called")
         // check errors on submission here
         // <&y2011.10.24.17:27:52&>
         sesCoordLR.URtranslation;
         S.redirectTo("translationRound.html") 
      }  

      bind( "form", ns, 
         "startBtn"      -> SHtml.submit("I'm ready!", processSubmission)
      )
   }
}

}
}

