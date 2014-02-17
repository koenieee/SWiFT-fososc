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

class FinaliseSession
{  val sesCoordLR = sesCoord.is // Extract coord.ses.Core object from SessionVariable LR = Local Reference

   def render(ns: NodeSeq): NodeSeq =
   {  //var playerAnswerTF = ""
      sesCoordLR.URfinaliseSession
      
      def processSubmission() = 
      {  println("processSubmission called")
         // check errors on submission here
         // <&y2011.10.24.17:27:52&>
         sesCoordLR.closeSession
         S.redirectTo("startSession.html")
      }

      def processCloseSession() =
      {  sesCoordLR.closeSession
         S.redirectTo("/index.html")
      }

      bind( "form", ns, 
         "playAgainBtn"      -> SHtml.button("Play again", processSubmission),
         "closeSession"      -> SHtml.submit("Close session", processCloseSession)
      )
   }
}

}
}

