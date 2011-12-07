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

class TranslationRound
{  val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.

   def render(ns: NodeSeq): NodeSeq =
   {  def processSubmission() = 
      {  println("processSubmission called")
         // check errors on submission here
         // <&y2011.10.23.17:49:39&>
         println("TranslationRound.processSubmission: translation = " + sesCoordLR.translation)
         S.redirectTo("questionAttackRound.html") 
      }  

      // <? why can't scala infer that contentTranslationTA must be a String. (Omitting String, will result in an error).
      def processTranslationTA(contentTranslationTA:String) =
      {  sesCoordLR.translation = contentTranslationTA // <&y2011.11.17.18:53:43& add check in Coord.scala that the translation has indeed been defined when you start using it somewhere, perhaps with Option>
      }

      var boundForm = bind( "form", ns, 
            "translation" -> SHtml.textarea("Enter translation here", processTranslationTA, "rows" -> "10", "cols" -> "100"), // todo: how to make text area page width?
            "submit"      -> SHtml.submit("Submit", processSubmission)
          )
      bind("transround", boundForm, "sourceText" -> Text(sesCoordLR.text.NL))
   }
}

}
}
