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
import org.ocbkc.swift.model.Player
import Helpers._
import System.err.println
import org.ocbkc.swift.global.GlobalConstant._
import org.ocbkc.swift.global.TestSettings._
import org.ocbkc.swift.parser._
import scala.util.parsing.combinator.Parsers //{Success, Failure}


class TranslationRound
{  val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.
   var errorTrans:String = ""
   var translationTAcontents:String = if(!TEST) "Enter translation here." else sesCoordLR.cc.textCTLbyComputer

   def render(ns: NodeSeq): NodeSeq =
   {  def processSubmission() = 
      {  println("processSubmission called")

         // check errors on submission here
         
         // when errors, do a reload
         // <_&y2012.02.21.19:29:09& refactor using built-in parser of CoreContent ???> 
         val transCorrect:Boolean =  sesCoordLR.testSyntaxTranslation match
         {  case ""           => true
            case _            => false
         }

         // sesCoord
         // <&y2011.10.23.17:49:39&>
         println("TranslationRound.processSubmission: translation = " + sesCoordLR.cc.textCTLbyPlayer)
         
         sesCoord.URstopTranslation
         
         if(transCorrect) 
         {  sesCoordLR.cc.ParseTextCTLbyPlayer // <&y2012.02.22.15:31:48& is now needed to get constantsByPlayer and predsByPlayer filled. In future this should not be needed (see CoreContent)>
            S.redirectTo("bridgeconstruction.html") 
         }   
         else S.redirectTo("translationRound.html") 
      }

      def processTestTransBt() =
      {  println("processTestTransBt called")
         // <&y2012.01.19.09:56:18& is processTranslationTA indeed called before this method, otherwise I have a problem...>/(importance = 10)
          S.redirectTo("translationRound.html")
      }

      // <? why can't scala infer that contentTranslationTA must be a String. (Omitting String, will result in an error).
      def processTranslationTA(contentTranslationTA:String) =
      {  sesCoordLR.cc.textCTLbyPlayer = contentTranslationTA // <&y2011.11.17.18:53:43& add check in Coord.scala that the translation has indeed been defined when you start using it somewhere, perhaps with Option>
         // test
         val a = Player.currentUserId
         if(a.isEmpty)
            println("   no currentUserId:")
         else
            println("   currentUserId = " + a.open_!)
      }

      // <&y2012.02.26.01:34:10& perhaps more elegant to make use iof built-in parser of CoreContent>
      def errorTransWebText =
      {  Text( sesCoordLR.testSyntaxTranslation match
               {  case ""           => "Correct"  
                  case parseErrMsg  => parseErrMsg
               }
             )
      }

      /*
      case class SWiFTParseResult
      case class EmptyFile extends SWiFTParseResult
      case class FilledFile(ParseResult) extends SWiFTParseResult
      */
   
      val testExampleTextCTL = "p({a},{b})"
      var boundForm = bind( "form", ns, 
            "translation" -> SHtml.textarea(if(AUTOTRANSLATION && sesCoordLR.cc.textCTLbyPlayer.equals("")) testExampleTextCTL else sesCoordLR.cc.textCTLbyPlayer, processTranslationTA, "rows" -> "10", "style" -> "width: 100%;"), // todo: how to make text area page width?
            "testTransBt"     -> SHtml.button("test grammatically", processTestTransBt),
            "errorTrans"      -> errorTransWebText,
            "submitBt"        -> SHtml.submit("Submit", processSubmission),
            "startTime"			-> Text(sesCoordLR.cc.startTimeTranslation.is.toString())
            //"test"            -> Text(test)
          )
      bind("transround", boundForm, "sourceText" -> Text(sesCoordLR.cc.textNL))
   }
}

}
}
