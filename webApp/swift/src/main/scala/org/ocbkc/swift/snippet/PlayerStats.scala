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
import org.ocbkc.swift.global.GlobalConstant._
import org.ocbkc.swift.general.GUIdisplayHelpers._


class PlayerStats
{  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable.

   def render(ns: NodeSeq): NodeSeq =
   {  println("Playerstats.render called")
      val player = sesCoordLR.currentPlayer
      implicit val displayAsNoneAs = "not applicable"
      bind( "top", ns, 
            "shortestTransTime" -> Text("" + optionToUI(SesCoord.sesHis.shortestTranslationTime)),
            "sessionsPlayed"    -> Text("" + SesCoord.sesHis.totalNumber),
            "numberCorrect"     -> Text("" + SesCoord.sesHis.numberCorrect),
            "percentageCorrect" -> Text("" + optionToUI(SesCoord.sesHis.percentageCorrect))
          )
   }
}

}
}
