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
import org.ocbkc.swift.general.GUIdisplayHelpers._
import org.ocbkc.swift.OCBKC._
import org.ocbkc.swift.OCBKC.scoring.PlayerScores

class PlayerStats
{  val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.

   def render(ns: NodeSeq): NodeSeq =
   {  println("Playerstats.render called")
      val player = sesCoordLR.currentPlayer
      implicit val displayAsNoneAs = "not applicable"
      val test = PlayerScores.shortestTranslation(player)//it looks like its working, for all players  //0 //averageDurationTranslation(player, -1)
            bind( "top", ns, 
            "shortestTransTime" -> Text("" + optionToUI(sesCoord.sesHis.shortestTranslationTime)),
            "sessionsPlayed"    -> Text("" + sesCoord.sesHis.totalNumber),
            "numberCorrect"     -> Text("" + sesCoord.sesHis.numberCorrect),
            "playerShortest"     -> Text("" + test),
            "percentageCorrect" -> Text("" + optionToUI(sesCoord.sesHis.percentageCorrect))
          )
   }
}

}
}
