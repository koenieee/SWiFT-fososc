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

class PlayerStats
{  val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.
   def optionToUI(opt:Option[Any]):String =
   {  opt match
      {  case None         => "not applicable"
         case Some(thing)  => thing.toString
      }
   }

   def render(ns: NodeSeq): NodeSeq =
   {  println("Playerstats.render called")
      val player = sesCoordLR.currentPlayer
      bind( "top", ns, 
            "shortestTransTime" -> Text("" + optionToUI(sesCoord.sesHis.shortestTranslationTime)),
            "sessionsPlayed"    -> Text("" + sesCoord.sesHis.totalNumber),
            "numberCorrect"     -> Text("" + sesCoord.sesHis.numberCorrect),
            "percentageCorrect" -> Text("" + optionToUI(sesCoord.sesHis.percentageCorrect))
          )
   }
}

}
}
