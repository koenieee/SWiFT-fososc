package org.ocbkc.swift 
{
package snippet 
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.ocbkc.swift.global._
import org.ocbkc.swift.coord.ses._


class ConstitutionHistoric
{  println("ConstitutionHistoric constructor called")
   val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.

   val const:Constitution = S.param("constid") match
   {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                           { case Some(constLoc)   => { println("   Constitution id:" + idLoc); constLoc }
                             case None             => { S.redirectTo("notfound") }
                           }
      case _            => S.redirectTo("constitutions") 
   }

   val historicConst:const.HisCon = S.param("commitid") match
   {  case Full(commitidLoc)  => {  const.historicContentInScalaXML( commitidLoc ) match
                                    {  case Some(hc)  => hc
                                       case None      => S.redirectTo("notfound")
                                    }
                                 }
      case _            => S.redirectTo("error")
   }  


  val currentUserId:Int = Player.currentUserId match // <&y2012.06.23.14:41:16& refactor: put currentuserid in session var, and use that throughout the session-code>
      {  case Full(id)  => { id.toInt }
         case _         => { throw new RuntimeException("  No user id found.") }
      }

/*      
   def processSomeBtn() =
   {  S.redirectTo("history?id=" + const.get.constiId)
   }
*/
   def render(ns: NodeSeq): NodeSeq =
   {  println("ConstitutionHistoric.render")
      val emptyNode = <div></div> // <&y2012.06.02.18:53:13& nicer way of defining empty substitution?>

      var constLoc:Constitution = null // workaround for tuple error, note (object:ClassA, ..) = (null, ...) leads to match error in scala.
     
      val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

      val answer   = bind( "top", ns, 
                           "title"              -> Text("Constitution " + const.constiId.toString),
                           "revisionDatetime"   -> Text(df.format(historicConst.creationDatetimePOSIX*1000)),
                           "constitutionText"   -> historicConst.content,
                           "creationDate"       -> Text(df.format(const.creationTime).toString),
                           "creator"            -> Text(
                                       Player.find(const.creatorUserID) match
                                       {  case Full(player)  => player.swiftDisplayName
                                          case _             => { println("    bug: Player with id " + const.creatorUserID + " not found."); "player unknown (this is a bug, please report it)." }
                                       }
                                                       )

                           // "description"        -> { if( !errorRetrievingConstitution ) Text(constLoc.shortDescription) else emptyNode } <&y2012.07.28.20:23:17& todo>
                     )
      answer
   }
}


}
}
