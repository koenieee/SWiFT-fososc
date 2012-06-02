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

class Constitution
{  val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.

   def render(ns: NodeSeq): NodeSeq =
   {  println("Constitution.render snippet")
      def processEditBtn() =
      {  /* >>> SUC 
         Player.currentUserId match
         {  case Full(id)  => { Constitution.create(id.asInstanceOf[String].toInt) }
            case _         => { throw new RuntimeException("  No user id found.") }
         }
            <<< EUC */
      }

      def processHistoryBtn() =
      {
      }

      /* <? &y2012.06.02.14:38:52& what is an elegant way to progam the following? Problem is that if the pattern turns out to be None, you cannot return anything, or you have to use some ugly work around (tupling etc.) (go back with git to this date to get the right example...> */
      val EmptyNode = Text("") // <&y2012.06.02.18:53:13& nicer way of defining empty substitution?>

      val (constitutionHtml, creator, creationDate, title) = S.param("id") match
      {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                              {  case Some(const) => { println("   Constitution id:" + idLoc); (const.loadHtml, Text(""+const.creatorUserID), Text(""+const.creationTime), Text("Constitution " + const.id)) }
                                 case None        => (Text("No constitution with id " + idLoc + " exists"), EmptyNode, EmptyNode, Text("Constitution not found"))
                              }

         case Empty        => (Text("Cannot retrieve constitution: incorrect parameters in URL: use /constitution?id=[some number here]."), EmptyNode, EmptyNode, Text("Constitution not found"))
   //    case HalfFull => Always better then empty ;-)
      }
     
      val answer   = bind( "top", ns, 
                           "constitutionText"   -> constitutionHtml,
                           "editBt"             -> SHtml.button("Edit", processEditBtn), // <&y2012.05.30.16:25:40& disable button when no user is logged in>
                           "revisionHistory"    -> SHtml.button("History", processHistoryBtn),
                           "creator"            -> creator,
                           "title"              -> title,
                           "creationDate"       -> creationDate
                         )
      answer
   }
}

}
}
