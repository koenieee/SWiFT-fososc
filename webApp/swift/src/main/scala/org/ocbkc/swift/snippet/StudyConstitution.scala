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


class StudyConstitution
{  println("StudyConstitution constructor called")
   val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.
   // <&y2012.08.11.12:10:18& better get right constitution from Player info, not from URL id. Or no, that is difficult when someone has access to more than one constitution to study. Just do a check here whether he or she may see the constitution here!>

      
   val consti:Constitution = 
      if( sesCoordLR.constiSelectionProcedure == OneToStartWith )
      {  println("   constiSelectionProcedure == OneToStartWith")
         sesCoordLR.firstChosenConstitution.get // should always be there, because if no constitution has been chosen this page should be hidden.
      }
      else
      {  throw new RuntimeException("  constiSelectionProcedure not set, or it is one not supported yet by class StudyConstitution")
      }

/* <&y2012.08.11.12:36:47& framework for future processing, the next is currenlty not used.>

         S.param("id") match
         {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                                 { case Some(constLoc)   => { println("   Constitution id:" + idLoc); sesCoord.firstChosenConstitution = Some(constLoc); constLoc }
                                   case None             => { println("   BUG: constitution with id " + idLoc + " not found"); S.redirectTo("notfound") }
                                 }
            case _            => { println("   BUG: no id URL-parameter in studyConstitution.html given"); S.redirectTo("errorIdNotFound") }
*/

  val currentUserId:Int = Player.currentUserId match // <&y2012.06.23.14:41:16& refactor: put currentuserid in session var, and use that throughout the session-code>
      {  case Full(id)  => { id.toInt }
         case _         => { throw new RuntimeException("  No user id found.") }
      }

/*      
   def processSomeBtn() =
   {  S.redirectTo("history?id=" + consti.get.id)
   }
*/
   def render(ns: NodeSeq): NodeSeq =
   {  println("StudyConstitution.render")
      val emptyNode = <div></div> // <&y2012.06.02.18:53:13& nicer way of defining empty substitution?>

      val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

      val answer   = bind( "top", ns, 
                           "constitutionId"     -> Text(consti.id.toString),
                           "constitutionText"   -> consti.contentInScalaXML,
                           "creationDate"       -> Text(df.format(consti.creationTime).toString),
                           "creator"            -> Text(
                                       Player.find(consti.creatorUserID) match
                                       {  case Full(player)  => player.swiftDisplayName
                                          case _             => { println("    bug: Player with id " + consti.creatorUserID + " not found."); "player unknown (this is a bug, please report it)." }
                                       }
                                                       )

                     )
      answer
   }
}


}
}
