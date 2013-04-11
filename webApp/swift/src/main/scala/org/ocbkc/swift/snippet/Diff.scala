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


class Difference
{  println("Difference page is called")
   val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.

   val const:Constitution = S.param("constid") match
   {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                           { case Some(constLoc)   => { println("   Constitution id:" + idLoc); constLoc }
                             case None             => { S.redirectTo("notfound") }
                           }
      case _            => S.redirectTo("constitutions") 
   }
/**Not a nice way, any other suggestions? Not good with cases :(**/
   val historicConst1:const.HisCon = S.param("newerId") match
   {  case Full(commitidLoc)  => {  const.historicContentInScalaXML( commitidLoc ) match
                                    {  case Some(hc)  => hc
                                       case None      => S.redirectTo("notfound")
                                    }
                                 }
      case _            => S.redirectTo("error")
   }  
     val historicConst2:const.HisCon = S.param("olderId") match
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
   {  
      val emptyNode = <div></div> // <&y2012.06.02.18:53:13& nicer way of defining empty substitution?>

      var constLoc:Constitution = null // workaround for tuple error, note (object:ClassA, ..) = (null, ...) leads to match error in scala.
     
      val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

      val answer   = bind( "top", ns, 
                           "title"              -> Text("Constitution " + const.constiId.toString),
                           "revisionDatetime1"   -> Text(df.format(historicConst1.creationDatetimePOSIX*1000)),
                           "revisionDatetime2"   -> Text(df.format(historicConst2.creationDatetimePOSIX*1000)),
                           "constitutionText1"   -> historicConst1.content, 
                           "constitutionText2"   -> historicConst2.content,
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
