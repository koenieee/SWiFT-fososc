package org.ocbkc.swift 
{
package snippet 
{
import org.ocbkc.swift.global.Logging._
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import org.ocbkc.swift.OCBKC.scoring._
import org.ocbkc.swift.global._
import org.ocbkc.swift.general.GUIdisplayHelpers._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.eclipse.jgit.revwalk.RevCommit 
import org.eclipse.jgit.lib.ObjectId
import org.ocbkc.swift.global.LiftHelpers._
import _root_.net.liftweb.widgets.tablesorter.TableSorter

class AnalyseFluencySession
{  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable.

   val playerOption:Option[Player] = S.param("player_id") match
   {  case Full(player_id)  =>
      {  val msgStart = "   Player with id " + player_id

         Player.find(player_id) match
         {  case Full(player) => {  log(msgStart + " found!")
                                    Some(player)
                                 } // do nothing, player exists.
            case _            =>
            {  log("   " + msgStart + " not found... Me not happy. But ain't a bug I guess.")
               None
            }
         }
      }
   }


   def sessionTableRows(ns:NodeSeq, p:Player):NodeSeq =
   {  log("sessionTableRows called")
      
      case Some(player) =>
      {  TableSorter("#SessionTable")
         
         implicit val displayIfNone = "-"

         // create headers
         val header = bind(
               "top", chooseTemplate("top", "row", ns),
               "date"               -> <b>Creation date</b>,
               "fluency"            -> <b>Fluency</b>,
               "translationTime"    -> <b>Translation Time</b>
               )
      
         // create data rows
         header ++
         sesCoordLR.sessionsPlayedBy(player).flatMap(
         session =>
         {  val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

            bind( "top", chooseTemplate("top", "row", ns),
               "date"               -> Text("todo"),
               "fluency"            -> { Text("todo") },
               "translationTime"    -> { Text("todo") }
            )
         }
         )
      }
   }

   def render(ns: NodeSeq): NodeSeq =
   {  playerOption match
      {  case Some(player) => // bind player related fields
         {  val playerBindResult =
               bind( "top", ns,
                  TODO         
               )
 
            val constiOption = playerOption.collect{ p => Constitution.getById(p.firstChosenConstitution.is) }

            constiOption match
            {  case Some(consti) => // bind consti related fields
               {  bind( "top", playerBindResult,
                     "constName"    -> Text(consti.constiId.toString)
                     "player"       -> Text("TODO player id"),
                     "release"      -> Text("TODO release id"),
                     "sessionTable" -> sessionTableRows(ns, player),
                   )
               }
               case None =>
               {  bind( "top", playerBindResult,
                     TODO
                  )
               }
            }
         }
         case None =>
         {  bind( "top", ns, 
         }
   }
}

}
}

