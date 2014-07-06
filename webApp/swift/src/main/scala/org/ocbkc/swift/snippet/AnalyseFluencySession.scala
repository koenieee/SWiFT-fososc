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
{  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable
      
   def sessionTableRows(ns:NodeSeq, player:Player):NodeSeq =
   {  log("sessionTableRows called")
      
      TableSorter("#SessionTable")
         
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
            "date"               -> { Text("todo") },
            "fluency"            -> { Text("todo") },
            "translationTime"    -> { Text(session.durationTranslation.toString) }
         )
      }
      )
   }

   def render(ns: NodeSeq): NodeSeq =
   {  def renderWhenPlayerExists(player:Player) =
      {  val playerBindResult =
            bind( "top", ns,
                  "player"       -> Text(player.id.asString)
            )
 
         val constiOption:Option[Constitution] = Constitution.getById(player.firstChosenConstitution.is)

         constiOption match // is there a first chosen consti for this player?
         {  case Some(consti) => // bind consti related fields
            {  bind( "top", playerBindResult,
                  "constName"    -> Text(consti.constiId.toString),
                  "release"      -> Text(consti.currentVersionId),
                  "sessionTable" -> sessionTableRows(ns, player)
               )
            }
            case None =>
            {  log("[SHOULDDO] simply do not show any consti related info in this case, but notify the constigame player that the player did not yet chose a consti")
               bind( "top", playerBindResult,
                  "constName"    -> Text("No constitution chosen by this player yet"),
                  "release"      -> Text("-"),
                  "sessionTable" -> NodeSeq.Empty
               )
            }
         }
      }

      def renderWhenNoPlayerExists =
      {  log("[SHOULDDO] simply do not show any consti related info in this case, but notify the constigame player that the player did not yet chose a consti")
            bind( "top", ns,
                  "constName"    -> Text("No constitution chosen by this player yet"),
                  "release"      -> Text("-"),
                  "sessionTable" -> NodeSeq.Empty 
            )
      }

    val all_params = S.request.toList.flatMap(_.params).toMap
     all_params.flatMap{
       param => param match {

         case ("consti_id",List(consti_id)) => consti_id match{

           case consti_ID  =>
           {  val msgStart = "Constitution ID: " + consti_id

             Constitution.getById(consti_ID.toInt) match
             {  case Some(consti_id) => {  log(msgStart + " found! TODO")
               bind("top",ns,"constName" -> Text(consti_id.toString))
               //renderWhenPlayerExists(player)
             } // do nothing, player exists.
             case _            =>
             {  log("   " + msgStart + " not found... Me not happy. But doesn't have to be a bug.")
               S.redirectTo("todoerror_no_player_not_found")
             }
             }
           }
         }


         case ("player_id", List(player_id)) =>
           player_id match{
             case player_id  =>
             {  val msgStart = "   Player with id " + player_id

               Player.find(player_id) match
               {  case Full(player) => {  log(msgStart + " found!")
                 renderWhenPlayerExists(player)
               } // do nothing, player exists.
               case _            =>
               {  log("   " + msgStart + " not found... Me not happy. But doesn't have to be a bug.")
                 S.redirectTo("todoerror_no_player_not_found")
               }
               }
             }

           }

         case _ => log("no match in params")
           S.redirectTo("index")
       }
     } toSeq



     }


   }
}


}

