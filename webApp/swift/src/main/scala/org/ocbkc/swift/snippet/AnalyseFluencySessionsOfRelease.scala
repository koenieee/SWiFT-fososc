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
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.global._
import org.ocbkc.swift.general.GUIdisplayHelpers._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.eclipse.jgit.revwalk.RevCommit 
import org.eclipse.jgit.lib.ObjectId
import org.ocbkc.swift.global.LiftHelpers._
import _root_.net.liftweb.widgets.tablesorter.TableSorter

class AnalyseFluencySessionsOfRelease
{  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable

   def playerTableRows(ns:NodeSeq, release_id:VersionId):NodeSeq =
   {  log("sessionTableRows called")
      
      TableSorter("#PlayerTable")
         
      implicit val displayIfNone = "-"

      // create headers
      val header = 
         bind(
            "top", chooseTemplate("top", "row", ns),
               "playerId"        -> <b>Player</b>,
               "fluency"         -> <b>Fluency</b>,
               "masteredChallenge"        -> <b>Master</b>,
               "averageTranslationTime"   -> <b>Average Translation Time </b>,
               "shortestTransTime"        -> <b> Shortest Translation Time </b>,
               "sessionsPlayedB4accessToAllConstis"   -> <b> Total valid sessions played </b>
         )
   
      // create data rows
      header ++
      Constitution.playersWithRelease(release_id).flatMap
      {  p =>
         {  val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

            bind( "top", chooseTemplate("top", "row", ns),
               "playerId"        -> { Text(p.swiftDisplayName) },
               "fluency"         -> { Text("TODO") },
               "masteredChallenge"        -> { Text("TODO") },
               "averageTranslationTime"   -> { Text("TODO") },
               "shortestTransTime"        -> { Text("TODO") },
               "sessionsPlayedB4accessToAllConstis"   -> Text("TODO")
            )
         }
      }
   }

   def render(ns: NodeSeq): NodeSeq =
   {  S.param("release_id") match
      {  case Full(release_id) =>
         {  val msgStart = "   Release with id " + release_id

            if( Constitution.releaseExists(release_id) )
            {  log( msgStart + " found!")
               bind( "top", ns,
                  "playerTable" -> playerTableRows(ns, release_id),
               "constName" -> Text("TODO"),
               "release" -> Text("release Name")
               )
            } else
            {  log( "[POTENTIAL_BUG] " + msgStart + " not found... Me not happy. Perhaps the player used an old link to a release which has been deleted in the meanwhile.")
               S.redirectTo("index")
            }
         }

         case _ =>
         {  log(" Parameter release_id missing in URL.")
            S.redirectTo("index")
         }
      }
   }
}

}
}
