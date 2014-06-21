package org.ocbkc.swift 
{
package snippet 
{
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.DisplayHelpers._
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


/** Given a release id, gives a summary for each player who played with this release. So, it does not show the separate sessions, but given each player, for example the average score etc.
*/
class AnalyseFluencySessionsOfRelease
{  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable

   def playerTableRows(ns:NodeSeq, release_id:VersionId):NodeSeq =
   {  log("sessionTableRows called")
      

         
      implicit val displayIfNone = "-"

      // create headers
     /*
      val header = 
         bind(
            "top", chooseTemplate("top", "row", ns),
               "playerId"                    -> <b>Player</b>,
               "fluencyScore"                -> <b>Fluency Score</b>,
               "timeSessionPlayed"           -> <b>Time Session Played</b>,
               "averageFluency"              -> <b>Average Fluency</b>,
               "masteredChallenge"           -> <b>Mastered Challenge</b>,
               "averageDurationTranslation"  -> <b>Average Duration Translation</b>,
               "shortestTranslationTime"     -> <b>Shortest Translation Time</b>,
               "numberOfValidSessionsPlayed" -> <b>Number of valid sessions played</b>,
               "numberOfSessionsPlayed"      -> <b>Number of sessions played </b>,
               "sessionLink"                 -> <b>Analyse</b>
         )

      // create data rows
     val header =
      Constitution.playersWithRelease(release_id).flatMap
      {  p =>
         {  val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

            bind( "top", chooseTemplate("top", "row", ns),
               "playerId"                    -> { Text(p.swiftDisplayName) },
               "fluencyScore"                -> { Text(optionToUI(PlayerScores.fluencyScore(p).map{ defaultRounding }) ) },
               "averageFluency"              -> { Text(optionToUI(PlayerScores.averageFluency(p).map{ defaultRounding } ) ) },
               "timeSessionPlayed"           -> { Text(df.format(new java.util.Date((sesCoordLR.sessionsPlayedBy(p).head.stopTimeTranslation.get)*1000L)))},
               "masteredChallenge"           -> { Text("not implemented yet") },
               "averageDurationTranslation"  -> { Text( "" + optionToUI( PlayerScores.averageDurationTranslation(p).averageDurationTranslation ) ) },
               "shortestTranslationTime"     -> { Text("TODO") }, // I think merge from develop.javascriptdurationclock
               "numberOfValidSessionsPlayed" -> { Text( OCBKCinfoPlayer.numberOfValidSessionsPlayedBy(p).toString ) },
               "numberOfSessionsPlayed"      -> { Text( OCBKCinfoPlayer.numberOfSessionsPlayedBy(p).toString ) },
               "sessionLink"                 -> SHtml.link("analyseFluencySessionsPlayer.html?player_id="+p.id,()=>(),Text("Analyse"))
            )
         }
      } ++
        TableSorter("#sessionOfReleaseTable")*/
      val header = Elem(
      null,
      "table",
      new UnprefixedAttribute("id",
      Text("sessionOfReleaseTable"),
      new UnprefixedAttribute("class", Text("tablesorter"), Null)),
      TopScope,
      <thead><tr><th>Player</th>
      <th>Fluency Score</th>
      <th>Time Session Played</th>
      <th>Average Fluency</th>
      <th>Mastered Challenge</th>
      <th>Average Duration Translation</th>
      <th>Shortest Translation Time</th>
      <th>Number of valid sessions played</th>
      <th>Number of sessions played</th>
      <th>Analyse</th></tr></thead>
      ,
      <tbody>{ Constitution.playersWithRelease(release_id).map(
      p =>
      { val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")
        <tr>
        <td>{ p.swiftDisplayName}</td>
        <td>{ optionToUI(PlayerScores.fluencyScore(p).map{ defaultRounding })   }</td>
        <td>{ df.format(new java.util.Date((sesCoordLR.sessionsPlayedBy(p).head.stopTimeTranslation.get)*1000L))}</td>
        <td>{   optionToUI(PlayerScores.averageFluency(p).map{ defaultRounding } ) }</td>
        <td>todo</td>
        <td>{ optionToUI( PlayerScores.averageDurationTranslation(p).averageDurationTranslation ) }</td>
        <td>todo</td>
        <td>{ OCBKCinfoPlayer.numberOfValidSessionsPlayedBy(p).toString }</td>
        <td>{ OCBKCinfoPlayer.numberOfSessionsPlayedBy(p).toString  }</td>
        <td><a href={ "analyseFluencySessionsPlayer.html?player_id="+p.id }>Analyse</a></td>

        </tr>
      }
      )
      }
      </tbody>
      )
      log(header.toString)
      header ++ TableSorter("#sessionOfReleaseTable")
   }

   def render(ns: NodeSeq): NodeSeq =
   {  S.param("release_id") match
      {  case Full(release_id) =>
         {  val msgStart = "   Release with id " + release_id
            if( Constitution.releaseExists(release_id) )
            {  log( msgStart + " found!")
            
               Constitution.getConstiOfRelease(release_id) match
               {  case Some(consti) =>
                  {  bind( "top", ns,
                        "sessionOfReleaseTable" -> playerTableRows(ns, release_id),
                        "constName"             -> Text(consti.constiId.toString),
                        "release"               -> Text(release_id),
                        "scoring"               -> Text(consti.averageScore.toString)
                     )
                  }
                  case None =>
                  {  log("[ERROR] constitution of release " + release_id + " not found.")
                     S.redirectTo("../index")
                  }
               }
            } else
            {  log( "[POTENTIAL_BUG] " + msgStart + " not found... Me not happy. Perhaps the player used an old link to a release which has been deleted in the meanwhile.")
               S.redirectTo("../index")
            }
         }

         case _ =>
         {  log(" Parameter release_id missing in URL.")
            S.redirectTo("../index")
         }
      }
   }
}

}
}

