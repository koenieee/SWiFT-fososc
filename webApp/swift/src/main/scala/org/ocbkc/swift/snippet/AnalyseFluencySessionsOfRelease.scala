package org.ocbkc.swift
{
package snippet
{
import org.ocbkc.swift.global.Logging._
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import org.ocbkc.swift.OCBKC._
import org.ocbkc.swift.OCBKC.scoring._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import Helpers._
import _root_.net.liftweb.widgets.tablesorter.TableSorter

class AnalyseFluencySessionsOfRelease
{ val sesCoordLR = SesCoord.is // extract session coordinator object from session variable

  def playerTableRows(ns:NodeSeq, release_id:VersionId):NodeSeq =
  { log("sessionTableRows called")

    TableSorter("#sessionOfReleaseTable")

    implicit val displayIfNone = "-"

    // create headers
    val header =
    bind(
    "top", chooseTemplate("top", "row", ns),
    "playerId"                             -> <b>Player</b>,
    "fluency"                              -> <b>Fluency</b>,
    "masteredChallenge"                    -> <b>Master</b>,
    "averageTranslationTime"               -> <b>Average Translation Time </b>,
    "shortestTransTime"                    -> <b> Shortest Translation Time </b>,
    "sessionsPlayedB4accessToAllConstis"   -> <b> Total valid sessions played </b>,
    "sessionLink"                          -> <b>Session Link</b>
    )

    // create data rows
    header ++
    Constitution.playersWithRelease(release_id).flatMap
    { p =>
      { val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

        bind( "top", chooseTemplate("top", "row", ns),
        "playerId"                             -> { Text(p.swiftDisplayName) },
        "fluency"                              -> { Text("Todo") }, // PlayerScores.fluencyScoreSample(p)
        "masteredChallenge"                    -> { Text("TODO") },
        "averageTranslationTime"               -> { Text(PlayerScores.averageDurationTranslation(p).averageDurationTranslation.get.toString) },
        "shortestTransTime"                    -> { Text("TODO -> see showduration highscores") },
        "sessionsPlayedB4accessToAllConstis"   -> Text("todo"),
        "sessionLink"                          -> SHtml.link("analyseFluencySessionsPlayer.html?player_id="+p.id,()=>(),Text("Player Session"))
        )
      }
    }
  }

  def render(ns: NodeSeq): NodeSeq =
  {

    (S.param("release_id"), S.param("consti_id")) match
    { case (Full(release_id),Full(consti_id)) =>
      { val msgStart = "   Release with id " + release_id

        if( Constitution.releaseExists(release_id) )
        { log(msgStart + " found!")
          val constiOption = Constitution.getById(consti_id.toInt)

          log("ConstiOption" +constiOption)

          bind( "top", ns,
          "sessionOfReleaseTable"    -> playerTableRows(ns, release_id),
          "constName"                -> Text(constiOption.get.constiId.toString),
          "release"                  -> Text(release_id)
          )
        }
        else
        { log( "[POTENTIAL_BUG] " + msgStart + " not found... Me not happy. Perhaps the player used an old link to a release which has been deleted in the meanwhile.")

          S.redirectTo("../index")
        }
      }

      case _ =>
      { log(" Parameter release_id or consti_id missing in URL.")

        S.redirectTo("../index")
      }
    }
  }
}

}
}
