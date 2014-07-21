package org.ocbkc.swift.snippet

import scala.xml._
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.DisplayHelpers._
import net.liftweb.widgets.tablesorter.TableSorter
import net.liftweb.util.Helpers._
import org.ocbkc.swift.OCBKC.{OCBKCinfoPlayer, Constitution}
import org.ocbkc.swift.OCBKC.scoring.PlayerScores
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full
import org.ocbkc.swift.model._
import net.liftweb.mapper.By
import org.ocbkc.swift.general.GUIdisplayHelpers._
import net.liftweb.common.Full
import scala.xml.Text
import net.liftweb.common.Full

class analyseFluencySessionsPlayer {
  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable

  def sessionPlayerTable(ns:NodeSeq, playerID: Player):NodeSeq =
  { log("sessionPlayerTable called")

    implicit val displayIfNone = "-"
    val dateFormat =  new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")

    val header = Elem(
      null,
      "table",
      new UnprefixedAttribute("id",
        Text("analyseFluencySessionsPlayer"),
        new UnprefixedAttribute("class", Text("tablesorter"), Null)),
      TopScope,
      <thead><tr><th>Session</th>
        <th>Translation Endtime</th>
        <th>Fluency Score</th>
        <th>Duration Translation</th>
        <th>Answer Correct</th>
        <th>Analyse</th></tr></thead>
      ,
      <tbody>{  sesCoordLR.sessionsPlayedBy(playerID).map(
        session =>
        { val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")
          <tr>
            <td>{ session.id.toString }</td>
            <td>{ dateFormat.format(session.stopTimeTranslation.is)   }</td>
            <td>{ "" + optionToUI(PlayerScores.fluencyScore(session).map{ fs => defaultRounding(fs.toDouble) })}</td>
            <td>{  session.durationTranslation.get.toString  }</td>
            <td>{  session.answerPlayerCorrect.get match { case true => "Yes" case false => "No"}  }</td>
            <td><a href={ "analyseFluencySessionDetails.html?session_id="+session.id.toString }>Analyse</a></td>

          </tr>
        }
      )
        }
      </tbody>
    )
    log(header.toString)
    header ++ TableSorter("#analyseFluencySessionsPlayer")


  }

  def render(ns: NodeSeq): NodeSeq =
  { S.param("player_id") match
    { case Full(player_id) =>
      { val msgStart = "Input player_ID: " + player_id
        //println(Player.find(By(Player.id, player_id.toLong)))

        val player = Player.find(By(Player.id, player_id.toLong))

        if( !player.isEmpty )
        { log( msgStart + " found!")
          val constiOption:Option[Constitution] = Constitution.getById(player.get.firstChosenConstitution.is)
          log(constiOption.toString)
          //I get sometimes a None.Get. But the player exists, why is constiOption empty, if there are sessions played..?

          bind( "top", ns,
            "sessionPlayerTable"  -> sessionPlayerTable(ns, player.get),
            "player"              -> Text(player.get.firstName),
            "constName"           -> Text(constiOption.get.constiId.toString),
            "release"             -> Text(constiOption.get.currentVersionId),
            "graphLink"           -> SHtml.link("../fluencyTimeSeriesGraph.html?player="+player.get.userIdAsString,()=>(),Text("Link"))
          )
        }
        else
        { log("[POTENTIAL_BUG] " + msgStart + " not found.")
          S.redirectTo("../index")
        }
      }

      case _ =>
      { log(" Parameter player_id missing in URL.")
        S.redirectTo("../index")
      }
    }
  }
}
