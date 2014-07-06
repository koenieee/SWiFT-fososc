package org.ocbkc.swift.snippet

import scala.xml.{Text, NodeSeq}
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.DisplayHelpers._
import net.liftweb.widgets.tablesorter.TableSorter
import net.liftweb.util.Helpers._
import org.ocbkc.swift.OCBKC.Constitution
import org.ocbkc.swift.OCBKC.scoring.PlayerScores
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full
import org.ocbkc.swift.model._
import net.liftweb.mapper.By
import org.ocbkc.swift.general.GUIdisplayHelpers._

class analyseFluencySessionsPlayer {
  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable

  def sessionPlayerTable(ns:NodeSeq, playerID: Player):NodeSeq =
  { log("sessionPlayerTable called")

    TableSorter("#analyseFluencySessionsPlayer")

    implicit val displayIfNone = "-"
    val dateFormat =  new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")

    // create headers
    val header =
    bind("top", chooseTemplate("top", "row", ns),
      "sessionId"       -> <b>Session</b>,
      "translationEndTime" -> <b>Translation End-Time</b>,
      "score"           -> <b>Fluency Score</b>,
      "durTrans"        -> <b>Duration Translation</b>,
      "answerCor"       -> <b>Answer Correct</b>,
      "detailsLink"     -> <b>Analyse</b>
    )

    // log("Fluency Score: "+ PlayerScores.fluencyScoreSample(playerID).toString())

    header ++
    sesCoordLR.sessionsPlayedBy(playerID).flatMap
    { session =>

      bind( "top", chooseTemplate("top", "row", ns),
      "sessionId"      -> { Text(session.id.toString) },
      "translationEndTime"-> { Text(dateFormat.format(session.stopTimeTranslation.is)) },
      "score"          -> { Text("" + optionToUI(PlayerScores.fluencyScore(session).map{ fs => defaultRounding(fs.toDouble) })) },
      "durTrans"       -> { Text(session.durationTranslation.get.toString) },
      "answerCor"      -> { Text(session.answerPlayerCorrect.get match { case true => "Yes" case false => "No"}) },
      "detailsLink"    -> { SHtml.link("analyseFluencySessionDetails.html?session_id="+session.id.toString,()=>(),Text("Analyse")) }
      )
    }
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
            "release"             -> Text(constiOption.get.currentVersionId)
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
