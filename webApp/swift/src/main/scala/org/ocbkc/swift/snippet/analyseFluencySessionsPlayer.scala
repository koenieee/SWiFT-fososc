package org.ocbkc.swift.snippet

import scala.xml._
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.DisplayHelpers._
import net.liftweb.widgets.tablesorter.TableSorter
import net.liftweb.util.Helpers._
import org.ocbkc.swift.OCBKC.{OCBKCinfoPlayer, Constitution}
import org.ocbkc.swift.OCBKC.scoring.PlayerScores
import net.liftweb.http.{SHtml, S}

import org.ocbkc.swift.model._
import net.liftweb.mapper.By
import org.ocbkc.swift.general.GUIdisplayHelpers._

import scala.xml.Text
import java.io.File
import net.liftweb.common.Full
import org.jopendocument.dom.spreadsheet.SpreadSheet
import javax.swing.table.{TableColumnModel, DefaultTableModel}

class analyseFluencySessionsPlayer {
  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable
  val dateFormat =  new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
  def sessionPlayerTable(ns:NodeSeq, playerID: Player):NodeSeq =
  { log("sessionPlayerTable called")

    implicit val displayIfNone = "-"


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
            "odsDownload"         -> SHtml.link("",() => downloadOdsFile(player.get),Text("Export Spreadsheet")),
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
  def downloadOdsFile(playerID: Player)
  { log("Making ODS file..")

    val header: Array[AnyRef]= Array[AnyRef]("Session", "Translation Endtime","Fluency Score", "Duration Translation", "Answer Correct")
    val Sessions =sesCoordLR.sessionsPlayedBy(playerID)
    val data =  Array.ofDim[AnyRef](Sessions.length,5)

    Sessions.zipWithIndex.map
    { sess =>
      { data(sess._2) = Array[AnyRef](sess._1.id, dateFormat.format(sess._1.stopTimeTranslation.is), PlayerScores.fluencyScore(sess._1).map{ fs => defaultRounding(fs.toDouble) }.get.toString, sess._1.durationTranslation.get.toString , sess._1.answerPlayerCorrect.get match { case true => "TRUE" case false => "FALSE"})
      }
    }

    val tableModel = new DefaultTableModel( data, header)
    val file: File  = new java.io.File("src/main/webapp/Session"+playerID.firstName+".ods");
    SpreadSheet.createEmpty(tableModel).saveAs(file)
    S.redirectTo("/Session"+playerID.firstName+".ods")
    log("Exported Spreadsheet")

  }
}



