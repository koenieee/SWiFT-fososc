package org.ocbkc.swift.snippet

import scala.xml._
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import net.liftweb.common.Full
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.model._
import net.liftweb.mapper.By
import scala.xml.Text
import net.liftweb.common.Full
import net.liftweb.widgets.tablesorter.TableSorter

class AnalyseIntermediateTranslations
{ def render(ns: NodeSeq): NodeSeq=
  { S.param("sessionID") match {
    case Full(session) => {
      //val sessions: SessionInfo = SessionInfoMetaMapperObj.find(By(SessionInfoMetaMapperObj.id, session.toInt)).get
      val HistoryTranslation: List[IntermediateTranslation] = IntermediateTranslation.findAll(By(IntermediateTranslation.sessionInfo,session.toInt))
      if(!HistoryTranslation.isEmpty)
      {

        log("Found Intersession: " + HistoryTranslation)
        bind("top", ns,
          "user"  -> Text(HistoryTranslation.head.sessionInfo.open_!.userId.toString),
          "interTables"  -> generateTable(HistoryTranslation)
        )
    }
    else {
        bind("top", ns,
          "user"  -> Text(HistoryTranslation.head.sessionInfo.open_!.userId.toString),
          "interTables"  -> Text("No intermediate translations found for this player.")
        )
      }
    }
    case _ =>
    { log("BUG: SessionID not found.")

      S.redirectTo("../index")

    }

  }
  }

    def generateTable(interTranslation: List[IntermediateTranslation]): NodeSeq =
    { val header = Elem(
        null,
        "table",
        new UnprefixedAttribute("id",
          Text("analyseIntermediateTranslations"),
          new UnprefixedAttribute("class", Text("tablesorter"), Null)),
        TopScope,
        <thead><tr>
          <th>Intermediate Time</th>
          <th>Player Text</th>
          <th>Parse Errors</th>
          <th>Grammatically Correct</th>
          </tr></thead>
        ,
        <tbody>{  interTranslation.map(
          inter =>
          {  val dateFormat =  new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
            <tr>
              <td>{ dateFormat.format(inter.timeOffered.get)   }</td>
              <td>{ inter.textCTLbyPlayer.get }</td>

              <td>{ inter.parseErrorsAndWarnings.get }</td>
              <td>{ inter.grammaticallyCorrect.get match { case true => "Yes" case false => "No"  }}</td>

            </tr>
          }
        )
          }
        </tbody>
      )
      log(header.toString)
      header ++ TableSorter("#analyseIntermediateTranslations")
    }

}
