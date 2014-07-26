package org.ocbkc.swift.snippet
import scala.xml.{Text, NodeSeq}
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.DisplayHelpers._
import net.liftweb.util.Helpers._
import org.ocbkc.swift.OCBKC.Constitution
import org.ocbkc.swift.OCBKC.scoring.PlayerScores
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full
import org.ocbkc.swift.model._
import net.liftweb.mapper.By
import org.ocbkc.swift.general.GUIdisplayHelpers._


class analyseFluencySessionDetails
{ def render(ns: NodeSeq): NodeSeq =
  { implicit val displayNoneAs = "-"
  
    S.param("session_id") match
    { case Full(session_id) =>
      {
        val msgStart = "Session ID " + session_id
        val sesCoordBySession = SessionInfoMetaMapperObj.find(By(SessionInfoMetaMapperObj.id,session_id.toInt))
        log(sesCoordBySession.toString)

        if( !sesCoordBySession.isEmpty )
        { log( msgStart + " found!")
          Player.find(By(Player.id, sesCoordBySession.get.userId.get)) match
          { case Full(p) =>
            {  val constiOption:Option[Constitution] = Constitution.getById(p.firstChosenConstitution)
               val releaseId = p.releaseOfFirstChosenConstitution.get
               //  log(constiOption.toString)
               val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")
                bind( "top", ns,
                   "sessionID"     -> Text(session_id),
                   "constName"     -> Text(constiOption.get.constiId.toString),
                   "releaseIndex"  -> Text("R" + constiOption.get.releaseIndex(releaseId)), // put somewhere: {log("[ERROR] code &y2014.06.09.21:34:59&"); "not found"}),
                   "playerName"    -> Text(p.swiftDisplayName),
                   "questionNL"    -> Text(sesCoordBySession.get.questionNL),
                   "startTime"     -> Text(df.format(new java.util.Date((sesCoordBySession.get.startTimeTranslation.is)))),
                   "stopTime"      -> Text(df.format(new java.util.Date((sesCoordBySession.get.stopTimeTranslation.is)))),
                   "sourceText"    -> Text(sesCoordBySession.get.textNL),
                   "textCTLbyPlayer"   -> Text(sesCoordBySession.get.textCTLbyPlayer),
                   "bridgeCTL2NLplayer"-> Text(optionToUI(sesCoordBySession.get.bridgeCTL2NLplayer.map{ _.toString })),
                   "transTime"     -> Text(sesCoordBySession.get.durationTranslation.get.toString),
                   "score"         -> Text(optionToUI(PlayerScores.fluencyScore(sesCoordBySession.get).map{ fs => defaultRounding(fs.toDouble) })),
                   "answerCor"     -> Text(sesCoordBySession.get.answerPlayerCorrect.get match { case true => "Yes" case false => "No"}),
                   "interTrans"    ->  SHtml.link("analyseIntermediateTranslations.html?sessionID=" + session_id, () => (), Text("Link"))
                )
             }
             case _ =>
             { log("[ERROR] Session with id " + session_id + " does not have a player associated with it.")
               S.redirectTo("../index")
             }
         }
        }
        else
        { log( "[POTENTIAL_BUG] " + msgStart + " not found.")
          S.redirectTo("../index")
        }
      }

      case _ =>
      { log(" Param session_id is missing from url")
        S.redirectTo("../index")
      }
    }
  }




  //todo by Koen
  /*
  Bind with these:

    <h1>Details of Session: <top:sessionID /></h1>
        Constituton: <top:constName /><br />
        Release: <top:release /><br />
        Source Text: <top:sourceText/><br />
        Translation Time: <top:transTime /><br />
        Answer Correct: <top:answerCor /><br />
        Score Session: <top:score /><br />
  */

}
