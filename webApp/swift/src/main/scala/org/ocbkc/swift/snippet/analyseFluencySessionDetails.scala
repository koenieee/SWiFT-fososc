package org.ocbkc.swift.snippet
import scala.xml.{Text, NodeSeq}
import org.ocbkc.swift.global.Logging._
import net.liftweb.util.Helpers._
import org.ocbkc.swift.OCBKC.Constitution
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full
import org.ocbkc.swift.model._
import net.liftweb.mapper.By



class analyseFluencySessionDetails
{

  def render(ns: NodeSeq): NodeSeq =
  { S.param("session_id") match
    { case Full(session_id) =>
      {
        val msgStart = "Session ID " + session_id
        val sesCoordBySession = SessionInfoMetaMapperObj.find(By(SessionInfoMetaMapperObj.id,session_id.toInt))
        log(sesCoordBySession.toString)

        if( !sesCoordBySession.isEmpty )
        { log( msgStart + " found!")
          val constiOption:Option[Constitution] = Constitution.getById(Player.find(By(Player.id, sesCoordBySession.get.userId.get)).get.firstChosenConstitution)
        //  log(constiOption.toString)

          bind( "top", ns,
          "sessionID"     -> Text(session_id),
          "constName"     -> Text(constiOption.get.constiId.toString),
          "release"       -> Text(constiOption.get.currentVersionId), //is this correct?
          "sourceText"    -> Text(sesCoordBySession.get.textNL), //W: what do I need to get here?
          "transTime"     -> Text(sesCoordBySession.get.durationTranslation.toString),
          "score "        -> Text("Todo"),
          "answerCor"     -> Text(sesCoordBySession.get.answerPlayerCorrect.get match { case true => "Yes" case false => "No"})
          )
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
