package org.ocbkc.swift.snippet


import _root_.scala.xml._
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import net.liftweb.common.Full
import org.ocbkc.swift.model.{SessionInfo, SessionInfoMetaMapperObj, Player}
import net.liftweb.mapper.By
import org.ocbkc.swift.global.Logging._
/**
 * Created by koen on 28-6-14.
 */
class FluencyTimeSeries {

  def render(xhtml:NodeSeq):NodeSeq=
  { S.param("player") match
    { case Full(player_id) =>
      { val sessionsByPlayer:List[SessionInfo] = SessionInfoMetaMapperObj.findAll(By(SessionInfoMetaMapperObj.userId,player_id.toLong))
        log(sessionsByPlayer.toString)
       
        bind("top",xhtml,
            "user"    -> Text("todo"),
            "input"   ->Text(
            "[" +
              sessionsByPlayer.map(session =>
              "{ \"time\" : " + session.startTimeTranslation + ",\n" +
              "\"fluency\" : 40 },"
              ).mkString.dropRight(1)

              +"]"
            ),
            "graph"   ->Text("todo")
      )
      }
      case _ =>
      { log("Error, something went wrong.")
        S.redirectTo("/index")
      }
    }
  }
}
