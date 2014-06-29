package org.ocbkc.swift.snippet


import _root_.scala.xml._
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import net.liftweb.common.Full
import org.ocbkc.swift.model.{SessionInfoMetaMapperObj, Player}
import net.liftweb.mapper.By
import org.ocbkc.swift.global.Logging._
/**
 * Created by koen on 28-6-14.
 */
class FluencyTimeSeries {

  def render(xhtml:NodeSeq):NodeSeq=
  {
    S.param("player") match {
      case Full(player_id) =>
      {
       // val player = Player.find(By(Player.id, player_id.toLong))
        val sessionsByPlayer = SessionInfoMetaMapperObj.find(By(SessionInfoMetaMapperObj.userId,player_id.toLong))
        log(sessionsByPlayer.toString)
      bind("top",xhtml,
          "user" -> Text("todo"),
          "input" ->Text(
          "[" +
            sessionsByPlayer.map(session =>
            "{ \"x\" : " + session.startTimeTranslation + "," +
            "\"y\" : 40 },"
            )

            +"]"




          ),
          "graph" ->Text("todo")


      )
      }
      case _ => log("Error, something went wrong.")
        S.redirectTo("/index")

    }
  }
  //example format:
  /*
[ { "x" : 1,
    "y" : 5
  },

]




   */

}
