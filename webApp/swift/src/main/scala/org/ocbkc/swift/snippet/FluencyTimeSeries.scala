package org.ocbkc.swift.snippet


import _root_.scala.xml._
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import net.liftweb.common.Full
import org.ocbkc.swift.model.{SessionInfo, SessionInfoMetaMapperObj, Player}
import net.liftweb.mapper.By
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.OCBKC.scoring.PlayerScores
import org.ocbkc.swift.global.DisplayHelpers._

/**
 * Created by koen on 28-6-14.
 */
class FluencyTimeSeries {

  def render(xhtml: NodeSeq):NodeSeq=
  { log("Fluency Time Series Graph Called")
    S.param("player") match
    { case Full(player_id) =>
      { val player = Player.find(By(Player.id, player_id.toLong))
        if( !player.isEmpty )
        { log("Sessions for player: " + player)

        //val sessionsByPlayer:List[SessionInfo] = SessionInfoMetaMapperObj.findAll(By(SessionInfoMetaMapperObj.userId,player_id.toLong))
       // log("Sessions found: " + sessionsByPlayer.toString)
          val sessionWithIndex = SesCoord.is.sessionsPlayedBy(player.get).zipWithIndex

 	  log("Sessions found: " + sessionWithIndex)

          bind("top",xhtml,
              "user"    -> Text(player.get.swiftDisplayName),
              "input"   ->Text(
              "[" +
                sessionWithIndex.map(session =>
                  "{ " +
                  "\"time\" : " + session._1.startTimeTranslation + ",\n" +
                  "\"fluency\" : "+ PlayerScores.fluencyScore(session._1).map{ fs => defaultRounding(fs.toDouble) }.get + ", \n" +
                  "\"id\": "+ session._2 +",\n" +
		  "\"sessionID\": "+ session._1.id +"\n" +
                  "},"
                ).mkString.dropRight(1)

                +"]"
              )
          )
        }
        else
        { log("Player doesn't exists. ")
          S.redirectTo("/index")
        }
      }
      case _ =>
      { log("Error, something went wrong.")
        S.redirectTo("/index")
      }
    }
  }
}
