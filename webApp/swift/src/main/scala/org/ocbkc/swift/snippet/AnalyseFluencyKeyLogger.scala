package org.ocbkc.swift.snippet

import scala.xml.{Text, NodeSeq}
import org.ocbkc.swift.global.Logging._
import net.liftweb.util.Helpers
import Helpers._
import _root_.net.liftweb.http._
import net.liftweb.common.Full
import org.ocbkc.swift.model.SessionInfoMetaMapperObj
import net.liftweb.mapper.By
import scala.util.parsing.json.JSON

class AnalyseFluencyKeyLogger
{ def render(ns: NodeSeq): NodeSeq =
  { S.param("sessionID") match
  { case Full(sessionID) =>
    { log("keylogPlayBackPage called")
     val jsonFileName = SessionInfoMetaMapperObj.find(By(SessionInfoMetaMapperObj.id,sessionID.toInt)).get
     // val jsonFilename = "/home/koen/SWiFT-fososc/webApp/swift/persist/keylogs/keylog_session1_user2_1414783813749.log"
      val contents = "[" + scala.io.Source.fromFile(jsonFileName.fileNameKeylogs).mkString.dropRight(1) + "]"

      val jsonContent = JSON.parseFull(contents).get.asInstanceOf[List[Map[String, (Double, Int)]]]
      log("jsonContent: " + jsonContent)
      val keysTimes: List[((String, (Double, Double)), Int)] = jsonContent.flatMap(jsonObject=> Map(jsonObject("key").asInstanceOf[String] -> (jsonObject("time").asInstanceOf[Double],jsonObject("pos").asInstanceOf[Double]))).zipWithIndex
      log("Keytimes: " + keysTimes.toString)

      val timesDuration = ( 0.0 :: keysTimes.map(_._1._2).sliding(2).map { case Seq(x, y, _*) => y._1 - x._1 }.toList).zipWithIndex
      log("Duration TImes: " + timesDuration)

      val newJsonData: String = "[" + keysTimes.map( obj => "{\"key\": \"" + obj._1._1 + "\",\"time\":" + timesDuration(obj._2)._1 + ",\"pos\":" + obj._1._2._2 + "}," ).mkString.dropRight(1) + "]"
      log(newJsonData)

      bind("top", ns,
      "jsonData" -> Text(newJsonData),
      "userName"  -> Text(jsonFileName.userId.toString)
       )
    }

    case _ => log("no session id given")
        S.redirectTo("../index")
    }
  }
}
