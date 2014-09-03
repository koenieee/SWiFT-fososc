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

      val contents = "[" + scala.io.Source.fromFile(jsonFileName.fileNameKeylogs).mkString.dropRight(1) + "]"

      val jsonContent = JSON.parseFull(contents).get.asInstanceOf[List[Map[String,Any]]]
      val keysTimes: List[((String, Double), Int)] = jsonContent.flatMap(jsonObject=> Map(jsonObject("key").asInstanceOf[String] -> jsonObject("time").asInstanceOf[Double])).zipWithIndex
      log("Keytimes: " + keysTimes.toString)

      val timesDuration = ( 0.0 :: keysTimes.map(_._1._2).sliding(2).map { case Seq(x, y, _*) => y - x }.toList).zipWithIndex
      log("Duration TImes: " + timesDuration)

      val newJsonData: String = "[" + keysTimes.map( obj => "{\"key\": \"" + obj._1._1 + "\",\"time\":" + timesDuration(obj._2)._1 + "}," ).mkString.dropRight(1) + "]"
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
