package scalaTests

import org.scalatest.{GivenWhenThen, FlatSpec}
import net.liftweb.http.{LiftRules, S, LiftSession}
import net.liftweb.util.{Props, StringHelpers}
import net.liftweb.common.Empty
import org.ocbkc.swift.model._
import org.ocbkc.swift.snippet.SesCoord
import org.ocbkc.swift.jgit.InitialiseJgit
import org.ocbkc.swift.OCBKC.{FollowerConsti_join, OCBKCinfoPlayer, Constitution}
import org.ocbkc.swift.coord.ses.CoreSimu;
import net.liftweb.mapper._
import net.liftweb.db.StandardDBVendor
import scala.xml.Text
import scala.util.Random
import org.ocbkc.swift.test.SystemWithTesting
import org.ocbkc.swift.global.Logging._
import scalaTests.user
/**
 * Created by koen on 12-11-14.
 */
class StatisticsTest extends FlatSpec with GivenWhenThen{

  "Statistics" must "be correctly calculated by SWiFT" in {//"Statistics"

     val session : LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {
      //initialise everything
      Player.logUserIn(user.UserID);

      val sesCoord = SesCoord.get.sessionsPlayedBy(user.UserID).head;

      val cTM = SystemWithTesting.currentTimeMillis
      val duration = 1234
      log("   playerHasAccessToAllConstis just before this session = " + OCBKCinfoPlayer.playerHasAccessToAllConstis(user.UserID))
      sesCoord.startTime(cTM).save
      sesCoord.startTimeTranslation(cTM).save
      sesCoord.stopTime(cTM + duration).save
      sesCoord.stopTimeTranslation(cTM + duration).save

      sesCoord.answerPlayerCorrect(true).save
      sesCoord.serialize


      info("shortest translation time: "+ SesCoord.sesHis.shortestTranslationTime);
      info("totalNumer: " + SesCoord.sesHis.totalNumber);
      info("numberCorrect: " + SesCoord.sesHis.numberCorrect);
      info("percentageCorect: " + SesCoord.sesHis.percentageCorrect);


    }

  }

}
