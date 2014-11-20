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


/**
 * Created by koen on 12-11-14.
 */
class StatisticsTest extends FlatSpec with GivenWhenThen{

  "Statistics" must "be correctly calculated by SWiFT" in {//"Statistics"

    if (!DB.jndiJdbcConnAvailable_?)
    {  val vendor =
      new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
        Props.get("db.url") openOr
          "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
        Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    Schemifier.schemify(true, Schemifier.infoF _, Player, PlayerSessionInfo_join, SessionInfoMetaMapperObj, FollowerConsti_join, IntermediateTranslation, SessionInfo_IntermediateTranslation_join)
    val userID = Player.create.firstName("test").email("test@test.org").password("test123").validated(true);
    userID.save;

     val session : LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {
      //initialise everything
      Player.logUserIn(userID);

      val sesCoord = SesCoord.is
      InitialiseJgit() // This must happen before Constitution methods are called!
      // Initialisation/shutdown code for OCBKC stuffzzzzariowaikoeikikal
      Constitution.deserialize

      sesCoord.URchooseFirstConstitution(Constitution.constis.head.constiId)
      //simple translation session:
      sesCoord.URtryStartSession
      SesCoord.URconstiStudy
      SesCoord.URstartTranslation // Don't think this one is needed, it already covered by URalgorithmicDefenceSimplified. Note: in the previous version of SWiFT this was already carried out (with a working jaraforswift simulation).
      //SesCoord.URstopTranslation
      sesCoord.si.textCTLbyPlayer = "F(kibbeling)";
      val cTM = SystemWithTesting.currentTimeMillis
      val duration = 1234
      log("   playerHasAccessToAllConstis just before this session = " + OCBKCinfoPlayer.playerHasAccessToAllConstis(sesCoord.currentPlayer))
      sesCoord.si.startTime(cTM).save
      sesCoord.si.startTimeTranslation(cTM).save
      sesCoord.si.stopTime(cTM + duration).save
      sesCoord.si.stopTimeTranslation(cTM + duration).save

      sesCoord.si.answerPlayerCorrect(true).save
      sesCoord.si.serialize

      //handle rounds:
      SesCoord.URstopTranslation
      sesCoord.URstartBridgeConstruction
      SesCoord.URstopBridgeConstruction
      SesCoord.URstartQuestionAttack
      sesCoord.URstartAlgorithmicDefenceStage1

      sesCoord.URstartAlgorithmicDefenceStage2



      SesCoord.URfinaliseSession
      SesCoord.closeSession


      info("shortest translation time: "+ SesCoord.sesHis.shortestTranslationTime);
      info("totalNumer: " + SesCoord.sesHis.totalNumber);
      info("numberCorrect: " + SesCoord.sesHis.numberCorrect);
      info("percentageCorect: " + SesCoord.sesHis.percentageCorrect);


    }

  }

}
