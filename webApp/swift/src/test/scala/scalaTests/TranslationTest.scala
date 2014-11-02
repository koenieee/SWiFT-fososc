package org.ocbkc.swift {

import org.ocbkc.swift.snippet.SesCoord
import org.scalatest.FlatSpec
import org.scalatest.GivenWhenThen
import org.ocbkc.swift.model.Player
import net.liftweb.http.{S, LiftRules, Req, LiftSession}
import org.ocbkc.swift.global.TestSettings
import net.liftweb.mapper._
import net.liftweb.db.StandardDBVendor
import net.liftweb.util.{StringHelpers, Props}
import net.liftweb.common.Empty
import org.ocbkc.swift.OCBKC.Constitution
import bootstrap.liftweb.Boot
import org.ocbkc.swift.jgit.InitialiseJgit

class TranslationTest extends FlatSpec with GivenWhenThen {

    var userID:Player = Player;
    "A test-Player" must "be created" in {
      if (!DB.jndiJdbcConnAvailable_?)
      {  val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

        LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

        DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
      }
      userID = Player.create.firstName("test").email("test@test.org").password("test123").validated(true);
      val good: Boolean = userID.save

      info("PLayer saving: "+ good)

      assert(good)


    }

  "Translation Test Constitution 1"  must "correctly be handled by SWiFT" in { //"Translation Test Constitution 1"
  val session : LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {
      //initialise everything
      Player.logUserIn(userID);

      val sesCoordLR = SesCoord.is
      InitialiseJgit() // This must happen before Constitution methods are called!
      // Initialisation/shutdown code for OCBKC stuffzzzzariowaikoeikikal
      Constitution.deserialize
      sesCoordLR.URchooseFirstConstitution(Constitution.constis.head.constiId)
      sesCoordLR.URtryStartSession
      SesCoord.is.URconstiStudy
//begin testing a full translation round!
      sesCoordLR.URstartTranslation //start translation round
			given("a text: ");
      info("Text: "+ sesCoordLR.si.textNL)
			given("a correct Translation: ")
			val translation: String = "F(david)" //generate answer
      sesCoordLR.si.textCTLbyPlayer = translation;
      SesCoord.URstopTranslation

      sesCoordLR.URstartBridgeConstruction // start bridge round
			when("Round Bridge: ")
      val constant = sesCoordLR.constantsByPlayer
      info("Player constants: " + constant.toString())
      val entNLnames = sesCoordLR.si.bridgeCTL2NLcomputer.get.entityBridgeSents.map{ eb => eb.entNLnames(0) }
      sesCoordLR.addToPlayerBridge(constant.head, entNLnames.head) //only working ATM for translations with ONE object and only for FAST
			info(sesCoordLR.si.bridgeCTL2NLplayer.toString);

      SesCoord.URstopBridgeConstruction

      SesCoord.URstartQuestionAttack
			when("Round QuestionAttack: ")
      info("questionNL: " + SesCoord.si.questionNL)
      info("questionCTL: " + SesCoord.si.questionCTLcomputer_rb.get.toString)
      info("computerAnswer: " + SesCoord.si.answerComputerNL)

      sesCoordLR.URstartAlgorithmicDefenceStage1
			//val aQuestion = "Mention people or things which are fast. And... do not mention some, but mention all of them!" //generate question
			when("Round Algo1: ")
      info("questionNL: " + sesCoordLR.si.questionNL)
      info("algoDefPlayer: " + sesCoordLR.si.algoDefPlayer.get.sf.toString)
      info("computerAnswerFromSource: " + sesCoordLR.si.answerComputerNL)

      sesCoordLR.URstartAlgorithmicDefenceStage2

      then("Final check Round: ")
      info("questionNL: " +sesCoordLR.si.questionNL)
      info("algoDefPlayer: " + sesCoordLR.si.algoDefPlayer.get.sf.toString)
      info("answerFromSourceCTL: " + sesCoordLR.si.answerComputerCTL.get.toString)
      info("answerFromSourceNL: " + sesCoordLR.si.answerComputerNL)
      info("answerFromTransCTL: " + sesCoordLR.si.answerPlayerCTL.get.toString)
      info("answerFromTransNL: " + sesCoordLR.si.answerPlayerNL)
      info("conclusion: " + sesCoordLR.si.answerPlayerCorrect)
      assert(sesCoordLR.si.answerPlayerCorrect)

      sesCoordLR.URfinaliseSession
      sesCoordLR.closeSession
			
		}
  }


	}
}
