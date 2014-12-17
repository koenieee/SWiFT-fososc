package org.ocbkc.swift.scalaTest

import org.ocbkc.swift.snippet.SesCoord
import org.scalatest.FlatSpec
import org.scalatest.GivenWhenThen
import org.ocbkc.swift.model._
import net.liftweb.http.{SessionVar, S, LiftRules, LiftSession}
import net.liftweb.mapper._
import net.liftweb.db.StandardDBVendor
import net.liftweb.util.{StringHelpers, Props}
import net.liftweb.common.Empty
import org.ocbkc.swift.OCBKC.{FollowerConsti_join, Constitution}
import org.ocbkc.swift.jgit.InitialiseJgit
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query.plofofa.{PredApp_Plofofa, MostInfo}
import org.ocbkc.swift.logilang.query.plofofa
import org.ocbkc.swift.logilang.query.PatVar
import scala.Some
import org.ocbkc.swift.logilang.PredApp_FOL
import org.ocbkc.swift.logilang.bridge.brone.PredicateBridgeSent
import org.ocbkc.swift.logilang.bridge.brone.EntityBridgeSent
import org.ocbkc.swift.logilang.fofa.PredApp_Fofa
import org.ocbkc.swift.trans.TranslateFOLtheory2NL
import org.ocbkc.swift.cores.EfeChallengeTypes.{EfeQuerySent_rb, EfeAnswerLangSent}
import org.ocbkc.swift.cores.{EfeLang, TraitGameCore}
import org.ocbkc.swift.coord.ses
import org.ocbkc.swift.logilang.query.plofofa.translator.TranslatePlofofaSentToNL
import org.ocbkc.swift.logilang.fofa.translator.TranslateFofaSentToNL


object user {
   var UserID: Player = null;
}
class EfelangTest extends EfeLang(user.UserID.id) with TraitGameCore[EfeQuerySent_rb, EfeAnswerLangSent/* change to _rb when available */]{


   override def generateTranslationProblem: TranslationProblem =
   {
      val FOLcus  = new FOLtheory()
      val (bridgeDoc, fastPredicate, bigPredicate) = super.initialiseEfeDoc(FOLcus)

      val randomPersonCTLname = "ctlName" + "Kibbeling"
      val randomPersonConstant = FOLcus.gocConstant(randomPersonCTLname)

      val fastPredicateBridge = PredicateBridgeSent("B", List("big"))

      bridgeDoc.bridgeSents  ++= List(fastPredicateBridge)
      val entityBridge = EntityBridgeSent(randomPersonCTLname, List("Kibbeling"))

      bridgeDoc.bridgeSents ++= List(entityBridge)
      FOLcus.addPredApp(PredApp_FOL(fastPredicate, List(randomPersonConstant)))

      val answerCTL_option = Some(fofa.Forall(Var("x"), List(randomPersonConstant), PredApp_Fofa(bigPredicate, List(Var("x")))))
      val textNL = TranslateFOLtheory2NL.NLstyleStraight(FOLcus, bridgeDoc)(0)
      val algoDef_rb_option = Some(EfeQuerySent_rb(MostInfo(PatVar("s"), plofofa.Forall(Var("x"), PatVar("s"), PredApp_Plofofa(bigPredicate, List(Var("x")))))))

      TranslationProblem(FOLcus, textNL, bridgeDoc, algoDef_rb_option.get, answerCTL_option.get)

   }
   super.initialiseSessionInfo
}

class TranslationTest extends FlatSpec with GivenWhenThen {


   ignore must "correctly be handled by SWiFT" in {
      //"Translation Test Constitution 1"

      val session: LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
      S.initIfUninitted(session) {
         if (!DB.jndiJdbcConnAvailable_?) {
            val vendor =
               new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
                  Props.get("db.url") openOr
                     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
                  Props.get("db.user"), Props.get("db.password"))

            LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

            DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
         }
         Schemifier.schemify(true, Schemifier.infoF _, Player, PlayerSessionInfo_join, SessionInfoMetaMapperObj, FollowerConsti_join, IntermediateTranslation, SessionInfo_IntermediateTranslation_join)
         val userID = Player.create.firstName("test").email("test@test.org").password("test123").validated(true);
         user.UserID = userID;
         userID.save;

         //initialise everything
         Player.logUserIn(userID);


         InitialiseJgit() // This must happen before Constitution methods are called!
         // Initialisation/shutdown code for OCBKC stuffzzzzariowaikoeikikal
         Constitution.deserialize

         val EFeinput = new EfelangTest;

         info("EFEEE: " + EFeinput.initialiseSessionInfo)
         object SesCoord extends SessionVar(new ses.EfeCore(EFeinput))
         val sesCoordLR = SesCoord.is

         sesCoordLR.URchooseFirstConstitution(Constitution.constis.head.constiId)
         //start everything
         sesCoordLR.URtryStartSession
         SesCoord.is.URconstiStudy
         //generatedEfeDoc must be giving to generateTranslationProblem as starting point..

         //begin testing a full translation round!
         sesCoordLR.URstartTranslation //start translation round
         //      sesCoordLR.si.textCTLbyComputer = Some(generatedEfeDoc) //HOWTO?
         //     sesCoordLR.si.bridgeCTL2NLcomputer = Some(bridgeDoc)//HOWTO??
         info("CTL TExt: " + sesCoordLR.si.textCTLbyComputer)
         given("a text: ");
         info("Text: " + sesCoordLR.si.textNL)
         given("a correct Translation: ")
         val translation: String = "B(kibbeling)" //generate answer
         sesCoordLR.si.textCTLbyPlayer = translation;
         SesCoord.URstopTranslation

         //  sesCoordLR.si.textCTLbyComputer = Some(generatedEfeDoc);






         sesCoordLR.URstartBridgeConstruction // start bridge round
         when("Round Bridge: ")
         val constant = sesCoordLR.constantsByPlayer
         info("Player constants: " + constant.toString())
         val entNLnames = sesCoordLR.si.bridgeCTL2NLcomputer.get.entityBridgeSents.map {
            eb => eb.entNLnames(0)
         }
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
         info("questionNL: " + sesCoordLR.si.questionNL)
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

