package org.ocbkc.swift 
{
package snippet 
{

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import Helpers._
import org.ocbkc.swift.coord._
import org.ocbkc.swift.model._
import System.err.println
import net.liftweb.json._
import org.ocbkc.swift.logilang.query.folnuminqua._
import org.ocbkc.swift.logilang.query.ComparisonOperator._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang._

import net.liftweb.json._
import net.liftweb.json.ext._
import _root_.net.liftweb.http.js._ 

import org.ocbkc.swift.global.Logging._

// TODO &y2013.01.28.20:38:57& move to more general place
object sesCoord extends SessionVar(new ses.EfeCore(/* User, null, Round.NotStarted*/))

class StartSession
{  val sesCoordLR = sesCoord.is // Extract coord.ses.Core object from SessionVariable LR = Local Reference

   def render(ns: NodeSeq): NodeSeq =
   {  //var playerAnswerTF = ""
      
      // Begin For test remove
      println("### begin serialization test")
      val query = Sharpest(NumResPat(Geq, PatVar("n"), Var("x"), PredApp(Predicate("predje",2),List(Constant("a"), Var("x")))))
      println("   query serialized: " + query.serialize)
      
      /*
      //import net.liftweb.json._
      import JsonDSL._
      import Serialization._

      //implicit def formats = DefaultFormats
      implicit val formats = DefaultFormats
      // A: with trait [&y2012.05.18.11:19:45& tested: this works, all info. is expressed in the serialization!]
      sealed trait Trait1 {
        val name:String
      }

      case class CaseClass1(name:String) extends Trait1
      case class CaseClass2(name:String) extends Trait1
      case class Dummy(constant:CaseClass1)

      val scalaInstance = Dummy(CaseClass1("c1"))
      val serializedTestA = write(scalaInstance)
      println("   serialized " + scalaInstance + " to:" + serializedTestA)

      val deserializedTestA = read[Dummy](serializedTestA)
      println("   deserialized again to " + deserializedTestA)

      // Test B with superclass // <&y2012.05.18.11:19:09& ask on lift forum why this doesn't work (the name of the caseclasses is not expressed)>
      sealed class TestBSuperClass(val name:String) {
      }

      case class TestBCaseClass1(nameLoc:String) extends TestBSuperClass(nameLoc)
      case class TestBCaseClass2(nameLoc:String) extends TestBSuperClass(nameLoc)
      case class TestBDummy(constant:TestBCaseClass1)

      val testb = TestBDummy(TestBCaseClass1("c1"))
      println("   serialized " + testb + " to:" + write(testb))
      */
      log("### end serialization test")

      // End serialization test

      def processSubmission():JsCmd = 
      {  log("processSubmission called")
         // check errors on submission here
         // <&y2011.10.24.17:27:52&>
         
         sesCoordLR.URtryStartTranslation match
         {  case None  => // player may not start
            {  //log("[MUSTDO] change back to None.")
               log("[BUG] Somehow, the follow doesn't work (it doesn't show an alert). Why? Perhaps ask in lift community.")
               JsCmds.Alert("Currently, there are no fresh constitutions available. You will be automatically notified if one comes available.") 
               log("[COULDDO] alternative solution is to redirect to page with  the message.")
            }
            case Some(textNL) => // <&y2014.03.12.16:10:46& hmm, textNL not needed, refactor URtryStartTranslation?>
            {  S.redirectTo("translationRound.html") 
               JsCmds.Noop
            }
         }
      }  

      bind( "form", ns, 
         "startBtn"      -> SHtml.ajaxSubmit("I'm ready!", () => processSubmission)
      )
   }
}

}
}

