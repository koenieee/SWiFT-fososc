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
      println("### end serialization test")

      // End serialization test

      def processSubmission() = 
      {  println("processSubmission called")
         // check errors on submission here
         // <&y2011.10.24.17:27:52&>
         sesCoordLR.URstartTranslation

         // test json serialization
         case class TestPersistency(var val1:String)
         {  def this() = this("no constructors")
         }
         /*
         case class TestPersistency(val1:String)
         {  val val2: String = "with hurry try to find out why you do that"
         }
         */
         implicit val formats = Serialization.formats(NoTypeHints)
         
         //var testSer:String = Serialization.write(si)
         //err.println("  sesHis serialised to: " + testSer)
         //val testDeSer:SessionInfo = Serialization.read[SessionInfo](testSer)
         
         val testSer:String = Serialization.write(new TestPersistency())
         println("  Test1 = " + testSer)
         // end test

         S.redirectTo("translationRound.html") 
      }  

      bind( "form", ns, 
         "startBtn"      -> SHtml.submit("I'm ready!", processSubmission)
      )
   }
}

}
}

