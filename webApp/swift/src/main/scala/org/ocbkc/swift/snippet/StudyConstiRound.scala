package org.ocbkc.swift 
{
package snippet 
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.ocbkc.swift.global._
import org.ocbkc.swift.coord.ses._
import org.ocbkc.swift.global.Logging._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.http.js.JE._


/** Almost the same as StudyConstitution, but now integrated as a separate round in the first session of the fresh player.
  */
class StudyConstiRound
{  log("StudyConstiRound constructor called")
  val sessionInfo = SesCoord.si
  var startTime:Long = 0;
  //handy to keep everything in one file about the time etc..?
  def savesTimesTogether()
  {
    if(SesCoord.is.latestRoundFluencySession == RoundAlgorithmicDefenceStage2) //latest round -> so ended
    { log("StudyConstiRound, saveTimesTogether called")
      val durationList = sessionInfo.constiStudyIntervals
      val totalDurationStudyTime = durationList.foldLeft(0)(_.toInt + _.toInt)
      sessionInfo.constiStudyTotalDuration = Some(totalDurationStudyTime)
      log("Total Constitution Study Duration Time: " + sessionInfo.constiStudyTotalDuration .get)
    }
  }



   def processSubmission() = 
   {  log(sessionInfo.constiStudyIntervals.toString)
      log("processSubmission called")
      S.redirectTo("translationRound.html")
   }

  def render =
  {  SesCoord.is.URconstiStudy
    "#continue"   #> SHtml.submit("Continue", processSubmission) &
    "#liftGen" #> Script(JsRaw(
      "function gAction(action){" +
      SHtml.jsonCall(JsRaw("action"),(key:Any)=>
      { if(key == "pressed")
        {
          gPressed()
        }
        else
        {
          gReleased()
        }
        Noop
        })._2.toJsCmd
      + "}"
    ))



  }

  def gPressed()
  { if (SesCoord.is.latestRoundFluencySession != RoundTranslation)
    { println("G is pressed")
      startTime = System.currentTimeMillis
    }
  }

  def gReleased()
  {  if (SesCoord.is.latestRoundFluencySession != RoundTranslation)
     { val stopTime = System.currentTimeMillis
       println("previous startTime: " + startTime)
       println("G is released on time: " + stopTime)
       sessionInfo.constiStudyIntervals ::= (stopTime - startTime)

    }
  }
}





}

}
