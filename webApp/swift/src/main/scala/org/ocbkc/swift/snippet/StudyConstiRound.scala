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

   def processSubmission() = 
   {

     log(SesCoord.si.studyStopConstiTime.toString)


    log(SesCoord.si.studyStartConstiTime.toString)

     log("processSubmission called")

    // SesCoord.si.studyStopConstiTime = Some(System.currentTimeMillis)
  //   log("studyConstiTime: "+ (.get - SesCoord.si.studyStartConstiTime.get).toString)
      S.redirectTo("translationRound.html")
   }
/*
  if(SesCoord.si.studyStartConstiTime == None)
  {
    SesCoord.si.studyStartConstiTime = Some(System.currentTimeMillis)

  }
*/

def render =
   {  SesCoord.is.URconstiStudy


       "#continue"   #> SHtml.submit("Continue", processSubmission) &
      // "startTimeConsti" #> Text(SesCoord.si.studyStartConstiTime.get.toString)
        "#liftGen" #> Script(JsRaw(
       "function gAction(action){" +
         SHtml.jsonCall(JsRaw("action"),(a:Any)=>{
           if(a == "pressed")
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

  def gPressed(){
println("G is pressed")
    SesCoord.si.studyStartConstiTime ::= System.currentTimeMillis
  }

  def gReleased(){
println("G is released")
    SesCoord.si.studyStopConstiTime ::= System.currentTimeMillis
  }





}
}
}
