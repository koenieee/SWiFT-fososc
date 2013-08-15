package org.ocbkc.swift.snippet

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import js.JsCmds.SetHtml
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.ocbkc.swift.general.GUIdisplayHelpers._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.OCBKC.scoring._
import org.ocbkc.swift.global._
import org.ocbkc.swift.coord._
import org.ocbkc.swift.coord.ses._

class createProgress
{
	//Please implemt RoundAlgorithmicDefenceStage2

  def text: NodeSeq = {
	  println("createprogressbar is called");
   val  x = S.attr("w_page") openOr "geen param :("
   val lrfs = sesCoord.is.latestRoundFluencySession
   //println(sesCoord.is.latest);
   
              println("   latestRoundFluencySession = " + lrfs)
               lrfs match
               {  
				   case NotInFluencySession => chooseTemplate("pross", "1", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!) //Templates("templates-hidden" :: "progressbar2" :: Nil).map{ns => <x>{ns}</x>} toSeq
                  case RoundTranslation    => chooseTemplate("pross", "2", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)

                  case RoundBridgeConstruction => chooseTemplate("pross", "3", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
                  case RoundQuestionAttack => chooseTemplate("pross", "4", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
                  case RoundAlgorithmicDefenceStage1 => chooseTemplate("pross", "5", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
                 case RoundAlgorithmicDefenceStage2 =>  chooseTemplate("pross", "6", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
                  case _                   => chooseTemplate("pross", "7", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
               } 
            /*   
   x match
   {
    
    case "1" =>  <center><a href="startSession.html" style="color: #f8b500">Start</a> <b>-></b> Translation <b>-></b> Bridge Construction <b>-></b> Question Attack <b>-></b> Algorithmic Defence <b>-></b> Algorithmic 2 <b>-></b> End session</center>
	case _ => <b>nothing</b>
	}
  */
}
  
}

