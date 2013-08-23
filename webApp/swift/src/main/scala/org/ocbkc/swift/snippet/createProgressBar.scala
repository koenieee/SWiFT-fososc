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

def blaat= {
	
             
         val names = List((".start [href]" #> "startSession.html"),(".translation [href]" #> "translationRound.html"),(".bridge [href]" #>  "bridgeconstruction.html"),(".question [href]" #> "questionAttackRound.html"),(".defence1 [href]" #> "algorithmicDefenceRound.html"),(".defence2 [href]" #> "algorithmicDefenceRoundStage2.html"))
       
  
  ".urls *" #> names
              
}

  def text:NodeSeq= {
	  println("createprogressbar is called");
   val  x = S.attr("w_page") openOr "geen param :("
   val lrfs = sesCoord.is.latestRoundFluencySession
   //println(sesCoord.is.latest);
   
              println("   latestRoundFluencySession = " + lrfs)
              TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!
              //startSession.html: start
              //translationRound.html: translation
              //bridgeconstruction.html: bridge
              //href="questionAttackRound.html:"question
              //href="algorithmicDefenceRound.html": defence1
              //href="algorithmicDefenceRoundStage2.html": defence2
              //val urls = List(AttrBindParam("start","startSession.html","href"),AttrBindParam("translation","translationRound.html","href"),AttrBindParam("bridge","bridgeconstruction.html","href"),AttrBindParam("question","questionAttackRound.html","href"),AttrBindParam("defence1","algorithmicDefenceRound.html","href"),AttrBindParam("defence2","algorithmicDefenceRoundStage2.html","href"))
             //List.flatten(urls.slice(0,2))
              
         
               
  
              /*            
              bind("b",xhtml, 
              AttrBindParam("start","startSession.html","href"),
              AttrBindParam("translation","translationRound.html","href"),
              AttrBindParam("bridge","bridgeconstruction.html","href"),
              AttrBindParam("question","questionAttackRound.html","href"),
              AttrBindParam("defence1","algorithmicDefenceRound.html","href"),
              AttrBindParam("defence2","algorithmicDefenceRoundStage2.html","href")
              )
  
               lrfs match
               {  
				   case NotInFluencySession => translationRound.html
                  case RoundTranslation    => chooseTemplate("round", "translation", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)

                  case RoundBridgeConstruction => chooseTemplate("pross", "3", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
                  case RoundQuestionAttack => chooseTemplate("pross", "4", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
                  case RoundAlgorithmicDefenceStage1 => chooseTemplate("pross", "5", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
                 case RoundAlgorithmicDefenceStage2 =>  chooseTemplate("pross", "6", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
                  case _                   => chooseTemplate("pross", "7", TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!)
               } 
               
               */
               
            /*   
   x match
   {
    
    case "1" =>  <center><a href="startSession.html" style="color: #f8b500">Start</a> <b>-></b> Translation <b>-></b> Bridge Construction <b>-></b> Question Attack <b>-></b> Algorithmic Defence <b>-></b> Algorithmic 2 <b>-></b> End session</center>
	case _ => <b>nothing</b>
	}
  */
}
  
}

