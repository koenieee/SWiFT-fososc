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



  def text:NodeSeq= {
	  println("createprogressbar is called");
   val  x = S.attr("w_page") openOr "geen param :("
   val lrfs = sesCoord.is.latestRoundFluencySession
   //println(sesCoord.is.latest);
   
              println("   latestRoundFluencySession = " + lrfs)
              //TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!
  
               val indexKey:Int = lrfs match
               {  
				   case NotInFluencySession => 0
                  case RoundTranslation    => 1

                  case RoundBridgeConstruction => 2
                  case RoundQuestionAttack => 3
                  case RoundAlgorithmicDefenceStage1 => 4
                 case RoundAlgorithmicDefenceStage2 =>  5
                  case _                   => 6
               } 
  
  
              
              val progressBarTempl =TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!
  

val completedRoundTempl = chooseTemplate("progressbar", "completedRound", progressBarTempl )
val latestRoundTempl = chooseTemplate("progressbar", "latestRoundReached", progressBarTempl )
val roundToComeTempl = chooseTemplate("progressbar", "roundToCome", progressBarTempl )
val seperator =  chooseTemplate("progressbar", "seperator", progressBarTempl )
val mapRound2DisplayName = Map( 0 -> "Start", 1 -> "Translation", 2 -> "Bridge", 3->"Question Attack" ,4->"Defence 1",5->"Defence 2",6->"End Session").toList.sortBy{_._1}


val mapRound2DisplayLinks = Map( 0 -> "startSession.html", 1 -> "translationRound.html", 2 -> "bridgeconstruction.html", 3->"questionAttackRound.html" ,4->"algorithmicDefenceRound.html",5->"algorithmicDefenceRoundStage2.html",6->"End Session").toList.sortBy{_._1}


val indexLatestRound = indexKey

mapRound2DisplayName.flatMap
{  displayNameWithIndex =>
   {  
	   if( displayNameWithIndex._1 < indexLatestRound )
      {  
      bind( "completedRound", completedRoundTempl,
      "linkToRound"  ->  SHtml.link(mapRound2DisplayLinks(displayNameWithIndex._1)._2, () => (), Text(mapRound2DisplayName(displayNameWithIndex._1)._2)),
       "separ" -> seperator
		  )
      
      } 
      else if( displayNameWithIndex._1 == indexLatestRound )
      {  
      
      bind( "latestReachedRound", latestRoundTempl,"linkToRound"  ->  SHtml.link(mapRound2DisplayLinks(displayNameWithIndex._1)._2, () => (), Text(mapRound2DisplayName(displayNameWithIndex._1)._2)),
       "separ" -> seperator
		  )
     
     }         

      else
      {
		  bind( "roundToCome", roundToComeTempl ,"disabledRound"  ->  Text(mapRound2DisplayName(displayNameWithIndex._1)._2),
			"separ" -> seperator
		  )
      	 
	  }
	  
	 
      	
   }
} 




              
              
              
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

