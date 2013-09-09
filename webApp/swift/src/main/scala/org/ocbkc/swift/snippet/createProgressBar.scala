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
             // <&y2013.09.06.20:48:47& COULDDO refactor: get index from List(RoundTranslation, RoundBridgeConstruction, ...) is more compact coding and less shot gun surgery pattern.>
         
  
              
              val progressBarTempl =TemplateFinder.findAnyTemplate(List("templates-hidden", "progressbar")).open_!
  

val completedRoundTempl = chooseTemplate("progressbar", "completedRound", progressBarTempl )
val latestRoundTempl = chooseTemplate("progressbar", "latestRoundReached", progressBarTempl )
val roundToComeTempl = chooseTemplate("progressbar", "roundToCome", progressBarTempl )
val seperator =  chooseTemplate("progressbar", "seperator", progressBarTempl )
val mapRound2DisplayName = Map( RoundStartSession -> "Start", RoundTranslation -> "Translation", RoundBridgeConstruction -> "Bridge", RoundQuestionAttack -> "Question Attack", RoundAlgorithmicDefenceStage1->"Defence 1", RoundAlgorithmicDefenceStage2->"Defence 2", RoundFinaliseSession->"End Session")

val mapRound2DisplayLinks = Map( RoundTranslation -> "translationRound.html", RoundBridgeConstruction -> "bridgeconstruction.html", RoundQuestionAttack -> "questionAttackRound.html" ,RoundAlgorithmicDefenceStage1 -> "algorithmicDefenceRound.html", RoundAlgorithmicDefenceStage2->"algorithmicDefenceRoundStage2.html",RoundFinaliseSession->"End Session")

val indexLatestRound = roundsInOrder.indexOf( lrfs )

roundsInOrder.zipWithIndex.flatMap
{  roundWithIndex =>
   {  
	   if( roundWithIndex._1 < indexLatestRound )
      {  if(reviewable( roundWithIndex._2 ) ...
      bind( "completedRound", completedRoundTempl,
      "linkToRound"  ->  SHtml.link(mapRound2DisplayLinks(displayNameWithIndex._1)._2, () => (), Text(mapRound2DisplayName(displayNameWithIndex._1)._2)),
		  )++seperator
      
      } 
      else if( displayNameWithIndex._1 == indexLatestRound )
      {  
      
      bind( "latestReachedRound", latestRoundTempl,"linkToRound"  ->  SHtml.link(mapRound2DisplayLinks(displayNameWithIndex._1)._2, () => (), Text(mapRound2DisplayName(displayNameWithIndex._1)._2))
		  )++seperator
     
     }         
	else if( displayNameWithIndex._1 == mapRound2DisplayName.count - 1 )
		{  
			bind( "roundToCome", roundToComeTempl ,"disabledRound"  ->  Text(mapRound2DisplayName(displayNameWithIndex._1)._2)
		  )
	}
      else
      {
		  bind( "roundToCome", roundToComeTempl ,"disabledRound"  ->  Text(mapRound2DisplayName(displayNameWithIndex._1)._2)
		  )++seperator
      	 
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

