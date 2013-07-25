package org.ocbkc.swift.snippet

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
               {  case NotInFluencySession => <center><a href="startSession.html">Start</a> <b>-></b> Translation <b>-></b> Bridge Construction <b>-></b> Question Attack <b>-></b> Algorithmic Defence <b>-></b> Algorithmic 2 <b>-></b> End session</center>
                  case RoundTranslation    => <center><a href="startSession.html">Start</a> <b>-></b><a href="translationRound.html" > Translation </a><b>-></b> Bridge Construction <b>-></b> Question Attack <b>-></b> Algorithmic Defence <b>-></b> Algorithmic 2 <b>-></b> End session</center>

                  case RoundBridgeConstruction => <center><a href="startSession.html">Start</a> <b>-></b><a href="translationRound.html"> Translation </a><b>-></b> <a href="bridgeconstruction.html">Bridge Construction</a> <b>-></b> Question Attack <b>-></b> Algorithmic Defence <b>-></b> Algorithmic 2 <b>-></b> End session</center>
                  case RoundQuestionAttack => <center><a href="startSession.html">Start</a> <b>-></b><a href="translationRound.html"> Translation </a><b>-></b> <a href="bridgeconstruction.html">Bridge Construction</a> <b>-></b> <a href="questionAttackRound.html">Question Attack</a> <b>-></b> Algorithmic Defence <b>-></b> Algorithmic 2 <b>-></b> End session</center>
                  case RoundAlgorithmicDefenceStage1 => <center><a href="startSession.html">Start</a> <b>-></b><a href="translationRound.html"> Translation </a><b>-></b> <a href="bridgeconstruction.html">Bridge Construction</a> <b>-></b> <a href="questionAttackRound.html">Question Attack</a> <b>-></b> <a href="algorithmicDefenceRound.html">Algorithmic Defence</a> <b>-></b> Algorithmic 2 <b>-></b> End session</center>
                 case RoundAlgorithmicDefenceStage2 =>  <center><a href="startSession.html">Start</a> <b>-></b><a href="translationRound.html"> Translation </a><b>-></b> <a href="bridgeconstruction.html">Bridge Construction</a> <b>-></b> <a href="questionAttackRound.html">Question Attack</a> <b>-></b> <a href="algorithmicDefenceRound.html">Algorithmic Defence</a> <b>-></b> <a href="algorithmicDefenceRoundStage2.html">Algorithmic 2 </a><b>-></b> End session</center>
                  case _                   => <center><a href="startSession.html">Start</a> <b>-></b><a href="translationRound.html"> Translation </a><b>-></b> <a href="bridgeconstruction.html">Bridge Construction</a> <b>-></b> <a href="questionAttackRound.html">Question Attack</a> <b>-></b> <a href="algorithmicDefenceRound.html">Algorithmic Defence</a> <b>-></b> <a href="algorithmicDefenceRoundStage2.html">Algorithmic 2</a> <b>-></b> End session</center>
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

