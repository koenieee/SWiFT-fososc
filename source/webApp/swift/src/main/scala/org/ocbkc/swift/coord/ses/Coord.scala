package org.ocbkc.swift.coord
{  
import org.ocbkc.swift.model._
import System._
import org.ocbkc.swift.cores.{TraitGameCore, NotUna}
/* Conventions:
- Names of classes correspond with design $JN/...
- CTL = Computationally Transparent Language
- NL  = Natural Language
*/
package ses
{


import Round._

class Core(/* val player: User */var text: Text, var round: Round)
{  var startTime: Long = 0
   var totalPlayingTime: Long = 0
   var translation: String = ""
   val gameCore: TraitGameCore = new NotUna()
   var qaad: QuestionAttackAndDefence = null

   // Communication with User Interface
   // UR = User Request
   // user requests to prepare session
   def URprepare = 
   {  
   }

   def URtranslation:Text =  
   {  startTime = 3274325 // <&y2011.11.16.18:24:22& insert NOW>
      err.println("class Core: just set startTime to " + startTime)
      round = Trans
      text = gameCore.generateText
      text
   }

   def URquestionAttackAndDefence:QuestionAttackAndDefence = 
   {  qaad = gameCore.generateQuestionAndAnswers(text.NL, translation)
      qaad
   }

   def URalgorithmicDefence:QuestionAttackAndDefence =
   {  // algorithmic defince has already been carried out 'tacitly' during URquestionAttackAndDefence, therefore just return qaad.
      qaad
   }
}

/* Assumptions and conventions regarding UI:
- UI is an abstract layer around the actual UI implementation. Or perhaps better: is a kind of API between the Session coordinator and the implementation of the UI. This allows the specific UI solution (web based, OS-based, etc.) to be changed when required. Only the definition of the methods in the UI object have to be changed, without having to make changes to the ses.Core class.
*/



}
}
