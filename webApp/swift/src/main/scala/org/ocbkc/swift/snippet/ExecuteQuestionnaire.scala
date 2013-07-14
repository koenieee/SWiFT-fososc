package org.ocbkc.swift.snippet
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
//import _root_.net.liftweb.util.BindHelpers._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.questionnaire._
import org.ocbkc.swift.global.Logging._
import net.liftweb.mapper._
import Helpers._

class ExecuteQuestionnaire
{  def renderQuestions(ns: NodeSeq, questionnaire:Questionnaire):NodeSeq = 
   {  val questions:List[Question] = Questionnaire_Question_join.findAll( By(Questionnaire_Question_join.questionnaire, questionnaire)).map( join => join.question.obj.open_! )
      questions.flatMap
      {  question =>
         {  // retrieve additional info based on the type of question
            question.questionType.is match
            {  case 1 =>
               {  log("[MUSTDO] retrieve MultipleChoiceQuestion from DB")
               }

               case 2 =>
               {  log("[MUSTDO] retrieve FreeTextFixedCorrectAnswerQuestion from DB") 
               }
            }
            
            // generate html
            log("[MUSTDO] select template based on question type, now it just assumes FreeTextFixedCorrectAnswerQuestion")
            val questionTemplate = chooseTemplate("top", "question", ns)
            log("   questionTemplate = " + questionTemplate)
            val bindResultQuestionTemplate = bind( "question", questionTemplate,
               "orderNumber"  -> Text("TODO orderNumber"),
               "body"         ->
               {  val ftcqTemplate = chooseTemplate("question", "freeTextClosedQuestion", questionTemplate
                  log("   ftcqTemplate = " + ftcqTemplate )
                  val bindResultFtcqTemplate = bind( "ftcq", ftcqTemplate,
                     "text"      -> Text(question.questionFormulation.is),
                     "answerTf"  -> Text("TODO answerTf here")
                  )
                  log("   ftcqTemplate after bind = " + bindResultFtcqTemplate)
                  bindResultFtcqTemplate
               }
            )
            log("   questionTemplate after bind: " + bindResultQuestionTemplate)
            bindResultQuestionTemplate
         }
      }
   }

   def render(ns: NodeSeq) =
   {//{ TEST: simply pick the first questionnaire
      val qns = Questionnaire.findAll
      val firstQn = qns match
         {  case qn::restQ => qn
            case Nil       => logAndThrow("No test questionnaire found")
         }
   // }

   bind("top", ns,
         "qnName"    -> Text(firstQn.name.is),
         "questions" -> renderQuestions(ns, firstQn),
         "submitBt"  -> Text("TODO submitBt here")
      )
   }
}

}
