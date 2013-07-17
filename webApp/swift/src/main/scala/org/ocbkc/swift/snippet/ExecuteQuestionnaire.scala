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
      val ret = questions.flatMap
      {  question =>
         {  // generate html
            val questionTemplate = chooseTemplate("top", "question", ns)
            log("   questionTemplate = " + questionTemplate)
            val bindResultQuestionTemplate = bind( "question", questionTemplate,
               "orderNumber"  -> Text("TODO orderNumber"),
               "body"         ->
               {  // retrieve additional info based on the type of question
                                   
                  question.questionType.is match
                  {  case 1 =>
                     {  Text("[MUSTDO] retrieve MultipleChoiceQuestion from DB, and render (see 'case 2' for the structure of the code.)")
                     }

                     case 2 =>
                     {  val questionTemplates = chooseTemplate("top", "questionTemplates", TemplateFinder.findAnyTemplate(List("questionnaire", "question")).open_!)
                        log("   questionTemplates = " + questionTemplates)
                        val ftcqTemplate = chooseTemplate("question", "freeTextClosedQuestion", questionTemplates)

                        log("   ftcqTemplate = " + ftcqTemplate )
                        val bindResultFtcqTemplate = bind( "ftcq", ftcqTemplate,
                           "text"      -> Text(question.questionFormulation.is),
                           "answerTf"  -> Text("TODO answerTf here")
                        )
                        log("   ftcqTemplate after bind = " + bindResultFtcqTemplate)
                        bindResultFtcqTemplate
                     }
                  }

               }
            )
            log("   questionTemplate after bind: " + bindResultQuestionTemplate)
            bindResultQuestionTemplate
         }
      }
      log("   renderQuestions return value = " + ret)
      ret
   }

   def render(ns: NodeSeq) =
   {//{ TEST: simply pick the first questionnaire
      val qns = Questionnaire.findAll
      val firstQn = qns match
         {  case qn::restQ => qn
            case Nil       => logAndThrow("No test questionnaire found")
         }
    //}

   bind("top", ns,
         "qnName"    -> Text(firstQn.name.is),
         "question"  -> renderQuestions(ns, firstQn),
         "submitBt"  -> Text("TODO submitBt here")
      )
   }
}

}
