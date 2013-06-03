/** Abbreviations:
  *   MC = Multiple Choice
  */

package org.ocbkc.questionnaire
{  class Questionnaire extends List[Question]
   {
   }

   trait Question

   trait Answer
   {  var Question = 
   }

   abstract case class MC_Option(val code, val text)
   {  
   }

   class FreeTextAnswer extends Answer
   {  
   }

   trait ClosedQuestion extends Question
   {  def correctAnswer:Option[Answer]
   }

   /**
     */
   class MultipleChoiceQuestion extends ClosedQuestion
   {  var minimalNumberOfAnswers:Int = 1
      var maximumNumberOfAnswers:Int = 1

      case class MC_Option // an innner class MC_Option forces all code in this class which uses MC_Option to only contain options of THIS MultipleChoiceQuestion instance...
      class MC_Answer extends List[MC_Option] with Answer
      var options:List[MC_Option] = Nil

      private var _correctAnswer:MC_Answer

      override def correctAnswer:Option[MC_Answer] =
      {  _correctAnswer match
         {  case Nil          => None
            case Some(lmco)   => Some(lmco)
         }
      }

      def correctAnswer_=(mca:MC_Answer) =
      {  _correctAnswer = mca
      }
   }

   class FreeTextClosedQuestion extends ClosedQuestion
   {  
   }
}
