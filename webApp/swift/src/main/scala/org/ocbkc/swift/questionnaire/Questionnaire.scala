/** Abbreviations:
  *   MC = Multiple Choice
  */

package org.ocbkc.questionnaire
{  class Questionnaire
   {  var questions:List[Question] = Nil
   }

   trait Question
   {  var respondentsAnswer:Option[Answer] = None
   }

   trait Answer
   {  var Question:Question
   }

   abstract case class MC_Option(val code:String, val text:String)
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
      class MC_Answer extends Answer
      {  var options:List[MC_Option] = Nil
      }

      var options:List[MC_Option] = Nil // not to confuse with Scala's Option. This is a multiple choice option.

      private var _correctAnswer:MC_Answer = new MC_Answer

      /** @return None (correctAnswer is unknown), or Some(non-empty list of MC_Options) as answer.
        */
      override def correctAnswer:Option[MC_Answer] =
      {  if(_correctAnswer.options.isEmpty)
            None
         else
            Some(_correctAnswer)
      }

      def correctAnswer_=(mca:MC_Answer) =
      {  _correctAnswer = mca
      }
   }

   case class FreeTextAnswer extends Answer
   {  val text:String
   }

   trait FreeTextQuestion extends Question
   {  private var _correctAnswer:FreeTextAnswer = new FreeTextAnswer

      /** @return None (correctAnswer is unknown), or Some(non-empty list of MC_Options) as answer.
        */
      override def correctAnswer:Option[MC_Answer] =
      {  if(_correctAnswer.options.isEmpty)
            None
         else
            Some(_correctAnswer)
      }

      def correctAnswer_=(mca:MC_Answer) =
      {  _correctAnswer = mca
      }

   }

   class FreeTextClosedQuestion extends FreeTextQuestion with ClosedQuestion
   {
   }

   /** FreeTextAnswer applies to both FreeTextClosedQuestion and FreeTextOpenQuestion
     */
   case class FreeTextAnswer(question:FreeTextQuestion) extends Answer
   {
   }
}
