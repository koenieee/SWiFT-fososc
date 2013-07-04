/** Provides a Mapper-enabled model for OCBKC questionnaires.
  * Abbreviations:
  *   MC = Multiple Choice
  * 
  */

package org.ocbkc.lift.questionnaire
{  object Constants
   {  val MAX_SIZE_QUESTION_TEXT = 1000
      val MAX_SIZE_FREE_TEXT = 1000
   }

   class Questionnaire
   {  var questions:List[Question] = Nil
   }

   class Questionnaire_Question_join extends LongKeyedMetaMapper[Questionnaire_Question_join] with IdPK
   {  def getSingleton = Questionnaire_Question_join
      object questionnaire extends 
      object question extends 
   }

   trait Question extends [Owner<:LongKeyedMapper[Owner], T<:LongKeyedMapper[T]](_foreignMeta: => LongKeyedMetaMapper[T] 
   {  object questionText extends MappedString(this.asInstanceOf[Owner], Constants.MAX_FREE_TEXT_SIZE)
   }
/*
   WIW &y2013.06.05.15:22:10& refactoring of datastructure needed:
   - answerSession: for each session a player "does" the questionnaire, an answerSession object is created. It has unique id, AND it refers to a specific player and a specific questionnaire. The session is important, because a specific player may do a questionnaire more than once.
   - answers are all connected to a specific answerSession. And with that you know: which player, which questionnaire, and which session. 
   <&y2013.06.05.15:25:08& how to cope with a survey which is being changed?>
*/
   trait Answer [Owner<:LongKeyedMapper[Owner], T<:LongKeyedMapper[T]](_foreignMeta: => LongKeyedMetaMapper[T]
   {  object question extends MappedLong(this.asInstanceOf[Owner], Question)
      object questionnaireSession extends MappedLong(this.asInstanceOf[Owner], QuestionnaireSession)
   }

   class QuestionnaireSession extends LongKeyedMapper[QuestionnaireSession] with IdPK
   {  def getSingleton = QuestionnaireSession
      object questionnaire extends MappedLongForeignKey(this, questionnaire
      object respondent extends MappedLongForeignKey(this, Player)
   }

   object QuestionnaireSession extends QuestionnaireSession with LongKeyedMetaMapper[QuestionnaireSession]
   {  
   }

   trait Question_
   abstract case class MC_Option(val code:String, val text:String)
   {
   }

   trait ClosedQuestion[Owner<:LongKeyedMapper[Owner], T<:LongKeyedMapper[T]](_foreignMeta: => LongKeyedMetaMapper[T] extends Question 
   {  object correctAnswer extends MappedLongForeignKey(this.asInstanceOf[Owner], )
   }

   /**
     */
   class MultipleChoiceQuestion extends ClosedQuestion with LongKeyedMapper[QuestionnaireSession] with IdPK 
   {  object minimalNumberOfAnswers extends MappedInt[this]
      object maximumNumberOfAnswers extends MappedInt[this]

      case class MC_Option(id:String, text:String)      class MC_Answer extends Answer
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
