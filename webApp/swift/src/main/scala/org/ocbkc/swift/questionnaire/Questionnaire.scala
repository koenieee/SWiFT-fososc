/** Provides a Mapper-enabled model for OCBKC questionnaires.
  * Abbreviations:
  *   MC = Multiple Choice
  * 
  */
import _root_.net.liftweb.mapper._

package org.ocbkc.lift.questionnaire
{  object Constants
   {  val MAX_SIZE_QUESTION_TEXT = 1000
      val MAX_SIZE_FREE_TEXT = 1000
   }

   class Questionnaire extends LongKeyedMapper[Questionnaire] with IdPK
   {  // object questions extends MappedLongForeignKey[this, Question] done using join
      def getSingleton = Questionnaire
   }
   
   object Questionnaire extends Questionnaire with LongKeyedMetaMapper[Questionnaire]
   {
   }

   class Questionnaire_Question_join extends LongKeyedMapper[Questionnaire_Question_join] with IdPK
   {  override def getSingleton = Questionnaire_Question_join
      object questionnaire extends MappedLongForeignKey(this, Questionnaire)
      object question extends MappedLongForeignKey(this, Question)
   }

   object Questionnaire_Question_join extends Questionnaire_Question_join with LongKeyedMetaMapper[Questionnaire_Question_join]
   {
   }

   trait Question [T <: ...] extends LongKeyedMetaMapper[T]
   {  object questionText extends MappedString(this.asInstanceOf[Owner], Constants.MAX_FREE_TEXT_SIZE)
   }
/*
   WIW &y2013.06.05.15:22:10& refactoring of datastructure needed:
   - answerSession: for each session a player "does" the questionnaire, an answerSession object is created. It has unique id, AND it refers to a specific player and a specific questionnaire. The session is important, because a specific player may do a questionnaire more than once.
   - answers are all connected to a specific answerSession. And with that you know: which player, which questionnaire, and which session. 
   <&y2013.06.05.15:25:08& how to cope with a survey which is being changed?>
*/

   class QuestionnaireSession extends LongKeyedMapper[QuestionnaireSession] with IdPK
   {  def getSingleton = QuestionnaireSession
      object questionnaire extends MappedLongForeignKey(this, Questionnaire)
      object respondent extends MappedLongForeignKey(this, Player)
   }

   object QuestionnaireSession extends QuestionnaireSession with LongKeyedMetaMapper[QuestionnaireSession]
   {  
   }

/*
   abstract class ClosedQuestion[Owner<:LongKeyedMapper[Owner], T<:LongKeyedMapper[T]](_foreignMeta: => LongKeyedMetaMapper[T] extends Question 
   {  //object correctAnswer extends MappedLongForeignKey(this.asInstanceOf[Owner], )
   }
*/


   /**
     */
   class MultipleChoiceQuestion extends Question[MultipleChoiceQuestion]
   {  def getSingleton = Questionnaire_Question_join
      object minimalNumberOfAnswers extends MappedInt(this)
      object maximumNumberOfAnswers extends MappedInt(this)

      // in this branch temporarily deleted Answers and more stuff
   }
}
