/** Provides a Mapper-enabled model for OCBKC questionnaires.
  * Abbreviations:
  *   MC = Multiple Choice
  * 
  */
import _root_.net.liftweb.mapper._
import org.ocbkc.swift.model._

package org.ocbkc.questionnaire
{  object Constants
   {  val MAX_SIZE_QUESTION_TEXT = 1000
      val MAX_SIZE_ANSWER_FreeTextFixedCorrectAnswerQuestion = 1000
      val MAX_SIZE_NAME_QUESTIONNAIRE = 250
      val MAX_SIZE_MULTIPLE_OPTION = 500
   }

   import Constants._

   class Questionnaire extends LongKeyedMapper[Questionnaire] with IdPK
   {  // object questions extends MappedLongForeignKey[this, Question] done using join
      def getSingleton = Questionnaire
      object name extends MappedString(this, MAX_SIZE_NAME_QUESTIONNAIRE)
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
   {  def createJoin(questionnaire:Questionnaire, question:Question):Questionnaire_Question_join =
      {  val j = this.create.questionnaire(questionnaire).question(question)
         j.save
         j
      }
   }

   class Question extends LongKeyedMapper[Question] with IdPK
   {  def getSingleton = Question
      object questionType extends MappedLong(this)
      /* 1 = multiple choice
         2 = free text question with fixed answer
      */
      object questionFormulation extends MappedString(this, MAX_SIZE_QUESTION_TEXT)

      //{ todo &y2013.07.09.20:57:12& in future replace the following by one reference to any question table. Depending on the questionType, retrieve the correct table. Currenlty, it is not clear to me how to realise that. If I can store the foreignkeys etc. "manually" it should be possible.
      object multipleChoiceQuestion extends MappedLongForeignKey(this, MultipleChoiceQuestion)
      object multipleChoiceOptions extends MappedLongForeignKey(this, MultipleChoiceOption)
      object freeTextFixedCorrectAnswerQuestion extends MappedLongForeignKey(this, FreeTextFixedCorrectAnswerQuestion)
      //}
   }
/*
   WIW &y2013.06.05.15:22:10& refactoring of datastructure needed:
   - answerSession: for each session a player "does" the questionnaire, an answerSession object is created. It has unique id, AND it refers to a specific player and a specific questionnaire. The session is important, because a specific player may do a questionnaire more than once.
   - answers are all connected to a specific answerSession. And with that you know: which player, which questionnaire, and which session. 
   <&y2013.06.05.15:25:08& how to cope with a survey which is being changed?>
*/
   object Question extends Question with LongKeyedMetaMapper[Question]
   {
   }


   class QuestionnaireSession extends LongKeyedMapper[QuestionnaireSession] with IdPK
   {  def getSingleton = QuestionnaireSession
      object questionnaire extends MappedLongForeignKey(this, Questionnaire)
      object respondent extends MappedLongForeignKey(this, Player)
   }

   object QuestionnaireSession extends QuestionnaireSession with LongKeyedMetaMapper[QuestionnaireSession]
   {  
   }
   /**
     */
   class MultipleChoiceQuestion  extends LongKeyedMapper[MultipleChoiceQuestion] with IdPK
   {  def getSingleton = MultipleChoiceQuestion
      object correctAnswer extends MappedInt(this) // 1 = option 1, 2 = option 2
      object minimalNumberOfAnswers extends MappedInt(this)
      object maximumNumberOfAnswers extends MappedInt(this)

      // in this branch temporarily deleted Answers and more stuff
   }

   object MultipleChoiceQuestion extends MultipleChoiceQuestion with LongKeyedMetaMapper[MultipleChoiceQuestion]
   {  
   }
   
   class MultipleChoiceOption  extends LongKeyedMapper[MultipleChoiceOption] with IdPK
   {  def getSingleton = MultipleChoiceOption
      object options extends MappedString(this, MAX_SIZE_MULTIPLE_OPTION) // answer 1;answer2;etc.


   }

   object MultipleChoiceOption extends MultipleChoiceOption with LongKeyedMetaMapper[MultipleChoiceOption]
   {  
   }
   
   class MultipleChoiceOption_join extends LongKeyedMapper[MultipleChoiceOption_join] with IdPK
   {  override def getSingleton = MultipleChoiceOption_join
      object options extends MappedLongForeignKey(this, MultipleChoiceOption)

   }

   object MultipleChoiceOption_join extends MultipleChoiceOption_join with LongKeyedMetaMapper[MultipleChoiceOption_join]
   {  def createJoin(opi:MultipleChoiceOption):MultipleChoiceOption_join =
      {  val j = this.create.options(opi)
         j.save
         j
      }
   }
   

   class FreeTextFixedCorrectAnswerQuestion extends LongKeyedMapper[FreeTextFixedCorrectAnswerQuestion] with IdPK
   {  def getSingleton = FreeTextFixedCorrectAnswerQuestion
      object correctAnswer extends MappedString(this, MAX_SIZE_ANSWER_FreeTextFixedCorrectAnswerQuestion)
      object maximumNumberOfAnswers extends MappedInt(this)
   }

   object FreeTextFixedCorrectAnswerQuestion extends FreeTextFixedCorrectAnswerQuestion with LongKeyedMetaMapper[FreeTextFixedCorrectAnswerQuestion]
   {  
   }

}
