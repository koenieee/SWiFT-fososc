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
      /* 2 = multiple choice
         1 = free text question with fixed answer
         * 
         * I changed it ~Koen
      */
      object questionFormulation extends MappedString(this, MAX_SIZE_QUESTION_TEXT)

      //{ todo &y2013.07.09.20:57:12& in future replace the following by one reference to any question table. Depending on the questionType, retrieve the correct table. Currenlty, it is not clear to me how to realise that. If I can store the foreignkeys etc. "manually" it should be possible.
      object multipleChoiceQuestion extends MappedLongForeignKey(this, MultipleChoiceQuestion)

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
   class MultipleChoiceQuestion  extends LongKeyedMapper[MultipleChoiceQuestion] with OneToMany[Long, MultipleChoiceQuestion] with IdPK
   {  def getSingleton = MultipleChoiceQuestion
  
      object correctAnswer extends MappedLongForeignKey(MultipleChoiceAnswer)
      object question extends MappedString(this, 200)
      
	object answers extends MappedOneToMany(MultipleChoiceAnswer, MultipleChoiceAnswer.question_id, OrderBy(MultipleChoiceAnswer.id, Ascending))

      // in this branch temporarily deleted Answers and more stuff
   }

   object MultipleChoiceQuestion extends MultipleChoiceQuestion with LongKeyedMetaMapper[MultipleChoiceQuestion]
   {  
   }
   
   class MultipleChoiceAnswer  extends LongKeyedMapper[MultipleChoiceAnswer] with OneToMany[Long, MultipleChoiceAnswer] with IdPK
   {  def getSingleton = MultipleChoiceAnswer
      object question_id extends MappedLongForeignKey(this, MultipleChoiceQuestion)
   object answerFormulation extends MappedString(this, MAX_SIZE_MULTIPLE_OPTION)
      

   }

   object MultipleChoiceAnswer extends MultipleChoiceAnswer with LongKeyedMetaMapper[MultipleChoiceAnswer]
   {  
   }
   

   class FreeTextFixedCorrectAnswerQuestion extends LongKeyedMapper[FreeTextFixedCorrectAnswerQuestion] with IdPK
   {  def getSingleton = FreeTextFixedCorrectAnswerQuestion
      object correctAnswer extends MappedString(this, MAX_SIZE_ANSWER_FreeTextFixedCorrectAnswerQuestion)
   }

   object FreeTextFixedCorrectAnswerQuestion extends FreeTextFixedCorrectAnswerQuestion with LongKeyedMetaMapper[FreeTextFixedCorrectAnswerQuestion]
   {  
   }
}
