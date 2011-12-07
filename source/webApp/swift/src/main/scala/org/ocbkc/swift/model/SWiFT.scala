package org.ocbkc.swift.model
{

/*
class Source extends Enumeration
{  type Source = Value
   val SOURCE, TRANSLATION = Value   
}
*/

object Round extends Enumeration
{  type Round = Value
   val NotStarted, Trans, Qattack, Qdefence = Value
}

/* NL  = natural language
   CTL = computationally transparent language
*/
class Answer(var NL:String,  var CTL:String)
{
}

class Question(var NL:String, var CTL:String)
{
}

class QuestionAttackAndDefence(var question: Question, var answerFromSource: Answer, var answerFromTrans: Answer)
{  
}

class Text(var NL:String, var CTL:CTLtranslation)
{
}

class CTLtranslation(var player:String, var computer:String)
{
}

// SessionBundle bundles the complete core playing material for playing a given session
class SessionBundle(var text:Text, var qaad: QuestionAttackAndDefence )
{
}

}
