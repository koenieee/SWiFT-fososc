package org.ocbkc.swift.test.fullsimulation

import org.ocbkc.swift.test._
import org.ocbkc.swift.test.Types._
import org.ocbkc.swift.global.Types._
import scala.collection.immutable.HashMap

trait SimulatedEntity
{  var currentState = State("Start", None) // there is always at least a Start state
   var transitions:Map[State, DelayedStates]
   var proposedTransitionTo:Option[DelayedState]
   private val doNothing = Unit

   // def transitions_=

   def proposeTransitionTo:DelayedState =
   {  proposedTransitionTo = Some(transitions.get(currentState).get.getFirstState) // <&y2012.12.16.22:07:43& .get here ok, or is there some use case that None isn't a bug?>
      proposedTransitionTo.get  // .get, becuase None would be a bug.
   }
   
   /** Very important: only call this method if you are certain this is the first transition that is going to take place within the COMPLETE simulation. In that way, the total system state can change consistently, you don't want a *later* transition to take place first.
     */
   def transit2proposedTransition =
   {  proposedTransitionTo match
      {  case Some(delayedState) =>
         {  delayedState.state.event match
            {  case Some(event) => runSimulatedEvent( (delayedState.delay, event) )
               case None        => doNothing
            }

            currentState = delayedState.state
         }
         case None               => doNothing
      }
      updateTransitionModel
      Unit
   }


   private def runSimulatedEvent(event:DelayedSimulatedEvent) =
   {  println("runSimulatedEvent")
      SystemWithTesting.currentTimeMillis = SystemWithTesting.startTimeMillis_simu + event._1
      println("   time:" + SystemWithTesting.currentTimeMillis )
      println("   event:" + event._2 )
      event._2()
   }   

   def updateTransitionModel

   class State(val name: String, val event: Option[SimulatedEvent])

   object State
   {  var states:List[State] = Nil

      def apply(name:String, event:Option[SimulatedEvent]) =
      {  val state = new State(name, event)
         states = state :: states
         state
      }

      def find(name:String):Option[State] =
      {  states.find( _.name == name )
      }
   }
}


/** values in map: a function which produces a delay if it is called. It is a function because it in most cases the delay will not have a fixed duration, but depend on randomness and context info.
  * @todo is an empty map a programming error, or is there a use case for it? For now I assume the first.
  */
class DelayedStates extends HashMap[State, ()=>DurationInMillis]
{  def getFirstState:DelayedState = 
   {  val firstDelayedStateTuple = applyDelayFunctions.toList.sortWith{ case ((s1, d1), (s2, d2)) => d1 < d2 }(0) // (0) always possible because empty map is considered to be a bug
      DelayedState(firstDelayedStateTuple ._2, firstDelayedStateTuple._1)
   }

   def applyDelayFunctions =
   {  val result = this.mapValues{ delayFunction => delayFunction() }
      result
   }

}

case class DelayedState(delay:DurationInMillis, state:State)

class SimulatedPlayer extends SimulatedEntity
{  // create states
   // val qStart = TODO
   val qPlayTranslationSession = State("qStartTranslationSession")
   val qPlayConstiGame = State("qPlayConstiGame")
   val qUnsubscribe = State("qUnsubscribe")

   // >>> test
   def delayFunction=
   {  random TODO
   }
   // <<<

   transitions = 
   Map(
      qStartTranslationSession -> HashMap( (qPlayConstiGame, delayFunction) , (qPlayTranslationSession, delayFunction)  ) 
   )

   override def updateTransitionModel =
   {  
   }
}
