// <&y2012.12.17.12:53:50& move the traits etc. to general library, apart from SWiFT, this is so general.>
package org.ocbkc.swift.test.fullsimulation

import org.ocbkc.swift.test._
import org.ocbkc.swift.test.Types._
import org.ocbkc.swift.global.Types._
import scala.collection.immutable.HashMap
import scala.util.Random
import System.out.println

object TestSimulation
{  def main(args: Array[String]) =
   {  println("TestSimulation.main called")
      if( args.length != 0 ) 
         println("Usage: command without arguments")
      else
         TestRun.no1()
   }
}

trait SimulatedEntity
{  val qStart = State("qStart", None) // there is always at least a Start state
   var currentState = qStart
   var transitions:Map[State, DelayedStates] = Map()
   var proposedTransitionTo:Option[DelayedState] = None
   private val doNothing = Unit

   // def transitions_=

   def proposeTransitionTo:DelayedState =
   {  println("proposeTransitionTo called")
      proposedTransitionTo = Some(transitions.get(currentState).get.getFirstState) // <&y2012.12.16.22:07:43& .get here ok, or is there some use case that None isn't a bug?>
      val ret = proposedTransitionTo.get  // .get, because None would be a bug.
      println("   proposedTransitionTo = " + ret)
      ret
   }
   
   /** Very important: only call this method if you are certain this is the first transition that is going to take place within the COMPLETE simulation. In that way, the total system state can change consistently, you don't want a *later* transition to take place first.
     */
   def transit2proposedTransition =
   {  println("transit2proposedTransition called")
      proposedTransitionTo match
      {  case Some(delayedState) =>
         {  delayedState.state.event match
            {  case Some(event) => runSimulatedEvent( (delayedState.delay, event) )
               case None        => doNothing
            }
            
            currentState = delayedState.state
            println("   currentState becomes: " + currentState)
         }
         case None               => doNothing
      }
      updateTransitionModel
      Unit
   }


   private def runSimulatedEvent(event:DelayedSimulatedEvent) =
   {  println("runSimulatedEvent called")
      SystemWithTesting.currentTimeMillis = SystemWithTesting.startTimeMillis_simu + event._1
      println("   time:" + SystemWithTesting.currentTimeMillis )
      println("   event:" + event._2 )
      event._2()
   }   

   def updateTransitionModel

   class State(val name: String, val event: Option[SimulatedEvent])
   {  override def toString = "State( name = " + name + ", event = " + event + " )"
   }

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

   class DelayedStates(var delayedStatesMap: HashMap[State, ()=>DurationInMillis])
   {  def getFirstState:DelayedState = 
      {  val test1 = applyDelayFunctions()
         println("   test1 = " + test1)
         println("   test1 = " + test1)
         println("   test1 = " + test1)
         val test2 = test1.toList
         println("   test2 = " + test2)
         val firstDelayedStateTuple = test2.sortWith{ case ((s1, d1), (s2, d2)) => d1 < d2 }(0) // (0) always possible because empty map is considered to be a bug
         //val firstDelayedStateTuple = applyDelayFunctions.toList.sortWith{ case ((s1, d1), (s2, d2)) => d1 < d2 }(0) // (0) always possible because empty map is considered to be a bug
         println("   firstDelayedStateTuple._2 = " + firstDelayedStateTuple._2)
         DelayedState(firstDelayedStateTuple._2, firstDelayedStateTuple._1)
      }

      def applyDelayFunctions() =
      {  println("applyDelayFuctions called")
         val result = delayedStatesMap.mapValues{ delayFunction => delayFunction() }
         println("   delayedStates after applying delay functions:" + result)
         println("   delayedStates after applying delay functions:" + result)
         println("   delayedStates after applying delay functions:" + result)
         result
      }
   }

   case class DelayedState(delay:DurationInMillis, state:State)
}


/** values in map: a function which produces a delay if it is called. It is a function because it in most cases the delay will not have a fixed duration, but depend on randomness and context info.
  * @todo is an empty map a programming error, or is there a use case for it? For now I assume the first.
  */

class SimulatedPlayer extends SimulatedEntity
{  // create additional states
   val qPlayTranslationSession = State("qPlayTranslationSession", None)
   val qPlayConstiGame = State("qPlayConstiGame", None)
   val qUnsubscribe = State("qUnsubscribe", None)

   // >>> test
   def delayFunction =
   {  (30*1000 + Random.nextInt(60*1000)).toLong
   }
   // <<<

   transitions = 
   Map(
      qStart -> new DelayedStates( HashMap( (qPlayTranslationSession, () => delayFunction) ) ),
      qPlayTranslationSession -> new DelayedStates( HashMap( (qPlayConstiGame, () => delayFunction) , (qPlayTranslationSession, () => delayFunction) ) )
   )

   override def updateTransitionModel =
   {  
   }
}

object TestRun
{  
// >>> test
   def no1() =
   {  println("Run.apply called")
      val p1 = new SimulatedPlayer
      println("   start state is " + p1.currentState)
      p1.proposeTransitionTo
      p1.transit2proposedTransition
      p1.proposeTransitionTo
      p1.transit2proposedTransition
   }
// <<<
}
