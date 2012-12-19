// <&y2012.12.17.12:53:50& move the traits etc. to general library, apart from SWiFT, this is so general.>
/* Notations and conventions
Jn_A_B: join of A and B (conceptually: a class which instances contain a reference to an A and a reference to a B. Join is always 2 place, so disambiguation takes place as follows (by example):

Jn_Jn_A_B_C ==> Jn(Jn(A,b),C)
Jn_A_Jn_B_C ==> Jn(A, Jn(B,C))

OptJn: Option of a join A and B.
*/

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

trait SimEntity
{  val qStart = State("qStart") // there is always at least a Start state
   var current_Jn_Jn_State_Delay_Jn_SimProc_Duration = new Jn_Jn_State_Delay_Jn_SimProc_Duration(new Jn_State_Delay(qStart, 0), None) // state qStart has no process attached to it, and starts immediately.

   var timeAtBeginningCurrentState:TimeInMillis = System.currentTimeMillisVar_simu
   var transitions:Map[State, List[State]] = Map()
   var proposedTransitionTo:Option[DelayedState] = None
   private val doNothing = Unit

   // def transitions_=

   def proposeTransitionTo:Jn_Jn_SimProc_Duration_Delay =
   {  println("proposeTransitionTo called")
      if( timeAtBeginningCurrentState + current_Jn_Jn_State_Delay_Jn_SimProc_Duration.optJn_SimProc_Duration match
            {  case Some(jn_SimProc_Duration) => jn_SimProc_Duration.duration
               case None => 0
            } >
         System.currentTimeMillisVar_simu) // extra check, to see whether previous process has ended. Just in case SimuGod is not infallible... :-P
      proposedTransitionTo = Some(transitions.get(currentState).get.getFirstState) // <&y2012.12.16.22:07:43& .get here ok, or is there some use case that None isn't a bug?>
      val ret = proposedTransitionTo.get  // .get, because None would be a bug.
      println("   proposedTransitionTo = " + ret)
      ret
   }
   
   /** Very important: only call this method if you are certain this is the first transition that is going to take place within the COMPLETE simulation. In that way, the total system state can change consistently, you don't want a *later* transition to take place first.
     * Doesn't update time, this is the responsibiltiy of the calling SimGod, it should do so just before calling this function.
     */
   def transit2proposedTransition =
   {  println("transit2proposedTransition called")
      proposedTransitionTo match
      {  case Some(delayedState) =>
         {  delayedState.state.event match
            {  case Some(event) => runSimProc( event )
               case None        => doNothing
            }
            
            // TODO: move to SimulationGod: SystemWithTesting.currentTimeMillis = SystemWithTesting.startTimeMillis_simu + delayedState.delay // update system clock
            currentState = delayedState.state
            println("   currentState becomes: " + currentState)
         }
         case None               => doNothing
      }
      updateTransitionModel
      Unit
   }

   /** Won't update system clock to account for duration of process: reason is that some other entities may do things
     */
   private def runSimProc(event:SimProc) =
   {  println("runSimProc called")
      println("   time:" + SystemWithTesting.currentTimeMillis )
      println("   event:" + event )
      event.process()
   }   


   def updateTransitionModel

   case class State(val name:String)

   case class Jn_State_Delay(val state:State, val delay: DurationInMillis)

   object Jn_State_DelayGen
   {  var jns_State_DelayGen:List[Jn_State_DelayGen] = Nil
      
      def apply(state:State, delayGen: () => DurationInMillis) =
      {  jns_State_DelayGen = new jns_State_DelayGen(state, delayGen) :: jns_State_DelayGen
      }

      def find(state:State):Jn_State_DelayGen =
      {  jns_State_DelayGen.find(state).get // <&y2012.12.19.15:12:46& for now .get, investigate whether there is a sensible use case with None>
      }
   }

   class Jn_State_DelayGen(val state:State, val delayGen: () => DurationInMillis )
   {  def gen:Jn_State_Delay =
      {  Jn_State_Delay(state, delayGen())
      }
   }

   class Jn_SimProc_DurationGen(val simproc:SimProc, val durationGen: () => DurationInMillis )
   {  def gen:Jn_SimProc_Duration =
      {  Jn_SimProc_Duration(simproc, durationGen())
      }
   }

   class Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen(val jn_State_DelayGen:Jn_State_DelayGen , val optJn_SimProc_DurationGen: Option[Jn_SimProc_DurationGen])
   {  def gen:Jn_Jn_State_Delay_Jn_SimProc_Duration =
      {  Jn_Jn_State_Delay_OptJn_SimProc_Duration(
            jn_State_DelayGen.gen, 
            optJn_SimProc_DurationGen match 
            {  case Some(jn_SimProc_DurationGen) => jn_SimProc_DurationGen.gen
               case None => None
            }
         )
      }
   }

   object Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen
   {  var jns_Jn_State_DelayGen_OptJn_SimProc_DurationGen:List[Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen] = Nil

      def apply(jn_State_DelayGen:Jn_State_DelayGen, optJn_SimProc_DurationGen: Option[Jn_SimProc_DurationGen]) =
      {  jns_Jn_State_DelayGen_OptJn_SimProc_DurationGen = new Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen(jn_State_DelayGen, optJn_SimProc_DurationGen) :: jns_Jn_State_DelayGen_OptJn_SimProc_DurationGen
      }

      def find(state:State):Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen =
      {  val jns_State_DelayGen = Jns_State_DelayGen.find(state).get // <&y2012.12.19.15:12:46& for now .get, investigate whether there is a sensible use case with None>
         jns_Jn_State_DelayGen_OptJn_SimProc_DurationGen.find(jn_State_DelayGen).get
      }
   }  

   class Jn_Jn_State_Delay_OptJn_SimProc_Duration(val jn_State_DelayGen:Jn_State_DelayGen , val optJn_SimProc_DurationGen: Option[Jn_SimProc_DurationGen])
   {  //override def toString = "State( name = " + name + ", simProc = " + simProc + " )"
   }

   

   object State
   {  var states:List[State] = Nil

      def apply(name:String, event:Option[SimProc]) =
      {  val state = new State(name, event)
         states = state :: states
         state
      }

      def find(name:String):Option[State] =
      {  states.find( _.name == name )
      }
   }
   /** @todo <&y2012.12.18.10:35:14& how to refactor to prevent such long names?>
    * Note: the value of the hashmap is the delay-generator (not the duration generator for the SimProc)!
    */
   class StatesWithDelayGenAndjn_SimProc_DurationGen(var delayedStatesMap: HashMap[StateWithWithjn_SimProc_DurationGenWithDelayGen, ()=>DurationInMillis])
   {  def getFirstState:StateWithDelayWithjn_SimProc_DurationGen = 
      {  val firstStateWithjn_SimProc_DurationGenWithDelay = applyDelayFunctions.toList.sortWith{ case ((s1, d1), (s2, d2)) => d1 < d2 }(0) // (0) always possible because empty map is considered to be a bug
         val jn_SimProc_DurationGen = firstStateWithDelayWithjn_SimProc_DurationGen._1
         // now also generator duration of process
         val firstStateWithDelayWithjn_SimProc_Duration = 
         stateWithjn_SimProc_DurationWithDelay.


         println("   firstDelayedStateTuple._2 = " + firstDelayedStateTuple._2)
         StateWithDelay(firstDelayedStateTuple._2, firstDelayedStateTuple._1)
      }

      def applyDelayFunctions() =
      {  println("applyDelayFuctions called")
         val result = delayedStatesMap.map{ case (state, delayFunction) => (state, delayFunction()) }
         println("   delayedStates after applying delay functions:" + result)
         result
      }
   }

   case class StateWithjn_SimProc_DurationWithDelay(state:StateWithjn_SimProc_Duration, delay:DurationInMillis) // &y2012.12.17.22:33:40& WIW: I need to add something extra to represent an actual State with a FIXED duration for its process.
}

/*


*/

class jn_SimProc_DurationGen( process:() => Any, duration:() => DurationInMillis)

/** values in map: a function which produces a delay if it is called. It is a function because it in most cases the delay will not have a fixed duration, but depend on randomness and context info.
  * @todo is an empty map a programming error, or is there a use case for it? For now I assume the first.
  */

class SimPlayer extends SimEntity
{  // create additional states
   val qPlayTranslationSession = State("qPlayTranslationSession")
   val qPlayConstiGame = State("qPlayConstiGame")
   val qUnsubscribe = State("qUnsubscribe")


   // >>> test
   def delayFunction =
   {  (30*1000 + Random.nextInt(60*1000)).toLong
   }
   // <<<

   transitions = 
   Map(
      qStart -> qPlayTranslationSession,
      qPlayTranslationSession -> new DelayedStates( HashMap( (qPlayConstiGame, () => delayFunction) , (qPlayTranslationSession, () => delayFunction) ) )
   )

   // attach delaygenerators to states, processes to states, and durationgenerators to processes.
   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, () => delayFunction), None )
   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayConstiGame, () => delayFunction), None )

   override def updateTransitionModel =
   {  
   }
}

object TestRun
{  
// >>> test
   def no1() =
   {  println("TestRun.no1 called")
      val p1 = new SimPlayer
      println("   start state is " + p1.currentState)
      p1.proposeTransitionTo
      p1.transit2proposedTransition
      p1.proposeTransitionTo
      p1.transit2proposedTransition
   }
// <<<
}
