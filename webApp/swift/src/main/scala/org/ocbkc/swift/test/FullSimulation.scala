// <&y2012.12.17.12:53:50& move the traits etc. to general library, apart from SWiFT, this is so general.>
/* Notations and conventions
Jn_A_B: join of A and B (conceptually: a class which instances contain a reference to an A and a reference to a B. Join is always 2 place, so disambiguation takes place as follows (by example):

Jn_Jn_A_B_C ==> Jn(Jn(A,b),C)
Jn_A_Jn_B_C ==> Jn(A, Jn(B,C))

OptJn: Option of a join A and B.
*/

import org.ocbkc.swift.test._
import org.ocbkc.swift.test.Types._
import org.ocbkc.swift.global.Types._
import scala.collection.immutable.HashMap
import scala.util.Random
import System.out.println

package org.ocbkc.generic.test.simulation
{

/** The object that runs the complete simulation and has the Power to move the clock, to blow LIFE into the SimuEntities by making them move from one state to the other and executing their processes. They are just all little puppets moving in the light its Light.
  */
object SimuGod
{  def run =
   {  
      // 
      TransitionUtils.getFirst_Jn_Jn_State_Delay_OptJn_SimProc_Duration(SimEntity.simEntities.map{ se => se.proposeTransitionTo })
   }
}

/** Keeps track of all existing SimEntities
  */
object SimEntity
{  var simEntities:List[SimEntity] = Nil

   def register(se:SimEntity) =
   {  println("SimEntity.register called")
      if( simEntities.contains(se) ) throw new RuntimeException("   you tried to register the same simulated entity again, I'm known to be hospitable, but not to that extend, my friend...")
      simEntities = se :: simEntities
   }
}

trait SimEntity
{  val qStart = State("qStart") // there is always at least a Start state. Note: because it is an inner class (State), the State-type belongs to exactly one SmEntity instance.
   var current_Jn_Jn_State_Delay_OptJn_SimProc_Duration = new Jn_Jn_State_Delay_OptJn_SimProc_Duration(new Jn_State_Delay(qStart, 0), None) // state qStart has no process attached to it, and starts immediately.

   var timeAtBeginningCurrentState:TimeInMillis = SystemWithTesting.currentTimeMillis
   var transitions:Map[State, List[State]] = Map()
   var proposedTransitionTo:Option[Jn_Jn_State_Delay_OptJn_SimProc_Duration] = None
   private val doNothing = Unit
   SimEntity.register(this) 

   def proposeTransitionTo:Jn_Jn_State_Delay_OptJn_SimProc_Duration =
   {  println("proposeTransitionTo called")
      val timeAfterCompletionCurrentState = timeAtBeginningCurrentState + current_Jn_Jn_State_Delay_OptJn_SimProc_Duration.duration
      if( timeAtBeginningCurrentState + timeAfterCompletionCurrentState < SystemWithTesting.currentTimeMillis ) // extra check, to see whether previous process has ended. Just in case SimuGod is not infallible..
      {  throw new RuntimeException("   New proposal for transition requested, but I'm not even ready with the previous one!") }
      
      proposedTransitionTo = Some(TransitionUtils.genAndGetFirst_Jn_Jn_State_Delay_OptJn_SimProc_Duration(transitions.get( current_Jn_Jn_State_Delay_OptJn_SimProc_Duration.jn_State_Delay.state).get)) // <&y2012.12.16.22:07:43& .get here ok, or is there some use case that None isn't a bug?>
      val ret = proposedTransitionTo.get  // .get, because None would be a bug.
      println("   proposedTransitionTo = " + ret)
      ret
   }
   
   /** Very important: only call this method if you are certain this is the first transition that is going to take place within the COMPLETE simulation. In that way, the total system state can change consistently, you don't want a *later* transition to take place first.
     * Doesn't update time, this is the responsibiltiy of the calling SimGod, it should do so just before calling this function.
     */
   def doProposedTransition =
   {  println("doProposedTransition called")
      proposedTransitionTo match
      {  case Some(jn_Jn_State_Delay_OptJn_SimProc_Duration) =>
         {  jn_Jn_State_Delay_OptJn_SimProc_Duration.runSimProc
            
            // TODO: move to SimulationGod: SystemWithTesting.currentTimeMillis = SystemWithTesting.pause(delayedState.delay) // update system clock
            current_Jn_Jn_State_Delay_OptJn_SimProc_Duration = proposedTransitionTo.get
            println("   current_Jn_Jn_State_Delay_OptJn_SimProc_Duration becomes: " + current_Jn_Jn_State_Delay_OptJn_SimProc_Duration)
         }
         case None               => doNothing
      }
      updateTransitionModel
      Unit
   }

   def updateTransitionModel

   class State(name:String) extends org.ocbkc.generic.test.simulation.State(name) // <&y2012.12.20.13:53:52& better turn into case class?>[A &y2012.12.20.15:50:23& no because with case classes may create different instances with the same constructor values><&y2012.12.20.23:55:07& this is not a good idea I think (inner class of State : each instance of SimuEntity now gets its own qStart state! Find solution for this.>


   /** @todo exact purpose of this object? Isn't object Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen enough?
     */
   object Jn_State_DelayGen
   {  private var jns_State_DelayGen:List[Jn_State_DelayGen] = Nil
      
      def apply(state:State, delayGen: () => DurationInMillis):Jn_State_DelayGen =
      {  val jn_State_DelayGen = new Jn_State_DelayGen(state, delayGen)
         jns_State_DelayGen = jn_State_DelayGen :: jns_State_DelayGen
         jn_State_DelayGen
      }

      def find(state:State):Jn_State_DelayGen =
      {  jns_State_DelayGen.find{ jn_State_DelayGen => ( jn_State_DelayGen.state == state ) }.get // <&y2012.12.19.15:12:46& for now .get, investigate whether there is a sensible use case with None>
      }
   }

   class Jn_State_DelayGen(val state:State, val delayGen: () => DurationInMillis )
   {  def gen:Jn_State_Delay =
      {  Jn_State_Delay(state, delayGen())
      }
   }

/** Innerclass, because SimProc are specifically tied to this entity.
  *
  */
   class SimProc(val name:String, val function:() => Any) extends org.ocbkc.generic.test.simulation.SimProc(name, function)
   {  def run =
      {  function()
      }
   }

   object Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen
   {  var jns_Jn_State_DelayGen_OptJn_SimProc_DurationGen:List[Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen] = Nil

      def apply(jn_State_DelayGen:Jn_State_DelayGen, optJn_SimProc_DurationGen: Option[Jn_SimProc_DurationGen]) =
      {  jns_Jn_State_DelayGen_OptJn_SimProc_DurationGen = new Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen(jn_State_DelayGen, optJn_SimProc_DurationGen) :: jns_Jn_State_DelayGen_OptJn_SimProc_DurationGen
      }

      def find(state:State):Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen =
      {  jns_Jn_State_DelayGen_OptJn_SimProc_DurationGen.find{ jn => ( jn.jn_State_DelayGen.state == state ) }.get
      }
   }  

   object State
   {  var states:List[State] = Nil

      def apply(name:String) =
      {  val state = new State(name)
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
}

object TransitionUtils
{  def genAndGetFirst_Jn_Jn_State_Delay_OptJn_SimProc_Duration(states:List[State]):Jn_Jn_State_Delay_OptJn_SimProc_Duration = 
   {  println("TransitionUtils.getFirstState")
      val first_Jn_Jn_State_Delay_OptJn_SimProc_Duration = getFirst_Jn_Jn_State_Delay_OptJn_SimProc_Duration(applyTimingFunctions(states))
      println("   first_Jn_Jn_State_Delay_OptJn_SimProc_Duration = " + first_Jn_Jn_State_Delay_OptJn_SimProc_Duration)
      first_Jn_Jn_State_Delay_OptJn_SimProc_Duration
   }

   def getFirst_Jn_Jn_State_Delay_OptJn_SimProc_Duration(jns_Jn_State_Delay_OptJn_SimProc_Duration: List[Jn_Jn_State_Delay_OptJn_SimProc_Duration] ):Jn_Jn_State_Delay_OptJn_SimProc_Duration =
   {  jns_Jn_State_Delay_OptJn_SimProc_Duration.sortWith{ case (jn_Jn_State_Delay_OptJn_SimProc_Duration1, jn_Jn_State_Delay_OptJn_SimProc_Duration2) => ( jn_Jn_State_Delay_OptJn_SimProc_Duration1.jn_State_Delay.delay < jn_Jn_State_Delay_OptJn_SimProc_Duration2.jn_State_Delay.delay ) }(0)
   }

   def applyTimingFunctions(states:List[State]) =
   {  println("applyDelayFuctions called")
      val result = states.map{ s => Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen.find(s).gen }
      println("   delayedStates after applying delay functions:" + result)
      result
   }
}

abstract class State(val name:String) // <&y2012.12.20.13:53:52& better turn into case class?>[A &y2012.12.20.15:50:23& no because with case classes may create different instances with the same constructor values>
{  override def toString = "State( name = " + name + " )"
}

case class Jn_State_Delay(val state:State, val delay: DurationInMillis)
{  //override def toString = "Jn_State_Delay( state = " + state + ", delay = " + delay + ")"
}

class Jn_State_DelayGen(val state:State, val delayGen: () => DurationInMillis )
{  def gen:Jn_State_Delay =
   {  Jn_State_Delay(state, delayGen())
   }
}

abstract class SimProc(val name:String, val function:() => Any)
{  def run =
   {  function()
   }
}

case class Jn_SimProc_Duration(val simProc:SimProc, val duration: DurationInMillis )

class Jn_SimProc_DurationGen(val simProc:SimProc, val durationGen: () => DurationInMillis )
{  def gen:Jn_SimProc_Duration =
   {  Jn_SimProc_Duration(simProc, durationGen())
   }
}

class Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen(val jn_State_DelayGen:Jn_State_DelayGen, val optJn_SimProc_DurationGen: Option[Jn_SimProc_DurationGen])
{  def gen:Jn_Jn_State_Delay_OptJn_SimProc_Duration =
   {  Jn_Jn_State_Delay_OptJn_SimProc_Duration(
         jn_State_DelayGen.gen, 
         optJn_SimProc_DurationGen match 
         {  case Some(jn_SimProc_DurationGen) => Some(jn_SimProc_DurationGen.gen)
            case None => None
         }
      )
   }
}

/** values in map: a function which produces a delay if it is called. It is a function because it in most cases the delay will not have a fixed duration, but depend on randomness and context info.
  * @todo is an empty map a programming error, or is there a use case for it? For now I assume the first.
  */

// <&y2012.12.20.18:11:16& move following to separate file>
}

package org.ocbkc.swift.test.fullsimulation
{
import org.ocbkc.generic.test.simulation._

object TestSimulation
{  def main(args: Array[String]) =
   {  println("TestSimulation.main called")
      if( args.length != 0 ) 
         println("Usage: command without arguments")
      else
         TestRun.no1()
   }
}

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
      qStart -> List(qPlayTranslationSession),
      qPlayTranslationSession -> List(qPlayConstiGame, qPlayTranslationSession)
   )

   // attach delaygenerators to states, processes to states, and durationgenerators to processes.
   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, () => delayFunction), None )
   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayConstiGame, () => delayFunction), None )

   override def updateTransitionModel =
   {  
   }
}

object TestRun
{  def no1() =
   {  println("TestRun.no1 called")
      val p1 = new SimPlayer
      println("   start state is " + p1.current_Jn_Jn_State_Delay_OptJn_SimProc_Duration)
      p1.proposeTransitionTo
      p1.doProposedTransition
      p1.proposeTransitionTo
      p1.doProposedTransition
   }
}
}
