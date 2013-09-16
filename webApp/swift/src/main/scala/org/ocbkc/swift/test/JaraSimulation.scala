// <&y2012.12.17.12:53:50& move the traits etc. to general library, apart from SWiFT, this is so general.>
/* Notations and conventions
Jn_A_B: join of A and B (conceptually: a class which instances contain a reference to an A and a reference to a B. Join is always 2 place, so disambiguation takes place as follows (by example):

Jn_Jn_A_B_C ==> Jn(Jn(A,b),C)
Jn_A_Jn_B_C ==> Jn(A, Jn(B,C))

OptJn: Option of a join A and B.

Convention:

SimGod never notifies entities that another entity has started a state, while this might seem useful in some cases. However, SimGod assumes you to break a state into smaller states if you want to "notify" other entities that a certain entity started to get involved in a certain process. The philosophy is that this approaches reality in which it also may take some time before another entity notices changes in the other entities states.



*/

import org.ocbkc.swift.test._
import org.ocbkc.swift.test._
import org.ocbkc.swift.global.Types._
import scala.collection.immutable.HashMap
import scala.util.Random
import System.out.println
import org.ocbkc.swift.global.TestSettings
import org.ocbkc.generic.DateTime._

/** The jara simulation system (C) Chide Groenouwe.
  * based on finite state machines with probabilisticly delayed transitions and processes attached to states.
  */

package org.ocbkc.generic.test.simulation.jara
{

object AuxiliaryDefs
{  val doNothing = Unit
}

import AuxiliaryDefs._

/** The object that runs the complete simulation and has the Power to move the clock, to blow LIFE into the SimuEntities by making them move from one state to the other and executing their processes. They are just all little puppets moving in the light its Light.

Assumptions: as long as an entity is occupied it is not 
  */

object SimGod
{  val debug = true
   var startTimeCurrentRun:Option[POSIXtime] = None
   var endTimeLastRun:Option[POSIXtime] = None

   def resetJara = 
   {  SimEntity.reset
      startTimeCurrentRun = None
      endTimeLastRun = None
   }

   def sortByStop(sjs:List[(SimEntity, Jn_Start_Stop)]) =
   {  sjs.sortWith{ case ((_, Jn_Start_Stop(_,stop1)), (_, Jn_Start_Stop(_,stop2))) => stop1 < stop2 }
   }

   def sortByStart(sjs:List[(SimEntity, Jn_Start_Stop)]) =
   {  sjs.sortWith{ case ((_, Jn_Start_Stop(start1,_)), (_, Jn_Start_Stop(start2,_))) => start1 < start2 }
   }
   /** Conditions: the start time of the simulation is equal to the moment the run method is called.
       @param runConditionHolds iteration has value 0 before the execution of the first loop (and check of the condition). So if you want zero runs, produce false if iteration == 0.
     */

   def run( runConditionHolds:(Long, TimeInMillis) => Boolean ) =
   {  startTimeCurrentRun = Some(SystemWithTesting.currentTimeMillis)
      var unoccupiedEntitiesWithoutProposedActivities:List[SimEntity] = SimEntity.newSimEntities; SimEntity.newSimEntities = Nil
      //val uEWPA = unoccupiedEntitiesWithoutProposedActivities // abbreviations <&y2012.12.26.17:32:26& will not work like this, how will they?>
      var unoccupiedEntitiesWithProposedActivities:List[(SimEntity, Jn_Start_Stop)] = Nil // Requirement: always sorted by start time
      //val uEWPA = unoccupiedEntitiesWithProposedActivities
      var occupiedEntities_Jn_Start_Stop:List[(SimEntity, Jn_Start_Stop)] = Nil // Requirement: always sorted by stop time
      var environmentChange = false
      //val oEJSS = occupiedEntities_Jn_Start_Stop
      TestSettings.SIMULATECLOCK = true

      var iteration = 0

      while( runConditionHolds(iteration, SystemWithTesting.currentTimeMillis) )
      {  environmentChange = false
         iteration += 1
         // Ask all non-busy entities when they want to go to their next state, and pick the one that wants to do so earliest.
         println(" \n################ Iteration " + iteration + "\n")
         if( SimEntity.newSimEntities != Nil )
         {  unoccupiedEntitiesWithoutProposedActivities = unoccupiedEntitiesWithoutProposedActivities ++ SimEntity.newSimEntities
            SimEntity.newSimEntities = Nil
         }
         //println("  (current time, time since last clock-change) = " + SystemWithTesting.currentTimeMillis)
         println("  unoccupiedEntitiesWithoutProposedActivities = " + unoccupiedEntitiesWithoutProposedActivities)
         println("  unoccupiedEntitiesWithProposedActivities = " + unoccupiedEntitiesWithProposedActivities)
         println("  occupiedEntities_Jn_Start_Stop = " + occupiedEntities_Jn_Start_Stop) // WIW rename occupiedEntitiesWithFirstStopTime to occupiedEntities_Jn_Start_Stop, only in this init of the itteration! &y2012.12.29.01:09:32&
         if(debug)
         {  val  occupiedEntities = occupiedEntities_Jn_Start_Stop.map{ case (u,_) => u }
            val unoccupiedEntitiesWithProposedActivitiesStripped = unoccupiedEntitiesWithProposedActivities.map{ case (u,_) => u }

            if( occupiedEntities.intersect( unoccupiedEntitiesWithProposedActivitiesStripped ) != Nil )
            {  throw new RuntimeException("occupiedEntities and unoccupiedEntitiesWithProposedActivities are not disjoint") 
            }else
            {  println("occupiedEntities and unoccupiedEntitiesWithProposedActivities are disjoint")
            }

            if( unoccupiedEntitiesWithProposedActivitiesStripped.intersect{ unoccupiedEntitiesWithoutProposedActivities } != Nil )
            {  throw new RuntimeException("unoccupiedEntitiesWithProposedActivities and unoccupiedEntitiesWithoutProposedActivities are not disjoint, that just feels SO wrong... Hmm, it even IS wrong!")
            }

            if( occupiedEntities.intersect{ unoccupiedEntitiesWithoutProposedActivities } != Nil )
            {  throw new RuntimeException("occupiedEntities and unoccupiedEntitiesWithProposedActivities are not disjoint, that just feels SO wrong... Hmm, it even IS wrong!")
            }

            if( ( occupiedEntities ++ unoccupiedEntitiesWithProposedActivitiesStripped ++ unoccupiedEntitiesWithoutProposedActivities ).toSet != SimEntity.simEntities.toSet )
            {  throw new RuntimeException("oh, you make may day, baby, yet another error to punish you SLAP SLAP SLAP: somehow SimuEntities have dropped of SimuGod's radar...")
            }
         }

         // 1. If there are unoccupied entities without proposed future activities, first ask them to propose these.
         if( unoccupiedEntitiesWithoutProposedActivities != Nil )
         {  unoccupiedEntitiesWithProposedActivities = sortByStart( unoccupiedEntitiesWithProposedActivities ++ unoccupiedEntitiesWithoutProposedActivities.map{ se => (se, se.proposeTransition.toJn_Start_Stop) } )
            unoccupiedEntitiesWithoutProposedActivities = Nil
         }

         // 2. Determine the first coming event(s). These can be 2 things: completion of activity of one or more occupied entities, and/or the start of a proposed activity of an unoccupied entity. (Note: it is important to explicitly look at the list of UNoccupied entities, you don't want to start an entity which started during the last itteration to be started again!). [A] Move the clock to the time of the first coming event(s).  If these 2 things just mentioned coincide, do them both, in the following order: [B] first start the unoccupied entities and then "stop" the occupied entities (order because assumption is that system state changes by the first cannot influence the just occupied entities, because the time is to short (=0!). This is a random assumption, could also have decided the opposite, the point is that a choice must be made!). Completion of activity means moving the entity to the unoccupied entities list (without proposed future activities). Starting an unoccupied entity means calling its transition method, and then [MOV2OC] moving it to the list of occupied entities.
            
         val firstStartTimeUnoccupiedEntities:Option[POSIXtime] = if( unoccupiedEntitiesWithProposedActivities != Nil ) Some(unoccupiedEntitiesWithProposedActivities(0)._2.start) else None
         println("  firstStartTimeUnoccupiedEntities = " + firstStartTimeUnoccupiedEntities)
         val firstStopTimeOccupiedEntities:Option[POSIXtime] = if( occupiedEntities_Jn_Start_Stop != Nil) Some(occupiedEntities_Jn_Start_Stop(0)._2.stop) else None
         println("  firstStopTimeOccupiedEntities = " + firstStopTimeOccupiedEntities )

         firstStartTimeUnoccupiedEntities match
         {  case Some(fSTUE) =>
            {  firstStopTimeOccupiedEntities match
               {  case Some(fSTOE) =>  if( fSTUE < fSTOE )
                                       {  // move clock [A]
                                          SystemWithTesting.currentTimeMillis = fSTUE
                                          startUnoccupiedEntitiesWithFirstStartTime(fSTUE)
                                          // [B]

                                       } else if( fSTUE == fSTOE )
                                       {  SystemWithTesting.currentTimeMillis = fSTUE
                                          startUnoccupiedEntitiesWithFirstStartTime(fSTUE)
                                          stopOccupiedEntitiesWithFirstStopTime(fSTUE)
                                       } else // fSTUE > fSTOE
                                       {  SystemWithTesting.currentTimeMillis = fSTOE
                                          stopOccupiedEntitiesWithFirstStopTime(fSTOE)
                                       }
                  case None        =>  {  SystemWithTesting.currentTimeMillis = fSTUE
                                          startUnoccupiedEntitiesWithFirstStartTime(fSTUE)
                                       }
               }
            }
            case None        =>
            {  firstStopTimeOccupiedEntities match
               {  case Some(fSTOE)  => {  SystemWithTesting.currentTimeMillis = fSTOE
                                          stopOccupiedEntitiesWithFirstStopTime(fSTOE)
                                       }
                  case None         => throw new RuntimeException("There is no firstStartTimeUnoccupiedEntities and no firstStopTimeOccupiedEntities, that is a bug, dude...")
               }
            }
         }

         /** Assumption is that clock has already been moved to the right time
           */
         def startUnoccupiedEntitiesWithFirstStartTime(firstStartTimeUnoccupiedEntities:POSIXtime) =
         {  val unoccupiedEntitiesWithFirstStartTime = unoccupiedEntitiesWithProposedActivities.takeWhile{ case (_, Jn_Start_Stop(start,_)) => (start == firstStartTimeUnoccupiedEntities) }
            unoccupiedEntitiesWithFirstStartTime.map
            {  u => u._1.doProposedTransition
            }
            // [MOVE2OC]
            unoccupiedEntitiesWithProposedActivities = unoccupiedEntitiesWithProposedActivities -- unoccupiedEntitiesWithFirstStartTime
            occupiedEntities_Jn_Start_Stop = sortByStop( occupiedEntities_Jn_Start_Stop ++ unoccupiedEntitiesWithFirstStartTime )
            environmentChange = true
         }

         def stopOccupiedEntitiesWithFirstStopTime(firstStopTimeOccupiedEntities:POSIXtime) =
         {  val occupiedEntitiesWithFirstStopTime = occupiedEntities_Jn_Start_Stop.takeWhile{ case (_, Jn_Start_Stop(_,stop)) => (stop == firstStopTimeOccupiedEntities) }
            occupiedEntitiesWithFirstStopTime.foreach{ case (se, _) => se.finishProcess }

            unoccupiedEntitiesWithoutProposedActivities = unoccupiedEntitiesWithoutProposedActivities ++ occupiedEntitiesWithFirstStopTime.map{ case (o,_) => o }
            occupiedEntities_Jn_Start_Stop = occupiedEntities_Jn_Start_Stop -- occupiedEntitiesWithFirstStopTime
            environmentChange = true
         }

         // 3. If in step 2 an occupied entity reached completion, the proposed activities of unoccupied entities must be erased (because the occupied entity which just finished may change the system state and thus the decisions of these unoccupied entities about future activities).
         if( environmentChange )
         {  unoccupiedEntitiesWithoutProposedActivities = unoccupiedEntitiesWithoutProposedActivities ++ unoccupiedEntitiesWithProposedActivities.map{case (u,_) => u }
            unoccupiedEntitiesWithProposedActivities = Nil
         }
      }

      endTimeLastRun = Some(SystemWithTesting.currentTimeMillis)

      println("   Simulation finished. Stats:")
      println("      iterations = " + iteration)
      println("      startTime simulation = " + timeInMillis2dateString( startTimeCurrentRun.get ) )
      println("      endTime simulation = " + timeInMillis2dateString( endTimeLastRun.get ) )

      startTimeCurrentRun = None
   }
}

/** Keeps track of all existing SimEntities
  */
object SimEntity
{  var simEntities:List[SimEntity] = Nil
   var newSimEntities:List[SimEntity] = Nil // simentities which have not been "seen" by  SimGod. SimGod empties this list after he has seen them. Use case: if new entities are created during the simulation.

   def register(se:SimEntity) =
   {  println("SimEntity.register called")
      if( simEntities.contains(se) ) throw new RuntimeException("You tried to register the same simulated entity again, I'm known to be hospitable, but not to that extend, my friend...")
      simEntities = se :: simEntities
      newSimEntities = se :: newSimEntities
   }

   def reset =
   {  simEntities    = Nil
      newSimEntities = Nil
   }
}

case class Jn_Delay_Duration(delay:TimeInMillis, duration:TimeInMillis)
{  def toJn_Start_Stop =
   {  Jn_Start_Stop(SystemWithTesting.currentTimeMillis + delay, SystemWithTesting.currentTimeMillis + delay + duration)
   }
}


/** @param start: absolute starting time
  * @param stop: absolute stop time
  */
case class Jn_Start_Stop(start:POSIXtime, stop:POSIXtime)
{
}

trait SimEntity
{  val qStart = State("qStart") // there is always at least a Start state. Note: because it is an inner class (State), the State-type belongs to exactly one SmEntity instance.
   var current_Jn_Jn_State_Delay_OptJn_SimProc_Duration = new Jn_Jn_State_Delay_OptJn_SimProc_Duration(new Jn_State_Delay(qStart.asInstanceOf[org.ocbkc.generic.test.simulation.jara.State], 0), None) // state qStart has no process attached to it, and starts immediately.
   def currentState = { val cS = current_Jn_Jn_State_Delay_OptJn_SimProc_Duration.state.asInstanceOf[this.State]; println("currentState = " + cS); cS }

   var timeAtBeginningCurrentState:TimeInMillis = SystemWithTesting.currentTimeMillis
   var transitions:Map[State, List[State]] = Map()
   var proposedTransitionTo:Option[Jn_Jn_State_Delay_OptJn_SimProc_Duration] = None
   SimEntity.register(this)

   // historical info. about past processes/states (COULDDO &y2013.01.09.16:40:20& perhaps refactor, make a generic datastructure for this purpose? Currently only things needed so far can be found bac.
   var totalDurations:Map[State, DurationInMillis] = Map()
   var lastFinishTimeStateMap:Map[State, Option[POSIXtime]] = Map() // convention (for now): this contains the finishtime from the moment the process starts. It is updated as soon as the currently running process is finished. So, keep into account that it contains the finish time of the PREVIOUS process, if the current process is still running.

   var finishProcessCalled = false
   /** Instance of subclass of this trait should always call this method after all states have been created.
     * @todo (done &y2013.01.19.18:12:52&) &y2013.01.19.15:24:11& BUG: I now use the currernt time to initialise the lastFinishTimeStateMap, however, the time SHOULD be the time from which the desired ratio came into effect. A possible solution is to make it an option, and put in None. If the latter is the case, other parts of the program can act accordingly, for example assuming the time from which the desired ratio came into effect, instead of the lastFinishTimeState. This is also a more true representation, since there was no last finish time yet.
     */
   def initialisationAfterStateDefs =
   {  totalDurations = State.states.map( s => (s,0L) ).toMap
      lastFinishTimeStateMap = State.states.map( s => (s, None) ).toMap
   }

   /** @returns delay before proposed state
     */
   def proposeTransition:Jn_Delay_Duration =
   {  println(this + ".proposeTransition called")
      val timeAfterCompletionCurrentState = timeAtBeginningCurrentState + current_Jn_Jn_State_Delay_OptJn_SimProc_Duration.duration
      if( timeAtBeginningCurrentState + timeAfterCompletionCurrentState < SystemWithTesting.currentTimeMillis ) // extra check, to see whether previous process has ended. Just in case SimGod is not infallible..
      {  throw new RuntimeException("   New proposal for transition requested, but I'm not even ready with the previous one!") }

      proposedTransitionTo = Some(TransitionUtils. getFirst_Jn_Jn_State_Delay_OptJn_SimProc_Duration(
                                                      applyTimingFunctions(
                                                         transitions.get( currentState ).getOrElse(throw new RuntimeException("Transition table seems to be incomplete, check if the state " + currentState + " occurs at the right hand side of a transition in the transition table."))
                                                      )
                                                   )
                                 ) // <&y2012.12.16.22:07:43& .get here ok, or is there some use case that None isn't a bug?>
      val ret = proposedTransitionTo.get  // .get, because None would be a bug.
      println("   proposedTransitionTo = " + ret)
      Jn_Delay_Duration(ret.jn_State_Delay.delay, ret.duration)
   }
   
   def applyTimingFunctions(states:List[State]) =
   {  println(this + ".applyDelayFuctions called")
      val result = states.map{ s => Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen.find(s).gen }
      println("   delayedStates after applying delay functions:" + result)
      result
   }

   /** Very important: only call this method if you are certain this is the first transition that is going to take place within the COMPLETE simulation. In that way, the total system state can change consistently, you don't want a *later* transition to take place first.
     * Doesn't update time, this is the responsibiltiy of the calling SimGod, it should do so just before calling this function. Moreover, the assumption is that the entity isn't influenced anymore by the environment for the duration of the process that is attached to the proposed state.
     */
   def doProposedTransition =
   {  println(this + ".doProposedTransition called")
      proposedTransitionTo match
      {  case Some(jn_Jn_State_Delay_OptJn_SimProc_Duration) =>
         {  current_Jn_Jn_State_Delay_OptJn_SimProc_Duration = jn_Jn_State_Delay_OptJn_SimProc_Duration
            jn_Jn_State_Delay_OptJn_SimProc_Duration.runSimProc

            println("   current_Jn_Jn_State_Delay_OptJn_SimProc_Duration becomes: " + current_Jn_Jn_State_Delay_OptJn_SimProc_Duration)
         }
         case None               => doNothing
      }
      finishProcessCalled = false
      updateTransitionModel
      Unit
   }

   def finishProcess =
   {  println(this + ".finishProcess called")
      if( finishProcessCalled )
      {  throw new RuntimeException("finishProcess called twice for the same process")
      }
      val state = current_Jn_Jn_State_Delay_OptJn_SimProc_Duration.state.asInstanceOf[this.State]
      println("   finishing process of state = " + state)
      val duration = current_Jn_Jn_State_Delay_OptJn_SimProc_Duration.duration

      lastFinishTimeStateMap = lastFinishTimeStateMap.updated(state, Some(SystemWithTesting.currentTimeMillis))

      totalDurations = totalDurations.updated(state, totalDurations(state) + duration)
      // &y2013.01.05.12:50:49& this shows again that it would be better to refactor the join constructs, this doesn't feel so elegant.
      println("   updated totalDuration for state " + state + " to " + totalDurations(state) + ".")
      finishProcessCalled = true
   }

   def updateTransitionModel

   /** @todo Make less generic, but in a subclass of simentity which adds support for generic delayfunctions
     */
   def durStartRatDesReq2TerminLastExe(state:State, startRatDes:TimeInMillis):Option[DurationInMillis] =
   {  lastFinishTimeStateMap(state) match
      {  case None         => None
         case Some(lFTS)   => Some(lFTS - startRatDes)
      }
   }

   class State(name:String) extends org.ocbkc.generic.test.simulation.jara.State(name) // <&y2012.12.20.13:53:52& better turn into case class?>[A &y2012.12.20.15:50:23& no because with case classes may create different instances with the same constructor values><&y2012.12.20.23:55:07& this is not a good idea I think (inner class of State : each instance of SimuEntity now gets its own qStart state! Find solution for this.>
   {  override def toString = "State( name = " + name + ", hashCode = " + hashCode + " )"
   }

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

/** Innerclass, because SimProcs are specifically tied to this entity.
  *
  */
   class SimProc(name:String, function:(DurationInMillis) => Any) extends org.ocbkc.generic.test.simulation.jara.SimProc(name, function)

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
      {  println(this + ".State.apply called")
         val state = new State(name)
         println("  created state: " + state)
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
{  def getFirst_Jn_Jn_State_Delay_OptJn_SimProc_Duration(jns_Jn_State_Delay_OptJn_SimProc_Duration: List[Jn_Jn_State_Delay_OptJn_SimProc_Duration] ):Jn_Jn_State_Delay_OptJn_SimProc_Duration =
   {  jns_Jn_State_Delay_OptJn_SimProc_Duration.sortWith{ case (jn_Jn_State_Delay_OptJn_SimProc_Duration1, jn_Jn_State_Delay_OptJn_SimProc_Duration2) => ( jn_Jn_State_Delay_OptJn_SimProc_Duration1.jn_State_Delay.delay < jn_Jn_State_Delay_OptJn_SimProc_Duration2.jn_State_Delay.delay ) }(0)
   }
}

abstract class State(val name:String) // <&y2012.12.20.13:53:52& better turn into case class?>[A &y2012.12.20.15:50:23& no because with case classes may create different instances with the same constructor values>
{  override def toString = "State( name = " + name + " id = " + super.toString + " )"
}

case class Jn_State_Delay(val state:State, val delay: DurationInMillis)
{  //override def toString = "Jn_State_Delay( state = " + state + ", delay = " + delay + ")"
}

class Jn_State_DelayGen(val state:State, val delayGen: () => DurationInMillis )
{  def gen:Jn_State_Delay =
   {  Jn_State_Delay(state, delayGen())
   }
}

abstract class SimProc(val name:String, val function:(DurationInMillis) => Any)
{  def run(d:DurationInMillis) =
   {  println("Running SimProc " + name)
      function(d)
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

case class Jn_Jn_State_Delay_OptJn_SimProc_Duration(val jn_State_Delay:Jn_State_Delay, val optJn_SimProc_Duration: Option[Jn_SimProc_Duration])
{  //override def toString = "State( name = " + name + ", simProc = " + simProc + " )"
   /** @returns 0 if there is no process attached to the state, otherwise the duration of the process.
     *
     */

   def duration:Long =
   {  optJn_SimProc_Duration match
         {  case Some(jn_SimProc_Duration) => jn_SimProc_Duration.duration
            case None => 0L
         }
   }

   def simProc =
   {  optJn_SimProc_Duration match
      {  case Some(jn_SimProc_Duration) => jn_SimProc_Duration.simProc
         case None => None
      }
   }

   def runSimProc =
   {  optJn_SimProc_Duration match
      {  case Some(jn_SimProc_Duration) => jn_SimProc_Duration.simProc.run(jn_SimProc_Duration.duration) // pass the duration to the process, it may in some cases need this information.
         case None => doNothing
      }
   }

   def state:State =
   {  jn_State_Delay.state
   }
}

/** values in map: a function which produces a delay if it is called. It is a function because it in most cases the delay will not have a fixed duration, but depend on randomness and context info.
  * @todo is an empty map a programming error, or is there a use case for it? For now I assume the first.
  */

// <&y2012.12.20.18:11:16& move following to separate file>
}


/** Intended to test the simulation library. So it is kinda meta test!
  *
  */
package org.ocbkc.generic.test.simulation.jara.test
{
import org.ocbkc.generic.test.simulation.jara._
/*
object TestSimulation
{  def main(args: Array[String]) =
   {  println("TestSimulation.main called")
      if( args.length != 0 ) 
         println("Usage: command without arguments")
      else
         TestRun.no6(3)
   }
}
*/
class SimTestPlayer extends SimEntity
{  // create additional states
   println("SimPlayer constructor of " + this)
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
      qPlayTranslationSession -> List(qPlayConstiGame, qPlayTranslationSession),
      qPlayConstiGame -> List(qPlayConstiGame, qPlayTranslationSession)
   )

   // attach delaygenerators to states, processes to states, and durationgenerators to processes.
   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, () => delayFunction), None )
   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayConstiGame, () => delayFunction), None )

   override def updateTransitionModel =
   {  
   }
}

object TestRun
{  /** Don't change this class ever, only if it is for debugging. This fixes the test settings. If you want to test with other parameters, create an additional class
     */
   class SimTestPlayer extends SimEntity
   {  // create additional states
      println("SimPlayer constructor of " + this)
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
         qPlayTranslationSession -> List(qPlayConstiGame, qPlayTranslationSession),
         qPlayConstiGame -> List(qPlayConstiGame, qPlayTranslationSession)
      )

      // attach delaygenerators to states, processes to states, and durationgenerators to processes.
      Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, () => delayFunction), None )
      Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayConstiGame, () => delayFunction), None )

      override def updateTransitionModel =
      {
      }
   }

   class SimTestPlayer2 extends SimEntity
   {  // create additional states
      println("SimPlayer constructor of " + this)
      val qPlayTranslationSession = State("qPlayTranslationSession")
      val qPlayConstiGame = State("qPlayConstiGame")
      val qUnsubscribe = State("qUnsubscribe")

      def delayFunction =
      {  (30*1000 + Random.nextInt(2)).toLong
      }

      transitions = 
      Map(
         qStart -> List(qPlayTranslationSession),
         qPlayTranslationSession -> List(qPlayConstiGame, qPlayTranslationSession),
         qPlayConstiGame -> List(qPlayConstiGame, qPlayTranslationSession)
      )

      // attach delaygenerators to states, processes to states, and durationgenerators to processes.
      Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, () => delayFunction), None )
      Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayConstiGame, () => delayFunction), None )

      override def updateTransitionModel =
      {  
      }
   }

   class SimTestPlayer3 extends SimEntity
   {  // create additional states
      println("SimPlayer constructor of " + this)
      val qPlayTranslationSession = State("qPlayTranslationSession")
      val qPlayConstiGame = State("qPlayConstiGame")
      val qUnsubscribe = State("qUnsubscribe")

      def delayFunction =
      {  (30*1000 + Random.nextInt(10)).toLong
      }

      def durationFunction =
      {  (30*1000 + Random.nextInt(10)).toLong
      }

      transitions = 
      Map(
         qStart -> List(qPlayTranslationSession),
         qPlayTranslationSession -> List(qPlayConstiGame, qPlayTranslationSession),
         qPlayConstiGame -> List(qPlayConstiGame, qPlayTranslationSession)
      )

      // attach delaygenerators to states, processes to states, and durationgenerators to processes.
      Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, () => delayFunction), Some(new Jn_SimProc_DurationGen(new SimProc("procPlayTranslationSession", d => println("process called")), () => durationFunction)) )
      Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayConstiGame, () => delayFunction), Some(new Jn_SimProc_DurationGen(new SimProc("procPlayTranslationSession", d => println("process called")), () => durationFunction)) )

      override def updateTransitionModel =
      {  
      }
   }

   class SimTestPlayer4 extends SimEntity
   {  // create additional states
      println("SimPlayer constructor of " + this)
      val qPlayTranslationSession = State("qPlayTranslationSession")
      val qPlayConstiGame = State("qPlayConstiGame")
      val qUnsubscribe = State("qUnsubscribe")

      def delayFunction =
      {  (15*1000 + Random.nextInt(30*1000)).toLong
      }

      def durationFunction =
      {  (15*1000 + Random.nextInt(30*1000)).toLong
      }

      transitions = 
      Map(
         qStart -> List(qPlayTranslationSession),
         qPlayTranslationSession -> List(qPlayConstiGame, qPlayTranslationSession),
         qPlayConstiGame -> List(qPlayConstiGame, qPlayTranslationSession)
      )

      // attach delaygenerators to states, processes to states, and durationgenerators to processes.
      Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, () => delayFunction), Some(new Jn_SimProc_DurationGen(new SimProc("procPlayTranslationSession", d => println("process called")), () => durationFunction)) )
      Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayConstiGame, () => delayFunction), Some(new Jn_SimProc_DurationGen(new SimProc("procPlayTranslationSession", d => println("process called")), () => durationFunction)) )

      override def updateTransitionModel =
      {  
      }
   }

   def no1() =
   {  println("TestRun.no1 called")
      val p1 = new SimTestPlayer
      println("   start state is " + p1.current_Jn_Jn_State_Delay_OptJn_SimProc_Duration)
      p1.proposeTransition
      p1.doProposedTransition
      p1.proposeTransition
      p1.doProposedTransition
   }

   def no2() =
   {  println("TestRun.no2 called")
      val p1 = new SimTestPlayer
      SimGod.run( { case (iteration, _) => iteration < 2 })
   }  

   def no3() =
   {  println("TestRun.no3 called")
      val p1 = new SimTestPlayer
      val p2 = new SimTestPlayer
      SimGod.run( { case (iteration, _) => iteration < 2 })
   } 

   def no4(numberOfPlayers:Int) =
   {  println("TestRun.no4 called")
      for( i <- ( 0 until numberOfPlayers ) ) new SimTestPlayer
      SimGod.run( { case (iteration, _) => iteration < 100 })
   } 

   /** Purposes: test what happens if there is more than one SimEntity with a shortest start time to the next state.
     */
   def no5(numberOfPlayers:Int) =
   {  println("TestRun.no5 called")
      for( i <- ( 0 until numberOfPlayers ) ) new SimTestPlayer2
      SimGod.run({ case (iteration, _) => iteration < 100 })
   } 

   def no6(numberOfPlayers:Int) =
   {  println("TestRun.no6 called")
      for( i <- ( 0 until numberOfPlayers ) ) new SimTestPlayer3
      SimGod.run({ case (iteration, _) => iteration < 100 })
   }

   def no7(numberOfPlayers:Int) =
   {  println("TestRun.no7 called")
      for( i <- ( 0 until numberOfPlayers ) ) new SimTestPlayer4
      SimGod.run({ case (iteration, _) => iteration < 1000 })
   }

   def no8(numberOfPlayers:Int) =
   {  println("TestRun.no8 called")
      for( i <- ( 0 until numberOfPlayers ) ) new SimTestPlayer3
      SimGod.run({ case (iteration, _) => iteration < 1000 })
   }
}
}
