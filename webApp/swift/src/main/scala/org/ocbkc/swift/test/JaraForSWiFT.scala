/** Simulates players playing the SWiFT game. Intended for testing purposes.
  *
  */
package ocbkc.swift.test.simulation.jara
{
import org.ocbkc.generic.test.simulation.jara._
import org.ocbkc.swift.coord.ses.CoreSimu
import _root_.org.ocbkc.swift.model._
import org.ocbkc.swift.OCBKC._
import scala.util.Random
import org.ocbkc.swift.global._
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.Types._
import org.ocbkc.swift.test.SystemWithTesting
import org.ocbkc.generic.random.RandomExtras._
import org.ocbkc.swift.test.TestHelpers._
import scala.math.Numeric

object Test
{  val debug = true
   val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")
}

import Test._

object PlayingSimulator
{  def start(durationInMillis:Long) =
   {  println("PlayingSimulator.start called")
      SimGod.resetJara
      val durationSimulation = durationInMillis //3*24*60*60*1000 //
      println(durationSimulation)
      TestSettings.SIMULATECLOCK = true
      TestSettings.SIMULATEPLAYINGWITHJARARUNNING = true

      val adminId = GlobalConstant.adminOpt.get.id.is
      val constiAlpha = Constitution.create(adminId)
      constiAlpha.publish(
"""<h2>Article 1</h2>

<p>publication 1</p>
""", "publication 1", adminId.toString
      )

      new SimSubscriptions()
      val startTimeSim = SystemWithTesting.currentTimeMillis
      SimGod.run{ case (_, timeInMillis) => ( timeInMillis < startTimeSim + durationSimulation ) }
      


}
}

/** Wrapper for random object with fixed seed. A fixed seed makes it easy to recreate found bugs, by using the same seed.
  */
object SharedRandom
{  val gb = new Random(837479112L)

   def get =
   {  gb
   }
}

/** Very coarse simulation.
  * @param liftPlayer: the lift player object which will be "operated" by this simplayer object.
  */

trait DelayFunctionGenerator
{  //def generate:()=>DurationInMillis
}

trait DurationFunctionGenerator
{
}


object SimpleDelayFunctionGenerator extends DelayFunctionGenerator
{  def generate =
   {  () => (15*1000 + SharedRandom.get.nextInt(30*1000)).toLong
   }
}  

/*
         Notation: *: given a priori (from the perspective of this function
   
         startRatDesReq: moment from which ratDes is being achieved. With normal usage, this is a fixed moment, usually the creation moment of the entity.
         durStartRatDesReq2TerminLastExe * = duration from start simulation to time termination of the last execution of the process
         durTotSinceStartRatDesReq *    = actual total simulated duration of game from start simulation
         durProcSinceStartRatDesReq *   = actual total duration spent on the process associated with this state
         ratDes *       = desired ratio duration/dura
         catchRat *     = catch up ratio
         catchUpTime    = catch up time after which to achieve the desired ratio
         durProcCatch   = duration to be spent on process in the catch up time to achieve the desired ratio
         durProcPerExe * = expected value of the duration of the process each time it is executed from beginning to end. 
         noCatchExes    = number of required executions of the process in the catch up time to achieve the desired ratio
         delayCatchExp     = required expected value of duration of delay before each execution of the process in the catch-up time, in agreement with achieving the desired ratio (if the delay would each time be equal to its expected value), AFTER THE TERMINATION OF THE LAST PROCESS EXECUTION
         delayCatchAct = the actual next delay after randomisation AFTER THE TERMINATION OF THE LAST PROCESS EXECUTION, so not the actual time. This is the final result

         delayCatchActAfterNow = the same as the delayCatchAct, but now transformed to counting the delay from now.
         
         ratDes =  ( durProcSinceStartRatDesReq + durProcCatch ) / ( durStartSimu2TerminLastExe + catchUpTime )
         => durProcCatch = ratDes * ( durStartRatDesReq2TerminLastExe + catchUpTime ) - durProcSinceStartRatDesReq

         noCatchExes = durProcCatch / durProcPerExe
         delayCatchExp = ( catchUpTime - durProcCatch ) / noCatchExes 
         
         delayCatchAct = 
         durTerminLastExe2Now = durTotSinceStartRatDesReq - durStartRatDesReq2TerminLastExe
         delayCatchActAfterNow = max(0, delayCatchAct - durTerminLastExe2Now)
      */

/** @todo give a better name
  * @todo would be nice to pass the durationfunction to the generate method, to prevent possible errors. However, that requires refactoring of this function (it has to become an object).
  */
object DelayFunctionType1Generator extends DelayFunctionGenerator
{  /** @param randomDelayRatio the allowed deviation around the expected value of the expected value of the delay. (Note that the delay is calculated by the generated function, it doesn't (and shouldn't) have to be provided.) It will use a random distribution between [delay - (delay * randomDelayRatio), delay + (delay * randomDelayRatio)]. E.g. if the expected delay is 18 minutes, and randomDelayRatio = 0.5, then the delay will be randomly drawn from [ 18-9 = 9, 18+9 = 27]
     * @param name name used for debugging purposes. Convention: proc + name of proces. E.g. procStartGame
       @param durStartRatDesReq2TerminLastExe function which produces the time from the start of the simulation of this entity (or another start moment from which you want to achive the ratDes) to the end of 
     * @todo &y2013.01.09.23:58:32& rename parameters to more consistent names
     * @todo &y2013.01.18.20:34:27& what if noCatchExes  is smaller than 1, is the behaviour as desired? [A &y2013.01.18.20:43:58& analysed it: what happens is that the delay will become longer than the catchup time, so it will take longer to reach the desired ratio. However, if you would do it earlier (within the catchup time), the process will get ahead immediately. So whether this is desired or not is simply a matter of preference.]
     * &y2013.01.18.20:46:44& explanation for fact that using overallDuration etc, a process will get its turn more often might be that in spite of the fact it will be carried out as often as a normal process (using "local" durations and lastfinishtime of processes) - it will have multiple "shots" at it - so a higher probability one will make it.
     */
   def generate( 
      ratDes:Double,
      catchUpTime:DurationInMillis,
      durStartRatDesReq2TerminLastExe_Opt: () => Option[DurationInMillis], // <&y2013.01.16.15:48:49& change TerminLastExe to momentStartComingDelay (or something similar) to make it more general, e.g. when you don't use terminlastexe but another moment.>
      durProcPerExe: DurationInMillis,
      randomDelayRatio: Double,
      durTotSinceStartRatDesReq: () => DurationInMillis,
      durProcSinceStartRatDesReq: () => DurationInMillis,
      ranseq: Random,
      name:String
      ) =
   {  //val ta = ( () => SystemWithTesting.currentTimeMillis )
      println("DelayFunctionType1Generator.generate called")
      if( durProcPerExe <= 0 ) throw new RuntimeException("   durProcPerExe should be > 0, while it is " + durProcPerExe)
      
      () =>
      {  println(name + " called")
         println("   ratDes " + ratDes + ", catchUpTime = " + durationFromMillisToHumanReadable(catchUpTime) + ", durStartRatDesReq2TerminLastExe = " + { durStartRatDesReq2TerminLastExe_Opt() match { case None => "n/a"; case Some(dS) => durationFromMillisToHumanReadable(dS) } } + ", durProcPerExe = " +  durationFromMillisToHumanReadable(durProcPerExe) + ", durTotSinceStartRatDesReq() = " + durationFromMillisToHumanReadable(durTotSinceStartRatDesReq()) + ", durProcSinceStartRatDesReq() = " + durationFromMillisToHumanReadable(durProcSinceStartRatDesReq()) + ".")
         println("   current ratio = " + durProcSinceStartRatDesReq().toDouble/durTotSinceStartRatDesReq().toDouble)
         
         val dSRDR2TLE:DurationInMillis =
         durStartRatDesReq2TerminLastExe_Opt() match
         {  case Some(dS)  => dS
            case None      => 0L
         }

         if(debug)
         {  def isRatio(r:Double) = { r >= 0 && r <= 1 }
            if( !isRatio(ratDes) ) throw new RuntimeException("   ratDes should be in [0,1]")
            if(catchUpTime < 0) throw new  RuntimeException("   catchupTime should be positive")
            if(dSRDR2TLE < 0) throw new  RuntimeException("   durStartRatDesReq2TerminLastExe should be positive")
            if(durTotSinceStartRatDesReq() < dSRDR2TLE) throw new RuntimeException("   durTotSinceStartRatDesReq < durStartRatDesReq2TerminLastExe")
            if(durProcPerExe < 0) throw new  RuntimeException("   durProcPerExe should be positive")
            if(durTotSinceStartRatDesReq() < 0) throw new  RuntimeException("   durTotSinceStartRatDesReq should be positive")
         }

         val durProcCatch = ratDes * ( dSRDR2TLE + catchUpTime ) - durProcSinceStartRatDesReq() // TODO what if durProcCatch >=0, but < durProcPerExe ??? Did I take that into account?
        
         /* if the process is too far behind, this number will be greater than catchUpTime */
         println("    durProcCatch = " + durationFromMillisToHumanReadable(durProcCatch.toLong) )

         val delayCatchExp:Long =
         if( durProcCatch < 0  ) // process is so much ahead that you can't get even after catchUpTime, even if you would do nothing during this catchUpTime.
         {  println("   this process is so far ahead that it can't get to ratDes, even if doing nothing during the catchUpTime...") 
            ( ( durProcSinceStartRatDesReq() + durProcPerExe ).toDouble/ratDes - dSRDR2TLE.toDouble - durProcPerExe.toDouble ).toLong
            /* Calculate the minimal delay which will make it even, in spite of the fact it will be longer than the catch up time. . Equation solved for this purpose: 
                  ratDes = ( durProcSinceStartRatDesReq + durProcPerExe) / ( dSRDR2TLE + delayCatchExp + durProcPerExe )
                  => delayCatchExp = ( durProcSinceStartRatDesReq + durProcPerExe ) / ratDes - durTotSinceStartRatDesReq - durProcPerExe
             */
         } else if ( durProcCatch <= catchUpTime) // you can get even
         {  println("   process can get to ratDes within catchUpTime")
            val noCatchExes = durProcCatch / durProcPerExe
            println("    noCatchExes = " + noCatchExes )
            ( ( catchUpTime - durProcCatch.toDouble ) / noCatchExes.toDouble ).toLong
         } else // process is so far behind, that it can't be caught up within catchUpTime.
         {  println("   process is so far behind, that it can't be caught up within catchUpTime to ratDes.")
            0L
         }

         println("   delayCatchExp = " + durationFromMillisToHumanReadable(delayCatchExp) )

         val maxDev = delayCatchExp * randomDelayRatio
         val delayCatchAct = ( delayCatchExp.toDouble - maxDev + 2 * maxDev * ranseq.nextDouble ).toLong // possible bug small probability with rounding errors that it goes below 0.

         println("   delayCatchAct = " + durationFromMillisToHumanReadable(delayCatchAct))
         if( delayCatchAct < 0 ) throw new RuntimeException("value of delayCatchAct is below 0")

         val durTerminLastExe2Now = durTotSinceStartRatDesReq() - dSRDR2TLE
         val delayCatchActAfterNow = (delayCatchAct - durTerminLastExe2Now).max(0L)
         println("   delayCatchActAfterNow = " + durationFromMillisToHumanReadable(delayCatchActAfterNow))
         if(debug)
         { // COULDDO extra tests
         }
         delayCatchActAfterNow
      }
   }
}  


/** @todo better name
  */
object DurationFunctionType1Generator extends DurationFunctionGenerator
{  def generate(name:String, expectedValue: DurationInMillis, randomDurationRatio: Double, ranseq: Random):() => DurationInMillis =
   {  () =>
      {  println(name + " called")
         val maxDev = expectedValue * randomDurationRatio
         println("   maxDev = " + maxDev)
         val durationAct = ( expectedValue.toDouble - maxDev + 2 * maxDev * ranseq.nextDouble ).toLong // possible bug small probability with rounding errors that it goes below 0?
         println("durationAct = " + durationAct)
         durationAct
      }
   }
}


/** <&y2013.01.16.16:26:46& COULDDO In future perhaps generalise to JaraSimulation>
  */
object SimPlayer
{  def overallTerminTimeLastExe(s:State) =
   {  SimEntity.simEntities.collect{ case se:SimPlayer => se.lastFinishTimeStateMap(se.State.find(s.name).get) }.max
   }
   def overallDuration(s:State) =
   {  SimEntity.simEntities.collect{ case se:SimPlayer => se.totalDurations(se.State.find(s.name).get) }.sum
   }
}

class SimPlayer(val liftPlayer:Player) extends SimEntity
{  println("SimPlayer constructor of " + this + "called")
   val ran = SharedRandom.get

   // @todo &y2013.01.19.19:19:08& move to more generic level (but not too!)
   def durStartRatDesReq2OverallTerminLastExe(state:State, startRatDes:TimeInMillis):Option[DurationInMillis] =
   {  SimPlayer.overallTerminTimeLastExe(state) match
      {  case None         => None
         case Some(oTTLE)  => Some(oTTLE - startRatDes)
      }
   }
  
   // Some settings outside jara simulation system
   val MAX_NO_CONSTI:Option[Int] = Some(3) // Set to None for unlimited. Default = None.

   // Some state information outside the jara simulation system
   var sesCoord:CoreSimu = null
   val playerId = liftPlayer.id.get
   println("   liftPlayer = " + liftPlayer )
   var startTimeSession:Option[POSIXtime] = None

   // create additional states
   val qPlayTranslationSession = State("qPlayTranslationSession")
   val qCreateSession = State("qCreateSession")
   val qChooseFirstConsti = State("qChooseFirstConsti")
   val qCreateNewConsti = State("qCreateNewConsti")
   val qEditExistingConsti = State("qEditExistingConsti")
   val qChooseReleaseCandidate = State("qChooseReleaseCandidate")
   //val qUnsubscribe = State("qUnsubscribe")
   
   initialisationAfterStateDefs

   val durationPlayTranslationSessionExp  = 2 * 60 * 1000
   val durationEditExistingConstiExp      = 5 * 60 * 1000
   val durationChooseFirstConstiExp       = 1 * 60 * 1000
   val durationChooseReleaseCandidateExp  = 1 * 60 * 1000
   val durationCreateNewConstiExp         = 1 * 60 * 1000 // nonsensical duration for qCreateNewConsti, but done just to make it work.
   // &y2013.01.10& Is not elegant and is confusing: startTimeSession and lastFinishTimeStateMap should come from the same source.

   val delayPlayTranslationSession = DelayFunctionType1Generator.generate( 0.1, 60 * 60 * 1000, () => durStartRatDesReq2TerminLastExe(qPlayTranslationSession, startTimeSession.get), durationPlayTranslationSessionExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSession.get),() => totalDurations(qPlayTranslationSession), ran, "delayPlayTranslationSession" )
   val delayEditExistingConsti = DelayFunctionType1Generator.generate( 0.01, 60 * 60 * 1000, () => durStartRatDesReq2TerminLastExe(qEditExistingConsti, startTimeSession.get), durationEditExistingConstiExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSession.get),() => totalDurations(qEditExistingConsti), ran, "delayEditExistingConsti" )
   val delayChooseFirstConsti = DelayFunctionType1Generator.generate( 0.1, 60 * 60 * 1000, () => durStartRatDesReq2TerminLastExe(qChooseFirstConsti, startTimeSession.get), durationChooseFirstConstiExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSession.get),() => totalDurations(qChooseFirstConsti), ran, "delayChooseFirstConsti" )
   val delayChooseReleaseCandidate = DelayFunctionType1Generator.generate( 0.3, 60 * 60 * 1000, () => durStartRatDesReq2TerminLastExe(qChooseReleaseCandidate, startTimeSession.get), durationChooseReleaseCandidateExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSession.get),() => totalDurations(qChooseReleaseCandidate), ran, "delayChooseReleaseCandidate" )

   //For qCreateNewConsti don't look at the last the player created a constitution, but the last time ANY player created a constitution.
   val delayCreateNewConsti = DelayFunctionType1Generator.generate( 0.003, 60 * 60 * 1000, () => durStartRatDesReq2OverallTerminLastExe(qCreateNewConsti, SimGod.startTimeCurrentRun.get), durationCreateNewConstiExp, 0.25, () => (SystemWithTesting.currentTimeMillis - SimGod.startTimeCurrentRun.get),() => SimPlayer.overallDuration(qCreateNewConsti), ran, "delayCreateNewConsti" )

/*
   /** @todo &y2013.01.02.13:39:24& Refactor: put this way of calculating delayFunctions into the generic Jara lib. E.g. in a class
     */
*/

   def delayFunction =
   {  (90*1000 + SharedRandom.get.nextInt(20*1000)).toLong
   }

   val durationPlayTranslationSession = DurationFunctionType1Generator.generate("durationPlayTranslationSession", durationPlayTranslationSessionExp, 0.25, ran)
   val durationEditExistingConsti = DurationFunctionType1Generator.generate("durationEditExistingConsti", durationEditExistingConstiExp, 0.25, ran)
   val durationChooseFirstConsti = DurationFunctionType1Generator.generate("durationChooseFirstConsti", durationChooseFirstConstiExp, 0.25, ran)
   val durationChooseReleaseCandidate = DurationFunctionType1Generator.generate("durationChooseReleaseCandidate", durationChooseReleaseCandidateExp, 0.25, ran)
   val durationCreateNewConsti = DurationFunctionType1Generator.generate("durationCreateNewConsti", durationCreateNewConstiExp, 0.25, ran)

   transitions =
   Map(
      qStart -> List(qCreateSession),
      qCreateSession -> List(qChooseFirstConsti),
      qChooseFirstConsti -> List(qPlayTranslationSession),
      qPlayTranslationSession -> List(qEditExistingConsti, qPlayTranslationSession, qCreateNewConsti, qChooseReleaseCandidate), // in fact qCreateNewConsti is only allowed after a certain minimal number of sessions played, so actually executing the attached process succesfully only happens after. COULDDO: build this into these transitions somehow?
      qEditExistingConsti -> List(qEditExistingConsti, qPlayTranslationSession, qCreateNewConsti, qChooseReleaseCandidate),
      qCreateNewConsti -> List(qEditExistingConsti, qPlayTranslationSession, qCreateNewConsti, qChooseReleaseCandidate),
      qChooseReleaseCandidate -> List(qEditExistingConsti, qPlayTranslationSession, qCreateNewConsti)
   )

   // attach delaygenerators to states, processes to states, and durationgenerators to processes.

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qCreateSession, () => 0), Some(new Jn_SimProc_DurationGen(new SimProc("procCreateSession", procCreateSession(_)), () => 0)) )

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qChooseFirstConsti, delayChooseFirstConsti), Some(new Jn_SimProc_DurationGen(new SimProc("procChooseFirstConsti", procChooseFirstConsti(_)), durationChooseFirstConsti)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qCreateNewConsti, delayCreateNewConsti), Some(new Jn_SimProc_DurationGen(new SimProc("procCreateNewConsti", procCreateNewConsti(_)), durationCreateNewConsti)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, delayPlayTranslationSession), Some(new Jn_SimProc_DurationGen(new SimProc("procPlayTranslationSession", procPlayTranslationSession(_)), durationPlayTranslationSession)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qEditExistingConsti, delayEditExistingConsti), Some(new Jn_SimProc_DurationGen(new SimProc("procEditExistingConsti", procEditExistingConsti(_)), durationEditExistingConsti)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qChooseReleaseCandidate, delayChooseReleaseCandidate), Some(new Jn_SimProc_DurationGen(new SimProc("procChooseReleaseCandidate", procChooseReleaseCandidate(_)), durationChooseReleaseCandidate)))

   /** Just assume that player plays in one LOOOOONG session, instead of logging in and out again sometimes.
     */
   def procCreateSession(d: DurationInMillis) =
   {  sesCoord = new CoreSimu(liftPlayer)
      startTimeSession = Some(SystemWithTesting.currentTimeMillis)
   }

   // &y2013.01.01.17:21:30& TODO: more states needed: edit existing consti or create new one
   def procEditExistingConsti(d: DurationInMillis) =
   {  val ccount = Constitution.count
      if(ccount < 1) throw new RuntimeException("No constitutions created yet")
      val randomConstiId = 1 + SharedRandom.get.nextInt(ccount)
      val consti = Constitution.getById(randomConstiId).get
      sesCoord.URpublishConsti(consti,
"""<h2>Article 1</h2>

<p>""" + SharedRandom.get.nextString(20) + """</p>
""", "publication COULDO"
      )
   }

   def procChooseReleaseCandidate(d: DurationInMillis) =
   {  // choose random constitution of which this person is leader, and which has versions after the last version with a release status.
      val cwtv_ls = Constitution.constisWithTrailingVersionsWithoutReleaseStatus.filter{ _.leadersUserIDs.contains(playerId) }

      pickRandomElementFromList(cwtv_ls, SharedRandom.get) match
      {  case Some(cwtv_l) =>
         {  sesCoord.URsetReleaseCandidate(cwtv_l, true)
         }
         case None =>
         {  log("   No constisWithTrailingVersionsWithoutReleaseStatus for which player " + + playerId + " is leader. (I would have bowed for you your Highness the Player, but you ain't leader for any Consti with this property.)")
         }
      }
   }

   def procCreateNewConsti(d: DurationInMillis) =
   {  MAX_NO_CONSTI match
      {  case None      => createNewConsti
         case Some(mnc) =>
         {  if( Constitution.count <= mnc ) createNewConsti
         }
      }

      def createNewConsti =
      {  val newConsti = Constitution.create(playerId)
         newConsti.initialiseNew // needed?
         newConsti.publish(
"""<h2>Article 1</h2>

<p>""" + SharedRandom.get.nextString(20) + """</p>
""", "publication COULDO", playerId.toString
      )
      }
   }


   def procChooseFirstConsti(d: DurationInMillis) =
   {  println("procChooseFirstConsti called")

      pickRandomElementFromList(Constitution.constisWithAReleaseOrVirginRelease, SharedRandom.get) match
      {  case Some(randomConsti) =>
         {  sesCoord.URchooseFirstConstitution(randomConsti.constiId)
         }
         case None               =>
         {  throw new RuntimeException("No consti available to choose from! (There should be at least one with a released version).")
         }
      }
   }

   def procPlayTranslationSession(duration: DurationInMillis) =
   {  val winSession = ran.nextBoolean
      
      sesCoord.URtryStartTranslation
      log("[POTENTIAL_BUG] Jara is not yet prepared for dealing with URtryStartTranslation (instead of the previous URstartTranslation)")
      sesCoord.URstopTranslation
      sesCoord.URalgorithmicDefenceSimplified(winSession, duration)
   }

   override def updateTransitionModel =
   {
   }
}

/** @todo for SimSubscriptions I need another DelayFunctionGenerator type, one that can cope with processes which have a duration of 0. Now solved it in another way, but this is a quick fix.
  */
class SimSubscriptions extends SimEntity
{  val ran = SharedRandom.get
   // <{ create additional states
   val qNewSubscription = State("qNewSubscription")
   // >}

   initialisationAfterStateDefs
   val durationNewSubscriptionExp = 5 * 60 * 1000 // &y2013.01.07.20:36:40& unrealisatic, but currently you have to set it about equal to other duration functions otherwise the associated delay will always win from others.
   
   val delayNewSubscription = DelayFunctionType1Generator.generate( 0.05, 60 * 60 * 1000, () => durStartRatDesReq2TerminLastExe(qNewSubscription, SimGod.startTimeCurrentRun.get), durationNewSubscriptionExp, 0.25, () => (SystemWithTesting.currentTimeMillis - SimGod.startTimeCurrentRun.get),() => totalDurations(qNewSubscription), ran, "delayNewSubscription" )

   val durationNewSubscription = DurationFunctionType1Generator.generate("durationNewSubscription", durationNewSubscriptionExp, 0.25, ran)

   transitions = 
   Map(
      qStart -> List(qNewSubscription),
      qNewSubscription -> List(qNewSubscription)
   )

   // attach delaygenerators to states, processes to states, and durationgenerators to processes.
   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qNewSubscription, delayNewSubscription), Some(new Jn_SimProc_DurationGen(new SimProc("procNewSubscription", procNewSubscription(_)), durationNewSubscription)) )

   var subscriptionId:Long = 0

   def procNewSubscription(d:DurationInMillis) =
   {  val liftPlayer:Player = Player.create.firstName("Aap" + subscriptionId).email("aap" + subscriptionId + "@test.org").password("asdfghjk").validated(true)
      liftPlayer.save
      new SimPlayer(liftPlayer)
      subscriptionId += 1
   }

   def updateTransitionModel = {}
}

}
