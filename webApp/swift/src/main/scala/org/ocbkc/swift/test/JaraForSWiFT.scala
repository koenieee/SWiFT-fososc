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
import org.ocbkc.swift.global.Types._
import org.ocbkc.swift.test.SystemWithTesting
import scala.math.Numeric

object PlayingSimulator
{  def start(iterations:Int) =
   {  println("PlayingSimulator.start called")
      val startTimeSimulation = SystemWithTesting.currentTimeMillis
      new SimSubscriptions(startTimeSimulation)
      SimGod.run(iterations)
   }
}

/** Wrapper for random object with fixed seed. A fixed seed makes it easy to recreate found bugs, by using the same seed.
  */
object GlobalRandom
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
   {  () => (15*1000 + GlobalRandom.get.nextInt(30*1000)).toLong
   }
}  

/*
         Notation: *: given a priori (from the perspective of this function
   
         durStartSim2TerminLastExe * = duration from start simulation to time termination of the last execution of the process
         durTotAct *    = actual total simulated duration of game from start simulation
         durProcAct *   = actual total duration spent on the process associated with this state
         ratDes *       = desired ratio duration/dura
         catchRat *     = catch up ratio
         catchUpTime    = catch up time after which to achieve the desired ratio
         durProcCatch   = duration to be spent on process in the catch up time to achieve the desired ratio
         durProcPerExe * = expected value of the duration of the process each time it is executed from beginning to end. 
         noCatchExes    = number of required executions of the process in the catch up time to achieve the desired ratio
         delayCatchExp     = required expected value of duration of delay before each execution of the process in the catch-up time, in agreement with achieving the desired ratio (if the delay would each time be equal to its expected value), AFTER THE TERMINATION OF THE LAST PROCESS EXECUTION
         delayCatchAct = the actual next delay after randomisation AFTER THE TERMINATION OF THE LAST PROCESS EXECUTION, so not the actual time. This is the final result

         delayCatchActAfterNow = the same as the delayCatchAct, but now transformed to counting the delay from now.
         
         ratDes =  ( durProcAct + durProcCatch ) / ( durStartSimu2TerminLastExe + catchUpTime )
         => durProcCatch = ratDes * ( durStartSim2TerminLastExe + catchUpTime ) - durProcAct

         noCatchExes = durProcCatch / durProcPerExe
         delayCatchExp = ( catchUpTime - durProcCatch ) / noCatchExes 
         
         delayCatchAct = 
         durTerminLastExe2Now = durTotAct - durStartSim2TerminLastExe
         delayCatchActAfterNow = max(0, delayCatchAct - durTerminLastExe2Now)
      */

/** @todo give a better name
  * @todo would be nice to pass the durationfunction to the generate method, to prevent possible errors. However, that requires refactoring of this function (it has to become an object).
  */
object DelayFunctionType1Generator extends DelayFunctionGenerator
{  /** @param randomDelayRatio the allowed deviation around the expected value of the expected value of the delay. (Note that the delay is calculated by the generated function, it doesn't (and shouldn't) have to be provided.) It will use a random distribution between [delay - (delay * randomDelayRatio), delay + (delay * randomDelayRatio)]. E.g. if the expected delay is 18 minutes, and randomDelayRatio = 0.5, then the delay will be randomly drawn from [ 18-9 = 9, 18+9 = 27]
     * @param name name used for debugging purposes. Convention: proc + name of proces. E.g. procStartGame
       @param durStartSim2TerminLastExe function which produces the time from the start of the simulation of this entity (or another start moment from which you want to achive the ratDes) to the end of 
     */
   def generate( 
      ratDes:Double,
      catchUpTime:DurationInMillis,
      durStartSim2TerminLastExe: () => DurationInMillis,
      durProcPerExe: DurationInMillis,
      randomDelayRatio: Double,
      durTotAct: () => DurationInMillis,
      durProcAct: () => DurationInMillis,
      ranseq: Random, name:String 
      ) =
   {  //val ta = ( () => SystemWithTesting.currentTimeMillis )
      println("DelayFunctionType1Generator.generate called")
      if( durProcPerExe <= 0 ) throw new RuntimeException("   durProcPerExe should be > 0, while it is " + durProcPerExe)
      
      () =>
      {  println(name + " called")
         println("   ratDes " + ratDes + ", catchUpTime = " + catchUpTime + ", durProcPerExe = " + durProcPerExe + ", durTotAct() = " + durTotAct() + ", durProcAct() = " + durProcAct() + ".")
         println("   current ratio = " + durProcAct().toDouble/durTotAct().toDouble)

         val durProcCatch = ratDes * ( durStartSim2TerminLastExe() + catchUpTime ) - durProcAct()
        
         /* if the process is too far behind, this number will be greater than catchUpTime */
         println("    durProcCatch = " + durProcCatch )

         val delayCatchExp:Long =
         if( durProcCatch < 0  ) // process is so much ahead that you can't get even after catchUpTime, even if you would do nothing during this catchUpTime.
         {  println("   this process is so far ahead that it can't get to ratDes if doing nothing during the catchUpTime...") 
            ( ( durProcAct() + durProcPerExe ).toDouble/ratDes - durTotAct().toDouble - durProcPerExe.toDouble ).toLong
            /* Calculate the minimal delay which will make it even, in spite of the fact it will be longer than the catch up time. . Equation solved for this purpose: 
                  ratDes = ( durProcAct + durProcPerExe) / ( durTotAct + delayCatchExp + durProcPerExe )
                  => delayCatchExp = ( durProcAct + durProcPerExe ) / ratDes - durTotAct - durProcPerExe
             */
         } else if ( durProcCatch <= catchUpTime) // you can get even
         {  println("   process can get to ratDes within catchUpTime")
            val noCatchExes = durProcCatch / durProcPerExe
            println("    noCatchExes = " + noCatchExes )
            ( ( catchUpTime - durProcCatch.toDouble ) / noCatchExes.toDouble ).toLong
         } else // ^achterstand is so great, that it can't be caught up within catchUpTime.
         {  println("   process is so far behind, that it can't be caught up within catchUpTime to ratDes.")
            0L
         }

         println("   delayCatchExp = " + delayCatchExp )

         val maxDev = delayCatchExp * randomDelayRatio
         val delayCatchAct = ( delayCatchExp.toDouble - maxDev + 2 * maxDev * ranseq.nextDouble ).toLong // possible bug small probability with rounding errors that it goes below 0.

         println("   delayCatchAct = " + delayCatchAct )
         if( delayCatchAct < 0 ) throw new RuntimeException("value of delayCatchAct is below 0")
         val delayCatchActAfterNow = (delayCatchAct - durTerminLastExe2Now()).max(0L)
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

class SimPlayer(val liftPlayer:Player) extends SimEntity
{  println("SimPlayer constructor of " + this + "called")
   val ran = GlobalRandom.get
   
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
   //val qUnsubscribe = State("qUnsubscribe")
   
   initialisionAfterStateDefs

   val durationPlayTranslationSessionExp  = 2 * 60 * 1000
   val durationEditExistingConstiExp      = 5 * 60 * 1000
   val durationChooseFirstConstiExp       = 1 * 60 * 1000
   val durationCreateNewConstiExp         = 5 * 60 * 1000

   val delayPlayTranslationSession = DelayFunctionType1Generator.generate( 0.1, 60 * 60 * 1000, () => (SystemWithTesting.currentTimeMillis - lastFinishTimeStateMap(qPlayTranslationSession)), durationPlayTranslationSessionExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSession.get),() => totalDurations(qPlayTranslationSession), ran, "delayPlayTranslationSession" )
   val delayEditExistingConsti = DelayFunctionType1Generator.generate( 0.01, 60 * 60 * 1000, () => (SystemWithTesting.currentTimeMillis - lastFinishTimeStateMap(qEditExistingConsti)), durationEditExistingConstiExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSession.get),() => totalDurations(qEditExistingConsti), ran, "delayEditExistingConsti" )
   val delayChooseFirstConsti = DelayFunctionType1Generator.generate( 0.1, 60 * 60 * 1000, () => (SystemWithTesting.currentTimeMillis - lastFinishTimeStateMap(qChooseFirstConsti)), durationChooseFirstConstiExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSession.get),() => totalDurations(qChooseFirstConsti), ran, "delayChooseFirstConsti" )
   val delayCreateNewConsti = DelayFunctionType1Generator.generate( 0.01, 60 * 60 * 1000, () => (SystemWithTesting.currentTimeMillis - lastFinishTimeStateMap(qPlayTranslationSession)), durationCreateNewConstiExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSession.get),() => totalDurations(qCreateNewConsti), ran, "delayCreateNewConsti" )

/*
   /** @todo &y2013.01.02.13:39:24& Refactor: put this way of calculating delayFunctions into the generic Jara lib. E.g. in a class
     */
*/

   def delayFunction =
   {  (90*1000 + GlobalRandom.get.nextInt(20*1000)).toLong
   }

   val durationPlayTranslationSession = DurationFunctionType1Generator.generate("durationPlayTranslationSession", durationPlayTranslationSessionExp, 0.25, ran)
   val durationEditExistingConsti = DurationFunctionType1Generator.generate("durationEditExistingConsti", durationEditExistingConstiExp, 0.25, ran)
   val durationChooseFirstConsti = DurationFunctionType1Generator.generate("durationChooseFirstConsti", durationChooseFirstConstiExp, 0.25, ran)
   val durationCreateNewConsti = DurationFunctionType1Generator.generate("durationCreateNewConsti", durationCreateNewConstiExp, 0.25, ran)

   transitions =
   Map(
      qStart -> List(qCreateSession),
      qCreateSession -> List(qChooseFirstConsti),
      qChooseFirstConsti -> List(qPlayTranslationSession),
      qPlayTranslationSession -> List(qEditExistingConsti, qPlayTranslationSession, qCreateNewConsti), // in fact qCreateNewConsti is only allowed after a certain minimal number of sessions played, so actually executing the attached process succesfully only happens after. COULDDO: build this into these transitions somehow?
      qEditExistingConsti -> List(qEditExistingConsti, qPlayTranslationSession, qCreateNewConsti),
      qCreateNewConsti -> List(qEditExistingConsti, qPlayTranslationSession, qCreateNewConsti)
   )

   // attach delaygenerators to states, processes to states, and durationgenerators to processes.

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qCreateSession, () => 0), Some(new Jn_SimProc_DurationGen(new SimProc("procCreateSession", procCreateSession(_)), () => 0)) )

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qChooseFirstConsti, delayChooseFirstConsti), Some(new Jn_SimProc_DurationGen(new SimProc("procChooseFirstConsti", procChooseFirstConsti(_)), durationChooseFirstConsti)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qCreateNewConsti, delayCreateNewConsti), Some(new Jn_SimProc_DurationGen(new SimProc("procCreateNewConsti", procCreateNewConsti(_)), durationCreateNewConsti)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, delayPlayTranslationSession), Some(new Jn_SimProc_DurationGen(new SimProc("procPlayTranslationSession", procPlayTranslationSession(_)), durationPlayTranslationSession)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qEditExistingConsti, delayEditExistingConsti), Some(new Jn_SimProc_DurationGen(new SimProc("procEditExistingConsti", procEditExistingConsti(_)), durationEditExistingConsti)))

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
      val randomConstiId = 1 + GlobalRandom.get.nextInt(ccount)
      val consti = Constitution.getById(randomConstiId).get
      consti.publish(
"""<h2>Article 1</h2>

<p>""" + GlobalRandom.get.nextString(20) + """</p>
""", "publication COULDO", playerId.toString
      )
   }

   def procCreateNewConsti(d: DurationInMillis) =
   {  val newConsti = Constitution.create(playerId)
      newConsti.publish(
"""<h2>Article 1</h2>

<p>""" + GlobalRandom.get.nextString(20) + """</p>
""", "publication COULDO", playerId.toString
      )
   }

   def procChooseFirstConsti(d: DurationInMillis) =
   {  println("procChooseFirstConsti called")

      val ccount = Constitution.count
      if( ccount > 0)
      {  val randomConstiId = 1 + GlobalRandom.get.nextInt(ccount)
         sesCoord.URchooseFirstConstitution(randomConstiId)
      } else
      {  throw new RuntimeException("No consti available to choose from!")
      }
   }

   def procPlayTranslationSession(duration: DurationInMillis) =
   {  val winSession = ran.nextBoolean
      
      sesCoord.URtranslation
      sesCoord.URstopTranslation
      sesCoord.URalgorithmicDefenceSimplified(winSession, duration)
   }

   override def updateTransitionModel =
   {
   }
}

/** @todo for SimSubscriptions I need another DelayFunctionGenerator type, one that can cope with processes which have a duration of 0. Now solved it in another way, but this is a quick fix.
  */
class SimSubscriptions(startTimeSimulation: DurationInMillis) extends SimEntity
{  val ran = GlobalRandom.get
   // <{ create additional states
   val qNewSubscription = State("qNewSubscription")
   // >}

   initialiseTotalDurations
   val durationNewSubscriptionExp = 5 * 60 * 1000 // &y2013.01.07.20:36:40& unrealisatic, but currently you have to set it about equal to other duration functions otherwise the associated delay will always win from others.
   
   val delayNewSubscription = DelayFunctionType1Generator.generate( 0.1, 60 * 60 * 1000, durationNewSubscriptionExp, 0.25, () => (SystemWithTesting.currentTimeMillis - startTimeSimulation),() => totalDurations(qNewSubscription), ran, "delayNewSubscription" )

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
