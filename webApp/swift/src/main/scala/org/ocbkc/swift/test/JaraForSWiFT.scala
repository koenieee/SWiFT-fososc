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

object PlayingSimulator
{  def start(iterations:Int) =
   {  println("PlayingSimulator.start called")
      // Connect each lift player object to a simplayer.
      val players = Player.findAll
      players.foreach{ new SimPlayer(_) }
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

class SimPlayer(val liftPlayer:Player) extends SimEntity
{  println("SimPlayer constructor of " + this + "called")
   val ran = GlobalRandom.get

   // Some state information outside the jara simulation system
   var sesCoord:CoreSimu = null
   val playerId = liftPlayer.id.get
   println("   liftPlayer = " + liftPlayer )

   // create additional states
   val qPlayTranslationSession = State("qPlayTranslationSession")
   val qCreateSession = State("qCreateSession")
   val qChooseFirstConsti = State("qChooseFirstConsti")
   val qCreateNewConsti = State("qCreateNewConsti")
   val qEditExistingConsti = State("qEditExistingConsti")
   //val qUnsubscribe = State("qUnsubscribe")

/*
   def delayqEditExistingConsti =
   {  /* Later partly to be replaced with references to variables from outside the scope of this function.
         Notation: *: given a priori (from the perspective of this function
   
         ta *           = actual time (now)
         durTotAct *    = actual total simulated duration from start simulation
         durProcAct *   = actual total duration spent on the process associated with this state
         ratDes *       = desired ratio duration/dura
         catchRat *     = catch up ratio
         catchUpTime    = catch up time after which to achieve the desired ratio
         durProcCatch   = duration to be spent on process in the catch up time to achieve the desired ratio
         durProcPerExe * = duration of process each time it is executed frmo beginning to end. 
         noCatchExes    = number of required executions of the process in the catch up time to achieve the desired ratio
         delayCatch     = duration of delay before each execution of the process in the catch-up time, in agreement with achieving the desired ratio
         ...
         
         ratDes = ( durTotAct + durProcCatch ) / ( durTotAct * (1 + catchRat) )
         => durProcCatch = ratDes * ( durTotAct * (1 + catchRat) ) - durTotAct

         noCatchExes = durProcCatch / durProcPerExe
         catchUpTime = catchRat * durTotAct
         delayCatch = ( catchUpTime - durProcCatch ) / noCatchExes
      */
   }
*/

   def delayFunction =
   {  (15*1000 + GlobalRandom.get.nextInt(30*1000)).toLong
   }

   def delayCreateNewConsti =
   {  (15*1000 + GlobalRandom.get.nextInt(30*1000)).toLong
   }

   def durationFunction =
   {  (15*1000 + GlobalRandom.get.nextInt(30*1000)).toLong
   }

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

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qCreateSession, () => 0), Some(new Jn_SimProc_DurationGen(new SimProc("procCreateSession", () => procCreateSession), () => 0)) )

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qChooseFirstConsti, () => delayFunction), Some(new Jn_SimProc_DurationGen(new SimProc("procChooseFirstConsti", () => procChooseFirstConsti), () => durationFunction)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qCreateNewConsti, () => delayFunction), Some(new Jn_SimProc_DurationGen(new SimProc("procCreateNewConsti", () => procCreateNewConsti), () => delayCreateNewConsti)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qPlayTranslationSession, () => delayFunction), Some(new Jn_SimProc_DurationGen(new SimProc("procPlayTranslationSession", () => procPlayTranslationSession), () => durationFunction)))

   Jn_Jn_State_DelayGen_OptJn_SimProc_DurationGen( Jn_State_DelayGen(qEditExistingConsti, () => delayFunction), Some(new Jn_SimProc_DurationGen(new SimProc("procEditExistingConsti", () => procEditExistingConsti), () => durationFunction)) )

   /** Just assume that player plays in one LOOOOONG session, instead of logging in and out again sometimes.
     */
   def procCreateSession =
   {  sesCoord = new CoreSimu(liftPlayer)
   }

   // &y2013.01.01.17:21:30& TODO: more states needed: edit existing consti or create new one
   def procEditExistingConsti =
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

   def procCreateNewConsti =
   {  val newConsti = Constitution.create(playerId)
      newConsti.publish(
"""<h2>Article 1</h2>

<p>""" + GlobalRandom.get.nextString(20) + """</p>
""", "publication COULDO", playerId.toString
      )
   }

   def procChooseFirstConsti =
   {  println("procChooseFirstConsti called")

      val ccount = Constitution.count
      if( ccount > 0)
      {  val randomConstiId = 1 + GlobalRandom.get.nextInt(ccount)
         sesCoord.URchooseFirstConstitution(randomConstiId)
      } else
      {  throw new RuntimeException("No consti available to choose from!")
      }
   }

   def procPlayTranslationSession =
   {  val winSession = ran.nextBoolean
      sesCoord.URtranslation
      sesCoord.URstopTranslation
      sesCoord.URalgorithmicDefenceSimplified(winSession)
   }

   override def updateTransitionModel =
   {
   }
}
}
