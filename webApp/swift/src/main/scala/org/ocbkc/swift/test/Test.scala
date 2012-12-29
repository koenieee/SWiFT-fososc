import System._
import org.ocbkc.swift.global._
import org.ocbkc.swift.global.Types._
import scala.util.Random

package org.ocbkc.swift.test
{  
   // <&y2012.10.16.20:51:51& coulddo refactor this with reflection, so that the implementation can be optimised = if no test, all code directly calls System.currentTimeMillis>
object SystemWithTesting
{  val startTimeMillis_simu:POSIXtime = System.currentTimeMillis
   private var currentTimeMillisVar_simu:POSIXtime = startTimeMillis_simu // take the current time as the start time of the simulated clock
   //private var lastTimeMillis_simu:POSIXtime = currentTimeMillisVar_simu
   val msgSimulateClockOff = "The simulated clock is turned off, so it makes no sense to call this method."

   def currentTimeMillis:POSIXtime =
   {  if(TestSettings.SIMULATECLOCK)
      {  currentTimeMillisVar_simu
      }
      else
      {  System.currentTimeMillis
      }
   }

/*
   def currentTimeMillisAndTimeSinceLastEvent:(POSIXtime, DurationInMillis) =
   {  if(TestSettings.SIMULATECLOCK) (currentTimeMillisVar_simu, currentTimeMillisVar_simu - lastTimeMillis_simu) 
      else throw new RuntimeException(msgSimulateClockOff)
   }
*/
   // &y2012.10.29.00:34:11& perhaps use this method as few times as possible, and use pause instead (the latter enforces chronological order...)
   /** @todo test TestSettings.SIMULATECLOCK
     */
   def currentTimeMillis_= (newTime:POSIXtime):Unit =
   {  println("SystemWithTesting.currentTimeMillis_= called")
      println("   param newTime = " + newTime)
      println("   so clock jumps " + ( newTime - currentTimeMillisVar_simu ) + " to the future" )
      if(newTime < currentTimeMillisVar_simu )
      {  throw new RuntimeException("   proposed newTime (" + newTime + ") is smaller than of last position of clock (" + currentTimeMillisVar_simu + "), however, clock may only move forward, or stay in the same place.")
      }
      else
      {  //println("   lastTimeMillis_simu was " + lastTimeMillis_simu )
         //println("   and becomes equal to last value of currentTimeMillisVar_simu = " + currentTimeMillisVar_simu + " to it")
         //lastTimeMillis_simu = currentTimeMillisVar_simu
         currentTimeMillisVar_simu = newTime
      }
   }


/** @param min in milliseconds
* @param max in milliseconds
*/
   def pause(min:Long, max:Long, randomSeq:Random) =
   {  val pause = min + randomSeq.nextInt((max - min).toInt).toLong
      //lastTimeMillis_simu = currentTimeMillisVar_simu
      currentTimeMillisVar_simu += pause
   }


/** @param duration in milliseconds
*/
   def pause(duration:Long) =
   {  //lastTimeMillis_simu = currentTimeMillisVar_simu
      currentTimeMillisVar_simu += duration
   }
}

object Types
{   type SimulatedEvent = () => Any
    type DelayedSimulatedEvent = (POSIXtime, SimulatedEvent)
}

object TestHelpers
{
/** @param min in milliseconds
  * @param max in milliseconds
  */

   def randomPause(min:Long, max:Long, randomSeq:Random):Long =
   {  min + randomSeq.nextInt( (max - min).toInt).toLong
   }
}

}
