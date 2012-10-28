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
   private var lastTimeMillis_simu:POSIXtime = currentTimeMillisVar_simu

   def currentTimeMillis:POSIXtime =
   {  if(TestSettings.SIMULATECLOCK)
      {  currentTimeMillisVar_simu
      }
      else
      {  System.currentTimeMillis
      }
   }

   // &y2012.10.29.00:34:11& perhaps use this method as few times as possible, and use pause instead (the latter enforces chronological order...)
   def currentTimeMillis_= (newTime:POSIXtime):Unit =
   {  if(newTime < lastTimeMillis_simu ) throw new RuntimeException("   timeOfEvent is smaller than of last position of clock, clock may only move forward.")
      lastTimeMillis_simu = currentTimeMillisVar_simu
      currentTimeMillisVar_simu = newTime
   }


/** @param min in milliseconds
* @param max in milliseconds
*/
   def pause(min:Long, max:Long, randomSeq:Random) =
   {  val pause = min + randomSeq.nextInt((max - min).toInt).toLong
      SystemWithTesting.currentTimeMillisVar_simu += pause
   }


/** @param duration in milliseconds
*/
   def pause(duration:Long) =
   {  SystemWithTesting.currentTimeMillisVar_simu += duration
   }
}

object Types
{  type SimulatedEvent = (POSIXtime, () => Any) 
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
