import System._
import org.ocbkc.swift.global._

package org.ocbkc.swift.test
{  
   // <&y2012.10.16.20:51:51& coulddo refactor this with reflection, so that the implementation can be optimised = if no test, all code directly calls System.currentTimeMillis>
   object SystemAndExtras
   {  var currentTimeMillisVar:Long = 0L
      
      def currentTimeMillis
      {  if(TestSettings.SIMULATECLOCK)
         {  currentTimeMillisVar
         }
         else
         {  System.currentTimeMillis
         }
      }
   }

}
