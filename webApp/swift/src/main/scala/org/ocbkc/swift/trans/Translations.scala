/** @todo &y2014.02.12.16:44:38& unify with other translations package
  */
package org.ocbkc.swift.trans

import org.ocbkc.swift.logilang.bridge.brone._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.global.Logging._

/**
  * @todo: provide a method which produces different alternative translations into NL, for example 
           "Akwasi is big and fast."
           or
           "Akwasi is big.
            Akwasi is fast."
  */

object Translation
{  /**   Straight, because it tries to keep the original organisation of the CTL sentences in tact. E.g.
     *
            B(akwasi)
            F(akwasi)

         will be translated in 
       
            "Akwasi is big.
             Akwasi is fast."

         and NOT in

            "Akwasi is big and fast."
   */
   def FOltheory2NL_straight(ft:FOLtheory, bs:BridgeDoc) =
   {  ft.stats.map
      {  predicateApp2NL(_, bs)
      }.collect{ case(Some(nlSent)) => nlSent }
   }

   def predicateApp2NL(folStat:FOLstatement, bs:BridgeDoc):Option[String] =
   {  folStat match
      {  case PredApp_FOL(pred, List(constant:Constant)) =>
         {  if(pred.arity == 1)
            {  Some(bs.constant2NLnoun(constant).getOrElse(logAndThrow("No bridgesentence for constant " + constant))+ " is " +  bs.predicate2NLAdjective(pred).getOrElse(logAndThrow("No bridgesentence for predicate " + pred)) + ".")
            }
            else
               None
         }
      }
   }
}
