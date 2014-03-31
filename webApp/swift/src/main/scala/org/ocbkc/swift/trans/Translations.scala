/** @todo &y2014.02.12.16:44:38& unify with other translations package
  */
package org.ocbkc.swift.trans

import org.ocbkc.swift.logilang.bridge.brone._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.natlang.generation._

/**
  * @todo: provide a method which produces different alternative translations into NL, for example 
           "Akwasi is big and fast."
           or
           "Akwasi is big.
            Akwasi is fast."
  */

/** Notations:
  * _
  */
object TranslateFOLtheory2NL// @todo extends translation to NL
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
   def NLstyleStraight(ft:FOLtheory, bs:BridgeDoc) =
   {  ft.stats.map
      {  predApp(_, bs)
      }.collect{ case(Some(nlSent)) => nlSent }
   }

   def predApp(folStat:FOLstatement, bs:BridgeDoc):Option[String] =
   {  folStat match
      {  case PredApp_FOL(pred, List(constant:Constant)) =>
         {  if(pred.arity == 1)
            {  Some(bs.constant2entNLname(constant).getOrElse(logAndThrow("No bridgesentence for constant " + constant))+ " is " + bs.pred2NLadjectiveOrException(pred) + ".")
            }
            else
               None
         }
      }
   }
   
   /** @folStats: folStats which contain at most one predicate, otherwise 
     */
   def NLstyleDistributePredicate(folStats: List[FOLstatement], bs: BridgeDoc) =
   {  logAndThrow("Not yet implemented")
   }

   /** The same as distributePredicate, but assumes that p is applied to all constants in the associated KRdoc (FOLtheory). So, be careful! Call this method directly if you dispose over the list of constants and the predicates (then it is more efficient to do so).
       @throws Size constants should be at least 2, otherwise an error will be produced (in the logfile).
       @throws Assumes that all constants are in the bridgeDoc, otherwise will throw exception
     */
   def NLstyleDistributePredicateUnchecked(constants:List[Constant], pred:Predicate, bridgeDoc: BridgeDoc):String =
   {  if(constants.size < 2) logAndThrow("[ERROR] number of constants should be 2 or greater")
      NLgen.commaAndList(constants.map{ c => bridgeDoc.constant2entNLname(c).get }) ++ " are each " ++ bridgeDoc.pred2NLadjectiveOrException(pred) ++ "."
   }
}
