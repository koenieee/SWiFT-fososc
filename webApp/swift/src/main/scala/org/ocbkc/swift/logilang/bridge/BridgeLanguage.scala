// todo rename file

/** Brone is a bridge language which can be used for any CTL which has constants that consist of strings. It forms a bridge between these constants and the proper names used in NL.
Currently it is used for:
- TODO 
  */

package org.ocbkc.swift.logilang.bridge.brone

import org.ocbkc.swift.logilang._
import scala.collection.mutable.ListBuffer
import org.ocbkc.swift.global.Logging._

// <&y2013.12.22.12:36:19& give this bridge language a good name, for now I called it brone (bridge langauge one)

trait BroneSent


/** Semantics: All entCTL strings, are CTL constants. All entNL strings are NL words which represent an entity (such as nouns, proper names). all CTL constants in entCTL and all NL words in entNL refer to the same, single, entity. entCTLname and entNLname must each contain at least one element.
  */



/** For now it is assumed that each predicate which occurs in a PredicateBridgeSent is one place and its application can be translated to the form:

entityCTLname is predNLname

example: 
P(e)
EntityBridge(e, Edward)
PredicateBridgeSent(P, big)

so in this case:
"Edward is big"

  */
case class PredicateBridgeSent(predCTLname: String, predNLname: List[String]) extends BroneSent

/** Assumptions: each entNLname refers unambiguously to an entity. So that means that all entNLnames in the List refer to the same entity. Moreover, it is assumed that the bridge is complete: a counter-example, if ("c", List("Aardvark Jumbo", "The sleeper") occurs, and ("d", List("The sleeper")), then the second is not complete: "Aardvark Jumbo" is another name for the same entity, so should also occur in the second list.
@todo &y2014.02.27.19:32:33& perhaps refactoring would be handy: introduce the notion of a NLentityGroup or so, in which all NLnames for a specirfic entity are collected. Then you can reuse the same group for more constants.
@todo &y2014.02.27.19:57:39& perhaps it is better to use Constant instead of the String identifier of a Constant for entCTL...
  */
case class EntityBridgeSent(entCTLname: String, entNLnames: List[String]) extends BroneSent
{  // <&y2014.01.03.09:44:35& apply method which check whether there is at least one element. TODO by Mussie...
}

class BridgeDoc
{  val bridgeSents:ListBuffer[BroneSent] = new ListBuffer

   def entityBridgeSents = bridgeSents.collect{ case bs:EntityBridgeSent => bs.asInstanceOf[EntityBridgeSent] }

   def predicateBridgeSents = bridgeSents.collect{ case bs:PredicateBridgeSent => bs.asInstanceOf[PredicateBridgeSent] }

   /** @returns the NL names of all entities defined in this bridge
     */
   def entNLnames:List[String] =
   {  entityBridgeSents.map{ case EntityBridgeSent(_, enn) => enn }.flatten.toSet.toList //toSet.toList to remove duplicate elements.
   }

   override def toString = 
   {  "BridgeDoc(" + bridgeSents.map{ _.toString }.mkString(", ") + ")"
   }

   /** @returns the first natural language noun which occurs in the bridge for the given constant. 
       @todo for making the right translation, also must be checked some properties of the natural language word in the bridge, does it allow the given construction? Information needs to be added to natlang.Info for this purpose. <& Is this indeed so?>
     */
   def constant2entNLname(c:Constant):Option[String] =
   {  entityBridgeSents.find{ case EntityBridgeSent(entCTLname, _) => entCTLname == c.name  }.collect{ case EntityBridgeSent(_, entNLname) => entNLname(0) }
   }


   def entNLname2entCTLname(entNLname:String):Option[String] =
   {  entityBridgeSents.find{ case EntityBridgeSent(_, entNLnames) => entNLnames.contains(entNLname) }.collect{ case EntityBridgeSent(entCTLname, _) => entCTLname }
   }

   def pred2NLadjective(p:Predicate):Option[String] =
   {  predicateBridgeSents.find{ case PredicateBridgeSent(predCTLname,_) => predCTLname == p.name  }.collect{ case PredicateBridgeSent(predCTLname, predNLname) => predNLname(0) }
   }

   /** Only use this when you are certain that the predicate is in the bridge
     */
   def pred2NLadjectiveOrException(p:Predicate):String =
   {  pred2NLadjective(p).getOrElse(logAndThrow("No bridgesentence for predicate " + p))
   }
}

/** move to separate file
  */
package translators
{
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query.plofofa._


/** Translators which translate between the same CTL, used with different bridges.
  * @todo coulddo, after major refactoring of FOL (see todo's there) - replace constant-names (strings) with Constant objects
  * @todo after major refactoring of FOL, put type bounds on TargetLang__TP
  */
trait BridgeBasedAutoCTLtranslator[CTLsent__TP <: CTLsent, TargetLang__TP]
{ 
   def apply(ctlsent: CTLsent__TP, bsSource: BridgeDoc, bsTarget: BridgeDoc, targetLang: TargetLang__TP ):Option[CTLsent__TP]
   
   /** @returns Map[String, Option[String]], where both Strings represent constant identifiers. For example (("c1", Some("joke"))) states that constant c1 refers to the same entity as the constant joke in the representation with bsTarget. (("c1", None)) states that there is no constant in the representation with bsTarget which represents constant 1. This is all assumes that the bridgedocs provide complete information  (all constants are mapped to natural language nouns).
     */
   def generateConstantMap(bsSource: BridgeDoc, bsTarget: BridgeDoc):Map[String, Option[String]] =
   {  bsSource.entityBridgeSents.map
      {  case EntityBridgeSent(entCTLname, firstEntNLname::_) =>
         {  (  entCTLname,
               {  bsTarget.entNLname2entCTLname(firstEntNLname)
               }
            )
         }
      }.toMap
   }

   
   def generatePredMap(bsSource: BridgeDoc, bsTarget: BridgeDoc):Map[String, Option[String]] =
   {  logAndThrow("not yet implemented")
/* { unfinished
      bsSource.predicateBridgeSents.map
      {  case PredicateBridgeSent(predCTLname, firstPredNLname::_) =>
         {  (  predCTLname,
               {  bsTarget.WIW entNLname2entCTLname(firstEntNLname)
               }
            )
         }
      }.toMap
*/
// }
   }

   abstract class ConstantTranslationError
   case class SomeConstantsUntranslatable extends ConstantTranslationError

   /** @returns (None, FINISH...
     */
   def translateConstants(constantNames:List[String], bsSource:BridgeDoc, bsTarget:BridgeDoc):(Option[List[String]], Option[ConstantTranslationError]) =
   {  val constantMap = generateConstantMap(bsSource, bsTarget)

      val constantNamesTranslated = constantNames.map{ constantMap(_) }
      if(constantNamesTranslated.contains(None))
         (None, Some(SomeConstantsUntranslatable()))
      else
         (Some(constantNamesTranslated.map{ case Some(cn) => cn }), None)
   }
}

object BridgeBasedAutoPlofafaTranslator extends BridgeBasedAutoCTLtranslator[PlofofaPat_rb, FOLtheory]
{  override def apply(prb: PlofofaPat_rb, bsSource: BridgeDoc, bsTarget: BridgeDoc, targetLang: FOLtheory):Option[PlofofaPat_rb] =
   {  Some(prb) // the same! There are no constants in the (current version of the) Plofofa language 
   
   
/* For the future:
 *      PlofofaPat_rb.sf match
 *      {  case Forall(vr:Var, setPatVar:PatVar, predApp:PredApp) =>
 *         {  Forall(
 *         }
 *
 *         case MostInfo(patVar, forallPat: Forall) =>
 *         {  MostInfo(patVar, apply(forallPat))
 *         }
 *      }
 */
   }
}
}
