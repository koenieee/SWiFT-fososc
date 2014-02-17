/** In the future investigate another model with better reuse among CTLs:
- using a superclass with innerclasses defining Predicate, PredApp, etc, so that these can be reused, while still having a slightly different semantics, or additional constructs added to them. For now, each CTL is defined from scratch.
*/

package org.ocbkc.swift.logilang.query.plofofa
{
import System._
import java.io._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.test.CLIwithFileInput
import org.ocbkc.swift.logilang.query._
import net.liftweb.json._
import net.liftweb.json.ext.EnumSerializer
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.logilang.bridge.brone._
import org.ocbkc.swift.logilang.translations._

// import scala.util.parsing.combinator.Parsers._

/* Conventions:
Abbreviation for constitution: consti (const is to much similar to constant).

*/

// BEGIN TEST
object TestPlofofaCLI extends CLIwithFileInput
{  def main(args: Array[String]) =
   {  if( args.length != 0 ) println("Usage: command, without arguments")
      def f:String =
      {  val query = MostInfo(PatVar("s"), Forall(Var("x"), PatVar("s"), PredApp_Plofofa(Predicate("B",1), List(Var("x")))))
         "   query serialized: " + query.serialize
      }
      //applyFunctionToFile(f)
      println(f)
   }
}

// END TEST

trait PlofofaPat extends QuerySent
{  
}

/** PlofofaPat representation bundle
  */
class PlofofaPat_rb() extends CTLrepresentationBundle[PlofofaPat] with QuerySent // extends QuerySent @todo &y2014.01.20.16:28:57& extends QuerySent: yes or no?
{  override val displayNameCTL = "PlofofaPat"
   val transform = PlofofaRepresentationTransforms
}

object PlofofaPat_rb extends CTLrepresentationBundleFactory[PlofofaPat, PlofofaPat_rb]
{  override def apply = new PlofofaPat_rb()
}

object PlofofaRepresentationTransforms extends CTLrepresentationTransforms[PlofofaPat]
{  override def pf2sf(pf:String):ParseResult[PlofofaPat]  =
   {  logAndThrow("PlofofaPat_rb.pf2sf Not implemented in this increment: avoid usage, by always explicitly providing the scala format of the plofofapat.")
   }

   /** Translation Scala-format to pure-format
     * @todo to be implemented by Mussie?
     */
   override def sf2pf(sf:PlofofaPat):String =
   {/*sf match
      {
      }*/
      "TODO"
   }
}
/** Example in pure format: mostInfo(s_, forall x from s_.P(c_2, x))
  */
case class MostInfo(patVar: PatVar, forallPat: Forall) extends PlofofaPat // I don't assume nesting of quantifiers is allowed, so I don't have to indicate WHICH number variable I want to have the most informative (MostInfo) value of.
{  def serialize =
   {  /* <? &y2012.05.18.15:40:46& the following gives an error because + cannot be used to add Formats. How can this be accomplished? Or isn't it possible, and if not, why not?>/(   relatedTo = {[lift-json]}
   
   implicit val formats = DefaultFormats + Serialization.formats(ShortTypeHints(List(classOf[Var], classOf[Constant]))) + (new EnumSerializer(ComparisonOperator))
      */

      implicit val formats:Formats = Serialization.formats(ShortTypeHints(List(classOf[Var], classOf[Constant])))
      //implicit val formats = Serialization.formats(FullTypeHints(List(classOf[Term]))) + FieldSerializer[Var]() + new EnumSerializer(ComparisonOperator)
      var fqser:String = Serialization.write(this)
      err.println("  MostInfo statement " + this + "\nserialised to: " + fqser)
   }
}

// Example: mostInfo(s_, forall x from s_.P(c_2, x))
case class Forall(vr:Var, setPatVar:PatVar, predApp:PredApp_Plofofa) extends PlofofaPat
case class PredApp_Plofofa(override val p:Predicate, override val terms:List[SimpleTerm]) extends PredApp(p, terms) with PlofofaPat

package translator
{
import org.ocbkc.swift.logilang.translations._


/** Tranforms a query in Plofofa using one set of bridgestats, to the equivalent query in Plofafa using another set of bridgestats.
  * This can be used to translate queries when people used different constants to denote entities, etc.
  */
object QueryTranslator
{  def apply(bridgeStatsComputer:List[BroneSent], bridgestatsPlayer:List[BroneSent], query:PlofofaPat):PlofofaPat =
   {  null // TODO
   }
}

object TranslatePlofofaSentToNL extends TranslateCTL2NL[PlofofaPat_rb]
{  override def apply(prb: PlofofaPat_rb, bs: BridgeDoc):String =
   {  translate(prb.sf, bs)
   }

   private def translate(p: PlofofaPat, bs: BridgeDoc):String =
   {  p match
      {  case Forall(vr, setPatVar, PredApp_Plofofa(pred, _)) =>
         {  "Mention people or things which are " ++  bs.predicate2NLAdjective(pred).getOrElse(logAndThrow("No bridgesentence for predicate " + pred)) ++ "."
         }
//Forall(Var(name = x),PatVar(s),PredApp(Predicate(name = F, arity = 1),List(Var(name = x)))) (of class org.ocbkc.swift.logilang.query.plofofa.Forall)
         case MostInfo(patVar, forallPat) =>
         {  translate(forallPat, bs) ++ " And... do not mention some, but mention all of them!"
         }
      }
   }
}

}
}
