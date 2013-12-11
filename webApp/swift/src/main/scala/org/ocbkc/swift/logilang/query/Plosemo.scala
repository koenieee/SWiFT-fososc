/** In the future investigate another model with better reuse among CTLs:
- using a superclass with innerclasses defining Predicate, PredApp, etc, so that these can be reused, while still having a slightly different semantics, or additional constructs added to them. For now, each CTL is defined from scratch.
*/

package org.ocbkc.swift.logilang.query.plosemo
{
import System._
import java.io._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.test.CLIwithFileInput
import org.ocbkc.swift.logilang.query._
import net.liftweb.json._
import net.liftweb.json.ext.EnumSerializer
// import scala.util.parsing.combinator.Parsers._

/* Conventions:
Abbreviation for constitution: consti (const is to much similar to constant).

*/

// BEGIN TEST
object TestPlosemoCLI extends CLIwithFileInput
{  def main(args: Array[String]) =
   {  if( args.length != 0 ) println("Usage: command, without arguments")
      def f:String =
      {  val query = MostInfo(PatVar("s"), Forall(Var("x"), PatVar("s"), PredApp(Predicate("B",1), List(Var("x")))))
         "   query serialized: " + query.serialize
      }
      //applyFunctionToFile(f)
      println(f)
   }
}


// END TEST

// Questionlanguage: Folnuminqua

// each FOL theory is associated with its own list of predicate and constant symbols, I.e. there may be more constants with the same name and id, as long as they are partr
trait PlosemoPat
{  
}
/** Example in pure format: mostInfo(s_, forall x from s_.P(c_2, x))
  */
case class MostInfo(patVar: PatVar, forallPat: Forall) extends PlosemoPat // I don't assume nesting of quantifiers is allowed, so I don't have to indicate WHICH number variable I want to have the most informative (MostInfo) value of.
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
case class Forall(vr:Var, setPatVar:PatVar, predApp:PredApp) extends PlosemoPat
case class PredApp_Plosemo(override val p:Predicate, override val terms:List[SimpleTerm]) extends PredApp(p, terms) with PlosemoPat

}
