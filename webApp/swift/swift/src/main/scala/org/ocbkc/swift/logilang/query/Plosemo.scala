package org.ocbkc.swift.logilang.query
{
import System._
import java.io._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.parser.CLIwithFileInput
import net.liftweb.json._
import net.liftweb.json.ext.EnumSerializer
// import scala.util.parsing.combinator.Parsers._

/* Conventions:
Abbreviation for constitution: consti (const is to much similar to constant).

*/


// BEGIN TEST
object TestPlosemoCLI extends CLIwithFileInput
{  import ComparisonOperator._
   def main(args: Array[String]) =
   {  if( args.length != 0 ) println("Usage: command, without arguments")
      def f:String =
      {  val query = MostInfo(NumResPat(Geq, PatVar("n"), Var("x"), PredApp(Predicate("p",2),List(Constant("a"), Var("x")))))
         "   query serialized: " + query.serialize
      }
      //applyFunctionToFile(f)
      println(f)
   }
}


// END TEST


// not complete FOL yet

// Questionlanguage: Folnuminqua

// each FOL theory is associated with its own list of predicate and constant symbols, I.e. there may be more constants with the same name and id, as long as they are partr
case class PlosemoSentence
{  /*
   def serialize =
   {  // implicit val formats = Serialization.formats(NoTypeHints)
      implicit val formats = DefSerialization.formats(NoTypeHints) + new EnumSerializer(ComparisonOperator)
      // implicit val formats = net.liftweb.json.DefaultFormats + new EnumSerializer(ComparisonOperator)
      var fqser:String = Serialization.write(this)
      err.println("  FolnuminquaQuery serialised to: " + fqser)
   }
   */
   //override def toString
}

// NumResPat = Number Retriction Pattern 
case class MostInfo(numrespat:NumResPat) extends PlosemoSentence // I don't assume nesting of quantifiers is allowed, so I don't have to indicate WHICH number variable I want to have the most informative (MostInfo) value of.
{  def serialize =
   {  /* <? &y2012.05.18.15:40:46& the following gives an error because + cannot be used to add Formats. How can this be accomplished? Or isn't it possible, and if not, why not?>/(   relatedTo = {[lift-json]}
   
   implicit val formats = DefaultFormats + Serialization.formats(ShortTypeHints(List(classOf[Var], classOf[Constant]))) + (new EnumSerializer(ComparisonOperator))
   */

      implicit val formats:Formats = Serialization.formats(ShortTypeHints(List(classOf[Var], classOf[Constant]))) + (new EnumSerializer(ComparisonOperator))
      //implicit val formats = Serialization.formats(FullTypeHints(List(classOf[Term]))) + FieldSerializer[Var]() + new EnumSerializer(ComparisonOperator)
      var fqser:String = Serialization.write(this)
      err.println("  MostInfo statement " + this + "\nserialised to: " + fqser)
   }
}

object ComparisonOperator extends Enumeration
{  type ComparisonOperator = Value
   val Geq = Value
}

import ComparisonOperator._

case class NumResPat(comOp:ComparisonOperator, patvar:PatVar, boundvar:Var, predapp:PredApp) extends Plosemo
case class PatVar(id:String)
//case class Var(id:String)


/*  <&y2012.04.23.17:01:11&For current increment, do not yet implement the following, but do it for a next:>
class ComparisonOperator

case class Eqt() extends ComparisonOperator
case class Gt() extends ComparisonOperator
case class Lt() extends ComparisonOperator
case class Geq() extends ComparisonOperator
case class Leq() extends ComparisonOperator
*/
// <&y2012.04.22.00:22:40& make use of Clean data structures I designed: copy the idea here.>
}

// Answer language:


