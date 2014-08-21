/* Wrapper to simplify calling scalac and scala from scala programs.

This is intended to be used by hvpl.compile only, thus use the latter package if you want to call scalac.
*/
package org.ocbkc.swift.tpwrap
{
import org.ocbkc.swift.global.GlobalConstant._
import System._
import scala.sys.process._
import java.io._
import org.ocbkc.swift.test.CLIwithFileInput
//import org.ocbkc.swift.
import scala.util.matching._
import scala.util.matching.Regex._

import org.ocbkc.swift.logilang._


// Begin testing

object TestEproverWrap
{  def main(args: Array[String]) =
   {  println("args(0) = " + args(0))
      println("args = " + args.mkString)
      //args
      val output = Eprover("--cpu-limit=30 --memory-limit=Auto --tstp-format -s --answers " + args(0))
      println("Output of eprover:")
      println(output)
      println("Extract constants:")
      println(output.extractConstants)
   }
}

object TestParadoxWrap
{  def main(args: Array[String]) =
   {  println("args(0) = " + args(0))
      println("args = " + args.mkString)
      //args
      Paradox(args(0))
   }
}


// End testing

/* 
Conventions:
- I follow the names used in the definition of the syntax of Scala, from "The Scala Language Specification 2.9" as much as possible. If this is not the case, _ns is appended to the name.
- when definition is simplified (compared with original scala syntax definition), in the comment of the parser "simplified" is added.
- if the preferred name of a parser is already a word used in scala (e.g. a keyword), then the prefix scala is added to distinguish it. (E.g. type becomes scalaType)
*/

object Eprover
{  val eproverpath = EPROVER_PATH

   def apply(params:String):EproverCliOutput =
   {  val cmd  = eproverpath + "/eprover " + params
      val cli  = new EproverCliOutput
      val pl   = ProcessLogger( o => (cli.out += o + "\n"), e => (cli.err += e + "\n") )
      val procBuilder = sys.process.Process(cmd, new java.io.File( EPROVER_PATH ))
      val s: Int = procBuilder!(pl)
      
      // Now delete input file, to prevent rereading it in the future... This can be switched of temporarily for debugging purposes: you can then still read the file.
      err.println("just tried to run '" + cmd + "' on commandline...")
      err.println("exit value (0 is good) = " + s + "\n")
      err.println("### begin errorstream:\n" + (if(cli.err == "") "None" else cli.err))
      err.println("### end errorstream\n\n")
      err.println("### begin outputstream:\n" + (if(cli.out == "") "None" else cli.out))
      err.println("### end outputstream")
      cli
   }

   class EproverCliOutput(var out:String, var err:String)
   {  def this() = this("","")
      def extractConstants:List[Constant] =
      {  var cs:List[Constant] = Nil
         // working gvim regex (POSIX regex) = \[\[\([^]]\+\)\]|_\] 
         val regex = """# SZS answers Tuple \[\[([^\]]+)\]|_\]""".r // WIW: translate from vim syntax to java syntax: only left: | and ^\ are they correct?
         println("   matched groups:")
         for(matchLoc <- regex.findAllIn(out).matchData; group <- matchLoc.subgroups)
         { println(group); if (group != null) cs +:= Constant(group) 
	 }
         cs
         
      /* Example output
# SZS status Theorem
# SZS answers Tuple [[c]|_]
# SZS answers Tuple [[b]|_]
# SZS answers Tuple [[a]|_]

 tested succesful regex in gvim format: '<,'>s/# SZS answers Tuple \[\[\([^\]]\)\+\]|_\]/\1/gc
 */
         
      }

      override def toString =
      {  "EproverCliOutput\n" +
         "{\n" +
         "out = " + out + "\n" + 
         "err = " + err + "\n" +
         "}"
      }

   }
}

object Paradox
{  val paradoxpath = PARADOX_PATH

   def apply(params:String):ParadoxCliOutput =
   {  val cmd  = paradoxpath + "/paradox " + params
      val cli  = new ParadoxCliOutput
      val pl   = ProcessLogger( o => (cli.out += o + "\n"), e => (cli.err += e + "\n") )

      val procBuilder =  sys.process.Process(cmd, new java.io.File( PARADOX_PATH )) 
      val s:Int = procBuilder!(pl)
      
      // Now delete input file, to prevent reareading it in the future... This can be switched of temporarily for debugging purposes: you can then still read the file.
      err.println("trying to run '" + cmd + "' on commandline...")
      err.println("exit value (0 is good) = " + s + "\n")
      err.println("### begin errorstream:\n" + (if(cli.err == "") "None" else cli.err))
      err.println("### end errorstream\n\n")
      err.println("### begin outputstream:\n" + (if(cli.out == "") "None" else cli.out))
      err.println("### end outputstream")
      cli
   }

   class ParadoxCliOutput(var out:String, var err:String)
   {  def this() = this("","")
      override def toString =
      {  "ParadoxCliOutput:\n" + 
         "out = " + out + "\n" + 
         "err = " + err + "\n"
      }

      // Out: -1 if no model size was found
      def getModelSize:Int =
      {  var result = -1
         val regex = """% domain size is ([0-9]+)""".r // WIW: translate from vim syntax to java syntax: only left: | and ^\ are they correct?
         println("   matched groups:")
         for(matchLoc <- regex.findAllIn(out).matchData; group <- matchLoc.subgroups)
         {  println(group); if (group != null) result = group.toInt 
	 }
         result
      }
   }
}

}
