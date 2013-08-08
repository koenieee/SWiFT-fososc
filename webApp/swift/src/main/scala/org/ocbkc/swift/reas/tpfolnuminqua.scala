package org.ocbkc.swift.reas
{
import org.ocbkc.swift.parser._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang.query.ComparisonOperator._
import org.ocbkc.swift.tpwrap._
import query._
import System.err.println
import java.io._

/* Assumed is: args(0) = input file, where first line is list of space separated constantnames, the rest is the folminqua file */
object TestTpFolnuminquaCLI extends CLIwithFileInput
{  def main(args: Array[String]) =
   {  if( args.length != 1 ) println("Usage: command filename")
      def f(folminquaFile:String):String =
      {  val ft:FOLtheory = Folminqua2FOLtheoryParser.parseAll(Folminqua2FOLtheoryParser.folminquaTheory, folminquaFile) match
         {  case Folminqua2FOLtheoryParser.Success(ftl,_)         => ftl
            case  failMsg@Folminqua2FOLtheoryParser.Failure(_,_)  => { "  parse error: " + failMsg.toString; new FOLtheory() }
         }
 
         var ret:String = ""

         val query = Sharpest(NumResPat(Geq, PatVar("n"), Var("x"), PredApp(Predicate("p",2),List(Constant("a"), Var("x"))))) // <&y2012.04.24.09:39:15& for test nice if you could also read this from the command line>

         ret += "#### Original theory:\n" + ft +"\n\n"
   
         ret += "#### Applying query\n\n"
         
         ret += "Query = " + query.toString + "\n"


         ret += "Answer = " + Folnuminqua.query(query, ft)

         ret
      }
      applyFunctionToFile(f, args(0))
   }
}

object Folnuminqua
{  
/*
- rewrite the equal statements such that the equal statements are eliminated in the following way:
go through all equality statements, a = b. For each: 
   If a is in C, relabel all constants b to a
      else if b is in C, relabel all constants a to b
      else relabel all constants to a. 
   Eliminate a = b

Purpose: project all inequality information into restrictions on constants in C.
Running example:
*/

// <&y2012.04.23.17:53:13& perhaps better to also define a Folminqua explicitly, now I use my FOL classes (which are I, btw,  think now identical to Folminqua, and not FOL)>
   def query(query:FolnuminquaQuery, ft:FOLtheory):Int =
   {  // &y2012.04.23.17:50:00& Only what matches is currently supported. Thus, warnings that the match statement is not exhaustive is a consequence of deliberate choice.
      println("####   incoming FOLtheory:\n" + ft)
      val ft_noEqStats = ft.copy // <_&y2012.04.28.22:23:54& how to do cloning in scala?>
      ft_noEqStats.removeStats( { case stat:Equal => true; case stat:Unequal => true; case _ => false } )
      println("\n####   (in)eq stats removed:\n" + ft_noEqStats)
      //println("   incoming theory must stay unaffected:\n" + ft)
      var ft_noEqStats_fof = ft_noEqStats.exportToTPTPfof
      
      // translate query to fof
      val queryFof:String =
      query match // <&y2012.04.24.10:02:31& perhaps use only one match on the query, and put interesting parts in aux. vars.
      {  case Sharpest(NumResPat(Geq, PatVar(numpatvarname), Var(boundvarname), PredApp(p,consts))) => 
         {  val pv = "PV" + boundvarname // <&y2012.04.24.19:49:43& not sure what requirements are of question-var in tptp fof format...>
            "fof(form" + ft_noEqStats.stats.length + ", question, ? [" + pv + "] : " + p.name + consts.map( c => if( c.name.equals(boundvarname) ) pv else c.name ).mkString("(",",",")") + ")."
            // <&y2012.04.24.19:54:14& would be nice to have a map which stops when it finds the first occurrence of an item satisfying a boolean function.
         }
      }
      ft_noEqStats_fof += queryFof
      // println("\n#### added query to theory:\n" + ft_noEqStats_)
      println("\n####   translated to fof and added query in fof format:\n" + ft_noEqStats_fof)

      // write to file
      var outFile = new File("ft_noEqStats.fof")
      var fullpath = outFile.getAbsolutePath
      println("\n####  creating file: " + fullpath)
      var out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.print(ft_noEqStats_fof)
      out.flush
      out.close

      // apply eprover
      val eproverResult = Eprover("--cpu-limit=30 --memory-limit=Auto --tstp-format -s --answers " + fullpath)
      println("####   eprover's result =\n" + eproverResult)
      val c:List[Constant] = eproverResult.extractConstants
      println("   extracted constants = " + c)
      if(c.length != 0) // needed, because Paradox will produce model size = 1 for a theory which only consists of fof(form0, , introconstants()) <? &y2012.05.07.11:38:43& isn't that incorrect? Ask Paradox developers/Geof Sutcliffe>
      {  // eliminate equality statements with c as preferred constants
         println("\n####   start eliminate equality statements")
         val ft_EqInEqIntroConsStats = ft.copy
         val pred = Predicate("introduceconstants", c.length) // create and add introduceconstants statement
         ft_EqInEqIntroConsStats.removeStats( { case stat:Equal => false; case stat:Unequal => false; case _ => true } )
         println("   create a statement to introduce all constants from C and transform to TPTP fof")
         ft_EqInEqIntroConsStats.addStat(PredApp(pred, c))
         println("      first drop everything but (in)equality stats:")
         println("      ft_EqInEqIntroConsStats =  " + ft_EqInEqIntroConsStats)
         println("      applying EqualityEliminitor:")
         val ft_onlyInEqIntroConsStats = EqualityEliminator(ft_EqInEqIntroConsStats, c)

         println("\n#### drop all inequality statements which contains at least one constant which is NOT in C.")
         def f(cs:List[Constant], stat:FOLstatement):Boolean =
         {  stat match
            {  case Unequal(c1, c2) => !cs.contains(c1) || !cs.contains(c2)
               case _               => false
            }
         }

         ft_onlyInEqIntroConsStats.removeStats(f(c,_))
         println("   ft_onlyInEqIntroConsStats becomes " + ft_onlyInEqIntroConsStats)
         println("\n#### apply paradox")
         val ft_onlyInEqIntroConsStats_fof = ft_onlyInEqIntroConsStats.exportToTPTPfof
         println("   ft_onlyInEqIntroConsStats_fof = " + ft_onlyInEqIntroConsStats_fof)
         // write to file
         outFile = new File("ft_onlyInEqIntroConsStats.fof")
         fullpath = outFile.getAbsolutePath
         println("\n####  creating file: " + fullpath)
         out = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
         out.print(ft_onlyInEqIntroConsStats_fof)
         out.flush
         out.close

         val paradoxResult = Paradox("--model --verbose 0 " + fullpath)
         println("   result of paradox:\n" + paradoxResult)
         // println("   model size = " + paradoxResult.getModelSize)
         /* Example output paradox:
   +++ PROBLEM: ft_onlyInEqIntroConsStats.fof
   Reading 'ft_onlyInEqIntroConsStats.fof' ... OK
   +++ SOLVING: ft_onlyInEqIntroConsStats.fof
   domain size 1
   domain size 2
   domain size 3
   +++ BEGIN MODEL
   % domain size is 3

   a = !1

   b = !2

   c = !3

   introduceconstants(X1,X2,X3,X4) <=> $true
   +++ END MODEL
   +++ RESULT: Satisfiable
   */
         paradoxResult.getModelSize
      }
      else 0
   }
}

}
