package org.ocbkc.swift.reas
{
import org.ocbkc.swift.parser._
import org.ocbkc.swift.logilang._
import System.err.println
import org.ocbkc.swift.test.CLIwithFileInput

/* Assumed is: args(0) = input file, where first line is list of space separated constantnames, the rest is the folminqua file */
object EqualityEliminatorCLI extends CLIwithFileInput
{  def main(args: Array[String]) =
   {  def f(constantsAndFolminqua:String):String =
      {  val ft:FOLtheory = Folminqua2FOLtheoryParser.parseAll(Folminqua2FOLtheoryParser.folminquaTheory, constantsAndFolminqua) match
         {  case Folminqua2FOLtheoryParser.Success(ftl,_)  => ftl
            case  failMsg@Folminqua2FOLtheoryParser.Failure(_,_)           => return "  parse error: " + failMsg.toString
         }
         /* 
         <&y2012.04.13.19:31:57& finish. For now I don't read constants from first line...>
         val prefConstantsStr = TODOgetFirstLine(constantsAndFolminqua)
         val prefConstantNames = TODOextractConstantNames(constantStr)
         val prefConstants = prefConstantNames.map(getConstant(_))
         */
         def getConstant(cn:String) = ft.getConstant(cn) match
         {  case None    => { println("Constant: " + cn + " not present in theory..."); null }
            case Some(c) => c
         }

         // val prefConstantNames = List( "a", "b" )
         val prefConstantNames = List( "y", "z" )
         val prefConstants = prefConstantNames.map(getConstant(_))

 
         var ret:String = ""

         ret +=  "#### Original theory:\n" + ft +"\n\n"
   
         ret += "Constants:\n" + ft.constants + "\n"

         ret +=   "#### Theory with equalities eliminated with "

         ret += EqualityEliminator(ft, prefConstants)
         ret
      }
      applyFunctionToFile(f, args(0))
   }
}

object EqualityEliminator
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
   def apply(ft:FOLtheory, prefCs:List[Constant]):FOLtheory =
   {  //first remove equality statement from ft, and then do the constant replacement in the rest.
      println("eliminateEqualities")
      println("   ft was:" + ft)
      ft.stats.find( { case s:Equal => true; case _ => false } ) match
      {  case None => { println("   ft becomes:" + ft); ft }
         case Some(Equal(a,b)) => { ft.removeStat(Equal(a,b))  // <&y2012.04.13.09:29:37& can be made more efficient by collecting the index of the Equal stat during the find above.>
                                    if (prefCs.contains(a)) ft.substituteConstant(b,a)
                                    else if (prefCs.contains(b)) ft.substituteConstant(a,b)
                                    else ft.substituteConstant(b,a)
                                    println("   ft becomes:" + ft)
                                    apply(ft, prefCs)
                                  }
      }
   }
}

}
