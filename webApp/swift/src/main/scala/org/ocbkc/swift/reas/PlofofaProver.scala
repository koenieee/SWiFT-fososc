// <rename reas to prover>
package org.ocbkc.swift.reas.plofofa
{
import org.ocbkc.swift.reas
import org.ocbkc.swift.parser._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.fofa._
import org.ocbkc.swift.logilang.fofa
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang.query.ComparisonOperator._
import org.ocbkc.swift.logilang.query.plofofa._
import org.ocbkc.swift.logilang.query.plofofa
import org.ocbkc.swift.tpwrap._
import query._
import System.err.println
import java.io._
import org.ocbkc.swift.test.CLIwithFileInput
import org.ocbkc.swift.global.Logging._
//import org.specs2.mutable._

/*class PlofofaProverSpec extends Specification
{  val predicateB = Predicate("B",1)
   val plofofaQuery = MostInfo(PatVar("s"), Forall(Var("x"), PatVar("s"), PredApp(predicateB, List(Var("x")))))
   val folTheory = new FOLtheory
   folTheory.addStat(PredApp_FOL(predicateB, List(Constant("makkelPowerConnect"))))

   val queryResult = Prover.query(plofofaQuery, folTheory)

   "Prover.query(plofofaQuery, folTheory)" should
   {  " equal null, because we are still testing" in
      {  queryResult must be equalTo(null)
      }
   }*/
   /*
   "The 'Hello world' string" should {
 "contain 11 characters" in {
   "Hello world" must have size(11)
 }
 "start with 'Hello'" in {
   "Hello world" must startWith("Hello")
 }
 "end with 'world'" in {
   "Hello world" must endWith("world")
   */
//}

object TestPlofofaProverCLI extends CLIwithFileInput
{  def main(args: Array[String]) =
   {  println("SWiFTpom.dir == " + System.getProperty("SWiFTpom.dir"))

      if( args.length != 0 ) println("Usage: command (without parameters)")
      else
      {  val predicateB = Predicate("B",1)
         val predicateF = Predicate("F",1)
         val plofofaQuery = MostInfo(PatVar("s"), plofofa.Forall(Var("x"), PatVar("s"), PredApp_Plofofa(predicateB, List(Var("x")))))
         val folTheory = new FOLtheory
         folTheory.addStat(PredApp_FOL(predicateB, List(Constant("makkelPowerConnect"))))
         folTheory.addStat(PredApp_FOL(predicateB, List(Constant("loxolopPower"))))
         folTheory.addStat(PredApp_FOL(predicateF, List(Constant("ebePower"))))
         folTheory.addStat(PredApp_FOL(predicateB, List(Constant("rotsigeBodemMakkeltje"))))
         val result = Prover.query(plofofaQuery, folTheory)
      }
      /*
      if( args.length != 1 ) println("Usage: command filename")
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


         ret += "Answer = " + Prover.query(query, ft)

         ret
      }
      applyFunctionToFile(f, args(0))
      */
   }
}

object Prover extends reas.ProverTrait
{  /** This method represents a 
     * @todo shoulddo: change FOLtheory to FOLtheory_rb
     */
   def query(queryParam:PlofofaPat_rb, ft:FOLtheory):FofaSent =
   {  this.query(queryParam.sf, ft)
   }

   def query(queryParam:PlofofaPat, ft:FOLtheory):FofaSent =
   {  log("reas.plofofa.Prover called")
      log("   query = " + queryParam)

      // translate FOLtheory to FOF
      val ftFof = ft.exportToTPTPfof
      // translate query to FOF
      val queryFof = queryParam match
      {  case MostInfo(patVar, forallPat) =>
         {  forallPat match
            {  case plofofa.Forall(forallVar:Var, setPatVar:PatVar, predApp:PredApp) =>
               {  if( setPatVar != patVar )
                  {  log("   error in query, pattern variables are not the same")
                  } else
                  {  // transform to fof query
                     val fofPVname = "PV" + forallVar.name // <rename to official name of pattern variables in fof.> Possibly confusing: pattern variable in Plofofa != pattern variable in FOF (in this case)!
                     predApp match
                     {  case  PredApp_Plofofa(p, consts) =>
                        {  "fof(form" + ft.stats.length + ", question, ? [" + fofPVname + "] : " + p.name + consts.map( c => if( c.equals(forallVar) ) fofPVname else c.name ).mkString("(",",",")") + ")."
                        }
                     }
                  }
               }
            }
         }
      }
      val ftAndQueryFof = ftFof + queryFof
      println("\n####   FOLtheory translated to fof and added query in fof format:\n" + ftAndQueryFof)

      // write to file
      var outFile = new File("ft.fof")
      var fullpath = outFile.getAbsolutePath
      println("\n####  creating file: " + fullpath)
      var out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.print(ftAndQueryFof)
      out.flush
      out.close

      // apply eprover
      val eproverResult = Eprover("--cpu-limit=30 --memory-limit=Auto --tstp-format -s --answers " + fullpath)
      println("####   eprover's result =\n" + eproverResult)

      val result = queryParam match
      {  case MostInfo(patVar, plofofa.Forall(forallVar:Var, setPatVar:PatVar, PredApp_Plofofa(pred, terms))) =>
         {  fofa.Forall(forallVar, eproverResult.extractConstants, PredApp_Fofa(pred, terms))
         }
      }

      log(" Final result: " + result)

      result
   }
}

}
