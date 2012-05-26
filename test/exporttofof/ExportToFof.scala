package test.exporttofof
{
import System._
import java.io._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.parser._

object TestExportToTPTPfof extends CLIwithFileInput
{  def main(args: Array[String]) =
   {  def f(folminqua:String):String =
      {  val ft:FOLtheory = Folminqua2FOLtheoryParser.parseAll(Folminqua2FOLtheoryParser.folminquaTheory, folminqua) match
         {  case Folminqua2FOLtheoryParser.Success(ftl,_)  => ftl
            case  failMsg@Folminqua2FOLtheoryParser.Failure(_,_)           => return "  parse error: " + failMsg.toString
         }
     
         var ret:String = ""

         ret += "#### Theory in FOL.scala format, my friend:\n" + ft +"\n\n"
         ret += "#### Exported to tptp fof it becomes, became and will become, and never fail to become:\n\n"
         ret += ft.exportToTPTPfof
         ret
      }
      applyFunctionToFile(f, args(0))
   }
}

}
