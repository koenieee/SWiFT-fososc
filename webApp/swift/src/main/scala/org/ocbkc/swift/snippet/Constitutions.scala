package org.ocbkc.swift 
{
package snippet 
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
import net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.OCBKC.scoring._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
//import _root_.net.liftweb.widgets.tablesorter.{TableSorter, DisableSorting, Sorting, Sorter}
import org.ocbkc.swift.general.GUIdisplayHelpers._
import org.ocbkc.swift.global._
import org.ocbkc.swift.test._
import org.ocbkc.swift.global.Logging._

class Constitutions
{  val sesCoordLR = SesCoord.is; // extract session coordinator object from session variable.
/*
   val headers = List( (0, Sorter("digit")), (2, Sorter("float")), (3, Sorter("shortDate") ))
   
   val sortList = (0,Sorting.DSC) :: Nil

   val options = TableSorter.options(headers,sortList)
*/
   def list =
   {  def displayConstis: List[CssSel] =
      {  /*
         if(SesCoord.Test.initConstitutions) // only for testing, remove after test.
         {  val c1 = Constitution.create(0) // test
            c1.shortDescription = "for analytical people"
            val c2 = Constitution.create(0) // test
            c2.shortDescription = "for visual people"
            SesCoord.Test.initConstitutions = false
         }
         */
          // val doc =  """"<ul>""" + Constitution.constis.map(c => """  <li> Constitution """ + c.constiId + "</li>").foldLeft("")((a,b) => a + "\n" + b) + "\n</ul>"
            // val doc =  <ul> Constitution.constis.map(c => <li> Constitution { c.constiId } </li>).foldLeft("")((a,b) => a b) </ul>
            implicit val displayIfNone = "-"
            val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

            Constitution.constis.sortWith((c1,c2) => c1.constiId > c2.constiId ).map{
            c =>
               ".id *" #> { <a href={ "constitution?id=" + c.constiId  }>{ c.constiId }</a> } &
               ".description *" #> { c.shortDescription } &
               ".fluency *"   #>  { optionToUI(ConstiScores.averageFluencyLatestReleaseWithScore(c.constiId).collect{ case afs:(VersionId,Double) => afs._2 } ) } &
               ".APC *"       #> { optionToUI(ConstiScores.averagePercentageCorrect(GlobalConstant.AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer, c.constiId)) } &
               ".ADT *"       #> { optionToUI(ConstiScores.averageDurationTranslation(GlobalConstant.AverageDurationTranslation.minimalNumberOfSessionsPerPlayer, c.constiId)) } &
               ".creationDate *"   #>  { df.format(c.creationTime) }
               //
            }

      }
      
      def processCreateNewBt() =
      {  println("Constitutions.processCreateNewBt called")
         val const:Constitution = Player.currentUserId match
         {  case Full(id)  => {  val c = Constitution.create(id.toLong)
                                 c.initialiseNew
                                 c
                              }
            case _         => { throw new RuntimeException("  No user id found.") }
         }
         S.redirectTo("constitution.html?id=" + const.constiId + "&edit=true&firstedit=true") // <&y2012.06.05.10:05:58& redirectTo, does this also terminate the execution of the method, or does the method remain indefinitely on the stack?>
      }

      if( Constitution.constis.size == 0 ){
         log("No constitutions found")
         "#constitutionsTable" #> Text("There is no constitution population yet...")
      }
       else{
         log("Generating constiTable")
         ".tableRows *" #> {
            implicit val displayIfNone = "-"
            val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

            Constitution.constis.sortWith((c1,c2) => c1.constiId > c2.constiId ).map{
               c =>
                  log("ConstiID: " + c.constiId)
                     ".id *" #> SHtml.link("constitution?id=" + c.constiId, () => (), Text(c.constiId.toString))
                     ".description *" #>  c.shortDescription
                     ".fluency *"   #>   optionToUI(ConstiScores.averageFluencyLatestReleaseWithScore(c.constiId).collect{ case afs:(VersionId,Double) => afs._2 } )
                     ".APC *"       #>  optionToUI(ConstiScores.averagePercentageCorrect(GlobalConstant.AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer, c.constiId))
                     ".ADT *"       #>  optionToUI(ConstiScores.averageDurationTranslation(GlobalConstant.AverageDurationTranslation.minimalNumberOfSessionsPerPlayer, c.constiId))
                     ".creationDate *"   #>   df.format(c.creationTime)
               //
            }
         }
      }
      "#createNewBt"    #> SHtml.button("Create", processCreateNewBt) // <&y2012.05.25.10:09:21& disable button when no user is logged in>
   }
}

}
}
