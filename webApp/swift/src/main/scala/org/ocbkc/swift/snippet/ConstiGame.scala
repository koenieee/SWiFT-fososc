package org.ocbkc.swift 
{
package snippet 
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.OCBKC.scoring._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import _root_.net.liftweb.widgets.tablesorter.{TableSorter, DisableSorting, Sorting, Sorter}
import org.ocbkc.swift.general.GUIdisplayHelpers._
import org.ocbkc.swift.global._

class ConstiGameTable
{  val sesCoordLR = SesCoord.is // extract session coordinator object from session variable.

// <&y2012.11.19.22:33:08& refactor: make one buildConstiTable for different snippets (constitutions.html, selectConstitution.html etc.)>

   def buildCollaborationConstiTable(ns: NodeSeq):NodeSeq = {
      // Calls bind repeatedly, once for each Constitution that is followed
      println("buildCollaborationConstiTable called")
      val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")
      implicit val displayNoneAs = "-"
      val followedConstis = sesCoordLR.currentPlayer.followedConstis
      val tableRowsTemplate = chooseTemplate("top", "tableRows", ns)
      
      println("   tableRowsTemplate = " + tableRowsTemplate)
      println("   number of followed constis = " + followedConstis.size)
      followedConstis.flatMap{ constiId
      => {  val c = Constitution.getById(constiId).get // .get, because SHOULD always exist, otherwise some other bug exists.
            bind("constiColumn", tableRowsTemplate,
               "id" -> <a href={ "constitution?id=" + constiId}>{ constiId }</a>,
               "description" -> c.shortDescription,
               "fluency" -> optionToUI(ConstiScores.averageFluencyLatestReleaseWithScore(c.constiId).collect{ case afs:(VersionId,Double) => afs._2 }),
               "APC" -> optionToUI(ConstiScores.averagePercentageCorrect(GlobalConstant.AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer, c.constiId)),
               "ADT" -> optionToUI(ConstiScores.averageDurationTranslation(GlobalConstant.AverageDurationTranslation.minimalNumberOfSessionsPerPlayer, c.constiId)),
               "creationDate" -> df.format(c.creationTime).toString,
               "sessionConstiLink" -> SHtml.link("analyse/analyseFluencySessionsConsti.html?consti_id="+constiId,()=>(),Text("Session Consti"))
            )
         }
      }
   }   

   def render(ns: NodeSeq): NodeSeq =
   {  val answer   = bind( "top", ns, 
                           "tableRows"  -> buildCollaborationConstiTable(ns)
                         )

      answer
   }
}
}
}

