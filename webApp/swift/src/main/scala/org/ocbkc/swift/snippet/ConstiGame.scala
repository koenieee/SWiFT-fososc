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
import org.ocbkc.swift.OCBKC.scoring._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import _root_.net.liftweb.widgets.tablesorter.{TableSorter, DisableSorting, Sorting, Sorter}
import org.ocbkc.swift.general.GUIdisplayHelpers._
import org.ocbkc.swift.global._

class ConstiGameTable
{  /* >>> Unfinished code
   val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.
      <<< */

// <&y2012.11.19.22:33:08& refactor: make one buildConstiTable for different snippets (constitutions.html, selectConstitution.html etc.)>

   def buildCollaborationConstiTable(ns: NodeSeq) = {
   // Calls bind repeatedly, once for each Entry in entries
   /* >>> Unfinished code

    &y2012.11.19.22:40:54& WIW Use this to complete the code that is under it:

   <tbody>{ .map(
                           c => <tr><td><a href={ "constitution?id=" + c.constiId  }>{ c.constiId }</a></td><td>{ displayNoneIfEmpty(c.shortDescription) }</td><td>{ optionToUI(ConstiScores.averageFluency(GlobalConstant.AverageFluency.minimalSampleSizePerPlayer, c.constiId, GlobalConstant.AverageFluency.fluencyConstantK)) }</td><td>{ optionToUI(ConstiScores.averagePercentageCorrect(GlobalConstant.AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer, c.constiId)) }</td><td>{ optionToUI(ConstiScores.averageDurationTranslation(GlobalConstant.AverageDurationTranslation.minimalNumberOfSessionsPerPlayer, c.constiId)) }</td><td>{ df.format(c.creationTime).toString }</td></tr>)
      <<< */

   Constitution.constis.filter(TODOconstisOfThisPlayer).flatMap({ row =>
      bind("constiColumn", chooseTemplate("top", "constiColumn", template),
      "id" -> <a href={ "constitution?id=" + c.constiId  }>{ c.constiId }</a>,
      "description" -> displayNoneIfEmpty(c.shortDescription),
      "fluency" -> optionToUI(ConstiScores.averageFluency(GlobalConstant.AverageFluency.minimalSampleSizePerPlayer, c.constiId, GlobalConstant.AverageFluency.fluencyConstantK)),
      "PCA" -> TODO,
      "ADT" -> TODO,
      "creationDate" -> TODO
      })

   def render(ns: NodeSeq): NodeSeq =
   {  val answer   = bind( "top", ns, 
                           "collaborationConstiTable"  -> buildCollaborationConstiTable(ns),
                         )

      answer
   }
}

}
}
