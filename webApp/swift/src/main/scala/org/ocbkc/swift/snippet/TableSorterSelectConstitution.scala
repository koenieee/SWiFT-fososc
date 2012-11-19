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

class TableSorterSelectConstitution
{  val headers = List( (0, Sorter("text")), (2, Sorter("digit")), (3, Sorter("digit")) )
   val sortList = List( (2,Sorting.DSC))

   val options = TableSorter.options(headers,sortList)

   def render(ns: NodeSeq): NodeSeq =
   {  TableSorter("#constitutionsTable", options)
   }
}

}
}
