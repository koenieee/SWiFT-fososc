package org.ocbkc.swift.snippet

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.widgets.tablesorter.{TableSorter, DisableSorting, Sorting, Sorter}

class TableSorterDemo 
{  val headers = List( (0, Sorter("text")), (3,Sorter("currency")) )
   val sortList = (3,Sorting.DSC) :: Nil
  
   val options = TableSorter.options(headers,sortList)
  
   def render(xhtml: NodeSeq) :NodeSeq = 
   {  TableSorter("#myTable", options)
   }  
}
