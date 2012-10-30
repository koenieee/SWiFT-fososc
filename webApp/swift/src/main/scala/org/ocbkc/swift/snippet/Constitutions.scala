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


class Constitutions
{  val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.

   // /* >>> Code in progress

   val headers = List( (0, Sorter("text")), (2, Sorter("float")) )
   val sortList = (0,Sorting.DSC) :: Nil

   val options = TableSorter.options(headers,sortList)

   //    <<< */
   def list(ns: NodeSeq): NodeSeq =
   {  def displayConstis:NodeSeq = 
      {  /*
         if(sesCoord.Test.initConstitutions) // only for testing, remove after test.
         {  val c1 = Constitution.create(0) // test
            c1.shortDescription = "for analytical people"
            val c2 = Constitution.create(0) // test
            c2.shortDescription = "for visual people"
            sesCoord.Test.initConstitutions = false
         }
         */
         if( Constitution.constis.size == 0 )
            Text("There is no constitution population yet...")
         else
         {  // val doc =  """"<ul>""" + Constitution.constis.map(c => """  <li> Constitution """ + c.constiId + "</li>").foldLeft("")((a,b) => a + "\n" + b) + "\n</ul>"
            // val doc =  <ul> Constitution.constis.map(c => <li> Constitution { c.constiId } </li>).foldLeft("")((a,b) => a b) </ul>
            // <&y2012.03.23.19:20:18& displayNoneIfEmpty doesn't work, don't know why>
            def displayNoneIfEmpty(d:String):String = if( d.equals("") ) "None" else d
            val doc = 
            Elem(
               null,
               "table",
               new UnprefixedAttribute("id", Text("constitutionsTable"), new UnprefixedAttribute("class", Text("tablesorter"), Null)),
               TopScope,  
               <thead><tr><td>ID</td><td>description</td><td>PCA</td></tr></thead>::{
                  Elem(
                     null,
                     "tbody",
                     Null,
                     TopScope,
                        Constitution.constis.sortWith((c1,c2) => c1.constiId > c2.constiId ).map(
                           c => <tr><td><a href={ "constitution?id=" + c.constiId  }>Constitution { c.constiId }</a></td><td>{ displayNoneIfEmpty(c.shortDescription) }</td><td>{ ConstiScores.averagePercentageCorrect(4, c.constiId) }</td></tr>): _* 
                  )
                  } 
            )
t
            // <&y2012.05.28.12:13:54& perhaps more elegant to refer to constitutions by using a html-parameter>
            // <&y2012.06.29.22:54:28& COULDDO optimise sorting function, by doing it only once, it is now done everytime.>
            println("   doc = " + doc)
            //XML.loadString(doc)
            doc
         }
      }
      
      def processCreateNewBt() =
      {  println("Constitutions.processCreateNewBt called")
         val const:Constitution = Player.currentUserId match
         {  case Full(id)  => {  Constitution.create(id.toLong)
                              }
            case _         => { throw new RuntimeException("  No user id found.") }
         }
         S.redirectTo("constitution.html?id=" + const.constiId + "&edit=true&firstedit=true") // <&y2012.06.05.10:05:58& redirectTo, does this also terminate the execution of the method, or does the method remain indefinitely on the stack?>
      }

      val answer   = bind( "top", ns, 
                           "constisInTable"  -> displayConstis,
                           "createNewBt"    -> SHtml.button("Create", processCreateNewBt) // <&y2012.05.25.10:09:21& disable button when no user is logged in>
                         )
      answer
      //TableSorter("#constitutionsTable", options)
   }
}

}
}
