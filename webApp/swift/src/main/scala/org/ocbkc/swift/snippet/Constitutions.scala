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
import Helpers._
import System.err.println
import org.ocbkc.swift.model._

class Constitutions
{  val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.

   def list(ns: NodeSeq): NodeSeq =
   {  def displayConstis:NodeSeq = 
      {  if(sesCoord.Test.initConstitutions)
         {  println("Constitutions.displayConstis called")
            val c1 = Constitution.create(0) // test
            c1.shortDescription = "for analytical people"
            val c2 = Constitution.create(0) // test
            c2.shortDescription = "for visual people"
            sesCoord.Test.initConstitutions = false
         }
         if( Constitution.constis.size == 0 )
            Text("There is no constitution population yet...")
         else
         {  // val doc =  """"<ul>""" + Constitution.constis.map(c => """  <li> Constitution """ + c.id + "</li>").foldLeft("")((a,b) => a + "\n" + b) + "\n</ul>"
            // val doc =  <ul> Constitution.constis.map(c => <li> Constitution { c.id } </li>).foldLeft("")((a,b) => a b) </ul>
            // <&y2012.03.23.19:20:18& displayNoneIfEmpty doesn't work, don't know why>
            def displayNoneIfEmpty(d:String):String = if( d.equals("") ) "None" else d
            val doc =  Elem(null, "table", Null, TopScope,  
            <tr><td>ID</td><td>description</td></tr>::Constitution.constis.reverse.map(c => <tr><td><a href="">Constitution { c.id }</a></td>{ displayNoneIfEmpty(c.shortDescription) }<td><a href="">edit</a></td></tr>): _*  )
            println("   doc = " + doc)
            //XML.loadString(doc)
            doc
         }
      }
      
      def processCreateNewBt() =
      {  Player.currentUserId match
         {  case Full(id)  => { Constitution.create(id.asInstanceOf[String].toInt) }
            case _         => { throw new RuntimeException("  No user id found.") }
         }
      }

      val answer   = bind( "top", ns, 
                           "constisInTable"  -> displayConstis,
                           "createNewBt"    -> SHtml.button("Create", processCreateNewBt) // <&y2012.05.25.10:09:21& disable button when no user is logged in>
                         )
      answer
   }
}

}
}
