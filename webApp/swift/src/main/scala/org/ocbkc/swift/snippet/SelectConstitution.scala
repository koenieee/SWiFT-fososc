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

class SelectConstitution
{  val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.

   // if the URL contains an id for a constitution, then the choice has been made. So redirect to studyConstitution. Direct redirection ot studuConstitution after the choice cannot be done, because first the field firstChosenConstitution has to be set right, so that SiteMap (see Boot.scala) gives the user access to the studyConstitution link.
   S.param("id") match
   {  case Full(idLoc)  => {  val consti = Constitution.getById(idLoc.toInt) 
                              consti match
                              { case Some(constLoc)   => {  println("   Constitution id:" + idLoc )
                                                            sesCoordLR.firstChosenConstitution = consti
                                                            S.redirectTo("studyConstitution")
                                                         }
                                case None             => { S.redirectTo("notfound.html") }
                              }
                           }
      case _            => Unit // = do nothing, and just continue running the constructor of SelectConstitution
   }


   def render(ns: NodeSeq): NodeSeq =
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
         {  // val doc =  """"<ul>""" + Constitution.constis.map(c => """  <li> Constitution """ + c.id + "</li>").foldLeft("")((a,b) => a + "\n" + b) + "\n</ul>"
            // val doc =  <ul> Constitution.constis.map(c => <li> Constitution { c.id } </li>).foldLeft("")((a,b) => a b) </ul>
            // <&y2012.03.23.19:20:18& displayNoneIfEmpty doesn't work, don't know why>
            def displayNoneIfEmpty(d:String):String = if( d.equals("") ) "None" else d
            val doc =  Elem(null, "table", Null, TopScope,  
            <tr><td>ID</td><td>description</td></tr>::Constitution.constis.sortWith((c1,c2) => c1.id > c2.id ).map(c => <tr><td><a href={ "selectConstitution?id=" + c.id  }>Constitution { c.id }</a></td><td>{ displayNoneIfEmpty(c.shortDescription) }</td></tr>): _*  )
            // <&y2012.05.28.12:13:54& perhaps more elegant to refer to constitutions by using a html-parameter>
            // <&y2012.06.29.22:54:28& COULDDO optimise sorting function, by doing it only once, it is now done everytime.>
            println("   doc = " + doc)
            //XML.loadString(doc)
            doc
         }
      }
      /*
      def processCreateNewBt() =
      {  val const:Constitution = Player.currentUserId match
         {  case Full(id)  => { Constitution.create(id.asInstanceOf[String].toInt)  }
            case _         => { throw new RuntimeException("  No user id found.") }
         }
         S.redirectTo("constitution.html?id=" + const.id + "&edit=true&firstedit=true") // <&y2012.06.05.10:05:58& redirectTo, does this also terminate the execution of the method, or does the method remain indefinitely on the stack?>
      }
      */

      val answer   = bind( "top", ns, 
                           "constisInTable"  -> displayConstis
                           // "createNewBt"     -> SHtml.button("Create", processCreateNewBt) // <&y2012.05.25.10:09:21& disable button when no user is logged in>
                         )
      answer
   }
}

}
}
