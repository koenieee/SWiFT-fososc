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
import org.ocbkc.swift.general.GUIdisplayHelpers._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.OCBKC.scoring._
import org.ocbkc.swift.global._


class SelectConstitution
{  println("Constructor SelectConstitution called")
   val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.
   val player = sesCoordLR.currentPlayer

   // if the URL contains an id for a constitution, then the choice has been made. So redirect to studyConstitution. Direct redirection ot studuConstitution after the choice cannot be done, because first790623 the field firstChosenConstitution has to be set right, so that SiteMap (see Boot.scala) gives the user access to the studyConstitution link.
   S.param("id") match
   {  case Full(idLoc)  => {  println("   URL parameter id = " + idLoc)
                              val consti = Constitution.getById(idLoc.toInt) 
                              consti match
                              { case Some(constLoc)   => {  println("   Found constitution with this id" )
                                                            sesCoordLR.URchooseFirstConstitution(constLoc.constiId)
                                                            println("   now redirecting player to studyConstitution")
                                                            S.redirectTo("/studyConstitution")
                                                         }
                                case None             => { S.redirectTo("notfound") }
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
         implicit val displayIfNone = "-"
         val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

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
               <thead><tr><th>id</th><th>description</th><th>fluency</th><th>APC</th><th>ADT</th><th>Creation date</th></tr></thead>,
               <tbody>{ Constitution.constisWithAReleaseOrVirginRelease.sortWith((c1,c2) => c1.constiId > c2.constiId ).map(
                           c => <tr><td><a href={ "selectConstitution?id=" + c.constiId  }>{ c.constiId }</a></td><td>{ displayNoneIfEmpty(c.shortDescription) }</td><td>{ optionToUI(ConstiScores.averageFluencyLatestReleaseWithScore(GlobalConstant.AverageFluency.minimalSampleSizePerPlayer, c.constiId, GlobalConstant.AverageFluency.fluencyConstantK).collect{ case afs:(VersionId,Double) => afs._2 } ) }</td><td>{ optionToUI(ConstiScores.averagePercentageCorrect(GlobalConstant.AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer, c.constiId)) }</td><td>{ optionToUI(ConstiScores.averageDurationTranslation(GlobalConstant.AverageDurationTranslation.minimalNumberOfSessionsPerPlayer, c.constiId)) }</td><td>{ df.format(c.creationTime).toString }</td></tr>)
               }
               </tbody>
            )
            // <&y2012.05.28.12:13:54& perhaps more elegant to refer to constitutions by using a html-parameter>
            // <&y2012.06.29.22:54:28& COULDDO optimise sorting function, by doing it only once, it is now done everytime.>
            println("   doc = " + doc)
            //XML.loadString(doc)
            doc
         }
      }


      val answer   = bind( "top", ns, 
                           "constisInTable"  -> displayConstis
                           // "createNewBt"     -> SHtml.button("Create", processCreateNewBt) // <&y2012.05.25.10:09:21& disable button when no user is logged in>
                         )
      answer
   }
}

}
}
