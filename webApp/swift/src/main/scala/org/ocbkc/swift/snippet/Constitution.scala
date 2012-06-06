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
import org.ocbkc.swift.global._

class ConstitutionSnippet
{  val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.

   def render(ns: NodeSeq): NodeSeq =
   {  println("ConstitutionSnippet.render")
      def processEditBtn(id:Int) =
      {  S.redirectTo("constitution?id=" + id + "&edit=true")
      }

      def processHistoryBtn() =
      {
      }

      def processCancelBtn(const:Constitution) =
      {  // <&y2012.06.06.19:21:59& SHOULDDO: first are you certain box if text area contains more than a few characters>
         Constitution.remove(const)
         S.redirectTo("constitutions")
      }

      def processConstitutionTA(taContent:String) =
      {
      }

      /* <? &y2012.06.02.14:38:52& what is an elegant way to progam the following? Problem is that if the pattern turns out to be None, you cannot return anything, or you have to use some ugly work around (tupling etc.) (go back with git to this date to get the right example...> */
      val emptyNode = <div></div> // <!-- empty node --> <&y2012.06.02.18:53:13& nicer way of defining empty substitution?>

      val (errorRetrievingConstitution:Boolean, const:Constitution, constitutionHtml, creator, creationDate, title) = S.param("id") match
      {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                              {  case Some(const) => { println("   Constitution id:" + idLoc); (false, const, const.loadHtml, Text("" + const.creatorUserID), Text(""+const.creationTime), Text("Constitution " + const.id)) }
                                 case None        => { (true, null, Text("No constitution with id " + idLoc + " exists"), emptyNode, emptyNode, Text("Constitution not found")) }
                              }

         case Empty        => (true, null, Text("Cannot retrieve constitution: incorrect parameters in URL: use /constitution?id=[some number here]."), emptyNode, emptyNode, Text("Constitution not found"))
   //    case HalfFull => Always better then empty ;-)
      }

      lazy val constitutionEditor = SHtml.textarea(const.loadHtml.toString, processConstitutionTA, "rows" -> "10", "cols" -> "100" )

      val editmode:Boolean = S.param("edit") match // <&y2012.06.05.10:33:56& how html parameters simply look if parameter exists, I want to do: if edit param is in then edit>
      {  case Full(pval) => { println("edit url param = " + pval); pval.equals("true") }
         case _          => false
      }


      val justCreated = false // <&y2012.06.05.22:07:34& finish: retrieve from parameter>
     
      val answer   = bind( "top", ns, 
                           "constitutionText"   -> { if(editmode && !errorRetrievingConstitution) { constitutionEditor } else { const.loadHtml } },
                           "editBt"             -> { if(!editmode && !errorRetrievingConstitution ) SHtml.button("Edit", () => processEditBtn(const.id)) else emptyNode }, // <&y2012.05.30.16:25:40& disable button when no user is logged in>
                           "revisionHistory"    -> SHtml.button("History", processHistoryBtn),
                           "creator"            -> creator,
                           "title"              -> title,
                           "creationDate"       -> creationDate,
                           "cancelBt"           -> { if(justCreated) SHtml.button("Cancel", () => processCancelBtn(const)) else emptyNode }
                         )
      answer
   }
}

}
}
