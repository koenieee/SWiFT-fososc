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
{  val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.
   var constitutionTAcontent:String = ""
   var descriptionTFcontent:String = ""
   var const:Option[Constitution] = None
   val currentUserId:Int = Player.currentUserId match // <&y2012.06.23.14:41:16& refactor: put currentuserid in session var, and use that throughout the session-code>
      {  case Full(id)  => { id.toInt }
         case _         => { throw new RuntimeException("  No user id found.") }
      }
   var editmode = false

   def render(ns: NodeSeq): NodeSeq =
   {  println("ConstitutionSnippet.render")
      def processEditBtn(id:Int) =
      {  S.redirectTo("constitution?id=" + id + "&edit=true")
      }

      def processHistoryBtn() =
      {  
      }

      def processSaveBtn() =
      {  if( const.isDefined )
         {  val constLoc = const.get
            constLoc.save(constitutionTAcontent)
            constLoc.shortDescription = descriptionTFcontent
            S.redirectTo("constitution?id=" + constLoc.id + "&edit=true")
         } else // <&y2012.06.23.17:46:32& perhaps refactor, this cannot happen, because there is no save button when there is no constitution.>
         {  S.redirectTo("constitutions")
         }

      }
 
      def processGeneralSaveBtn() =
      {  if( const.isDefined )
         {  val constLoc = const.get
            if( editmode )
            {  constLoc.save(constitutionTAcontent)
               constLoc.shortDescription = descriptionTFcontent
            }
            S.redirectTo("constitution?id=" + constLoc.id + { if( editmode ) "&edit=true" else "" })
         } else // <&y2012.06.23.17:46:32& perhaps refactor, this cannot happen, because there is no save button when there is no constitution.>
         {  S.redirectTo("constitutions")
         }

      }
     
      def processCancelBtn(const:Constitution, firstEdit:Boolean) =
      {  // <&y2012.06.06.19:21:59& SHOULDDO: first are you certain box if text area contains more than a few characters>
          if(firstEdit)
          {  Constitution.remove(const) // if the constitution didn' exist yet (this was the first edit) then remove the complete constitution. Otherwise, simply discard the current edit and go back to the constitution just being edited.
             S.redirectTo("constitutions")
          }
          else
             S.redirectTo("constitution?id=" + const.id + "&edit=false")
      }

      def processDescriptionTf(descriptionTFcontentLoc:String) =
      {  descriptionTFcontent = descriptionTFcontentLoc
      }

      def processRemoveBtn(const:Constitution) =
      {  // <&y2012.06.18.16:18:55& dialog here> 
         Constitution.remove(const) // if the constitution didn' exist yet (this was the first edit) then remove the complete constitution. Otherwise, simply discard the current edit and go back to the constitution just being edited.
          S.redirectTo("constitutions.html")
      }

      def processConstitutionTA(taContent:String) =
      {  constitutionTAcontent = taContent
      }

      def processFollowCheckbox(checked:Boolean) =
      {  if( const.isDefined ) // <&y2012.06.23.14:52:50& necessary? The checkbox shouldn't even show when there is no constitution defined>
         {  val constLoc = const.get
            if( checked )
            {  if( !constLoc.followers.contains(currentUserId))
                  constLoc.followers ::= currentUserId
            }                  
            else
               constLoc.followers = constLoc.followers.filterNot( _ == currentUserId )
         }
      }
      /* <? &y2012.06.02.14:38:52& what is an elegant way to progam the following? Problem is that if the pattern turns out to be None, you cannot return anything, or you have to use some ugly work around (tupling etc.) (go back with git to this date to get the right example...> */
      val emptyNode = <div></div> // <!-- empty node --> <&y2012.06.02.18:53:13& nicer way of defining empty substitution?>
      var constLoc:Constitution = null // workaround for tuple error, note (object:ClassA, ..) = (null, ...) leads to match error in scala.
      val (errorRetrievingConstitution:Boolean, errorMsg:String, creator:Int, creationDate:Long, title:String) = S.param("id") match
      {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                              {  case Some(constLoc2) => { println("   Constitution id:" + idLoc); constLoc = constLoc2; (false, "", constLoc.creatorUserID, constLoc.creationTime, "Constitution " + constLoc.id) }
                                 case None        => { (true, "No constitution with this id exists.", 0, 0L, "Constitution not found") }
                              }
         case _           => S.redirectTo("constitutions") //(true, "Cannot retrieve constitution: incorrect parameters in URL: use /constitution?id=[some number here].", 0, 0L, "Constitution not found")
   //    case HalfFull => Always better then empty ;-)
      }

      if( constLoc != null) const = Some(constLoc)

      lazy val constitutionEditor = SHtml.textarea(constLoc.plainContent, processConstitutionTA, "rows" -> "10", "cols" -> "300" )

      editmode = S.param("edit") match // <&y2012.06.05.10:33:56& how html parameters simply look if parameter exists, I want to do: if edit param is in then edit>
      {  case Full(pval) => { println("edit url param = " + pval); pval.equals("true") }
         case _          => false
      }


      val firstEdit = S.param("firstedit") match
      {  case Full(pval) => { println("firstedit url param = " + pval); pval.equals("true") }
         case _          => false
      } // < &y2012.06.10.17:37:17& I think it is better to do this differently: do not create the constitution as yet, but do this after the first save. Danger of current approach is that if someones session crashes, the constitution continues to exist.>
     

   
      val answer   = bind( "top", ns, 
                           "revisionHistory"    -> SHtml.button("History", processHistoryBtn),
                           "followCheckbox"     -> SHtml.checkbox(constLoc.followers.contains(currentUserId), processFollowCheckbox),
                           "saveGeneralControlBt"             -> SHtml.button("Save", () => processGeneralSaveBtn),

                           "edit"          -> {   if( !editmode ) 
                                                         emptyNode
                                                      else
                                                         bind( "top", chooseTemplate("top","edit", ns),
                                                            "cancelBt" -> SHtml.button("Cancel", () => processCancelBtn(constLoc, firstEdit)),
                                                            "saveBt" -> SHtml.button("Save", () => processSaveBtn),
                                                            "descriptionTextfield" -> SHtml.text(constLoc.shortDescription, processDescriptionTf),
                                                            "constitutionEditor" -> constitutionEditor)
                                              },
                           "view"    -> {   if( editmode ) 
                                                         emptyNode
                                                      else
                                                         bind( "top", chooseTemplate("top","view", ns), 
                                                            "constitutionText" -> { if(!errorRetrievingConstitution) constLoc.contentInScalaXML else Text(errorMsg) }, 
                                                            "editBt" -> SHtml.button("Edit", () => processEditBtn(constLoc.id)))

                                        },
                           "creator"            -> { if( !errorRetrievingConstitution ) Text(creator.toString) else emptyNode },
                           "title"              -> Text(title),
                           "creationDate"       -> { if( !errorRetrievingConstitution ) Text(creationDate.toString) else emptyNode },
                           "description"        -> { if( !errorRetrievingConstitution ) Text(constLoc.shortDescription) else emptyNode }
                     )
      answer
   }
}

}
}
