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
import org.ocbkc.swift.coord.ses._
import org.xml.sax.SAXParseException 

abstract class Error

case class NoPublishDescriptionError()  extends Error

case class ErrorInHtml(val saxParseException:SAXParseException) extends Error

case class ContentB4Reload(val constitutionTAcontent:String, val descriptionTFcontent:String, val publishDescriptionTAcontent:String)

class ConstitutionSnippet
{  println("ConstitutionSnippet constructor called")
   val sesCoordLR = sesCoord.is // extract session coordinator object from session variable.
   var constitutionTAcontent:String = ""
   var descriptionTFcontent:String = ""
   var publishDescriptionTAcontent:String = ""
   var const:Option[Constitution] = None // <&y2012.07.04.18:52:11& refactor: constitution must always be present for this page to be rendered (with its buttons), otherwise redirect to error page>
   object ContentB4ReloadRequestVar extends RequestVar[Option[ContentB4Reload]](None)
   object ErrorRequestVar extends RequestVar[List[Error]](Nil)
   val errorsLR = ErrorRequestVar.is // extract errors list from request var
   println("   errorsLR = "  + errorsLR)
   val contentB4ReloadOpt = ContentB4ReloadRequestVar.is
   println("   contentB4ReloadOpt = " + contentB4ReloadOpt )
   println("   find noPublishDescriptionError command gives: " + errorsLR.find( { case _:NoPublishDescriptionError => true; case _  => false } ) )
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
      {  S.redirectTo("history?id=" + const.get.id)
      }

      def updateConstitutionContent(const:Constitution) =
      {  var changes:Boolean = false
         if( const.plainContent.equals( constitutionTAcontent ) ) // <&y2012.06.27.12:59:21& refactor using javascript? So that you can see at the client whether someone has started typing, and thus has made a change. The current solution is computationally far more expensive.> 
         {  println("   No changes in constitution body.")
         }
         else
         {  const.save(constitutionTAcontent)
            changes = true
         }
         
         if( const.shortDescription.equals(descriptionTFcontent) )
         {  println("   No changes in description.")
         }
         else
         {  const.shortDescription = descriptionTFcontent
            changes = true
         }
         
         if( changes )
            sesCoordLR.mailFollowersUpdate(const, MailMessage.update2text(const)) // <&y2012.06.27.12:54:01& check whether something has really changed...>
      }
/*  <&y2012.07.29.14:31:05& perhaps for future version allow intermediate changes (not yet published)
      def processSaveBtn() =
      {  println("processSaveBtn called")
         if( const.isDefined )
         {  val constLoc = const.get
            updateConstitutionContent(constLoc)
            S.redirectTo("constitution?id=" + constLoc.id + "&edit=true")
         } else // <&y2012.06.23.17:46:32& perhaps refactor, this cannot happen, because there is no save button when there is no constitution.>
         {  S.redirectTo("constitutions")
         }

      }

*/
 
      def processGeneralSaveBtn() =
      {  println("processGeneralSaveBtn called")
         if( const.isDefined )
         {  val constLoc = const.get
            if( editmode )
            {  updateConstitutionContent(constLoc)
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

      def processPublishDescriptionTf(description:String) =
      {  publishDescriptionTAcontent = description
      }

      def processRemoveBtn(const:Constitution) =
      {  // <&y2012.06.18.16:18:55& dialog here> 
         Constitution.remove(const) // if the constitution didn' exist yet (this was the first edit) then remove the complete constitution. Otherwise, simply discard the current edit and go back to the constitution just being edited.
          S.redirectTo("constitutions.html")
      }

      def processPublishBtn() = 
      {  println("ConstitutionSnippet.processPublishBtn called")
         val contB4Rel = ContentB4Reload(constitutionTAcontent, descriptionTFcontent, publishDescriptionTAcontent)
         var errors:List[Error] = Nil
         if( const.isDefined ) // <&y2012.06.30.19:41:13& SHOULDDO: and only if something changed (you can probably check this with jgit)>
         {  val constLoc = const.get
            if( publishDescriptionTAcontent.equals("") ) // <&y2012.07.01.19:01:51& MUSTDO: or only containing white space characters>
            {  println("   no publish description found, give feedback to player (s)he should provide one!")
               errors = NoPublishDescriptionError() :: errors
            }
            else
            {  // first check for syntactic correctness of html file
               constLoc.checkCorrectnessXMLfragment(constitutionTAcontent) match
               {  case constLoc.XMLandErr(Some(xml), _)  => constLoc.publish(constitutionTAcontent, publishDescriptionTAcontent, currentUserId.toString)
                  case constLoc.XMLandErr(None, saxParseExeception)  => {  println("   Error in html: " + saxParseExeception.getMessage()); errors = ErrorInHtml(saxParseExeception) :: errors }
               }
            }
            S.redirectTo("constitution?id=" + constLoc.id + "&edit=true", () => (ErrorRequestVar( errors ), ContentB4ReloadRequestVar(Some(contB4Rel))))
         }
         else
         {  S.redirectTo("constitutions")
         }
      }

      def processConstitutionTA(taContent:String) =
      {  constitutionTAcontent = taContent
      }

      def processFollowCheckbox(checked:Boolean) =
      {  if( const.isDefined ) // <&y2012.06.23.14:52:50& necessary? The checkbox shouldn't even show when there is no constitution defined>
         {  val constLoc = const.get
            if( checked )
            {  if( !constLoc.followers.contains(currentUserId))
               {  constLoc.followers ::= currentUserId
                  sesCoordLR.mailFollowersUpdate(constLoc, MailMessage.newfollower(constLoc))
               }
            }                  
            else
            {  constLoc.followers = constLoc.followers.filterNot( _ == currentUserId )
               sesCoordLR.mailFollowersUpdate(constLoc, MailMessage.lostfollower(constLoc))
               // <&y2012.06.27.14:00:21& send mail to unfollowerto confirm.>
            }  
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
      val errHtml = errorsLR.find( { case _:ErrorInHtml => true; case _ => false } )
 
      lazy val constitutionEditor = SHtml.textarea( { errorsLR match
                                                      {  case List() => constLoc.plainContent
                                                         case _      => { println("   error in html, so prefill constitution editor with text before reload"); contentB4ReloadOpt.get.constitutionTAcontent } // if there is an error, then there is always a contentB4Reload, so you can do the get without problem.
                                                      }
                                                    }, processConstitutionTA, "rows" -> "10", "style" -> "width: 99%;" 
                                                  )
      editmode = S.param("edit") match // <&y2012.06.05.10:33:56& how html parameters simply look if parameter exists, I want to do: if edit param is in then edit>
      {  case Full(pval) => { println("edit url param = " + pval); pval.equals("true") }
         case _          => false
      }


      val firstEdit = S.param("firstedit") match
      {  case Full(pval) => { println("firstedit url param = " + pval); pval.equals("true") }
         case _          => false
      } // < &y2012.06.10.17:37:17& I think it is better to do this differently: do not create the constitution as yet, but do this after the first save. Danger of current approach is that if someone's session crashes, the constitution continues to exist.>
     
      val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

      val answer   = bind( "top", ns, 
                           "revisionHistory"    -> SHtml.button("History", processHistoryBtn),
                           "followCheckbox"     -> SHtml.checkbox(constLoc.followers.contains(currentUserId), processFollowCheckbox),
                           "saveGeneralControlBt"             -> SHtml.button("Save", () => processGeneralSaveBtn),
                           "numberOfFollowers"  -> Text(constLoc.followers.size.toString),
                           "edit"          -> {   if( !editmode ) 
                                                         emptyNode
                                                      else
                                                         bind( "top", chooseTemplate("top","edit", ns),
                                                            "cancelBt" -> SHtml.button("Cancel", () => processCancelBtn(constLoc, firstEdit)),
                                                            //"saveBt" -> SHtml.button("Save", () => processSaveBtn),
                                                            "descriptionTextfield" -> SHtml.text(contentB4ReloadOpt match { case Some(cB4rl) => cB4rl.descriptionTFcontent; case None => constLoc.shortDescription }, processDescriptionTf, "style" -> "width: 99%;"),
                                                            "noPublishDescriptionError" -> { if( errorsLR.find( { case _:NoPublishDescriptionError => true; case _  => false } ).isDefined) { println("   player forgot publish description, naughty boy."); Text("ERROR PLEASE PROVIDE THIS!") } else { println("   player provided publish description: good good boy."); emptyNode } },
                                                            "errorInHtml" -> { errHtml match 
                                                                              {  case Some(ErrorInHtml(e))  => Text("Error on line " + e.getLineNumber() + ", at character " + e.getColumnNumber() + ": " + e.getMessage())
                                                                                 case None                  => emptyNode
                                                                              }
                                                                             },        
                                                            "publishBt"          -> SHtml.button("Publish", () => processPublishBtn()),
                                                            "publishDescriptionTextfield" -> SHtml.text(contentB4ReloadOpt match { case Some(cB4rl) => cB4rl.publishDescriptionTAcontent; case None => "" }, processPublishDescriptionTf, "style" -> "width: 99%;"),
                                                            "constitutionEditor" -> constitutionEditor
                                                             )
                                               },
                           "view"    -> {    if( editmode )
                                                emptyNode
                                             else
                                                bind( "top", chooseTemplate("top","view", ns), 
                                                   "constitutionText" -> { if(!errorRetrievingConstitution) constLoc.contentInScalaXML else Text(errorMsg) }, 
                                                   "editBt" -> SHtml.button("Edit", () => processEditBtn(constLoc.id)))

                                        },
                           "creator"            -> { if( !errorRetrievingConstitution ) Text(creator.toString) else emptyNode },
                           "title"              -> Text(title),
                           "creationDate"       -> { if( !errorRetrievingConstitution ) Text(df.format(creationDate).toString) else emptyNode },
                           "description"        -> { if( !errorRetrievingConstitution ) Text(constLoc.shortDescription) else emptyNode }
                     )
      answer
   }
}

object MailMessage
{  def update2text(const:Constitution):String =
"""Constitution """ + const.id + """ has been updated. If you want to review the changes please visit this link:

""" + GlobalConstant.SWIFTURL  + "/constitution?id=" + const.id + """

""" + how2unfollow

   def newfollower(const:Constitution) =
"""Constitution """ + const.id + """ has a new follower. Visit this link to see all followers:

""" + GlobalConstant.SWIFTURL  + "/constitution?id=" + const.id + """

""" + how2unfollow

   def lostfollower(const:Constitution) =
"""Constitution """ + const.id + """ lost a follower. Visit this link to see all followers:

""" + GlobalConstant.SWIFTURL  + "/constitution?id=" + const.id + """

""" + how2unfollow

   val how2unfollow = 
"""You are receiving this email because you are a follower of the mentioned constitution of the SWiFT game. If you want to unfollow the constitution, visit the above link.
"""
}
}
}
