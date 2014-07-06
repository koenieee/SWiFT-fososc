package org.ocbkc.swift 

package snippet 
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.js._ 
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.ocbkc.swift.global._
import org.ocbkc.swift.global.LiftHelpers._
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.DisplayHelpers._
import org.ocbkc.swift.coord.ses._
import org.ocbkc.swift.general.GUIdisplayHelpers._
import org.ocbkc.swift.OCBKC.scoring._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._

import org.xml.sax.SAXParseException 
import org.ocbkc.swift.model.Player

import org.ocbkc.swift.messages._
import org.ocbkc.swift.messages.MailUtils._
import org.ocbkc.swift.messages.MailMessages._

abstract class Error

case class NoPublishDescriptionError()  extends Error

case class ErrorInHtml(val saxParseException:SAXParseException) extends Error

case class ContentB4Reload(val constitutionTAcontent:String, val descriptionTFcontent:String, val publishDescriptionTAcontent:String)

/** @todo &y2013.01.27.17:19:24& move to more general lib, and add the other fields (always optional), as soon as needed.
  *  
  */

class ConstitutionSnippet
{  println("ConstitutionSnippet constructor called")
   val sesCoordLR = SesCoord.is // extract session coordinator object from session variable.
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
   val currentUserId:Long = Player.currentUserId match // <&y2012.06.23.14:41:16& refactor: put currentuserid in session var, and use that throughout the session-code>
      {  case Full(id)  => { id.toLong }
         case _         => { logAndThrow("  No user id found.") }
      }
   var editmode = false

   def render(ns: NodeSeq): NodeSeq =
   {  println("ConstitutionSnippet.render")
      def processEditBtn(constiId:Int) =
      {  S.redirectTo("constitution?id=" + constiId + "&edit=true")
      }

      def processHistoryBtn() =
      {  S.redirectTo("history?id=" + const.get.constiId)
      }

      def analyseScoresBtn() =
      {  S.redirectTo("analyse/analyseFluencySessionsConsti?consti_id=" + const.get.constiId)
      }

/* &y2013.01.27.18:57:58& Still needed? 

      def updateConstitutionContent(const:Constitution) =
      {  println("updateConstitutionContent called")
         var changes:Boolean = false
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
         {  mailOtherFollowersUpdate(const, MailMessage.update2text(const), currentUserId)
         }
      }
*/

/*  <&y2012.07.29.14:31:05& perhaps for future version allow intermediate changes (not yet published)
      def processSaveBtn() =
      {  println("processSaveBtn called")
         if( const.isDefined )
         {  val constLoc = const.get
            updateConstitutionContent(constLoc)
            S.redirectTo("constitution?id=" + constLoc.constiId + "&edit=true")
         } else // <&y2012.06.23.17:46:32& perhaps refactor, this cannot happen, because there is no save button when there is no constitution.>
         {  S.redirectTo("constitutions")
         }

      }

*/

   /** Deprecated
     * Call this when const has been updated, and you want to notify all followers.
     * All followers are mailed, except for the follower who did the update (that is the one this snippet is serving right now.)
     */


      def processGeneralSaveBtn() =
      {  println("processGeneralSaveBtn called")
         if( const.isDefined )
         {  val constLoc = const.get
            /*
            if( editmode )
            {  updateConstitutionContent(constLoc)
            }
            */
            S.redirectTo("constitution?id=" + constLoc.constiId + { if( editmode ) "&edit=true" else "" })
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
             S.redirectTo("constitution?id=" + const.constiId + "&edit=false")
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
               {  case constLoc.XMLandErr(Some(xml), _) =>
                  {  SesCoord.URpublishConsti(constLoc, constitutionTAcontent, publishDescriptionTAcontent)
                  }
                  case constLoc.XMLandErr(None, saxParseExeception)  => { println("   Error in html: " + saxParseExeception.getMessage); errors = ErrorInHtml(saxParseExeception) :: errors }
               }
            }
            S.redirectTo("constitution?id=" + constLoc.constiId + "&edit=true#kippetje", () => (ErrorRequestVar( errors ), ContentB4ReloadRequestVar(Some(contB4Rel))))
         }
         else
         {  S.redirectTo("constitutions")
         }
      }

      def processConstitutionTA(taContent:String) =
      {  constitutionTAcontent = taContent
      }

      def processReleaseCandidateCb(checked:Boolean) =
      {  log("processReleaseCandidateCb called.")
         val constLoc = const.get
         if(!sesCoordLR.URsetReleaseCandidate(constLoc, checked))
         {  log("Uncheck the checkbox, [SHOULDDO:] and give a warning.")

            // documentation on html checkbox:
            // http://www.w3.org/TR/html-markup/input.checkbox.html 
            JsCmds.SetElemById("releaseCandidateCb", JsExp.strToJsExp(""), "checked") & JsCmds.Alert("I'm afraid setting release candidate is forbidden... (TODO improve this message).")
         } else
         {  JsCmds.Noop
         }
      }

      def processFollowCheckbox(checked:Boolean) =
      {  if( const.isDefined ) // <&y2012.06.23.14:52:50& necessary? The checkbox shouldn't even show when there is no constitution defined>
         {  val constLoc = const.get
            if( checked && !constLoc.followers.contains(currentUserId) )
            {  sesCoordLR.addFollower(sesCoordLR.currentPlayer, constLoc)
               sendOtherFollowersUpdateMail(constLoc, MailMessages.newfollower(constLoc), sesCoordLR.currentPlayer)
            } else if( !checked && constLoc.followers.contains(currentUserId) )
            {  sesCoordLR.removeFollower(sesCoordLR.currentPlayer, constLoc )
               // <&y2012.06.27.14:00:21& send mail to unfollower to confirm.>
            } else
            {  println("   update mail to " + currentUserId + " not necessary: follower status unchanged.")
            }
         }
      }
      /* <? &y2012.06.02.14:38:52& what is an elegant way to progam the following? Problem is that if the pattern turns out to be None, you cannot return anything, or you have to use some ugly work around (tupling etc.) (go back with git to this date to get the right example...> */
      var constLoc:Constitution = null // workaround for tuple error, note (object:ClassA, ..) = (null, ...) leads to match error in scala.
      var creator:Player = null
      val (errorRetrievingConstitution:Boolean, errorMsg:String, creatorId:Long, creationDate:Long, title:String) = S.param("id") match
      {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                              {  case Some(constLoc2) =>
                                 {  println("   Constitution id:" + idLoc)
                                    constLoc = constLoc2
                                    creator = Player.find(constLoc.creatorUserID) match
                                    {  case Full(player)  => player
                                       case _             => throw new RuntimeException("Player with id " + constLoc.creatorUserID + " not found.")
                                    }
                                    (false, "", constLoc.creatorUserID, constLoc.creationTime, "Constitution " + constLoc.constiId)
                                 }
                                 case None        => 
                                 {  S.redirectTo("constitutionNotFound")//(true, "No constitution with this id exists.", 0, 0L, "Constitution not found")
                                 }
                              }
         case _           => S.redirectTo("constitutions") //(true, "Cannot retrieve constitution: incorrect parameters in URL: use /constitution?id=[some number here].", 0, 0L, "Constitution not found")
   //    case HalfFull => Always better then empty ;-)
      }

      if( constLoc != null) const = Some(constLoc)
      val errHtml = errorsLR.find( { case _:ErrorInHtml => true; case _ => false } )
 
      lazy val constitutionEditor = SHtml.textarea( { errorsLR match
                                                      {  case List() => constLoc.plainContent
                                                         case _      => { log("   error in html, so prefill constitution editor with text before reload"); contentB4ReloadOpt.get.constitutionTAcontent } // if there is an error, then there is always a contentB4Reload, so you can do the get without problem.
                                                      }
                                                    }, processConstitutionTA, "rows" -> "10", "style" -> "width: 99%;", "id" -> "edit" 
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
      implicit val displayIfNone = "-"
      val fluencyScoreOpt = ConstiScores.averageFluencyLatestReleaseWithScore( constLoc.constiId )
      val latestReleaseIdOpt = fluencyScoreOpt.collect{ case (id,_) => id }
      println("CURRENTUSERID::");
	println(constLoc.followers.contains(currentUserId));
      val answer   = bind( "top", ns, 
                           "revisionHistory"    -> SHtml.button("History", processHistoryBtn),
                           "analyseScores"    -> SHtml.button("Analyse Scores", analyseScoresBtn),
                           "followCheckbox"     -> SHtml.ajaxCheckbox(constLoc.followers.contains(currentUserId), selected => processFollowCheckbox(selected)),//SHtml.checkbox(constLoc.followers.contains(currentUserId), processFollowCheckbox),
                           "saveGeneralControlBt"             -> SHtml.button("Save", () => processGeneralSaveBtn),
                           "numberOfFollowers"  -> Text(constLoc.followers.size.toString),
                           "followerInstructions" ->
                           {  if( !constLoc.followers.contains(currentUserId) ) 
                                 emptyNode
                              else
                                 Text("TODO: you are a follower, so here some guidelines")
                           },
                           "edit"          -> {   if( !editmode ) 
                                                         emptyNode
                                                      else
                                                         bind( "top", chooseTemplate("top","edit", ns),
                                                            "cancelBt" -> SHtml.button("Cancel", () => processCancelBtn(constLoc, firstEdit)),
                                                            //"saveBt" -> SHtml.button("Save", () => processSaveBtn),
                                                            "descriptionTextfield" -> SHtml.text(contentB4ReloadOpt match { case Some(cB4rl) => cB4rl.descriptionTFcontent; case None => constLoc.shortDescription }, processDescriptionTf, "style" -> "width: 99%;"),
                                                            "noPublishDescriptionError" -> { if( errorsLR.find( { case _:NoPublishDescriptionError => true; case _  => false } ).isDefined) { println("   player forgot publish description, naughty boy."); <font color="red" ><b>ERROR: PROVIDE</b></font> } else { println("   player provided publish description: good good boy."); emptyNode } },
                                                            "errorInHtml" -> { errHtml match 
                                                                              {  case Some(ErrorInHtml(e))  => <font color="red"><b>{ "COULD NOT PUBLISH:\nError on line " + e.getLineNumber + ", at character " + e.getColumnNumber + ": " + e.getMessage }</b></font>
                                                                                 case None                  => emptyNode
                                                                              }
                                                                             },        
                                                            "publishBt"          -> SHtml.button("Publish", () => processPublishBtn()),
                                                            "publishDescriptionTextfield" -> SHtml.text(contentB4ReloadOpt match { case Some(cB4rl) => cB4rl.publishDescriptionTAcontent; case None => "" }, processPublishDescriptionTf, "style" -> "width: 99%;"),
                                                            "constitutionEditor" -> constitutionEditor
                                                             )
                                               },
                           "view"    -> {    log("[MUSTDO] display button if latest release is a ReleaseVirgin. Pressing the button while remove the releasevirgin status. When the player clicks it, reload page to show the checkbox for Release Candidate again.")
                                             if( editmode )
                                                emptyNode
                                             else
                                                bind( "top", chooseTemplate("top","view", ns), 
                                                   "constitutionText" -> { if(!errorRetrievingConstitution) constLoc.contentInScalaXML else Text(errorMsg) }, 
                                                   "editBt" -> SHtml.button("Edit", () => processEditBtn(constLoc.constiId)),
                                                   "releaseCandidateCb"     ->
                                                   {  val accessToReleaseCandidateCb =
                                                      {  if( ( constLoc.releaseStatusLastVersion == Some(Release) ) || !( constLoc.leadersUserIDs.contains(currentUserId) ) )
                                                         {  log("   latestVersionIsRelease or current player isn't leader of this consti")
                                                            List("disabled" -> "disabled")
                                                         }  else
                                                         {  Nil
                                                         }
                                                      }

                                                      SHtml.ajaxCheckbox(constLoc.releaseStatusLastVersion == Some(ReleaseCandidate), checked => processReleaseCandidateCb(checked), ("id" -> "releaseCandidateCb" ) :: accessToReleaseCandidateCb:_*)
                                                   }
                                                )

                                        },
                           "creator"            -> { if( !errorRetrievingConstitution ) Text(creator.swiftDisplayName) else emptyNode },
                           "leaders"            -> { if( !errorRetrievingConstitution ) 
                                                     {   val leaderNamesStringList = constLoc.leadersUserIDs.map
                                                               {  userId => Player.find(userId) match
                                                                  {  case Full(player) => player.swiftDisplayName
                                                                     case _            => throw new RuntimeException("Player with id " + userId + " not found.")
                                                                  }
                                                               }
                                                         log("   leaderNamesStringList = " + leaderNamesStringList)
                                                         Text(leaderNamesStringList.mkString(", "))
                                                     } 
                                                     else
                                                      emptyNode
                                                   },
                           "title"              -> Text(title),
                           "creationDate"       -> { if( !errorRetrievingConstitution ) Text(df.format(creationDate).toString) else emptyNode },
                           "latestRelease"      -> { Text(optionToUI( latestReleaseIdOpt.collect{ case lr:VersionId => "R" + constLoc.releaseIndex(lr) } ) )
                                                   },
                           "fluency"            -> { Text(optionToUI(fluencyScoreOpt.collect{ case (_, fs) => fs }.map{ defaultRounding } )) },
                           "description"        -> { if( !errorRetrievingConstitution ) Text(constLoc.shortDescription) else emptyNode }
                     )
      answer
   }
}

}
