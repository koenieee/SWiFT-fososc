package org.ocbkc.swift.messages

import org.ocbkc.swift.model.Player
import org.ocbkc.swift.OCBKC._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import net.liftweb.util.Mailer
import net.liftweb.util.Mailer._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import System.err.println
import org.ocbkc.swift.global._

/** Mail represents a mail-message. It is used in various methods that can send mails, such us MailUtils.sendMail
  */
case class Mail(to: Option[String], subject:String, body:String)

/** utilities to send mails to players of the game. Actual message-content is not defined in this object, but in MailMessages
  */
object MailUtils
{   /** Mails a mail to multiple or one player(s).
     * @param mail the mail to be sent
     * @param players the players to which the mail will be sent
     * This is the basic mail method - try to use the auxiliary methods (mailAllFollowersUpdate, etc.) instead of this one when possible.
     * 
     */
   def sendMail(mail:Mail, players:List[Player])
   {  players.foreach{ p => sendMail(mail, p) }
      Unit
   }

   /** Same as sendMail, but now to only one player
     */
   def sendMail(mail:Mail, player:Player)
   {  println("sendMail called")
      println("   player email = " + player.email.get)
      Mailer.sendMail(From("cg@xs4all.nl"), Subject(mail.subject), To(player.email.get), new PlainMailBodyType(mail.body))
      println("   mail sent!")
      Unit
   }

   /** Given a follower of constitution const, mail the other followers of that same constitution the given mail
     */
   def sendOtherFollowersUpdateMail(const: Constitution, mail:Mail, thisFollower: Player ) =
   {  sendMail(mail, const.followersAsPlayerObjs.filterNot(_ == thisFollower) )
   }

   /** Mail all followers of constitution const the given mail
     */
   def sendAllFollowersUpdateMail(const: Constitution, mail:Mail) =
   {  sendMail(mail, const.followersAsPlayerObjs )
   }
}


/** This object contains (building blocks for, and complete) prefabricated mail messages.
  */
object MailMessages
{  private def sentenceOpening(const:Constitution) = "Constitution " + const.constiId

   /** Prefabricated mail message: the message to be sent to all followers when a constitution has been updated (new publication).
     */
   def newPublication(const:Constitution):Mail =
Mail(
None,
sentenceOpening(const) + " has been edited by someone else...",
"""Constitution """ + const.constiId + """ has been edited by someone else. If you want to review the changes please visit this link:

""" + GlobalConstant.SWIFTURL  + "/constitution?id=" + const.constiId + """
s
""" + how2unfollow
)

   /** Prefabricated mail message: the message to be sent when a consti has a new follower.
     */
   def newfollower(const:Constitution) =
Mail(
None,
sentenceOpening(const) + " has a new follower!"
,
"""Great news... constitution """ + const.constiId + """, a constitution which you already follow, has a new follower. Visit this link to see all followers:

""" + GlobalConstant.SWIFTURL  + "/constitution?id=" + const.constiId + """

""" + how2unfollow
)

   def lostfollower(const:Constitution) =
      Mail(
         None,
sentenceOpening(const) + " lost a follower :-(..."
         ,
sentenceOpening(const) + """ lost a follower. Visit the following link to see all followers:

""" + link2consti(const) + """

""" + how2unfollow
      )

   def newFluencyScore(const:Constitution, releaseId:VersionId) =
   {  Mail(
         None,
         sentenceOpening(const) + " received a fluency score!",
         "Release " + const.releaseIndex(releaseId) + " of " + sentenceOpening(const) + """, a constitution which you follow, received its first or an updated fluency score! You are a follower of this constitution, so quickly click the following link to compare the score of this release, with scores of other constitutions (is it better?), and the previous releases of this constitution (has it improved?):

""" + link2consti(const) + """

""" + how2unfollow // TODO &y2013.01.28.18:56:53& also include score numbers here.
      )
   }

   private val how2unfollow =
"""You are receiving this email because you are a follower of the mentioned constitution of the SWiFT game. If you want to unfollow the constitution, please visit the above link.
"""
   private def link2consti(const:Constitution) = GlobalConstant.SWIFTURL  + "/constitution?id=" + const.constiId
}

