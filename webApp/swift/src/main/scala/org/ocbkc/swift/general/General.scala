package org.ocbkc.generic
{

package random
{
import System._
import scala.util.Random

object RandomExtras
{  def nextBetween(ranSeq: Random, min:Int, max:Int):Int = 
   {  min + ranSeq.nextInt( max - min + 1 )
   }

   def pickRandomElementFromList[A](list:List[A], rs:Random):Option[A] =
   {  list match
      {  case Nil => None
         case _   => Some(list(rs.nextInt( list.length )))
      }
   }
}
}

object DateTime
{  import org.ocbkc.swift.global.Types._

   val dateFormat = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss") // also for reuse!

   def timeInMillis2dateString(time:TimeInMillis) =
   {  dateFormat.format(time).toString
   }
}
/*
//import javax.mail._ <&y2012.06.25.19:45:04& remove this, because I found net.liftweb.util.Mailer.  to do this>
//import javax.mail.internet._


object Mail
{  def send(aFromEmailAddr:String, aToEmailAddr:String, aSubject:String, aBody:String) = 
   {  //Here, no Authenticator argument is used (it is null).
      //Authenticators are used to prompt the user for user
      //name and password.
      val session:Session = Session.getDefaultInstance( fMailServerConfig, null )
      val message = new MimeMessage( session )
      try {
         //the "from" address may be set in code, or set in the
         //config file under "mail.from" ; here, the latter style is used
         //message.setFrom( new InternetAddress(aFromEmailAddr) );
         message.addRecipient(
         Message.RecipientType.TO, new InternetAddress(aToEmailAddr)
         )
         message.setSubject( aSubject )
         message.setText( aBody )
         Transport.send( message )
      }
      catch (ex:MessagingException){
         err.println("Cannot send email. " + ex)
      }
   }
}
*/
}
