package org.ocbkc.swift.general
{  
import System._

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
