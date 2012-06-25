package org.ocbkc.swift.general
{  
import System._
{  object Mail
   {  def send(aFromEmailAddr:String, aToEmailAddr:String, aSubject:String, aBody:String) = 
      {  //Here, no Authenticator argument is used (it is null).
         //Authenticators are used to prompt the user for user
         //name and password.
         session:Session = Session.getDefaultInstance( fMailServerConfig, null )
         message = new MimeMessage( session )
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
}
}
