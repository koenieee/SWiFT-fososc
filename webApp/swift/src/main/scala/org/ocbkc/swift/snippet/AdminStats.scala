package org.ocbkc.swift

package snippet
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor, By}
import System.err.println
import org.ocbkc.swift.model._
import org.ocbkc.swift.global._

import org.ocbkc.swift.global.LiftHelpers._
import org.ocbkc.swift.coord.ses._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.test._
import ocbkc.swift.test.simulation.jara._
import org.ocbkc.swift.model.Player
import java.io._
import java.lang.{ Long => JLong }
import org.apache.commons.io.FileUtils;



/*
 * Cleanup non-used imports.
 * */




class AdminStats
{
	println("AdminStats  page is called")
	

//val test = Player.loggedIn match {
 // case Full(user) => "Hello " + user.firstName + "!"
 // case _ => "Who are you? Please login."
//}
	//def testLoggedIn(page : String)
   ///   val test = Player.currentUser.open_!
      //user.children.flatMap(child.name)
	
	//val players_current_loggedin = test
	//var n_blaat = "test";
	//Player.bulkDelete_!!(By(Player.superUser,false))
	 
	val players_current_loggedin = Player.findAll(By(Player.firstName, Player.firstName)) match
   {  case If(() => Player.loggedIn_? => {  println("logged iu")
                              
                           } // do nothing, player exists.
      case _            => {  "niet ingelogd"
                           }

   }
   

	def settings (xhtml : NodeSeq) : NodeSeq =
	{

		  bind("entry", xhtml,
			"LoggedPlayers" -> SHtml.text(players_current_loggedin, n_blaat = _))
			

	}

}
object SessionState {

   object loggedInUserName extends SessionVar[Box[String]](Empty)
}

}

