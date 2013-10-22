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
	
	val players_current_loggedin = Player.loggedInUsers()


	def settings() =
	{    // println(players_current_loggedin);

			"#row *" #> players_current_loggedin.map{x =>
        "#userid *" #> x.id.toString &
          "#username *" #> x.firstName &
          "#email *" #> x.email &
      "#subs *" #> x.firstChosenConstitution.get
      }


  }

}


}

