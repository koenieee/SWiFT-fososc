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




class AdminPage
{
	println("Adminpage is called")
	val path = GlobalConstant.WEBAPROOT;
	var blaat = "nog niks";
	var n_blaat = "nog niks";

	def settings (xhtml : NodeSeq) : NodeSeq =
	{
		
		  bind("entry", xhtml,
			"JaraSetting" -> SHtml.text(blaat, n_blaat = _),
			"changeDur" -> SHtml.submit("Change Duration", changeDur))
	
	}
	def changeDur()
	{
			
		DB.use(DefaultConnectionIdentifier) {
		conn => DB.prepareStatement("TRUNCATE TABLE USERS;TRUNCATE TABLE CORECONTENT;TRUNCATE TABLE FOLLOWERCONSTI_JOIN;TRUNCATE TABLE PLAYERCORECONTENT_JOIN;", conn) 
				{
				  st => st.executeUpdate()
		}

		}
		FileUtils.deleteDirectory(new File("src/main/webapp/constitutions"));
		FileUtils.deleteDirectory(new File("persist/"));

		//CREATE NEW ADMIN USER
		val p = Player.create.firstName(GlobalConstant.ADMINFIRSTNAME).email("cg@xs4all.nl").password("asdfghjk").superUser(true).validated(true)  // <&y2012.08.30.20:13:36& TODO read this information from a property file, it is not safe to have it up here (in open source repo)>
                              p.save
                              p
		println("Jara called");
		PlayingSimulator.start(JLong.parseLong(n_blaat));

	}
	
	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
	  val p = new java.io.PrintWriter(f)
	  try { op(p) } finally { p.close() }
	}
	

}
}


