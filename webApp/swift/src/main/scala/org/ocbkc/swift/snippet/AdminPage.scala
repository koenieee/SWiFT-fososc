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
import org.ocbkc.swift.OCBKC.Constitution
import org.ocbkc.swift.test._
import ocbkc.swift.test.simulation.jara._
import org.ocbkc.swift.model.Player
import java.io._
import java.lang.{ Long => JLong }
import org.apache.commons.io.FileUtils;
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._
import org.eclipse.jgit.storage.file._
/*
 * Cleanup non-used imports.
 * */




class AdminPage
{
	println("Adminpage is called")
	//val path = GlobalConstant.WEBAPROOT;
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
	//MetaMapper.bulkDelete_!!
	
	Player.bulkDelete_!!(By(Player.superUser,false))
        Constitution.removeAll
	/*	  val admin = Player.findAll(By(Player.firstName, GlobalConstant.ADMINFIRSTNAME)) match
   {  case Full(player) => {  println("is admin")
                              
                           } // do nothing, player exists.
      case _            => {  println("not admin => deleted")
                              val p = Player.create.firstName(GlobalConstant.ADMINFIRSTNAME).email("cg@xs4all.nl").password("asdfghjk").superUser(true).validated(true)  // <&y2012.08.30.20:13:36& TODO read this information from a property file, it is not safe to have it up here (in open source repo)>
                              p.save
                              p
                           }

   }*/
	/*	DB.use(DefaultConnectionIdentifier) {
		conn => DB.prepareStatement("TRUNCATE TABLE USERS;TRUNCATE TABLE CORECONTENT;TRUNCATE TABLE FOLLOWERCONSTI_JOIN;TRUNCATE TABLE PLAYERCORECONTENT_JOIN;", conn) 
				{
				  st => st.executeUpdate()
		}

		}*/
		
		FileUtils.deleteDirectory(new File(GlobalConstant.CONSTITUTIONOBJECTDIR));
		FileUtils.deleteDirectory(new File(GlobalConstant.CONSTITUTIONHTMLDIR));
		FileUtils.deleteDirectory(new File(GlobalConstant.CORECONTENTOBJECTDIR));


    // Initialise git repository for constitutions if there isn't one created yet.
    // Check whether there is already git tracking
    val gitfile = new File(GlobalConstant.CONSTITUTIONHTMLDIR + "/.git")
    if( gitfile.exists)
    { println("   .git file exists in " + GlobalConstant.CONSTITUTIONHTMLDIR + ", so everything is under (version) control, my dear organic friend...")
    }
    else
    { println("   .git file doesn't exist yet in " + GlobalConstant.CONSTITUTIONHTMLDIR + ", creating new git repo...")
      val jgitInitCommand:InitCommand = Git.init()
      jgitInitCommand.setDirectory(new File(GlobalConstant.CONSTITUTIONHTMLDIR))
      jgitInitCommand.call()
    }


		println("Jara called");
		   
		PlayingSimulator.start(JLong.parseLong(n_blaat));

	}
	
	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
	  val p = new java.io.PrintWriter(f)
	  try { op(p) } finally { p.close() }
	}
	

}
}


