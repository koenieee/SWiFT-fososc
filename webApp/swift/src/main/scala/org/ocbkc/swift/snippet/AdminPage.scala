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
import org.ocbkc.swift.global.Logging._
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
import org.ocbkc.swift.jgit.InitialiseJgit
 
/*
 * Cleanup non-used imports.
 * */




class AdminPage
{
	println("Adminpage is called")
	//val path = GlobalConstant.WEBAPROOT
	val lines = TestSettings.readJaraDur
	var blaat = lines
	var n_blaat = lines
	 // println(lines)
	 
	 // the SessionVar will contain a String with "Anonymous" as default value.
	object JaraDur extends SessionVar[String]("JaraDuration")

	def settings (xhtml : NodeSeq) : NodeSeq =
	{
		
		  bind("entry", xhtml,
			"JaraSetting" -> SHtml.text(blaat, n_blaat = _),
			"changeDur" -> SHtml.submit("Change Duration", changeDur),
			"StartSimu" -> SHtml.submit("Start Simulation", startsimu))
	
	}
	def startsimu()
	{
		println("startsimu called");
		Player.bulkDelete_!!(By(Player.superUser,false))
		Constitution.removeAll

		FileUtils.deleteDirectory(new File(GlobalConstant.CONSTITUTIONOBJECTDIR))
		FileUtils.deleteDirectory(new File(GlobalConstant.CONSTITUTIONHTMLDIR))
		FileUtils.deleteDirectory(new File(GlobalConstant.CORECONTENTOBJECTDIR))

                InitialiseJgit()

                Constitution.createConstiAlphaIfDoesntExist

 		log("Calling Jara")	          
                PlayingSimulator.start(JaraDur.is.toLong)
		
	}
	
		
		

	def changeDur()
	{
    JaraDur.set(n_blaat)
    S.notice("Changed Jara Duration to: "+ n_blaat)
    
    
	//MetaMapper.bulkDelete_!!
	
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
		
}
		
	}
}


