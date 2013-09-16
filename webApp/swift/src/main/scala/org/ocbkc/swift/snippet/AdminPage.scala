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

import _root_.net.liftweb.http._
import js._
import JsCmds._
import JE._
/*
 * Cleanup non-used imports.
 * */




class AdminPage
{
	// the SessionVar will contain a String with "Anonymous" as default value.
	object JaraDur extends SessionVar[String]("1")

	println("Adminpage is called")
	//val path = GlobalConstant.WEBAPROOT
//JsonCmd(2,null,false,Map(command -> 2, params -> false))

	var n_blaat = JaraDur.is 
	 // println(lines)
	 
		
	def settings =
	{
			
    "#jsonscript" #> Script(json.jsCmd) &
			".startSimu [onclick]" #> Text(json.call(ElemById("startSimu") ~> Value,ElemById("inputbox") ~> Value).toJsCmd)
 
	
	
	}
	
	object json extends JsonHandler {
    def apply(in: Any): JsCmd =
    {
		in match {
        case JsonCmd("submit", _, p: String, _) => 
        {
		println(p)
		SetHtml("jararesult",Text("Running Simulation.."));
		JaraDur.set(p)
    
		println("startsimu called");
		Player.bulkDelete_!!(By(Player.superUser,false))
		Constitution.removeAll

		FileUtils.deleteDirectory(new File(GlobalConstant.CONSTITUTIONOBJECTDIR))
		FileUtils.deleteDirectory(new File(GlobalConstant.CONSTITUTIONHTMLDIR))
		FileUtils.deleteDirectory(new File(GlobalConstant.CORECONTENTOBJECTDIR))

                InitialiseJgit()

                Constitution.createConstiAlphaIfDoesntExist

 		log("Calling Jara")	          
                PlayingSimulator.start(JaraDur.is.toLong * 1000 * 60 * 60)
		
                SetHtml("jararesult", Text("Simulation Ended!"))
	}
	}
		
	}
	
  }
  
  
		
		

		
	}
}


