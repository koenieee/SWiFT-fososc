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
import System.err.println
import org.ocbkc.swift.model._
import org.ocbkc.swift.global._
import org.ocbkc.swift.global.LiftHelpers._
import org.ocbkc.swift.coord.ses._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.test._
import org.ocbkc.swift.model.Player
import java.io._
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
		
		//<KOEN>:: Possible security leak, any other tips? If you're ever going to use this public....
		
		scala.io.Source.fromFile(path + "/src/main/scala/org/ocbkc/swift/test/JaraForSWiFT.scala").getLines().foreach {   
			line =>
			
				if (line.startsWith("      val durationSimulation =") == true)
				{
					
					blaat = line.stripPrefix("      val durationSimulation =")
					println(blaat);
				}
			 }
		  bind("entry", xhtml,
			"JaraSetting" -> SHtml.text(blaat, n_blaat = _),
			"changeDur" -> SHtml.submit("Change Duration", changeDur))
	
	}
	def changeDur()
	{
		//<KOEN>:: Possible security leak, any other tips? If you're ever going to use this public....
		//TODO: input validation

		val lines = scala.io.Source.fromFile(path + "/src/main/scala/org/ocbkc/swift/test/JaraForSWiFT.scala").getLines()
		val lol = lines.map(in => if (in startsWith("      val durationSimulation = " + blaat)) "      val durationSimulation = " + n_blaat else in)
		//TODO: Write to same file, while lift is running. 
		printToFile(new File(path + "/src/main/scala/org/ocbkc/swift/test/JaraForSWiFT2.scala"))(p => {
		  lol.foreach(p.println)
		})
			println("File Saved");
	//	PlayingSimulator.start
	}
	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
	  val p = new java.io.PrintWriter(f)
	  try { op(p) } finally { p.close() }
	}
	

}
}


