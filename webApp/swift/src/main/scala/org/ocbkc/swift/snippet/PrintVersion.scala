package org.ocbkc.swift.snippet

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.ocbkc.swift.general.GUIdisplayHelpers._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.OCBKC.scoring._
import org.ocbkc.swift.global._


class Version_swift
{
  /*<-- Koen: I think :), one day you are using a file to set the version number, so I keep this piece of code:
  val source = scala.io.Source.fromFile(GlobalConstant.PERSISTENT_DATA_MAIN_VERSION_PATHNAME)
	  val lines = source.mkString
source.close()*/
val lines = GlobalConstant.MAIN_VERSION
  def printing(xhtml: NodeSeq): NodeSeq = {
  {
		<h4 class="alt">
				  <a href={ "http://htmlpreview.github.io/?https://github.com/swiftgame/SWiFT-fososc/blob/develop/webApp/swift/version_history.html#version" + lines} ><i>SWiFT fososc</i></a> 
					version {lines}
		</h4>
  }
  }
  
}

