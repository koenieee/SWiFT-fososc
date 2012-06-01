package org.ocbkc.swift.global
{  
//import org.ocbkc.swift.model._
//import System._
//import org.ocbkc.swift.cores.{TraitGameCore, NotUna}
//import org.ocbkc.swift.cores.gameCoreHelperTypes._
/* Conventions:
- Names of classes correspond with design $JN/...
- CTL = Computationally Transparent Language
- NL  = Natural Language
*/

object GlobalConstant
{  val TEST = true
   val NEWLINE = System.getProperty("line.separator")
   val WEBAPROOT = "/home/waimondrio/jowneeGitProjects/SWiFTfososc/source/webApp/swift" // change this when moving the project. 
   val CONSTITUTIONDIR = "src/main/webapp/constitutions/"
}

object TestSettings
{  val AUTOLOGIN = true // <&y2012.01.13.10:20:07& not used yet, make it so>
   val AUTOTRANSLATION = false
}

}
