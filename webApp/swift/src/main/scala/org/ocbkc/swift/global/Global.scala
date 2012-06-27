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
   val WEBAPROOT = "/home/waimondrio/jowneeGitProjects/SWiFTfososc/source/webApp/swift" // CHANGE TO YOUR MACHINE
   val ABSOLUTEPATHS = true // set true when you want all paths in this dir to be absolute, sometimes handy when you want to execute parts of the application from another directory. Default should be false.
   
   private val PREFIX = if( ABSOLUTEPATHS ) WEBAPROOT + "/" else ""
   val CONSTITUTIONHTMLDIR = PREFIX + "src/main/webapp/constitutions/"
   val PERSISTDIR = PREFIX + "persist" // directory to hold all data required for making app persistent (= survive shutdown and starts)
   val CONSTITUTIONOBJECTDIR = PERSISTDIR + "/constobjs"
   val SWIFTURL = "http://127.0.0.1:8080"
}

object TestSettings
{  val AUTOLOGIN = true
   val AUTOTRANSLATION = false
}

}
