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
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._
import org.eclipse.jgit.storage.file._
import java.io._

object GlobalConstant
{  val TEST = true
   val NEWLINE = System.getProperty("line.separator")
   val WEBAPROOT = "/home/waimondrio/jowneeGitProjects/SWiFTfososc/source/webApp/swift" // CHANGE TO YOUR MACHINE
   val ABSOLUTEPATHS = true // set true when you want all paths in this dir to be absolute, sometimes handy when you want to execute parts of the application from another directory. Default should be false.
   
   private val PREFIX = if( ABSOLUTEPATHS ) WEBAPROOT + "/" else ""
   val CONSTITUTIONHTMLDIR = PREFIX + "src/main/webapp/constitutions/"
   val PERSISTDIR = PREFIX + "persist" // directory to hold all data required for making app persistent (= survive shutdown and starts)
   val CONSTITUTIONOBJECTDIR = PERSISTDIR + "/constobjs"
   val CORECONTENTOBJECTDIR = PERSISTDIR + "/corecontentobjs"
   val SWIFTURL = "http://127.0.0.1:8080"
   val ADMINFIRSTNAME = "Admin"
   /*
   val jgitRepo = new Repository(new File(new File(CONSTITUTIONHTMLDIR)))

   jgitRepo.create()
   jgitRepo.getConfig().setBoolean("core", null, "bare", false)
   */
   
   val jgitBuilder:FileRepositoryBuilder = new FileRepositoryBuilder()
   val jgitRepo:Repository = jgitBuilder.setGitDir(new File(CONSTITUTIONHTMLDIR + "/.git"))
      //.readEnvironment() // scan environment GIT_* variables
      //.findGitDir() // scan up the file system tree <&y2012.06.30.19:51:12& perhaps leave this one out, it SHOULD be in this dir, not in a superdir>
      .build()
   println("   jgitRepo directory: " + jgitRepo.getDirectory() )
   println("   jgitRepo is bare (false is correct): " + jgitRepo.isBare())
   

   val jgit = new Git(jgitRepo) // <? &y2012.06.30.18:53:23& or isn't this thread safe? I now share one jgit object accross user-sessions (I think... because I instantiate this thing in Boot.scala). Perhaps I should instantiate one per user-session...>
   println(jgit.status().call().getUntracked)

   // create paths
   createDirIfNotExists(CONSTITUTIONOBJECTDIR)
   createDirIfNotExists(CONSTITUTIONHTMLDIR)
   createDirIfNotExists(CORECONTENTOBJECTDIR)
   
/** TODO: <&y2012.10.01.15:14:30& refactor: put in general lib>
  * @returns: false dir doesn't exist and could not be created; true: dir exists (if it didn't before, it was created succesfully)
  */
   def createDirIfNotExists(pathname:String):Boolean =
   {  println("createDirIfNotExists called")
      println("   arg pathname = " + pathname)
      val outFile = new File(pathname + "/")
      if( !outFile.exists )
      {  println("   path " + pathname + " (this is a path to Pure Wisdom) doesn't exist yet")
         val mkdirSuccess = outFile.mkdirs
         println("   so creating...  succesful: " + { if(mkdirSuccess) "of course, as always, success is my middle name..." else "Fuck it, no... This is ruining my good humour." } )
         mkdirSuccess
      } else
      {  println("   path already exists, dude, you woke me for nothin'... That means free time for me, humble method, I'm gonna continue my dreamy nap...")
         true
      }
   }
}

object TestSettings
{  val AUTOLOGIN                       = false
   val AUTOTRANSLATION                 = false
   val CREATETESTUSERBASE              = false
   /* <&y2012.09.29.19:44:55& TODO: if constitutions DO exist, don't create new constitutions. Or perhaps better: erase them but not before prompting the developer> */
   val CREATEDUMMYCONSTITUTIONS        = false // creates a number of constitutions with several updates and releases, but also some users.
   val SIMULATEPLAYING                 = false
   val SIMULATECLOCK                   = true
   if( CREATETESTUSERBASE && CREATEDUMMYCONSTITUTIONS) throw new RuntimeException("Tests CREATETESTUSERBASE and CREATEDUMMYCONSTITUTIONS are mutually exclusive")
}

}
