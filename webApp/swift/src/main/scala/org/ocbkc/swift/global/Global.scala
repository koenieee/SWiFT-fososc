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
import org.ocbkc.swift.jgit.Translations._
import org.ocbkc.swift.model._

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
   var admin:Option[Player] = None
   def adminGitUserId = {  println("retrieving adminGitUserId...")
                           admin match
                           {  case Some(admin) =>
                              {  println("successful")
                                 Some(gitUserId(admin))
                              }
                              case _ =>
                              {  println("  there is no lift-admin account")
                                 None
                              }
                           } // MUSTDO: admin.flatMap{ gitUserId(_) } // perhaps also better put in org.ocbkc.swift.jgit.Translations._ ?
                        }

   val MINsESSIONSb4ACCESS2ALLcONSTIS = 4
   val GIThASHsIZE = 41 + 10 // + 10, I'm not certain it is 41. Better safe than sorry.

   // Scoring

   abstract class ScoringConstants // purely intended for commentary purposes.

   object AveragePercentageCorrect extends ScoringConstants
   {  val minimalNumberOfSessionsPerPlayer = 2
   }
   
   object AverageDurationTranslation extends ScoringConstants
   {  val minimalNumberOfSessionsPerPlayer = AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer
   }

   object AverageFluency extends ScoringConstants
   {  val minimalSampleSizePerPlayer   = AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer
      val fluencyConstantK             = 1000000
   }

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

/** @todo &y2013.01.20.18:12:52& move this one to a more general place
  */
object Types
{  type POSIXtime = Long
   type DurationInMillis = Long
   type TimeInMillis = Long
}

// <&y2012.10.29.17:00:46& improve this, some tests dependent on other ones, now manually selected - should be done automatically>
object TestSettings
{  val AUTOLOGIN                       = true
   val AUTOTRANSLATION                 = false // true
   val CREATETESTUSERBASE              = false // true
   /* <&y2012.09.29.19:44:55& TODO: if constitutions DO exist, don't create new constitutions. Or perhaps better: erase them but not before prompting the developer> */
   val CREATEDUMMYCONSTITUTIONS        = false // true // creates a number of constitutions with several updates and releases, but also some users.
   val SIMULATEPLAYINGWITHJARA         = false // Simulate playing with Jara during Boot. After boot normal playing (by real persons) can be continued from there.
   var SIMULATEPLAYINGWITHJARARUNNING = false // simulation process is currently running
   val SIMULATEPLAYINGWITHFIRSTSIMSYSTEM = false // true mutually exclusive with CREATEDUMMYCONSTITUTIONS
   var SIMULATECLOCK                   = true // false, always on when doing tests. <&y2012.12.12.23:32:04& automatically switch this on when needed>
   if( CREATEDUMMYCONSTITUTIONS && SIMULATECLOCK ) throw new RuntimeException("CREATEDUMMYCONSTITUTIONS && SIMULATECLOCK are mutually exclusive")
   // vim swap false true: s/false \/\/ true/true \/\/ false/gc
   // vim swap true false: s/true \/\/ false/false \/\/ true/gc

}

}
