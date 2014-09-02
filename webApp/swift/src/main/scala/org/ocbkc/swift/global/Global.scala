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
import org.ocbkc.swift.global.Logging._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor, By}
import org.ocbkc.swift.OCBKC.Constitution
import org.apache.commons.io.FileUtils
import org.ocbkc.swift.jgit.InitialiseJgit
import net.liftweb.common.{Box,Empty,Failure,Full}
import org.ocbkc.swift.model._
import org.ocbkc.generic._
import org.ocbkc.swift.test._

object GlobalConstant
{  val TEST = true
   val NEWLINE = System.getProperty("line.separator")
   val WEBAPP_BASE_DIR =
   {  val  SWiFTpomDirProp = System.getProperty("SWiFTpom.dir")
      if(SWiFTpomDirProp != null)
         SWiFTpomDirProp
      else
         System.getProperty("user.dir")
   }

   log("   WEBAPP_BASE_DIR = " + WEBAPP_BASE_DIR)

   val OS = System.getProperty("os.name").toLowerCase

   
   val PERSISTENT_DATA_MAIN_VERSION_PATHNAME = WEBAPP_BASE_DIR + "/persistentDatastructureMainVersion.txt"
    val MAIN_VERSION = scala.io.Source.fromFile(WEBAPP_BASE_DIR + "/mainVersion.txt").mkString // Read versioningProtocol.txt in the base directory of the web application to find out how to change version numbers. Read version_history.html to find out what has changed in subsequent versions...

   /* 
    * Dependency usage: first run mvn dependency:unpack
    * global.scala will check if you're running windows, mac or linux
    * BINARIES is only used in GameCore.Scala
    * Eprover_Path is only used in tpwrap.scala
    * Paradox_Path is only used in tpwrap.scala
    */
    
   //only used for AdGen & textgenerator (did a recursive grep search!) //ONLY FOR LINUX
   val BINARIES = WEBAPP_BASE_DIR + "/binaries" // CHANGE TO YOUR MACHINE
   val SWIFTBINARIES = BINARIES + "/swift" // CHANGE TO YOUR MACHINE
  
   //EPROVER: determine OS, no need to check if running 32 or 64 bits ==> created by Koen, used in tpwrap.scala
   val EPROVER_PATH = 	if(OS.startsWith("linux")) {  BINARIES + "/eprover/Linux";	}	else if(OS.startsWith("mac os x")) { 	BINARIES + "/eprover/Mac OSX";	}	else if(OS.startsWith("windows")) {  BINARIES + "/eprover/Windows";} else { BINARIES + "/eprover/Linux"; }
	
   //PARADOX: ONLY LINUX!!! ==> created by Koen, used in tpwrap.scala
   val PARADOX_PATH = BINARIES + "/paradox"
  
   val CONSTITUTIONHTMLDIR = "src/main/webapp/constitutions/"
   val PERSISTDIR = "persist" // directory to hold all data required for making app persistent (= survive shutdown and starts)
   val CONSTITUTIONOBJECTDIR = PERSISTDIR + "/constobjs"
   val SESSIONINFOOBJECTDIR = PERSISTDIR + "/sessionInfoobjs"
   val SWIFTURL = "http://127.0.0.1:8080"
   val TESTADMINEMAIL = "admin@test.org"
   val TESTADMINFIRSTNAME = "AdminTest"
   val TESTADMINPW = "asdfasdf"
   
   var adminOpt:Option[Player] = None
   def adminGitUserId = {  log("retrieving adminGitUserId...")
                           adminOpt.collect{ case admin => Some(gitUserId(admin)) }
                        }.get // convention is that this method may only be called when an admin account exists, so .get is possible

   val MINsESSIONSb4ACCESS2ALLcONSTIS = 2
   log("[POTENTIAL_BUG]  <&y2014.05.12.14:56:26& shouldn't MINsESSIONSb4ACCESS2ALLcONSTIS be equal to minimalNumberOfSessionsPerPlayer>")

   val GIThASHsIZE = 41 + 10 // + 10, I'm not certain it is 41. Better safe than sorry.
   val INITIALISATIOnDATaDIR = WEBAPP_BASE_DIR + "/initialisationData" 
   val CONSTI_ALPHA_INIT = INITIALISATIOnDATaDIR + "/efe/constitutionAlpha_core"

   log("[MUSTDO] [POTENTIALBUG] Determine the right MAX_TRANSLATION_LENGTH, or find another way to make it persistent without a max size, just pointer to a file for example. Current solution is or very inefficient (takes a lot of space), or is dangerous (if the size turns out not big enough for some translations). Same holds for MAX_LENGTH_PARSE_ERROR.")
   val MAX_TRANSLATION_LENGTH = 1024 
   val MAX_LENGTH_PARSE_ERROR = 1024

   // Scoring

   abstract class ScoringConstants // abstract class purely intended for commentary purposes.

   object AveragePercentageCorrect extends ScoringConstants
   {  val minimalNumberOfSessionsPerPlayer = 2
   }
   
   object AverageDurationTranslation extends ScoringConstants
   {  val minimalNumberOfSessionsPerPlayer = AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer
   }

   /** @param minimalSampleSizePerConsti given a consti C, the minimal number of fluency players who used consti C and have a valid fluency score that are required to assign a fluency score to the consti.
     */
   object AverageFluency extends ScoringConstants
   {  val minimalSampleSizePerPlayer   = AveragePercentageCorrect.minimalNumberOfSessionsPerPlayer
      val minimalSampleSizePerConsti   = 2
      val fluencyConstantK             = 1000000
   }

   /*
   val jgitRepo = new Repository(new File(new File(CONSTITUTIONHTMLDIR)))

   jgitRepo.create()
   jgitRepo.getConfig().setBoolean("core", null, "bare", false)
   */
   
   var jgitBuilder:Option[FileRepositoryBuilder] = None
   var jgitRepo:Option[Repository] = None   
   var jgit:Option[Git] = None

   initialiseSWiFTdirs

   def initialiseSWiFTdirs
   {   // create paths
      createDirIfNotExists(CONSTITUTIONOBJECTDIR)
      createDirIfNotExists(CONSTITUTIONHTMLDIR)
      createDirIfNotExists(SESSIONINFOOBJECTDIR)
      createDirIfNotExists(INITIALISATIOnDATaDIR)
   }

   /**  If you want to start a running SWiFT instance as if it started with an completely clean database/persistency info. For example used by simulations.
     *  @todo Move to other object.
     * 
     */
   def clearAndReinitialiseSWiFTdatabase =
   {  Player.bulkDelete_!!(By(Player.superUser,false))
      Constitution.removeAll

      FileUtils.deleteDirectory(new File(GlobalConstant.CONSTITUTIONOBJECTDIR))
      FileUtils.deleteDirectory(new File(GlobalConstant.CONSTITUTIONHTMLDIR))
      log("[BUG] The following 'FileUtils.deleteDirectory(new File(GlobalConstant.SESSIONINFOOBJECTDIR))' does not seem to have effect??")
      FileUtils.deleteDirectory(new File(GlobalConstant.SESSIONINFOOBJECTDIR))

      initialiseSWiFTdirs

      InitialiseJgit()
   }

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
         println("   so creating...  succesful: " + { if(mkdirSuccess) "of course, as always, success is my middle name..." else "No... this is ruining my good humour." } )
         mkdirSuccess
      } else
      {  println("   path already exists, dude, you woke me for nothin'... That means free time for me, humble method, I'm gonna continue my dreamy nap...")
         true
      }
   }
}

object ScalaHelpers
{  val doNothing = Unit
}  

/** @todo &y2013.01.20.18:12:52& move this one to a more general place
  */
object Types
{  type POSIXtime          = Long
   type DurationInMillis   = Long
   type TimeInMillis       = Long
}

// <&y2012.10.29.17:00:46& improve this, some tests dependent on other ones, now manually selected - should be done automatically>
object TestSettings
{  object AUTOLOGIN
   {  val ON      = false
      val USER_ID = "2" // 1 is Admin. If you choose another number, make certain that that user exist. For example, if you have deleted the users data, then set CREATETESTUSERBASE to true.
   }

   val AUTOTRANSLATION                 = false // true
   val CREATETESTUSERBASE              = false // false
   /* <&y2012.09.29.19:44:55& TODO: if constitutions DO exist, don't create new constitutions. Or perhaps better: erase them but not before prompting the developer> */
   val CREATEDUMMYCONSTITUTIONS        = false // true // creates a number of constitutions with several updates and releases, but also some users.
   val STARTJARASIMULATIONDURINGBOOT   = false // Simulate playing with Jara during Boot. After boot normal playing (by real persons) can be continued from there.
   val SIMULATEPLAYINGWITHFIRSTSIMSYSTEM = false // true mutually exclusive with CREATEDUMMYCONSTITUTIONS
  
 
   // { never change the following manually, they are used by other parts of the program
   var SIMULATEPLAYINGWITHJARARUNNING  = false // simulation process is currently running
   var SIMULATECLOCK                   = false // false, always on when doing tests. <&y2012.12.12.23:32:04& automatically switch this on when needed>
   var SIMULATEPLAYINGWITHJARA         = false
   // }
   if( CREATEDUMMYCONSTITUTIONS && SIMULATECLOCK ) throw new RuntimeException("CREATEDUMMYCONSTITUTIONS && SIMULATECLOCK are mutually exclusive")
   // vim swap false true: s/false \/\/ true/true \/\/ false/gc
   // vim swap true false: s/true \/\/ false/false \/\/ true/gc
   

}

object Logging
{  def logAndThrow(msg:String) =
   {  println(msg)
      throw new RuntimeException(msg)
   }

   def log(msg:String) =
   {  val user_info = Player.currentUser match // <&y2012.08.04.20:16:59& refactor rest of code to use this currentPlayer, instead of doing this again and again....>
      {  case Full(player) => "[" + player.id + ", " + player.swiftDisplayName + "]"
         case _            => ""
      }
      val datetime = "[" + DateTime.timeInMillis2dateString(SystemWithTesting.currentTimeMillis) + "]"
      println(datetime + " " + user_info + "   " + msg)
   }

   /** Log and pass
     */
   def logp[T](msg: T => String, obj:T):T =
   {  log(msg(obj))
      obj
   }
}

object LiftHelpers
{  val emptyNode = <div></div> // <!-- empty node --> <&y2012.06.02.18:53:13& nicer way of defining empty substitution?>

}

object DisplayHelpers
{  def defaultRounding(d:Double):Double =
   {  "%.2f".format(d).toDouble
   }
}


}
