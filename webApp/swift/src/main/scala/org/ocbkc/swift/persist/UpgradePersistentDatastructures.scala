package org.ocbkc.persist

import org.ocbkc.swift.global.Logging._
import java.io._

/* Naming conventions:
- abbreviate persistent with pers for private identifiers. For public, use the full name.
*/

object Test
{  def main(args:Array[String])
   {  val testUPD = new PersDataUpgrader(persDataUpgraders)

      testUPD.apply("1.2.0", "2.4.1")
   }

   val persDataUpgraders =
      List(
         new PersDataUpgraderSingleVersion("3.2.1.7")
         {  override def apply
            {  super.apply
            }
         },
        new PersDataUpgraderSingleVersion("2.3.0")
         {  override def apply
            {  super.apply
            }
         },
         new PersDataUpgraderSingleVersion("1.2.2")
         {  override def apply
            {  super.apply
            }
         }        
      )
}

/** Always call initialise first thing when booting your application.
  * @todo &y2013.05.20.12:27:40& replace the constructor parameter with the yet to define upgraders.
  * @todo &y2013.05.20.12:54:50& refactor: move swift specific parts to other packages, for easier reuse of this package in other web applications.
  * @todo &y2013.05.20.13:50:00& integrate all non-swift specific parts into PersDataUpgrader.
  */
object PersDataUpgrader4SWiFT extends PersDataUpgrader(Nil) //Test.persDataUpgraders)
{  var persistentDataMainVersion:Option[String] = None
   var persistentDataMainVersion_PathName:Option[String] = None
   var currentMainVersion:Option[String] = None
   var initialised:Boolean = false

   def apply =
   {  if(!initialised)
         logAndThrow("PersDataUpgrader4SWiFT.apply called, but first you should call initialise!")
      else
      {  if(persistentDataMainVersion.get == currentMainVersion.get )
         {  log("   versions are equal, no upgrade required")
         }else
         {  super.apply(persistentDataMainVersion.get, currentMainVersion.get)
            persistentDataMainVersion = Some(currentMainVersion.get)
            writePersistentDataMainVersionToFile
         }
      }
   }

   def initialise(persistentDataMainVersion_PathName:String, currentMainVersion:String)
   {  log("PersDataUpgrader4SWiFT.initialise called")
      this.currentMainVersion = Some(currentMainVersion)
      this.persistentDataMainVersion_PathName = Some(persistentDataMainVersion_PathName)
      val file = new File(persistentDataMainVersion_PathName)
      if( !file.exists )
      {  log("   " + persistentDataMainVersion_PathName + " doesn't exist yet. This means this application is booted for the first time (or all persistent data has been erased).")
         persistentDataMainVersion = Some(currentMainVersion)
         writePersistentDataMainVersionToFile(file)
      }else
      {  log("   " + persistentDataMainVersion_PathName + " exists.")
         val in:BufferedReader   = new BufferedReader(new FileReader(file))
         persistentDataMainVersion = Some(in.readLine)
         log("   read from file: persistentDataMainVersion = " + persistentDataMainVersion.get)
      }
      initialised = true
   }

   private def writePersistentDataMainVersionToFile
   {  val file = new File(persistentDataMainVersion_PathName.get)
      writePersistentDataMainVersionToFile(file)
   }

   private def writePersistentDataMainVersionToFile(file:File)
   {  log("writePersistentDataMainVersionToFile called")
      log("   writing to file persistentDataMainVersion = " + persistentDataMainVersion.get)
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(file)))
      out.println(persistentDataMainVersion.get)
      out.flush
      out.close
   }
}

/** Does upgrade from a given version to another given version.
  */
class PersDataUpgrader(persDataUpgraders:List[PersDataUpgraderSingleVersion])
{  def apply(fromVersion:String, toVersion:String) =
   {  log("PersDataUpgrader.apply called")
      log("   upgrading persistent data from " + fromVersion + " to version " + toVersion + ".")
      if( fromVersion < toVersion )
      {  val persDataUpgraders_sorted = persDataUpgraders.sortWith{ (pdu1, pdu2) => pdu1.version < pdu2.version }
         log("   first, datastructures were changed in the following versions: " + persDataUpgraders_sorted.map{ _.version }.mkString(", "))
         val persDataUpgraders_sortedFiltered = persDataUpgraders_sorted.filter{ pdu => ( pdu.version > fromVersion && pdu.version <= toVersion ) }
         log("   So, I'm going to apply the data-upgrades associated with the following versions: " + ( persDataUpgraders_sortedFiltered match { case List() => "hmm, none of 'em!"; case nonEmptyList => nonEmptyList.map(_.version).mkString(", ") }) )
         persDataUpgraders_sortedFiltered.foreach{ _.apply }
      } else if(fromVersion == toVersion)
      {  log("   versions are equal: no upgrade required.")
      } else
      {  logAndThrow("   toVersion < fromVersion, not allowed... Are you trying to DOWNGRADE? I can't believe my webcams (I have no eyes, you know)... If you *really* try to run a previous version - first delete all persistent data, and then reboot the SWiFT application. The latter may occur regularly if you are a developer, and working on different branches.")
      }
   }
}

/** Allows defining an upgrade for a specific version. The upgrade is intended to convert the persistent datastructures to the persistent data structures used in the given version.
   @param version the main version of the application (which introduced a change in the persistent datastructure.) It defines an upgrade from the last known version of the datastructure to the given version.
  */
abstract class PersDataUpgraderSingleVersion(val version:String)
{  /** Call this in the beginning of your overriding method
    */
   def apply
   {  log("PersDataUpgraderSingleVersion.apply called")
      log("Doing upgrade for version " + version)
   }
}
