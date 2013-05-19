package org.ocbkc.persist

import org.ocbkc.swift.global.Logging._
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

object PersDataUpgrader4SWiFT extends PersDataUpgrader(Test.persDataUpgraders)
{
}

/** Does upgrade from a given version to another given version.
  */
class PersDataUpgrader(persDataUpgraders:List[PersDataUpgraderSingleVersion])
{  def apply(fromVersion:String, toVersion:String) =
   {  log("PersDataUpgrader.apply called")
      log("   upgrading persistent data from " + fromVersion + " to version " + toVersion + ".")
      val persDataUpgraders_sorted = persDataUpgraders.sortWith{ (pdu1, pdu2) => pdu1.version < pdu2.version }
      log("   first, datastructures were changed in the following versions: " + persDataUpgraders_sorted.map{ _.version }.mkString(", "))
      val persDataUpgraders_sortedFiltered = persDataUpgraders_sorted.filter{ pdu => ( pdu.version > fromVersion && pdu.version <= toVersion ) }
      log("   So, I'm going to apply the data-upgrades associated with the following versions: " + ( persDataUpgraders_sortedFiltered match { case List() => "hmm, none of 'em!"; case nonEmptyList => nonEmptyList.map(_.version).mkString(", ") }) )
      persDataUpgraders_sortedFiltered.foreach{ _.apply }
   }
}

/** Allows defining an upgrade for a specific version.
  */
abstract class PersDataUpgraderSingleVersion(val version:String)
{  /** Call this in the beginning of your overriding method
    */
   def apply
   {  log("PersDataUpgraderSingleVersion.apply called")
      log("Doing upgrade for version " + version)
   }
}
