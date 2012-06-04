package org.ocbkc.swift.OCBKC
{  
import _root_.scala.xml._
import org.ocbkc.swift.model._
import System._
import org.ocbkc.swift.cores.{TraitGameCore, NotUna}
import org.ocbkc.swift.cores.gameCoreHelperTypes._
import org.ocbkc.swift.global._
import net.liftweb.json._
import java.io._
import net.liftweb.common.{Box,Empty,Failure,Full}
//import scala.util.parsing.combinator.Parsers._
import org.ocbkc.swift.parser._

/* Conventions:
Abbreviation for constitution: consti (const is to much similar to constant).


*/

object ConstitutionTypes
{  type ConstiId = Int
}

import ConstitutionTypes._

/* <& &y2012.06.03.13:36:12& how to turn this into a complete case class, so only using vals, also for things that (may or will) change such as the average score, description etc?>
   <? &y2012.06.03.14:21:23& previous>
   
*/
case class Constitution(val id:ConstiId, // unique identifier for this constitution
                        val creationTime:Long,
                        val creatorUserID:Int,
                        var averageScore:Int, // redundant, for efficiency
                        var shortDescription:String,
                        val predecessorId:Option[ConstiId]
                       )
{  val htmlFileName = "constitution" + id + ".html"

   // create html file which holds the constitution
   {  val outFile = new File(GlobalConstant.CONSTITUTIONHTMLDIR + htmlFileName)

      err.println("  creating file: " + outFile.getAbsolutePath)
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.print(Constitution.templateNewConstitution(id))
      out.flush()
      out.close()
   }

   /* <&y2012.06.02.20:19:54& optimisations needed, and if so, how would be best? Now it loads the html at each call> */
   def loadHtml =
   {  XML.loadFile(GlobalConstant.CONSTITUTIONHTMLDIR + "constitution" + id + ".html")
   }
   /* <? &y2012.05.28.16:36:27& what would be a good way to store constitutions? In a database? As files? In memory in a string field (of course not, but just to make my list complete)?> 
   */

   /* Only serializes the object - not the html text which is already stored... */
   def serialize =
   {  implicit val formats = Serialization.formats(NoTypeHints)
      var constSer:String = Serialization.write(this)
      err.println("  Constitution-object serialised to: " + constSer)
      // write constitution-object to file with unique name

      var outFile = new File( GlobalConstant.CONSTITUTIONOBJECTDIR + "/Constitution" + id)
      err.println("   creating file: " + outFile.getAbsolutePath)
      // outFile.getParentFile().mkdirs() these should already exist
      // outFile.createNewFile() // <&y2011.12.23.13:39:00& is this required, or is the file automatically created when trying to write to it?>
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.println(constSer)
      out.close()
   }
}

object Constitution
{  var constis:List[Constitution] = Nil
   var highestId:Int = 0 // <&y2012.03.18.17:31:11& deserialise in future when starting session>
   /*
   def create():Constitution =
   {  // if no user is given assume 'master' user, TODO <&y2012.05.24.20:44:12& determine fixed number for this, for now I used ID 0>
      create(0)
   }
   */

   def deserialize =
   {  println("Constitution.deserialize called")
      val constObjDir = new File(GlobalConstant.CONSTITUTIONOBJECTDIR)
      val constitutionFiles = constObjDir.listFiles()
      if( constitutionFiles != null && constitutionFiles.length != 0 )
      {  implicit val formats = Serialization.formats(NoTypeHints) // <? &y2012.01.10.20:11:00& is this a 'closure' in action? It is namely used in the following function>

         def readConst(file:File):Unit =
         {  println("   converting to Constitution-object: " + file.getPath() )
            val in:BufferedReader   = new BufferedReader(new FileReader(file))
            var inStr:String        = in.readLine()
            val const:Constitution  = Serialization.read[Constitution](inStr)
            constis ::= const
         }
         
         constitutionFiles map readConst
      }
      else
      {  println("   No serialised Constitution objects found in permanent storage!")
      }
   }

   def serialize = 
   {  println("Constitution.serialize called")
      constis.map(_.serialize)
   }

   def templateNewConstitution(constitutionId:Int):String =
"""<lift:children><h2>Short description: ...</h2>

<h2>Article 1</h2><p>...</p></lift:children>
"""
   def create(creatorUserID:Int):Constitution = 
   {  highestId += 1
      val now = currentTimeMillis().toLong
      val c = Constitution( highestId, now, creatorUserID, 0, "No description provided.", None )
      constis = c::constis
      c
   }

   def getById(id:Int):Option[Constitution] =
   {  constis.find( _.id == id )
   }

   /* <&y2012.05.24.11:03:48& for next subincrement>
   def remove(c:Constitution) = ...
   */
}

}
