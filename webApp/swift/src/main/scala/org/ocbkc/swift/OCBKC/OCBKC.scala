package org.ocbkc.swift.OCBKC
{  
import _root_.scala.xml._
//import org.ocbkc.swift.model._
import System._
import org.ocbkc.swift.cores.{TraitGameCore, NotUna}
import org.ocbkc.swift.cores.gameCoreHelperTypes._
import org.ocbkc.swift.global._
import net.liftweb.json._
import java.io._
import org.apache.commons.io.filefilter._
import net.liftweb.common.{Box,Empty,Failure,Full}
//import scala.util.parsing.combinator.Parsers._
import org.ocbkc.swift.parser._
import GlobalConstant.jgit
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.gitective.core.BlobUtils

/* Conventions:
Abbreviation for constitution: consti (const is to much similar to constant).


*/
object TestSerialization
{  def main(args: Array[String]) =
   {  if( args.length != 0 ) println("Usage: command without arguments")
      val const1 = Constitution(1,15,2,0,"Lets go organic!",None, List())
      const1.serialize
   }
}


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
                        val predecessorId:Option[ConstiId],
                        var followers:List[Int] // followers are users following this constitution. This includes optional features such as receiving emails when an update is made to that constitution etc.
                       )
{  val htmlFileName = "constitution" + id + ".html"

   // create html file which holds the constitution
   save(Constitution.templateNewConstitution(id))

   /* <&y2012.06.02.20:19:54& optimisations needed, and if so, how would be best? Now it loads the html at each call> */
   def plainContent:String =
   {  val content:String = scala.io.Source.fromFile(GlobalConstant.CONSTITUTIONHTMLDIR + htmlFileName).mkString
      content
   }
/* &y2012.06.23.14:39:56& just use followers.contains(...) directly.
   def followedByUser(userId:Int):boolean = 
   {  followers.contains(userId) 
   }
*/
   def gitUserId(liftUserId:String) = // <&y2012.07.23.17:16:15& refactor: move to more generic class in webapp>
   {  new PersonIdent(liftUserId, "swiftgame")   }
   
   // <&y2012.06.12.21:35:34& optimise: only reload when something changed>
   def contentInScalaXML:Elem =
   {  plaintTextXMLfragment2ScalaXMLinLiftChildren(plainContent)
   }

   def plaintTextXMLfragment2ScalaXMLinLiftChildren(plain:String):Elem =
   {  val contentWrapped = "<lift:children>" + plain + "</lift:children>"
      println("   contentWrapped:String = " + contentWrapped)
      val xml = XML.loadString( contentWrapped )
      xml
   }

   /* <? &y2012.05.28.16:36:27& what would be a good way to store constitutions? In a database? As files? In memory in a string field (of course not, but just to make my list complete)?> 
   */

   def save(constitutionText:String) =
   {  // <&y2012.06.11.19:53:08& first add lift:children tag to make it a well-formed xml file.>
      val outFile = new File(GlobalConstant.CONSTITUTIONHTMLDIR + htmlFileName)

      err.println("  saving file: " + outFile.getAbsolutePath)
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.print(constitutionText)
      out.flush()
      out.close()
   }
   
   // Adds and commits constitutionText using jgit
   def publish(constitutionText:String, commitMsg:String, userId:String) =
   {  println("Constitution.publish called")
      save(constitutionText)

      val fullPath2Const = htmlFileName

      // now add and commit to git repo
      val username = gitUserId(userId) // <&y2012.06.30.19:16:16& later refactor with general methods for translating jgit name to lift user name back and forth>
      println("   jgit author id = " + username.toString)
      println("   now adding and committing: " + fullPath2Const )
      //jgit.status
      val addCommand = jgit.add()
      println("   isUpdate() (should be false) " + addCommand.isUpdate())
      addCommand.addFilepattern( fullPath2Const ).call()
      val status = jgit.status().call()
      println("   added files " + status.getAdded() )
      println("   modified files " + status.getModified() )
      println("   changed files " + status.getChanged() )
      println("   untracked files " + status.getUntracked() )
      jgit.commit().setAuthor(username).setCommitter(username).setMessage(commitMsg).call()
   }

   def restore(commitId:RevCommit, liftUserId:String) // <&y2012.07.23.17:17:39& better do resolving of commit-hash to commit-object here>
   {  val gitUi = gitUserId(liftUserId)
      val dateFormat =  new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
 
      val commitMsg = "Restored version of " + dateFormat.format(commitId.getCommitTime().toLong*1000)
      jgit.checkout().addPath(htmlFileName).setStartPoint(commitId).call()
      jgit.commit().setAuthor(gitUi).setCommitter(gitUi).setMessage(commitMsg).call()
   }

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

   
   case class HisCon(val content:Elem, val creationDatetimeMillis:Long)

   def historicContentInScalaXML(commitId:String):Option[HisCon] =
   {  import GlobalConstant.jgitRepo
      val rw = new RevWalk(jgitRepo)
      val revcom:RevCommit = rw.parseCommit(ObjectId.fromString(commitId))
      val hisCon = BlobUtils.getContent(jgitRepo, revcom, htmlFileName)
      val hisConXML = plaintTextXMLfragment2ScalaXMLinLiftChildren(hisCon)
      Some(HisCon(hisConXML, revcom.getCommitTime().toLong * 1000 ))
   }
   
   def getHistory:List[RevCommit] =
   {  import scala.collection.JavaConverters._ 
      jgit.log().addPath( htmlFileName ).call().asScala.toList
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
   /* <&y2012.06.04.17:03:35& problem here: exception occurs. I think it has something to do with the fact that the serialized Constitution object doesn't contain the last value (predecessorId), don't know why...> */

   {  println("Constitution.deserialize called")
      val constObjDir = new File(GlobalConstant.CONSTITUTIONOBJECTDIR)
      val fileFilter:FileFilter = new WildcardFileFilter("Constitution*")
      val constitutionFiles = constObjDir.listFiles(fileFilter)
      if( constitutionFiles != null && constitutionFiles.length != 0 )
      {  implicit val formats = Serialization.formats(NoTypeHints) // <? &y2012.01.10.20:11:00& is this a 'closure' in action? It is namely used in the following function>

         def readConst(file:File):Unit =
         {  println("   converting to Constitution-object: " + file.getPath() )
            val in:BufferedReader   = new BufferedReader(new FileReader(file))
            var inStr:String        = in.readLine()
            println("   serialized form: " + inStr)
            val const:Constitution  = Serialization.read[Constitution](inStr)
            constis ::= const
         }
         
         constitutionFiles map readConst
         constis = constis.sortWith((c1,c2) => c1.id < c2.id )
         val highestIdFile       = new File( GlobalConstant.CONSTITUTIONOBJECTDIR + "/highestId")
         val in:BufferedReader   = new BufferedReader(new FileReader(highestIdFile))
         var inStr:String        = in.readLine()
         highestId               = inStr.toInt
         println("   read from file: highestId = " + highestId)
      }
      else
      {  println("   No serialised Constitution objects found in permanent storage!")
      }
   }

   def serialize = 
   {  println("Constitution.serialize called")
      constis.map(_.serialize)

      var outFile = new File( GlobalConstant.CONSTITUTIONOBJECTDIR + "/highestId")
      err.println("   creating file: " + outFile.getAbsolutePath)
      // outFile.getParentFile().mkdirs() these should already exist
      // outFile.createNewFile() // <&y2011.12.23.13:39:00& is this required, or is the file automatically created when trying to write to it?>
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.println(highestId.toString)
      out.close()
   }

   def templateNewConstitution(constitutionId:Int):String =
"""<h2>Article 1</h2>

<p>...</p>
"""
   def create(creatorUserID:Int):Constitution = 
   {  highestId += 1
      val now = currentTimeMillis().toLong
      val c = Constitution( highestId, now, creatorUserID, 0, "No description provided.", None, List(creatorUserID) )
      constis = c::constis
      c
   }

   def getById(id:Int):Option[Constitution] =
   {  constis.find( _.id == id )
   }

   def remove(c:Constitution) = 
   {  constis = constis.filterNot( _ == c )
   }
}

}
