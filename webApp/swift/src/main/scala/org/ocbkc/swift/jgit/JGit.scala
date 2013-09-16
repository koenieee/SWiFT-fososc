
/** Contains connections beween jgit and SWiFT
 *
 */
package org.ocbkc.swift.jgit
{  
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._
import org.eclipse.jgit.storage.file._
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk, RevTag}
import org.gitective.core.BlobUtils
import org.ocbkc.swift.model.Player
import org.ocbkc.swift.global._
import org.ocbkc.swift.global.Logging._
import java.util.Date
import scala.collection.JavaConversions._
import java.io._

object InitialiseJgit
{  def apply() =
   {  import GlobalConstant.{jgitBuilder, jgitRepo, jgit}

      // Initialise git repository for constitutions if there isn't one created yet.
      // Check whether there is already git tracking
      val gitfile = new File(GlobalConstant.CONSTITUTIONHTMLDIR + "/.git")
      if( gitfile.exists)
      { log("   .git file exists in " + GlobalConstant.CONSTITUTIONHTMLDIR + ", so everything is under (version) control, my dear organic friend...")
      }
      else
      { log("   .git file doesn't exist yet in " + GlobalConstant.CONSTITUTIONHTMLDIR + ", creating new git repo...")
        val jgitInitCommand:InitCommand = Git.init
        jgitInitCommand.setDirectory(new File(GlobalConstant.CONSTITUTIONHTMLDIR))
        jgitInitCommand.call
      }

      // now open the git repository just created for usage throughout the program (jgitRepo represents the "opened" git repo).
      jgitBuilder = Some(new FileRepositoryBuilder())
      jgitRepo = Some(jgitBuilder.get.setGitDir(new File(GlobalConstant.CONSTITUTIONHTMLDIR + "/.git"))
      //.readEnvironment() // scan environment GIT_* variables
      //.findGitDir() // scan up the file system tree <&y2012.06.30.19:51:12& perhaps leave this one out, it SHOULD be in this dir, not in a superdir>
         .build()
      )
      println("   jgitRepo directory: " + jgitRepo.get.getDirectory )
      println("   jgitRepo is bare (false is correct): " + jgitRepo.get.isBare())
      jgit = Some(new Git(jgitRepo.get)) // <? &y2012.06.30.18:53:23& or isn't this thread safe? I now share one jgit object accross user-sessions (I think... because I instantiate this thing in Boot.scala). Perhaps I should instantiate one per user-session...>
      println(jgit.get.status.call.getUntracked)
   }
}

object Translations
{  def gitUserId(p:Player):PersonIdent =
   {  gitUserId(p.userIdAsString)
   }
   
   def gitUserId(liftUserId:String):PersonIdent =
   {  new PersonIdent(liftUserId, "swiftgame")
   }

   def gitUserId(liftUserId:String, date:Date):PersonIdent =
   {  val pi = gitUserId(liftUserId)
      new PersonIdent(pi, date)
   }   
}

/** @todo move this to another general jgit utils package. The package swift.jgit is intended for interaction between swift and jgit that are specific to these two systems. Hmm, but this requires refactoring... Now it is really tied to SWiFT through GlobalConstant.jgitRepo
  */
object JgitUtils
{  def revComFromCommitId(commitId: String):Option[RevCommit] =
   {  val rw = new RevWalk(GlobalConstant.jgitRepo.get)
      Some(rw.parseCommit(ObjectId.fromString(commitId)))
      // TODO: handle exception thrown by revComFromCommitId, turn them into none if commit is not found, otherwise just throw the exception.
   }

   /** @todo highly inefficient, other solution possible (or at least some caching/memoisation)? Problem sits in jgit, for example see http://stackoverflow.com/questions/7501646/jgit-retrieve-tag-associated-with-a-git-commit
     */
   def tagsOf(commitId:String):List[Ref] =
   {  log("tagsOf called")
      val allTags:List[Ref] = GlobalConstant.jgit.get.tagList.call.toList
      val commitId_jgitFormat = ObjectId.fromString(commitId)
      val tagsOfCommitId = allTags.filter
      {  tag =>
         {  GlobalConstant.jgitRepo.get.peel(tag).getPeeledObjectId.equals(commitId_jgitFormat)
         }
      }

      tagsOfCommitId
   }

}

}
