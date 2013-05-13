
/** Contains connections beween jgit and SWiFT
 *
 */
package org.ocbkc.swift.jgit
{  
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk, RevTag}
import org.gitective.core.BlobUtils
import org.ocbkc.swift.model.Player
import org.ocbkc.swift.global._
import org.ocbkc.swift.global.Logging._
import java.util.Date
import scala.collection.JavaConversions._

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
   {  val rw = new RevWalk(GlobalConstant.jgitRepo)
      Some(rw.parseCommit(ObjectId.fromString(commitId)))
      // TODO: handle exception thrown by revComFromCommitId, turn them into none if commit is not found, otherwise just throw the exception.
   }

   /** @todo highly inefficient, other solution possible (or at least some caching/memoisation)? Problem sits in jgit, for example see http://stackoverflow.com/questions/7501646/jgit-retrieve-tag-associated-with-a-git-commit
     */
   def tagsOf(commitId:String):List[Ref] =
   {  log("tagsOf called")
      val allTags:List[Ref] = GlobalConstant.jgit.tagList.call.toList
      val commitId_jgitFormat = ObjectId.fromString(commitId)
      val tagsOfCommitId = allTags.filter
      {  tag =>
         {  GlobalConstant.jgitRepo.peel(tag).getPeeledObjectId.equals(commitId_jgitFormat)
         }
      }

      tagsOfCommitId
   }

}

}
