
/** Contains connections beween jgit and SWiFT
 *
 */
package org.ocbkc.swift.jgit
{  
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.gitective.core.BlobUtils
import org.ocbkc.swift.model.Player
import org.ocbkc.swift.global._

object Translations
{  def gitUserId(p:Player):PersonIdent =
   {  gitUserId(p.userIdAsString)
   }
   
   def gitUserId(liftUserId:String):PersonIdent =
   {  new PersonIdent(liftUserId, "swiftgame")
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
}

}
