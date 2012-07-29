package org.ocbkc.swift 
{
package snippet 
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import org.ocbkc.swift.OCBKC._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.ocbkc.swift.global._
import org.ocbkc.swift.coord.ses._
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.lib.ObjectId
/*
object Error extends Enumeration {
  type Error = Value
  val noPublishDescriptionError = Value
}
*/
class Restore
{  println("Restore constructor called")
   val const:Constitution = S.param("constid") match
   {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                           { case Some(constLoc)   => { println("   Constitution id:" + idLoc); constLoc }
                             case None             => { S.redirectTo("notfound.html") }
                           }
      case _            => S.redirectTo("constitutions")
   }

   val commit:RevCommit = S.param("commitid") match
   {  case Full(commitidLoc)  => {  val rw = new RevWalk(GlobalConstant.jgitRepo)
                                    rw.parseCommit(ObjectId.fromString(commitidLoc))
                                 }
      case _            => S.redirectTo("constitutions")
   }

   println("   commitid = " + commit.name)

   // Try restoring
   val currentUserId:Int = Player.currentUserId match // <&y2012.06.23.14:41:16& refactor: put currentuserid in session var, and use that throughout the session-code>
      {  case Full(id)  => { id.toInt }
         case _         => { throw new RuntimeException("  No user id found.") }
      }
   println("   Trying to restore...")
   const.restore(commit, currentUserId.toString) 

   def render(ns: NodeSeq): NodeSeq =
   {  println("Restore.render")
     
      val answer   = bind( "top", ns, 
                           "publicationDate"    -> Text(commit.getCommitTime.toString),
                           "constId"            -> Text(const.id.toString),
                           "returnMsg"          -> Text("TODO")
                         )
      answer
   }
}

}
}
