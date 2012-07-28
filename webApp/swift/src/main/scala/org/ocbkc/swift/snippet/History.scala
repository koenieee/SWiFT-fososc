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
import org.eclipse.jgit.revwalk.RevCommit 
import org.eclipse.jgit.lib.ObjectId

class History
{  val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable.
   var checkedCommits:List[RevCommit] = Nil

   val const:Constitution = S.param("id") match
   {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                           { case Some(constLoc)   => { println("   Constitution id:" + idLoc); constLoc }
                             case None             => { S.redirectTo("notfound.html") }
                           }
      case _            => S.redirectTo("constitutions")
   }

   def historyTableRows(ns:NodeSeq):NodeSeq =
   {  println("historyTableRows")
      if( const == null ) println("   bug: const == null")
      const.getHistory.flatMap( revcom => bind( "top", chooseTemplate("top", "row", ns),            
         "view"               -> SHtml.link("constitutionhistoric?constid=" + const.id + "&commitid=" + revcom.name(), () => Unit, Text("view")),
         "restore"            -> SHtml.link("restore?constid=" + const.id + "&commitid=" + revcom.name(), () => Unit, Text("restore")),
         "checkbox"           -> SHtml.checkbox(false, processCheckbox(_, revcom)),
         "publishDescription" -> Text(revcom.getFullMessage()),
         "date"               -> Text(revcom.getCommitTime().toString),
         "author"             -> Text(revcom.getAuthorIdent().toString)
         ))
   }

   def processDiffButton() =
   {  println("processDiffButton called")
      if( checkedCommits.size != 2)
      {  println("   checkedCommits.size != 2, so cannot do diff.")
         S.redirectTo("history?id=" + const.id) // <&y2012.07.13.19:22:38& add error param here>
      }

      checkedCommits match
      {  case List(commitIdNewer,commitIdOlder) => S.redirectTo("diff?newerId=" + commitIdNewer.name + "&olderId=" + commitIdOlder.name ) 
         case _ => throw new RuntimeException("  my dear friend, checkedCommits has size other than 2, but that cannot happen in this part of the program, you should be deeply ashamed of yourself... But I'm forgiving, very forgiving... Piece of chocolate donated to me can buy you some time...")
      }
   }


   def processCheckbox(checked:Boolean, commitId:RevCommit) =
   {  println("processCheckbox called")
      println("   commit id = " + commitId.name )
      println("   commit time = " + commitId.getCommitTime().toString)
      if( checked )
      {  println("   checkbox of commit is checked by user.")
         checkedCommits = commitId::checkedCommits
      }
   }

   def render(ns: NodeSeq): NodeSeq =
   {  
      bind( "top", ns, 
            "constname"    -> Text(const.id.toString),
            "diffBt"       -> SHtml.button("Show difference", processDiffButton),
            "history"      -> historyTableRows(ns) 
          )
   }
}

}
}
