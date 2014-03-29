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
import org.ocbkc.swift.OCBKC.scoring._
import org.ocbkc.swift.global._
import org.ocbkc.swift.general.GUIdisplayHelpers._
import Helpers._
import System.err.println
import org.ocbkc.swift.model._
import org.eclipse.jgit.revwalk.RevCommit 
import org.eclipse.jgit.lib.ObjectId
import org.ocbkc.swift.global.LiftHelpers._
import _root_.net.liftweb.widgets.tablesorter.TableSorter

class History
{  val sesCoordLR = SesCoord.is; // extract session coordinator object from session variable.
   var checkedCommits:List[RevCommit] = Nil

   val const:Constitution = S.param("id") match
   {  case Full(idLoc)  => Constitution.getById(idLoc.toInt) match
                           { case Some(constLoc)   => { println("   Constitution id:" + idLoc); constLoc }
                             case None             => { S.redirectTo("notfound.html") }
                           }
      case _            => S.redirectTo("constitutions")
   }

   def historyTableRows(ns:NodeSeq):NodeSeq =
      { 
		   println("historyTableRows called")
		TableSorter("#myTable");
      
      implicit val displayIfNone = "-"
      if( const == null ) println("   bug: const == null")

      // create headers
      val header = bind(
            "top", chooseTemplate("top", "row", ns),
            "view"               -> emptyNode,
            "restore"            -> emptyNode,
            "checkbox"           -> emptyNode,
            "publishDescription" -> <b>Description</b>,
            "date"               -> <b>Creation date</b>,
            "release"          -> <b>Release</b>,
            "author"             -> <b>Author</b>,
            "fluency"            -> <b>Fluency</b>
            )

      // create data rows
      header ++
      const.getHistory.flatMap(
      revcom => 
      {  val playerId = revcom.getAuthorIdent.getName
         val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")
         val isRelease = const.isRelease(revcom.name)

         bind( "top", chooseTemplate("top", "row", ns),            
            "view"               -> SHtml.link("constitutionhistoric?constid=" + const.constiId + "&commitid=" + revcom.name, () => Unit, Text("view")),
            "restore"            -> SHtml.link("restore?constid=" + const.constiId + "&commitid=" + revcom.name, () => Unit, Text("restore")),
            "checkbox"           -> SHtml.checkbox(false, processCheckbox(_, revcom)),
            "publishDescription" -> Text(revcom.getFullMessage),
            "date"               -> Text(df.format(revcom.getCommitTime.toLong*1000).toString),
            "release"          -> {    println("   release?")
                                       println("   const.commitIdsReleases: " + const.commitIdsReleases)
                                       println("   searching revcom.name = " + revcom.name)
                                       if(isRelease) Text("R" + const.releaseIndex(revcom.name)) else Text("-")
                                    },
            "author"             -> Text
                                    (  Player.find(playerId) match
                                       {  case Full(player)  => player.swiftDisplayName
                                          case _             => { println("    bug: Player with id " + playerId + " not found."); "player unknown (this is a bug, please report it)" }
                                       }
                                    ),
            "fluency"             -> { if( isRelease ) optionToUI(ConstiScores.averageFluency(GlobalConstant.AverageFluency.minimalSampleSizePerPlayer, revcom.name, GlobalConstant.AverageFluency.fluencyConstantK)) else "-" }
            )
      }
      )         
   }

   def processDiffButton() =
   {  println("processDiffButton called")
      if( checkedCommits.size != 2)
      {  println("   checkedCommits.size != 2, so cannot do diff.")
         S.redirectTo("history?id=" + const.constiId) // <&y2012.07.13.19:22:38& add error param here>
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
            "constname"    -> Text(const.constiId.toString),
            "diffBt"       -> SHtml.button("Show difference", processDiffButton),
            "history"      -> historyTableRows(ns) 
          )
          
   }
}

}
}
