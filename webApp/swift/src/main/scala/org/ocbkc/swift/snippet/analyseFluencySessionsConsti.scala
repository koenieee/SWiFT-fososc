package org.ocbkc.swift.snippet

import scala.xml._
import net.liftweb.util.Helpers._
import org.ocbkc.swift.global.DisplayHelpers._
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.OCBKC.Constitution
import org.ocbkc.swift.OCBKC.scoring.{PlayerScores, ConstiScores}
import org.ocbkc.swift.general.GUIdisplayHelpers._
import net.liftweb.widgets.tablesorter.TableSorter
import net.liftweb.common.Full
import scala.xml.Text
import net.liftweb.common.Full

class analyseFluencySessionsConsti
{ //todo by Wenzel.

  def constiTable(consti_id: Constitution, ns:NodeSeq):NodeSeq =
  { val constitution: Constitution = consti_id
    implicit val displayNoneAs = "-"
    val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

    val header = Elem(
      null,
      "table",
      new UnprefixedAttribute("id",
      Text("analyseFluencySessionsPlayer"),
      new UnprefixedAttribute("class", Text("tablesorter"), Null)),
      TopScope,
      <thead><tr><th>Release Index</th>
      <th>Fluency Score</th>
      <th>Creation Date</th>
      <th>Analyse</th></tr></thead>
      ,
      <tbody>{  constitution.commitIdsReleases.map(
      releaseId =>
      { val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")
        <tr>
        <td>{ "R" + constitution.releaseIndex(releaseId) }</td>
        <td>{ optionToUI(ConstiScores.averageFluency(releaseId).map{ defaultRounding })  }</td>
        <td>{df.format(constitution.creationTime)}</td>
        <td><a href={ "analyseFluencySessionsOfRelease.html?release_id="+releaseId }>Analyse</a></td>

        </tr>
      }
      )
      }
      </tbody>
    )
    header ++ TableSorter("#analyseFluencySessionsPlayer")

  }

  def render(ns: NodeSeq):NodeSeq =
  { S.param("consti_id") match
    { case Full(consti_id) =>
      { log("Searching for Constis with ID: " + consti_id)
        log("System Locale:" )
        val constitution: Option[Constitution] = Constitution.getById(consti_id.toInt)

        if( !constitution.isEmpty )
        { log("constiID " + consti_id + " Found!")

          bind("top", ns,
          "constName"         -> Text(constitution.get.constiId.toString),
          "description"       -> Text(constitution.get.shortDescription),
          "constiTable"       -> constiTable(constitution.get, ns)
          )

        }
        else
        { log("Constitution with ConstiID " + consti_id + " Not found!")
          S.redirectTo("../index")
        }
      }
      case _ =>
      { log("[BUG] Consti_id not found, redirect to index")
        S.redirectTo("../index")

      }
    }
  }

  //Bind with these:

  /*
        <top:sessionTable>
                <top:row>
                    <tr>
                        <td><top:release />              </td>
                        <td><top:currentVersionId />           </td>
                        <td><top:creationDate />   </td>
                        <td><top:analyseLink />   </td>
                    </tr>
                </top:row>
            </top:sessionTable>


  */
}
