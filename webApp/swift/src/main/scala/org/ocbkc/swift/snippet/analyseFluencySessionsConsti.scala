package org.ocbkc.swift.snippet

import scala.xml.{Text, NodeSeq}
import net.liftweb.util.Helpers._
import org.ocbkc.swift.global.DisplayHelpers._
import scala.xml.Text
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.OCBKC.Constitution
import org.ocbkc.swift.OCBKC.scoring.ConstiScores
import org.ocbkc.swift.general.GUIdisplayHelpers._

class analyseFluencySessionsConsti
{ //todo by Wenzel.

  def constiTable(consti_id: Constitution, ns:NodeSeq):NodeSeq =
  { val constitution: Constitution = consti_id
    implicit val displayNoneAs = "-"
    val df = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm")

    val header =
    bind("top", chooseTemplate("top","row", ns),
//      "releaseUUID"      -> <b>Release Unique ID</b>,
      "releaseIndex"     -> <b>Release Index</b>,
      "fluencyScore"     -> <b>Fluency Score</b>,
      "creationDate"     -> <b>Creation Date</b>,
      "analyseLink"      -> <b>Analyse</b>
    )
    header ++
    constitution.commitIdsReleases.flatMap
    { releaseId =>
      bind("top", chooseTemplate("top", "row", ns),
//        "releaseUUID"        -> Text(releaseId),
        "releaseIndex"       -> Text("R" + constitution.releaseIndex(releaseId)),
        "fluencyScore"       -> Text(optionToUI(ConstiScores.averageFluency(releaseId).map{ defaultRounding })),
        "creationDate"       -> Text(df.format(constitution.creationTime).toString),
        "analyseLink"        -> SHtml.link("analyseFluencySessionsOfRelease.html?release_id="+releaseId, ()=>(), Text("Analyse"))
      )
    }
  }

  def render(ns: NodeSeq):NodeSeq =
  { S.param("consti_id") match
    { case Full(consti_id) =>
      { log("Searching for Constis with ID: " + consti_id)
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
