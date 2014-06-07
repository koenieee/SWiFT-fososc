package org.ocbkc.swift.snippet

import scala.xml.{Text, NodeSeq}
import net.liftweb.util.Helpers._
import scala.xml.Text
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.Full
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.OCBKC.Constitution
import org.ocbkc.swift.general.GUIdisplayHelpers._

class analyseFluencySessionsConsti
{ //todo by Wenzel.

  def constiTable(consti_id: Constitution, ns:NodeSeq):NodeSeq =
  { val releaseID = "empty"
    val constitution: Constitution = consti_id

    val header =
    bind("top", chooseTemplate("top","row", ns),
      "release"          -> <b>Release</b>,
      "currentVersionId" -> <b>Current Version ID</b>,
      "creationDate"     -> <b>Creation Date</b>,
      "analyseLink"      -> <b>Analyse</b>
    )
    header ++
    constitution.commitIdsReleases.flatMap //this one?
    { release =>
      bind("top", chooseTemplate("top", "row", ns),
        "release"            -> Text(release),
        "currentVersionId"   -> Text("TODO"),
        "creationDate"       -> Text(constitution.creationTime.toString),
        "analyseLink"        -> SHtml.link("analyseFluencySessionsOfRelease.html?consti_id="+consti_id.constiId+"&release_id="+release,()=>(),Text("Release Session"))
      )
    }
  }

  def render(ns: NodeSeq):NodeSeq=
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
