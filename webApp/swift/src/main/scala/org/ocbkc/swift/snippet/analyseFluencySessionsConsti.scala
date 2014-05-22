package org.ocbkc.swift.snippet

import scala.xml.{Text, NodeSeq}
import net.liftweb.util.Helpers._
import scala.xml.Text


class analyseFluencySessionsConsti {
//todo by Wenzel.

  def render(ns: NodeSeq):NodeSeq={
    bind("top", chooseTemplate("top", "row", ns),
      "release" -> Text("TODO"),
    "currentVersionId" -> Text("TODO"),
    "creationDate" -> Text("TODO"),
    "analyseLin" -> Text("TODO")



    )


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
