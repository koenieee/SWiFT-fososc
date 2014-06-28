package org.ocbkc.swift.snippet


import _root_.scala.xml._
import net.liftweb.util.Helpers._

/**
 * Created by koen on 28-6-14.
 */
class FluencyTimeSeries {

  def render(xhtml:NodeSeq):NodeSeq=
  {
    bind("top",xhtml,
        "user" -> Text("todo"),
        "input" ->Text("todo"),
        "graph" ->Text("todo")


    )


  }

}
