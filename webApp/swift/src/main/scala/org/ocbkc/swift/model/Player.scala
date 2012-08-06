package org.ocbkc.swift {
package model {

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import org.ocbkc.swift.OCBKC.ConstiSelectionProcedure._
import org.ocbkc.swift.OCBKC.Constitution

/**
 * The singleton that has methods for accessing the database
 */

object Player extends Player with MetaMegaProtoUser[Player] {
  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content">
			       <lift:bind /></lift:surround>)
  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email, locale, timezone, password, textArea)

  // comment this line out to require email validations
  override def skipEmailValidation = true

}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class Player extends MegaProtoUser[Player] {
   def getSingleton = Player // what's the "meta" server

   // define an additional field for a personal essay
   object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
   }

   def swiftDisplayName:String =
   {  firstName.get + " " + lastName.get + " (id: " + userIdAsString + ")"
   }

   val constiSelectionProcedure = OneToStartWith // embodies which procedure is followed to let people study the constitutions, e.g.: allow them to choose only one constitution and study it the first time they play, or allow any constitution to be consulted at any moment etc. etc.
   var firstChosenConstitution:Option[Constitution] = None // <&y2012.08.03.10:20:25& perhaps in future refactor, or supplement, with more generic, row of chosen constitutions>
   var isFirstTimePlayer:Boolean = true // <&y2012.08.04.19:43:17& set this to true after first session has been completed (or other conditions?)>

}

}
}
