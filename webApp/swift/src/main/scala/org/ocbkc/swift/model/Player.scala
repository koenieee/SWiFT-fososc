package org.ocbkc.swift {
package model {

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._

import bootstrap.liftweb.BootHelpers
import org.ocbkc.swift.OCBKC._
import org.ocbkc.swift.OCBKC.ConstitutionTypes._
import org.ocbkc.swift.snippet.SesCoord
import org.ocbkc.swift.global._
import org.ocbkc.swift.test._
import org.ocbkc.swift.global.Logging._

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

   override def login =
   {  if( constiSelectionProcedure == OneToStartWith )
      {  LiftRules.viewDispatch.append
         {  // This is an explicit dispatch to a particular method based on the path
            case List("constiTrainingDecision") =>
               Left(() => Full( BootHelpers.dispatch4ConstiTrainingDecision ))
         }
      }
      super.login
   }

   override def create:Player =
   {  println("   Player.create called") 
      val p = super.create. constiSelectionProcedureInner(CSP2int(RandomOneToStartWith)).firstChosenConstitution(-1)
      p.save
      p
   }

   val int2CSP = Map(0->NoProc, 1->OneToStartWith, 2->RandomOneToStartWith)
   val CSP2int = int2CSP.map(_.swap)
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

   // because I don't know how to make a MappedField referring to singleton objects, work with integers and a Map (see object Player) to connect them to the singleton objects, which is used by a wrapper method constiSelectionProcedure.
   /** Why is this defined on the level of the player, shouldn't it be a global setting?
     */
   def constiSelectionProcedure =
   {  Player.int2CSP(constiSelectionProcedureInner)
   }

   object constiSelectionProcedureInner extends MappedInt(this) // embodies which procedure is followed to let people study the constitutions, e.g.: allow them to choose only one constitution and study it the first time they play, or allow any constitution to be consulted at any moment etc. etc
/*
   def firstChosenConstitution:Option[Constitution] =
   {   
   }
*/
   val thisPlayer = this
   object firstChosenConstitution extends MappedInt(this)
   {  /** Important: this is an intelligent apply, which also automatically sets the timeFirstChosenConstitution, releaseOfFirstChosenConstitution, and notifies Constitution that it was chosenAsFirstConsti.
        */
      override def apply(constiId:Int) =
      {  if(constiId != -1) // -1 is passed when a Player-object is created by the Mapper-framework (true?), and doesn't mean there is actually a first consti being chosen.
         {  val consti = Constitution.getById(constiId).getOrElse(throw new RuntimeException("Constitution with id = " + constiId + " doesn't exist, while it should..."))
            thisPlayer.timeFirstChosenConstitution(SystemWithTesting.currentTimeMillis).save
            consti.chosenAsFirstConsti // WARNING: must be called before setting releaseOfFirstChosenConstitution!
            thisPlayer.releaseOfFirstChosenConstitution(consti.lastReleaseCommitId.getOrElse(logAndThrow("get should be possible because the player is only presented constitutions which HAVE a last release."))).save
         }

         super.apply(constiId)
      }
   }
   
   // <&y2012.08.03.10:20:25& perhaps in future refactor, or supplement, with more generic, row of chosen constitutions>
   object releaseOfFirstChosenConstitution extends MappedString(this, GlobalConstant.GIThASHsIZE)

   /** @todo &y2013.05.12.11:46:29& perhaps automatically set this when firstChosenConstitution is set. However that can be a problem is data that is restored somehow, then current system time and moment of chosing may be different.
     */
   object timeFirstChosenConstitution extends MappedLong(this)
   def followedConstis:List[ConstiId] =
   {  FollowerConsti_join.findAll( By(FollowerConsti_join.player, this) ).map{fcj => fcj.constiId.is}
   }
   
   //object deletePlayer extends MappedString(this, 100)
  // object createPlayer extends MappedString(this, 100)
   
}

}
}
