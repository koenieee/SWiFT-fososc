// todo rename file

/** Brone is a bridge language which can be used for any CTL which has constants that consist of strings. It forms a bridge between these constants and the proper names used in NL.
Currently it is used for:
- TODO 
  */

package org.ocbkc.swift.logilang.bridge.brone

import org.ocbkc.swift.logilang._

// <&y2013.12.22.12:36:19& give this bridge language a good name, for now I called it brone (bridge langauge one)

trait BroneSent


/** Semantics: All entCTL strings, are CTL constants. All entNL strings are NL words which represent an entity (such as nouns, proper names). all CTL constants in entCTL and all NL words in entNL refer to the same, single, entity. entCTLname and entNLname must each contain at least one element.
  */



/** For now it is assumed that each predicate which occurs in a PredicateBridgeSent is one place and its application can be translated to the form:

entityCTLname is predNLname

example: 
P(e)
EntityBridge(e, Edward)
PredicateBridgeSent(P, big)

so in this case:
"Edward is big"

  */
case class PredicateBridgeSent(predCTLname: List[String], predNLname: List[String]) extends BroneSent

case class EntityBridgeSent(entCTLname: List[String], entNLname: List[String]) extends BroneSent
{  // <&y2014.01.03.09:44:35& apply method which check whether there is at least one element. TODO by Mussie...
}

class BridgeDoc
{  val bridgeSents:List[BroneSent] = Nil

   def entityBridgeSents = bridgeSents.collect{ case bs:EntityBridgeSent => bs.asInstanceOf[EntityBridgeSent] }

   def predicateBridgeSents = bridgeSents.collect{ case bs:EntityBridgeSent => bs.asInstanceOf[EntityBridgeSent] }

   /** @todo for making the right translation, also must be checked some properties of the natural language word in the bridge, does it allow the given construction? Information needs to be added to natlang.Info for this purpose. <& Is this indeed so?>
     */
   def constant2NLnoun(c:Constant):Option[String] =
   {  entityBridgeSents.find{ case EntityBridgeSent(entCTLname, _) => entCTLname == c.name  }
   }

   def predicate2NLAdjective(p:Predicate):Option[String] =
   {  predicateBridgeSents.find{ case PredicateBridgeSent(predCTLname,_) => predCTLname == p.name  }
   }
}
