package org.ocbkc.swift 
{
package snippet 
{

import _root_.scala.xml._
import _root_.scala.xml.transform._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import org.ocbkc.swift.lib._
import Helpers._
import org.ocbkc.swift.coord._
import org.ocbkc.swift.model._
import System.err.println

class InviteToConstitutionDesign
{  val sesCoordLR = SesCoord.is; // extract session coordinator object from session variable.

   def render(ns: NodeSeq): NodeSeq =
   {  object IncludeInvitationText extends RewriteRule 
      {  // (prefix: String, label: String, attributes: MetaData, scope: NamespaceBinding, child: Node*): Elem 
         override def transform(n: Node): Seq[Node] = n match 
         {  case Elem("top", "invitationtext", _, _, invitationText@_*) => invitationText
            case other => other
         }
      }

      val includeInvitationTextTranform = new RuleTransformer(IncludeInvitationText)

      if( sesCoordLR.numOfSessionsAfterConstiAccess == 0 )
         includeInvitationTextTranform.transform(ns)
      else
         NodeSeq.Empty // return an empty nodeseq
   }
}
}
}


