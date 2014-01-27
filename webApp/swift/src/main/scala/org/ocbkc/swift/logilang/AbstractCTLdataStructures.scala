/** 
  * Protocol:
  * - If a top-level sentence of a CTL, coincides with another CTL language (for example: PredApp(...)), the following naming convention is followed:
      PredApp_FOL, PredApp_InstaRet
      <&y2013.12.02.15:26:48& refactor in the future, because this requires PredApp to be defined twice (adding to the shotgun surgery anti-pattern).>
  */

package org.ocbkc.swift.logilang

import org.ocbkc.swift.global.Logging._
 
trait QueryLang // <move to package .query.
trait CTLbase
trait CTLsent

/* <&y2014.01.26.19:05:50& if only allowing correct pf in CTLrepresentationTransforms, don't I again introduce the necessity to build in "already parsed" checks in SessionInfo again???> [A &y2014.01.26.19:05:56& yes, as far as I can tell. For now, just skip this optimization]
*/

/** Requires the pf to be correct. Otherwise it will reject creating the representation bundle.
  */
abstract class CTLrepresentationBundle[ScalaRepresentation__TP]()
{
   val transform:CTLrepresentationTransforms[ScalaRepresentation__TP]

   var pf_ :Option[String] = None // pure format
   protected var sf_ :Option[ScalaRepresentation__TP] = None // Scala data-structure format

   val displayNameCTL:String

   def sf:ScalaRepresentation__TP =
   {  sf_ match
      {  case Some(sfLocal) => sfLocal
         case None      => // WIW: this shouldn't happen because if initialised with this(pf) then it is always immediately parsed to check correctness.
         {  pf_ match
            {  case Some(pf) =>
               {  sf_ =
                     Some(
                        transform.pf2sf(pf) match
                        {  case transform.Pf2sfResult(None, parseErrorMsg, _) => 
                           {  logAndThrow("There is a parse error in the pf format " + parseErrorMsg)
                           }
                           case transform.Pf2sfResult(Some(sfLocal), _, _) => sfLocal
                        }
                     )

                  sf_.get
               }
               case None     => logAndThrow( "No Scala representation of " + displayNameCTL + " available.")
            }
         }
      }
   }  
}

trait CTLrepresentationBundleFactory[CTLrepresentationBundle__TP <: CTLrepresentationBundle[Object]]
{  protected def apply():CTLrepresentationBundle__TP // the default factory must be supplied.
   
   case class FactoryResult(parseResult:Option[CTLrepresentationBundle__TP], parseErrorMessage:String)

   def apply(pf: String):FactoryResult =
   {  val crb:CTLrepresentationBundle__TP = apply()
      //@todo mustdo check whether pf parses correctly, otherwise reject creation.
      crb.pf_ = Some(pf)
      FactoryResult(Some(crb), "") // @todo include parse error when not succeeded etc.
   }
// TODO   def apply(sf: ScalaRepresentation__TP) = { this(); sf_ = Some(sf) }
}

trait CTLrepresentationTransforms[ScalaRepresentation__TP]
{  case class Pf2sfResult(parseResult:Option[ScalaRepresentation__TP], parseErrorMessage:String, parseWarningMessage:String)
   /** Translation pure format to Scala format. Returns None when there is an error in the syntax of pf.
     */
   def pf2sf(pf:String):Pf2sfResult

   /** Translation Scala-format to pure-format
     */
   def sf2pf(sf:ScalaRepresentation__TP):String
}
