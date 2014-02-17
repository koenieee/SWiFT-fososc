/** 
  * Protocol:
  * - If a top-level sentence of a CTL, coincides with another CTL language (for example: PredApp(...)), the following naming convention is followed:
      PredApp_FOL, PredApp_InstaRet
      <&y2013.12.02.15:26:48& refactor in the future, because this requires PredApp to be defined twice (adding to the shotgun surgery anti-pattern).>
  */

package org.ocbkc.swift.logilang

import org.ocbkc.swift.global.Logging._
 
trait QueryLang // <move to package .query>
trait CTLbase
trait CTLsent
//trait CTLsent_rb

/* <&y2014.01.26.19:05:50& if only allowing correct pf in CTLrepresentationTransforms, don't I again introduce the necessity to build in "already parsed" checks in SessionInfo again???> [A &y2014.01.26.19:05:56& yes, as far as I can tell. For now, just skip this optimization]
*/

/** Requires the pf to be correct. Otherwise it will reject creating the representation bundle.
  */
abstract class CTLrepresentationBundle[ScalaRepresentation__TP]()
{  val transform:CTLrepresentationTransforms[ScalaRepresentation__TP]

   private[logilang] var pf_ :Option[String] = None // pure format
   private[logilang] var sf_ :Option[ScalaRepresentation__TP] = None // Scala data-structure format

   val displayNameCTL:String

   def pf:String =
   {  pf_ match
      {  case Some(pfLocal) => pfLocal
         case None =>
         {  val pfLocal = sf_.get.toString // get should always work because there are only two representations in this bundle. If pf_ is not defined, sf_ MUST be.
            pf_ = Some(pfLocal)
            pfLocal
         }
      }
   }

   def sf:ScalaRepresentation__TP =
   {  sf_ match
      {  case Some(sfLocal) => sfLocal
         case None      => // WIW: this shouldn't happen because if initialised with this(pf) then it is always immediately parsed to check correctness.
         {  pf_ match
            {  case Some(pf) =>
               {  sf_ =
                     Some(
                        transform.pf2sf(pf) match
                        {  case ParseResult(None, parseErrorMsg, _) => 
                           {  logAndThrow("There is a parse error in the pf format " + parseErrorMsg)
                           }
                           case ParseResult(Some(sfLocal), _, _) => sfLocal
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

//trait CTLrepresentationBundleFactory[ScalaRepresentation__TP]
trait CTLrepresentationBundleFactory[ScalaRepresentation__TP, CTLrepresentationBundle__TP <: CTLrepresentationBundle[ScalaRepresentation__TP]]
{  protected def apply():CTLrepresentationBundle__TP // the default factory must be supplied.
   case class FactoryResult(parseResult:Option[CTLrepresentationBundle__TP], parseErrorMessage:String, parseWarningMessage:String)

   /** @returns
     */
   def apply(pf: String):FactoryResult =
   {  val crb:CTLrepresentationBundle__TP = apply()

      crb.pf_ = Some(pf)
           
      crb.transform.pf2sf(pf) match
      {  case ParseResult(None,       errMsg, warnMsg)  => FactoryResult(None, errMsg, warnMsg)
         case ParseResult(Some(sf),   _,      warnMsg)  =>
         {  crb.sf_ = Some(sf)
            FactoryResult(Some(crb),  ""    , warnMsg)
         }
      }
   }

   def apply(sf: ScalaRepresentation__TP):CTLrepresentationBundle__TP =
   {  val crb:CTLrepresentationBundle__TP = apply()

      crb.sf_ = Some(sf)
      crb
   }
}


/** @todo move to more general lib (parser lib)
  */
case class ParseResult[ParseResultExpression__TP](parseResult:Option[ParseResultExpression__TP], parseErrorMessage:String, parseWarningMessage:String)

trait CTLrepresentationTransforms[ScalaRepresentation__TP]
{  /** Translation pure format to Scala format. Returns None when there is an error in the syntax of pf.
     */
   def pf2sf(pf:String):ParseResult[ScalaRepresentation__TP]

   /** Translation Scala-format to pure-format
     */
   def sf2pf(sf:ScalaRepresentation__TP):String
}
