/** 
  * Protocol:
  * - If a top-level sentence of a CTL, coincides with another CTL language (for example: PredApp(...)), the following naming convention is followed:
      PredApp_FOL, PredApp_InstaRet
      <&y2013.12.02.15:26:48& refactor in the future, because this requires PredApp to be defined twice (adding to the shotgun surgery anti-pattern).>
  */

package org.ocbkc.swift.logilang
 
trait QueryLang // <move to package .query.
trait CTLbase
trait CTLsent
