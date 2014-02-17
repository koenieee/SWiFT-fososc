package org.ocbkc.swift.logilang.query

import org.ocbkc.swift.logilang._

/** Supertrait of query-sentences.
  */
trait QuerySent extends CTLsent

/** <&y2013.12.02.15:12:34& refactor, move to general CTL construction lib>
  */

case class PatVar(id:String)

/** <&y2013.12.03.09:54:56& or should this be part of logilang>
 */
object ComparisonOperator extends Enumeration
{  type ComparisonOperator = Value
   val Geq = Value
}

/*  <&y2012.04.23.17:01:11&For current increment, do not yet implement the following, but do it for a next:>
class ComparisonOperator

case class Eqt() extends ComparisonOperator
case class Gt() extends ComparisonOperator
case class Lt() extends ComparisonOperator
case class Geq() extends ComparisonOperator
case class Leq() extends ComparisonOperator
*/
// <&y2012.04.22.00:22:40& make use of Clean data structures I designed: copy the idea here.>
