package org.ocbkc.swift.logilang.translations
{
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.bridge.brone._


/** @todo &y2014.02.12.17:09:31& perhaps in the future different BridgeDocs formats come available, then change it to a type-parameter instead
  */
trait TranslateCTL2NL[CTLsent__TP <: CTLsent]
{  def apply(ctlSent: CTLsent__TP, bs: BridgeDoc):String
}

}
