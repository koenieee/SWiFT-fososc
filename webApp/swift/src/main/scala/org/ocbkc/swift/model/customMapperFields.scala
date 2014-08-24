package org.ocbkc.swift.model

import org.ocbkc.swift.cores.EfeChallengeTypes._
import net.liftweb.mapper.{Mapper, MappedField}
import org.ocbkc.swift.logilang.FOLtheory
import org.ocbkc.swift.logilang.bridge.brone.BridgeDoc

/**
 * Created by koen on 24-8-14.
 */

abstract class MappedEfeQuerySent_rb[T <: Mapper[T]] (val fieldOwner : T) extends MappedField[EfeQuerySent_rb,T]
{}

abstract class MappedFOLtheory[T <: Mapper[T]] (val fieldOwner : T) extends MappedField[FOLtheory,T]
{ }

abstract class MappedBridgeDoc[T <: Mapper[T]] (val fieldOwner : T) extends MappedField[BridgeDoc,T]
{ }

abstract  class MappedEfeAnswerLangSent[T <: Mapper[T]] (val fieldOwner : T) extends MappedField[EfeAnswerLangSent,T]
{}


