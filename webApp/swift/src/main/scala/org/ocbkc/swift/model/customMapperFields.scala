package org.ocbkc.swift.model

import org.ocbkc.swift.cores.EfeChallengeTypes._
import net.liftweb.mapper.{Mapper, MappedField}
import org.ocbkc.swift.logilang.FOLtheory
import org.ocbkc.swift.logilang.bridge.brone.BridgeDoc
import scala.xml.{NodeSeq, Text}
import net.liftweb.common.{Empty, Box, Full}
import net.liftweb.json.{JsonAST, JValue}
import net.liftweb.http.js.{JsExp, JE}
import scala.xml.Text
import net.liftweb.common.Full
import scala.reflect.Method
import java.sql.Date
import net.liftweb.db.DriverType


/**
 * Created by koen on 24-8-14.
 */

abstract class MappedEfeQuerySent_rb[T <: Mapper[T]] (val fieldOwner : T) extends MappedField[EfeQuerySent_rb,T]
{ private var data: EfeQuerySent_rb = defaultValue
  private var orgData: EfeQuerySent_rb = defaultValue

  private def st(in: EfeQuerySent_rb) {
    data = in
    orgData = in
  }
  def defaultValue: EfeQuerySent_rb = EfeQuerySent_rb
  def dbFieldClass = classOf[EfeQuerySent_rb]
  protected def i_is_! = data
  protected def i_was_! = orgData
  override def doneWithSave() {
    orgData = data
  }

  import scala.reflect.runtime.universe._
  def manifest: TypeTag[EfeQuerySent_rb] = typeTag[EfeQuerySent_rb]

  def sourceInfoMetadata(): SourceFieldMetadata{type ST = EfeQuerySent_rb} =
    SourceFieldMetadataRep(name, manifest, new FieldConverter {
      type T = EfeQuerySent_rb
      def asString(v: T): String = v.toString
      def asNodeSeq(v: T): Box[NodeSeq] = Full(Text(asString(v)))
      def asJson(v: T): Box[JValue] = Full(JsonAST.JString(v))
      def asSeq(v: T): Box[Seq[SourceFieldInfo]] = Empty
    })
  override def readPermission_? = true
  override def writePermission_? = true
  protected def i_obscure_!(in : EfeQuerySent_rb) = defaultValue
  protected def real_i_set_!(value : EfeQuerySent_rb): EfeQuerySent_rb = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }
  def asJsExp: JsExp = JE.Num(get)
  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JString(get))

  override def setFromAny(in: Any): EfeQuerySent_rb = {
    in match {
      case db => this.set(db)

    }
  }
  def real_convertToJDBCFriendly(value: EfeQuerySent_rb): Object = new EfeQuerySent_rb
  def targetSQLType = EfeQuerySent_rb
  def jdbcFriendly(field : String) = new EfeQuerySent_rb
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
    (inst, v) => doField(inst, accessor, {case f:MappedEfeQuerySent_rb[T] => f.st(if (v == null) defaultValue else v.getTime)})
  def buildSetStringValue(accessor: Method, columnName: String): (T, String) =>
    Unit = (inst, v) => doField(inst, accessor, {case f: MappedEfeQuerySent_rb[T] => f.st(toDouble(v))})

  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) =>
    Unit = (inst, v, isNull) => doField(inst, accessor, {case f: MappedEfeQuerySent_rb[T] => f.st(if (isNull) defaultValue else v)})

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) =>
    Unit = (inst, v) => doField(inst, accessor, {case f: MappedEfeQuerySent_rb[T] => f.st(toString(v))})
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.doubleColumnType + notNullAppender()
}

abstract class MappedFOLtheory[T <: Mapper[T]] (val fieldOwner : T) extends MappedField[FOLtheory,T]
{ }

abstract class MappedBridgeDoc[T <: Mapper[T]] (val fieldOwner : T) extends MappedField[BridgeDoc,T]
{ }

abstract  class MappedEfeAnswerLangSent[T <: Mapper[T]] (val fieldOwner : T) extends MappedField[EfeAnswerLangSent,T]
{}


