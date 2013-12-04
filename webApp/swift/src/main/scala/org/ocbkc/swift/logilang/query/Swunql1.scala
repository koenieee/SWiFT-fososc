package org.ocbkc.swift.logilang.query.swunql1
{
import System._
import java.io._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang.query.folnuminqua._
import org.ocbkc.swift.logilang.query.plosemo._
import net.liftweb.json._
import org.ocbkc.swift.test.CLIwithFileInput

trait Swunql1Sent

case class SwunqlPlosemoPat(plosemoPat:PlosemoPat) extends Swunql1Sent

case class SwunqlFolnuminqueQueryPat(folnuminqueQuery:FolnuminquaQuery) extends Swunql1Sent

}
