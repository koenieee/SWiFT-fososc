package org.ocbkc.swift.logilang.query.swunql
{
import System._
import java.io._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.parser.CLIwithFileInput
import net.liftweb.json._

abstract class Swunql1Sent
{  
}

case class SwunqlPlosemoPat(plosemoPat:PlosemoPat) extends Swunql1Sent

case class SwunqlFolnuminqueQueryPat(folnuminqueQuery:FolnuminquaQuery) extends Swunql1Sent


