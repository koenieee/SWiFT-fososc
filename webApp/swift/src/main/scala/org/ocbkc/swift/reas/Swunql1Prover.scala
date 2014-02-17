// <rename reas to prover>
package org.ocbkc.swift.reas.swunql1
{
import org.ocbkc.swift.logilang.query.swunql1._
import org.ocbkc.swift.reas
import org.ocbkc.swift.reas.plofofa
import org.ocbkc.swift.reas.folnuminqua
import org.ocbkc.swift.parser._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang.query.ComparisonOperator._
import org.ocbkc.swift.tpwrap._
import query._
import System.err.println
import java.io._
import org.ocbkc.swift.test.CLIwithFileInput

object Prover extends reas.ProverTrait
{  def query(query:Swunql1Sent, ft:FOLtheory) = // <explicitly define return type>
   {  query match
      {  case SwunqlPlofofaPat(spp) => plofofa.Prover.query(spp, ft)
         case SwunqlFolnuminqueQueryPat(sfqp) => folnuminqua.Prover.query(sfqp, ft)
      }
   }
}
}
