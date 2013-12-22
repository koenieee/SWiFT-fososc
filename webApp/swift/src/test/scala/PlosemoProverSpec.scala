// <rename reas to prover>
package org.ocbkc.swift.reas.plosemo
{
import org.ocbkc.swift.reas
import org.ocbkc.swift.parser._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.logilang.query._
import org.ocbkc.swift.logilang.query.ComparisonOperator._
import org.ocbkc.swift.logilang.query.plofofa._
import org.ocbkc.swift.tpwrap._
import query._
import System.err.println
import java.io._
import org.ocbkc.swift.test.CLIwithFileInput
import org.ocbkc.swift.global.Logging._
//import org.specs2.mutable._

/* &y2013.12.05.19:48:04& WIW:
- tried to run mvn specs2:run-specs, but there is nothing in target/test-classes, I think I have to put the PlosemoProverSpec class in another spot (see pom.xml!)

*/

/*
class PlosemoProverSpec extends Specification
{  val predicateB = Predicate("B",1)
   val plosemoQuery = MostInfo(PatVar("s"), Forall(Var("x"), PatVar("s"), PredApp(predicateB, List(Var("x")))))
   val folTheory = new FOLtheory
   folTheory.addStat(PredApp_FOL(predicateB, List(Constant("makkelPowerConnect"))))

   val queryResult = Prover.query(plosemoQuery, folTheory)
/*
   "Prover.query(plosemoQuery, folTheory) is null: " ! ( queryResult == null )
*/ 
   /*
   "Prover.query(plosemoQuery, folTheory)" should
   {  " equal null, because we are still testing" in
      {  queryResult must beEqualTo(1)
      }
   }
   */
   /*
   "The 'Hello world' string" should {
 "contain 11 characters" in {
   "Hello world" must have size(11)
 }
 "start with 'Hello'" in {
   "Hello world" must startWith("Hello")
 }
 "end with 'world'" in {
   "Hello world" must endWith("world")
   */
}
*/
}
