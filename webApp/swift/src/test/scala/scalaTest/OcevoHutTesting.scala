package org.ocbkc.swift.scalaTest

import org.ocbkc.ocevohut.{LocalSelectiveFitnessFunction, Individual, SUS}
import org.scalatest._

import scala.math._

/**
 * Created by koen on 17-12-14.
 */
class OcevoHutTesting extends FlatSpec with GivenWhenThen {
   "Ocevohut testing" must " be correctly runned" in {

      /**
       * For testing purposes
       */
      object MainTest
      {  def main(args:Array[String])
      {  object Types
      {  type _3DpointGenotype = (Int, Int, Int)
      }

         import Types._

         class _3Dpoint(x:Int, y:Int, z:Int) extends Individual[_3DpointGenotype]
         {  val genotype = (x,y,z)
         }

         /** Fitness is the inverse of the distance to the optimal point at (4,5,98) + a small number to prevent division by zero.
           */
         def oFiFun(_3dpg:_3DpointGenotype):Double =
         {  1 / (sqrt( pow( 4d - _3dpg._1, 2d ) + pow( 5d  - _3dpg._2, 2d ) + pow(98d - _3dpg._3, 2d) ) + 0.0001)
         }

         val SUSfor3dpg = new SUS[_3DpointGenotype]
         info("SUSfor3dpg variable: " + SUSfor3dpg)
         val test_pop = List(
            new _3Dpoint(1,2,3),
            new _3Dpoint(3,5,90),
            new _3Dpoint(6,5,100),
            new _3Dpoint(4,5,97)
         )
         info("Testing POP: " + test_pop)
         val selFiFun:LocalSelectiveFitnessFunction[_3DpointGenotype] = new LocalSelectiveFitnessFunction[_3DpointGenotype]
         {  override def apply(gt:_3DpointGenotype):Option[Double] =
         {  Some(oFiFun(gt))
         }
         }

         info("Final output: " + SUSfor3dpg(test_pop, selFiFun))
      }
      }
   }

}