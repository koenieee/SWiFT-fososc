package org.ocbkc.swift.scalaTest

import org.ocbkc.ocevohut.{LocalSelectiveFitnessFunction, Individual, SUS}
import org.scalatest._

import scala.math._

/**
 * Created by koen on 17-12-14.
 */
class OcevoHutTesting extends FlatSpec with GivenWhenThen {
"" must " give a correct output" in {

   /**
    * For testing purposes
    */
   object Types
   {  type _3DpointGenotype = (Int, Int, Int)
   }

   import Types._

   // The individuals of the population are 3D points in space
   class _3Dpoint(x:Int, y:Int, z:Int) extends Individual[_3DpointGenotype]
   {  val genotype = (x,y,z)
      override def toString: String = "[3DPoint x="+x+" y="+y+" z="+z+"]" //added this one, to see in this testing case also the output of 3D point (instead of memory addresses)
   }

   /** Fitness is the inverse of the distance to the optimal point at (4,5,98) + a small number to prevent division by zero.
     */
   def oFiFun(_3dpg:_3DpointGenotype):Double =
   {  1 / (sqrt( pow( 4d - _3dpg._1, 2d ) + pow( 5d  - _3dpg._2, 2d ) + pow(98d - _3dpg._3, 2d) ) + 0.0001)
   }

   val SUSfor3dpg = new SUS[_3DpointGenotype]

   val test_pop = List(
      new _3Dpoint(1,2,3),
      new _3Dpoint(3,5,90),
      new _3Dpoint(6,5,100),
      new _3Dpoint(4,5,97)
   )

   // define the local selective fitness function to be equal to oFiFun. (so, in fact, it is not local).
   val selFiFun:LocalSelectiveFitnessFunction[_3DpointGenotype] = new LocalSelectiveFitnessFunction[_3DpointGenotype]
   {  override def apply(gt:_3DpointGenotype):Option[Double] =
   {  Some(oFiFun(gt))
   }
   }

   val numberOfPop = 5
   val outputofSUS = SUSfor3dpg(test_pop, selFiFun, numberOfPop)
   val totalOfPop = outputofSUS.map(indi => indi._2).sum

   // Test how SUS works on test_pop, and the given oFiFun (the latter of which is wrapped in selFiFun).
      info("Final output: " + outputofSUS)
      info("Number of linked children: " + totalOfPop)
      assert(numberOfPop == totalOfPop)
   }
}