package scalaTest

import org.ocbkc.ocevohut.Types.FitnessFunctionType
import org.ocbkc.ocevohut.{CreateSigmaScaledFitnessFunction, Individual}
import org.scalatest.{GivenWhenThen, FlatSpec}
import scala.math._

/**
 * Created by koen on 1-3-15.
 */
class SigmaScalingTesting extends FlatSpec with GivenWhenThen
{ "Sigma Scaling" must " be correctly runned" in
  {

     object Types
     {  type _3DpointGenotype = (Int, Int, Int)
     }

     import Types._

     // The individuals of the population are 3D points in space
     class _3Dpoint(x:Int, y:Int, z:Int) extends Individual[_3DpointGenotype]
     {  val genotype = (x,y,z)
        override def toString: String = "[3DPoint x="+x+" y="+y+" z="+z+"]" //added this one, to see in this testing case also the output of 3D point (instead of memory addresses)
     }

     def oFiFun(_3dpg:_3DpointGenotype):Double =
     {  1 / (sqrt( pow( 4d - _3dpg._1, 2d ) + pow( 5d  - _3dpg._2, 2d ) + pow(98d - _3dpg._3, 2d) ) + 0.0001)
     }

     val Sigmafor3dpg = new CreateSigmaScaledFitnessFunction[_3DpointGenotype]

     val test_pop = List(
        new _3Dpoint(1,2,3),
        new _3Dpoint(3,5,90),
        new _3Dpoint(6,5,100),
        new _3Dpoint(4,5,97)
     )

     val globalSelFunc:FitnessFunctionType[_3DpointGenotype] = new FitnessFunctionType[_3DpointGenotype]
     {  override def apply(gt:_3DpointGenotype):Double =
     {  oFiFun(gt)
     }
     }

     info("Final output: " + Sigmafor3dpg(test_pop, globalSelFunc))
  }
}


