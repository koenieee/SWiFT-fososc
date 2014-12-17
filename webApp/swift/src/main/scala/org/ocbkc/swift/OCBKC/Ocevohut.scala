//wiw &y2014.10.22.01:29:21& current code compiles, now test by writing test driver, made preps in [swift git repo root]/test/ocevohut, but realised that mc is perhaps not needed: just make use of the classes already compiled by mvn.

package org.ocbkc

import scala.util.Random
import org.ocbkc.generic._
import org.ocbkc.generic.random._
import scala.math._
import org.ocbkc.swift.global.Logging._

package object ocevohut
{  val ranSeq = new Random
}

package ocevohut
{

/** Genotype__TP represents the genotype of the individuals of the population.
  */
object Types
{  type FitnessFunctionType[Genotype__TP] = Genotype__TP => Double
}

import Types._

/** 
  * For testing purposes
  */
object MainTest
{  def main(args:Array[String])
   {  object Types
      {  type _3DpointGenotype = (Int, Int, Int)
      }

      import Types._

      // The individuals of the population are 3D points in space
      class _3Dpoint(x:Int, y:Int, z:Int) extends Individual[_3DpointGenotype]
      {  val genotype = (x,y,z)
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

      // Test how SUS works on test_pop, and the given oFiFun (the latter of which is wrapped in selFiFun).
      println("" + SUSfor3dpg(test_pop, selFiFun))
   }
}

/** @param populationSize size of the population in iterations > 0
  * @param oFiFun objective fitness function
  */

trait OcevohutTrait[Genotype__TP]
{  type IndividualType = Individual[Genotype__TP]

   var Pop: List[IndividualType]
   var iteration: Int = 0
   val populationSize:Int
   val numberOfClones:Int
   val numberOfChildren = populationSize - numberOfClones
   
   def oFiFun(g:Genotype__TP):Double = 0d // TODO
   
   /** @returns a Map with items (Individual__TP, number_of_children assigned to this parent)
     */
   def selectParents:Map[IndividualType, Int] =
   {  // TODO
      null
   }

   def selectClones:List[IndividualType] =
   {  // TODO
      null
   }
}

/** Creates a CreateScaledFitnessFunctionTrait based on a population pop, and a globalFitnessFunction.
  */

trait CreateScaledFitnessFunctionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]], globalFitnessFunction:FitnessFunctionType[Genotype__TP]):LocalSelectiveFitnessFunction[Genotype__TP]
}

trait MapBasedLocalSelectiveFitnessFunction[Genotype__TP] extends LocalSelectiveFitnessFunction[Genotype__TP]
{  val map:Map[Genotype__TP, Double]
   override def apply(gt:Genotype__TP):Option[Double] =
   {  map.get(gt)
   }
}

trait LocalSelectiveFitnessFunction[Genotype__TP]
{  def apply(gt:Genotype__TP):Option[Double]
}

/** 
  */
class CreateSigmaScaledFitnessFunction[Genotype__TP] extends CreateScaledFitnessFunctionTrait[Genotype__TP]
{  override def apply(pop:List[Individual[Genotype__TP]], globalFitnessFunction:FitnessFunctionType[Genotype__TP]):LocalSelectiveFitnessFunction[Genotype__TP] =
   {  null // TODO
   }
}


/** @todo perhaps more elegant to also allow global selective fitness functions, for that redesign of LocalSelectiveFitnessFunction is needed (it should have the same type as a global one) - perhaps using (possibly) partially defined functions.
  */
trait ProportionalSelectionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]], selFiFun:LocalSelectiveFitnessFunction[Genotype__TP]):Map[Individual[Genotype__TP], Int]
}

/** Given a LocalSelectiveFitnessFunction, this assigns children to parents according to Stochatic Universal Sampling (SUS). The number of children assigned is equal to population size of parameter pop.
@todo there is a bug in here, the assigned children are sometimes less, sometimes more than the population size parameter pop.
  */

class SUS[Genotype__TP] extends ProportionalSelectionTrait[Genotype__TP]
{  override def apply(pop:List[Individual[Genotype__TP]], selFiFun:LocalSelectiveFitnessFunction[Genotype__TP]):Map[Individual[Genotype__TP], Int] =
   {  val n:Int = pop.size
      val totalFitness:Double    = pop.map{ i => selFiFun(i.genotype).get }.fold(0d){(a:Double,b:Double)=>a+b}
      val averageFitness:Double  = totalFitness/n
      val gridLength:Double      = averageFitness // just a synonym, that is same extension, however, different intension (Rudolf Carnap, the morning star is the evening star" - ;-) )
      val randomShift:Double     = RandomExtras.nextBetween(ranSeq, 0d, averageFitness)

      /** Some notes, with an example calculation:

          length = 2
          offset = 0.5
          gridlenght = 1

          val gridFits = ( length - offset )/gridlength 
          { if(round(gridFits) == gridFits) gridFits else (gridFits + 1) }

         val passOffset = gridLength - ( ( length - offset ) % gridLength )
         */
      def calculateNumberOfChildrenAndPassOffset(i:Individual[Genotype__TP], offset:Double):((Individual[Genotype__TP], Int), Double) =
      {  val fitnessI:Double        = selFiFun(i.genotype).getOrElse(logAndThrow("Hey, dude, you gave me a local selFiFun that isn't defined on member " + i + " of this population... Ain't not smart, youknow..."))
         val gridFits:Double        = ( fitnessI - offset )/gridLength
         val numberOfChildren:Int   = ( if(floor(gridFits) == gridFits) gridFits else (gridFits + 1) ).toInt
         val passOffset:Double      = gridLength - ( ( gridLength - offset ) % gridLength )

         ((i, numberOfChildren), passOffset)
      }

      ListUtils.mapWithLeftContext(pop, randomShift, calculateNumberOfChildrenAndPassOffset).toMap
   }
}

trait Individual[Genotype__TP]
{  val genotype:Genotype__TP
}
}
