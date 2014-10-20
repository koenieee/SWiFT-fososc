package org.ocbkc.swift

import scala.util.Random
import org.ocbkc.generic._
import org.ocbkc.generic.random._
import scala.math._


package object ocevohut
{  val ranSeq = new Random
}

package ocevohut
{

object Types
{  type FitnessFunctionType[Genotype__TP] = Genotype__TP => Double
}

import Types._

/** 
  * For testing purposes
  */
class Main
{  def main(args:Array[String])
   {  object Types
      {  type _3DpointGenotype = (Int, Int, Int)
      }

      import Types._

      class _3Dpoint(x:Int, y:Int, z:Int) extends Individual[_3DpointGenotype]
      {  val genotype = (x,y,z)
      }

      /** Fitness is the distance to the optimal point at (4,5,98)
        */
      def oFiFun(_3dpg:_3DpointGenotype):Double =
      {  sqrt( pow( 4d - _3dpg._1, 2d ) + pow( 5d  - _3dpg._2, 2d ) + pow(98d - _3dpg._3, 2d) )
      }
      
      val SUSfor3dpg = new SUS[_3DpointGenotype]

      val test_pop = List(
                        new _3Dpoint(1,2,3),
                        new _3Dpoint(3,5,90),
                        new _3Dpoint(6,5,80)
                        )

      SUSfor3dpg(test_pop


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

trait CreateScaledFitnessFunctionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]]):LocalSelectiveFitnessFunction[Genotype__TP]
}

trait LocalSelectiveFitnessFunction[Genotype__TP]
{  val map:Map[Genotype__TP, Double]
   def apply(gt:Genotype__TP):Option[Double] =
   {  map.get(gt)
   }
}

class CreateSigmaScaledFitnessFunction[Genotype__TP] extends CreateScaledFitnessFunctionTrait[Genotype__TP]
{  override def apply(pop:List[Individual[Genotype__TP]]):LocalSelectiveFitnessFunction[Genotype__TP] =
   {  null // TODO
   }
}


/** @todo perhaps more elegant to also allow global selective fitness functions, for that redesign of LocalSelectiveFitnessFunction is needed (it should have the same type as a global one) - perhaps using (possibly) partially defined functions.
  */
trait ProportionalSelectionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]], selFiFun:LocalSelectiveFitnessFunction[Genotype__TP]):Map[Individual[Genotype__TP], Int]
}

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
      {  val fitnessI:Double        = selFiFun(i.genotype)
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
