package org.ocbkc.swift

import scala.util.Random
import org.ocbkc.generic._
import org.ocbkc.generic.random._

package object ocevohut
{  val ranSeq = new Random
}

package ocevohut
{
import scala.math._

object Types
{  type SelectiveFitnessFunctionType[Genotype__TP] = Genotype__TP => Double
}

import Types._

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
   

   val oFiFun = null // TODO

   /** @returns a Map with items (Individual__TP, number_of_children assigned to this parent)
     */
   def selectParents:Map[IndividualType, Int] =
   {  // TODO
      null
   }

   def selectClones:List[Individual__TP] =
   {  // TODO
      null
   }
}

trait CreateScaledFitnessFunctionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]]):LocalSelectiveFitnessFunction[Genotype__TP]
}

class LocalSelectiveFitnessFunction[Genotype__TP]
{  val map:Map[Genotype__TP, Double]
   def apply(gt:Genotype__TP):Option[Double]
   {  map(gt)
   }
}

class CreateSigmaScaledFitnessFunction{Genotype__TP] extends CreateScaledFitnessFunctionTrait[Genotype__TP]
{  override def apply(pop:List[Individual[Genotype__TP]]):LocalSelectiveFitnessFunction[Genotype__TP] =
   {  null // TODO
   }
}

trait ProportionalSelectionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]], selFiFun:SelectiveFitnessFunctionType[Genotype__TP]):Map[Individual[Genotype__TP], Int]
}

class SUS[Genotype__TP] extends ProportionalSelectionTrait[Genotype__TP]
{  override def apply(pop:List[Individual[Genotype__TP]], selFiFun:SelectiveFitnessFunctionType[Genotype__TP]):Map[Individual[Genotype__TP], Int] =
   {  val n:Int = pop.size
      val totalFitness:Double    = pop.map{ i => selFiFun(i.genotype) }.foldLeft(0d)(_+_)
      val averageFitness:Double  = totalFitness/n
      val gridLength:Double      = averageFitness // just a synonym, that is same extension, however, different intension (Rudolf Carnap, the morning star is the evening star" - ;-) )
      val randomShift:Double     = RandomExtras.nextBetween(ranSeq, 0d, averageFitness)

      /** length = 2
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
