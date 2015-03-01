//wiw &y2014.10.22.01:29:21& current code compiles, now test by writing test driver, made preps in [swift git repo root]/test/ocevohut, but realised that mc is perhaps not needed: just make use of the classes already compiled by mvn.


package org.ocbkc

import scala.util.Random
import org.ocbkc.generic._
import org.ocbkc.generic.random._

import org.ocbkc.swift.global.Logging._

package object ocevohut
{  val ranSeq = new Random
}

package ocevohut
{


import org.ocbkc.ocevohut.Types.FitnessFunctionType

/** Genotype__TP represents the genotype of the individuals of the population.


This package provides is a framework to create ANY evolutionary computation system. So it does not come with predefined fitness function definitions, or a predefined genotype space (therefore the type parameter). It is library, which allows you to define those yourself, and then let the library do the rest of the work, such as fitness function scaling, sampling, and evolutionary algorithm execution. You simply have to provide the fitness function definitions, and make a selection of available scaling/sampling methods.

@param Genotype__TP represents the genotype of the individuals of the population.
  */
object Types
{  type FitnessFunctionType[Genotype__TP] = Genotype__TP => Double
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

/** Use this trait to make a creator of a scaled fitness function based on a population pop, and a globalFitnessFunction. So, extend it and override apply.
  */

trait CreateScaledFitnessFunctionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]], globalFitnessFunction:FitnessFunctionType[Genotype__TP], constant: Double):LocalSelectiveFitnessFunction[Genotype__TP]
}

trait MapBasedLocalSelectiveFitnessFunctiongridLength[Genotype__TP] extends LocalSelectiveFitnessFunction[Genotype__TP]
{  val map:Map[Genotype__TP, Double]
   override def apply(gt:Genotype__TP):Option[Double] =
   {  map.get(gt)
   }
}

/** LocalSelectiveFitnessFunction represents a local selective fitness function. Create your own local selective fitness function by extending this class and overriding the apply method. The apply *may* be partially defined, and does not have to range over the complete Genetype__TP domain (no wonder, it is a *local* selective function). Let apply return None for all points in the genotype space where it is undefined.
 */

trait LocalSelectiveFitnessFunction[Genotype__TP]
{  def apply(gt:Genotype__TP):Option[Double]
}

/** 
  */
class CreateSigmaScaledFitnessFunction[Genotype__TP] extends CreateScaledFitnessFunctionTrait[Genotype__TP]
{  override def apply(pop:List[Individual[Genotype__TP]], globalFitnessFunction:FitnessFunctionType[Genotype__TP], constant: Double = 0.32):LocalSelectiveFitnessFunction[Genotype__TP] =
   {  val resultingMultiSet: List[Double]   = pop.map( elementP => globalFitnessFunction(elementP.genotype) )
      val sampleStndDeviation               = sampleStandardDeviation(resultingMultiSet) * constant
      val sampleMean: Double                = resultingMultiSet.sum / resultingMultiSet.size
      val betweenBrackets                   = sampleMean - sampleStndDeviation

      new LocalSelectiveFitnessFunction[Genotype__TP]()
      {  override def apply(gt: Genotype__TP): Option[Double]=
         {  Some( globalFitnessFunction(gt) - betweenBrackets )
         }
      }
   }

   def sampleStandardDeviation(ls: List[Double]): Double =
   {  ls match
      {  case Nil  => 0.0
         case list =>
         {  val average: Double       = list.sum / list.size
            val newList: List[Double] = list.map(inNumber => math.pow( inNumber - average, 2 ))
            val sumList: Double       = newList.sum
            sumList / list.size
         }
      }
   }
}


/** @todo perhaps more elegant to also allow global selective fitness functions, for that redesign of LocalSelectiveFitnessFunction is needed (it should have the same type as a global one) - perhaps using (possibly) partially defined functions.
@returns A map, in which each pair (i, number_of_children) consists of an individual i with a particular genotype (of type Genotype__TP), and the number of children (nochild) that individual i will produce. So, if number_of_children = 4, then individual i will contribute 4 children to the next generation. Note that there may be MORE than one individual with the same genotype!
  */
trait ProportionalSelectionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]], selFiFun:LocalSelectiveFitnessFunction[Genotype__TP], n: Int):Map[Individual[Genotype__TP], Int]
}

/** Given a LocalSelectiveFitnessFunction, this assigns children to parents according to Stochatic Universal Sampling (SUS). The number of children assigned is equal to population size of parameter pop.
@todo there is a bug in here, the assigned children are sometimes less, sometimes more than the population size parameter pop.
  */

class SUS[Genotype__TP] extends ProportionalSelectionTrait[Genotype__TP]
{  override def apply(pop:List[Individual[Genotype__TP]], selFiFun:LocalSelectiveFitnessFunction[Genotype__TP], n: Int):Map[Individual[Genotype__TP], Int] =
   {  //val n:Int = pop.size //changed as parameter
      val totalFitness:Double    = pop.map{ i => selFiFun(i.genotype).get }.fold(0d){(a:Double,b:Double)=>a+b}
      val averageFitness:Double  = totalFitness/n
      val gridLength:Double      = averageFitness // just a synonym, that is same extension, however, different intension (Rudolf Carnap, the morning star is the evening star" - ;-) )
      val randomShift:Double     = RandomExtras.nextBetween(ranSeq, 0d, averageFitness)

      /** Some notes, with an example calculation for one iteration (determining number of children for a given individual i):
          
          fitness_i     = 2
          offset        = 0.5
          gridlength    = 1

          // Attempt 1 (wrong)
          {

          val gridFits = ( fitness_i - offset )/gridlength 
          { if(floor(gridFits) == gridFits) gridFits else (gridFits + 1) }
         
            val passOffset = gridLength - ( ( length - offset ) % gridLength )
          }
          // Attempt 2m (sketch)

         
          (totalOffspring passOffset) = 
            { if offset less_than fitness_i: 
               {  gridfits = (fitness_i - offset) / gridLength;
                  to = if( isWholeNumber(gridfits) ) ( gridfits ) else floor(gridfits) + 1.
                  (to, gridLength - ( ( fitness_i - offset ) % gridLength ) )
               }
               else (0, offset - fitness_i)
            }            

          def isWholeNumber(d:double) = floor(d) == d
          
          Explanation: the special cases are the ones where the spokes coincide with the boundary between two area's of the roulette wheel (see thesis CG). In this case the spoke is considered to be hovering over the RIGHT area (not the left).
            val passOffset = gridLength - ( ( length - offset ) % gridLength )

         */

      def calculateNumberOfChildrenAndPassOffset(i:Individual[Genotype__TP], offset:Double):((Individual[Genotype__TP], Int), Double) =
      {  val fitnessI:Double           = selFiFun(i.genotype).getOrElse(logAndThrow("Hey, dude, you gave me a local selFiFun that isn't defined on member " + i + " of this population... Ain't not smart, youknow..."))

         if(offset < fitnessI)
         {  val gridFits:Double        = ( fitnessI - offset )/gridLength
            val numberOfChildren:Int   = ( if(isWholeNumber(gridFits)) gridFits else math.floor(gridFits) + 1 ).toInt
            val passOffset:Double      = gridLength - ( ( fitnessI - offset ) % gridLength )
            ((i, numberOfChildren), passOffset)
         }
         else
         {  ((i, offset.toInt), (offset - fitnessI))
         }
      }

      def isWholeNumber(d:Double):Boolean = math.floor(d) == d

      ListUtils.mapWithLeftContext(pop, randomShift, calculateNumberOfChildrenAndPassOffset).toMap
   }
}

trait Individual[Genotype__TP]
{  val genotype:Genotype__TP
}
}
