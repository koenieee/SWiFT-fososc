package org.ocbkc.swift.ocevohut 

object Types
{  type SelectiveFitnessFunctionType[Genotype__TP] = Genotype__TP => Double
}

/** @param populationSize size of the population in iterations > 0
  */

trait OcevohutTrait[Individual__TP]
{  var Pop: List[Individual__TP]
   var iteration: Int = 0
   val populationSize:Int
   val numberOfClones:Int
   val numberOfChildren = populationSize - cloneSize

   /** @returns a Map with items (Individual__TP, number_of_children assigned to this parent)
     */
   def selectParents:Map[Individual__TP, Int] =
   {
   }

   def selectClones:List[Individual__TP] =
   {
   }
   
   def SUS =
   {  
   }
}

trait ProportionalSelectionTrait[Genotype__TP]
{  def apply(pop:List[Individual[Genotype__TP]], selFiFun:SelectiveFitnessFunctionType[Genotype__TP]):Map[Individual[Genotype__TP], Int]
}

object SUS extends ProportionalSelectionTrait[Genotype__TP]
{  override def apply(pop:List[Individual[Genotype__TP]], selFiFun:SelectiveFitnessFunctionType[Genotype__TP]):Map[Individual[Genotype__TP], Int]
   {  val n = pop.count
      val totalFitness = pop.map{ i => selFiFun(i.genotype) }.foldLeft(0)(+)
      val averageFitness = totalFitness/n
      val randomShift = RandomExtras(rs, 0, averageFitness)

      /** length = 2
          offset = 0.5
          gridlenght = 1

          val gridFits = ( length - offset )/gridlength 
          { if(round(gridFits) == gridFits) gridFits else (gridFits + 1) }

         val passOffset = gridLength - ( ( length - offset ) % gridLength )
         */

   }
}

trait Individual[Genotype__TP]
{  val genotype Genotype__TP
}
