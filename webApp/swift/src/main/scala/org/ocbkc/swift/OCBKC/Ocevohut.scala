object Types
{  type SeletiveFitnessFunctionType[Individual__TP] = Individual__TP => Double
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

trait ProportionalSelectionTrait[Individual__TP]
{  def apply(List[Individual__TP], ProportionalSelection[Individual__TP]):Map[Individual__TP, Int]
}

object SUS extends ProportionalSelectionTrait
{  def apply(List[Individual__TP], ProportionalSelection[Individual__TP]):Map[Individual__TP, Int]
   {  
   }
}
