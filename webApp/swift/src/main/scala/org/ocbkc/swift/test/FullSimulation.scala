class SimulatedPlayer
{  val CurrentState =
   var Transition = Map[PlayerState, RandomPlayerState]
}


/** assumption is that if probabilities do not sum up to 100%, there is no transition (or transition to the original state)
  */
class ToRandomPlayerStates(states:Map[State, Probability])
{  if( states.map{ _._2 }.foldLeft(0)(+) > 1) throw new RuntimeException("ToRandomPlayerStates must have probabilities which sum up to 1 at most")
}
