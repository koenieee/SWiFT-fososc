
/**
  * @todo: provide a method which produces different alternative translations into NL, for example 
           "Akwasi is big and fast."
           or
           "Akwasi is big.
            Akwasi is fast."
  */

object Translation
{  /**   Straight, because it tries to keep the original organisation of the CTL sentences in tact. E.g.
     *
            B(akwasi)
            F(akwasi)

         will be translated in 
       
            "Akwasi is big.
             Akwasi is fast."

         and NOT in

            "Akwasi is big and fast."
   */
   def FOltheory2NL_straight(ft:FOltheory, bs:BridgeDoc) =
   {  ft.stats.map
      {  predicateApp2NL(_, bs)
      }.collect{ case(Some(NLsent)) => NLsent }
   }

   def predicateApp2NL(predapp:PredApp, bs:BridgeDoc):Option[String] =
   {  predapp match
      {  case PredApp_FOL(p@Predicate(pname, arity), List(constant)) =>
         {  if(arity == 1)
            {  bs.constant2NLnoun(c) ++ " is " ++  bs.predicate2NLAdjective(p) ++ "."
            }
            else
               None
         }
      }
   }
}
