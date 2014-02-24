package org.ocbkc.swift.natlang.generation

object NLgen
{  def commaAndList(entities:List[String]):String =
   {  entities match
      {  case e1::Nil      => e1
         case e1::e2::Nil  => e1 ++ " and " ++ e2
         case e1::rest     => e1 ++ ", " ++ commaAndList(rest)
         case Nil          => ""
      }
   }
}
