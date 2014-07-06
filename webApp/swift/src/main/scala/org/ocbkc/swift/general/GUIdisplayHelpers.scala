package org.ocbkc.swift.general
{
object GUIdisplayHelpers
{  def optionToUI(opt:Option[Any]):String =
   {  opt match
      {  case None         => ""
         case Some(thing)  => thing.toString
      }
   }
}
}
