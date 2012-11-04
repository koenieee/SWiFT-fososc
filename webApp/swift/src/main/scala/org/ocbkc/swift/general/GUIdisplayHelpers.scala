package org.ocbkc.swift.general
{
object GUIdisplayHelpers
{  def optionToUI(opt:Option[Any])(implicit displayNoneAs:String):String =
   {  opt match
      {  case None         => displayNoneAs
         case Some(thing)  => thing.toString
      }
   }
}
}
