package scalaTest

import org.ocbkc.ocevohut.Types.FitnessFunctionType
import org.ocbkc.ocevohut.{CreateSigmaScaledFitnessFunction, Individual}
import org.scalatest.{GivenWhenThen, FlatSpec}
import scala.math._

/**
 * Created by koen on 1-3-15.
 */
class SigmaScalingTesting extends FlatSpec with GivenWhenThen
{ "Sigma Scaling" must " be correctly runned" in
  {
     class TestStrings(inword: String) extends Individual[String]
     {  if(inword.toList.count(_.isInstanceOf[Char]) != 11)
        {  throw new IllegalStateException("This word: " + inword + " has not the length of 11")
        }
        val genotype = inword
        override def toString: String = "[String = " + inword + " ]"
     }

     def getIntByChar(cs: Char): Int =
     {  val listAlfa:List[(Char, Int)]  = "abcdefghijklmnopqrstuvwxyz ".toUpperCase.toList.zipWithIndex
        listAlfa.find(e => e._1 == cs).get._2+1
     }

     def genIntList(exL: List[Char] = List()): List[Int] =
     {  exL match {
           case Nil => Nil
           case head::tail => getIntByChar(head) :: genIntList(tail)
        }
     }

     /** This function returns a percentage between 0 .. 1 of how much a input word comes close to the target word (Hello World)
       *  So if the word: Hello World is in alphabetical number: List(8, 5, 12, 12, 15, 27, 23, 15, 18, 12, 4),
       *  and another word: Alloa Earth is:  List(1, 12, 12, 15, 1, 27, 5, 1, 18, 20, 8)
       *  the percentage that Alloa Earth matches Hello World is 0.2727272727272727, so about 27 percent.

      */
     def oFiFun(word: String):Double =
     {  val intList = genIntList(word.toUpperCase.toList)
        val target: List[Int] = List(8, 5, 12, 12, 15, 27, 23, 15, 18, 12, 4) //"the perfect fitness " or target => text: Hello World in uppercase
        val zippedList = intList zip target
        val compare = zippedList.map(item => item._1 == item._2)
        val countGoodOnes = compare count(_ == true)
        (countGoodOnes * 100d / 11d ) / 100d //11 comes from 11 elements of 'the one', see perfect above
     }

     val SigmaforString = new CreateSigmaScaledFitnessFunction[String]

     val test_pop = List(
        new TestStrings("Hallo Werel"),
        new TestStrings("Allo Walrod"),
        new TestStrings("Kiplo Stakl"),
        new TestStrings("Hello World"),
        new TestStrings("VisseEnzovr")
     )
     info("Test Populuation: " + test_pop)

     val globalSelFunc:FitnessFunctionType[String] = new FitnessFunctionType[String]
     {  override def apply(gt:String):Double =
        {  oFiFun(gt)
        }
     }

     info("Final output: " + SigmaforString(test_pop, globalSelFunc).apply("Hallo World")) //output:

  }
}


