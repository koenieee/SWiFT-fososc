/*
Part of SWiFT game (c) Chide Groenouwe



### CONVENTIONS
None so far

*/

package org.ocbkc.swift.parser
{
import scala.collection.JavaConversions._
import scala.util.parsing.combinator._
import System._
import java.io._
import org.ocbkc.swift.logilang._
import org.ocbkc.swift.test.CLIwithFileInput

object Folminqua2FOLtheoryParserCLI extends CLIwithFileInput
{  def main(args: Array[String])
   {  applyFunctionToFile(Folminqua2FOLtheoryParser.parseAll(Folminqua2FOLtheoryParser.folminquaTheory, _).toString, args(0))
   }
}

object ConstantSubtitutionCLI extends CLIwithFileInput
{  def main(args: Array[String])
   {  def f(folminqua:String):String =
      {  val ft:FOLtheory = Folminqua2FOLtheoryParser.parseAll(Folminqua2FOLtheoryParser.folminquaTheory, folminqua) match
         {  case Folminqua2FOLtheoryParser.Success(ftl,_)  => ftl
            case  failMsg@Folminqua2FOLtheoryParser.Failure(_,_)           => return "  parse error: " + failMsg.toString
         }

         def getConstant(cn:String) = ft.getConstant(cn) match
         {  case None    => { println("Constant: " + cn + " not present in theory..."); null }
            case Some(c) => c
         }
     
         var ret:String = ""

         ret +=  "#### Original theory:\n" + ft +"\n\n"
   
         ret += "Constants:\n" + ft.constants + "\n"

         val c1:Constant = getConstant(args(0))
         val c2:Constant = getConstant(args(1))

         if(c1!=null && c2 != null) 
         {  ft.substituteConstant(c1, c2)
            ret += "#### Theory with substitutions:\n\n"
            ret += "Constants:\n" + ft.constants + "\n"
            ret += ft
         }

         ret
      }
      applyFunctionToFile(f, args(2))
   }
}

object FolminquaParserCLI extends CLIwithFileInput
{  def main(args: Array[String])
   {  applyFunctionToFile(FolminquaParser.parseAll(FolminquaParser.folminquaTheory, _).toString, args(0))
      /*FolminquaParser.parseAll(FolminquaParser.folminquaTheory, lines)) match
      {  case s@Success(_) => println(s)
         case f@Failure(_,_) => prinln(f)
      }*/
   }
}



/*
### INPUT: Folminqua in user syntax. Example:
P({Akwasi}, {John, Alexia})
inequal(Akwasi, John)


### OUTPUT: Folminqua in Clean data structure (after gToString). Example:
[(FHpre2stat (_Tuple2 [13]is_trainee_of (_Tuple2 [[11]Christopher] [[9]Anastasia,[6]Alexia,[7]Sirtaki,[4]Alex,[5]Frank]))),(FHeqstat (_Tuple2 Neq [[9]Anastasia,[6]Alexia,[7]Sirtaki,[4]Alex,[5]Frank]))]
[(FHpre2stat (_Tuple2 [3]pee (_Tuple2 [[2]aa] [[2]bb])))]
*/

class SWiFTparser extends JavaTokenParsers
{  def NL = "\r\n" | "\n"
   def space = """[\t\s]""".r // <repace with general space match symbol>
   def spaces = rep(space)
   override val skipWhitespace = false
}

/** The superclass containing the sub-parsers which can be reused in all parsers which belong to the "Alpha Group" - this is the group of parsers developed for the first languages for the SWiFT game, such as Folnuminqua and EfeLang, and related parsers such as the bridge parsers belonging to one of these languages. Parsers added to this class may not postprocess!
  */
class AlphaGroupParser extends SWiFTparser
{   def id: Parser[String]              = """[a-z][a-zA-Z0-9_\-]*""".r
}


//< &y2012.02.18.22:22:27& perhaps more elegant to define separate type for list of constants and also one for predicates. To give the type checking system of scala extra info to discover bugs.

case class FolminquaParseResult(var bridgeInClean:String, var constants:List[String], var predicates:List[String])

case class ConstantList(list:List[String])


/** Deprecated: use Folminqua2FOLtheoryParser
  */
class Folminqua extends AlphaGroupParser
{  import HelperFunctions._
 
   // For now: simplification of language because of incompleteness reasoner.
   // <&y2012.02.25.13:52:46& restore original definition after having made reasoner complete.>

   def folminquaTheory = 
      eqstat ~ NL ~ pre2stat ~ rep(NL) ^^ { case Tuple3(cleanformat1,consts1,preds1) ~ _ ~ Tuple3(cleanformat2, consts2, preds2) ~ _ => FolminquaParseResult(printList(List(cleanformat1,cleanformat2),"[",",","]"), (consts1 ++ consts2).distinct, (preds1 ++ preds2 ).distinct) } | 
      pre2stat ~ NL ~ eqstat ~ rep(NL) ^^ { case Tuple3(cleanformat1,consts1,preds1) ~ _ ~ Tuple3(cleanformat2, consts2, preds2) ~ _ => FolminquaParseResult(printList(List(cleanformat1,cleanformat2),"[",",","]"), (consts1 ++ consts2).distinct, (preds1 ++ preds2 ).distinct) } |
      pre2stat ~ rep(NL) ^^ { case Tuple3(cleanformat, constst, preds) ~ _ => FolminquaParseResult("[" + cleanformat + "]", constst, preds) } |
      eqstat ~ rep(NL)  ^^ { case Tuple3(cleanformat, constst, preds) ~ _ => FolminquaParseResult("[" + cleanformat + "]", constst, preds) }
   /*
   def folminquaTheory                 = repsep(folminquaSentence, NL) <~ rep(NL) ^^ (sentences => FolminquaParseResult(printList(sentences.map({ case (sentence,_,_)=>sentence}),"[",",","]"), sentences.map({case (_,constants,_)=>constants}).flatten.distinct, sentences.map({case (_,_,preds)=>preds}).flatten.distinct))
   */
   // def firstOf3[A,B,C](tuple3:(A,B,C)) = match first case { (first,_,_) }
   def folminquaSentence               = ( eqstat | pre2stat ) ^^ (x=>{println("debug folminquaSentence result: " + x); x})
   // <&y2012.02.26.01:54:47& solve error here:                           below should be at least one constant, not zero!>
   def eqstat                          = eqpre ~ "(" ~ constantId ~ "," ~ rep1sep(constantId, ",") ~ ")" ^^ { case eqpre ~ "(" ~ constant ~ "," ~ list ~ ")" => ("(FHeqstat (_Tuple2 " + (if (eqpre.equals("equal")) "Eq" else "Neq") + " " + printList((constant::list).map(addLength),"[",",","]") + "))", constant::list, Nil)}
   def eqpre                           = "equal" | "inequal"
   def pre2stat                      = preId ~ "(" ~ constantList ~ "," ~ constantList ~ ")" ^^ { case preId ~ "(" ~ set1 ~ "," ~ set2 ~ ")" => ("(FHpre2stat (_Tuple2 " + addLength(preId) + " (_Tuple2 " + printList(set1.map(addLength), "[", ",", "]") + " " + printList(set2.map(addLength),"[",",","]") + ")))", (set1 ++ set2).distinct, List(preId))}
   def constantList                    = (constantId ^^ ( x => List(x)) ) | ("{" ~> repsep(constantId,",")) <~ "}"
   def constantId                      = id//  ^^ (x => "[" + (x.length) + "]" + x)
   def preId                           = id//  ^^ (x => "[" + (x.length) + "]" + x)
   
   // <&y2012.01.22.21:12:32& exclude equal and inequal from preId>
   def addLength(id:String) = "[" + (id.length) + "]" + id
}


// convert folminqua doc to FOLtheory datastructure
class Folminqua2FOLtheoryParser extends AlphaGroupParser
{  import HelperFunctions._
   def folminquaTheory                 = 
      repsep(folminquaSentence, NL) <~ rep(NL) ^^ { case sentenceLists => // Be ware: folminquaSentence returns a list of FOLstat instances.
         val ft = new FOLtheory
         ft.addStats(sentenceLists.flatten) match
         {  case None => { println("folminquaTheory parser returned:\n" + ft); ft } // successful
            case Some(Tuple2(s,i)) => 
            {  val m = "Folminqua2FOLtheoryParser.folminquaTheory: my dear, dear beloved, friend, a fatal error occurred, couldn't add one or more statement(s) to scala FOLtheory class!"
               println(m)
               throw new RuntimeException(m)
            }
         }
      }
   
   // def firstOf3[A,B,C](tuple3:(A,B,C)) = match first case { (first,_,_) }
   def folminquaSentence             = ( eqstat | pre2stat ) // ^^ ( x => {println("debug folminquaSentence result: " + x); x} )
   // <&y2012.02.26.01:54:47& solve error here:                           below should be at least one constant, not zero!>
   def eqstat                      = eqpre ~ "(" ~ constantId ~ "," ~ rep1sep(constantId, ",") ~ ")" ^^ { 
      case eqpre ~ "(" ~ constant ~ "," ~ list ~ ")" => 
      {  val constantNames = constant::list
         val constants     = constantNames.map(cn => Constant(cn) )
         if( eqpre.equals("equal")) Equal.convertTo2place(constants)
         else  Unequal.convertTo2place(constants)
      }
   }
               
               // old { case eqpre ~ "(" ~ constant ~ "," ~ list ~ ")" => ("(FHeqstat (_Tuple2 " + (if (eqpre.equals("equal")) "Eq" else "Neq") + " " + printList((constant::list).map(addLength),"[",",","]") + "))", constant::list, Nil)}
   def eqpre                           = "equal" | "inequal"
   def pre2stat                       = preId ~ "(" ~ constantList ~ "," ~ constantList ~ ")" ^^ { 
      case preId ~ "(" ~ set1 ~ "," ~ set2 ~ ")" => 
      {  val pred = Predicate(preId, 2)
         val constantsSet1 = set1.map(cname => Constant(cname))
         val constantsSet2 = set2.map(cname => Constant(cname))
         FOLutils.pairsFrom2Lists(constantsSet1, constantsSet2).map(p => PredApp_FOL(pred, List(p._1, p._2)))
      }
   }

   // old: { case preId ~ "(" ~ set1 ~ "," ~ set2 ~ ")" => ("(FHpre2stat (_Tuple2 " + addLength(preId) + " (_Tuple2 " + printList(set1.map(addLength), "[", ",", "]") + " " + printList(set2.map(addLength),"[",",","]") + ")))", (set1 ++ set2).distinct, List(preId))}
   def constantList                    = (constantId ^^ ( x => List(x)) ) | ("{" ~> repsep(constantId,",")) <~ "}"
   def constantId                      = id//  ^^ (x => "[" + (x.length) + "]" + x)
   def preId                           = id//  ^^ (x => "[" + (x.length) + "]" + x)
   
   // <&y2012.01.22.21:12:32& exclude equal and inequal from preId>
   def addLength(id:String) = "[" + (id.length) + "]" + id
}

object Folminqua2FOLtheoryParser extends Folminqua2FOLtheoryParser
{
}

/*

hurelan(friend,is_friend_of,friend)
entity(c1,Akwasi)

[(HurelanStat (_HurelanStat_ [13]is_patient_of [7]patient [7]dentist)),(EntityStat (_EntityStat_ [8]Quadario [8]Quadario))
]

*/

class HurelanBridge extends AlphaGroupParser
{  // <&y2012.02.17.10:00:54& eat superfluous enters, etc.>
   def bridge        = repsep(bridgeStat,NL) <~ rep(NL) ^^ (x => HelperFunctions.printList(x, "[", ",", "]")) // Example output before transformer is applied: List( ("hurelanstat(...)", List("a","b","c"), List("p","q")) )
   def bridgeStat    = hurelanStat | entityStat
   def entityStat    = ((((("entity" ~ "(") ~> entityIdCTL) <~ ",") ~ entityNL) <~ ")") ^^ { case entityIdCTL ~ entityNL => "(EntityStat (_EntityStat_ " + entityIdCTL + " " + entityNL + "))" }
   def hurelanStat   = ((((((("hurelan" ~ "(") ~> firstPosInNL) <~ ",") ~ predicate) <~ ",") ~ secondPosInNL) <~ ")") ^^ { case first ~ pred ~ second => "(HurelanStat (_HurelanStat_ " + pred + " " + first + " " + second + "))"}
   def firstPosInNL  = wordNL ^^ (x => "[" + (x.length) + "]" + x)
   def predicate     = id ^^ (x => "[" + (x.length) + "]" + x)
   def secondPosInNL = wordNL ^^ (x => "[" + (x.length) + "]" + x)
   def entityNL      = wordNL ^^ (x => "[" + (x.length) + "]" + x)
   def entityIdCTL   = id ^^ (x => "[" + (x.length) + "]" + x)
   def wordNL        = wordNLregexStr.r
   def wordNLregexStr = """[a-zA-Z\-]+""" 
}

object HurelanBridge extends HurelanBridge

object FolminquaParser extends Folminqua

object HelperParsers extends JavaTokenParsers

object HelperFunctions
{  def printList(list:List[String], open:String, sep:String, close:String):String = list match 
      {  //case x::Nil => open + x + close
         case x::xs => open + x + xs.foldLeft("")((a,b) => a + sep + b) + close
         case Nil => ""
      }
}

}
