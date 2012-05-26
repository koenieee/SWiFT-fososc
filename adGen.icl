module adGen
/* Algorithmic Defence Generator API, reads arguments to offer function from file. Expects:

input:
filename: adGen.clean.in
with arguments of type AlgorithmicDefenceGenerator, seperated by exactly 1 newline each

output:
filename: adGen.clean.out

*/

import genToFromString, StdGeneric, StdEnv, gast, GenericByCG, textgenerator_version2 

// Output of Start is identical to the output written to the file AlgorithmicDefenceGenerator.clean.out, to simplify logging. If the output becomes too large in the future, perhaps trim it or don't return it at all.
// <&y2011.12.16.10:57:50& how to write a good exit value to the output (according to unix standards) or am I doing it correctly?>
Start::*World->(String, World*)
Start world = (outputFuncApp, world3)
where
   world3 = OpenFileAndWriteString "adGen.clean.out" outputFuncApp world2
   (outputFuncApp, world2) = OpenFileAndApplyFunction2 "adGen.clean.in" AlgorithmicDefenceGeneratorAPI world

/* 
<&y2011.12.21.12:13:18& A problem: should I handle syntactical errors in the input file with the abort mechanism (side effect) or should it be output of the function? Pro's and cons:
- pro: syntactical errors in the input are NOT bugs - because a player may make errors.
- cons: error as output of function violates the principle that I want to strictly separate errors from the interprogram interface and errors on the function level.>

[&y2011.12.21.12:16:07& Solution could be: do first a separate syntax check with a separate external function. However, this is a bit unwieldy, you already do the parsing here...]
[&y2011.12.21.12:17:31& Still, I need a separate parser, because also before creating the algo ddefence etc. I need to give the player feedback about the correctness of his translation!]
[DD &y2011.12.21.12:34:15& For now, I go with the last options! This implies that the AlgorithmicDefenceGeneratorAPI just aborts when a syntax error is encountered, I.e. it assumes the syntax to be correct already.]
*/

AlgorithmicDefenceGeneratorAPI::File -> String
AlgorithmicDefenceGeneratorAPI file = (export2QuestionLangScalaFormat (AlgorithmicDefenceGenerator bridgestatsComputer question bridgestatsPlayer))
where
   bridgestatsPlayer
      # (num, maybe) = gFromString {|*|} 0 bridgestatsPlayerStr
      | isJust maybe = fromJust maybe
                     = abort("Error, bridgestatsPlayer could not be parsed\n")

   bridgestatsComputer 
      # (num, maybe) = gFromString {|*|} 0 bridgestatsComputerStr
      | isJust maybe = fromJust maybe
                     = abort("Error, bridgestatsComputer could not be parsed\n")

   question
      # (num, maybe) = gFromString {|*|} 0 questionStr
      | isJust maybe = fromJust maybe
                     = abort("Error, question could not be parsed\n")

   (bridgestatsComputerStr, filerest1)       = sfreadline file
   (questionStr, filerest2)               = sfreadline filerest1
   (bridgestatsPlayerStr, filerest3)         = sfreadline filerest2


// <&y2011.12.07.22:39:58& move to generic lib>

// If this function doesn't abort, it was succesfull. So no error handling is needed after calling this function.
OpenFileAndWriteString :: String String *env -> *env | FileSystem env
OpenFileAndWriteString filename filecontent filesys
   # (ok, file, filesys) = fopen filename FWriteText filesys
   | not ok              = abort("Cannot open output file: '" +++ filename +++ "'")
   # file               = fwrites filecontent file
   # (ok, filesys)      = fclose file filesys
   | not ok             = abort("Cannot close output file: '" +++ filename +++ "'")
                        = filesys

derive gToString HurelanStat_, EntityStat_, BridgeStat_, PatternLangFolmuminquaNumQuant_, SharpestPatStat_, NumRestrPat_, Literal_, Comparison_, FHterm_, (,,,)
derive gFromString HurelanStat_, EntityStat_, BridgeStat_, PatternLangFolmuminquaNumQuant_, SharpestPatStat_, NumRestrPat_, Literal_, Comparison_, FHterm_, (,,,)
derive bimap HurelanStat_, EntityStat_, BridgeStat_, PatternLangFolmuminquaNumQuant_, SharpestPatStat_,  NumRestrPat_, Literal_, Comparison_, FHterm_, (,,,)
