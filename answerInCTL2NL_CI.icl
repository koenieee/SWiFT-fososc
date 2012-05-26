module answerInCTL2NL_CI
/* Algorithmic Defence Generator API, reads arguments to offer function from file. Expects:

input:
filename: answerInCTL2NL.clean.in
with arguments of type answerInCTL2NL, seperated by exactly 1 newline each

output:
filename: answerInCTL2NL.clean.out

*/

import genToFromString, StdGeneric, StdEnv, gast, GenericByCG, textgenerator_version2 

// Output of Start is identical to the output written to the file answerInCTL2NL.clean.out, to simplify logging. If the output becomes too large in the future, perhaps trim it or don't return it at all.
// <&y2011.12.16.10:57:50& how to write a good exit value to the output (according to unix standards) or am I doing it correctly?>
Start::*World->(String, World*)
Start world = (outputFuncApp, world3)
where
   world3 = OpenFileAndWriteString "answerInCTL2NL_CI.clean.out" outputFuncApp world2
   (outputFuncApp, world2) = OpenFileAndApplyFunction2 "answerInCTL2NL_CI.clean.in" answerInCTL2NLAPI world

/* 

*/
// AnswerInCTL2NL::(AnswerLang_, [Hurelan_cfnl]) -> [SentenceNL_]

answerInCTL2NLAPI::File -> String
answerInCTL2NLAPI file = gToString {|*|} (AnswerInCTL2NL (answer, hurelans))
where
   answer
      # (num, maybe) = gFromString {|*|} 0 answerStr
      | isJust maybe = fromJust maybe
                     = abort("Error, answer could not be parsed\n")

   hurelans
      # (num, maybe) = gFromString {|*|} 0 hurelansStr
      | isJust maybe = fromJust maybe
                     = abort("Error, hurelans could not be parsed\n")

   (answerStr, filerest1)      = sfreadline file
   (hurelansStr, filerest2)   = sfreadline filerest1

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

derive gToString HurelanStat_, EntityStat_, BridgeStat_, PatternLangFolmuminquaNumQuant_, SharpestPatStat_, NumRestrPat_, Literal_, Comparison_, FHterm_, FHstat, Folnuminqua_, FHeqpre, SharpestStat_, SentenceNL_, (,,,)
derive gFromString HurelanStat_, EntityStat_, BridgeStat_, PatternLangFolmuminquaNumQuant_, SharpestPatStat_, NumRestrPat_, Literal_, Comparison_, FHterm_, FHstat, Folnuminqua_, FHeqpre, SharpestStat_, SentenceNL_,  (,,,)
derive bimap HurelanStat_, EntityStat_, BridgeStat_, PatternLangFolmuminquaNumQuant_, SharpestPatStat_,  NumRestrPat_, Literal_, Comparison_, FHterm_, FHstat, Folnuminqua_, FHeqpre, SharpestStat_, SentenceNL_,  (,,,)
