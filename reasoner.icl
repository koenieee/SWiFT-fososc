module reasoner
/* Algorithmic Defence Generator API, reads arguments to offer function from file. Expects:

input:
filename: reasoner.clean.in
with arguments of type reasoner, seperated by exactly 1 newline each

output:
filename: reasoner.clean.out

*/

import genToFromString, StdGeneric, StdEnv, gast, GenericByCG, textgenerator_version2

// Output of Start is identical to the output written to the file reasoner.clean.out, to simplify logging. If the output becomes too large in the future, perhaps trim it or don't return it at all.
// <&y2011.12.16.10:57:50& how to write a good exit value to the output (according to unix standards) or am I doing it correctly?>
Start::*World->(String, World*)
Start world = (outputFuncApp, world3)
where
   world3 = OpenFileAndWriteString "reasoner.clean.out" outputFuncApp world2
   (outputFuncApp, world2) = OpenFileAndApplyFunction2 "reasoner.clean.in" reasonerAPI world

/* 

*/
// Reasoner::FHdoc QuestionLang_ -> AnswerLang_


// The API returns the output of the function in a file with 2 lines, the first line = the first element of the 2-tuple, etc.
reasonerAPI::File -> String
reasonerAPI file = item1c +++ "\n" +++ (gToString {|*|} item2)
where
   item1c // trick to prevent using gToString on Maybe ..., which seems not to be implemented <?to{[Pieter Koopman]}&y2011.12.27.17:14:13& how to realise this? Ask Pieter Koopman?>
      | isJust item1 = gToString {|*|} (fromJust item1)
                     = "Nothing"
   (item1, item2) =  Reasoner fhdoc question
   fhdoc
      # (num, maybe) = gFromString {|*|} 0 fhdocStr
      | isJust maybe = fromJust maybe
                     = abort("Error, fhdoc could not be parsed\n(fhdoc = \"" + fhdocStr + "\"")

   question
      # (num, maybe) = gFromString {|*|} 0 questionStr
      | isJust maybe = fromJust maybe
                     = abort("Error, question could not be parsed\n")

   (fhdocStr, filerest1)      = sfreadline file
   (questionStr, filerest2)   = sfreadline filerest1

// <&y2011.12.07.22:39:58& move to generic lib>

// If this function doesn't abort, it was succesfull. So no error handling is needed after calling this function.
// <_&y2011.12.27.21:48:54& I think problem here: the \n is not printed to the file. Solve>[A &y2011.12.27.22:03:14& no it is printed, tested, no errors here.>

OpenFileAndWriteString :: String String *env -> *env | FileSystem env
OpenFileAndWriteString filename filecontent filesys
   # (ok, file, filesys) = fopen filename FWriteText filesys
   | not ok              = abort("Cannot open output file: '" +++ filename +++ "'")
   # file               = fwrites filecontent file
   # (ok, filesys)      = fclose file filesys
   | not ok             = abort("Cannot close output file: '" +++ filename +++ "'")
                        = filesys

derive gToString PatternLangFolmuminquaNumQuant_, SharpestPatStat_, NumRestrPat_, Literal_, Comparison_, FHterm_, FHstat, Folnuminqua_, FHeqpre, SharpestStat_, (,,,)
derive gFromString PatternLangFolmuminquaNumQuant_, SharpestPatStat_, NumRestrPat_, Literal_, Comparison_, FHterm_, FHstat, Folnuminqua_, FHeqpre, SharpestStat_, (,,,)
derive bimap PatternLangFolmuminquaNumQuant_, SharpestPatStat_,  NumRestrPat_, Literal_, Comparison_, FHterm_, FHstat, Folnuminqua_, FHeqpre, SharpestStat_, (,,,)
