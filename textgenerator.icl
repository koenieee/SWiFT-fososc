module textgenerator

import StdMisc, StdString, StdTuple, StdInt, StdList, genLibTest

Start = text_nlg testkr

/* Notation

   FSF = fixed sentence fragment
   _nlg = suffix for natural language generating function.
   _rw  = suffix that this thing represents a '*r*oot *w*ord' - a word devoid of inflictions, plural etc. E.g. the root word of 'parts' is 'part' the root word of 'went' is 'go'. Etc.
   _ir = intermediate representation, a thing represented in the representation between the kr and the final natural language text. It should have a clear semantics, but already be much closer to natural language than the original kr.
   _2fw = a function that transforms a root word to (*2*) a *f*inal *w*ord after information about the required infliction has been provided.
 
*/

//textgenerator::KRmemberMeronym -> String
// sentence example: A bike consists, among other things, of the following parts: wheels, a saddle, pedals and a frame.

::KRcomposition :== (NounRootWord, NounRootWordEnumeration)

// Test
testkr::KRcomposition
testkr = (NRW "orthography", NRWEnum [NRW "punctuation", NRW "alphabet", NRW "special character"])

// Semantics of (a, b): entity a is, among other things, composed of the things enumerated in b.
// <&y2011.06.07.21:41:13& replace with adt, to better reflect semantics in structure>

/*
[(,0), (,1), SP part_rw [refers2] 3,2), (enumeration,3)]

*/

/* Notation conventions

_ci = context independent IR, this means this can be translated directly into natural language without investigating the context (surrounding representations). E.g. a rootword cannot be translated, it is 'underdetermined', but Sng "part" can be translated directly into the word "parts"

*/

::RootWord = RW String // a rootword is represented just as a string, however, to distinguish it explictly from other strings, put RW in front of it... In this way the type checker can see it as well how it is intended. It is important that a root word is not seen as ordinary word, but as a 'representative' of a group of words that are 'inflictions' of each other.
::NounRootWord                 = NRW String // ts
::PluralOrSingularNounWord_cir = Plr NounRootWord | Sng NounRootWord // ts
::NounRootWordEnumeration      = NRWEnum [NounRootWord] // ts
::NounEnumeration              = NEnum [PluralOrSingularNounWord_cir] //ts
::IntRep                       = IRposnw PluralOrSingularNounWord_cir | IRne NounEnumeration | IRnrwe NounRootWordEnumeration | IRnounref NounRootWord NounRootWordEnumeration | IRnrw NounRootWord

derive genShow RootWord, NounRootWord, PluralOrSingularNounWord_cir, NounRootWordEnumeration, NounEnumeration, IntRep

/*
instance toString IntRep
where
   toString (IRposnw (Plr (NRW s))) = "IRposnw (Plr (NRW" +++ s +++ "))"
   toString (IRne    (ne))          = "IRne (" +++ toString ne +++ ")"
   toString (IRnrwe  (nrwe))        = "IRnrwe (" +++ toString nrwe +++ ")"
   toString rest                    = abort "(around) line 51: error: instance toString IntRep: no pattern available\n"

instance toString NounEnumeration
where
   toString (NEnum ne) = toString2 ne
*/

instance toString NounRootWord
where
   toString (NRW w) = "NRW " +++ w

/*
instance toString PluralOrSingularNounWord_cir
where
   toString (Plr (NRW nrw)) = "(Plr (NRW " +++ nrw +++ "))"
   toString (Sng (NRW nrw)) = "(Sng (NRW " +++ nrw +++ "))"

instance toString NounRootWordEnumeration
where
   toString (NRWEnum nrwe) = "NRWEnum " +++ toString2 nrwe

toString2::[x]->String | toString x
toString2 l =  toStringLOS (map toString l)

// LOS = list of strings
toStringLOS::[String] -> String
toStringLOS x = "[" +++ toStringCoreLOS x +++ "]"

toStringCoreLOS []     = ""
toStringCoreLOS [x:xs] = x +++ ", " +++ toStringCoreLOS xs

/*
instance toString [a]
where
   toString []     = ""
   toString [x:xs] = toString x +++ ", " +++ toString [xs]
*/
/* IRposn = plural or singular noun word
   IRnounref: NounRootWord refers to the nounrootwordenumeration. E.g. NounRootWord NRW "part" (NRWEnum ["wheel","spoke","tyre","frame"]) implies that NRW "part" should become "parts" if you translate it to natural language.
*/ 

*/

space =: " "

text_nlg::KRcomposition->String
text_nlg kr = cap1stLetter (cir2nlg (IRnrw whole)) +++ space +++ consistsFSF_nlg +++ cir2nlg (IRposnw (ref (NRW "part") parts)) +++ ": " +++ cir2nlg (IRnrwe parts)
where 
   whole     = fst kr
   parts     = snd kr
// translates a thing in intermediate represention into natural language when possible (or choosing a default translation when there are more options, e.g. nl( part_rw ) will be translated to the singular "part".

ref::NounRootWord NounRootWordEnumeration -> PluralOrSingularNounWord_cir
ref nrw (NRWEnum nrws) // = Sng nrw
                     | length nrws == 1  = Sng nrw
                                         = Plr nrw

f::NounRootWord NounRootWordEnumeration -> PluralOrSingularNounWord_cir
f n (NRWEnum m) = Sng n

cir2nlg :: IntRep -> String
cir2nlg (IRposnw (Plr (NRW word))) = word +++ "s"
cir2nlg (IRposnw (Sng (NRW word))) = word
cir2nlg (IRnrwe (NRWEnum nrwe))    = andList (map nrw2nlg nrwe)
cir2nlg (IRnrw  (NRW nrw))         = nrw
cir2nlg rest                       = abort ("cir2nlg: error: you try to transform a context dependent intermediate represention into natural language " +++ show1 rest +++ "\n")


/* Auxiliary function for cir2nlg */

nrw2nlg::NounRootWord->String
nrw2nlg (NRW nrw) = nrw

andList::[String]->String
andList [w:[]] = toString w
andList [w:ws] 
             | length ws == 1   = (toString w) +++ " and " +++ andList ws
                                = (toString w) +++ ", "    +++ andList ws

consistsFSF_nlg = "consists, among other things, of the following "

// Post processing natural language




// General language generators
cap1stLetter::String->String
cap1stLetter word = word // <&y2011.06.16.09:43:19& finish> 


// Auxiliary functions
/* <&y2011.06.16.09:44:09& finish
randomSelect::[a]->a
randomSelect [1..n] = randomly select from list one element
*/
