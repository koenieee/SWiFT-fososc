/* Notation

   FSF = fixed sentence fragment
   _nlg = suffix for natural language generating function. Its result is always a natural language fragment (or complete text/sentence).
   _rw  = suffix that this thing represents a '*r*oot *w*ord' - a word devoid of inflictions, plural etc. E.g. the root word of 'parts' is 'part' the root word of 'went' is 'go'. Etc.
   _ir = intermediate representation, a thing represented in the representation between the kr and the final natural language text. It should have a clear semantics, but already be much closer to natural langauge than the original kr.
   _2fw = a function that transforms a root word to (*2*) a *f*inal *w*ord after information about the required infliction has been provided.
 
*/

textgenerator::KRmemberMeronym -> String
// sentence example: A bike consists, among other things, of the following parts: wheels, a saddle, pedals and a frame.

::Word ::= String
::KRcomposition ::= (Word, [Word])

// Test
kr = ("orthography", ["punctuation"])

// Semantics of (a, b): entity a is, among other things, composed of the things enumerated in b.
// <&y2011.06.07.21:41:13& replace with adt, to better reflect semantics in structure>

/*
[(,0), (,1), SP part_rw [refers2] 3,2), (enumeration,3)]

*/

text_nlg kr = cap1stLet fst[kr]  ++ consistsFSF_nlg ++ ref RW "part" partList_ir constistsOf_nlg length kr ++ nl partList_ir
where 
   whole    = fst[kr]
   part     = snd[kr]
   partWord = snd plural word 

// translates a thing in intermediate represention into natural language when possible (or choosing a default translation when there are more options, e.g. nl( part_rw ) will be translated to the singular "part".
nl 

partList_ir = andList_nlg snd[kr]

" consists, among other things, of the following parts: " ++ 

consistsFSF_nlg = "consists, among other things, of the following "
consistsOf_nlg 1 = consistsFSF_nlg ++ "the following part: "
consistsOf_nlg n = consistsFSF_nlg ++ "the following 

// General language generators

consistsOf :: n -> String
andList_nlg :: word
andList_nlg [word] 
cap1stLet Word -> Word
cap1stLet word = 


// Auxiliary functions
randomSelect::[a]::String -> String


randomSelect [1..n] = randomly select from list one element
