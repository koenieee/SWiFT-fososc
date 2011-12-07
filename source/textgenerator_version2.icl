/* General todos:
<&y2011.08.07.16:08:32& Keep into account inequality axioms in FHdoc2nl. If there are no inequality , you should add additional natural language statements, e.g.: "It is possible that .. are different names for the same child". Or better: such texts should not be generated, so simply always add all inequality axioms. >
<& &y2011.08.07.16:11:12& Interesting problem here: I can simply ASSUME the inequality axioms are there and are correct to make the right translation. However, it would be much more elegant when you would check the inequality axioms, and either produce an error when they are not all present in the right way, OR add additional natural language statements to state an exception to una.
<&y2011.08.07.16:09:55& add argument aggregation in predicates>

4r*/

// &y2011.07.27.17:04:39& First I focus on FH2nl, then on SynRep.

module textgenerator_version2

import StdMisc, StdString, StdTuple, StdInt, StdList, genLibTest, GenericByCG, MersenneTwister, StdDebug, ArgEnv, StdListExtensions, StdMaybe

/*
Start = PluraliseNoun "scientist"
*/

/* Interface ADT: interface to external langauges
Wrap parts of the answer in a ADT so that it is easy for the receiving app to see the function of the different parts
*/

::InterfaceADT = TextCTL FHdoc | TextNL [SentenceNL_] | QuestionAttackCTL PatLang_ | QuestionAttackNL [SentenceNL_] | AnswerCTL AnswerLang_ | AnswerNL [SentenceNL_]
/*
Start = (docNL, fhdoc, qaNL, qaCTL, Reasoner (filter isPre2Stat fhdoc) qaCTL)
where
   qaNL  = TranslateQuestionCTL2NL qaCTLcomplete
   qaCTL = fst qaCTLcomplete
   qaCTLcomplete =  GenerateQuestionAttack fhdoc hurelans_cfnl
   docNL = FHdoc2nl fhdoc positions hurelans_cfnl
   positions      = map (\h -> (h.hra_f, 1)) hurelans_cfnl
   fhdoc = fst sr
   hurelans_cfnl = [snd sr]
   sr = SynRep
*/
Start::[InterfaceADT]
Start = [TextCTL fhdoc, TextNL docNL, QuestionAttackCTL qaCTL, QuestionAttackNL qaNL, AnswerCTL answerCTL, AnswerNL answerNL]
where
   answerNL = AnswerInCTL2NL (answerCTL, hurelans_cfnl)
   answerCTL =  Reasoner fhdoc qaCTL
   qaNL  = TranslateQuestionCTL2NL qaCTLcomplete
   qaCTL = fst qaCTLcomplete
   qaCTLcomplete =  GenerateQuestionAttack fhdoc hurelans_cfnl
   docNL = FHdoc2nl fhdoc positions hurelans_cfnl
   positions      = map (\h -> (h.hra_f, 1)) hurelans_cfnl
   fhdoc = fst sr
   hurelans_cfnl = [snd sr]
   sr = SynRep

/*
Start = (SynRep, FHdoc2nl fhdoc positions hurelans_cfnl)
   where 
      fhdoc          = fst sr
      hurelans_cfnl  = [snd sr]
      // set all positions on 1, to 'suggest' the player to create hurelan-predicates with the same order of roles.
      positions      = map (\h -> (h.hra_f, 1)) hurelans_cfnl
      sr             = SynRep
*/
//Start = getElement_HM [(1,"uno"), (2, "dos")] 3
//Start = "Network Universalis"
//Start = FHhurelanstat2nl (FHpre2stat ("hasChild",["Akwasi", "Sirtaki"])) 2
//Start = searchHurelan_cfnl "hasChild"
// Start = FilterPreStat "hasChild"  [FHpre2stat ("hasChild",["Akwasi", "Sirtaki"]), FHpre2stat ("hasColor",["Loxolop", "white"])]
// Start = SynRep
//Start = FHdoc2nl [FHpre2stat ("hasChild",[["Akwasi", "Fianna"], ["Sirtaki", "Mirbaka", "Oliver"]])] [("hasChild", 2)]

// #####################################################################################
// ### Data stuctures for formal language Folminqua + Hurelan, abbreviated with FH here.


/* Note:
- Doesn't 'check' for right number of places used in a predicate
- Hurelan is explicitly represented because it is ...
*/


::FHdoc   :== [FHstat]
::FHstat         = FHeqstat (FHeqpre,[FHconstant]) | FHpre2stat (FHpre,([FHconstant],[FHconstant])) | FHhurelanprestat (FHpre,[[FHconstant]]) | Hurelan FHpre

// <_&y2011.11.27.22:15:54& FHpre2stat [[FHconstant]] must be expanded with free variables!>
// <&y2011.11.27.22:23:13& [[FHterm_]] is bit strange, what is the meaning of more free variables in one list? E.g. [[c1, c2], [x, y]]. Better not allow this, abnd only allow either a list of constants, OR a free variable.
// <_&y2011.11.27.22:26:39& Consider restricting FHdoc to a language without free variables at all, because, in fact they are not part of it. Then redefine a variant with free vars  for the extensions which need it.>

/* FHpre2stat (FHpre,[[FHconstant]]) meaning: 
FHconstant may only contain 2 elements. Each element e1 in the first element of FHconstant, has a relation FHpre(e1,e2) with each element e2 in the second element of FHconstant. Example. FHprestat ("hasChild", [[Akwasi, Winaina], [Sirtaki, Odin]]) would translate to FOL as follows:

hasChild(Akwasi,Sirtaki) 
hasChild(Akwasi,Odin)
hasChild(Winaina,Sirtaki)
hasChild(Winaina,Odin)
*/ 

// prestat = predicate statement

::FHconstant   :== String
::FHpre        :== String
::FHeqpre      =   Eq | Neq // eqpre = equality predicate

isEqStat::FHstat -> Bool
isEqStat (FHeqstat _)   = True
isEqStat fhstat         = False

eqStat      = find isEqStat [(FHeqstat (Neq, ["Akwasi", "Sirtaki"]))]

isPre2Stat::FHstat -> Bool
isPre2Stat (FHpre2stat _)   = True
isPre2Stat fhstat           = False

/* #####################################################################################
   ### Clean constructs for representing information about natural language. This is 
   ### needed for the synthesis stage. 
*/

/* Notations:

nli = natural language information, information about natural language. A suffix used for all data (structures) that provide information about how to use given constructs from natural language (English).

*/

// <&y2011.07.26.09:39:38& also specify a formal language not embedded in Clean for this purpose (in fact "an ontology")

ProperNamesForPersons_nli :: [String]
ProperNamesForPersons_nli |
               length names < 2 = abort("\nProperNamesForPersons_nli: error, please define more than one proper names in my definition\n")
                                = names
where
     names = ["Akwasi", "Ebere", "Marijke", "Christopher", "Gabriel", "Chide", "John-Jules", "Gerard", "Sirtaki", "Bonjordoki", "Ivana", "Daniel", "Kevin", "Pondiwu", "Henk-Jan", "Anastasia", "Alexia", "Ronald", "Frank", "Crista", "Sybren", "Eva", "Jan", "Alex", "Quintus", "Quadario", "Rapunzel", "Pjoipjoibyroi"] // _nli added because this represents the information that the words in the list can be used as proper names for persons in natural language.
PNFP_length =: length ProperNamesForPersons_nli

Hurelans_nli :: [(String,String)]
Hurelans_nli = [("teacher", "student"), ("child", "parent"), ("doctor", "patient"), ("leader", "follower"), ("sibling", "sibling"), ("dentist", "patient"), ("grandparent", "grandchild"), ("husband", "wife"), ("trainer", "trainee"),("friend","friend"),("enemy","enemy"),("employer","employee")]

// if not mentioned in this list, assume the regular conjugation with -s
IrregularPluralNouns::[(String, String)]
IrregularPluralNouns = [("child", "children"), ("grandchild", "grandchildren"),("enemy", "enemies")]


// <&y2011.07.27.12:57:14& problem: some roles have a sex (sister, wife, brother). For now I simply ommit them, so I limit to hurelans that are indifferent to sex>

/* #####################################################################################
   ### Intermediate languages in the process of translating Folminqua + Hurelan to 
   ### natural language
*/

// example of translation with intermediate representations in between:

/*
In syntax of formal languages:
FL: {hasChild(Akwasi, Sirtaki), Hurelan(hasChild)} --> IR: RelationEITOrOLEiMPLYINGcOUNTERrOLE(ent1 = "Sirtaki", ent2 = "Akwasi", roleEnt1 = NRW("child")) --> NL: "Sirtaki is a child of Akwasi" 
rELATIONeXPRESSEDiNtERMSoFrOLEiMPLYINGcOUNTERrOLE = reitoriMPcOrO

abbrev. used in general: EITO = expressed in terms of

In syntax of datastructures of Clean:

[FHpre2stat "hasChild" ["Akwasi", "Sirtaki"], Hurelan "hasChild"]
->

{ // &y2011.11.26.17:49:21& wiw: writing intersection fuction in GenericByCG ent1 = "Sirtaki",
  ent2  = "Akwasi",
  roleEnt1 = "child"
}

NRW = Noun Root Word

' ROLEiMPLYINGcOUNTERrOLE =  RiMPcOrO: connecting two entities in which one plays a role that always implies its counterpart: teacher (implies student), parent (implies child), master (implies student), director (implies organisation) (note that this notion is not the same as a relational antonym, it is weaker, because for such an antonym it should work both ways. The example with directory <-> organisation doesn't work both ways, a director implies something that is directed, but an organisation does not imply that it should be directed. You also notice this in "this organisation HAS the following direcor, but this director has the following organisation doesn't work (it should be this director is head of this organisation or something similar).
*/

::NounRootWord                 = NRW String // ts

::ReitoRiMPcOrO = {  ent1::String,
                     ent2::String, // for now strings to keep it simple
                     ent1Role::NounRootWord
                  }

// <&y2011.07.26.09:59:41& extend IntRep from version1 of this code with RiMPcOrO>

/* #####################################################################################
   ### Representing information required to extend the semantics of Folminqua + Hurelan 
   ### in terms of natural language (or connect the semantics of Folminqua + Hurelan to that 
   ### of natural language.

   Conventions:
      Types for this purpose have the suffix: cfnl = Connection Folminqua+hurelan and Natural Language
      Fields of records that are associated with natural language carry suffix _nl, while those of Folminqua carries suffic _f.
*/

::Hurelan_cfnl =  {  hra_f   :: String,
                     ent1_nl :: String,
                     ent2_nl :: String
                  }

/* Example:

{ hra_f = "hasChild", 
  ent1_nl = "parent", 
  ent2_nl = "child"
}

This states: hurelan "hasChild" in Folminqua is defined as follows: a hasChild b if a fulfills the natural language role "parent" with respect to b, or if b fulfills the role expressed with the natural language word "child"

*/
/* 
hurelans_cfnl :: [Hurelan_cfnl]

hurelans_cfnl = [ {  hra_f   = "hasChild", 
                     ent1_nl = "parent", 
                     ent2_nl = "child"
                  }
                ]
*/
// Move to generics


/* Get first element in list that satisfies condition c
*/
// <&y2011.08.06.17:48:21& move to generics>
getFirst::(a -> Bool) [a] -> [a]
getFirst c [x:xs]
                  | c x   = [x]
                          = getFirst c xs
getFirst c []     = []

::HashMap hash val :== [(hash,val)]

getElement_HM :: (HashMap hash val) hash -> [val] | Eq hash

getElement_HM [(hashInMap,val):hm] hash 
                           | hashInMap == hash = [val]
                                               = getElement_HM hm hash
getElement_HM [] hash = []


searchHurelan_cfnl::String [Hurelan_cfnl] -> [Hurelan_cfnl]
searchHurelan_cfnl hra hurelans_cfnl = getFirst (\h -> h.hra_f == hra) hurelans_cfnl

/* #####################################################################################
   ### FH2nl (Folminqua to natural language): translates a document represented in 
   ### Folminqua + Hurelan into natural language. The current version only concerns 
   ### it self about Hurelan predicates, others are disregarded. Morover, it only treats 
   ### those hurelan predicates that are provided in the arguments.

Algorithm:
- For each predicate in the second argument, do:
   - Find all indviduals I,j for which P(i,j)

*/


/* 
IN:
Second argument: specifies for each hurelan which side of the relation must be aggregated. The Int must thus be 1 or 2, and nothing else. <&y2011.07.31.12:39:13& add definition of aggregation>
The FHpre MUST be a hurelan - this is not enforced now by the datatype
<&y2011.08.01.15:22:18& realise this is needed>

OUT:
List of strings, each string represent one natural language sentence.
*/

::SentenceNL_ = SentenceNL String

// <&y2011.11.23.15:37:13& create a FHdoc2nl with optinal positions parameter>
FHdoc2nl :: FHdoc (HashMap FHpre Int) [Hurelan_cfnl] -> [SentenceNL_]

FHdoc2nl fhdoc preposs hurelans_cfnl = map (FHhurelanstat2nl_loc hurelans_cfnl) (filter isPre2Stat fhdoc) // pre is now the one that has the turn, work with that one
// <&y2011.11.15.16:03:11& rename preposs to role order, because this is describes more accurately what is happening?>
  // <&y2011.11.29.20:15:31& currenlty I neglect the in equality/inequality statements, and assume all the inequality statements are ok (= for all objects it is stated they are mutually distinct). This will lead to incorrect results when the (in)equality statements are not expressing this. Change this in the future when required.>
where
   FHhurelanstat2nl_loc hurelans_cfnl fhstat = FHhurelanstat2nl fhstat (selectPos fhstat) hurelans_cfnl
   selectPos (FHpre2stat (pre,constants)) 
                                       | length posInMap == 0 = abort ("No position defined for predicate " +++ pre +++ ".") // not found 
                                                              = posInMap!!0
   where 
      posInMap = getElement_HM preposs pre //selectPos fhstat = abort "Error: currently no support for other statements than FHpre"



/* Assumes all 
N: preposs: is a hashmap with 'positions' for the translation of hurelans. For example, given the hurelan (child, parent):

1 means that the verb in the sentence will be expressed in terms of the first role in the hurelan, 2, well if it ain't clear now...
<&y2011.11.15.16:06:48& is this completely right? I don't have this clear yet. The hurelan_cfnl positions indicate the role each entity plays in the predicate, however, the pos hashmap does something different. But what?>

OUT:

...

Example:

FHpre2stat ("hasChild",["Akwasi", "Sirtaki"]) 1
--> "Akwasi is a parent of Sirtaki"

FHpre2stat ("hasChild",["Akwasi", "Sirtaki"]) 2
--> "Sirtaki is a child of Akwasi"

*/


FHhurelanstat2nl::FHstat Int [Hurelan_cfnl] -> SentenceNL_
FHhurelanstat2nl  (FHpre2stat (fhpre, fhconstants)) pos hurelans_cfnl
               | pos == 1  = SentenceNL ( andList(fst fhconstants) +++ " is a " +++ roleEnt1 +++ " of " +++ andList(snd fhconstants) ) // <&y2011.08.06.18:17:48& also define a connection between constants and the way to represent these constants as nouns in nl. I know assume the label of the constant to coincide with the nl noun, but that is not tidy
               | pos == 2  = SentenceNL ( andList(fst fhconstants) +++ " is a " +++ roleEnt2 +++ " of " +++ andList(fst fhconstants) )
                           = abort("Error: FHhurelanstat2nl offered pos = " +++ toString pos +++ ".")
               where
                  roleEnt1 
                           | length hl_cfnl == 0  = "[error during translation: hurelan_cfnl not found]"
                                                  = (hl_cfnl!!0).ent1_nl
                  roleEnt2 
                           | length hl_cfnl == 0  = "[error during translation: hurelan_cfnl not found]"
                                                  = (hl_cfnl!!0).ent2_nl
                  hl_cfnl = searchHurelan_cfnl fhpre hurelans_cfnl

// <&y2011.08.06.14:24:52& Should be a bit different: type is too wide for this function, so either extend function defs to also cover rest, or define narrower type...>

// <_& &y2011.08.01.15:28:00& algorithm for aggregation given that FHdoc only contains ONE predicate which expresses a hurelan>

// <<< EUC

// FilterPreStat: turns out (currently) not to be needed.

FilterPreStat :: FHpre FHdoc -> FHdoc // FHpre must be a hurelan

FilterPreStat fhpre fhdoc = filter containsPre fhdoc
where
   containsPre     (FHpre2stat (fhpre_loc,args))
                   | fhpre_loc == fhpre      = True
                                             = False
   containsPre     fhstat = False

/*
   ### 
   ### End FH2nl
   ### 
   ###
   ######################################################################################
*/

// SynRep: Synthesise a representation of a state of affairs (in Folminqua + Hurelan)
/* Algorithm:
   - pick a random hurelan
   - pick a random number for the number of persons in the first or second position, and then pick 1 person for the person in the remaining position of the hurelan. [&y2011.08.08.13:20:04&: Idea is: make it possible to enforce a minimum number of persons having the same relation with 1 other person, as to ensure the !una condition can be violated! However, more than 1 only in one position to keep things simple for now]
*/



MAX_MULTIPLICITY :== 2

// For now I have solved the random sequence problem by simply generating different random sequences. This requires additional work, because you must create a separate seed for each of them.

// maximum size integer: 2147483648 (10 digits) (verify!) 

SEED_4ranSeeds =: firstCommandLineArg
randomSeeds = genRandInt SEED_4ranSeeds

firstCommandLineArg = trace ("seed = " +++ toString seed +++ "\n") seed
   where
      seed
         |  size cl == 2       = charArrayToInt (cl.[1]) // <&y2011.11.15.22:31:56& also test whether first argument indeed is a string that represents an Int>
                               = abort usageMessage
      cl = getCommandLine
      usageMessage = "Usage: textgen SEED, where SEED is an integer between -2147483647 and 2147483647"

// randomHurelanIndices
SEED_4ranHurInd :== randomSeeds!!0 // <&y2011.11.13.17:48:13& initialise from with external seed, also the other seeds in this program...>
randomHurelanIndices = map (\x -> (abs x) rem (length Hurelans_nli) ) (genRandInt SEED_4ranHurInd)

// randomPositions
SEED_4ranPos    :== randomSeeds!!1
randomPositions   =    map (\x -> (abs x) rem 2 + 1) (genRandInt SEED_4ranPos)

// randomMultiplicities for the position of the hurelan in which more than 1 person may occur.

SEED_4multipl   :== randomSeeds!!2
randomHurelanMultiplicities = map (\x -> (abs x) rem MAX_MULTIPLICITY + 1) (genRandInt SEED_4multipl)

SEED_4ranPersons :== randomSeeds!!3
SEED_4ranPerson  :== randomSeeds!!4

// <_&y2011.08.08.18:12:14& perhaps use MersenneTwister instead of current random function? (See cleanlibs, already downloaded>[A &y2011.11.10.19:11:10& done]

SynRep :: (FHdoc, Hurelan_cfnl)
// <&y2011.08.08.16:09:40& change Hurelan_cfnl such that entities are in ordered construct (e.g. List) instead of name. Will simplify other implementations.
// <&y2011.08.08.17:32:59&> currently, in the nl construct "... are XXX of ...", only the first position may have multiplicity 1>, the other must be 1. Change so that this can also be the other way around.
SynRep = (fhdoc, createdHurelanPredicate) // randomHurelans[0] because for now only generate one hurelan predicate.
where
   fhdoc = [FHpre2stat (randomPredname,([randomPerson], randomPersons)), FHeqstat (Neq,randomPersons)] // create a one-statement document only. The first position of the predicate only one person (simplifying design decision for increment).
   createdHurelanPredicate =  { hra_f    = randomPredname, // create new predicate based on the randomly selected hurelan from natural language
                                ent1_nl  = roleFirst,
                                ent2_nl  = roleSecond
                              } // &y2011.08.08.16:11:57& for now I assume people to translate hurelan predicates "literally", thus not 'Sirtaki is a child of Akwasi' --> 'hasParent(Sirtaki, Akwasi). Of course I can change this in theHurelan future. This remark by the way should be in the translation verification routine, not here.
   randomPredname             
      |  randomPos == 1 = "is_" +++ fst randomHurelan +++ "_of"
      |  randomPos == 2 = "is_" +++ snd randomHurelan +++ "_of"
                        = abort "Error at createHurelanPredicate/randomPos, pos must be 1 or 2."
   roleFirst
      | randomPos == 1  = fst randomHurelan
                        = snd randomHurelan
   roleSecond
      | randomPos == 1  = snd randomHurelan
                        = fst randomHurelan

   randomHurelan        = randomHurelans!!0
   randomPos            = randomPositions!!0
   randomHurelans       = map (\i -> Hurelans_nli!!i) randomHurelanIndices // [&y2011.11.13.17:49:24& at this moment, this is in fact overkill because only one hurelan is needed. But it is nice for future increments.]

   randomPerson         = (randomPick ProperNamesForPersons_nli 1 SEED_4ranPerson)!!0
   randomPersons        = randomPick (removeMember randomPerson ProperNamesForPersons_nli) randomMultiplicity SEED_4ranPersons // <&y2011.11.14.15:24:36& important: when you need to create MORE than 1 list of randomPersons in a future increment of the software, don't forget to provide a new unique seed each time. You could do this by creating an additional random sequence which you use as seeds for this purpose>
   randomMultiplicity   = min (PNFP_length - 1) (randomHurelanMultiplicities!!0)


andList::[String]->String
andList [w:[]] = toString w
andList [w:ws] 
             | length ws == 1   = (toString w) +++ " and " +++ andList ws
                                = (toString w) +++ ", "    +++ andList ws

// <&y2011.11.23.12:50:48& in future work with the intermediate representation which I also created in the first version of textgenerator (code still available). In that way you make the types of natural language structures/words transparent to Clean's type system.

::SingularNoun :== String
::PluralNoun   :== String

PluraliseNoun::SingularNoun -> PluralNoun
// <&y2011.11.23.14:02:29& not so elegant, what I miss is the possibility to do pattern matching on local variables instead of only on the parameters of the function.
PluraliseNoun singnoun = Result IrregularPlural
where
      Result Nothing       = singnoun +++ "s"
      Result (Just plural) = snd plural
      IrregularPlural = find (\(x,y) -> x == singnoun) IrregularPluralNouns

// &y2011.11.21.13:50:37& Hurelan_cfnl fullfills the role of "bridge info" as explained in DSID2011.11.19. <&y2011.11.21.13:53:33& refactor Hurelan_cfnl in future to a more general name reflecting the DSID scheme>


/* ### 
   ### End SynRep
   ### 
   ###
   ######################################################################################
*/



/* #####################################################################################
   ### Rendering of PatternLang(Folnuminqua, numberquantifier) in a Clean type
   ###
   ###
*/

::QuestionLang_ :== PatternLangFolmuminquaNumQuant_
::PatLang_ :== PatternLangFolmuminquaNumQuant_ // just for completeness: within this namespace PatLang suffices, but in a bigger namespace with other pattern languages it may be required to disambiguate by choosing the longer name.

::PatternLangFolmuminquaNumQuant_ = PatLang SharpestPatStat_ // should also include, but for now simplify by not allowing this: | PLfnStat NumRestr_
::SharpestPatStat_   = SharpestPat (PatVar_, NumRestrPat_)
::NumRestrPat_       = NumRestrPat (Comparison_, PatVar_, BoundVar_, Literal_)
::Comparison_        = Eqt | Gt | Lt | Leq | Geq
::Literal_           = Literal (FHpre_, [FHterm_])
::FHterm_   = FHvar Var_ | FHconstant Constant_
::BoundVar_ :== String
::FHpre_    :== FHpre
::Constant_ :== String
::Var_      :== String
::PatVar_   :== String

// Example (& test)
// exampleAndTestStat = PatLang (SharpestPat ("n", NumRestrPat (Leq, "n", "x", Literal ("is_parent_of",[["x"],["Sirtaki", "Lariska", "Ngomo"]]))))


/* #####################################################################################
   ### GenerateQuestionAttack
   ###
   ###
*/
// <&y2011.11.21.14:03:35&> QuestionInCtl define it here

GenerateQuestionAttack:: FHdoc [Hurelan_cfnl] -> (QuestionLang_, [Hurelan_cfnl])

/* <&y2011.11.21.22:43:47& wouldn't it be better to always package a FHdoc with its corresponding bridge2NL information? Without that, you must somehow derive from the context what the intended bridge was...>
 @DD(
   project = [SWiFTfososc]
   created = &y2011.11.21.22:48:03&
   definition = for now my design decision is to do so.
*/
/*
::FHdoc   :== [FHstat]

::FHstat         = FHeqstat (FHeqpre,[FHconstant]) | FHpre2stat (FHpre,[[FHconstant]]) | FHhurelanprestat (FHpre,[[FHconstant]]) | Hurelan FHpre
*/

GenerateQuestionAttack fhdoc h_cfnl = GenerateQuestionAttackCore (filter isPre2Stat fhdoc) h_cfnl
// <&y2011.11.29.20:20:43& currently I assume: only 1 pre2stat in fhdoc. If this condition doesn't hold any more, this function must be modified!>

GenerateQuestionAttackCore [FHpre2stat (fhpre,([subject],_))] h_cfnl = (PatLang (SharpestPat ("n", NumRestrPat (Geq, "n", "x", Literal (fhpre, [FHconstant subject, FHvar "x"])))), h_cfnl)

// <&y2011.11.21.22:57:08& How to cope with arity of predicates, I now assumed a general n-ary predicate, or is it more elegant to restrict it to hurelan 2place predicates?>

// <&y2011.11.21.23:22:05& most elegant name for type of TranslateQuestionCTL2NL? This name is perhaps TOO generic, there are may CTL's possible...>

TranslateQuestionCTL2NL::(PatLang_, [Hurelan_cfnl]) -> [SentenceNL_]
/* &y2011.11.23.14:41:44& not two arguments, but a tuple, because I want to be able to pass this 'package' easily from one function to another

   <&y2011.11.30.17:59:55& Good idea! Instead of stating 'minimally has' it is better to say 'does NOT have less than n'. In that way you express that you look for the sharpest number you can derive from the given axioms! Also change this in the formal question language!>[A &y2011.12.02.20:10:24& doesn't work! if the statement is true for n, then it is also true for all m smaller than n.>
( importance = 9
)

  COULDDO:
- <&y2011.11.23.14:42:24& currently doesn't check for complete correctness of the PatLang question (I think). Doesn't matter for nowm, since it is automatically generated and thus MUST be correct>
*/
TranslateQuestionCTL2NL (PatLang (SharpestPat (num1, NumRestrPat (Geq, num2, var1, Literal (fhpre, [FHconstant subject, FHvar var2])))), hurelans_cfnl)
   | not(num1 == num2) = abort ("TranslateQuestionCTL2NL: error, number restriction pattern variables inequal")
   | not(var1 == var2) = abort("TranslateQuestionCTL2NL: error, bound variables unequal")
                  = [SentenceNL ("How many " +++ pluralobjectrole +++ " does " +++ (toString subject) +++ " minimally have?")]
   where
      pluralobjectrole  = PluraliseNoun objectrole
      objectrole        = hurelan_cfnl.ent2_nl
      hurelan_cfnl      = (searchHurelan_cfnl fhpre hurelans_cfnl)!!0 // <&y2011.11.23.14:39:41& curently this is overkill, there is only one hurelan_cfnl statement in hurlans_cfnl, the right one, but perhaps for future increments this is the right generic implementation>

/* #####################################################################################
   ### Answer Language
   ### See swiftfososc(!una).pdf specification
   ###
*/

::AnswerLang_ :== Folnuminqua_ // just for completeness: within this namespace PatLang suffices, but in a bigger namespace with other pattern languages it may be required to disambiguate by choosing the longer name.
::Folnuminqua_    = Folnuminqua SharpestStat_
::SharpestStat_   = Sharpest SharpestPatStat_ Int


// <&y2011.11.23.12:30:56& improve question so that people should give the sharpest answer>

/* #####################################################################################
   ### Reasoner
   ###
   ###
*/

// <&y2011.11.23.21:38:23& invent more specific name for reasoner>

// SharpestAnswer transforms a given SharpestPatStat and an integer n to the answer that the first is "equal" to the latter. Without any analysis, this function is purely a syntactical aid, and intended to be called after certainty that the correctness of this transformation has already been asserted.
SharpestAnswer::SharpestPatStat_ Int -> AnswerLang_
SharpestAnswer sharpestpat n = (Folnuminqua (Sharpest sharpestpat n))

/* &y2011.11.25.09:35:35& I now first make create a simple incomplete implementation which:
- only derives if inequality axioms are present between ALL individuals which satisfy the formula of the question (the free variable after removal of the number restriction quantifier).
*/


Reasoner::FHdoc QuestionLang_ -> AnswerLang_

// &y2011.11.25.09:46:27& I assume all inequality axioms to have been aggregated into one axiom
// &y2011.11.25.09:47:52& I assume the same for the hurelan statements
// <&y2011.11.25.09:46:14& don't assume this in future increment!>
Reasoner fhdoc question
         | noStat == 2           = Answer question case2Statements
         | noStat == 1           = Answer question (case1Statement (fhdoc!!0))
         | noStat == 0           = Answer question 0
                                 = abort("more than 2 statements: not implemented yet")
                                 // V if there are more than 2 statements: respond: not implemented yet.
// if there are 2 statements: one statement must be a number restriction, the other an inequality, otherwise respond: not implemented yet.
// if there is 1 statement: 
// using sharpest >= question, first look for statement with the same predicate, if there isn't any, then derive 0
// all other questions: respond: not implemented yet.
//  if there is, investigate if the subject is the same, if not derive >=0. If it is: then investigate if all individuals occur in the inequality axiom. If so, substitute for the number variable the number of individuals, if there isn't substitute 1.
   where
      noStat = length fhdoc
      case2Statements::Int
      case2Statements //= (length (entitiesFromNeqStat (fromJust eqStat)))
            | isNothing eqStat  = abort("2 statements, but no inequality statement, not implemented yet")
            | isNothing preStat = abort("2 statements, but no predicate statement, not implemented yet")
            | sameEls           = (length (entitiesFromNeqStat (fromJust eqStat))) // COULDDO inefficiency here, sameElementsUEq already calcualted this length.
                                = abort("Equality statement does not contain the same individuals as the predicate statement, not implemented yet.")
            where
               sameEls = sameElementsUEq (entitiesFromNeqStat (fromJust eqStat)) (onePreStatDerivation (fromJust preStat) question)
      // <&y2011.11.27.17:50:01& conditions unnecessarily strong here, you don't need same elements, there is weaker condition: the individuals in the prestat must all occur in the inequality statement (but there may occur MORE individuals in inequality statement)>\
      Answer (PatLang sharpestpat) n = SharpestAnswer sharpestpat n
      entitiesFromNeqStat::FHstat->[FHconstant]
      entitiesFromNeqStat (FHeqstat (Neq, entities)) = trace_loc entities
         where
            trace_loc ents = ents
            //trace_loc ents = trace ("entitiesFromNeqStat: result has length = " +++ (toString (length ents)) +++ ents!!0 +++ "\n" ) ents

      // entitiesWhichDifferFromEqStat (FHeqstat _)               = []
      // entitiesWhichDifferFromEqStat rest = abort("entitiesWhichDifferFromEqStat: error, I may only be applied to FHeqstat statements")
      // <&y2011.11.26.13:48:41& perhaps better refactor FHdoc language definition such that "sublanguages", such as equality statements, are separate types. This helps a more finegrained type declaration of functions, which may only operate on such fragments>
      case1Statement fhstat=:(FHpre2stat _)
                        | opsd == 0    = 0
                        | opsd > 0     = 1
                        where opsd = length (onePreStatDerivation fhstat question)
      case1Statement (FHeqstat _) = 0
      eqStat::Maybe FHstat
      eqStat      = find isEqStat fhdoc
      preStat::Maybe FHstat
      preStat     = find isPre2Stat fhdoc
      // assumption of onePreStatDerivation function: the given statement is the ONLY one in the axiom set. If predicates match, returns the number of names (without checking inequality!) satifying the question
      // <&y2011.11.26.13:46:06& perhaps it is better to cut the question of to the point of the number restriction, because this function does nothing with that info.>
      onePreStatDerivation::FHstat QuestionLang_ -> [FHconstant]
      onePreStatDerivation (FHpre2stat (fhpre, ([subjectFhstat], objects))) (PatLang (SharpestPat (sharpvar, NumRestrPat (Geq, numres, boundvar1, Literal (patpre,[FHconstant subjectPat, FHvar boundvar2])))))
            | not (boundvar1 == boundvar2)   = abort("onePreStatDerivation: error, boundvars in question not equal.")
            | not (sharpvar == numres)       = abort("onePreStatDerivation: error, sharpvar and numres in question not equal.")
            | (fhpre == patpre) && (subjectFhstat == subjectPat) = trace_loc objects
                                                                 = trace_loc []
            where
               trace_loc objs = trace ("onePreStatDerivation: result has length = " +++ (toString (length objs)) +++ "\n" +++ objs!!0) objs
               // get error in this one: trace_loc objs = trace ("onePreStatDerivation: result = " +++ (toString (map toString objs))) objs

/* #####################################################################################
   ### Translate answer in CTL to answer in NL
   ###
   ###
*/

AnswerInCTL2NL::(AnswerLang_, [Hurelan_cfnl]) -> [SentenceNL_]

AnswerInCTL2NL ((Folnuminqua (Sharpest (SharpestPat (sharpvar,(NumRestrPat (Geq, numres,boundvar1,(Literal (patpre,[(FHconstant subject),(FHvar boundvar2)])))))) answernumber)), hurelans_cfnl)
            | not (boundvar1 == boundvar2)   = abort("AnswerInCTL2NL: error, boundvars in question not equal.")
            | not (sharpvar == numres)       = abort("AnswerInCTL2NL: error, sharpvar and numres in question not equal.")
                                             = [SentenceNL ((toString subject) +++  " has not less than " +++ (toString answernumber) +++ " " +++ objectrole4sentence +++ ".")]
   where
      objectrole4sentence
            |  answernumber == 1 = objectrole
                                 = PluraliseNoun objectrole
      objectrole        = hurelan_cfnl.ent2_nl
      hurelan_cfnl      = (searchHurelan_cfnl patpre hurelans_cfnl)!!0 
// <&y2011.11.30.17:46:01& SHOULD DO proof that the choice for the ent2_nl is always correct given the assumptions. And make the latter explicit, or better: enforce them programmatically.>
// <&y2011.11.30.17:43:15& COULDDO (low priority) would be nice if you could use the NL generation of the question to construct the answer, I now do it separately, changing one, will not change the other.>

AlgorithmicDefenceGenerator::[Hurelan_cfnl] QuestionLang_ [Hurelan_cfnl]->QuestionLang_

/* AlgorithmicDefenceGenerator TextCTLbyComputer NL2CTLbridgeComputer AlgorithmicDefenceOfTextCTLbyComputer NL2CTLbridgePlayer -> AlgorithmicDefenceOfTextCTLbyPlayer

This function basically translated the algorithmic defence generated for the translation by the computer into the algorithmic defence of the translation generated by the player
*/

/* <&y2011.12.03.21:04:30& build in possibility for player to have the opposite order of the predicate generated by the computer, e.g. instead of is_parent_of, is_child_of. This has repercussions on other parts of the program as well!>

&y2011.12.03.21:05:41& However, for now the assumption is that the order is the same. If it isn't an error will be generated.
&y2011.12.03.21:06:04& Moreover it is assumed that the player chose the same natural langauge entity roles to define his hurelan_cfnl (the bridge from CTL to NL). 
<&y2011.12.03.21:06:58& in future, if that is not the case, go into dialogue with player to disambiguate>
&y2011.12.03.21:18:07& also assumed is that there is only one bridge info given, which directly applies to the question given. In future it may be required look up the right sentence in the bridge information.
*/

// &y2011.12.03.23:47:07& bugfree through compiler, but not tested yet
AlgorithmicDefenceGenerator [hurelan_cfnlComputer] (PatLang (SharpestPat (sharpvar, NumRestrPat (Geq, numres, boundvar1, Literal (fhpre, [FHconstant subject, FHvar boundvar2]))))) [hurelan_cfnlPlayer]
      | not (boundvar1 == boundvar2)   = abort("AlgorithmicDefenceGenerator: error, boundvars in question not equal.")
      | not (sharpvar == numres)       = abort("AlgorithmicDefenceGenerator: error, sharpvar and numres in question not equal.")
      | (h11 && h22) = PatLang (SharpestPat (sharpvar, NumRestrPat (Geq, numres, boundvar1, Literal (fhprePlayer, [FHconstant subject, FHvar boundvar2]))))
      | (h12 && h21) = abort("Order of predicate positions the opposite of that of the computer generated algorithmic defence, not implemented yet.")
                    = abort("Could not map player's CTL to computer generated CTL. Probably possible, with more interactivity, but not implemented yet.")
      where
         fhprePlayer = hurelan_cfnlPlayer.hra_f
         h11 = (hurelan_cfnlComputer.ent1_nl == hurelan_cfnlPlayer.ent1_nl)
         h22 = (hurelan_cfnlComputer.ent2_nl == hurelan_cfnlPlayer.ent2_nl)
         h12 = (hurelan_cfnlComputer.ent1_nl == hurelan_cfnlPlayer.ent2_nl)
         h21 = (hurelan_cfnlComputer.ent2_nl == hurelan_cfnlPlayer.ent1_nl)

