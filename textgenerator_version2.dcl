definition module textgenerator_version2

import StdEnv, StdGeneric, StdMaybe, gast, genToFromString

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

//CTLspecifcs = CTLs, used for holding information that can differ among Folminqua CTLs. Uses include: define translations between CTLs.
::CTLspecific_ = CTLs_FHpre FHpre_ | CTLs_FHconstant Constant_

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

::BridgeStats_    :== [BridgeStat_]
::BridgeStat_     = EntityStat EntityStat_ | HurelanStat HurelanStat_
::EntityStat_     =  {  ent_ctl :: String,
                        ent_nl  :: String
                     }
::HurelanStat_    =  {  hra_ctl :: String,
                        ent1_nl :: String,
                        ent2_nl :: String
                     }

/* #####################################################################################
   ### Answer Language
   ### See swiftfososc(!una).pdf specification
   ###
*/

::AnswerLang_ :== Folnuminqua_ // just for completeness: within this namespace PatLang suffices, but in a bigger namespace with other pattern languages it may be required to disambiguate by choosing the longer name.
::Folnuminqua_    = Folnuminqua SharpestStat_
::SharpestStat_   = Sharpest SharpestPatStat_ Int


// <&y2011.11.23.12:30:56& improve question so that people should give the sharpest answer>

::SentenceNL_ = SentenceNL String

AlgorithmicDefenceGenerator::BridgeStats_ QuestionLang_ BridgeStats_ -> QuestionLang_
export2QuestionLangScalaFormat::QuestionLang_ -> String
Reasoner::FHdoc QuestionLang_ -> (Maybe AnswerLang_, String)
// <&y2012.02.06.19:58:00& uncomment the following after debugging!>
AnswerInCTL2NL::(AnswerLang_, BridgeStats_) -> [SentenceNL_]
