\name{world}
\alias{world}
\docType{data}
\title{
Model parameters
}
\description{
Model parameters that hold during the simulation for the entire lineage
}
\usage{data("world")}
\format{
  The format is:
List of 64
 $ nAgents                         : num 2
 $ deathAge                        : num 2000
 $ procreationAge                  : num 0.55
 $ crossover                       : logi TRUE
 $ replace                         : logi TRUE
 $ weigh                           : logi TRUE
 $ distinctions                    : num [1:9] 2 2 2 2 2 9 9 9 9
 $ wordLength                      : int [1:3] 8 9 10
 $ vowels                          : chr [1:6] "a" "e" "i" "o" ...
 $ consonants                      : chr [1:15] "b" "d" "f" "g" ...
 $ nNouns                          : num 499
 $ nVerbs                          : num 199
 $ proportionIntrans               : num 0.2
 $ linkingPreference               : num 5
 $ local                           : logi TRUE
 $ useCommonGround                 : logi TRUE
 $ commonGroundStart               : num 3
 $ dahlS                           : num [1:4] 21 10 21 44
 $ dahlA                           : num [1:4] 38 22 33 7
 $ dahlO                           : num [1:4] 3 3 10 84
 $ oddsNewA                        : num 0.0333
 $ oddsNewOther                    : num 0.25
 $ referenceNoise                  : num 0.2
 $ roleNoise                       : num 0.3
 $ nEvents                         : int [1:11] 10 11 12 13 14 15 16 17 18 19 ...
 $ nTurns                          : int [1:16] 5 6 7 8 9 10 11 12 13 14 ...
 $ talkAge                         : num 0.05
 $ turnChange                      : num [1:2] 2 1
 $ personTopicality                : num [1:4] 2 1 2 2
 $ topicContinuity                 : num [1:2] 3 1
 $ checkSuccess                    : logi TRUE
 $ solutionMethod                  : chr "bestMarker"
 $ reductionFrequencyThreshold     : num 0.05
 $ reductionCollostructionThreshold: num 3
 $ reductionRecencyThreshold       : num 2
 $ formSetFrequency                : num 3
 $ suffixThreshold                 : num 6
 $ refCheck                        : logi TRUE
 $ referenceThreshold              : num 4
 $ generalization                  : logi TRUE
 $ firstInFirstOut                 : logi TRUE
 $ distinctiveness                 : num 0.05
 $ candidateScoring                : chr "all"
 $ frequency                       : chr "relative"
 $ activationImpact                : num 0.2
 $ collostructionImpact            : num 0.2
 $ semanticWeightImpact            : num 0.1
 $ economyImpact                   : num 0.1
 $ recencyDamper                   : num 5
 $ activationNoise                 : num 2
 $ functionBlocking                : logi TRUE
 $ wordOrder                       : logi TRUE
 $ topicCopy                       : logi TRUE
 $ semUpdateAge                    : num 0.5
 $ erosion                         : logi TRUE
 $ erosionMax                      : num 2
 $ formBlocking                    : logi TRUE
 $ desemanticization               : logi TRUE
 $ desemanticizationCeiling        : num 0.4
 $ desemanticizationPower          : num 2
 $ minimalSpecification            : num 1
 $ verbalRoleMarker                : logi FALSE
 $ semUpdateThreshold              : num 0.02
 $ saveAll                         : logi FALSE
}
\details{
\code{nAgents}:    number of founding agents

\code{deathAge}:    age, in number of utterances, at which agents die

\code{procreationAge}:    point at which agents procreate (relative to their death age). If NA, no offspring. Best to procreate after semUpdate;)

\code{crossover}:    If true, lexicon of off spring is combination of those of parents. If false, each parent will get a child with identical lexicon 

\code{replace}:    Should minor modifications be made to non-used words?

\code{weigh}:    In comparing meanings and determining whose the actor, should meaning dimensions be equally important (F) or should first dimension be more important than second, but less important than second plus third, etc. (T). Slows down simulation in combination with high number of events per situation (>10)

The following set of parameters applies to the lexicon specifically:

\code{distinctions}:    dimensionality and distinctionality of meaning representations (distinctions are normalized to 0--1 range).

\code{wordLength}:    initial length of words, can be single valued or range.

\code{vowels}:    vowels of alphabet constituting the words

\code{consonants}:    consonants of alphabet constituting the words

\code{nNouns}:    number of nouns in the lexicon

\code{nVerbs}:    number of verbs in the lexicon

\code{proportionIntrans}:    proportion of intransitive verbs in both lexicon and events. Probably .5 in real life, but smaller in the interest of argument marking

\code{linkingPreference}:    preference of external (internal) predicate role for higher (lower) values ("prominent performers"). linkingPreference is odds of highest against lowest role/value. 1 is no preference.

\code{local}:    Do agents have the words/the possibility to refer to themselves?

The following set of parameters applies to the generation of the situational context, i.e., the set of target and distractor events (cf. Steels).

\code{useCommonGround}:    Do speech participants share a common ground or are all words/concepts equally likely and accessible.

\code{commonGroundStart}:   number of elements (excluding speech participants) that are present in common ground when conversation starts. Elements are randomly selected from lexicon.

\code{dahlS}:    odds for intransitive subject to be 1, 2, 3Animate, and 3Inanimate person respectively (based on Dahl 2000, 45-51)

\code{dahlA}:    odds for external role to be 1, 2, 3Animate, and 3Inanimate person. First three numbers are summed if local==F. 

\code{dahlO}:    odds for internal role to be 1, 2, 3Animate, and 3Inanimate person. 

\code{oddsNewA}:    odds for a non common-ground element to enter as A argument of one of the events in the situation (element will be added to the common ground if discussed; cf. DuBois 1987: 828, Table 7)

\code{oddsNewOther}:    odds for a non common-ground element to enter as S or O argument of one of the events in the situation (element will be added to the common ground if discussed)

\code{referenceNoise}:    how much "referential" noise is there in the world (0--1)? The less noise, the closer the world matches the concepts and relations in the language.

\code{roleNoise}:    How much noise is there in the world with respect to the event roles that nouns are expected and found to perform. 

\code{nEvents}:    Number of events that are ongoing in speech situation, one of which is selected to talk about. If set to 1, no distractor events occur.  

The following set of parameters applies to the conversations two agents have:

\code{nTurns}:    What is the range of communicative turns conversations consist of (before common ground is reset)

\code{talkAge}:    At which point (relative to their death age) do agents start to talk? (Until then, they only listen) If zero, less learning from parents

\code{turnChange}:    odds for speech-act participants to change speech-act roles

\code{personTopicality}:    Preference for speaker, addressee, animate third person, and inanimate third person respectively to be the topic of the utterance and participant in a situation (based on Dahl's S and A numbers)

\code{topicContinuity}:    odds for continuing with the same topic vs starting a new one

The following set of parameters applies to the production process:

\code{checkSuccess}:    Should expected recovery of meaning be checked? (cf. Aristar for "typing" scores)

\code{solutionMethod}:    If check success shows utterance should be elaborated, how is this done? Options: firstFail, bestMarker, worstPerformer, random, secondArgument, internal, external, both

\code{reductionFrequencyThreshold}:    Relative frequency threshold at which forms get reduced. 

\code{reductionCollostructionThreshold}:    Absolute collostruction-frequency threshold at which forms get reduced.

\code{reductionRecencyThreshold}:    idem for recency

\code{formSetFrequency}:    number of times an item has to be used before its form is set, after which its representation will no longer change

\code{suffixThreshold}:    productionEffort threshold (in number of characters) at which words markers suffixed to their host

\code{refCheck}:    Should referential threshold be reached for words to refer?

\code{referenceThreshold}:    production effort (in number of characters)  necessary for an utterance to be sufficiently referential (a la Ariel). If lower, a more expressive expression is added sentence first.

\code{generalization}:    Should agents try to derive generalizations from the tendencies they observe? Applies from second generation onwards only (cf. Yang)

\code{firstInFirstOut}:    Is utterance production incremental? (cf. Bock and Levelt)

The following set of parameters applies both to the production and interpretation process:

\code{distinctiveness}:    If two forms are similar in meaning (or in role typing in case of global marking), how big should the difference be for the speaker to think the distinction is sufficiently clear?

\code{candidateScoring}:    In what order should candidates be considered (first one to suffice is selected): by activation, frequency, match, economy, collostruction, all. 

\code{frequency}:    If frequency plays a role, should it do so absolutely or relatively (i.e. frequency as argument, or role or index marker)

\code{activationImpact}:    if candidateOrdering=='all', how should (rescaled) activation be weighed with respect to match? Activation is function of frequency and recency. Impact==1: equally, impact below 1: impact times less important, impact above 1: impact times more important.

\code{collostructionImpact}:    If candidateOrdering=='all', how should (rescaled) collostruction frequency be weighed with respect to match? Also used by VERBMORPHOLOGY

\code{semanticWeightImpact}:    If candidateOrdering=='all', how should semantic weight be weighed with respect to match (given Grice: do not say more than necessary)

\code{economyImpact}:    If candidateOrdering=='all', how should economy be weighed with respect to match (given Grice: do not say more than necessary)

\code{recencyDamper}:    decreases activation of most recent items [RESCALE(jitter(log((frequency+1)/(recency+1+recencyImpact)), factor=activationNoise))]

\code{activationNoise}:    noise factor that is added to activation values of items [RESCALE(jitter(log((frequency+1)/(recency+1+recencyImpact)), factor=activationNoise))]

\code{functionBlocking}:    Should frequent usage for some function (argument, role marker, index marker) inhibit other functions? (only applies if frequency==relative). And: should reference to certain person values block others?

\code{wordOrder}:    Should agents try to use word-order generalizations to mark/determine roles?

\code{topicCopy}:    Should a (pronominal) copy of a reestablished topic be put adjacent to the verb (a la Givon; only applies if topicFirst has been derived)? 

The following set of parameters applies both to the process of language change

\code{semUpdateAge}:    At which point (relative to their death age) do agents update their lexical representations? Should be lower than procreationAge for cultural evolution to apply

\code{erosion}:    Should forms erode?

\code{erosionMax}:   How short may form representations become in number of characters?

\code{formBlocking}:    Should agent refrain from reducing forms if this leads to ambiguity?

\code{desemanticization}:    Should forms desemanticize?

\code{desemanticizationCeiling}:    proportion of utterances in which an item occurs at which it desemanticizes maximally (.3?)

\code{desemanticizationPower}:    Development of thresholds for subsequent dimensions to be removed. 1 for linear development. Best between 1 and 2? The lower, the more difficult to desemanticize, as the threshold develops linearly to the same target (desemanticizationCeiling)

\code{minimalSpecification}:    minimum number of dimensions along which referential items have to be specified (in the presence of other candidate expressions for same person). If null, words will be replaced once meaningless

\code{verbalRoleMarker}:    Can verb markers be distinctive for role (within person)? Cf. Bhat...

\code{semUpdateThreshold}:    proportion of number of utterances in which a construction has to occur before it is fused/lexicalized


The following parameter is for data management:

\code{saveAll}:    Should usageHistory be stored in graveyard?

}
\references{
Ariel, M. (1999). The development of person agreement markers: from pronouns to higher accessibility markers. In M. Barlow & S. Kemmer (Eds.), Usage based models of language (pp. 197-260). Stanford: CSLI.

Aristar, A.R. (1997). Marking and hierarchy. Types and the grammaticalization of case markers. Studies in Language, 21 (2), 313-368.

Bhat, D.N.S. (2004), Pronouns. Oxford [etc.]: Oxford University Press.

Croft, W. (2000). Explaining language change: An evolutionary approach. Harlow etc.: Longman.

Dahl, O. (2000). Egophoricity in discourse and syntax. Functions of Language, 7 (1), 37-77.

Deacon, T. (1997). The symbolic species. London: Penquin.

Dowty, D. (1991). Thematic proto-roles and argument selection. Language 67 (3), 547-619.

Du Bois, J.W. (1987), "The discourse basis of ergativity". Language 63 (4): 805-855

Gaerdenfors, P. (2000). Conceptual spaces: The geometry of thought. Cambridge, MA: MIT.

Givon, T. (1995). Functionalism and grammar. Amsterdam/Philadelphia: John Benjamins.

Grice, H. P. (1975). Logic and conversation. In P. Cole & J. L. Morgan (Eds.), Syntax and semantics: Speech acts (Vol. 3, pp. 41-58). New York: Academic Press.

Heine, B. & Kuteva, T. (2007). The genesis of grammar. a reconstruction. Oxford: Oxford University Press.

Hopper, P. J. (1987). Emergent grammar. In Proceedings of BLS (Vol. 13, pp. 139-157).

Jackendoff, R. (2002). Foundations of language: Brain, meaning, grammar, evolution. Oxford: Oxford University Press.

Levelt, W.J.M. (1989). Speaking. From intention to articulation. Cambridge, MA: MIT Press.

Smith, K. & Kirby, S. (2008). Cultural evolution: Implications for understanding the human language faculty and its evolution. Phil. Trans. R. Soc. B, 363, 3591-3603.

Steels, L. (1997). Constructing and sharing perceptual distinctions. Machine Learning, ECML-97, 4-13.

Tomasello, M. (2003). Constructing a language: A usage-based theory of language acquisition. Cambridge, MA: Harvard University Press.

Van Valin, R. (1999). Generalized semantic roles and the syntax-semantics interface. In F. Corblin, C. Dobrovie-Sorin, & J.-M. Marandin (Eds.), Empirical issues in formal syntax and semantics 2 (pp. 373-389). The Hague: Thesus.

Yang, Ch. (2016), The price of linguistic productivity. Cambridge, MA: MIT Press.
}
\examples{
length(world)
head(world, 10)
}
\keyword{datasets}
