\name{MoLE-package}
\alias{MoLE-package}
\alias{MoLE}
\docType{package}
\title{
\packageTitle{MoLE}
}
\description{
\packageDescription{MoLE}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{MoLE}
\packageIndices{MoLE}
Set the model parameters in \code{world}. 
Found a new population (\code{FOUND}). 
Run a simulation (\code{RUN}).

For language to change (and argument-marking grammar to develop), the simulation has to run for several hours.

}
\author{
\packageAuthor{MoLE}

Maintainer: \packageMaintainer{MoLE}
}
\references{
Ariel, M. (1999),  The development of person agreement markers: from pronouns to higher accessibility markers. In Barlow & Kemmer (Eds.), Usage based models of language (pp. 197-260),  Stanford: CSLI.

Aristar, A.R. (1997),  Marking and hierarchy. Types and the grammaticalization of case markers. Studies in Language, 21 (2), 313-368.

Arbib, M. A.  (2015), Language evolution. An emergentist perspective. In MacWhinney and  OGrady (eds), The Handbook of Language Emergence. West Sussex, UK: Wiley/Blackwell, pp. 600-623.

Bhat, D.N.S. (2004), Pronouns. Oxford [etc.]: Oxford University Press.

Bickerton, D. (1981),  Roots of language. Ann Arbor, Mich.: Karoma.

Bybee, J. (2010), Language, usage, and cognition. Cambridge, UK: Cambridge University Press.

Croft, W. (2000),  Explaining language change: An evolutionary approach. Harlow etc.: Longman.

Dahl, O. (2000),  Egophoricity in discourse and syntax. Functions of Language, 7 (1), 37-77.

Deacon, T. (1997),  The symbolic species. London: Penquin.

Dowty, D. (1991),  Thematic proto-roles and argument selection. Language 67 (3): 547-619.

Dryer, M. S. (2002),  Case distinctions, rich verb agreement, and word order type. Theoretical Linguistics 28: 151-157.

Dryer, M. S. (2013),  Order of subject, object and verb. In Dryer & Haspelmath (Eds.), The World Atlas of Language Structures Online. Leipzig: Max Planck Institute for Evolutionary Anthropology.

Du Bois, J.W. (1987), The discourse basis of ergativity. Language 63 (4): 805-855

Gaerdenfors, P. (2000),  Conceptual spaces: The geometry of thought. Cambridge, MA: MIT.

Givon, T. (1995),  Functionalism and grammar. Amsterdam/Philadelphia: John Benjamins.

Grice, H. P. (1975),  Logic and conversation. In P. Cole & J. L. Morgan (Eds.), Syntax and semantics: Speech acts (Vol. 3, pp. 41-58),  New York: Academic Press.

Heine, B. & Kuteva, T. (2007),  The genesis of grammar. A reconstruction. Oxford: Oxford University Press.

Hopper, P. J. (1987),  Emergent grammar. In Proceedings of BLS (Vol. 13, pp. 139-157), 

Jackendoff, R. (2002),  Foundations of language: Brain, meaning, grammar, evolution. Oxford: Oxford University Press.

Levelt, W.J.M. (1989),  Speaking. From intention to articulation. Cambridge, MA: MIT Press.

Lestrade, S. (2010), The space of case. PhD thesis, Radboud University Nijmegen.

Lestrade, S. (2015a), The interaction of argument-marking strategies. In: S. Lestrade, P. de Swart, and L. Hogeweg (eds), Addenda. Artikelen voor Ad Foolen. Radboud University Nijmegen, 251-256.

Lestrade, S. (2015b), Simulating the development of bound person marking. In: H. Baayen, et al. (eds), Proceedings of the 6th Conference on Quantitative Investigations in Theoretical Linguistics. Tuebingen: University of Tuebingen.

Lestrade, S. (2015c), A case of cultural evolution: The emergence of morphological case. Linguistics in the Netherlands [AVT 32]: 105-115.

Lestrade, S. (2016), The emergence of argument marking. In: S.G. Roberts et al.(eds) The Evolution of Language: Proceedings of the 11th International Conference (EVOLANG11)

Saffran, J.R. (2001), Statistical language learning: Mechanisms and constraints. Current Directions in Psychological Science 12(4): 110-114.

Smith, K. & Kirby, S. (2008),  Cultural evolution: Implications for understanding the human language faculty and its evolution. Phil. Trans. R. Soc. B, 363, 3591-3603.

Steels, L. (1997),  Constructing and sharing perceptual distinctions. Machine Learning, ECML-97, 4-13.

Tomasello, M. (2003),  Constructing a language: A usage-based theory of language acquisition. Cambridge, MA: Harvard University Press.

Van Valin, R. (1999),  Generalized semantic roles and the syntax-semantics interface. In F. Corblin, C. Dobrovie-Sorin, & J.-M. Marandin (Eds.), Empirical issues in formal syntax and semantics 2 (pp. 373-389),  The Hague: Thesus.

Yang, Ch. (2016), The price of linguistic productivity. Cambridge, MA: MIT Press.

}
\keyword{ language evolution }
\keyword{ language change }
\keyword{ argument marking }
\keyword{ simulation }

\examples{
\dontrun{
FOUND()
RUN(.00001)
(situation=SITUATION(1))
(proposition=PROPOSITION(1, situation))
(prep=PREPARE(1, proposition, situation)) 
(utterance=PRODUCE(1, prep))
(interpretation=INTERPRET(2, utterance, situation))
head(population[[1]]$nouns)
}
}
