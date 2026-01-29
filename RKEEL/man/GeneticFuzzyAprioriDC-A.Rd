\docType{class}
\name{GeneticFuzzyAprioriDC_A}
\alias{GeneticFuzzyAprioriDC_A}
\alias{R6_GeneticFuzzyAprioriDC_A}
\title{GeneticFuzzyAprioriDC_A KEEL Association Rules Algorithm}
\description{
GeneticFuzzyAprioriDC_A Association Rules Algorithm from KEEL. 
}
\usage{
GeneticFuzzyAprioriDC_A(dat, seed, NumberofEvaluations, PopulationSize, 
  ProbabilityofMutation, ProbabilityofCrossover, ParameterdforMMACrossover, 
  NumberofFuzzyRegionsforNumericAttributes, UseMaxOperatorfor1FrequentItemsets, 
  MinimumSupport, MinimumConfidence)
}
\arguments{
\item{dat}{Dataset as a data.frame object}
\item{seed}{seed. Default value = 1286082570}
\item{NumberofEvaluations}{Number of Evaluations. Default value = 10000}
\item{PopulationSize}{Population Size. Default value = 50}
\item{ProbabilityofMutation}{Probability of Mutation. Default value = 0.01}
\item{ProbabilityofCrossover}{Probability of Crossover. Default value = 0.8}
\item{ParameterdforMMACrossover}{Parameterd for MMA Crossover. Default value = 0.35}
\item{NumberofFuzzyRegionsforNumericAttributes}{Number of Fuzzy Regions for Numeric Attributes. Default value = 3}
\item{UseMaxOperatorfor1FrequentItemsets}{Use Max Operator for 1 Frequent Itemsets. Default value = "false"}
\item{MinimumSupport}{Minimum Support. Default value = 0.1}
\item{MinimumConfidence}{Minimum Confidence. Default value = 0.8}


}
\value{
A arules class with the  Association Rules for both \code{dat} dataset.
}

\details{

\code{$run()}  Run algorith \cr \cr
\code{$showRules(numRules)}  Show a number of rules. By default all rules. \cr \cr
\code{$getInterestMeasures()}  Return a data.frame with all interest measures of set rules. \cr \cr
\code{$sortBy(interestMeasure)}  Order set rules by interest measure. \cr \cr
\code{$writeCSV(fileName, sep)}  Create CSV file with set rules. Default \code{fileName="rules" sep=","} \cr \cr
\code{$writePMML(fileName)}  Create PMML file with set rules. Default \code{fileName="rules"} \cr \cr
\code{$addInterestMeasure(name, colName)}  Add interest measures to set rules. Some interest measures supported: \cr \cr

"allConfidence" (Omiencinski, 2003) \cr \cr
"crossSupportRatio", cross-support ratio (Xiong et al., 2003) \cr \cr
"lift", interest factor (Brin et al. 1997) \cr \cr
"support", supp (Agrawal et al., 1996) \cr \cr
"addedValue", added Value, AV, Pavillon index, centered confidence (Tan et al., 2002) \cr \cr
"chiSquared", X^2 (Liu et al., 1999) \cr \cr
"certainty", certainty factor, CF, Loevinger (Berzal et al., 2002) \cr \cr
"collectiveStrength" \cr \cr
"confidence", conf (Agrawal et al., 1996) \cr \cr
"conviction" (Brin et al. 1997) \cr \cr
"cosine" (Tan et al., 2004) \cr \cr
"coverage", cover, LHS-support \cr \cr
"confirmedConfidence", descriptive confirmed confidence (Kodratoff, 1999) \cr \cr
"casualConfidence", casual confidence (Kodratoff, 1999) \cr \cr
"casualSupport", casual support (Kodratoff, 1999) \cr \cr
"counterexample", example and counterexample rate \cr \cr
"descriptiveConfirm", descriptive-confirm (Kodratoff, 1999) \cr \cr
"doc", difference of confidence (Hofmann and Wilhelm, 2001) \cr \cr
"fishersExactTest", Fisher's exact test (Hahsler and Hornik, 2007) \cr \cr
"gini", Gini index (Tan et al., 2004) \cr \cr
"hyperLift" (Hahsler and Hornik, 2007) \cr \cr
"hyperConfidence" (Hahsler and Hornik, 2007) \cr \cr
"imbalance", imbalance ratio, IR (Wu, Chen and Han, 2010) \cr \cr
"implicationIndex", implication index (Gras, 1996) \cr \cr
"improvement" (Bayardo et al., 2000) \cr \cr
"jaccard", Jaccard coefficient (Tan and Kumar, 2000) \cr \cr
"jMeasure", J-measure, J (Smyth and Goodman, 1991) \cr \cr
"kappa" (Tan and Kumar, 2000) \cr \cr
"klosgen", Klosgen (Tan and Kumar, 2000) \cr \cr
"kulczynski" (Wu, Chen and Han, 2007; Kulczynski, 1927) \cr \cr
"lambda", Goodman-Kruskal lambda, predictive association (Tan and Kumar, 2000) \cr \cr
"laplace", L (Tan and Kumar 2000) \cr \cr
"leastContradiction", least contradiction (Aze and Kodratoff, 2004 \cr \cr
"lerman", Lerman similarity (Lerman, 1981) \cr \cr
"leverage", PS (Piatetsky-Shapiro 1991) \cr \cr
"mutualInformation", uncertainty, M (Tan et al., 2002) \cr \cr
"oddsRatio", odds ratio alpha (Tan et al., 2004) \cr \cr
"phi", correlation coefficient phi (Tan et al. 2004) \cr \cr
"ralambrodrainy", Ralambrodrainy Measure (Ralambrodrainy, 1991) \cr \cr
"RLD", relative linkage disequilibrium (Kenett and Salini, 2008) \cr \cr
"sebag", Sebag measure (Sebag and Schoenauer, 1988) \cr \cr
"support", supp (Agrawal et al., 1996) \cr \cr
"varyingLiaison", varying rates liaison (Bernard and Charron, 1996) \cr \cr
"yuleQ", Yule's Q (Tan and Kumar, 2000) \cr \cr
"yuleY", Yule's Y (Tan and Kumar, 2000) \cr \cr
For more information see ?arules::interestMeasure  \cr \cr
 
}
\examples{
#Load KEEL dataset
dat<-RKEEL::loadKeelDataset("car")

#Create algorithm
algorithm <- RKEEL::GeneticFuzzyAprioriDC_A(dat)

#Run algorithm
algorithm$run()

#Rules in format arules
algorithm$rules

#Show a number of rules
algorithm$showRules(2)

#Return a data.frame with all interest measures of set rules
algorithm$getInterestMeasures()

#Add interst measure YuleY to set rules
algorithm$addInterestMeasure("YuleY","yulesY")

#Sort by interest measure lift
algorithm$sortBy("lift")

#Save rules in CSV file
\donttest{algorithm$writeCSV(paste0(tempdir(), "/myrules"))}
}
\keyword{association rules}
