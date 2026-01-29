\docType{class}
\name{EARMGA_A}
\alias{EARMGA_A}
\alias{R6_EARMGA_A}
\title{EARMGA_A KEEL Association Rules Algorithm}
\description{
EARMGA_A Association Rules Algorithm from KEEL. 
}
\usage{
EARMGA_A(dat, seed, FixedLengthofAssociationRules, PopulationSize, 
  TotalNumberofEvaluations, DifferenceBoundaryNOTUSED, ProbabilityofSelection, 
  ProbabilityofCrossover, ProbabilityofMutation, 
  NumberofPartitionsforNumericAttributes)
}
\arguments{
\item{dat}{Dataset as a data.frame object}
\item{seed}{seed. Default value = 1286082570}
\item{FixedLengthofAssociationRules}{Fixed Length of Association Rules. Default value = 2}
\item{PopulationSize}{PopulationSize. Default value = 100}
\item{TotalNumberofEvaluations}{Total Number of Evaluations. Default value = 50000}
\item{DifferenceBoundaryNOTUSED}{Difference Boundary NOT USED. Default value = 0.01}
\item{ProbabilityofSelection}{Probability of Selection. Default value = 0.75}
\item{ProbabilityofCrossover}{Probability of Crossover. Default value = 0.7}
\item{ProbabilityofMutation}{Probability of Mutation. Default value = 0.1}
\item{NumberofPartitionsforNumericAttributes}{Number of Partitions for Numeric Attributes. Default value = 4}


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
\donttest{
#Load KEEL dataset
dat<-RKEEL::loadKeelDataset("car")

#Create algorithm
algorithm <- RKEEL::EARMGA_A(dat)

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
algorithm$writeCSV(paste0(tempdir(), "/myrules"))
}
}
\keyword{association rules}
