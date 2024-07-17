\name{daRoll}
\alias{daRoll}
\docType{data}
\title{Voting records for the Healthy Forests Restoration Act in 2003 and the associated characteristics of congressmen}

\description{This data set contains the voting records for the Healthy Forests Restoration Act in 2003, as used in Sun (2006). The characteristics of individual congressmen are also included. There are 537 observations and 22 variables.

\tabular{ll}{
\code{state} \tab state name for a congressman \cr
\code{district} \tab district for a congressman; 0 for senators  \cr
\code{name} \tab  Family name of a congressman \cr
\code{voteMay} \tab  voting record in May 2003 in the House; 1 if yes, 0 if no, and NA if not voted \cr
\code{voteNov} \tab  voting record in Nov 2003 in both the House and Senate \cr 
\code{RepParty} \tab Dummy equals one if Republican  \cr
\code{East} \tab Regional dummy for 11 northeastern states  \cr
\code{West} \tab Regional dummy for 11 western states  \cr
\code{South} \tab Regional dummy for 13 southern states  \cr
\code{PopDen} \tab  Population density - 1000 persons per km2\cr  
\code{PopRural} \tab  Population density per km2 \cr
\code{Edu} \tab  Percentage of population over 25 with a Bachelor's degree \cr
\code{Income} \tab  Median family income ($1,000) \cr
\code{FYland} \tab  Percentage of federal lands in total forestlands 2002 \cr
\code{Size} \tab Value of shipments of forest industry 1997 (million dollars)  \cr     
\code{ContrFY} \tab Contribution from forest firms (1,000 dollars)  \cr
\code{ContrEN} \tab Contribution from environmental groups (1,000 dollars)  \cr
\code{Sex} \tab Dummy equals one if male  \cr
\code{Lawyer} \tab Dummy equals one if lawyer \cr
\code{Member} \tab Dummy equals one if a committee member for the HFRA \cr   
\code{Year} \tab Number of years in the position  \cr
\code{Chamber} \tab Dummy equals one if House and zero if Senate  \cr
}
}
\usage{data(daRoll)}

\format{A data frame object with 537 rows and 22 variables. This is a cross-sectional dataset that are generating from merging several raw datasets.}

\details{This is the combinded final data set used in the study of Sun (2006).}

\source{See Table 1 in Sun (2006) for detail.}

\references{
Sun, C. 2006. A roll call analysis of the Healthy Forests Restoration Act and constituent interests in fire policy. Forest Policy and Economics 9(2):126-138.
}

\seealso{\code{\link{glm}}; \code{\link{maBina}}.}

\examples{

# generate four datasets used in Sun (2006)
data(daRoll)
xn <- c('RepParty', 'East', 'West', 'South', 'PopDen', 
  'PopRural', 'Edu', 'Income', 'FYland', 'Size', 
  'ContrFY', 'ContrEN', 'Sex', 'Lawyer', 'Member', 'Year', 'Chamber')

f1 <- daRoll[!is.na(daRoll$voteMay), c('voteMay', xn)]
f2 <- daRoll[!is.na(daRoll$voteNov) & daRoll$Chamber == 1, c('voteNov', xn)]
f3 <- daRoll[!is.na(daRoll$voteNov), c('voteNov', xn)]
f4 <- daRoll[!is.na(daRoll$voteNov) & daRoll$RepParty == 0, c('voteNov', xn)]
rownames(f1) <- 1:nrow(f1); rownames(f2) <- 1:nrow(f2)
rownames(f3) <- 1:nrow(f3); rownames(f4) <- 1:nrow(f4)
colnames(f1)[1] <- colnames(f2)[1] <- 'Vote'
colnames(f3)[1] <- colnames(f4)[1] <- 'Vote'
dim(f1); dim(f2); dim(f3); dim(f4)
tail(f3) 
}
\keyword{datasets}