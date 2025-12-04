\name{distTestData}
\alias{distTestData}
\docType{data}
\title{
An artificial data set to test distance/dissimilarity measures
}
\description{
Artificial data for counts of 32 species at 5 sites.
}
\usage{data(distTestData)}
\format{
  A matrix with 5 rows (sites), labelled A-B, and 32 columns (species).
}
\details{
Sites A, B and C each have 16 species and 158 individuals.\cr
Sites A and B have the same species, but the numbers of each are different.\cr
Site C has a completely different set of 16 species, but the same number of individuals.\cr
Site D has the same species in the same proportions as A, but twice the number of individuals.\cr
Site E has 32 species and 316 individuals.
}
\source{
Artificial data.
}

\examples{
data(distTestData)
# Display the data:
print(t(distTestData))

distShell(distTestData, distJaccard)
#     A   B   C   D
# B 0.0            
# C 1.0 1.0        
# D 0.0 0.0 1.0    
# E 0.5 0.5 0.5 0.5
# Jaccard index ignores counts, so sees AB, AD and BD as identical (zero distance).

round(distShell(distTestData, distMorisitaHorn), 2)
#      A    B    C    D
# B 0.89               
# C 1.00 1.00          
# D 0.00 0.89 1.00     
# E 0.33 0.93 0.33 0.33
# Morisita-Horn index considers proportions, so AD are identical but not AB or BD.

round(distShell(distTestData, distBrayCurtis), 2)
#      A    B    C    D
# B 0.84               
# C 1.00 1.00          
# D 0.33 0.84 1.00     
# E 0.33 0.89 0.33 0.50
# Bray-Curtis index is affected by abundance as well as proportions, so AD are no longer identical.
# Site C only overlaps with D, so AC, BC and CD are 1.00 for all indices.
# Site E overlaps with all the others, so AE, BE, CE and DE all lie between 0 and 1 for all indices.
}
\keyword{datasets}
