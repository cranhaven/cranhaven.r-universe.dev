\name{RPatternJoin-package}
\alias{RPatternJoin-package}
\alias{RPatternJoin}
\docType{package}
\title{
  String Similarity Joins for Hamming and Levenshtein Distances
}
\description{
  This project is a tool for words edit similarity joins under small (\eqn{< 3}) edit distance constraints. 
  It works for Levenshtein distance and Hamming (with allowed insertions/deletions to the end) distance. 
}
\details{
  The package offers several similarity join algorithms, all of which can be accessed through the \code{\link[=similarityJoin]{similarityJoin}} function.
  The software was originally developed for edit similarity joins of short amino-acid/nucleotide sequences from Adaptive Immune Repertoires, 
  where the number of words is relatively large (\eqn{10^5-10^6}) and the average length of words is relatively small (\eqn{10-100}).
  The algorithms will work with any alphabet and any list of words, however, larger lists or word sizes can lead to memory issues.
}
\author{
  Daniil Matveev <dmatveev@sfsu.edu>
}
\examples{
library(RPatternJoin)

## Small example

similarityJoin(c("ABC", "AX", "QQQ"), 2, "Hamming", output_format = "adj_pairs")
#       [,1] [,2]
# [1,]    1    1
# [2,]    1    2
# [3,]    2    1
# [4,]    2    2
# [5,]    3    3


## Larger example

# The `edit_dist1_example` function generate a random list 
# of `num_strings` strings with the average string length=`avg_len`.
strings <- edit_dist1_example(avg_len = 25, num_strings = 5000)

# Firstly let's do it with `stringdist` package.
\donttest{
library(stringdist)
unname(system.time({
  which(stringdist::stringdistmatrix(strings, strings, "lv") <= 1, arr.ind = TRUE)
})["elapsed"])
# Runtime on macOS machine with 2.2 GHz i7 processor and 16GB of DDR4 RAM:
# [1] 63.773
}

# Now let's do it with similarityJoin function.
unname(system.time({
  similarityJoin(strings, 1, "Levenshtein", output_format = "adj_pairs")
})["elapsed"])
# Runtime on the same machine:
# [1] 0.105
}
\seealso{
  \code{\link[=similarityJoin]{similarityJoin}},
  \code{\link[=edit_dist1_example]{edit_dist1_example}}
}
\keyword{ package }
