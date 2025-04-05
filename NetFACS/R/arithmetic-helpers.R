# helper functions used internally by NetFACS

# Pointwise Mutual Information: 
pmi <- function(pAandB, pA, pB) {
  log2(pAandB / (pA * pB))
}

# normalized pmi; ranges between -1 and 1
# -1 for never occurring together, 0 for independence, and +1 for complete co-occurrence.
npmi <- function(pmiAB, pAandB) {
  pmiAB / (-log2(pAandB))
}

# Information entropy
H <- function(p) {
  -sum(p * log2(p))
}

# Maximum entropy
Hmax <- function(m) {
  log2(nrow(m))
}

# calculate entropy from matrix
matrix_entropy <- function(m) {
  mcombs <- apply(m, 1, paste, collapse = "")
  p <- table(mcombs) / length(mcombs)
  H(p)
}