# Biodiversity indices

# Where possible, these give output in units of species,
#  conceptually the number of common species in the community.

# Inverse Simpson's index aka Hill's N2
# =====================================

biodSimpson <- function(abVec, correct=TRUE) {
  # abVec = vector of counts, one element per species.
  #   Other measures of abundance, eg biomass, can be used if correct=FALSE.
  # correct: if TRUE, small sample correction is applied, cf Hurlbert,
  #   rather than Hill's N2.
  # Sanity check:
  if(any(abVec < 0))
    stop("Negative abundances are not recognised.")
  # Convert a matrix into a vector and round: 
  if(correct)
    abVec <- round(abVec)
  if(is.matrix(abVec) || is.data.frame(abVec))
    abVec <- rowSums(abVec)

  N <- sum(abVec)
  if(correct) {
    1 / sum((abVec * (abVec - 1)) / (N * (N - 1))) 
  } else {
    1 / sum(abVec^2 / N^2)
  }
}

# Exponential Shannon's index aka Hill's N1
# =========================================

biodShannon <- function(abVec) {
  # abVec = vector of measures of abundance,
  #   eg biomass or counts, one element per species.
  # Sanity check:
  if(any(abVec < 0))
    stop("Negative abundances are not recognised.")

  # Convert a matrix into a vector: 
  if(is.matrix(abVec) || is.data.frame(abVec))
    abVec <- rowSums(abVec)

  p <- abVec[abVec > 0] / sum(abVec)
  exp(-sum(p * log(p)))
}

# Inverse Berger Parker index aka Hill's N[Inf]
# =============================================

biodBerger <- function(abVec) {
  # abVec = vector of measures of abundance,
  #   eg biomass or counts, one element per species.
  # Sanity check:
  if(any(abVec < 0))
    stop("Negative abundances are not recognised.")

  # Convert a matrix into a vector: 
  if(is.matrix(abVec) || is.data.frame(abVec))
    abVec <- rowSums(abVec)

  sum(abVec) / max(abVec)
}

# Exponential Brillouin index
# ===========================

biodBrillouin <- function(cntVec) {
  # cntVec = vector of counts, one element per species.
  # Sanity check:
  if(any(cntVec < 0))
    stop("Negative abundances are not recognised.")
  # Convert a matrix into a vector: 
  if(is.matrix(cntVec) || is.data.frame(cntVec))
    cntVec <- rowSums(cntVec)
  cntVec <- round(cntVec)
  N <- sum(cntVec)
  exp((lfactorial(N) - sum(lfactorial(cntVec))) / N)
}

