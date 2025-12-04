
# Adapted from Kery & Schaub (2012), written by Michael Schaub.

# Not exported

# Function to create a m-array based on capture-histories (CH) plus vector of frequencies.

ch2mArray <- function(CH, freq=1){
  CH <- as.matrix(CH)  # might be a data frame
  if(length(freq) == 1)
    freq <- rep(freq, nrow(CH))
  if(length(freq) != nrow(CH))
    stop("'freq' must have one value for each row of the capture history matrix.")
  nocc <- ncol(CH)
  if(any(freq < 0)) {
    # When did the loss occur?
    getLast <- function(x)
      max(which(x > 0))
    last <- apply(CH[freq < 0, ], 1, getLast)
    lost <- tabulate(last, nbins=nocc)
    freq <- abs(freq)
  } else {
    lost <- rep(0, nocc)
  }
  ma <- matrix(0, nocc, nocc+1)
    # First column and last row will be removed later
    # Last col is for number never-seen-again
  for(i in 1:nrow(CH)) {
    cht <- which(CH[i, ] != 0) # When was animal caught?
    # Fill in release/recapture data
    for(z in seq_along(cht[-1]))  # Does nothing if length(cht) = 1
      ma[cht[z], cht[z+1]] <- ma[cht[z], cht[z+1]] + freq[i]
  }
  # Marked animals never seen again:
  totCH <- sweep(CH, 1, freq, "*")
  ma[, nocc+1] <- colSums(totCH) - rowSums(ma) - lost
  # Remove 1st col (REcaptures on 1st occasion = all zeros)
  #  and last row (releases  on last occasion will never be recaptured).
  return(ma[-nocc, -1])
}

