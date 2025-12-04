# Calculation of rarefaction estimator (Mao Tau) used in EstimateS.

# See the EstimateS Users Guide:
# http://viceroy.eeb.uconn.edu/estimates/EstimateSPages/EstSUsersGuide/EstimateSUsersGuide.htm
# downloaded 2013-03-14.

# This corresponds to EstimateS output columns 3 & 6:
#   Sobs (Mao Tau)
# 	Sobs SD (Mao Tau)
# For the confidence interval, just use +/- 1.96 * SD

richRarefy <- function(incmat)  {
  # Calculates a sample-based rarefaction curve using Mao's tau estimator
  # incmat = matrix of incidences or counts, species x occasions
  # Returns: a matrix with columns for
  #  Mao's tau estimator
  #  approximate standard deviation

  incmat <- round(incmat) > 0  # turn counts into incidences

  H <- dim(incmat)[2]  # number of samples
  Sobs <- sum(rowSums(incmat) > 0) # number of sps observed; rows with all zeros don't count 
  # see Eqn 5 in Colwell+ 2004
   sj <- table(rowSums(incmat))   # Number of species encountered on 1, 2, 3,.. occasions
   s1 <- sj["1"] ; if(is.na(s1)) s1 <- 0  # Number of uniques
   s2 <- sj["2"] ; if(is.na(s2)) s2 <- 0  # Number of duplicates
   jay <- as.numeric(names(sj))
  # To estimate variance, we need an estimate of species richness; Colwell+ 2004 (Eqn 7)
  #   use a form of "Chao2":
  Stilde <- if(s2 > 0) Sobs + (H-1)*s1^2/(2*H*s2) else Sobs

  tau <- sigma <- numeric(H)
  for(h in 1:H) {
    smalljay <- jay[jay <= (H - h)]  # Values of j for which alfjh != 0
    alfjh <- c(
        exp(lfactorial(H - h) + lfactorial(H - smalljay) -
            lfactorial(H - h - smalljay) - lfactorial(H)),
        rep(0, length(jay)-length(smalljay)))
    # ifelse won't work here
    tau[h] <- Sobs - sum(alfjh*sj)
    sigma[h] <- sqrt(sum(sj*(1-alfjh)^2) - tau[h]^2/Stilde)
  }
  return(cbind(MaoTau = tau, SD = sigma))
}

