#' Compute the fraction of sites fixed between populations
#'
#' This function will compute the fraction of sites that constitute a fixed
#' difference between populations.
#'
#' More precisely, we define fixed differences as sites where the number of
#' minor-allele reads of a given population is equal to the total coverage of
#' that population and equal to zero in the other population.
#'
#' For models with two populations, this function compares the two present-day
#' populations. For models with four populations, this function performs a
#' pairwise comparison of the populations at each of the locations. For the
#' models with four populations, we also assess the fraction of sites that
#' represent a fixed difference between any of the populations and the remaining
#' three populations.
#'
#' @param minor is a matrix with the number of minor-allele reads. Each row of
#'   the matrix is a different population and each column a different site.
#' @param total is a matrix with the total coverage. Each row of the matrix is a
#'   different population and each column a different site.
#' @param nPops is an integer indicating the total number of populations.
#'
#' @return a numeric vector with a single entry when `nPops` is equal to 2 or
#'   with three entries when `nPops` is set to 4.
#'
#' @keywords internal
#'
#' @export
fixed <- function(minor, total, nPops) {

  # check if the input is in the correct format
  if(nrow(minor) != nPops | nrow(total) != nPops)
    stop(paste("Using an incorrect input. There should be one row per population in both the minor and total matrices"))

  # get the number of fixed sites in the first population that, at the same time, do not exist in the second population
  fixA <- minor[1, ] == total[1, ] & minor[2, ] == 0
  # get the number of fixed sites in the second population that, at the same time, do not exist in the first population
  fixB <- minor[2, ] == total[2, ] & minor[1, ] == 0

  # combine the previous information from both populations
  fixAB <- c(fixA, fixB)

  if (nPops == 2) {

    # fraction of sites showing a fixed difference between both populations is obtained by performing a sum of the previous vector
    # because each time one populations has a frequency of 1 and the other has a frequency of 0 - that corresponds to a TRUE
    # and dividing that sum by the total number of sites
    Sf <- sum(fixAB) / ncol(minor)

  } else {

    # get the number of fixed sites in the third population that, at the same time, do not exist in the fourth population
    fixC <- minor[3, ] == total[3, ] & minor[4, ] == 0
    # get the number of fixed sites in the fourth population that, at the same time, do not exist in the third population
    fixD <- minor[4, ] == total[4, ] & minor[3, ] == 0
    # combine the previous information from both populations - this is the information for the other location
    fixCD <- c(fixC, fixD)
    # compute the fraction of sites with a fixed difference between the populations at each location
    Sf <- c(sum(fixAB) / ncol(minor), sum(fixCD) / ncol(minor))

    # We can also look at this globally - for all the four populations
    # check, for each population, which sites are fixed for that population and absent from the others
    fixA <- minor[1, ] == total[1, ] & minor[2, ] == 0 & minor[3, ] == 0 & minor[4, ] == 0
    fixB <- minor[2, ] == total[2, ] & minor[1, ] == 0 & minor[3, ] == 0 & minor[4, ] == 0
    fixC <- minor[3, ] == total[3, ] & minor[1, ] == 0 & minor[2, ] == 0 & minor[4, ] == 0
    fixD <- minor[4, ] == total[4, ] & minor[1, ] == 0 & minor[2, ] == 0 & minor[3, ] == 0
    # combine all of this information into a single vector
    gfix <- c(fixA, fixB, fixC, fixD)
    # the global fraction of sites with a fixed difference can be obtained by dividing the sum of the previous vector
    # by the total number of sites
    gSf <- sum(gfix) / ncol(minor)
    # add this global fraction to the vector containing the information about the pairwise comparison of locations
    Sf <- c(Sf, gSf)
  }

  # output the fraction of sites showing a fixed difference between the populations
  Sf
}

#' Compute the fraction of exclusive sites
#'
#' This function will compute the fraction of sites showing an exclusive
#' polymorphism to a given population.
#'
#' More precisely, we define exclusive polymorphisms as sites that are
#' segregating in only one of the populations. To clarify, we define segregating
#' sites as sites where the number of minor-allele reads of a given population
#' at a given site are not equal to zero or to the total coverage of that
#' population. We then check if those segregating sites are also segregating in
#' the other population.
#'
#' For models with two populations, this function compares the two present-day
#' populations. For models with four populations, this function performs a
#' pairwise comparison of the populations at each of the locations. For the
#' models with four populations, we also assess the fraction of sites that are
#' segregating only in one population and not in the other three.
#'
#' @param minor is a matrix with the number of minor-allele reads. Each row of
#'   the matrix is a different population and each column a different site.
#' @param total is a matrix with the total coverage. Each row of the matrix is a
#'   different population and each column a different site.
#' @param nPops is an integer indicating the total number of populations.
#'
#' @return a numeric vector with two entries when `nPops` is equal to 2 or
#'   with five entries when `nPops` is set to 4.
#'
#' @keywords internal
#'
#' @export
exclusive <- function(minor, total, nPops) {

  # check if the input is in the correct format
  if(nrow(minor) != nPops | nrow(total) != nPops)
    stop(paste("Using an incorrect input. There should be one row per population in both the minor and total matrices"))

  # get the number of segregating sites in the first population
  # i.e. the number of sites where the number of reads with the minor allele is not equal to either the total nreads or zero
  segA <- minor[1, ] != total[1, ] & minor[1, ] != 0
  # get the number of segregating sites in the second population
  segB <- minor[2, ] != total[2, ] & minor[2, ] != 0

  # combine the previous information from both populations - this is the information for one of the locations
  segAB <- rbind(segA, segB)

  # we are only interested in the sites where the sum of values per column is equal to 1
  # because this means that we have TRUE (i.e segregating site) in one population and FALSE in the other (non-segregating site)
  keep <- colSums(segAB) == 1
  # remove columns where both populations are fixed for one allele or have no reads for that allele
  segAB <- segAB[, keep, drop = FALSE]
  # now by performing a sum over the rows we can obtain the number of sites, for each of the populations
  # where that population is polymorphic while the other is not
  segAB <- unname(rowSums(segAB))

  if (nPops == 2) {

    # now we can calculate the fraction of sites showing an exclusive polymorphism for each population
    # this is obtained by dividing the number of exclusive sites by the total number of sites
    Sx <- segAB / ncol(minor)

  } else {

    # now we can calculate the fraction of sites showing an exclusive polymorphism for each population
    # this is obtained by dividing the number of exclusive sites by the total number of sites
    SxAB <- segAB / ncol(minor)

    # get the number of segregating sites in the third population
    segC <- minor[3, ] != total[3, ] & minor[3, ] != 0
    # get the number of segregating sites in the fourth population
    segD <- minor[4, ] != total[4, ] & minor[4, ] != 0
    # combine the previous information from both populations - this is the information for the second location
    segCD <- rbind(segC, segD)

    # we are only interested in the sites where the sum of values per column is equal to 1
    # because this means that we have TRUE (i.e segregating site) in one population and FALSE in the other (non-segregating site)
    keep <- colSums(segCD) == 1
    # remove columns where both populations are fixed for one allele or have no reads for that allele
    segCD <- segCD[, keep, drop = FALSE]
    # now by performing a sum over the rows we can obtain the number of sites, for each of the populations
    # where that population is polymorphic while the other is not
    segCD <- unname(rowSums(segCD))
    # now we can calculate the fraction of sites showing an exclusive polymorphism for each population
    # this is obtained by dividing the number of exclusive sites by the total number of sites
    SxCD <- segCD / ncol(minor)

    # We can also look at this globally - for all the four populations
    seg <- rbind(segA, segB, segC, segD)
    # again, we are only interested in sites where the sum of the columns is one
    # this time, this means that the allele is segregating in only one population and not in the other three
    keep <- colSums(seg) == 1
    # now by performing a sum we can obtain the number of sites where one population is polymorphic while the others are not
    seg <- sum(keep)
    # calculate the fraction of sites showing an exclusive polymorphism for a single population
    gSeg <- seg / ncol(minor)

    # combine all the different information into a single vector
    Sx <- c(SxAB, SxCD, gSeg)
  }

  # output the fraction of sites showing an exclusive polymorphism for a given population
  Sx
}


#' Compute the fraction of sites shared between populations
#'
#' This function will compute the fraction of sites with a shared polymorphism
#' between populations.
#'
#' More precisely, we define shared polymorphisms as sites that are segregating
#' in both populations. To clarify, we define segregating sites as sites where
#' the number of minor-allele reads of a given population at a given site are
#' not equal to zero or to the total coverage of that population. We then check
#' if those segregating sites are also segregating in the other population.
#'
#' For models with two populations, this function compares the two present-day
#' populations. For models with four populations, this function performs a
#' pairwise comparison of the populations at each of the locations. For the
#' models with four populations, we also assess the fraction of sites that are
#' segregating only in one population and not in the other three.
#'
#' @param minor is a matrix with the number of minor-allele reads. Each row of
#'   the matrix is a different population and each column a different site.
#' @param total is a matrix with the total coverage. Each row of the matrix is a
#'   different population and each column a different site.
#' @param nPops is an integer indicating the total number of populations.
#'
#' @return a numeric vector with a single entry when `nPops` is equal to 2 or
#'   with three entries when `nPops` is set to 4.
#'
#' @keywords internal
#'
#' @export
shared <- function(minor, total, nPops) {

  # check if the input is in the correct format
  if(nrow(minor) != nPops | nrow(total) != nPops)
    stop(paste("Using an incorrect input. There should be one row per population in both the minor and total matrices"))

  # get the number of segregating sites in the first population
  # i.e. the number of sites where the number of reads with the minor allele is not equal to either the total nreads or zero
  segA <- minor[1, ] != total[1, ] & minor[1, ] != 0
  # get the number of segregating sites in the second population
  segB <- minor[2, ] != total[2, ] & minor[2, ] != 0

  # combine the information into a single matrix
  segAB <- rbind(segA, segB)
  # we are only interested in the sites where the sum of values per column is equal to 2
  # because this means that we have TRUE (i.e segregating site) in one population and TRUE in the other (also segregating)
  sharedAB <- sum(colSums(segAB) == 2)
  # the total number of TRUES in the previous line is the number of sites with a shared polymorphism between the populations

  if (nPops == 2) {

    # calculate the fraction of sites showing a shared polymorphism between the two populations
    # this is obtained by dividing the number of shared sites by the total number of sites
    SS <- sharedAB / ncol(minor)

  } else {

    # get the number of segregating sites in the third population
    segC <- minor[3, ] != total[3, ] & minor[3, ] != 0
    # get the number of segregating sites in the fourth population
    segD <- minor[4, ] != total[4, ] & minor[4, ] != 0
    # combine the previous information from both populations - this is the information for the other location
    segCD <- rbind(segC, segD)
    # get number of sites with a shared polymorphism between the populations in the second location
    sharedCD <- sum(colSums(segCD) == 2)

    # now we can calculate the fraction of sites showing a shared polymorphism between the populations - and for each site
    # this is obtained by dividing the number of exclusive sites by the total number of sites
    SS <- c(sharedAB / ncol(minor), sharedCD / ncol(minor))

    # We can also look at this globally - for all the four populations
    seg <- rbind(segA, segB, segC, segD)
    # get number of sites with a shared polymorphism between all of the populations
    shared <- sum(colSums(seg) == nPops)
    # the fraction of sites with a polymorphism shared between all of the populations is obtained by dividing
    # the number of shared sites by the total number of sites
    gSeg <- shared / ncol(minor)

    # combine all the different information into a single vector
    SS <- c(SS, gSeg)
  }

  # output the fraction of sites with a polymorphism shared between the populations
  SS
}


#' Compute expected heterozygosity within a population
#'
#' This functions calculates the value of the expected heterozygosity for each
#' SNP.
#'
#' @param Pop_Pi is a matrix or list of allele frequencies. When dealing with a
#'   single locus, this input is a matrix and when dealing with multiple loci it
#'   is a list. Each entry of that list is a matrix representing a different
#'   locus. Each row of that matrix should correspond to a different population
#'   and each column to a different SNP.
#'
#' @return if the input is a single matrix, the output will be a matrix where
#'   each row represents a different population and each column is the expected
#'   heterozygosity of a population at that site. If the input is a list, the
#'   output will also be a list, with each entry corresponding to a different
#'   locus. Each of those entries will be a matrix with different populations in
#'   different rows and the expected heterozygosity of different sites at
#'   different columns.
#'
#' @keywords internal
#'
#' @export
Expected_Het <- function(Pop_Pi) {

  # dealing with a single matrix of population allelic frequencies - a single locus or simulation
  if(inherits(Pop_Pi, "matrix")) {

    # compute the expected heterozygosity for a site - this code goes across all sites
    het <- apply(Pop_Pi, c(1,2), function (frequency) 2*frequency*(1 - frequency))

  } else { # dealing with more than one locus or simulation

    het <- lapply (Pop_Pi, FUN = function(x) {
      apply(x, c(1,2), function (frequency) 2*frequency*(1 - frequency))})
  }

  # output the expected heterozygosity
  het
}


#' Compute mean expected heterozygosity within a population
#'
#' This functions calculates the value of the expected heterozygosity for each
#' site and then computes the mean of those values in order to obtain the mean
#' expected heterozygosity within each population.
#'
#' @param Pop_Pi is a matrix or list of allele frequencies. When dealing with a
#'   single locus, this input is a matrix and when dealing with multiple loci it
#'   is a list. Each entry of that list is a matrix representing a different
#'   locus. Each row of that matrix should correspond to a different population
#'   and each column to a different SNP.
#'
#' @return if the input is a single matrix, the output will be a vector where
#'   the first entry is the mean expected heterozygosity of the first population
#'   and the n entry is the mean expected heterozygosity of the nth population.
#'   If the input is a list, the output will also be a list, with each entry
#'   corresponding to a different locus. Each of those entries will be a vector
#'   with the mean expected heterozygosity per population for that locus.
#'
#' @keywords internal
#'
#' @export
meanExpected_Het <- function(Pop_Pi) {

  # dealing with a single matrix of population allelic frequencies - a single locus or simulation
  if(inherits(Pop_Pi, "matrix")) {

    # compute the expected heterozygosity for a site - this code goes across all sites
    het <- apply(Pop_Pi, c(1,2), function (frequency) 2*frequency*(1 - frequency))
    # compute the mean across rows - each pool (or population) occupies one row
    # so this gives the mean expected heterozygosity of each population
    mean_het <- rowMeans(het)

  } else { # dealing with more than one locus or simulation

    het <- lapply (Pop_Pi, FUN = function(x) {
      apply(x, c(1,2), function (frequency) 2*frequency*(1 - frequency))})

    # compute the mean expected heterozygosity of each population
    mean_het <- lapply (het, FUN = function(x) {rowMeans(x)})
  }

  # output the mean expected heterozygosity
  mean_het
}


#' Compute heterozygosity between all pairs of populations
#'
#' This function computes the value of the mean expected heterozygosity between
#' all pairwise combinations of different populations.
#'
#' If you wish to see what the different combinations are, please run
#' \code{combn(nrow(Pop_Pi), m = 2)}. It should also be noted that the order of
#' the combinations obtained by using that command is the same as the order of
#' the output vector. This functions works when the input is a matrix or a list.
#'
#' @param Pop_Pi is a matrix or list of allele frequencies. When dealing with a
#'   single locus, this input is a matrix and when dealing with multiple loci it
#'   is a list. Each entry of that list is a matrix representing a different
#'   locus. Each row of that matrix should correspond to a different population
#'   and each column to a different SNP.
#'
#' @return  if the input is a matrix, this will be a vector where each entry
#'   represents the mean expected heterozygosity between two populations. When a
#'   list is used as input, the output will also be a list and each entry of
#'   that list will be a vector with the mean expected heterozygosity between
#'   pairs of populations for that locus.
#'
#' @keywords internal
#'
#' @export
Het_Between <- function(Pop_Pi) {

  # when dealing with a single matrix of population frequencies - one locus or one simulation
  if(inherits(Pop_Pi, "matrix")) {

    # considering only two populations (according to Hudson (1992) estimator, using the formula of Chen (2015):
    # http://journals.plos.org/plosone/article/metrics?id=10.1371/journal.pone.0135368
    if(nrow(Pop_Pi) == 2) {

      HB <- (1/2)*((2*Pop_Pi[1,]*(1-Pop_Pi[2,]))+(2*Pop_Pi[2,]*(1-Pop_Pi[1,])))
      # (1/2) because we are comparing pairs of populations
      Mean_HB <- mean(HB)

    } else { # considering more than two populations

      Pairwise <- utils::combn(nrow(Pop_Pi), 2)
      HB <- as.matrix(apply(Pairwise, 2, function(col) {
        (1/2)*(2*Pop_Pi[col[1],]*(1-Pop_Pi[col[2],])+2*Pop_Pi[col[2],]*(1-Pop_Pi[col[1],]))}))

      # this adds a correction for the situations where there is only one site
      if(ncol(HB) == 1)
        HB <- t(HB)

      # Compute the mean per column - each column contains the values for one of the pairwise combinations
      # So by computing the column mean, we compute the mean heterozygosity between two populations
      Mean_HB <- colMeans(HB)
    }

    # starting here, we are dealing with the situations where we have more than one simulation or locus
    # meaning that the input is a list, instead of a matrix

  } else {

    # get the number of pops - look at the number of the rows for the first simulation or locus
    nPops <- nrow(Pop_Pi[[1]])
    # create a matrix with all the possible pairwise comparisons between the populations
    Pairwise <- utils::combn(nPops, 2)
    HB <- lapply(Pop_Pi, FUN = function(y) {
      as.matrix(apply(Pairwise, 2, function(col) {
        (1/2)*(2*y[col[1],]*(1-y[col[2],])+2*y[col[2],]*(1-y[col[1],]))}))
    })

    # when dealing with just one comparison - two populations - you only need to compute the mean across all sites
    if (nPops == 2) {
      Mean_HB <- lapply(HB, FUN = function(x) {mean(x)})

    } else { # when dealing with more than two populations

      # correction for simulations with a single polymorphic site
      HB <- lapply(HB, FUN = function(H) {
        if(ncol(H) == 1) {
          H <- t(H)
        } else {
          H = H
        }
      })

      # compute the mean for each column
      Mean_HB <- lapply(HB, FUN = function(x) {colMeans(x)})
    }
  }

  # output the results of the function
  Mean_HB
}


#' Calculate FST
#'
#' This function computes FST according to Hudson's estimator following Bathia.
#' Note that the frequencies for the two populations should be entered as
#' separate inputs.
#'
#' Note that this functions computes a single FST value between two populations
#' and does not perform pairwise comparisons of multiple populations.
#'
#' @param freq1 is a numeric vector with the allele frequencies for population
#'   1. Each entry of the vector should be a different site.
#' @param freq2 is a numeric vector with the allele frequencies for population
#'   2. Each entry of the vector should be a different site.
#' @param ss1 vector with the sample size for population 1. Each entry of the
#'   vector should contain the number of reads for a different site.
#' @param ss2 vector with the sample size for population 2. Each entry of the
#'   vector should contain the number of reads for a different site.
#'
#' @return a numeric value which is the FST between the two populations.
#'
#' @keywords internal
#'
#' @export
getFst <- function(freq1, freq2, ss1, ss2) {

  p1 <- freq1*(1-freq1)
  p2 <- freq2*(1-freq2)
  pdiffsquare <- (freq1-freq2)^2
  numerator <- pdiffsquare - (p1/(ss1-1)) - (p2/(ss2-1))
  denominator <- (freq1*(1-freq2)) + (freq2*(1-freq1))
  den <- sum(denominator, na.rm = T)

  if (den > 0)
    res <- sum(numerator, na.rm = T)/den
  else
    res <- 0

  # output the FST value
  res
}


#' Pairwise FST among populations
#'
#' This functions calculates pairwise FST values according to Hudson's estimator
#' following Bathia. FST values are calculated for each pairwise combination of
#' the populations present in the data (defined by the \code{nPops} parameter).
#'
#' This functions performs pairwise comparisons of multiple populations and thus
#' returns multiple FST values, one for each comparison. However, this function
#' computes FST for a single locus.
#'
#' @param nPops is an integer indicating how many populations are present in the
#'   data.
#' @param Pop_pi is a matrix of allele frequencies. Each row of that matrix
#'   should correspond to a different population and each column to a different
#'   site.
#' @param coverage is a matrix containing depths of coverage. Each row of that
#'   matrix should correspond to a different population and each column to a
#'   different site.
#'
#' @return a upper triangular matrix with the pairwise FST values between each
#'   population.
#'
#' @keywords internal
#'
#' @export
pairFST <- function(nPops, Pop_pi, coverage) {

  # create a matrix to store the FST values
  pairfst <- matrix(NA, ncol = nPops, nrow = nPops)

  # dealing with a matrix - single locus simulation
  for(i in 1:(nPops-1)) {

    for(j in (i+1):nPops) {

      # apply the getFst() function to all the different pairwise combinations of the populations
      pairfst[i,j] <- getFst(Pop_pi[i,], Pop_pi[j,], coverage[i,], coverage[j,])
    }
  }

  # output FST between pairs of populations
  pairfst
}


#' Pairwise FST among populations and across multiple loci
#'
#' This functions calculates pairwise FST values according to Hudson's estimator
#' following Bathia. FST values are calculated for each pairwise combination of
#' the populations present in the data (defined by the \code{nPops} parameter).
#'
#' This functions performs pairwise comparisons of multiple populations and thus
#' returns multiple FST values, one for each comparison. Additionally, this
#' function computes FST for multiple loci, returning a value for each pairwise
#' comparison per locus.
#'
#' @param nPops is an integer indicating how many populations are present in the
#'   data.
#' @param Pop_pi is a list of allele frequencies. Each entry of that list is a
#'   matrix representing a different locus. Each row of that matrix should
#'   correspond to a different population and each column to a different site
#' @param coverage is a list containing depths of coverage. Each entry of that
#'   list is a matrix representing a different locus. Each row of that matrix
#'   should correspond to a different population and each column to a different
#'   site
#'
#' @return a list where each entry corresponds to a different locus. Each of
#'   those entries is a upper triangular matrix with the pairwise FST values
#'   between each population.
#'
#' @keywords internal
#'
#' @export
popsFST <- function(nPops, Pop_pi, coverage) {

  # check if the Pop_pi is in the correct (list) format
  if(!inherits(Pop_pi, "list"))
    stop(paste("This function should be utilized with more than one locus. Please check"))

  # compute FST across a list - multiple locus simulation
  listFST <- lapply(1:length(Pop_pi), function(locus)
    pairFST(nPops = nPops, Pop_pi = Pop_pi[[locus]], coverage = coverage[[locus]]))

  # output FST between pairs of populations
  listFST
}


#' Calculate the abba portion of the D-statistic
#'
#' Computes the value for the ‘ABBA’ allelic pattern.
#'
#' @param p1 is a numeric vector with the allele frequencies for population 1.
#'   Each entry of the vector should be a different site.
#' @param p2 is a numeric vector with the allele frequencies for population 2.
#'   Each entry of the vector should be a different site.
#' @param p3 is a numeric vector with the allele frequencies for population 3.
#'   Each entry of the vector should be a different site.
#'
#' @return a numeric vector with one value per site of the locus.
#'
#' @keywords internal
#'
#' @export
abba <- function(p1, p2, p3) {

  # compute abba
  (p1 * (1 - p2) * (1 - p3)) + ((1 - p1) * p2 * p3)
}


#' Calculate the baba portion of the D-statistic
#'
#' Computes the value for the ‘BABA’ allelic pattern.
#'
#' @param p1 is a numeric vector with the allele frequencies for population 1.
#'   Each entry of the vector should be a different site.
#' @param p2 is a numeric vector with the allele frequencies for population 2.
#'   Each entry of the vector should be a different site.
#' @param p3 is a numeric vector with the allele frequencies for population 3.
#'   Each entry of the vector should be a different site.
#'
#' @return a numeric vector with one value per site of the locus.
#'
#' @keywords internal
#'
#' @export
baba <- function(p1, p2, p3) {

  # compute baba
  ((1 - p1) * p2 * (1 - p3)) + (p1 * (1 - p2) * p3)
}


#' calculate D-statistic
#'
#' Computes the value of the D-statistic given the values of the two particular
#' allelic patterns.
#'
#' @param ABBA is a numeric vector with the values for the ‘ABBA’ allelic
#'   pattern.
#' @param BABA is a numeric vector with the values for the ‘BABA’ allelic
#'   pattern.
#'
#' @return a numeric value representing the D-statistic value for a particular
#'   conformation of the populations.
#'
#' @keywords internal
#'
#' @export
D.stat <- function(ABBA, BABA) {

  # compute dstat
  (sum(ABBA) - sum(BABA)) / (sum(ABBA) + sum(BABA))
}


#' Perform D-statistics analysis
#'
#' This functions calculates 3 different D-statistic values from pooled
#' sequenced data, using the allelic frequencies of the minor allele.
#'
#' The three different combinations computed here are: D-statistic 1 sets the
#' the W ecotype in the first location (N2) as P1, the W ecotype in the second
#' location (N4) as P2 and the C ecotype at the first location (N1) as P3.
#' D-statistic 2 sets the W ecotype in the first location (N2) as P1, the C
#' ecotype in the second location (N3) as P2 and the C ecotype at the first
#' location (N1) as P3. D-statistic 3 sets the W ecotype at the first location
#' (N2) a P1, the C ecotype at the first location (N1) as P2 and the W ecotype
#' at the second location (N4) as P3.
#'
#' @param pop_pi is a matrix of allele frequencies. Each row of that matrix
#'   should correspond to a different population and each column to a different
#'   SNP.
#'
#' @return a numeric vector with the three D-statistics values. Each
#'   confirmation of the populations corresponds to a different entry of the
#'   vector.
#'
#' @keywords internal
#'
#' @export
D.statPool <- function(pop_pi) {

  # calculate D-stat with the crab population in site 1 as P3, wave pop in site 1 as P1 and wave pop in site 2 as P2
  ABBA <- abba(p1 = pop_pi[2,], p2 = pop_pi[4,], p3 = pop_pi[1,])
  BABA <- baba(p1 = pop_pi[2,], p2 = pop_pi[4,], p3 = pop_pi[1,])

  DStat1 <- D.stat(ABBA, BABA)

  # calculate D-stat with the crab population in site 1 as P3 and with the wave pop in site 1 as P1
  # but with the crab pop at site 2 as P2
  ABBA <- abba(p1 = pop_pi[2,], p2 = pop_pi[3,], p3 = pop_pi[1,])
  BABA <- baba(p1 = pop_pi[2,], p2 = pop_pi[3,], p3 = pop_pi[1,])

  DStat2 <- D.stat(ABBA, BABA)

  # calculate D-stat with the wave population in site 2 as P3, wave pop in site 1 as P1 and crab pop in site 1 as P2
  ABBA <- abba(p1 = pop_pi[2,], p2 = pop_pi[1,], p3 = pop_pi[4,])
  BABA <- baba(p1 = pop_pi[2,], p2 = pop_pi[1,], p3 = pop_pi[4,])

  DStat3 <- D.stat(ABBA, BABA)

  # output the values of D-stat: With different populations as P3
  dstat <- c(DStat1, DStat2, DStat3)
  dstat
}
