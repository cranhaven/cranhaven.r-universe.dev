#' Draw parameters from the priors
#'
#' This function creates a named vector of parameters that can be used as input
#' in the command line of the scrm package. Please note that this function needs
#' to be adjusted if you wish to test the effect of different prior
#' distributions.
#'
#' @param Nref The minimum and maximum value of the uniform distribution for the
#'   effective population size of the reference population (Nref).
#' @param ratio The minimum and maximum value of the distribution from which the
#'   relative size of the present-day and ancestral populations are drawn. The
#'   size of these populations is set as a ratio of the size of the Nref
#'   population. All of these ratios are drawn from a log10 uniform
#'   distribution.
#' @param split The minimum and maximum values, at the 4Nref scale, of the
#'   uniform distribution from which the values of the times of the split events
#'   are draw. Both the time of the recent split event and the distance between
#'   the two split events are drawn from this distribution.
#' @param pool The minimum and maximum values of the uniform distribution from
#'   which the value of the error associated with DNA pooling is drawn. More
#'   specifically, this value is related with the unequal individual
#'   contribution to the pool.
#' @param seq The minimum and maximum values of the uniform distribution from
#'   which the value of the error associated with DNA sequencing is drawn. This
#'   parameter should be supplied as a decimal number between zero and one.
#' @param CW The minimum and maximum value of the uniform distribution from
#'   which the migration rate between the two divergent ecotypes inhabiting the
#'   same location is drawn. We consider that this parameter is drawn on a m
#'   scale. This is the migration rate from ecotype C to ecotype W.
#' @param WC The minimum and maximum value of the uniform distribution from
#'   which the migration rate between the two divergent ecotypes inhabiting the
#'   same location is drawn. We consider that this parameter is drawn on a m
#'   scale. This is the migration rate from ecotype W to ecotype C.
#' @param CC The minimum and maximum value of the uniform distribution from
#'   which the migration rate between similar ecotypes inhabiting different
#'   locations is drawn. We consider that this parameter is drawn on a m scale.
#'   This is the migration between the two C ecotypes at two different
#'   locations.
#' @param WW The minimum and maximum value of the uniform distribution from
#'   which the migration rate between similar ecotypes inhabiting different
#'   locations is drawn. We consider that this parameter is drawn on a m scale.
#'   This is the migration between the two W ecotypes at two different
#'   locations.
#' @param ANC The minimum and maximum value of the uniform distribution from
#'   which the migration rate between the two ancestral populations is drawn. We
#'   consider that this parameter is drawn on a m scale.
#' @param bT The minimum and maximum values of the distribution from which the
#'   proportion of the simulated loci where no migration occurs between
#'   divergent ecotypes is drawn. The maximum value should not be higher than
#'   one.
#' @param bCW The minimum and maximum values of the distribution from which the
#'   proportion of the simulated loci where no migration occurs from the C
#'   ecotype towards the W ecotype is drawn. The maximum value should not be
#'   higher than one.
#' @param bWC The minimum and maximum values of the distribution from which the
#'   proportion of the simulated loci where no migration occurs from the W
#'   ecotype towards the C ecotype is drawn. The maximum value should not be
#'   higher than one.
#' @param model Either "2pops", "Single" or "Parallel" indicating for which
#'   model should parameters be drawn.
#' @param digits An optional integer indicating the number of decimal places to
#'   use when rounding certain parameters. The default is five.
#'
#' @return a vector with one named entry per relevant parameter. Each entry is
#'   the sampled value from the prior for that particular parameter.
#'
#' @examples
#' # for a model with two populations
#' createParams(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250), seq = c(0.0001, 0.001),
#' split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3), bT = c(0, 0.2), model = "2pops")
#'
#' # for a single origin scenario
#' createParams(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250), seq = c(0.0001, 0.001),
#' split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3), CC =  c(1e-13, 1e-3),
#' WW = c(1e-13, 1e-3), ANC = c(1e-13, 1e-3), bT = c(0, 0.2), bCW = c(0, 0.5),
#' bWC = c(0, 0.5), model = "Single")
#'
#' @export
createParams <- function(Nref, ratio, split, pool, seq, CW, WC, CC = NA, WW = NA, ANC = NA, bT, bCW = NA, bWC = NA,
                         model, digits = 5) {

  # check if the input is correct - the selected model should be one of the following
  if(model %in% c("2pops", "Single", "Parallel") == FALSE)
    stop("The selected model should be either 2pops, Single or Parallel. Please check")

  # draw a value for the population size of the most ancestral population - it's also the Nref
  Nrf <- stats::runif(n = 1, min = Nref[1], max = Nref[2])
  # Draw the values for the extant and ancestral population sizes
  # Values are drawn as a ratio of the Ne size - a value of 2 means that the population is twice the size of the Ne
  N1 <- round(stats::runif(n = 1, min = log10(ratio[1]), max = log10(ratio[2])), digits = 5)
  N2 <- round(stats::runif(n = 1, min = log10(ratio[1]), max = log10(ratio[2])), digits = 5)
  # set the values in the natural scale
  N1 <- 10^N1; N2 <- 10^N2

  # Draw the split times from a uniform distribution - times are drawn on a 4Nref scale
  Split <- stats::runif(n = 1, min = split[1], max = split[2])
  # Draw the errors for the pooling parameter
  Pool_Error <- stats::runif(n = 1, min = pool[1], max = pool[2])
  # and the sequencing error
  Error <- stats::runif(n = 1, min = seq[1], max = seq[2])

  # draw the migration rate (at the m scale) - this corresponds to the migration between different ecotypes in the same site
  # the migration rate from crab to wave is drawn as mCW - for the first site
  mCW1 <- stats::runif(n = 1, min = CW[1], max = CW[2])
  # and the migration rate from wave to crab is drawn as mWC - for the first site
  mWC1 <- stats::runif(n = 1, min = WC[1], max = WC[2])

  # proportion of the genome without migration - total barrier
  total <- stats::rbeta(n = 1, shape1 = 1, shape2 = 10)
  # replace values below the minimum threshold with the minimum
  total[total < 1e-2] <- bT[1]
  # and values above the maximum threshold with the maximum
  total[total > bT[2]] <- bT[2]

  # stop the function if we are working with the two-population model
  if(model == "2pops") {
    # assume that the proportion of the genome with unrestricted migration is 1 - proportion without migration
    pMig <- 1 - total
    # create the parameters vector for this particular model
    parameters <- c(Nrf, N1, N2, Split, Pool_Error, Error, mCW1, mWC1, pMig, total)
    # add names to the entries of the vector
    names(parameters) <- c("Nref", "N1", "N2", "Split", "PoolError", "SeqError", "mCW", "mWC", "pM", "pNO")
    # stop the function and output the parameters vector
    stop(return(parameters))
  }

  # draw the values for the remaining present-day population sizes
  N3 <- round(stats::runif(n = 1, min = log10(ratio[1]), max = log10(ratio[2])), digits = 5)
  N4 <- round(stats::runif(n = 1, min = log10(ratio[1]), max = log10(ratio[2])), digits = 5)
  # set the values in the natural scale
  N3 <- 10^N3; N4 <- 10^N4

  # draw the values for the ancestral population sizes
  # values are drawn as a ratio of the Ne size - a value of 2 means that the population is twice the size of the Ne
  NA1 <- round(stats::runif(n = 1, min = log10(ratio[1]), max = log10(ratio[2])), digits = 5)
  NA2 <- round(stats::runif(n = 1, min = log10(ratio[1]), max = log10(ratio[2])), digits = 5)
  # set the values in the natural scale
  NA1 <- 10^NA1; NA2 <- 10^NA2

  # draw the second split time from a uniform distribution - times are drawn on a 4Ne scale
  Dsplit <- stats::runif(n = 1, min = split[1], max = split[2])

  # draw the migration rate (at the m scale) - this corresponds to the migration between different ecotypes in the same site
  # the migration rate from crab to wave is drawn as mCW - for the second site
  mCW2 <- stats::runif(n = 1, min = CW[1], max = CW[2])
  # and the migration rate from wave to crab is drawn as mWC - for the second site
  mWC2 <- stats::runif(n = 1, min = WC[1], max = WC[2])

  # if required, draw additional migration rates
  if(!any(is.na(CC)))
    mCC <- stats::runif(n = 1, min = CC[1], max = CC[2]) # between crab ecotypes in different locations
  else
    mCC <- NA  # set mCC to NA

  # if required, draw additional migration rates
  if(!any(is.na(WW)))
    mWW <- stats::runif(n = 1, min = WW[1], max = WW[2]) # between wave ecotypes in different locations
  else
    mWW <- NA  # set mWW to NA

  # if required, draw additional migration rates
  if(!any(is.na(ANC)))
    mAA <- stats::runif(n = 1, min = ANC[1], max = ANC[2])  # migration rates between the ancestral populations
  else
    mAA <- NA # set mAA to NA

  # if required, draw the proportion of the genome without migration
  if(!any(is.na(bCW))) {
    # from the crab to the wave ecotype
    pCW <- stats::rbeta(n = 1, shape1 = 1, shape2 = 10)
    # replace values below the minimum threshold with the minimum
    pCW[pCW < 1e-2] <- bCW[1]
    # and values above the maximum threshold with the maximum
    pCW[pCW > bCW[2]] <- bCW[2]

  } else {

    # set pCW to NA
    pCW <- NA
  }

  # if required, draw the proportion of the genome without migration
  if(!any(is.na(bWC))) {
    # from the wave to the crab ecotype
    pWC <- stats::rbeta(n = 1, shape1 = 1, shape2 = 10)
    # replace values below the minimum threshold with the minimum
    pWC[pWC < 1e-2] <- bWC[1]
    # and values above the maximum threshold with the maximum
    pWC[pWC > bWC[2]] <- bWC[2]

  } else {

    # set pWC to NA
    pWC <- NA
  }

  # assume that the proportion of the genome with unrestricted migration is 1 - proportion without migration
  pMig <- 1 - sum(c(total, pCW, pWC), na.rm = TRUE)

  # create the parameters vector for the single and parallel models
  parameters <- c(Nrf, N1, N2, N3, N4, NA1, NA2, Split, Dsplit, Pool_Error, Error, mCW1, mCW2, mWC1, mWC2, mCC, mWW, mAA,
                  pMig, pCW, pWC, total)

  # add names to the entries of the vector
  names(parameters) <- c("Nref", "N1", "N2", "N3", "N4", "NA1", "NA2", "Split", "Dsplit", "PoolError", "SeqError", "mCW1", "mCW2",
                         "mWC1", "mWC2", "mCC", "mWW", "mAA", "pM", "pCW", "pWC", "pNO")

  # remove any parameters that are set as NA
  parameters <- parameters[!is.na(parameters)]

  # output the parameters vector
  parameters
}


#' Create SCRM command line for a model with two populations
#'
#' This function creates a command line tailored for an isolation with migration
#' model with two populations. The command line can then be fed to the scrm
#' package to run the model.
#'
#' @param parameters A vector where each entry corresponds to a different
#'   parameter, e.g. one entry is the size of the reference population, another
#'   is the time of recent split, etc. Please note that this functions depends
#'   on the ordering of the parameters in the vector and thus, it should only be
#'   used with a vector created with the `createParams` function.
#' @param nSites An integer representing the number of base pairs that each
#'   locus should have.
#' @param nLoci An integer that represents how many independent loci should be
#'   simulated.
#' @param nDip An integer representing the total number of diploid individuals
#'   to simulate. Note that scrm actually simulates haplotypes, so the number of
#'   simulated haplotypes is double of this. Also note that this is the total
#'   number of diploid individuals and this function will distribute the
#'   individuals equally by the two populations.
#' @param mutrate A number representing the mutation rate assumed for the
#'   simulations.
#' @param extra is a logical value indicating whether the required number of
#'   loci should be enforced. The default is FALSE but, if set to TRUE, then
#'   additional loci will be simulated. These additional loci are simulated to
#'   try to have sufficient loci to keep the required number of loci after
#'   filtering.
#'
#' @return a character vector with two entries. The first entry is the scrm
#'   command line for the loci without any barriers against migration, while the
#'   second entry is the scrm command line for the loci without migration
#'   between divergent ecotypes.
#'
#' @examples
#' # create a vector with parameter values for a two populations model
#' params <- createParams(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250),
#' seq = c(0.0001, 0.001), split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3),
#' bT = c(0, 0.2), model = "2pops")
#'
#' # create the command line for the scrm package
#' cmd2pops(parameters = params, nSites = 2000, nLoci = 100, nDip = 100, mutrate = 2e-8)
#'
#' @export
cmd2pops <- function(parameters, nSites, nLoci, nDip, mutrate, extra = FALSE) {

  # this function is intended to be used with a two-population model
  nPops <- 2

  # Read the vector with the parameters and assign each parameter to the correct command name
  Ne <- parameters[1]
  # set the relative size of each population
  N1 <- parameters[2]; N2 <- parameters[3]

  # get the proportion of loci with migration - no barriers against migration between the different ecotypes
  pM <- parameters[9]
  # and the proportion of loci without any migration - total barrier against migration between the different ecotypes
  pNO <- parameters[10]

  # get the migration rates - between different ecotypes at the same site - this is the m value on the M = 4N0m formula
  # from the crab ecotype to the wave ecotype
  mCW <-  parameters[7]
  # from the wave ecotype to the crab ecotype
  mWC <-  parameters[8]

  # m <i> <j> <M>: Set the migration rate from population j to population i to M (looking forward in time)
  # and REMEMBER that M = 4N0m
  # set the migration rates to the scale of Nref - between different ecotypes at the same site
  mig_CW <- 4*Ne*mCW # -m 2 1 mig_CW

  # the migration from wave to crab is parametrized as a ratio of the migration from crab to wave
  # so we need to multiply the migration from crab to wave by this ratio
  mig_WC <- 4*Ne*mWC # -m 1 2 mig_WC

  # get the time of the split event
  split <- round(parameters[4], digits = 3)

  # Compute the value of theta
  mutrate_locus <- nSites*mutrate
  theta <- 4*Ne*mutrate_locus
  # Create a vector with information about how many haplotypes are sampled from each population
  n <- c(rep(nDip/nPops, times = nPops))

  # use a multinomial distribution to get the number of loci simulated under each category
  lociTotal <- as.vector(stats::rmultinom(n = 1, size = nLoci, c(pM, pNO)))

  # if extra is TRUE, then more loci than required per category will be simulated
  if(extra == TRUE) {
    # save the required number of simulated loci per category
    targetLoci <- lociTotal
    # simulate more 25 loci per category
    lociTotal <- lociTotal + 25
  }

  # cheat code: pop1 - crab in site 1; pop2 - wave in site 1; pop3 - crab in site 2; pop4 - wave in site 2
  # create command line with no barriers to migration
  # set the basic elements for scrm - nhap: total number of haplotypes that are simulated at each locus and
  # nrep: the number of independent loci that will be produced
  with.mig <- paste(paste(nDip*2, collapse = " "), lociTotal[1], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "),
                    # set the size of the present day populations - n <i> <n> Set the size of population i to n*N0.
                    "-n 1", N1, "-n 2", N2,
                    # m <i> <j> <M>: Set the migration rate from population j to population i to M (looking forward in time)
                    # set the migration rate between different ecotypes inhabiting the same location
                    "-m 2 1", mig_CW, "-m 1 2", mig_WC,
                    # now, set the migration rate right before the split event to zero by using the switch:
                    # -eM <t> <M>: assume a symmetric migration rate of M/(npop-1) at time t.
                    "-eM", split, "0",
                    # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                    # finally, set the size of the ancestral pop equal to the size of the reference population with:
                    # -eN <t> <n>: set the size of all populations to n*N0 at time t.
                    "-ej", split, "2 1 -eN", split, 1)

  # create a command line for the loci without any migration (between the different ecotypes)
  # set the basic elements for scrm - nhap: total number of haplotypes that are simulated at each locus and
  # nrep: the number of independent loci that will be produced
  without.mig <- paste(paste(nDip*2, collapse = " "), lociTotal[2], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "), "0",
                       # set the size of the present day populations - n <i> <n> Set the size of population i to n*N0.
                       "-n 1", N1, "-n 2", N2,
                       # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                       # finally, set the size of the ancestral pop equal to the size of the reference population with:
                       # -eN <t> <n>: set the size of all populations to n*N0 at time t.
                       "-ej", split, "2 1 -eN", split, 1)


  # combine the two different types of commands
  cmd_2pops <- c(with.mig, without.mig)

  # if extra is equal to TRUE, then we simulated more loci than required
  if(extra == TRUE)
    # include the required number of loci per category in the output
    cmd_2pops <- list(commands = cmd_2pops, targetLoci = targetLoci)

  # output the command line for the two population model
  cmd_2pops
}

#' Create SCRM command line for a single origin scenario
#'
#' This function creates a command line tailored for a scenario of single origin
#' to explain ecotype formation. The command line can then be fed to the scrm
#' package to run the model.
#'
#' For convenience, imagine we have two divergent ecotypes, named C and W. This
#' model assumes that the first population corresponds to the C ecotype at the
#' first location, the second population to the C ecotype in the second
#' location, the third population to the W ecotype in the first location and the
#' fourth population to the W ecotype in the second location.
#'
#' @param parameters A vector where each entry corresponds to a different
#'   parameter, e.g. one entry is the size of the reference population, another
#'   is the time of recent split, etc. Please note that this functions depends
#'   on the ordering of the parameters in the vector and thus, it should only be
#'   used with a vector created with the `createParams` function.
#' @param nSites An integer representing the number of base pairs that each
#'   locus should have.
#' @param nLoci An integer that represents how many independent loci should be
#'   simulated.
#' @param nDip An integer representing the total number of diploid individuals
#'   to simulate. Note that scrm actually simulates haplotypes, so the number of
#'   simulated haplotypes is double of this. Also note that this is the total
#'   number of diploid individuals and this function will distribute the
#'   individuals equally by the two populations.
#' @param mutrate A number representing the mutation rate assumed for the
#'   simulations.
#' @param extra is a logical value indicating whether the required number of
#'   loci should be enforced. The default is FALSE but, if set to TRUE, then
#'   additional loci will be simulated. These additional loci are simulated to
#'   try to have sufficient loci to keep the required number of loci after
#'   filtering.
#'
#' @return a character vector with four entries. The first entry is the scrm
#'   command line for the loci without any barriers against migration. The
#'   second entry is the command line for the loci without migration from the C
#'   towards the W ecotype. The third entry is command line for the loci without
#'   migration from the W towards the C ecotype and the last entry is the scrm
#'   command line for the loci without migration between divergent ecotypes.
#'
#' @examples
#' # create a vector with parameter values for the single origin scenario
#' params <- createParams(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250),
#' seq = c(0.0001, 0.001), split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3),
#' CC =  c(1e-13, 1e-3), WW = c(1e-13, 1e-3), ANC = c(1e-13, 1e-3), bT = c(0, 0.2),
#' bCW = c(0, 0.5), bWC = c(0, 0.5), model = "Single")
#'
#' # create the command line for the scrm package
#' cmdSingle(parameters = params, nSites = 2000, nLoci = 100, nDip = 400, mutrate = 2-8)
#'
#' @export
cmdSingle <- function(parameters, nSites, nLoci, nDip, mutrate, extra = FALSE) {

  # this function is intended to be used with a four-population model
  nPops <- 4

  # read the vector with the parameters and assign each parameter to the correct variable name
  Ne <- parameters[1]
  # set the relative size of each population - for the extant populations
  N1 <- parameters[2]; N2 <- parameters[3]; N3 <- parameters[4]; N4 <- parameters[5]
  # and the ancient populations
  NA1 <- parameters[6]; NA2 <- parameters[7]

  # get the proportion of loci with migration - no barriers against migration between the different ecotypes
  pM <- parameters[19]
  # get the proportion of loci without migration - from the crab to the wave ecotype at the same location
  pCW <- parameters[20]
  # get the proportion of loci without migration - from the wave to the crab ecotype at the same location
  pWC <- parameters[21]
  # and the proportion of loci without any migration - total barrier against migration between the different ecotypes
  pNO <- parameters[22]

  # get the migration rates - between different ecotypes at the same site - this is the m value on the M = 4N0m formula
  # from the crab ecotype to the wave ecotype - at the first site
  mCW1 <- parameters[12]
  # from the crab ecotype to the wave ecotype - at the second site
  mCW2 <- parameters[13]
  # from the wave ecotype to the crab ecotype - at the fist site
  mWC1 <- parameters[14]
  # from the wave ecotype to the crab ecotype - at the second site
  mWC2 <- parameters[15]

  # between crab populations inhabiting different locations
  mCC <- parameters[16]
  # between wave populations inhabiting different locations
  mWW <- parameters[17]
  # and between the two ancestral populations
  mAA <- parameters[18]

  # m <i> <j> <M>: Set the migration rate from population j to population i to M (looking forward in time)
  # REMEMBER that M = 4N0m
  # set the migration rates to the scale of Nref - between different ecotypes at the same site
  # from the crab ecotype to the wave ecotype - at the first site
  mig_CW1 <- 4*Ne*mCW1 # -m 3 1 mig_CW
  # from the crab ecotype to the wave ecotype - at the second site
  mig_CW2 <- 4*Ne*mCW2 # -m 4 2 mig_CW
  # from the wave ecotype to the crab ecotype - at the first site
  mig_WC1 <- 4*Ne*mWC1 # -m 1 3 mig_WC
  # from the wave ecotype to the crab ecotype - at the second site
  mig_WC2 <- 4*Ne*mWC2 # -m 2 4 mig_WC

  # between crab populations at different locations
  mig_CC <- 4*Ne*mCC
  # between wave populations at different locations
  mig_WW <- 4*Ne*mWW
  # and between the two ancestral populations
  mig_AA <- 4*Ne*mAA

  # get the time of the recent split event
  Rsplit <- round(parameters[8], digits = 3)
  # and the ancient split event
  Asplit <- round(parameters[9], digits = 3)
  # the actual time of the ancient split event is obtained by doing:
  Asplit <- Rsplit + Asplit

  # Compute the value of theta
  mutrate_locus <- nSites*mutrate
  theta <- 4*Ne*mutrate_locus
  # Create a vector with information about how many haplotypes are sampled from each population
  n <- c(rep(nDip/nPops, times = nPops))

  # create variables to ensure that the changes in migration rates occur at different times than the split time
  # a variable to inform when does migration start between ancestral populations
  tmAA <- Rsplit + 0.0001
  # create a variable to inform when does migration stop before the recent split
  # if Rsplit is zero, we can not subtract something from it
  if(Rsplit != 0) {
    # when Rsplit is not zero, set the end of the migration before the split
    tmRS <- Rsplit - 0.0001
  } else {
    # when Rsplit is zero, set the end of the migration at the split
    tmRS <- Rsplit
  }

  # create also a variable to inform when does migration start between ancestral populations
  tmAA <- Rsplit + 0.0001

  # use a multinomial distribution to get the number of loci simulated under each category
  lociTotal <- as.vector(stats::rmultinom(n = 1, size = nLoci, c(pM, pCW, pWC, pNO)))

  # if extra is TRUE, then more loci than required per category will be simulated
  if(extra == TRUE) {
    # save the required number of simulated loci per category
    targetLoci <- lociTotal
    # simulate more 25 loci per category
    lociTotal <- lociTotal + 25
  }

  # cheat code: pop1 - crab in site 1; pop2 - crab in site 2; pop3 - wave in site 1; pop4 - wave in site 2

  # create command line with no barriers to migration
  # set the basic elements for scrm - nhap: total number of haplotypes that are simulated at each locus and
  # nrep: the number of independent loci that will be produced
  with.mig <- paste(paste(nDip*2, collapse = " "), lociTotal[1], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "),
                    # set the size of the present day populations - n <i> <n> Set the size of population i to n*N0.
                    "-n 1", N1, "-n 2", N2, "-n 3", N3, "-n 4", N4,
                    # m <i> <j> <M>: Set the migration rate from population j to population i to M (looking forward in time)
                    # set the migration rate between different ecotypes inhabiting the same location
                    "-m 3 1", mig_CW1, "-m 4 2", mig_CW2, "-m 1 3", mig_WC1, "-m 2 4", mig_WC2,
                    # set the migration rate between the same ecotypes inhabiting different locations
                    "-m 2 1", mig_CC, "-m 1 2", mig_CC, "-m 4 3", mig_WW, "-m 3 4", mig_WW,
                    # set the migration, between all populations, to zero - immediately before the recent split
                    "-eM", tmRS, "0",
                    # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                    # looking backwards in time, it moves all lines from population j into population i at time t
                    # Migration rates into population j are set to 0 for the time further back into the past
                    "-ej", Rsplit, "1 2 -ej", Rsplit, "4 3",
                    # set the size of the ancient populations
                    # -en <t> <i> <n>: Set the size of population i to n*N0 at time t.
                    "-en", Rsplit, "2", NA1, "-en", Rsplit, "3", NA2,
                    # set the migration rate between the ancestral populations
                    "-em", tmAA, "2 3", mig_AA, "-em", tmAA, "3 2", mig_AA,
                    # set the migration, between all populations, to zero - immediately at the ancient split
                    "-eM", Asplit, "0",
                    # add a split event - this event creates the two ancestral populations
                    "-ej", Asplit, "2 3",
                    # finally, set the size of the most ancestral pop equal to the size of the reference population with:
                    # -eN <t> <n>: set the size of all populations to n*N0 at time t.
                    "-eN", Asplit, 1)

  # create command line with no migration from the crab to the wave ecotype within the same location
  no.mCW <- paste(paste(nDip*2, collapse = " "), lociTotal[2], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "),
                  # set the size of the present day populations - n <i> <n> Set the size of population i to n*N0
                  "-n 1", N1, "-n 2", N2, "-n 3", N3, "-n 4", N4,
                  # set the migration rate from wave to crab (looking forward in time) inhabiting the same location
                  "-m 3 1 0 -m 4 2 0 -m 1 3", mig_WC1, "-m 2 4", mig_WC2,
                  # set the migration rate between the same ecotypes inhabiting different locations
                  "-m 2 1", mig_CC, "-m 1 2", mig_CC, "-m 4 3", mig_WW, "-m 3 4", mig_WW,
                  # set the migration, between all populations, to zero - immediately before the recent split
                  "-eM", tmRS, "0",
                  # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                  "-ej", Rsplit, "1 2 -ej", Rsplit, "4 3",
                  # set the size of the ancient populations
                  "-en", Rsplit, "2", NA1, "-en", Rsplit, "3", NA2,
                  # set the migration rate between the ancestral populations
                  "-em", tmAA, "2 3", mig_AA,
                  # set the migration, between all populations, to zero - immediately at the ancient split
                  "-eM", Asplit, "0",
                  # add a split event - this event creates the two ancestral populations
                  "-ej", Asplit, "2 3",
                  # finally, set the size of the most ancestral pop equal to the size of the reference population with:
                  "-eN", Asplit, 1)

  # create command line with no migration from the wave to the crab ecotype within the same location
  no.mWC <- paste(paste(nDip*2, collapse = " "), lociTotal[3], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "),
                  # set the size of the present day populations
                  "-n 1", N1, "-n 2", N2, "-n 3", N3, "-n 4", N4,
                  # set the migration rate from crab to wave (looking forward in time) inhabiting the same location
                  "-m 3 1", mig_CW1, "-m 4 2", mig_CW1, "-m 1 3 0 -m 2 4 0",
                  # set the migration rate between the same ecotypes inhabiting different locations
                  "-m 2 1", mig_CC, "-m 1 2", mig_CC, "-m 4 3", mig_WW, "-m 3 4", mig_WW,
                  # set the migration, between all populations, to zero - immediately before the recent split
                  "-eM", tmRS, "0",
                  # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                  "-ej", Rsplit, "1 2 -ej", Rsplit, "4 3",
                  # set the size of the ancient populations
                  "-en", Rsplit, "2", NA1, "-en", Rsplit, "3", NA2,
                  # set the migration rate between the ancestral populations
                  "-em", tmAA, "3 2", mig_AA,
                  # set the migration, between all populations, to zero - immediately at the ancient split
                  "-eM", Asplit, "0",
                  # add a split event - this event creates the two ancestral populations
                  "-ej", Asplit, "2 3",
                  # finally, set the size of the most ancestral pop equal to the size of the reference population with:
                  "-eN", Asplit, 1)

  # create a command line for the loci without any migration (between the different ecotypes)
  # set the basic elements for scrm - nhap: total number of haplotypes that are simulated at each locus and
  # nrep: the number of independent loci that will be produced
  without.mig <- paste(paste(nDip*2, collapse = " "), lociTotal[4], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "), "0",
                       # set the size of the present day populations - n <i> <n> Set the size of population i to n*N0.
                       "-n 1", N1, "-n 2", N2, "-n 3", N3, "-n 4", N4,
                       # set the migration rate between the same ecotypes inhabiting different locations
                       "-m 2 1", mig_CC, "-m 1 2", mig_CC, "-m 4 3", mig_WW, "-m 3 4", mig_WW,
                       # set the migration, between all populations, to zero - immediately before the recent split
                       "-eM", tmRS, "0",
                       # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                       "-ej", Rsplit, "1 2 -ej", Rsplit, "4 3",
                       # set the size of the ancient populations
                       "-en", Rsplit, "2", NA1, "-en", Rsplit, "3", NA2,
                       # set the migration, between all populations, to zero - immediately at the ancient split
                       "-eM", Asplit, "0",
                       # add a split event - this event creates the two ancestral populations
                       "-ej", Asplit, "2 3",
                       # finally, set the size of the most ancestral pop equal to the size of the reference population with:
                       "-eN", Asplit, 1)

  # combine the two different types of commands
  cmdSingle <- c(with.mig, no.mCW, no.mWC, without.mig)

  # if extra is equal to TRUE, then we simulated more loci than required
  if(extra == TRUE)
    # include the required number of loci per category in the output
    cmdSingle <- list(commands = cmdSingle, targetLoci = targetLoci)

  # output the command line for the single origin model
  cmdSingle
}


#' Create SCRM command line for a parallel origin scenario
#'
#' This function creates a command line tailored for a scenario of parallel
#' origin to explain ecotype formation. The command line can then be fed to the
#' scrm package to run the model.
#'
#' For convenience, imagine we have two divergent ecotypes, named C and W. This
#' model assumes that the first population corresponds to the C ecotype at the
#' first location, the second population to the W ecotype in the first location,
#' the third population to the C ecotype in the second location and the fourth
#' population to the W ecotype in the second location.
#'
#' @param parameters A vector where each entry corresponds to a different
#'   parameter, e.g. one entry is the size of the reference population, another
#'   is the time of recent split, etc. Please note that this functions depends
#'   on the ordering of the parameters in the vector and thus, it should only be
#'   used with a vector created with the `createParams` function.
#' @param nSites An integer representing the number of base pairs that each
#'   locus should have.
#' @param nLoci An integer that represents how many independent loci should be
#'   simulated.
#' @param nDip An integer representing the total number of diploid individuals
#'   to simulate. Note that scrm actually simulates haplotypes, so the number of
#'   simulated haplotypes is double of this. Also note that this is the total
#'   number of diploid individuals and this function will distribute the
#'   individuals equally by the two populations.
#' @param mutrate A number representing the mutation rate assumed for the
#'   simulations.
#' @param extra is a logical value indicating whether the required number of
#'   loci should be enforced. The default is FALSE but, if set to TRUE, then
#'   additional loci will be simulated. These additional loci are simulated to
#'   try to have sufficient loci to keep the required number of loci after
#'   filtering.
#'
#' @return a character vector with four entries. The first entry is the scrm
#'   command line for the loci without any barriers against migration. The
#'   second entry is the command line for the loci without migration from the C
#'   towards the W ecotype. The third entry is command line for the loci without
#'   migration from the W towards the C ecotype and the last entry is the scrm
#'   command line for the loci without migration between divergent ecotypes.
#'
#' @examples
#' # create a vector with parameter values for the parallel origin scenario
#' params <- createParams(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250),
#' seq = c(0.0001, 0.001), split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3),
#' CC =  c(1e-13, 1e-3), WW = c(1e-13, 1e-3), ANC = c(1e-13, 1e-3), bT = c(0, 0.2),
#' bCW = c(0, 0.5), bWC = c(0, 0.5), model = "Parallel")
#'
#' # create the command line for the scrm package
#' cmdParallel(parameters = params, nSites = 2000, nLoci = 100, nDip = 400, mutrate = 2-8)
#'
#' @export
cmdParallel <- function(parameters, nSites, nLoci, nDip, mutrate, extra = FALSE) {

  # this function is intended to be used with a four-population model
  nPops <- 4

  # Read the vector with the parameters and assign each parameter to the correct command name
  Ne <- parameters[1]
  # set the relative size of each population - for the extant populations
  N1 <- parameters[2]; N2 <- parameters[3]; N3 <- parameters[4]; N4 <- parameters[5]
  # and the ancient populations
  NA1 <- parameters[6]; NA2 <- parameters[7]

  # get the proportion of loci with migration - no barriers against migration between the different ecotypes
  pM <- parameters[19]
  # get the proportion of loci without migration - from the crab to the wave ecotype at the same location
  pCW <- parameters[20]
  # get the proportion of loci without migration - from the wave to the crab ecotype at the same location
  pWC <- parameters[21]
  # and the proportion of loci without any migration - total barrier against migration between the different ecotypes
  pNO <- parameters[22]

  # get the migration rates - between different ecotypes at the same site - this is the m value on the M = 4N0m formula
  # from the crab ecotype to the wave ecotype - at the first site
  mCW1 <- parameters[12]
  # from the crab ecotype to the wave ecotype - at the second site
  mCW2 <- parameters[13]
  # from the wave ecotype to the crab ecotype - at the fist site
  mWC1 <- parameters[14]
  # from the wave ecotype to the crab ecotype - at the second site
  mWC2 <- parameters[15]

  # between crab populations inhabiting different locations
  mCC <- parameters[16]
  # between wave populations inhabiting different locations
  mWW <- parameters[17]
  # and between the two ancestral populations
  mAA <- parameters[18]

  # m <i> <j> <M>: Set the migration rate from population j to population i to M (looking forward in time)
  # and REMEMBER that M = 4N0m
  # set the migration rates to the scale of Nref - between different ecotypes at the same site
  # from the crab ecotype to the wave ecotype - at the first site
  mig_CW1 <- 4*Ne*mCW1 # -m 2 1 mig_CW
  # from the crab ecotype to the wave ecotype - at the second site
  mig_CW2 <- 4*Ne*mCW2 # -m 4 3 mig_CW
  # from the wave ecotype to the crab ecotype - at the first site
  mig_WC1 <- 4*Ne*mWC1 # -m 1 2 mig_WC
  # from the wave ecotype to the crab ecotype - at the second site
  mig_WC2 <- 4*Ne*mWC2 # -m 3 4 mig_WC

  # between crab populations at different locations
  mig_CC <- 4*Ne*mCC
  # between wave populations at different locations
  mig_WW <- 4*Ne*mWW
  # and between the two ancestral populations
  mig_AA <- 4*Ne*mAA

  # get the time of the recent split event
  Rsplit <- round(parameters[8], digits = 3)
  # and the ancient split event
  Asplit <- round(parameters[9], digits = 3)
  # the actual time of the ancient split event is obtained by doing:
  Asplit <- Rsplit + Asplit

  # Compute the value of theta
  mutrate_locus <- nSites*mutrate
  theta <- 4*Ne*mutrate_locus
  # Create a vector with information about how many haplotypes are sampled from each population
  n <- c(rep(nDip/nPops, times = nPops))

  # create a variable to be used for a minor correction related to the split time
  # this will ensure that the changes in migration rates occur at different times
  # a variable to inform when does migration start between ancestral populations
  tmAA <- Rsplit + 0.0001
  # if Rsplit is zero, we can not subtract something from it
  if(Rsplit != 0) {
    # when Rsplit is not zero, set the end of the migration before the split
    tmRS <- Rsplit - 0.0001
  } else {
    # when Rsplit is zero, set the end of the migration at the split
    tmRS <- Rsplit
  }

  # use a multinomial distribution to get the number of loci simulated under each category
  lociTotal <- as.vector(stats::rmultinom(n = 1, size = nLoci, c(pM, pCW, pWC, pNO)))

  # if extra is TRUE, then more loci than required per category will be simulated
  if(extra == TRUE) {
    # save the required number of simulated loci per category
    targetLoci <- lociTotal
    # simulate more 25 loci per category
    lociTotal <- lociTotal + 25
  }

  # cheat code: pop1 - crab in site 1; pop2 - wave in site 1; pop3 - crab in site 2; pop4 - wave in site 2

  # create command line with no barriers to migration
  # set the basic elements for scrm - nhap: total number of haplotypes that are simulated at each locus and
  # nrep: the number of independent loci that will be produced
  with.mig <- paste(paste(nDip*2, collapse = " "), lociTotal[1], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "),
                    # set the size of the present day populations - n <i> <n> Set the size of population i to n*N0.
                    "-n 1", N1, "-n 2", N2, "-n 3", N3, "-n 4", N4,
                    # m <i> <j> <M>: Set the migration rate from population j to population i to M (looking forward in time)
                    # set the migration rate between different ecotypes inhabiting the same location
                    "-m 2 1", mig_CW1, "-m 4 3", mig_CW2, "-m 1 2", mig_WC1, "-m 3 4", mig_WC2,
                    # set the migration rate between the same ecotypes inhabiting different locations
                    "-m 3 1", mig_CC, "-m 1 3", mig_CC, "-m 4 2", mig_WW, "-m 2 4", mig_WW,
                    # set the migration, between all populations, to zero - immediately before the recent split
                    "-eM", tmRS, "0",
                    # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                    "-ej", Rsplit, "1 2 -ej", Rsplit, "4 3",
                    # set the size of the ancient populations
                    # -en <t> <i> <n>: Set the size of population i to n*N0 at time t.
                    "-en", Rsplit, "2", NA1, "-en", Rsplit, "3", NA2,
                    # set the migration rate between the ancestral populations
                    "-em", tmAA, "2 3", mig_AA, "-em", tmAA, "3 2", mig_AA,
                    # set the migration, between all populations, to zero - immediately at the ancient split
                    "-eM", Asplit, "0",
                    # add a split event - this event creates the two ancestral populations
                    "-ej", Asplit, "2 3",
                    # finally, set the size of the most ancestral pop equal to the size of the reference population with:
                    # -eN <t> <n>: set the size of all populations to n*N0 at time t.
                    "-eN", Asplit, 1)

  # create command line with no migration from the crab to the wave ecotype within the same location
  no.mCW <- paste(paste(nDip*2, collapse = " "), lociTotal[2], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "),
                  # set the size of the present day populations - n <i> <n> Set the size of population i to n*N0
                  "-n 1", N1, "-n 2", N2, "-n 3", N3, "-n 4", N4,
                  # set the migration rate from wave to crab (looking forward in time) inhabiting the same location
                  "-m 2 1 0 -m 4 3 0 -m 1 2", mig_WC1, "-m 3 4", mig_WC2,
                  # set the migration rate between the same ecotypes inhabiting different locations
                  "-m 3 1", mig_CC, "-m 1 3", mig_CC, "-m 4 2", mig_WW, "-m 2 4", mig_WW,
                  # set the migration, between all populations, to zero - immediately before the recent split
                  "-eM", tmRS, "0",
                  # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                  "-ej", Rsplit, "1 2 -ej", Rsplit, "4 3",
                  # set the size of the ancient populations
                  "-en", Rsplit, "2", NA1, "-en", Rsplit, "3", NA2,
                  # set the migration rate between the ancestral populations
                  "-em", tmAA, "2 3", mig_AA, "-em", tmAA, "3 2", mig_AA,
                  # set the migration, between all populations, to zero - immediately at the ancient split
                  "-eM", Asplit, "0",
                  # add a split event - this event creates the two ancestral populations
                  "-ej", Asplit, "2 3",
                  # finally, set the size of the most ancestral pop equal to the size of the reference population with:
                  "-eN", Asplit, 1)

  # create command line with no migration from the wave to the crab ecotype within the same location
  no.mWC <- paste(paste(nDip*2, collapse = " "), lociTotal[3], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "),
                  # set the size of the present day populations
                  "-n 1", N1, "-n 2", N2, "-n 3", N3, "-n 4", N4,
                  # set the migration rate from crab to wave (looking forward in time) inhabiting the same location
                  "-m 2 1", mig_CW1, "-m 4 3", mig_CW2, "-m 1 2 0 -m 3 4 0",
                  # set the migration rate between the same ecotypes inhabiting different locations
                  "-m 3 1", mig_CC, "-m 1 3", mig_CC, "-m 4 2", mig_WW, "-m 2 4", mig_WW,
                  # set the migration, between all populations, to zero - immediately before the recent split
                  "-eM", tmRS, "0",
                  # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                  "-ej", Rsplit, "1 2 -ej", Rsplit, "4 3",
                  # set the size of the ancient populations
                  "-en", Rsplit, "2", NA1, "-en", Rsplit, "3", NA2,
                  # set the migration rate between the ancestral populations
                  "-em", tmAA, "2 3", mig_AA, "-em", tmAA, "3 2", mig_AA,
                  # set the migration, between all populations, to zero - immediately at the ancient split
                  "-eM", Asplit, "0",
                  # add a split event - this event creates the two ancestral populations
                  "-ej", Asplit, "2 3",
                  # finally, set the size of the most ancestral pop equal to the size of the reference population with:
                  "-eN", Asplit, 1)

  # create a command line for the loci without any migration (between the different ecotypes)
  without.mig <- paste(paste(nDip*2, collapse = " "), lociTotal[4], "-t",theta, "-I", paste(nPops), paste(n*2, collapse = " "), "0",
                       # set the size of the present day populations - n <i> <n> Set the size of population i to n*N0.
                       "-n 1", N1, "-n 2", N2, "-n 3", N3, "-n 4", N4,
                       # set the migration rate between the same ecotypes inhabiting different locations
                       "-m 3 1", mig_CC, "-m 1 3", mig_CC, "-m 4 2", mig_WW, "-m 2 4", mig_WW,
                       # set the migration, between all populations, to zero - immediately before the recent split
                       "-eM", tmRS, "0",
                       # add a split event -ej <t> <j> <i> in population i that creates population j (forwards in time)
                       "-ej", Rsplit, "1 2 -ej", Rsplit, "4 3",
                       # set the size of the ancient populations
                       "-en", Rsplit, "2", NA1, "-en", Rsplit, "3", NA2,
                       # set the migration rate between the ancestral populations
                       "-em", tmAA, "2 3", mig_AA, "-em", tmAA, "3 2", mig_AA,
                       # set the migration, between all populations, to zero - immediately at the ancient split
                       "-eM", Asplit, "0",
                       # add a split event - this event creates the two ancestral populations
                       "-ej", Asplit, "2 3",
                       # finally, set the size of the most ancestral pop equal to the size of the reference population with:
                       "-eN", Asplit, 1)

  # combine the four different types of commands
  cmdParallel <- c(with.mig, no.mCW, no.mWC, without.mig)

  # if extra is equal to TRUE, then we simulated more loci than required
  if(extra == TRUE)
    # include the required number of loci per category in the output
    cmdParallel <- list(commands = cmdParallel, targetLoci = targetLoci)

  # output the command line for the parallel origin model
  cmdParallel
}

#' Organize scrm output
#'
#' This function is utilized to sort out the scrm output. The order of the
#' populations changes accordingly to the model used (i.e. single or parallel
#' origin). Running this function will re-organize the output produced by scrm,
#' so that the populations are in the same order in both models.
#'
#' @param seg_sites a matrix of segregating sites as produced by scrm. Each
#'   column of the matrix is a different site and each row is a different
#'   haplotype.
#' @param nHap an integer representing the total number of haplotypes simulated.
#' @param nPops an integer, representing the total number of populations of the
#'   simulated model.
#'
#' @return a matrix of segregating sites, similar to `seg_sites` but with the
#'   populations organized so that the order is always the same, regardless of
#'   the model used.
#'
#' @keywords internal
#'
#' @export
organizeSCRM <- function(seg_sites, nHap, nPops) {

  # get the number of haplotypes simulated by population
  haPop <- nHap/nPops
  # create a vector with the index representing the beginning of each population
  beginPop <- seq(from = 1, to = nHap, by = haPop)

  # remove the name (position) of each site - this is something that scrm creates
  seg_sites <- unname(seg_sites)

  # in the single model, we need to switch the order of the second and third population
  # get the haplotypes corresponding to each population
  pop2 <- seg_sites[(beginPop[2]):(beginPop[3]-1), ]
  pop3 <- seg_sites[(beginPop[3]):(beginPop[4]-1), ]

  # re-organize the matrix of haplotypes with the populations in the correct order
  seg_sites[(beginPop[2]):(beginPop[3]-1), ] <- pop3
  seg_sites[(beginPop[3]):(beginPop[4]-1), ] <- pop2

  # output the matrix of haplotypes with the populations in the correct order
  seg_sites
}


#' Run scrm and obtain genotypes
#'
#' This function will run the scrm package, according to the command line
#' supplied as input. It will also combine haplotypes into genotypes and
#' re-organize the output if the simulations were performed under a single
#' origin scenario. This is to ensure that the output of the four-population
#' models will always follow the same order: the two divergent ecotypes in the
#' first location, followed by the two divergent ecotypes in the second
#' location.
#'
#' @param commands A character string containing the commands for the scrm
#'   package. This string can be created using the `cmd2pops`, the `cmdSingle`
#'   or the `cmdParallel` functions.
#' @param nDip An integer representing the total number of diploid individuals
#'   to simulate. Note that scrm actually simulates haplotypes, so the number of
#'   simulated haplotypes is double of this.
#' @param nPops An integer that informs of how many populations exist on the
#'   model you are trying to run.
#' @param model Either "2pops", "Single" or "Parallel" indicating which model
#'   should be simulated.
#'
#' @return a list with the simulated genotypes. Each entry is a different locus
#'   and, for each locus, different rows represent different individuals and
#'   each column is a different site.
#'
#' @examples
#' # create a vector with parameter values for a two populations model
#' params <- createParams(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250),
#' seq = c(0.0001, 0.001), split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3),
#' bT = c(0, 0.2), model = "2pops")
#'
#' # create the command line for the scrm package
#' cmds <- cmd2pops(parameters = params, nSites = 2000, nLoci = 10, nDip = 100, mutrate = 2e-8)
#'
#' # run SCRM and obtain the genotypes
#' runSCRM(commands = cmds, nDip = 100, nPops = 2, model = "2pops")
#'
#' @export
runSCRM <- function(commands, nDip, nPops, model) {

  # check if the input is correct - the selected model should be one of the following
  if(model %in% c("2pops", "Single", "Parallel") == FALSE)
    stop(paste("The selected model should be either 2pops, Single or Parallel. Please check"))

  # binding the variable locally to the function
  temp1 <- NULL

  # run the scrm package for each set of commands - with and without migration
  simulation <- lapply(commands, FUN = function(x) scrm::scrm(x))

  # extract the information from each simulation and store it on a temporary matrix
  for (i in 1:length(simulation)) {

    # create the temporary matrix for each simulation
    assign(paste("temp", i, sep = ""), simulation[[i]][["seg_sites"]])
  }

  if(length(simulation) != 1) {

    # combine all simulations into a matrix of haplotypes
    haplotypes <- append(temp1, unlist(mget(paste0("temp", 2:length(simulation))), recursive = FALSE, use.names = FALSE))

  } else {

    # if only set of simulations was performed, only one set of haplotypes exist
    haplotypes <- temp1
  }

  # get the total number of haplotypes
  nHap <- nDip*2

  # apply a correction for the situations where scrm does not produce a single polymorphic site
  # first check the dimensions of each list entry
  size <- matrix(unlist(lapply(haplotypes, dim)), ncol = 2, byrow = TRUE)

  # if one entry has no columns, i.e. no sites, then add columns containing only zeros to that entry
  if(any(size[, 2] == 0)) {

    # add two columns containing zeros to that locus
    haplotypes <- poolHelper::haplo.fix(haplotypes = haplotypes, nHap = nHap)
  }

  # re-organize output for the single model
  if (model == "Single") {

    # change the order of the populations in the single origin model
    # so that the order is always: ecotype C and W in the first location and ecotype C and W in the second location
    haplotypes <- lapply(haplotypes, function(segSites) organizeSCRM(segSites, nHap, nPops))
  }

  # convert the haplotypes to genotypes
  genotypes <- poolHelper::GetGenotypes(haplotypes, nDip = nDip)

  # output the genotypes
  genotypes
}


#' Force the simulations to contain the required number of loci
#'
#' This function attempts to force the required number of loci after the
#' filtering steps are performed.
#'
#' This is done by simulating extra loci for each of the different types of
#' simulations performed. The possible types of simulations include loci without
#' barriers against migration between divergent ecotypes, loci without migration
#' from the C towards the W ecotype, loci without migration from the W towards
#' the C ecotypes and loci where no migration occurs between divergent ecotypes.
#' Using this function, more loci than required are simulated for each of those
#' types of simulations.
#'
#' Then, a coverage-based filter is applied to the data, followed by a filter
#' based on a required number of minor-allele reads per site. Those filters
#' remove some loci from the data. The extra simulated loci should allow us to
#' keep the required number of loci per type of simulation even after filtering.
#'
#'
#' @param model a character, either 2pops", "Single" or "Parallel" indicating
#'   which model should be simulated.
#' @param parameters a vector of parameters used to create the command line for
#'   the scrm package. Each entry of the vector is a different parameter. Note
#'   that each vector entry should be named with the name of the corresponding
#'   parameter. The output of the `CreateParameters` function is the intended
#'   input.
#' @param nSites is an integer that specifies how many base pairs should scrm
#'   simulate, i.e. how many sites per locus to simulate.
#' @param nLoci an integer that represents how many independent loci should be
#'   simulated.
#' @param nDip an integer representing the total number of diploid individuals
#'   to simulate. Note that scrm actually simulates haplotypes, so the number of
#'   simulated haplotypes is double of this. Also note that this is the total
#'   number of diploid individuals and this function will distribute the
#'   individuals equally by the simulated populations.
#' @param mutrate an integer representing the mutation rate assumed for the
#'   simulations.
#' @param mean an integer or a vector defining the mean value of the negative
#'   binomial distribution from which different number of reads are drawn. It
#'   represents the mean coverage across all sites. If a vector is supplied, the
#'   function assumes that each entry of the vector is the mean for a different
#'   population.
#' @param variance an integer or a vector defining the variance of the negative
#'   binomial distribution from which different number of reads are drawn. It
#'   represents the variance of the total coverage across all sites. If a vector
#'   is supplied, the function assumes that each entry of the vector is the
#'   variance for a different population.
#' @param minimum an integer representing the minimum coverage allowed. Sites
#'   where any population has a depth of coverage below this threshold are
#'   removed from the data.
#' @param maximum an integer representing the maximum coverage allowed. Sites
#'   where any population has a depth of coverage above this threshold are
#'   removed from the data.
#' @param size a list with one entry per population. Each entry should be a
#'   vector containing the size (in number of diploid individuals) of each pool.
#'   Thus, if a population was sequenced using a single pool, the vector should
#'   contain only one entry. If a population was sequenced using two pools, each
#'   with 10 individuals, this vector should contain two entries and both will
#'   be 10.
#' @param min.minor is an integer representing the minimum allowed number of
#'   minor-allele reads. Sites that, across all populations, have less
#'   minor-allele reads than this threshold will be removed from the data.
#'
#' @return a list with two names entries
#'
#'   \item{pool}{a list with three different entries: major, minor and total.
#'   This list is obtained by running the \code{\link{forcePool}} function.}
#'
#'   \item{nPoly}{a numeric value indicating the mean number of polymorphic
#'   sites across all simulated locus.}
#'
#' @examples
#' # create a vector with parameter values for a two populations model
#' params <- createParams(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250),
#' seq = c(0.0001, 0.001), split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3),
#' bT = c(0, 0.2), model = "2pops")
#'
#' # simulate exactly 10 loci - using an isolation with migration model with two populations
#' forceLocus(model = "2pops", parameters = params, nSites = 1000, nLoci = 10, nDip = 100,
#' mutrate = 2e-8, mean = c(100, 100), variance = c(250, 250), minimum = 10, maximum = 200,
#' size = list(50, 50), min.minor = 0)
#'
#' @export
forceLocus <- function(model, parameters, nSites, nLoci, nDip, mutrate, mean, variance, minimum, maximum,
                       size, min.minor) {

  # create the command line to run the scrm package
  # the command line varies according to the selected model
  if(model == "2pops") {

    # create the command line for a star shaped model
    commands <- cmd2pops(parameters, nSites, nLoci, nDip, mutrate, extra = TRUE)
    # set the number of populations
    nPops <- 2

  } else if (model == "Parallel") {

    # create the command line for the parallel origin model
    commands <- cmdParallel(parameters, nSites, nLoci, nDip, mutrate, extra = TRUE)
    # set the number of populations
    nPops <- 4

  } else if (model == "Single") {

    # create the command line for the single origin model
    commands <- cmdSingle(parameters, nSites, nLoci, nDip, mutrate, extra = TRUE)
    # set the number of populations
    nPops <- 4

  } else {

    # if a correct model is not supplied as input for the function - stop and warn
    stop(paste("model should be 2pops, Parallel or Single. Please check!"))
  }

  # get the required number of loci per category - this is the number of loci we want in the end
  target <- commands[["targetLoci"]]
  # get the command line for scrm
  commands <- commands[["commands"]]

  # the length of the commands object indicates how many different categories of simulations we are performing
  # the possible categories are: loci with no barriers to migration, loci with no migration from one ecotype to the other
  # and loci without any migration between the different ecotypes
  nSims <- length(commands)

  # run the scrm package and obtain the genotypes
  genotypes <- lapply(commands, function(sim) runSCRM(commands = sim, nDip, nPops, model))

  # get the mean number of polymorphic sites
  nPoly <- mean(unlist(sapply(genotypes, function(sim) sapply(sim, ncol))))

  # simulate total number of reads per site
  reads <- lapply(genotypes, function(sim) poolHelper::simulateCoverage(mean, variance, genotypes = sim))

  # remove sites with a depth of coverage above or below the defined threshold
  reads <- lapply(1:nSims, function(sim)
    poolHelper::remove_by_reads(nLoci = length(reads[[sim]]), reads[[sim]], minimum = minimum,
                                maximum = maximum, genotypes = genotypes[[sim]]))

  # get the genotypes - without sites simulated with a coverage below or above the threshold
  genotypes <- lapply(reads, FUN = function(sim) lapply(sim, "[[", 2))
  # get the reads - without sites simulated with a coverage below or above the threshold
  reads <- lapply(reads, FUN = function(sim) lapply(sim, "[[", 1))

  # check the dimensions of the matrices with the genotypes
  # it is possible that some loci do not have any polymorphic site after the coverage filter
  dimensions <- lapply(genotypes, function(sim) sapply(sim, ncol))
  # keep only those loci where we have at least one polymorphic site
  tokeep <- lapply(dimensions, function(sim) sim != 0)
  # remove loci without polymorphic sites from the genotypes
  genotypes <- lapply(1:nSims, function(sim) genotypes[[sim]][tokeep[[sim]]])
  # remove loci without polymorphic sites from the matrices with the coverage
  reads <- lapply(1:nSims, function(sim) reads[[sim]][tokeep[[sim]]])

  # simulate individual contribution to the total number of reads
  indContribution <- lapply(1:nSims, FUN = function(sim) lapply(reads[[sim]], function(locus)
    poolHelper::popsReads(list_np = size, coverage = locus, pError = parameters["PoolError"])))

  # simulate the number of reference reads
  reference <- lapply(1:nSims, FUN = function(sim) lapply(1:length(genotypes[[sim]]), function(locus)
    poolHelper::numberReferencePop(genotypes = genotypes[[sim]][[locus]], indContribution = indContribution[[sim]][[locus]],
                                   size = size, error = parameters["SeqError"])))

  # simulate pooled sequencing data
  pool <- lapply(1:nSims, function(sim)
    poolHelper::poolPops(nPops, nLoci=length(indContribution[[sim]]), indContribution=indContribution[[sim]],
                         readsReference=reference[[sim]]))

  # if min.minor is not zero - filter out the sites with less than the required number of minor-allele reads
  if(min.minor != 0)
    pool <- poolHelper::filterPool(pool = pool, nloci = nLoci, min.minor = min.minor)

  # use an lapply to ensure that the reference allele of the simulations is also the major allele
  pool <- lapply(X = pool, function(sim) lapply(1:length(sim[["reference"]]), function(locus)
    poolHelper::findMinor(reference = sim[["reference"]][[locus]], alternative = sim[["alternative"]][[locus]],
                          coverage = sim[["total"]][[locus]])))

  # convert the pool list back to the previous format:
  # one entry for major allele, one for minor allele and a final one for total coverage
  pool <- lapply(1:nSims, function(sim)
    list(major = lapply(pool[[sim]], function(locus) locus[["major"]]),
         minor = lapply(pool[[sim]], function(locus) locus[["minor"]]),
         total = lapply(pool[[sim]], function(locus) locus[["total"]])))

  # remove loci without polymorphic sites and randomly select the required number of loci per category
  pool <- poolHelper::forcePool(nSims, pool = pool, target)

  # output the pooled sequencing data and the number of polymorphic sites prior to filtering
  list(pool = pool, nPoly = nPoly)
}


#' Compute summary statistics from Pooled DNA sequencing
#'
#' This function combines all the necessary steps to simulate pooled sequencing
#' data and compute summary statistics from that data.
#'
#' The sampled parameter values are incorporated into a command line for the
#' scrm package. Then, genetic data is simulated according to a model of ecotype
#' formation and the sampled parameters. Finally, various summary statistics are
#' calculated from the simulated data.
#'
#' @param parameters a vector of parameters used to create the command line for
#'   the scrm package. Each entry of the vector is a different parameter. Note
#'   that each vector entry should be named with the name of the corresponding
#'   parameter. The output of the `CreateParameters` function is the intended
#'   input.
#' @param model a character, either 2pops", "Single" or "Parallel" indicating
#'   which model should be simulated.
#' @param nDip an integer representing the total number of diploid individuals
#'   to simulate. Note that scrm actually simulates haplotypes, so the number of
#'   simulated haplotypes is double of this. Also note that this is the total
#'   number of diploid individuals and this function will distribute the
#'   individuals equally by the simulated populations.
#' @param size a list with one entry per population. Each entry should be a
#'   vector containing the size (in number of diploid individuals) of each pool.
#'   Thus, if a population was sequenced using a single pool, the vector should
#'   contain only one entry. If a population was sequenced using two pools, each
#'   with 10 individuals, this vector should contain two entries and both will
#'   be 10.
#' @param nLoci an integer that represents how many independent loci should be
#'   simulated.
#' @param nSites is an integer that specifies how many base pairs should scrm
#'   simulate, i.e. how many sites per locus to simulate.
#' @param mutrate an integer representing the mutation rate assumed for the
#'   simulations.
#' @param mean an integer or a vector defining the mean value of the negative
#'   binomial distribution from which different number of reads are drawn. It
#'   represents the mean coverage across all sites. If a vector is supplied, the
#'   function assumes that each entry of the vector is the mean for a different
#'   population.
#' @param variance an integer or a vector defining the variance of the negative
#'   binomial distribution from which different number of reads are drawn. It
#'   represents the variance of the total coverage across all sites. If a vector
#'   is supplied, the function assumes that each entry of the vector is the
#'   variance for a different population.
#' @param minimum an integer representing the minimum coverage allowed. Sites
#'   where any population has a depth of coverage below this threshold are
#'   removed from the data.
#' @param maximum an integer representing the maximum coverage allowed. Sites
#'   where any population has a depth of coverage above this threshold are
#'   removed from the data.
#' @param min.minor is an integer representing the minimum allowed number of
#'   minor-allele reads. Sites that, across all populations, have less
#'   minor-allele reads than this threshold will be removed from the data.
#' @param force is a logical value indicating whether the required number of
#'   loci should be enforced. The default is FALSE but, if set to TRUE, then
#'   additional loci will be simulated. These additional loci are simulated to
#'   try to have sufficient loci to keep the required number of loci after
#'   filtering.
#'
#' @return a list with several named entries. The number of entries depends of
#'   the chosen model.
#'
#'   \item{nPoly}{numeric, mean number of polymorphic sites across all simulated
#'   locus.}
#'
#'   \item{nFilter}{numeric, mean number of polymorphic sites retained after
#'   filtering across all simulated locus.}
#'
#'   \item{nLoci}{numeric, total number of loci retained after filtering.
#'   Summary statistics are calculated for these loci.}
#'
#'   \item{Sf}{numeric, fraction of sites fixed between populations. For the
#'   model with two populations, this is a single value. For the four-population
#'   models, this includes three values: the first is the fraction of fixed
#'   sites between the two populations in the first location, the second value
#'   is between the populations in the second location and the third value is
#'   the overall fraction of fixed sites, obtained by comparing each population
#'   against the other three.}
#'
#'   \item{Sx}{numeric, fraction of exclusive sites per population. When running
#'   the model with two populations, this entry has two values - one per
#'   population. For the four-population models, there is also one value per
#'   population, followed by a fifth value representing the fraction of sites
#'   that are segregating in only one of the populations.}
#'
#'   \item{SS}{numeric values representing the fraction of sites shared between
#'   populations. For the model with two populations, this is a single value.
#'   When running one of the four-population models, this entry has three
#'   values. The first is the fraction of shared sites between the two
#'   populations in the first location, the second value is between the
#'   populations in the second location and the third value is the fraction of
#'   shared sites across all four populations.}
#'
#'   \item{Mean_Het}{numeric, expected heterozygosity within each population.
#'   This entry has two values when using a two populations model and four when
#'   running one of the four-populations model.}
#'
#'   \item{SD_Het}{numeric, standard deviation of the expected heterozygosity
#'   for each population. This entry has two values when using a two populations
#'   model and four when running one of the four-populations model.}
#'
#'   \item{Mean_HetBet}{numeric, mean heterozygosity between all pairs of
#'   populations. For the two populations model, this is a single value
#'   representing the heterozygosity between the two populations. For the
#'   four-population models, this entry includes six values. The first value is
#'   the heterozygosity between the first and the second population, the second
#'   value is between the first and the third population, the third value is
#'   between the first and fourth population, the fourth value is between the
#'   second and third populations, the fifth value is between the second and
#'   fourth population and the sixth value is between the third and fourth
#'   populations.}
#'
#'   \item{SD_HetBet}{numeric, standard deviation of the mean heterozygosity
#'   between all pairs of populations. For the two populations model, this is a
#'   single value representing the standard deviation of heterozygosity between
#'   the two populations. When running one of the four-population models, this
#'   entry includes six values. The order of those entries is the same as for
#'   `Mean_HetBet`.}
#'
#'   \item{Mean_FST}{numeric, mean pairwise FST between populations. For the two
#'   populations model, this is a single value representing the mean FST between
#'   the two populations. For the four-population models, this entry includes
#'   six values. The first value is the mean FST between the first and second
#'   populations, the second is between the first and third population, the
#'   third is between the second and third populations, the fourth is between
#'   the first and fourth populations, the fifth value is between the second and
#'   fourth populations and the sixth is between the third and fourth
#'   populations.}
#'
#'   \item{SD_FST}{numeric, standard deviation of the mean pairwise FST between
#'   populations. For the two populations model, this is a single value
#'   representing the standard deviation of the FST between the two populations.
#'   When running one of the four-population models, this entry includes six
#'   values. The order of those entries is the same as for `Mean_FST`.}
#'
#'   \item{FSTQ1}{numeric, it is the 5% quantile of the mean pairwise FST
#'   distribution. For the two populations model, this is a single value
#'   representing the 5% quantile of the FST between the two populations. When
#'   running one of the four-population models, this entry includes six values.
#'   The order of those entries is the same as for `Mean_FST`.}
#'
#'   \item{FSTQ2}{numeric, it is the 95% quantile of the mean pairwise FST
#'   distribution. For the two populations model, this is a single value
#'   representing the 95% quantile of the FST between the two populations. For
#'   the four-population models, this entry includes six values. The order of
#'   those entries is the same as for `Mean_FST`.}
#'
#'   \item{Dstat}{numeric, value of D-statistic for various combinations of
#'   populations. This entry only exists if a four-population model was
#'   selected. It includes three different values. For the first value, P1 was
#'   the W ecotype in the first location P2 was the W ecotype in the second
#'   location and P3 was the C ecotype at the first location. For the second
#'   value P1 was again the W ecotype in the first location but P2 was the C
#'   ecotype in the second ecotype and P3 was the C ecotype at the first
#'   location. For the third value, P1 was also the W ecotype at the first
#'   location, P2 was the C ecotype at the first location and P3 was the W
#'   ecotype at the second location. For all combinations, P4 was assumed to be
#'   an outgroup fixed, at all sites, for the major allele.}
#'
#'   \item{SD_dstat}{numeric, standard deviation of D-statistic for various
#'   combinations of populations. This entry only exists if a four-population
#'   model was selected. Each entry is the standard deviation of the
#'   corresponding D-statistic in the `Dstat` entry.}
#'
#' @examples
#' # create a vector of parameters for a model with two populations
#' parameters <- createParams(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250),
#' seq = c(0.0001, 0.001), split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3),
#' bT = c(0, 0.2), model = "2pops")
#'
#' # simulate a two populations model:
#' # note that we are using two pools for each population, each with 50 individuals
#' poolStats(parameters = parameters, model = "2pops", nDip = 200, size = rep(list(rep(50, 2)), 2),
#' nLoci = 100, nSites = 2000, mutrate = 2e-8, mean = c(100, 80), variance = c(200, 180), minimum = 10,
#' maximum = 150, min.minor = 1)
#'
#' @export
poolStats <- function(parameters, model, nDip, size, nLoci, nSites, mutrate, mean, variance,
                      minimum, maximum, min.minor = NA, force = FALSE) {

  # if force is equal to TRUE, this function will try to force the required number of loci specified by the nLoci input
  if(force == TRUE) {

    # set the number of populations
    if(model == "2pops")
      nPops <- 2
    else
      nPops <- 4

    # force the simulations to contain the required number of loci
    pool <- forceLocus(model, parameters, nSites, nLoci, nDip, mutrate, mean, variance, minimum, maximum, size, min.minor)
    # get the number of polymorphic sites prior to filtering from the output of the previous function
    nPoly <- pool[[2]]
    # get the pooled sequencing data
    pool <- pool[[1]]


  } else { # if force is FALSE, this function will not try to constrain the required number of loci

    # create the command line to run the scrm package
    # the command line varies according to the selected model
    if(model == "2pops") {

      # create the command line for an isolation with migration model with two populations
      commands <- cmd2pops(parameters, nSites, nLoci = nLoci, nDip, mutrate)
      # set the number of populations
      nPops <- 2

    } else if (model == "Parallel") {

      # create the command line for the parallel origin model
      commands <- cmdParallel(parameters, nSites, nLoci = nLoci, nDip, mutrate)
      # set the number of populations
      nPops <- 4

    } else if (model == "Single") {

      # create the command line for the single origin model
      commands <- cmdSingle(parameters, nSites, nLoci = nLoci, nDip, mutrate)
      # set the number of populations
      nPops <- 4

    } else {

      # if a correct model is not supplied as input for the function - stop and warn
      stop(paste("model should be 2pops, Parallel or Single. Please check!"))
    }

    # run the scrm package and obtain the genotypes
    genotypes <- runSCRM(commands, nDip, nPops, model)

    # get the mean number of polymorphic sites
    nPoly <- mean(sapply(genotypes, ncol))

    # simulate number of reads
    reads <- poolHelper::simulateCoverage(mean, variance, genotypes = genotypes)

    # remove sites with a depth of coverage above or below the defined threshold
    reads <- poolHelper::remove_by_reads(nLoci = nLoci, reads, minimum = minimum, maximum = maximum,
                                         genotypes = genotypes)

    # get the genotypes - without sites simulated with a coverage below or above the threshold
    genotypes <- lapply(reads, function(locus) locus[[2]])

    # check the dimensions of the matrices with the genotypes
    dimensions <- matrix(unlist(lapply(genotypes, dim)), ncol = 2, byrow = TRUE)
    # we only wish to keep the locus where we have at least one polymorphic site
    tokeep <- dimensions[, 2] != 0
    # remove all loci without polymorphic sites
    genotypes <- genotypes[tokeep]

    # get the reads - without sites simulated with a coverage below or above the threshold
    reads <- lapply(reads, function(locus) locus[[1]])
    # use the same index to remove entries of the reads list that correspond to locus without polymorphic sites
    reads <- reads[tokeep]
    # ensure that each entry is a matrix
    reads <- lapply(reads, function(locus) matrix(locus, nrow = nPops))

    # after removing loci, update the nLoci object to reflect the number of loci that we now have
    nLoci <- length(genotypes)

    # simulate individual contribution to the total number of reads
    indContribution <- lapply(1:nLoci, function(locus)
      poolHelper::popsReads(list_np = size, coverage = reads[[locus]], pError = parameters["PoolError"]))

    # simulate the number of reference reads
    reference <- lapply(1:nLoci, function(locus)
      poolHelper::numberReferencePop(genotypes = genotypes[[locus]], indContribution = indContribution[[locus]],
                                     size = size, error = parameters["SeqError"]))

    # simulate pooled sequencing data
    pool <- poolHelper::poolPops(nPops = nPops, nLoci = nLoci, indContribution = indContribution,
                                 readsReference = reference)

    # if min.minor is not zero - filter out the sites with less than the required number of minor-allele reads
    if(min.minor != 0)
      pool <- poolHelper::filterPool(pool = pool, nloci = nLoci, min.minor = min.minor)

    # use an lapply to ensure that the reference allele of the simulations is also the major allele
    pool <- lapply(1:nLoci, function(locus)
      poolHelper::findMinor(reference = pool[["reference"]][[locus]], alternative = pool[["alternative"]][[locus]],
                            coverage = pool[["total"]][[locus]]))

    # convert the pool list back to the previous format
    # one entry for reference matrices, one for alternative and a final one for total matrices
    pool <- list(major = lapply(pool, function(locus) locus[["major"]]),
                 minor = lapply(pool, function(locus) locus[["minor"]]),
                 total = lapply(pool, function(locus) locus[["total"]]))

    # the previous step removed sites with a number of minor-allele reads below the threshold
    # this might mean that some loci are left without any polymorphic site
    # check the dimensions of the matrices with the number of minor-allele reads
    dimensions <- matrix(unlist(lapply(pool[["minor"]], dim)), ncol = 2, byrow = TRUE)
    # we only wish to keep locus where we have at least one polymorphic site
    tokeep <- dimensions[, 2] != 0
    # remove all loci without polymorphic sites
    pool[["major"]] <- pool[["major"]][tokeep]
    pool[["minor"]] <- pool[["minor"]][tokeep]
    pool[["total"]] <- pool[["total"]][tokeep]
  }

  # it is possible that some loci were eliminated during the previous steps
  # thus, it might be necessary to update the nLoci variable
  nLoci <- length(pool[["minor"]])

  # compute the fraction of sites showing a fixed difference between the populations
  Sf <- lapply(1:nLoci, function(locus)
    fixed(minor = pool[["minor"]][[locus]], total = pool[["total"]][[locus]], nPops))
  # combine the previous list into a single matrix - where each column is a different comparison
  Sf <- do.call(rbind, Sf)
  # get the mean, across all loci, for the fraction of sites showing a fixed difference
  Sf <- colMeans(Sf, na.rm = TRUE)

  # calculate the fraction of sites showing an exclusive polymorphism to a given population
  Sx <- lapply(1:nLoci, function(locus)
    exclusive(minor = pool[["minor"]][[locus]], total = pool[["total"]][[locus]], nPops))
  # combine the previous list into a single matrix - where each column is a different comparison
  Sx <- do.call(rbind, Sx)
  # compute the mean (across loci) for each population
  Sx <- colMeans(Sx, na.rm = TRUE)

  # compute the fraction of sites with a polymorphism shared between the populations
  SS <- lapply(1:nLoci, function(locus)
    shared(minor = pool[["minor"]][[locus]], total = pool[["total"]][[locus]], nPops))
  # combine the previous list into a single matrix - where each column is a different comparison
  SS <- do.call(rbind, SS)
  # get the mean, across all loci, for the fraction of sites shared polymorphism between the populations
  SS <- colMeans(SS, na.rm = TRUE)

  # Calculate population allelic frequency
  Pop_Pi <- poolHelper::calculatePi(listPool = pool, nLoci = nLoci)
  pool <- Pop_Pi[[2]]
  Pop_Pi <- Pop_Pi[[1]]

  # get the mean number of polymorphic sites - after filtering
  nFilter <- mean(sapply(Pop_Pi, ncol))

  # Compute the mean expected heterozygosity for each population and locus
  ExpHet <- meanExpected_Het(Pop_Pi)
  # Compute the mean (across loci) for each population
  PopHet <- do.call(rbind,ExpHet)
  # Compute mean expected heterozygosity for each population across all loci
  HetMean <- colMeans(PopHet, na.rm = TRUE)
  # And the standard deviation
  HetSD <- apply(PopHet, 2, stats::sd, na.rm = TRUE)

  # Compute the heterozygosity between populations
  HetBetween <- Het_Between(Pop_Pi)
  # Compute the mean (across loci) for each pairwise comparison
  PopBetHet <- do.call(rbind,HetBetween)
  MeanHetBet <- colMeans(PopBetHet, na.rm = TRUE)
  # And the standard deviation
  SDHetBet <- apply(PopBetHet, 2, stats::sd, na.rm = TRUE)

  # Calculate the FST value between populations
  FST <- popsFST(nPops = nPops, Pop_Pi, coverage = pool[["total"]])
  # The previous returns a matrix where each pairwise comparison has a FST value and the rest is NAs
  # This matrix can be reduced to a vector
  FST <- lapply(FST, FUN = function(x) {
    x[!is.na(x)]})
  # combine all entries into a single matrix
  FST <- do.call(rbind,FST)
  # replace negative values by zero
  FST[which(FST < 0)] = 0
  # Then, compute the mean FST value between pops
  MeanFST <- colMeans(FST, na.rm = TRUE)
  # And the standard deviation
  SDFST <- apply(FST, MARGIN = 2, stats::sd)
  # calculate the 5% and the 95% quantiles for the FST distribution
  FSTQ1 <- apply(FST, MARGIN = 2, function(col) unname(stats::quantile(col, probs = 0.05)))
  FSTQ2 <- apply(FST, MARGIN = 2, function(col) unname(stats::quantile(col, probs = 0.95)))

  # compute Dstat values - if 4 populations were used
  if (nPops == 4) {
    # calculate DSTAT values over a list - each entry is a different locus
    dstat <- lapply(Pop_Pi, function(pi) D.statPool(pi))
    # combine all the values into a single matrix
    tempdstat <- do.call(rbind,dstat)
    # compute the mean value for a single simulation
    dstat <- colMeans(tempdstat, na.rm = TRUE)
    # And the standard deviation across locus
    SD_dstat <- apply(tempdstat, MARGIN = 2 , stats::sd, na.rm = TRUE)

    # create the output
    output <- list(nPoly, nFilter, nLoci, Sf, Sx, SS, HetMean, HetSD, MeanHetBet, SDHetBet, MeanFST, SDFST, FSTQ1, FSTQ2,
                   dstat, SD_dstat)
    names(output) = c("nPoly", "nFilter", "nLoci", "Sf", "Sx", "SS", "Mean_Het", "SD_Het", "Mean_HetBet", "SD_HetBet",
                      "Mean_FST", "SD_FST", "FSTQ1", "FSTQ2", "Dstat", "SD_dstat")

  } else { # if we are working with models that don't have 4 populations

    # create the output
    output <- list(nPoly, nFilter, nLoci, Sf, Sx, SS, HetMean, HetSD, MeanHetBet, SDHetBet, MeanFST, SDFST, FSTQ1, FSTQ2)
    names(output) = c("nPoly", "nFilter", "nLoci", "Sf", "Sx", "SS", "Mean_Het", "SD_Het", "Mean_HetBet", "SD_HetBet",
                      "Mean_FST", "SD_FST", "FSTQ1", "FSTQ2")
  }

  # output the final result of the function
  output
}


#' Simulation of Pooled DNA sequencing
#'
#' This is a master function that goes to all the steps required to obtain
#' summary statistics from pooled sequencing data.
#'
#' Starts by creating a vector of parameters, with values drawn from the
#' respective prior distributions. Then those parameter values are used to
#' simulate genetic data under a coalescent approach. A series of steps is then
#' followed to turn that genetic data into pooled sequencing data. Finally, a
#' set of summary statistics is computed using the simulated pooled sequencing
#' data.
#'
#' @param model a character, either 2pops", "Single" or "Parallel" indicating
#'   which model should be simulated.
#' @param nDip an integer representing the total number of diploid individuals
#'   to simulate. Note that scrm actually simulates haplotypes, so the number of
#'   simulated haplotypes is double of this. Also note that this is the total
#'   number of diploid individuals and this function will distribute the
#'   individuals equally by the simulated populations.
#' @param nPops An integer, representing the total number of populations of the
#'   simulated model.
#' @param size a list with one entry per population. Each entry should be a
#'   vector containing the size (in number of diploid individuals) of each pool.
#'   Thus, if a population was sequenced using a single pool, the vector should
#'   contain only one entry. If a population was sequenced using two pools, each
#'   with 10 individuals, this vector should contain two entries and both will
#'   be 10.
#' @param nLoci an integer that represents how many independent loci should be
#'   simulated.
#' @param nSites is an integer that specifies how many base pairs should scrm
#'   simulate, i.e. how many sites per locus to simulate.
#' @param mutrate an integer representing the mutation rate assumed for the
#'   simulations.
#' @param mean an integer or a vector defining the mean value of the negative
#'   binomial distribution from which different number of reads are drawn. It
#'   represents the mean coverage across all sites. If a vector is supplied, the
#'   function assumes that each entry of the vector is the mean for a different
#'   population.
#' @param variance an integer or a vector defining the variance of the negative
#'   binomial distribution from which different number of reads are drawn. It
#'   represents the variance of the total coverage across all sites. If a vector
#'   is supplied, the function assumes that each entry of the vector is the
#'   variance for a different population.
#' @param minimum an integer representing the minimum coverage allowed. Sites
#'   where any population has a depth of coverage below this threshold are
#'   removed from the data.
#' @param maximum an integer representing the maximum coverage allowed. Sites
#'   where any population has a depth of coverage above this threshold are
#'   removed from the data.
#' @param min.minor is an integer representing the minimum allowed number of
#'   minor-allele reads. Sites that, across all populations, have less
#'   minor-allele reads than this threshold will be removed from the data.
#' @param Nref is the minimum and maximum value of the uniform distribution for
#'   the effective population size of the reference population (Nref).
#' @param ratio is the minimum and maximum value of the distribution from which
#'   the relative size of the present-day and ancestral populations are drawn.
#'   The size of these populations is set as a ratio of the size of the Nref
#'   population. All of these ratios are drawn from a log10 uniform
#'   distribution.
#' @param split is the minimum and maximum values, at the 4Nref scale, of the
#'   uniform distribution from which the values of the times of the split events
#'   are draw. Both the time of the recent split event and the distance between
#'   the two split events are drawn from this distribution.
#' @param pool is the the minimum and maximum values of the uniform distribution
#'   from which the value of the error associated with DNA pooling is drawn.
#'   More specifically, this value is related with the unequal individual
#'   contribution to the pool. This parameter should be supplied as a decimal
#'   number between zero and one.
#' @param seq is the minimum and maximum values of the uniform distribution from
#'   which the value of the error associated with DNA sequencing is drawn. This
#'   parameter should be supplied as a decimal number between zero and one.
#' @param CW is the minimum and maximum value of the uniform distribution from
#'   which the migration rate between the two divergent ecotypes inhabiting the
#'   same location is drawn. We consider that this parameter is drawn on a m
#'   scale. This is the migration rate from ecotype C to ecotype W.
#' @param WC is the minimum and maximum value of the uniform distribution from
#'   which the migration rate between the two divergent ecotypes inhabiting the
#'   same location is drawn. We consider that this parameter is drawn on a m
#'   scale. This is the migration rate from ecotype W to ecotype C.
#' @param CC is the minimum and maximum value of the uniform distribution from
#'   which the migration rate between similar ecotypes inhabiting different
#'   locations is drawn. We consider that this parameter is drawn on a m scale.
#'   This is the migration between the two C ecotypes at two different
#'   locations.
#' @param WW is the minimum and maximum value of the uniform distribution from
#'   which the migration rate between similar ecotypes inhabiting different
#'   locations is drawn. We consider that this parameter is drawn on a m scale.
#'   This is the migration between the two W ecotypes at two different
#'   locations.
#' @param ANC is the minimum and maximum value of the uniform distribution from
#'   which the migration rate between similar ecotypes inhabiting different
#'   locations is drawn. We consider that this parameter is drawn on a m scale.
#'   This is the migration between the two W ecotypes at two different
#'   locations.
#' @param bT is the minimum and maximum values of the distribution from which
#'   the proportion of the simulated loci where no migration occurs between
#'   divergent ecotypes is drawn. The maximum value should not be higher than
#'   one.
#' @param bCW is the minimum and maximum values of the distribution from which
#'   the proportion of the simulated loci where no migration occurs from the C
#'   ecotype towards the W ecotype is drawn. The maximum value should not be
#'   higher than one.
#' @param bWC is the minimum and maximum values of the distribution from which
#'   the proportion of the simulated loci where no migration occurs from the W
#'   ecotype towards the C ecotype is drawn. The maximum value should not be
#'   higher than one.
#' @param force is a logical value indicating whether the required number of
#'   loci should be enforced. The default is FALSE but, if set to TRUE, then
#'   additional loci will be simulated. These additional loci are simulated to
#'   try to have sufficient loci to keep the required number of loci after
#'   filtering.
#'
#' @return a list with several named entries. The number of entries depends of
#'   the chosen model.
#'
#'   \item{Nref}{numeric, sampled value from the prior for the effective
#'   population size of the reference population.}
#'
#'   \item{N1}{numeric, sampled value from the prior for the relative size of
#'   the present-day populations. This is the relative size of the first
#'   population.}
#'
#'   \item{N2}{numeric, sampled value from the prior for the relative size of
#'   the present-day populations. This is the relative size of the second
#'   population.}
#'
#'   \item{N3}{numeric, sampled value from the prior for the relative size of
#'   the present-day populations. This is the relative size of the third
#'   population. This entry only exists when the selected model has four
#'   populations.}
#'
#'   \item{N4}{numeric, sampled value from the prior for the relative size of
#'   the present-day populations. This is the relative size of the fourth
#'   population. This entry only exists when the selected model has four
#'   populations.}
#'
#'   \item{NA1}{numeric, sampled value from the prior for the relative size of
#'   the ancestral populations. This is the relative size of the ancestral
#'   population of N1 and N2. This entry only exists when the selected model has
#'   four populations.}
#'
#'   \item{NA2}{numeric, sampled value from the prior for the relative size of
#'   the ancestral populations. This is the relative size of the ancestral
#'   population of N3 and N4. This entry only exists when the selected model has
#'   four populations.}
#'
#'   \item{Split}{numeric, sampled value from the prior for the time, in 4Nref
#'   scale, of the recent split event.}
#'
#'   \item{Dsplit}{numeric, sampled value from the prior for the time, in 4Nref
#'   scale, of the distance between the two split events.}
#'
#'   \item{PoolError}{numeric, sampled value from the prior for the error
#'   associated with DNA pooling.}
#'
#'   \item{SeqError}{numeric, sampled value from the prior for the error
#'   associated with DNA sequencing.}
#'
#'   \item{mCW1}{numeric, sampled value from the prior for the migration rate
#'   between the two divergent ecotypes inhabiting the first location. This is
#'   the migration rate from ecotype C to ecotype W. For a two population model,
#'   this entry will be called mCW because that model considers a single
#'   location.}
#'
#'   \item{mCW2}{numeric, sampled value from the prior for the migration rate
#'   between the two divergent ecotypes inhabiting the second location. This is
#'   the migration rate from ecotype C to ecotype W. For a two population model,
#'   this entry will not exist.}
#'
#'   \item{mWC1}{numeric, sampled value from the prior for the migration rate
#'   between the two divergent ecotypes inhabiting the first location. This is
#'   the migration rate from ecotype W to ecotype C. For a two population model,
#'   this entry will be called mWC because that model considers a single
#'   location.}
#'
#'   \item{mWC2}{numeric, sampled value from the prior for the migration rate
#'   between the two divergent ecotypes inhabiting the second location. This is
#'   the migration rate from ecotype W to ecotype C. For a two population model,
#'   this entry will not exist.}
#'
#'   \item{mCC}{numeric, sampled value from the prior for the migration rate
#'   between similar ecotypes inhabiting different locations. This is the
#'   migration between the two C ecotypes at two different locations. For a two
#'   population model, this entry will not exist.}
#'
#'   \item{mWW}{numeric, sampled value from the prior for the migration rate
#'   between similar ecotypes inhabiting different locations. This is the
#'   migration between the two W ecotypes at two different locations. For a two
#'   population model, this entry will not exist.}
#'
#'   \item{mAA}{numeric, sampled value from the prior for the migration rate
#'   between the two ancestral populations. For a two population model, this
#'   entry will not exist.}
#'
#'   \item{pM}{numeric, sampled value from the prior for the proportion of the
#'   genome with no barriers against gene flow. This is the proportion of
#'   simulated loci where migration occurs in both directions between the
#'   divergent ecotypes.}
#'
#'   \item{pCW}{numeric, sampled value from the prior for the proportion of the
#'   genome where no migration occurs from the C ecotype towards the W ecotype.
#'   This is the proportion of simulated loci where migration occurs only from W
#'   towards C. This entry does not exist for the two populations model.}
#'
#'   \item{pWC}{numeric, sampled value from the prior for the proportion of the
#'   genome where no migration occurs from the W ecotype towards the C ecotype.
#'   This is the proportion of simulated loci where migration occurs only from C
#'   towards W. This entry does not exist for the two populations model.}
#'
#'   \item{pNO}{numeric, sampled value from the prior for the proportion of the
#'   genome with no gene flow between divergent ecotypes. This is the proportion
#'   of simulated loci where migration does not occur in both directions between
#'   the C and W ecotypes.}
#'
#'   \item{nPoly}{numeric, mean number of polymorphic sites across all simulated
#'   locus.}
#'
#'   \item{nFilter}{numeric, mean number of polymorphic sites retained after
#'   filtering across all simulated locus.}
#'
#'   \item{nLoci}{numeric, total number of loci retained after filtering.
#'   Summary statistics are calculated for these loci.}
#'
#'   \item{Sf}{numeric, fraction of sites fixed between populations. For the
#'   model with two populations, this is a single value. For the four-population
#'   models, this includes three values: the first is the fraction of fixed
#'   sites between the two populations in the first location, the second value
#'   is between the populations in the second location and the third value is
#'   the overall fraction of fixed sites, obtained by comparing each population
#'   against the other three.}
#'
#'   \item{Sx}{numeric, fraction of exclusive sites per population. When running
#'   the model with two populations, this entry has two values - one per
#'   population. For the four-population models, there is also one value per
#'   population, followed by a fifth value representing the fraction of sites
#'   that are segregating in only one of the populations.}
#'
#'   \item{SS}{numeric values representing the fraction of sites shared between
#'   populations. For the model with two populations, this is a single value.
#'   When running one of the four-population models, this entry has three
#'   values. The first is the fraction of shared sites between the two
#'   populations in the first location, the second value is between the
#'   populations in the second location and the third value is the fraction of
#'   shared sites across all four populations.}
#'
#'   \item{Mean_Het}{numeric, expected heterozygosity within each population.
#'   This entry has two values when using a two populations model and four when
#'   running one of the four-populations model.}
#'
#'   \item{SD_Het}{numeric, standard deviation of the expected heterozygosity
#'   for each population. This entry has two values when using a two populations
#'   model and four when running one of the four-populations model.}
#'
#'   \item{Mean_HetBet}{numeric, mean heterozygosity between all pairs of
#'   populations. For the two populations model, this is a single value
#'   representing the heterozygosity between the two populations. For the
#'   four-population models, this entry includes six values. The first value is
#'   the heterozygosity between the first and the second population, the second
#'   value is between the first and the third population, the third value is
#'   between the first and fourth population, the fourth value is between the
#'   second and third populations, the fifth value is between the second and
#'   fourth population and the sixth value is between the third and fourth
#'   populations.}
#'
#'   \item{SD_HetBet}{numeric, standard deviation of the mean heterozygosity
#'   between all pairs of populations. For the two populations model, this is a
#'   single value representing the standard deviation of heterozygosity between
#'   the two populations. When running one of the four-population models, this
#'   entry includes six values. The order of those entries is the same as for
#'   `Mean_HetBet`.}
#'
#'   \item{Mean_FST}{numeric, mean pairwise FST between populations. For the two
#'   populations model, this is a single value representing the mean FST between
#'   the two populations. For the four-population models, this entry includes
#'   six values. The first value is the mean FST between the first and second
#'   populations, the second is between the first and third population, the
#'   third is between the second and third populations, the fourth is between
#'   the first and fourth populations, the fifth value is between the second and
#'   fourth populations and the sixth is between the third and fourth
#'   populations.}
#'
#'   \item{SD_FST}{numeric, standard deviation of the mean pairwise FST between
#'   populations. For the two populations model, this is a single value
#'   representing the standard deviation of the FST between the two populations.
#'   When running one of the four-population models, this entry includes six
#'   values. The order of those entries is the same as for `Mean_FST`.}
#'
#'   \item{FSTQ1}{numeric, it is the 5% quantile of the mean pairwise FST
#'   distribution. For the two populations model, this is a single value
#'   representing the 5% quantile of the FST between the two populations. When
#'   running one of the four-population models, this entry includes six values.
#'   The order of those entries is the same as for `Mean_FST`.}
#'
#'   \item{FSTQ2}{numeric, it is the 95% quantile of the mean pairwise FST
#'   distribution. For the two populations model, this is a single value
#'   representing the 95% quantile of the FST between the two populations. For
#'   the four-population models, this entry includes six values. The order of
#'   those entries is the same as for `Mean_FST`.}
#'
#'   \item{Dstat}{numeric, value of D-statistic for various combinations of
#'   populations. This entry only exists if a four-population model was
#'   selected. It includes three different values. For the first value, P1 was
#'   the W ecotype in the first location P2 was the W ecotype in the second
#'   location and P3 was the C ecotype at the first location. For the second
#'   value P1 was again the W ecotype in the first location but P2 was the C
#'   ecotype in the second ecotype and P3 was the C ecotype at the first
#'   location. For the third value, P1 was also the W ecotype at the first
#'   location, P2 was the C ecotype at the first location and P3 was the W
#'   ecotype at the second location. For all combinations, P4 was assumed to be
#'   an outgroup fixed, at all sites, for the major allele.}
#'
#'   \item{SD_dstat}{numeric, standard deviation of D-statistic for various
#'   combinations of populations. This entry only exists if a four-population
#'   model was selected. Each entry is the standard deviation of the
#'   corresponding D-statistic in the `Dstat` entry.}
#'
#' @examples
#' # simulate Pool-seq data and compute summary statistics for a model with two populations
#' poolSim(model="2pops", nDip=400, nPops=2, nLoci=10, nSites=2000, mutrate=1.5e-8,
#' size=rep(list(rep(5, 20)), 2),mean=c(85, 65), variance=c(1400, 900), minimum=25,
#' maximum=165, min.minor=2, Nref=c(25000, 25000), ratio=c(0.1, 3), pool=c(5, 250),
#' seq=c(0.0001, 0.001), split=c(0, 3), CW=c(1e-13, 1e-3), WC=c(1e-13, 1e-3), bT=c(0, 0.5))
#'
#' # simulate Pool-seq data and compute summary statistics for a model with four populations
#' poolSim(model="Single", nDip=400, nPops=4, nLoci=10, nSites=2000, mutrate=2e-8,
#' size=rep(list(rep(5, 20)), 4), mean=c(85, 65, 65, 70), variance=c(1400, 900, 850, 1000),
#' minimum=25, maximum=165, min.minor=2, Nref=c(25000, 25000), ratio=c(0.1, 3), pool=c(5, 250),
#' seq=c(0.0001, 0.001), split=c(0, 3), CW=c(1e-13, 1e-3), WC=c(1e-13, 1e-3), CC=c(1e-13, 1e-3),
#' WW=c(1e-13, 1e-3), ANC=c(1e-13, 1e-3), bT=c(0, 0.2), bCW=c(0, 0.5), bWC=c(0, 0.5))
#'
#'
#' @export
poolSim <- function(model, nDip, nPops, size, nLoci, nSites, mutrate, mean, variance, minimum, maximum, min.minor = NA,
                    Nref, ratio, split, pool, seq, CW = NA, WC = NA, CC = NA, WW = NA, ANC = NA, bT = NA, bCW = NA,
                    bWC = NA, force = FALSE) {

  # check if the input is correct - when using the noMig, mig2pops, simple2pops or 2pops models, the nPops input should be 2
  if(model == "2pops" &  nPops != 2)
    stop(paste("The selected model should contain only two populations. Please check"))

  # check if the input is correct - the list "size" should contain one different entry per population
  if(inherits(size, "list") & length(size) != nPops)
    stop(paste("The list input - size - should have one entry per population. Please check"))

  # check if the input is correct - the vectors "mean" and "variance" should contain one different entry per population
  if(length(mean) != nPops | length(variance) != nPops)
    stop(paste("The mean and variances inputs should have one entry per population. Please check"))

  # create a dataframe with the parameters
  parameters <- createParams(Nref, ratio, split, pool, seq, CW, WC, CC, WW, ANC, bT, bCW, bWC, model)

  # use the parameters dataframe as input for the poolABC function
  SumStats <- poolStats(parameters, model, nDip, size, nLoci, nSites, mutrate, mean, variance, minimum, maximum, min.minor, force)

  # output the summary statistics and the parameters that led to those summary statistics
  c(parameters, SumStats)
}

