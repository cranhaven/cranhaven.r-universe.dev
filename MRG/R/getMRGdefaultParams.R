getMRGdefaultParams = function(
  mincount = 10, minpos = 4, nlarge = 2,
  plim = 0.85, verbose = FALSE, nclus = 1, clusType = NULL, 
  domEstat = TRUE, outfile = NULL, splitlim = 50000,
  checkDominance = TRUE, checkReliability = FALSE, userfun = NULL, strat = NULL,
  confrules = "individual", suppresslim = 0, plotIntermediate = FALSE, 
  reliabilitySplit = TRUE, remZeroes = TRUE,
  locAdj = "LL") {
  return(list(mincount = mincount, minpos = minpos, nlarge = nlarge,
              plim = plim, verbose = verbose, nclus = nclus, clusType = clusType, 
              domEstat = domEstat, outfile = outfile, splitlim = splitlim,
              checkDominance = checkDominance, checkReliability = checkReliability, userfun = userfun, strat = strat,
              confrules = confrules, suppresslim = suppresslim, plotIntermediate = plotIntermediate, 
              reliabilitySplit = reliabilitySplit, remZeroes = remZeroes,
              locAdj = locAdj))
}