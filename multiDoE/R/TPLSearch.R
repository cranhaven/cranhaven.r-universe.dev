#' @importFrom stats runif
TPLSearch <- function(facts, units, criteria, model, ...) {

  varargin <- list(...)[[1]]

  tplsearch <- list()     # contiene ar, stats
  tplsearch$ar <- list()
  tplsearch$stats <- 0

  nStrat <- length(facts)
  nCrit <- length(criteria)

  # default parameters
  restarts <- 1000
  levels <- 3
  etas <- matrix(1, 1, nStrat - 1)
  rngSeed <- NA
  restInit <- 50
  # interact <- 0

  # optional parameters
  if (length(varargin) != 0) {
    for (i in seq(1, length(varargin), 2)) {
      switch(varargin[[i]],
             "Restarts" = {
               restarts = varargin[[i+1]]
             },
             "Levels" = {
               levels = varargin[[i+1]]
             },
             "Etas" = {
               etas = varargin[[i+1]]
             },
             "RngSeed" = {
                rngSeed = varargin[[i+1]]
             },
             "RestInit" = {
               restInit = varargin[[i+1]]
             }
             # "Interactive" = {
             #   interact = varargin[[i+1]]
             # }
      )
    }
  }

  scalarizations <- restarts - (restInit * nCrit)
  mso <- MSOpt(facts, units, levels, etas, criteria, model)
  if (!is.na(rngSeed)){
    set.seed(rngSeed)
  }


  # good quality solutions for the single objectives
  initSol <- vector(mode = "list", length = nCrit)    # lista di lunghezza 6 con 6 soluzioni iniziali, [[i]] matrice
  initScores <- matrix(0, nCrit, nCrit)               # matrice 6 x 6
  totFEval <- 0                                       # numero

  for (i in 1:nCrit) {
    a <- t(c(numeric(i - 1), 1, numeric(nCrit - i)))   # alpha
    mssearch <- MSSearch(mso, a, "Restarts", restInit)

    initSol[[i]] <- mssearch$optsol
    initScores[i, ] <- mssearch$optsc$score
    totFEval <- totFEval + mssearch$feval
  }

  ar <- Archive(nCrit, scalarizations + nCrit)

  for (i in 1:nCrit) {
    ar <- Add(ar, initSol[[i]], initScores[i, ])
  }

  pf <- PFront(ar)

  if (length(tplsearch) > 1) {
    stats <- totFEval
  }

  # if (interact) {
  #   if (nCrit == 2){
  #     plot(pf$arch$scores[1:pf$arch$nsols, 1], pf$arch$scores[1:pf$arch$nsols, 2],
  #          col = "blue")
  #     points(pf$arch$scores[pf$ptrs, 1], pf$arch$scores[pf$ptrs, 2],
  #            col = "red")
  #     # Sys.sleep(Inf)
  #   }
  #   else if (nCrit == 3) {
  #     g1 <- scatterplot3d(pf$arch$scores[1:pf$arch$nsols, 1], pf$arch$scores[1:pf$arch$nsols, 2],
  #                   pf$arch$scores[1:pf$arch$nsols, 3], color = "blue")
  #     g1
  #     g1$points3d(pf$arch$scores[pf$ptrs, 1], pf$arch$scores[pf$ptrs, 2],
  #             pf$arch$scores[pf$ptrs, 3])
  #     #Sys.sleep(Inf)
  #   }
  # }

  for (i in 1:scalarizations) {

    norms <- c(pf$scmin, pf$scmax - pf$scmin)
    r <- matrix(runif(nCrit - 1), 1, nCrit - 1)
    alpha <- cbind(r, 1 - sum(r))
    start <- pf$arch$solutions[pf$ptrs[sample(1:length(pf$ptrs), 1)]]

    newMssearch <- MSSearch(mso, alpha, "Start", start[[1]], "Normalize", norms)
    newSol <- newMssearch$optsol
    newScore <- newMssearch$optsc$score
    newFeval <- newMssearch$feval

    pf$arch <- Add(pf$arch, newSol, newScore)
    pf <- Add_PF(pf, pf$arch$nsols)

    # if (interact) {
    #   if (nCrit == 2){
    #     points(pf$arch$scores[1:pf$arch$nsols, 1], pf$arch$scores[1:pf$arch$nsols, 2], col = "blue")
    #     points(pf$arch$scores[pf$ptrs, 1], pf$arch$scores[pf$ptrs, 2], col = "red")
    #     #Sys.sleep(Inf)
    #   }
    #   else if (nCrit == 3) {
    #     g <- scatterplot3d(pf$arch$scores[1:pf$arch$nsols, 1], pf$arch$scores[1:pf$arch$nsols, 2],
    #                   pf$arch$scores[1:pf$arch$nsols, 3], color = "blue")
    #     g
    #     g$points3d(pf$arch$scores[pf$ptrs, 1], pf$arch$scores[pf$ptrs, 2],
    #             pf$arch$scores[pf$ptrs, 3])
    #     #Sys.sleep(Inf)
    #   }
    # }

    if (length(tplsearch) > 1) {
      stats <- c(stats, newFeval + stats[which.max(stats)])
    }
  }

  tplsearch$ar <- pf$arch
  tplsearch$stats <- stats
  return(tplsearch)
}
