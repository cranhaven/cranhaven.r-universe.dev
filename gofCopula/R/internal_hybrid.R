# Internal function to estimate various tests for one copula at once and 
# derive the hybrid test simultaneously. Called by the gof and gofco function
.gofHybrid <- function(copula, x, tests = c("gofPIOSRn", "gofKernel"), 
                       customTests = NULL, param = 0.5, param.est = TRUE, 
                       df = 4, df.est = TRUE, margins = "ranks", 
                       param.margins = NULL, flip = NULL, M = 1000, MJ = 100, 
                       dispstr = "ex", m = 1, delta.J = 0.5, 
                       nodes.Integration = 12, lower = NULL, upper = NULL, 
                       seed.active = NULL, processes = 1) {
  n <- dim(x)[1]

  # flip the data when required
  if (!is.null(flip)) {
    if (!is.element(flip, c(0,90,180,270))) {
stop(
"flip has to be either of NULL, 0, 90, 180, 270."
)}
    x = .rotateCopula(x = x, flip = flip)
    if (flip == 0) {flip = NULL}
  }

  # derivation of the selected tests with by calling the individual
  # test. Derivation of tests via parametric bootstrap. 
  # Switch to Kendall's Tau if Maximum Likelihood estimation fails
  res_list <- list()
  res_list_err <- c()
  if (!is.null(tests)) {
    res_list <- mapply(function(k, Ms) {
      cat(paste0("Test ", k, " is running"), fill = TRUE)
      a <- tryCatch(doCall(.fcn = k, copula = copula, x = x, margins = NULL, 
                           flip = NULL, M = Ms, param = param, 
                           param.est = param.est, df = df, df.est = df.est, 
                           dispstr = dispstr, MJ = MJ, delta.J = delta.J, 
                           nodes.Integration = nodes.Integration, m = m, 
                           lower = lower, upper = upper, processes = processes, 
                           seed.active = seed.active), 
                    error = function(e) warning(e))
      a
    }, tests, M, SIMPLIFY = FALSE)
    if (any(unlist(lapply(res_list, 
                          function(x) !any(inherits(x, "gofCOP")))))) {
      res_list_err <- which(lapply(res_list, 
                                   function(x) 
                                     !any(inherits(x, "gofCOP"))) == TRUE)
      for (i in res_list_err) {
        res_list[[i]] = list(list(c(NA, NA)))
        names(res_list[[i]][[1]]) = "res.tests"
      }
    }
  }
  
  
  # Adding and derivation of the custom tests, if any
  # Derivation of tests via parametric bootstrap. 
  # Switch to Kendall's Tau if Maximum Likelihood estimation fails
  res_list2 <- list()
  res_list_err2 <- c()
  if (!is.null(customTests)) {
    res_list2 <- mapply(function(k, Ms) {
      cat(paste0("Test ", k, " is running"), fill = TRUE)
      a <- tryCatch(gofCustomTest(copula = copula, x = x, customTest = k, 
                                  margins = NULL, flip = NULL, M = Ms, 
                                  param = param, param.est = param.est, 
                                  df = df, df.est = df.est, dispstr = dispstr, 
                                  lower = lower, upper = upper, 
                                  processes = processes, 
                                  seed.active = seed.active), 
                    error = function(e) warning(e))
      a
    }, customTests, M, SIMPLIFY = FALSE)
    if (any(unlist(lapply(res_list2, 
                          function(x) !any(inherits(x, "gofCOP")))))) {
      res_list_err2 <- which(lapply(res_list2, 
                                    function(x) 
                                      !any(inherits(x, "gofCOP"))) == TRUE)
      for (i in res_list_err2) {
        res_list2[[i]] = list(list(c(NA, NA)))
        names(res_list2[[i]][[1]]) = "res.tests"
      }
    }
  }
  
  # Binding and structuring of results
  res <- do.call(rbind, c(lapply(res_list, function(x) x[[1]]$res.tests), 
                          lapply(res_list2, function(x) x[[1]]$res.tests)))
  resTheta <- do.call(rbind, c(lapply(res_list, function(x) x[[1]]$theta), 
                               lapply(res_list2, function(x) x[[1]]$theta)))
  resDf <- do.call(rbind, c(lapply(res_list, function(x) x[[1]]$df), 
                            lapply(res_list2, function(x) x[[1]]$df)))


  if (length(tests) > 1) {
    which_comb <- list()
    for (i in seq_len(2^NROW(res))) {
      which_comb[[i]] <- which(as.integer(intToBits(i)) == 1)
    }
    comb_exist <- which_comb[which(unlist(lapply(which_comb, length)) > 1)]

    pres <- c()
    for (i in seq_along(comb_exist)) {
      pres <- c(pres, min(length(res[comb_exist[[i]], 1]) * 
                            min(res[comb_exist[[i]], 1]), 1))
    }
    hybrid_comb_names <- paste("hybrid(", lapply(comb_exist, paste, 
                                                 collapse = ", "), ")", 
                               sep = "")
    matrix_names <- matrix(c(pres, rep(NA, length(pres)), 
                             rep(res[1, -c(1, 2)], each = length(pres))), 
                           byrow = FALSE, nrow = length(pres))
    rownames(matrix_names) <- hybrid_comb_names
    res1 <- rbind(res, matrix_names)
  } else {
    res1 <- res
  }
  
  # Assigning of gofCOP structure
  res <- structure(
    class = "gofCOP",
    list(
      list(
        method = sprintf(
    "Parametric bootstrap goodness-of-fit tests with hybrid test and %s copula",
          paste0(copula, flip)
        ),
        copula = paste0(copula, flip),
        margins = margins,
        param.margins = param.margins,
        theta = unique(unname(resTheta)),
        df = unique(unname(resDf)),
        res.tests = res1
      )
    )
  )
  names(res) <- paste0(copula, flip)
  return(res)
}
