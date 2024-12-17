print.desk = function (x, details, digits = 4, ...){

  obj = x
  if (missing(details)) {details = attr(obj, "details")}

  ##########################################
  # GENERAL FUNCTIONS ######################
  ##########################################

  tws = function (x, which = c("both", "left", "right")){
      which = match.arg(which)
      mysub = function(re, x) sub(re, "", x, perl = TRUE)
      if (which == "left")
          return(mysub("^[ \t\r\n]+", x))
      if (which == "right")
          return(mysub("[ \t\r\n]+$", x))
      mysub("[ \t\r\n]+$", mysub("^[ \t\r\n]+", x))
  }

  # Funktion zum Einrahmen eines Strings
  frame.it = function(s, uhc = "",lhc = "=", vc = ""){
    cat(rep(uhc, nchar(s)), "\n", sep = "")
    cat(vc, s,vc,"\n", sep = "")
    cat(rep(lhc, nchar(s)), "\n", sep = "")
  }

  # Funktion zum Unterstreichen eines Strings
  underline.it = function(s){
    cat(s,"\n")
    # cat(rep("\u00af", nchar(as.character(s))), sep = "")
    cat(rep("-", nchar(as.character(s))+1), sep = "")
  }

  # Funktion zur schönen Darstellung von Matrizen
  pretty.mat = function(A, drop0trailing = FALSE, adj.namewd = FALSE){
    if(inherits(A[1],"numeric")){A = round(A, digits)}
    if(is.matrix(A)){
      A = as.data.frame(format(A,
                               width = digits,
                               drop0trailing = drop0trailing,
                               scientific = FALSE,
                               format = "f",
                               trim = TRUE,
                               justify = "right"),
                            stringsAsFactors = FALSE)
      if(adj.namewd){
        name.width = max(sapply(names(A), nchar))
        names(A) = format(names(A), width = name.width, justify = "right")
        format(A, width = name.width, justify = "right")
      }
    }
    return(A)
  }

  # Funktion zur schönen Darstellung von Dataframes
  pretty.df = function(A, drop0trailing = FALSE, adj.namewd = FALSE){
    for (i in 1:dim(A)[2]){
      if (all(is.numeric(A[,i]))){
        A[,i] = round(A[,i], digits)
        A[,i] = format(A[,i],
                       width = digits,
                       drop0trailing = drop0trailing,
                       scientific = FALSE,
                       format = "f",
                       trim = TRUE,
                       justify = "right"
                       )
      }
    }
    if(adj.namewd){
      name.width = max(sapply(names(A), nchar))
      names(A) = format(names(A), width = name.width, justify = "right")
      format(A, width = name.width, justify = "right")
    }
  names(A) = c(sub("^\\s+", "", names(A[1])), names(A[-1]))
  return(A)
  }

  ##########################################
  # RESULTS DEFINITIONS ####################
  ##########################################
  switch(attr(obj, "type"),

         # OLS-REGRESSION (results)
         ols = {
           # Generate regression table
           regtab = matrix(NA, obj$ncoef, 4)
           regtab[,1] = obj$coef
           regtab[,2] = obj$std.err
           regtab[,3] = obj$t.value
           regtab[,4] = obj$p.value
           colnames(regtab) = c("coef", "std.err", "t.value", "p.value")
           rownames(regtab) = names(obj$coef)

           regtab = pretty.mat(regtab)
           regtab[,4] = format.pval(as.numeric(regtab[,4]), eps = 0.0001)
           attr(regtab,"title") = NULL #"Basic regression results:"
           attr(regtab,"rnames") = T
           results = list(regtab)
         },

         # IV-REGRESSION (results)
         ivr = {
           # Generate regression table
           regtab = matrix(NA, obj$ncoef, 4)
           regtab[,1] = obj$coef
           regtab[,2] = obj$std.err
           regtab[,3] = obj$t.value
           regtab[,4] = obj$p.value
           colnames(regtab) = c("coef", "std.err", "t.value", "p.value")
           rownames(regtab) = names(obj$coef)

           regtab = pretty.mat(regtab)
           regtab[,4] = format.pval(as.numeric(regtab[,4]), eps = 0.0001)
           attr(regtab,"title") = NULL #"Basic regression results:"
           attr(regtab,"rnames") = T

           statsA = matrix(NA, 3L, 1L)
           statsA[1] = if(length(obj$instrumented) == 1) obj$instrumented else paste(obj$instrumented, collapse = ", ")
           statsA[2] = if(length(obj$exogenous) == 1) obj$exogenous else paste(obj$exogenous, collapse = ", ")
           statsA[3] = if(length(obj$instruments) == 1) obj$instruments else paste(obj$instruments, collapse = ", ")
           dimnames(statsA) = list(c("Endogenous regressors:",
                                     "Exogenous regressors:",
                                     "Instruments:")
                                    , " " )
           statsA = pretty.mat(statsA, drop0trailing = TRUE)
           attr(statsA,"title") = NULL
           attr(statsA,"rnames") = T

           results = list(regtab, statsA)
         },

         # HILU-REGRESSION (results)
         hilu = {
           regtab = pretty.mat(obj$results)
           regtab[,4] = format.pval(as.numeric(regtab[,4]), eps = 0.0001)
           attr(regtab,"title") = "Minimal SSR Regression results:"
           attr(regtab,"rnames") = T
           more = matrix(NA, 3, 1)
           more[1] = obj$nregs
           more[2] = obj$idx.opt
           more[3] = obj$rho.opt
           dimnames(more) = list(c("Total number of regressions:",
                                    "Regression that minimizes SSR:",
                                    "Rho value that minimizes SSR:"
           )
           , " " )
           more = pretty.mat(more, drop0trailing = TRUE)
           attr(more,"title") = NULL # "Further statistics:"
           attr(more,"rnames") = T
           results = list(regtab, more)
         },

         # COCHORC-REGRESSION (results)
         cochorc = {
           regtab = pretty.mat(obj$results)
           regtab[,4] = format.pval(as.numeric(regtab[,4]), eps = 0.0001)
           attr(regtab,"title") = NULL
           attr(regtab,"rnames") = T
           stats = matrix(NA, 2, 1)
           stats[1] = obj$niter
           stats[2] = obj$rho.opt
           dimnames(stats) = list(c("Number of iterations performed:",
                                    "Final rho value:"
                                    )
                                  , " " )
           stats = pretty.mat(stats, drop0trailing = TRUE)
           attr(stats,"title") = NULL
           attr(stats,"rnames") = T
           results = list(regtab, stats)
         },

         # Predictions (results)
         pred = {
           predmat = data.frame(xnew = matrix(obj$xnew, ncol = obj$mod$ncoef - sum(ols.has.const(obj$mod))), pred.val = obj$pred.val)
           predmat = pretty.mat(as.matrix(predmat))
           attr(predmat,"title") = "Exogenous variable values and their corresponding predictions:"
           attr(predmat,"rnames") = T
           results = list(predmat)
         },

         # INTERVALS (results)
         int = {
           intmat = pretty.mat(as.matrix(obj$results))
           switch(attr(obj, "c.type"),
                  ci.beta = {
                    attr(intmat,"title") = "Interval estimator of model parameter(s):"
                  },
                  ci.y = {
                    attr(intmat,"title") = "Interval estimator of unknown, true y-value(s):"
                  },
                  pi = {
                    attr(intmat,"title") = "Interval estimator for predicted y-value(s):"
                  },
                  ai = {
                    attr(intmat,"title") = "Acceptance interval for model parameter(s):"
                  }
           )
           attr(intmat,"rnames") = T
           results = list(intmat)
         },

         # HYPOTHESIS TESTS (results)
         htest = {
           if(!is.null(obj$hyp)){
              hypo = as.data.frame(obj$hyp)
              if(dim(hypo)[1] == 1) rownames(hypo) = " "
              attr(hypo,"title") = "Hypotheses:"
            } else {hypo = NULL}

              testtab = pretty.df(obj$results)
              if(attr(obj, "test.type") == "qlrtest"){pval.col = 4} else {pval.col = 3}
              testtab[,pval.col] = format.pval(as.numeric(testtab[,pval.col]), eps = 0.0001)
              attr(testtab,"title") = "Test results:"

           if(attr(obj, "test.type") == "qlrtest"){
              stats = matrix(NA, 3L, 1L)
              stats[1] = length(obj$periods)
              stats[2] = obj$breakpoint
              stats[3] = obj$lambda
              dimnames(stats) = list(c("Number of periods considered:",
                                       "Period of break:",
                                       "Lambda value:"),
                                     " ")
              stats = pretty.mat(stats, drop0trailing = TRUE)
              stats = as.data.frame(stats)
              attr(stats,"title") = NULL
              attr(stats,"rnames") = T
            } else {stats = NULL}

              results = list(hypo, testtab, stats)
         },

         # AR(1) SIMULATION (RESULTS)
         ar1sim = {
           u.sim = pretty.mat(as.matrix(obj$u.sim))
           names(u.sim) = "u.sim"
           #u.sim = as.data.frame(u.sim[1:5,],u.sim[28:30,])
           attr(u.sim,"title") = "Simulated error terms u_t:"
           results = list(u.sim)
         },

         # REP.SAMPLE (RESULTS)
         repsamp = {
           #u.sim = pretty.mat(as.matrix(obj$u.sim))
           #names(u.sim) = "u.sim"
           #u.sim = as.data.frame(u.sim[1:5,],u.sim[28:30,])
           #attr(u.sim,"title") = "Simulated error terms u_t:"
           results = obj
         },

         # Box Cox REGRESSION (results)
         bcmodel = {
           regtab = matrix(NA, obj$results$ncoef, 4)
           regtab[,1] = obj$results$coef
           regtab[,2] = obj$results$std.err
           regtab[,3] = obj$results$t.value
           regtab[,4] = obj$results$p.value
           colnames(regtab) = c("coef", "std.err", "t.value", "p.value")
           rownames(regtab) = names(obj$results$coef)
           regtab = pretty.mat(regtab)
           regtab[,4] = format.pval(as.numeric(regtab[,4]), eps = 0.0001)
           attr(regtab,"title") = "Model exhibiting minimal SSR:"
           attr(regtab,"rnames") = T

           stats = matrix(NA, 5, 1)
           stats[1] = obj$nregs
           stats[2] = obj$idx.opt
           stats[3] = obj$val.opt
           stats[4] = as.numeric(obj$lambda[1])
           stats[5] = as.numeric(obj$lambda[2])
           dimnames(stats) = list(c("Total number of regressions:",
                                    "Regression that minimizes SSR:",
                                    "Minimal SSR value:",
                                    "Lambda (y):",
                                    "Lambda (x):")
           , " " )
           stats = pretty.mat(stats, drop0trailing = TRUE)
           attr(stats,"title") = NULL # "Further statistics:"
           results = list(regtab)
           dtls = list(stats)
         }
  )

  ##########################################
  # DETAILS DEFINITIONS ####################
  ##########################################

  if (( attr(obj, "details") & details ) | (!attr(obj, "details") & details)) {
    switch(attr(obj, "type"),

        # OLS-ESTIMATION (details)
        ols = {
          stats = matrix(NA, 9L, 1L)
          stats[1] = obj$nobs
          stats[2] = obj$ncoef
          stats[3] = obj$df
          stats[4] = obj$r.squ
          stats[5] = obj$adj.r.squ
          stats[6] = obj$ssr
          stats[7] = obj$sig.squ
          if(!is.null(obj$f.val)){
            stats[8] = obj$f.value
            stats[9] = obj$f.pvalue
          }
          dimnames(stats) = list(c("Number of observations:", "Number of coefficients", "Degrees of freedom:",
                             "R-squ.:", "Adj. R-squ.:", "Sum of squ. resid.:",
                             "Sig.-squ. (est.):", "F-Test (F-value):", "F-Test (p-value):"),
                             " ")
          stats = pretty.mat(stats, drop0trailing = TRUE)
          attr(stats,"title") = NULL # "Further statistics:"
          stats2 = matrix(NA, 2L, 1L)
          stats2[1] = obj$modform
          stats2[2] = if(is.null(obj$data.name)) "Not specified!" else obj$data.name
          dimnames(stats2) = list(c("Model formula:", "Used dataset:"), "")
          vcov = pretty.mat(obj$vcov)
          attr(vcov,"title") = "(Variance-) Covariance Matrix:"
          dtls = list(stats
                      #stats2, vcov
                      )
          },

        # IV-ESTIMATION (details)
        ivr = {
          stats = matrix(NA, 7L, 1L)
          stats[1] = obj$nobs
          stats[2] = obj$ncoef
          stats[3] = obj$df
          stats[4] = obj$r.squ
          stats[5] = obj$adj.r.squ
          stats[6] = obj$ssr
          stats[7] = obj$sig.squ
          dimnames(stats) = list(c("Number of observations:", "Number of coefficients", "Degrees of freedom:",
                                   "R-squ.:", "Adj. R-squ.:", "Sum of squ. resid.:", "Sig.-squ. (est.):"),
                                 " ")
          stats = pretty.mat(stats, drop0trailing = TRUE)
          attr(stats,"title") = NULL # "Further statistics:"

          fsa =obj$fsd
          fsa = pretty.mat(fsa, drop0trailing = TRUE)
          attr(fsa,"title") = "Weak instruments (H0) test:"

          etest = matrix(NA, 2L, 1L)
          etest[1] = obj$f.hausman
          etest[2] = obj$p.hausman
          dimnames(etest) = list(c("Wu-Hausman test (F-value):",
                                 "Wu-Hausman test (p-value):"),
                               " ")
          etest = pretty.mat(etest, drop0trailing = TRUE)
          attr(etest,"title") = "Exogeneity (H0) test:"

          dtls = list(
            #stats,
            fsa, etest)
        },

        # HILDRETH-LU ESTIMATION (details)
        hilu = {
          stats = matrix(NA, 1, 1)
          stats[1] = obj$nregs
          dimnames(stats) = list("Number of regressions performed: ", " " )
          stats = pretty.mat(stats, drop0trailing = TRUE)
          all.regs = pretty.mat(as.matrix(obj$all.regs), adj.namewd = FALSE)
          attr(all.regs,"title") = "Regression results over all rho values:"
          dtls = list(stats, all.regs)
          },

        # COCHRANE-ORCUTT ESTIMATION (details)
        cochorc = {
          all.regs = pretty.mat(obj$all.regs)
          attr(all.regs,"title") = "Iterated regression results:"
          trans = pretty.mat(cbind(obj$y.trans, obj$X.trans))
          attr(trans,"title") = "Transformed variables at final iteration:"
          dtls = list(all.regs, trans)
        },

        # PREDICTIONS (details)
        pred = {
          stats = as.matrix(data.frame(var.pe = obj$var.pe, sig.squ = obj$sig.squ, smpl.err = obj$smpl.err))
          stats = pretty.mat(stats, drop0trailing = TRUE)
          attr(stats,"title") = NULL #"Estimated variances of prediction error, of error term and the sampling error:"
          dtls = list(stats)
        },

        # INTERVAL ESTIMATION (details)
        int = {
          se = pretty.mat(as.matrix(obj$std.err), drop0trailing = TRUE)
          colnames(se) = " "
          attr(se,"title") = "Estimated standard deviations:"
          dtls = list(se)
        },

        # HYPOTHESIS TEST (details)
        htest = {
          switch(attr(obj, "test.type"),
                 ttest = {# t.test.coef
                   nd = pretty.df(data.frame(
                       type = obj$nulldist["type"],
                       df = obj$nulldist["df"], row.names = " "))
                   attr(nd,"title") = "Null distribution:"
                   se = pretty.mat(obj$std.err)
                   colnames(se) = " "
                   rownames(se) = obj$lcomb
                   attr(se,"title") = "Estimated standard deviation of linear combination:"
                   dtls = list(nd, se)
                 },
                 ftest = {# f.test.coef
                   nd = pretty.df(data.frame(
                     type = obj$nulldist["type"],
                     df1 = obj$nulldist[[2]][1],
                     df2 = obj$nulldist[[2]][2],
                     row.names = " "))
                   attr(nd,"title") = "Null distribution:"
                   ssr = pretty.df(data.frame(
                     SSR.H0 = obj$SSR.H0,
                     SSR.H1 = obj$SSR.H1,
                     row.names = " "))
                   attr(ssr,"title") = "SSR of Null- and regular model:"
                   dtls = list(nd, ssr)
                 },
                 gqtest = {# gq.test
                   hreg1 = pretty.mat(obj$hreg1)
                   attr(hreg1,"title") = "Group I regression results:"
                   misc1 = pretty.mat(obj$stats1, drop0trailing = TRUE)
                   attr(misc1,"title") = NULL
                   #
                   hreg2 = pretty.mat(obj$hreg2)
                   attr(hreg2,"title") = "Group II regression results:"
                   misc2 = pretty.mat(obj$stats2, drop0trailing = TRUE)
                   attr(misc2,"title") = NULL
                   dtls = list(hreg1, misc1, hreg2, misc2)
                 },
                 bctest = {# bc.test
                   stats = pretty.mat(obj$stats, drop0trailing = TRUE)
                   attr(stats,"title") = "SSRs of compared models:"
                   dtls = list(stats)
                 },
                 bptest = {# bp.test
                   hreg = pretty.mat(obj$hreg)
                   attr(hreg,"title") = "Auxiliary regression results:"
                   stats = pretty.mat(obj$stats, drop0trailing = TRUE)
                   attr(stats,"title") = NULL
                   dtls = list(hreg, stats)
                 },
                 whtest = {# wh.test
                   hreg = pretty.mat(obj$hreg)
                   attr(hreg,"title") = "Auxiliary regression results:"
                   stats = pretty.mat(obj$stats, drop0trailing = TRUE)
                   attr(stats,"title") = NULL
                   dtls = list(hreg, stats)
                 },
                 qlrtest = {# qlr.test
                   fstats = pretty.mat(obj$f.stats)
                   attr(fstats,"title") = "F-Statistics over all periods:"
                   cvint = pretty.mat(obj$lf.crit)
                   attr(cvint,"title") = "Interval of critical F-values:"
                   dtls = list(fstats, cvint)
                 },
                 pctest = {# pc.test
                   nd = pretty.df(data.frame(
                     type = obj$nulldist["type"],
                     df1 = obj$nulldist[[2]][1],
                     df2 = obj$nulldist[[2]][2],
                     row.names = " "))
                   attr(nd,"title") = "Null distribution:"

                   stats = matrix(NA, 4L, 1L)
                   stats[1] = obj$periods1
                   stats[2] = obj$periods.total
                   stats[3] = obj$SSR1
                   stats[4] = obj$SSR
                   dimnames(stats) = list(c("Periods phase 1:",
                                            "Periods total:",
                                            "SSR phase 1:",
                                            "SSR total:"),
                                          " ")
                   stats = pretty.mat(stats, drop0trailing = TRUE)
                   attr(stats,"title") = NULL
                   dtls = list(nd, stats)
                 },
                 jbtest = {# jb.test
                   nd = pretty.df(data.frame(
                     type = obj$nulldist["type"],
                     df = obj$nulldist[[2]],
                     row.names = " "))
                   attr(nd,"title") = "Null distribution:"

                   stats = matrix(NA, 3L, 1L)
                   stats[1] = obj$nobs
                   stats[2] = obj$skew
                   stats[3] = obj$kur
                   dimnames(stats) = list(c("Number of observations:",
                                            "Skewness:",
                                            "Kurtosis:"),
                                          " ")
                   stats = pretty.mat(stats, drop0trailing = TRUE)
                   attr(stats,"title") = NULL
                   dtls = list(nd, stats)
                 },

                 resettest = {# reset.test
                   nd = pretty.df(data.frame(
                     type = obj$nulldist["type"],
                     df1 = obj$nulldist[[2]][1],
                     df2 = obj$nulldist[[2]][2],
                     row.names = " "))
                   attr(nd,"title") = "Null distribution:"

                   stats = matrix(NA, 3L, 1L)
                   stats[1] = obj$L
                   stats[2] = obj$SSR0
                   stats[3] = obj$SSR1
                   dimnames(stats) = list(c("No. of param. tested (L):",
                                            "SSR H0-model:",
                                            "SSR H1-model:"),
                                          " ")
                   stats = pretty.mat(stats, drop0trailing = TRUE)
                   attr(stats,"title") = NULL
                   dtls = list(nd, stats)
                 }
          )# end of switch
        },

        # AR(1) SIMULATION (details)
        ar1sim = {
          stats = matrix(NA, 2L, 1L)
          stats[1] = obj$rho
          stats[2] = obj$n
          dimnames(stats) = list(c("True rho value:",
                                   "Number of periods:"),
                                 " ")
          stats = pretty.mat(stats, drop0trailing = TRUE)
          attr(stats,"title") = NULL
          dtls = list(stats)
        }
      )# end of switch
  } else {
    dtls = NULL
  } # end of details definitions

  ##########################################
  # START PRINTING #########################
  ##########################################

  # Print title
  cat("\n")
  if (!is.null(attr(obj, "title"))) {
    underline.it(attr(obj, "title"))
    cat("\n")
  }

  # Print results
  for (i in 1:length(results)){
    cat("\n")
    if (!is.null(attr(results[[i]],"title"))){
      cat(attr(results[[i]],"title"), "\n")
    }

    if(is.null(attr(results[[i]],"rnames"))){is.rnames = FALSE} else {is.rnames = TRUE}
    #if(!is.null(attributes(results[[i]]))) {
    #  if(names(results[[i]][1]) == "H0:") {is.rightheader = FALSE} else {is.rightheader = TRUE}
    #}

    if(!is.null(results[[i]])){
      print(results[[i]], row.names = is.rnames, print.gap = 2, quote = FALSE, right = TRUE)
    }

  }

  # Print details
  if (!is.null(dtls)) {
    for (i in 1:length(dtls)){
      cat("\n")
      if(!is.null(attr(dtls[[i]],"title"))){
        cat(attr(dtls[[i]],"title"), "\n")
      }
      print(dtls[[i]], row.names = TRUE, print.gap = 2, quote = FALSE)
    }
  }

}
