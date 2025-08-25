#' Model benchmark compare
#'
#' Compare the run time and output of various code chunks
#'
#' @param ... Functions to run
#' @param times Number of times to run
#' @param input Object to be passed as input to each function
#' @param inputi Function to be called with the replicate number then passed to each function.
#' @param evaluator An expression that the ... expressions will be passed as "." for evaluation.
#' @param post Function or expression (using ".") to post-process results.
#' @param target Values the functions are expected to (approximately) return.
#' @param targetin Values that will be given to the result of the run to produce output.
#' @param metric c("rmse", "t", "mis90", "sr27") Metric used to compare output values to target.
#' mis90 is the mean interval score for 90\% confidence, see Gneiting and Raftery (2007).
#' sr27 is the scoring rule given in Equation 27 of Gneiting and Raftery (2007).
#' @param paired Should the results be paired for comparison?
#' @param kfold First element should be the number of elements that are being
#' split into groups. If the number of folds is different from `times`, then
#' the second argument is the number of folds.
#' Use `ki` in `inputi` and `targeti` to select elements in the current fold.
#' @importFrom stats median predict t.test sd
#' @references Gneiting, T., & Raftery, A. E. (2007). Strictly proper scoring rules, prediction, and estimation. Journal of the American Statistical Association, 102(477), 359-378.
#'
#' @return Data frame of comparison results
#' @export
#'
#' @examples
#' # Compare distribution of mean for different sample sizes
#' mbc(mean(rnorm(1e2)),
#'     mean(rnorm(1e4)),
#'     times=20)
#'
#' # Compare mean and median on same data
#' mbc(mean(x),
#'     median(x),
#'     inputi={x=rexp(1e2)})
#'
#' # input given, no post
#' mbc({Sys.sleep(rexp(1, 30));mean(x)},
#'      {Sys.sleep(rexp(1, 5));median(x)},
#'     inputi={x=runif(100)})
#'
#' # input given with post
#' mbc(mean={Sys.sleep(rexp(1, 30));mean(x)},
#'     med={Sys.sleep(rexp(1, 5));median(x)},
#'     inputi={x=runif(100)},
#'     post=function(x){c(x+1, x^2)})
#'
#' # input given with post, 30 times
#' mbc(mean={Sys.sleep(rexp(1, 30));mean(x)+runif(1)},
#'     med={Sys.sleep(rexp(1, 50));median(x)+runif(1)},
#'     inputi={x=runif(100)},
#'     post=function(x){c(x+1, x^2)}, times=10)
#'
#' # Name one function and post
#' mbc({mean(x)+runif(1)},
#'     a1={median(x)+runif(1)},
#'     inputi={x=runif(100)},
#'     post=function(x){c(rr=x+1, gg=x^2)}, times=10)
#'
#' # No input
#' m1 <- mbc(mean={x <- runif(100);Sys.sleep(rexp(1, 30));mean(x)},
#'           med={x <- runif(100);Sys.sleep(rexp(1, 50));median(x)})
mbc <- function(..., times=5, input, inputi, evaluator, post, target, targetin,
                metric="rmse", paired, kfold) {
  if (!missing(input) && !missing(inputi)) {
    stop("input and inputi should not both be given in")
  }
  # dots are the functiosn to run, n is the number of functions
  # dots <- list(...)
  dots <- as.list(match.call(expand.dots = FALSE)$`...`)
  n <- length(dots)
  gcFirst <- FALSE # arg passed to system.time, slows down by .07 sec per evaluation

  fnames <- names(dots)
  if (is.null(fnames)) {fnames <- rep("", n)}
  fnoname <- which(fnames == "")
  # if (length(fnoname) > 0) {fnames[fnoname] <- paste0("f", fnoname)}
  if (length(fnoname) > 0) {fnames[fnoname] <- unlist(lapply(dots, function(ss) {paste0(trimws(deparse(ss)), collapse=' ')}))[fnoname]}
  while(anyDuplicated(fnames)) {
    dup <- duplicated(fnames)
    itemp <- which(dup)[1]
    fnames[itemp] <- paste0(fnames[itemp], itemp)
  }
  if (length(fnames) == 1) {fnames <- list(fnames)}

  if (!missing(kfold)) {
    if (length(kfold) == 1) {
      folds <- times
      kfoldN <- kfold
    } else if (length(kfold) == 2) {
      kfoldN <- kfold[1]
      folds <- kfold[2]
    } else {
      stop("kfold must be # of indices and use times for # of folds or c(# of indices, # of folds #923052")
    }
    if (!is.numeric(folds) || (folds != as.integer(folds))) {stop("kfold must be integers #52027")}
    if (!is.numeric(kfoldN) || (kfoldN != as.integer(kfoldN))) {stop("kfold must be integers #52028")}
  }

  # Create objects to hold output data
  runtimes <- matrix(NA, n, times)
  dimnames(runtimes)[[1]] <- fnames
  outs <- rep(list(rep(list(NA),times)), n)
  # if (!missing(post)) {postout <- array(data = NA, dim = c(n, times, post_length))} #rep(list(rep(list(NA),times)), n)}

  # Create a progress bar to show progress
  pb <- progress::progress_bar$new(total=times*n)
  pb$tick(0)

  # Loop over each replicate
  for (j in 1:times) {
    # Update kfold if finished set
    if (!missing(kfold) && (((j-1) %% folds) == 0)) {
      ki_all <- sample(1:kfoldN)
    }
    if (!missing(kfold)) {
      ki <- ki_all[(1:kfoldN %% folds) != (j-1)%%folds]
    }

    # Get input for replicate if inputi given
    if (!missing(inputi)) {
      if (missing(paired)) {paired <- TRUE} # Same inputs so pair them

      # Try to parse inputi, first check for expression.
      # If first char is "{", then it is
      inputi_expr <- match.call(expand.dots = FALSE)$`inputi`
      if (substr(as.character(inputi_expr[1]),1,1) == "{") {
        # Create new environment to evaluate expression
        input <- new.env(parent = parent.frame())
        # Add ki to env if using kfold
        if (!missing(kfold)) {
          # input$ki <- ki_all[(1:kfoldN %% folds) != (j-1)%%folds]
          input$ki <- ki
        }
        # Evaluate expression
        inputi_expr_out <- eval(inputi_expr, input)
        # If it was single value instead of setting variable, e.g. "rnorm(10)"
        #   instead of "x <- rnorm(10)", then store that
        if (length(ls(input)) == 0) { # Store the output in input
          # input$inputi_expr_out <- inputi_expr_out
          input$x <- inputi_expr_out
        }
      } else if (is.function(inputi)) { # Next see if it is a function
        input <- inputi(j)
      } else if (is.list(inputi)) { # Or a list
        input <- inputi[[j]]
      } else { # Otherwise it is an expression, so do same as above.
        # stop("inputi not recognized")
        input <- new.env(parent = parent.frame())
        inputi_expr_out <- eval(inputi_expr, input)
        if (length(ls(input)) == 0) { # Store the output in input
          # input$inputi_expr_out <- inputi_expr_out
          input$x <- inputi_expr_out
        }
      }
    } else { # inputi not given, so not paired
      if (missing(paired)) {paired <- FALSE} # Inputs not paired so don't pair unless told to
    }

    # Make input a list/env, this will cause trouble
    if (!missing(input) && !is.list(input) && !is.environment(input)) {
      input <- list(x=input)
    }

    # Loop over each function
    for (i in 1:n) {

      # See if there is input to each
      if (!missing(input)) { # Single input for all
        inputenv <- input
      } else { # No input at all
        if (missing(evaluator)) {
          inputenv <- new.env(parent = parent.frame()) # Only ki and parent.frame
        } else { # Use evaluator, dots are input to evaluator to be evaluated
          # This time there is no input, so create it
          inputenv <- new.env()
        }
        if (!missing(kfold)) { # If kfold, set ki
          inputenv$ki <- ki
        }
      }

      # Run it here
      if (gcFirst) {gc(F)}
      start_time <- Sys.time()
      if (missing(evaluator)) {
        out <- eval(dots[[i]], envir=inputenv)
      } else {
        expr_evaluator <- match.call(expand.dots = FALSE)$`evaluator`
        inputenv$. <- eval(dots[[i]])
        out <- eval(expr_evaluator, envir=inputenv)
      }
      end_time <- Sys.time()
      runtime <- as.numeric(end_time - start_time, units="secs")

      # Save runtime and out
      runtimes[i, j] <- runtime #['elapsed']
      outs[[i]][[j]] <- out

      # Run post-processing if post or target given
      if (!missing(post) || !missing(target)) {
        # Run post if given
        if (!missing(post)) {
          # po <- post(out) # Used to require it to be function, just evaluate.
          # Evaluate post as expression, then as function if still function.
          post_env <- new.env(parent = parent.frame())
          post_env$. <- out
          if (!missing(kfold)) { # Add ki if using kfold
            post_env$ki <- input$ki
          }
          post_expr <- match.call(expand.dots = FALSE)$`post`
          po <- eval(post_expr, post_env)
          if (is.function(po)) {
            po <- po(out)
          }
        } else {
          po <- out
        }
        if (!missing(targetin)) { # If targetin in given, use predict function
          # If k-fold, run it as expression with ki first
          if (!missing(kfold)) {
            # Create new environment to evaluate expression
            tar_env <- new.env(parent = parent.frame())
            # Add ki to env
            if (!missing(kfold)) {
              # tar_env$ki <- input$ki #ki_all[(1:kfoldN %% times) == j-1]
              tar_env$ki <- ki
            }
            # Evaluate expression
            targeti_expr <- match.call(expand.dots = FALSE)$`targetin`

            # inputi_expr_out <- eval(inputi_expr, input)
            targetin <- eval(targeti_expr, tar_env)
          }
          # if (any(exists(paste0("predict.",class(po))))) {
          if (is.function(targetin)) {
            targetinj <- targetin(j)
          } else if (is.list(targetin) && !is.data.frame(targetin)) {
            targetinj <- targetin[[j]]
          } else {
            targetinj <- targetin
          }
          po <- predict(po, targetinj, se.fit=any(c("t","mis90","sr27") %in% metric))
          # }
        } else {
          targetinj <- NULL
        }
        # Run
        if (!missing(target)) {
          po.metric <- c()
          if ("rmse" %in% metric) {
            if (is.function(po)) {stop("Can't calculate rmse for function output")}
            targetj <- if (is.function(target)) {target(j)}
            else if (is.list(target)) {target[[j]]}
            else if (is.character(target) && !is.character(po) && (target%in%names(targetinj))) {targetinj[[target]]}
            else if (is.character(target) && !is.character(po)) {input[[target]]}
            else {target}
            po.mean <- if ("fit" %in% names(po)) po$fit else if ("mean" %in% names(po)) po$mean else {po}
            po.metric <- c(po.metric, rmse=sqrt(mean((po.mean - targetj)^2)))
          }
          if ("t" %in% metric) {
            targetj <- if (is.function(target)) {target(j)}
            else if (is.list(target)) {target[[j]]}
            else if (is.character(target) && !is.character(po) && (target%in%names(targetinj))) {targetinj[[target]]}
            else if (is.character(target) && !is.character(po)) {input[[target]]}
            else {target}
            po.mean <- if ("fit" %in% names(po)) po$fit else if ("mean" %in% names(po)) po$mean else {stop("Can't get fit/mean from post/out")}
            po.se <- po$se
            po.t <- (po.mean - targetj) / po.se
            po.tsum <- summary(po.t) #c(mean=mean(po.t)) # rmse=sqrt(mean((po - targetj)^2)))
            names(po.tsum) <- paste0("", names(po.tsum), " t")
            po.metric <- c(po.metric, po.tsum)
          }
          if ("mis90" %in% metric) {
            targetj <- if (is.function(target)) {target(j)}
            else if (is.list(target)) {target[[j]]}
            else if (is.character(target) && !is.character(po) && (target%in%names(targetinj))) {targetinj[[target]]}
            else if (is.character(target) && !is.character(po)) {input[[target]]}
            else {target}
            po.mean <- if ("fit" %in% names(po)) po$fit else if ("mean" %in% names(po)) po$mean else {stop("Can't get fit/mean from post/out")}
            po.se <- po$se
            po.mis <- 3.28 * po.se + 20 * pmax(0, po.mean - targetj - 1.64 * po.se) + 20 * pmax(0, -po.mean + targetj - 1.64 * po.se)
            # po.t <- (po$mean - targetj) / po$se
            po.mismean <- c(mis90=mean(po.mis)) # summary(po.t) #c(mean=mean(po.t)) # rmse=sqrt(mean((po - targetj)^2)))
            # names(po) <- paste0("", names(po), " mis90")
            po.metric <- c(po.metric, po.mismean)
          }
          if ("sr27" %in% metric) {
            targetj <- if (is.function(target)) {target(j)}
            else if (is.list(target)) {target[[j]]}
            else if (is.character(target) && !is.character(po) && (target%in%names(targetinj))) {targetinj[[target]]}
            else if (is.character(target) && !is.character(po)) {input[[target]]}
            else {target}
            po.mean <- if ("fit" %in% names(po)) po$fit else if ("mean" %in% names(po)) po$mean else {stop("Can't get fit/mean from post/out")}
            po.se <- po$se
            po.sr <- -((po.mean-targetj)/po.se)^2 - 2 * log(po.se)
            po.srmean <- c(sr27=mean(po.sr))
            po.metric <- c(po.metric, po.srmean)
          }
          #else {stop("Only metric recognized are rmse and t and mis90")}
          po <- po.metric
        }
        if (i==1 && j==1) { # Initialize once we know length
          postout <- array(data = NA, dim = c(n, times, length(po)))
          dimnames(postout)[[1]] <- fnames
          if (!is.null(names(po))) {dimnames(postout)[[3]] <- names(po)}
        }
        postout[i,j,] <- po
      }
      pb$tick() # tick progress bar
    } # end for i in 1:n
  }

  # Create list to return, set to class mbc so S3 methods can be used
  out_list <- list()
  class(out_list) <- c("mbc", class(out_list))

  # Process run times
  out_list$Raw_Run_times <- runtimes
  out_list$Run_times <-
    if (times > 5) {
      # plyr::adply(runtimes, 1, function(x) data.frame(min=min(x), med=median(x), mean=mean(x), max=max(x)), .id = 'Function')
      plyr::adply(runtimes, 1, function(x) {c(summary(x), sd=sd(x), neval=times)}, .id = 'Function')
    } else {
      plyr::adply(runtimes, 1, function(x) {sx <- unname(sort(x)); c(Sort=(sx), mean=mean(x), sd=sd(x), neval=times)}, .id = 'Function')
    }

  # Run post to post process
  if (!missing(post) || !missing(target)) {
    if (times > 5) {
      post_df_disp <- plyr::adply(postout, c(1,3), function(x) {as.data.frame(t(c(summary(x), sd=sd(x))))}, .id = c('Func','Stat'))
      if (paired || !paired) { # Used to only compared, now do both, there's an if(paired) below
        if (n > 1) {
          for (i1 in 1:(n-1)) {
            for (i2 in (i1+1):n) {
              for (istat in 1:(dim(postout)[3])) {
                if (paired) {
                  labeli <- paste0(dimnames(postout)[[1]][i1],'-',dimnames(postout)[[1]][i2])
                  diffs <- postout[i1,,istat] - postout[i2,,istat]
                  if (sd(diffs) > 0) {
                    ttest_diffs <- t.test(diffs)
                  } else {
                    ttest_diffs <- data.frame(statistic=NA, p.value=NA)
                  }
                  ttest_diffs <- t.test(diffs)
                  statname <- dimnames(postout)[[3]][istat]
                  if (is.null(statname)) {statname <- istat}
                  # comp12 <- data.frame(Func=labeli, Stat=statname, min=min(diffs), med=median(diffs), mean=mean(diffs), max=max(diffs), sd=sd(diffs), t=ttest_diffs$statistic, p=ttest_diffs$p.value)
                  comp12 <- cbind(data.frame(Func=labeli, Stat=statname),
                                  as.data.frame(t(c(summary(diffs), sd=sd(diffs), t=ttest_diffs$statistic, p=ttest_diffs$p.value)))
                  )
                  # names(comp12)[3:(3+length(diffs)-1)] <- paste0("V", 1:(length(diffs)))
                  # print(comp12)
                  # post_df_disp <- rbind(post_df_disp, comp12)
                } else { # Not paired, do unpaired t-test
                  if (sd(postout[i1,,istat]) > 0 || sd(postout[i2,,istat]) > 0) {
                    ttest_diffs <- t.test(postout[i1,,istat], postout[i2,,istat], paired=FALSE)
                    # labeli <- ttest_diffs$data.name
                  } else {
                    ttest_diffs <- list(statistic=NA, p.value=NA, conf.int=c(NA,NA))
                  }
                  labeli <- paste0(dimnames(postout)[[1]][i1],' vs ',dimnames(postout)[[1]][i2])
                  statname <- dimnames(postout)[[3]][istat]
                  if (is.null(statname)) {statname <- istat}
                  comp12 <- data.frame(Func=labeli, Stat=statname, conf.low=ttest_diffs$conf.int[1], conf.up=ttest_diffs$conf.int[2], t=ttest_diffs$statistic, p=ttest_diffs$p.value)
                }
                if (i1 == 1 && i2 == 2 && istat == 1) {
                  comp_df <- comp12
                } else {
                  comp_df <- rbind(comp_df, comp12)
                }
              }
            }
          }
          out_list$Compare <- comp_df
        }
      } else {

      }
    } else { # times <= 5
      if (paired || !paired) { # Don't sort if paired, get differences
        post_df_disp <- plyr::adply(postout, c(1,3), function(x) {c(x, mean=mean(x), sd=sd(x))}, .id = c('Func','Stat'))
        if (n > 1 && times > 1) {
          for (i1 in 1:(n-1)) {
            for (i2 in (i1+1):n) {
              for (istat in 1:(dim(postout)[3])) {
                if (paired) {
                  labeli <- paste0(dimnames(postout)[[1]][i1],'-',dimnames(postout)[[1]][i2])
                  diffs <- postout[i1,,istat] - postout[i2,,istat]
                  if (sd(diffs) > 0) {
                    ttest_diffs <- t.test(diffs)
                  } else {
                    ttest_diffs <- data.frame(statistic=NA, p.value=NA)
                  }
                  statname <- dimnames(postout)[[3]][istat]
                  if (is.null(statname)) {statname <- istat}
                  comp12 <- data.frame(Func=labeli, Stat=statname, t(diffs), mean=mean(diffs), sd=sd(diffs), t=ttest_diffs$statistic, p=ttest_diffs$p.value)
                  names(comp12)[3:(3+length(diffs)-1)] <- paste0("V", 1:(length(diffs)))
                  # print(comp12)
                  # post_df_disp <- rbind(post_df_disp, comp12)
                } else { # not paired, do unpaired t-test
                  if (sd(postout[i1,,istat]) > 0 || sd(postout[i2,,istat]) > 0) {
                    ttest_diffs <- t.test(postout[i1,,istat], postout[i2,,istat], paired=FALSE)
                    # labeli <- ttest_diffs$data.name
                  } else {
                    ttest_diffs <- list(statistic=NA, p.value=NA, conf.int=c(NA,NA))
                  }
                  labeli <- paste0(dimnames(postout)[[1]][i1],' vs ',dimnames(postout)[[1]][i2])
                  statname <- dimnames(postout)[[3]][istat]
                  if (is.null(statname)) {statname <- istat}
                  comp12 <- data.frame(Func=labeli, Stat=statname, conf.low=ttest_diffs$conf.int[1], conf.up=ttest_diffs$conf.int[2], t=ttest_diffs$statistic, p=ttest_diffs$p.value)
                }
                if (i1 == 1 && i2 == 2 && istat == 1) {
                  comp_df <- comp12
                } else {
                  comp_df <- rbind(comp_df, comp12)
                }
              }
            }
          }
          out_list$Compare <- comp_df
        }

        # } else { # If not paired/ordered, then sort them
        #   post_df_disp <- plyr::adply(postout, c(1,3), function(x) {sx <- sort(x); c(Sort=(sx), mean=mean(x), sd=sd(x))}, .id = c('Func','Stat'))
      }
    }
    out_list$RawOutput <- outs
    out_list$Output <- postout
    out_list$Output_disp <- post_df_disp
  } else { # No post or target
    # Check if all outs have same length, then convert to df if small enough just as postprocessing would
    #  in case single return value already is post
    lengths <- sapply(outs, function(listi) {sapply(listi, length)})
    len <- lengths[[1]]
    if (all(lengths == len) && any(class(outs[[1]][[1]]) %in% c("numeric", "character", "logical", "integer"))) { # Try to auto-post-process
      # Convert data to array
      postout <- array(data = NA, dim = c(n, times, len))
      dimnames(postout)[[1]] <- fnames
      # if (class(outs[[1]][[1]]) %in% c("numeric", "character", "logical")) {
      for (i in 1:n) {
        for (j in 1:times) {
          postout[i, j, ] <- outs[[i]][[j]]
        }
        # }
      }
      # Post-process
      if (is.numeric(postout)) {
        if (times > 5) {
          post_df_disp <- plyr::adply(postout, c(1,3), function(x) {as.data.frame(t(c(summary(x), sd=sd(x))))}, .id = c('Func','Stat'))
        } else {
          if (paired) {
            post_df_disp <- plyr::adply(postout, c(1,3), function(x) {c(V=x, mean=mean(x), sd=sd(x))}, .id = c('Func','Stat'))
          } else {
            post_df_disp <- plyr::adply(postout, c(1,3), function(x) {sx <- sort(x); c(Sort=(sx), mean=mean(x), sd=sd(x))}, .id = c('Func','Stat'))
          }
        }
      } else if (is.logical(postout)) {
        # post_df_disp <- plyr::adply(postout, c(1,3), table, .id = c('Func','Stat'))
        post_df_disp <- plyr::adply(postout, c(1,3), function(x) {tr <- sum(x);c('TRUE'=tr,'FALSE'=times-tr)}, .id = c('Func','Stat'))
      } else if (is.character(postout)) {
        uniques <- unique(c(postout))
        post_df_disp <- plyr::adply(postout, c(1,3), function(x) {sapply(c("a","b"), function(ci) {sum(x==ci)})}, .id = c('Func','Stat'))
      } else {
        post_df_disp <- postout
      }
      # Set outputs
      out_list$RawOutput <- outs
      out_list$Output <- postout
      out_list$Output_disp <- post_df_disp
    } else { # Can't auto-post-process
      out_list$Output <- outs
    }
  }
  if (is.array(out_list$Output)) {
    dimnames(out_list$Output)[[1]] <- fnames
  }
  out_list
}


#' Plot mbc class
#'
#' @param x Object of class mbc
#' @param ... Additional parameters
#' @importFrom graphics stripchart
#'
#' @return None
#' @export
#'
#' @examples
#' m1 <- mbc(mn= {Sys.sleep(rexp(1, 30));mean(x)},
#'           med={Sys.sleep(rexp(1, 5));median(x)},
#'           input=runif(100))
#' plot(m1)
plot.mbc <- function(x, ...) {
  # stripchart(x$Run_times)
  stripchart(as.data.frame(t(x$Raw_Run_times)), main="Run times", xlab="Seconds")
  for (i in 1:dim(x$Output)[3]) {
    stripchart(as.data.frame(t(x$Output[,,i])), main=dimnames(x$Output)[[3]][i])
  }
}

#' Print mbc class
#'
#' @param x Object of class mbc
#' @param ... Additional parameters
#'
#' @return None
#' @export
#'
#' @examples
#' m1 <- mbc({Sys.sleep(rexp(1, 30));mean(x)},
#'           {Sys.sleep(rexp(1, 5));median(x)},
#'           input=runif(100))
#' print(m1)
print.mbc <- function(x, ...) {
  nam <- names(x)
  if ('Run_times' %in% nam) {
    cat("Run times (sec)\n")
    print(x$Run_times)
  }
  if ('Output_disp' %in% nam) {
    cat("\nOutput summary\n")
    if (length(unique(x$Output_disp$Stat)) > 1) { # If more than 1 stat, print separately by function
      tp <- plyr::dlply(x$Output_disp, 'Func', identity)
      suppress <- lapply(tp, print)
    } else {
      print(x$Output_disp)
    }
    # } else if ('Output' %in% nam) {
    #   if (is.data.frame(x$Output) || is.numeric(x$Output)) { # Only print df, not list
    #     cat("\nOutput \n")
    #     print(x$Output)
    #   }
  }
  if ("Compare" %in% nam) {
    cat("\nCompare\n")
    print(x$Compare)
  }
}
