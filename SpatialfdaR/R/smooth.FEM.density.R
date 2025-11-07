smooth.FEM.density <- function(obspts, cvec, FEMbasis, K1=NULL, lambda=0,
                              conv=.0001, iterlim=50, dbglev=FALSE)
{
  #  Compute a smooth FEM density surface of a triangulated region.
  #  Arguments:
  #  OBSPTS   ... N by two matrix containing X- and Y-coordinates of points
  #  FEMBASIS ... A functional parameter or fdPar object.  This object
  #               contains the specifications for the functional data
  #               object to be estimated by smoothing the data.  See
  #               comment lines in function fdPar for details.
  #               The functional data object Intensityfd in IntensityfdPAROBJ is used
  #               to initialize the optimization process.
  #               Its coefficient array contains the starting values for
  #               the iterative minimization of mean squared error.
  #  K1       ... An order n square stiffness matrix produced using function stiff
  #  LAMBDA   ... A non-negative real number controlling the size of the roughness
  #               penalty LAMBDA*t(CVEC) %*% K1 %*% CVEC
  #  CONV    ...  Convergence criterion, 0.0001 by default
  #  ITERLIM ...  maximum number of iterations, 50 by default.
  #  DBGLEV  ...  Controls the level of output on each iteration.  If 0,
  #               no output, if 1, output at each iteration, if higher,
  #               output at each line search iteration. 1 by default.

  #  Return is a named list object with these members:
  #  CVEC        ... Optimal coefficient vector
  #  INTENSITYFD ... Functional data object for the intensity function
  #  FLIST       ... List with members F, grad and norm values at convergence
  
  # Last modified 3 October 2022 by Jim Ramsay

  #  check OBSPTS

  if (!is.numeric(obspts)) stop("PTS is not numeric.")
  obsptsdim <-dim(obspts)
  if (obsptsdim[2] != 2) {
    stop("Argument PTS does not have two columns.")
  }
  N <- obsptsdim[1]

  #  the starting values for the coefficients are in FD object Intensityfd

  nbasis  <- FEMbasis$nbasis  #  number of basis functions
  onesbas <- matrix(1,nbasis,1)
  climit  <- 1e10*cbind(-onesbas,onesbas)
  active  <- rep(TRUE,nbasis)

  #  --------------------------------------------------------------------
  #              loop through variables and curves
  #  --------------------------------------------------------------------

  #  Compute initial function and gradient values

  result <- FEMdensity(cvec, obspts, FEMbasis, K1, lambda)
  F      <- result$F
  grad   <- result$grad
  norm   <- result$norm

  #  compute the initial expected Hessian

  hessmat <- diag(rep(1,nbasis))

  #  evaluate the initial update vector for correcting the initial cvec

  deltac <- -grad

  #  initialize iteration status arrays

  iternum <- 0
  status  <- c(iternum, F, norm)
  if (dbglev >= 1) {
    cat("\nIter.   PENSSE   Grad Length\n")
    cat(iternum)
    cat("        ")
    cat(round(status[2],4))
    cat("      ")
    cat(round(status[3],4))
  } else {
    cat(".")
  }

  #  -------  Begin iterations  -----------

  MAXSTEPITER <- 10
  MAXSTEP     <- 2
  trial       <- 1
  reset       <- FALSE
  linemat     <- matrix(0,3,5)
  cvecold     <- cvec
  resultold   <- result
  dbgwrd      <- dbglev >= 2

  if (iterlim > 0) {
    for (iter in 1:iterlim) {
      iternum <- iternum + 1
      #  initialize logical variables controlling line search
      dblwrd <- rep(FALSE,2)
      limwrd <- rep(FALSE,2)
      stpwrd <- FALSE
      ind    <- 0
      ips    <- 0
      #  compute slope at 0 for line search
      linemat[2,1] <- sum(deltac*grad)
      #  normalize search direction vector
      sdg     <- sqrt(sum(deltac^2))
      deltac  <- deltac/sdg
      dgsum   <- sum(deltac)
      linemat[2,1] <- linemat[2,1]/sdg
      # initialize line search vectors
      linemat[,1:4] <- outer(c(0, linemat[2,1], F),rep(1,4))
      stepiter <- 0
      if (dbglev >= 2) {
        cat("\n")
        cat(paste("                 ", stepiter, "  "))
        cat(format(round(t(linemat[,1]),6)))
      }
      #  break with error condition if initial slope is nonnegative
      if (linemat[2,1] >= 0) {
        if (dbgwrd >= 2) print("Initial slope nonnegative.")
        ind <- 3
        break
      }
      #  return successfully if initial slope is very small
      if (linemat[2,1] >= -1e-7) {
        if (dbglev >= 2) print("Initial slope too small")
        ind <- 0
        break
      }
      #  first step set to trial
      linemat[1,5]  <- trial
      cvecnew <- cvec
      #  Main iteration loop for linesearch
      for (stepiter in 1:MAXSTEPITER) {
        #  ensure that step does not go beyond limits on parameters
        limflg  <- FALSE
        #  check the step size
        stepresult <-
          stepchk(linemat[1,5], cvec, deltac, limwrd, ind,
                  climit, active, dbgwrd)
        linemat[1,5] <- stepresult[[1]]
        ind          <- stepresult[[2]]
        limwrd       <- stepresult[[3]]
        if (linemat[1,5] <= 1e-7)
        {
          #  Current step size too small ... terminate
          resultold  <- result
          cvecnew <- cvec
          gvecnew <- resultold$grad
          if (dbglev >= 2) {
            print("Stepsize too small")
            print(linemat[1,5])
          }
          #if (limflg) ind <- 1 else ind <- 4
          #break needed??
        }
        #  compute new function value and gradient
        cvecnew <- cvec + linemat[1,5]*deltac
        result  <- FEMdensity(cvecnew, obspts, FEMbasis, K1, lambda)
        Fnew <- result$F
        gradnew <- result$grad
        linemat[3,5] <- Fnew
        #  compute new directional derivative
        linemat[2,5] <- sum(deltac*gradnew)
        if (dbglev >= 2) {
          cat("\n")
          cat(paste("                 ", stepiter, "  "))
          cat(format(round(t(linemat[,5]),6)))
        }
        #  compute next line search step, also test for convergence
        stepresult <- stepit(linemat, ips, dblwrd, MAXSTEP)
        linemat <- stepresult[[1]]
        ips     <- stepresult[[2]]
        ind     <- stepresult[[3]]
        dblwrd  <- stepresult[[4]]
        trial   <- linemat[1,5]
        #  ind == 0  mean convergence
        if (ind == 0 | ind == 5) break
        #  end of line search loop
      }
      cvecdif <- cvecnew - cvec
      graddif <- gradnew - grad
      cvec    <- cvecnew
      F       <- Fnew
      grad    <- gradnew
      #  check that function value has not increased
      if (F > resultold$F) {
        # if it has, terminate iterations with a message
        if (dbglev >= 2) {
          cat("Criterion increased: ")
          cat(format(round(c(resultold$F, F),4)))
          cat("\n")
        }
        #  reset parameters and fit
        cvec   <- cvecold
        result <- resultold
        deltac <- -result$grad
        if (reset) {
          # This is the second time in a row that
          #  this has happened ... quit
          if (dbglev >= 2) cat("Reset twice, terminating.\n")
          break
        } else {
          reset <- TRUE
        }
      } else {
        if (abs(resultold$F - F) < conv) {
          if (dbglev >= 1) cat("\n")
          break
        }
        cvecold   <- cvec
        resultold <- result
        if (iter < iterlim)  {
          hessdif <- hessmat %*% graddif
          fac     <- sum(graddif*cvecdif)
          fae     <- sum(graddif*hessdif)
          sumdif  <- sum(graddif*graddif)
          sumdel  <- sum(cvecdif*cvecdif)
          if (fac > sqrt(1e-3*sumdif*sumdel)) {
            graddif <- cvecdif/fac - hessdif/fae
            hessmat <- hessmat + cvecdif %*% t(cvecdif)/fac -
              hessdif %*% t(hessdif)/fae +
              graddif %*% t(graddif)*fae
          }
          #  update the line search direction vector
          deltac <- -hessmat %*% grad
          reset <- 0;
        }
      }
      #  display iteration status
      status <- c(iternum, Fnew, result$norm)
      if (dbglev >= 1) {
        cat("\n")
        cat(iternum)
        cat("        ")
        cat(round(status[2],4))
        cat("      ")
        cat(round(status[3],4))
      }
    }
  }

  Intensityfd <- fd(cvec, FEMbasis)

  outputList <- list(cvec=cvec, Intensityfd=Intensityfd, Flist=result)
  return(outputList)
}

