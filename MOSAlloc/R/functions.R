# Filename: functions.R # file of help functions
#
# Date: 01.05.2024
# Author: Felix Willems
# Contact: mail.willemsf+MOSAlloc@gmail.com
#         (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
#
# Please report any bugs or unexpected behavior to
# mail.willemsf+MOSAlloc@gmail.com 
# (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
#
#---------------------------------------------------------------------------

# Constructor for sparse cone matrices: st_col and st_row specify the location
# of X inside of a larger matrix M in compressed sparse row (CSR) format. 
# E.g. M[st_row + 1, st_col + 1] gives X[1,1].
getColRowVal <- function(X, st_col, st_row) {
  nonzero <- which(X != 0)
  list(col(X)[nonzero] + st_col,
       row(X)[nonzero] + st_row,
       X[nonzero])
}

# WCM problem solve for a priori weight w - returns extensive information
wECOSCsolve <- function(w, dsc, D, d, a, c, l, u, scalepar, scale_t,
                         A_init, b_init, ecos_control, t0, t1) {

  # get dimension of input data
  H <- ncol(D)
  kD <- length(d)
  kA <- length(a)
  kC <- length(c)

  # scale problem depending on w
  A_init[1:kD, 2 * H + 1] <- -1 * scale_t / as.vector(w) / as.vector(dsc)

  # solve problem via ECOSolveR
  sol <- ECOSolveR::ECOS_csolve(
    c = c(matrix(0, 1, 2 * H), 1),
    G = A_init,
    h = b_init,
    dims = list(l = nrow(A_init) - 3 * H, q = matrix(3, H, 1), e = 0),
    A = NULL,
    b = numeric(0),
    bool_vars = integer(0),
    int_vars = integer(0),
    control = ecos_control
  )

  # get computation time
  t2 <- proc.time()

  # reconstruct optimal sample size, objective vector, and dual variables
  if (kA == 0) {KA <- NULL} else {KA <- 1:kA}
  nopt <- sol$x[1:H] / scalepar
  #topt <- sol$x[2*H + 1] * d
  if (sum(A_init[1:kD, 1]) == 0) {
    J <- c(D %*% matrix(1 / nopt, ncol = 1) - d)
  } else {
    J <- c(D %*% matrix(nopt, ncol = 1) - d)
  }
  lambda <- sol$z[1:kD] * scale_t / as.vector(w) / as.vector(dsc)
  #solz <- sol$z * scale_t
  solz <- sol$z * max(w * J) / sol$x[2 * H + 1]

  # return
  return(list(
    w = w,
    n = nopt,
    J = J,
    Objective = NULL,
    Utiopian = NULL,
    Normal = lambda * w,
    dfJ = NULL,
    Sensitivity = list(D = lambda,
                       A = solz[KA + kD] / a, # sensitivity of pre. con.
                       C = solz[1:kC + kD + kA] / c, # / c
                       lbox = solz[1:H + kD + kA + kC] / l, # / l
                       ubox = solz[1:H + kD + kA + kC + H] / u), # / u
    Qbounds = sol$x[1:H] * sol$x[1:H + H],
    Dbounds = w * J,
    Scalingfactor = scalepar,
    Ecosolver = list(Ecoinfostring = sol$infostring,
                     Ecoredcodes = sol$retcodes,
                     Ecosummary = sol$summary),
    Timing = array(c(t2[3] - t0[3], t2[3] - t1[3], sol$timing), c(1, 5),
                   dimnames = list(NULL,c("TotalTime", "InnerTime",
                                          paste0("ECOS_",names(sol$timing))))),
    Iteration = NULL))
}

# Solve weighted sum scalarization with equal preference weighting of the
# objective components (function is used as initial step of the projection
# method)
sumSol <- function(w, A_init, b_init, Q, H, ecos_control) {

  # get dimension of input data
  k   <- nrow(A_init) - Q
  idx <- 1:k + Q

  # solve problem via ECOSolveR
  sol <- ECOSolveR::ECOS_csolve(
    c = c(as.vector(matrix(w, 1, Q) %*% A_init[1:Q, 1:(2 * H)]), 0),
    G = A_init[idx, ],
    h = b_init[idx],
    dims = list(l = k - 3 * H, q = matrix(3, H, 1), e = 0),
    A = NULL,
    b = numeric(0),
    bool_vars = integer(0),
    int_vars = integer(0),
    control = ecos_control)

  return(list(feasibility = sol$retcodes,
              x = sol$x))
}

# Computes the scaled utopian vector, i.e. computes the univariate optimal
# objective values
getUnix <- function(A_init, b_init, Q, H, ecos_control, mc_cores) {

  k     <- nrow(A_init)
  qsize <- matrix(3, H, 1)
  lsize <- k - sum(qsize) - Q + 1

  if (mc_cores == 1L) {
    out <- lapply(1:Q, function(i) {
      idx <- c(i, 1:(k - Q) + Q)
      A_in <- A_init[idx,]
      b_in <- b_init[idx]

      sol <- ECOSolveR::ECOS_csolve(
        c = c(matrix(0, 1, 2 * H), 1),
        G = A_in,
        h = b_in,
        dims = list(l = lsize, q = qsize, e = 0),
        A = NULL,
        b = numeric(0),
        bool_vars = integer(0),
        int_vars = integer(0),
        control = ecos_control)
      sol$x[1:H]
    })
  } else {
    out <- parallel::mclapply(1:Q, function(i) {
      idx <- c(i, 1:(k - Q) + Q)
      A_in <- A_init[idx, ]
      b_in <- b_init[idx]

      sol <- ECOSolveR::ECOS_csolve(
        c = c(matrix(0, 1, 2*H), 1),
        G = A_in,
        h = b_in,
        dims = list(l = lsize, q = qsize, e = 0),
        A = NULL,
        b = numeric(0),
        bool_vars = integer(0),
        int_vars = integer(0),
        control = ecos_control)
      sol$x[1:H]
    }, mc.cores = mc_cores)
  }
  return(do.call("rbind", out))
}

# Solve inner projection method step for weight w; internal solve function
# with less overhead than mosalloc()
wSolve <- function(w, dsc, A_init, b_init, D, d, df,
                   scalepar, scale_t, ecos_control) {
  # get system time for time computation
  t1 <- Sys.time()

  # get dimension of input data
  H <- ncol(D)
  Q <- nrow(D)
  qsize <- matrix(3, H, 1)

  # scale problem depending on w
  A_init[1:Q, 2 * H + 1] <- -1 * scale_t / as.vector(w) / as.vector(dsc)

  # solve problem via ECOSolveR
  sol <- ECOSolveR::ECOS_csolve(
    c = c(matrix(0, 1, 2 * H), 1),
    G = A_init,
    h = b_init,
    dims = list(l = nrow(A_init) - sum(qsize), q = qsize, e = 0),
    A = NULL,
    b = numeric(0),
    bool_vars = integer(0),
    int_vars = integer(0),
    control = ecos_control)

  # reconstruct sample size, objective vector, dual variables
  n       <- sol$x[1:H] / scalepar
  #z       <- sol$x[1:H + H]*scalepar
  #J       <- c(D %*% matrix(1 / n, ncol = 1) - d)
  if (sum(A_init[1:Q, 1]) == 0) {
    J <- c(D %*% matrix(1 / n, ncol = 1) - d)
  } else {
    J <- c(D %*% matrix(n, ncol = 1) - d)
  }
  lam     <- sol$z[1:Q] * scale_t / as.vector(w) / as.vector(dsc)
  # calculate normal vector, gradient and projection for J
  normalJ <- lam * as.vector(w)
  gradfJ  <- df(J)
  p       <- as.vector(normalJ - sum(normalJ**2)/sum(normalJ*gradfJ) * gradfJ)
  #p      <- as.vector(-gradfJ + sum(gradfJ*normalJ)/sum(normalJ**2)*normalJ)
  #p      <- p/sqrt(sum(p**2))

  # get system time for time computation
  t2 <- Sys.time()

  return(list(n = n, z = sol$x[1:H + H]*scalepar, J = J, w = w,
              lamda = lam, normalJ = normalJ,
              gradfJ = gradfJ, projection = p,
              timing = list(solver = sol$timing,
                            framework = t2 - t1 - sol$timing[1],
                            runtime = t2 - t1),
              Qbound = sol$x[1:H] * sol$x[1:H + H],
              Dbound = as.vector(A_init[1:Q,] %*% matrix(sol$x, ncol = 1) -
                                   matrix(b_init[1:Q], ncol = 1)),
              solver = sol))
}

# Function returning the angle between vectors G and N
optDeg <- function(G, N) {
  G <- as.vector(G)
  N <- as.vector(N)
  val <- c((G %*% N) / (sqrt(sum(G**2)) * sqrt(sum(N**2))))
  if (val < 1) {
    c(acos((G %*% N) / (sqrt(sum(G**2)) * sqrt(sum(N**2)))) * 180 / pi)
  }else{
    0
  }
}

# Step-wise weighted Chebychev minimization(WCM) for weight w
# (equivalent variation of Algorithm 1 in Willems, 2025)
wcSolquick <- function(A_init, b_init, Q, H, ecos_control) {

  # get input data + dimensions
  A0    <- A_init
  b0    <- b_init
  Qidx  <- 1:Q
  qsize <- matrix(3, H, 1)
  lsize <- nrow(A0) - sum(qsize)

  # loop while objective index set is nonempty
  while (length(Qidx) != 0) {

    sol <- ECOSolveR::ECOS_csolve(
      c = c(matrix(0, 1, 2 * H), 1),
      G = A0,
      h = as.vector(b0),
      dims = list(l = lsize, q = qsize, e = 0),
      A = NULL,
      b = numeric(0),
      bool_vars = integer(0),
      int_vars = integer(0),
      control = ecos_control)

    # get current upper bound t
    t_temp <- sol$x[H * 2 + 1]

    # find tightest objective constraint
    nr <- which.max(as.vector(A0[Qidx,]%*%matrix(sol$x, ncol = 1) - matrix(
      b0[Qidx],ncol=1)))
    # find strata whose sample size is fixed after current iteration
    hsave <- A0[Qidx,,drop=FALSE][nr, 1:H + H] != 0
    # drop all objectives with certain strata from index set
    dropQidx <- apply(A0[Qidx, (1:H)[!hsave] + H, drop=FALSE],1,sum) == 0

    # fix certain objectives as a constraint
    b0[Qidx[dropQidx]] <-  b0[Qidx[dropQidx]] - A0[Qidx[dropQidx],
                                                   H * 2 + 1] * t_temp
    A0[Qidx[dropQidx], H * 2 + 1] <- 0
    # reduce set of objective constraints and go to next iterate
    Qidx <- Qidx[!dropQidx]
  }
  return(sol$x[1:H])
}

# function for quadratic Berzier curve
quadraticBezier <- function(t, b0, b1, b2) {
  b1 + (1 - t)**2 * (b0 - b1) + t**2 * (b2 - b1)
}
# function for first derivative of Bezier curve
dquadraticBezier <- function(t, b0, b1, b2) {
  2 * (1 - t) * (b1 - b0) + 2 * t * (b2 - b1)
}
# function for second derivative of Bezier curve
d2quadraticBezier <- function(t, b0, b1, b2) {
  2 * (b2 + b0) - 4 * b1
}

# summarize mosaSTRS object (29.12.2025)
sumMosaSTRSObj <- function(x) {
  # reorganize objective output
  objout <- x$objectives
  if (!is.null(x$opt_w)) {
    objout$opt_w <- x$opt_w
  }
  if (x$method == "WSS") {
    objout$init_w <- x$init_w[2, ]
    colnames(objout)[5] <- "init_w_WCM"
    objout$init_w_WSS <- x$init_w[1, ]
  } else {
    objout$init_w <- x$init_w[1, ]
  }
  if (x$sense != "min_cost") {
    objout <- objout[, c((1:ncol(objout))[-4], 4)]
  }
  x$objout <- objout
  x[c("vname", "sense", "method", "objout",
      "precision", "cost", "n_opt")]
}