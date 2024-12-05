#' @title Fast Integrative Clustering for High-Dimensional Multi-View Data Using
#'   Variational Bayesian Inference
#'
#' @description `iClusterVB` offers a novel, fast, and integrative approach to
#'   clustering high-dimensional, mixed-type, and multi-view data. By employing
#'   variational Bayesian inference, iClusterVB facilitates effective feature
#'   selection and identification of disease subtypes, enhancing clinical
#'   decision-making.
#'
#' @param mydata A list of length R, where R is the number of datasets,
#'   containing the input data.
#'    * Note: For \bold{categorical} data, \code{0}'s must be re-coded to
#'   another, non-\code{0} value.
#' @param dist A vector of length R specifying the type of data or distribution.
#'   Options include: 'gaussian' (for continuous data), 'multinomial' (for
#'   binary or categorical data), and 'poisson' (for count data).
#' @param K The maximum number of clusters, with a default value of 10. The
#'   algorithm will converge to a model with dominant clusters, removing
#'   redundant clusters and automating the determination of the number of
#'   clusters.
#' @param initial_method The initialization method for cluster allocation.
#'   Options include: "VarSelLCM" (default), "random", "kproto" (k-prototypes),
#'   "kmeans" (continuous data only), "mclust" (continuous data only), or "lca"
#'   (poLCA, categorical data only).
#' @param VS_method The variable/feature selection method. Options are 0 for
#'   clustering without variable/feature selection (default) and 1 for
#'   clustering with variable/feature selection.
#' @param initial_cluster The initial cluster membership. The default is NULL,
#'   which uses initial_method for initial cluster allocation. If not NULL, it
#'   will override the initial values setting for this parameter.
#' @param initial_vs_prob The initial variable/feature selection probability, a
#'   scalar. The default is NULL, which assigns a value of 0.5.
#' @param initial_fit Initial values based on a previously fitted iClusterVB
#'   model (an iClusterVB object). The default is NULL.
#' @param initial_omega Customized initial values for feature inclusion
#'   probabilities. The default is NULL. If not NULL, it will override the
#'   initial values setting for this parameter. If VS_method = 1, initial_omega
#'   is a list of length R, with each element being an array with dimensions
#'   \{dim=c(N, p\[\[r\]\])\}. Here, N is the sample size and p\[\[r\]\] is the
#'   number of features for dataset r, where r = 1, ..., R.
#' @param input_hyper_parameters A list of the initial hyper-parameters of the
#'   prior distributions for the model. The default is NULL, which assigns
#'   alpha_00 = 0.001, mu_00 = 0, s2_00 = 100, a_00 = 1, b_00 = 1,kappa_00 = 1,
#'   u_00 = 1, v_00 = 1.
#' @param max_iter The maximum number of iterations for the VB algorithm. The
#'   default is 200.
#' @param early_stop Whether to stop the algorithm upon convergence or to
#'   continue until \code{max_iter} is reached. Options are 1 (default) to stop
#'   when the algorithm converges, and 0 to stop only when \code{max_iter} is
#'   reached.
#' @param per Print information every "per" iterations. The default is 10.
#' @param convergence_threshold The convergence threshold for the change in
#'   ELBO. The default is 0.0001.
#'
#' @note If any of the data views are "gaussian", please include them
#'   \bold{first}, both in the input data \code{mydata} and correspondingly in
#'   the
#'   distribution vector \code{dist}. For example, \code{dist <-
#'   c("gaussian","gaussian", "poisson", "multinomial")}, and \bold{not}
#'   \code{dist <- c("poisson", "gaussian","gaussian", "multinomial")} or
#'   \code{dist <- c("gaussian", "poisson", "gaussian", "multinomial")}
#'
#'
#'
#' @return The \code{iClusterVB} function creates an object (list) of class
#'   \code{iClusterVB}. Relevant outputs include:
#'
#'
#'   \item{\code{elbo}:}{ The evidence lower bound for each iteration.}
#'   \item{\code{cluster}:}{ The cluster assigned to each individual.}
#'   \item{\code{initial_values}:}{ A list of the initial values.}
#'   \item{\code{hyper_parameters}:}{ A list of the hyper-parameters.}
#'  \item{\code{model_parameters}:}{A list of the model parameters after the
#'  algorithm is run.}
#'    - Of particular interest is \code{rho}, a list of the posterior
#'   inclusion probabilities for the features in each of the data views. This is
#'   the probability of including a certain predictor in the model, given the
#'   observations. This is only available if \code{VS_method = 1}.
#'
#'
#' @examples
#' # sim_data comes with the iClusterVB package.
#' dat1 <- list(
#'   gauss_1 = sim_data$continuous1_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   gauss_2 = sim_data$continuous2_data[c(1:20, 61:80, 121:140, 181:200), 1:75],
#'   poisson_1 = sim_data$count_data[c(1:20, 61:80, 121:140, 181:200), 1:75])
#'
#' dist <- c(
#'   "gaussian", "gaussian",
#'   "poisson")
#'
#' # Note: `max_iter` is a time-intensive step.
#' # For the purpose of testing the code, use a small value (e.g. 10).
#' # For more accurate results, use a larger value (e.g. 200).
#'
#' fit_iClusterVB <- iClusterVB(
#'   mydata = dat1,
#'   dist = dist,
#'   K = 4,
#'   initial_method = "VarSelLCM",
#'   VS_method = 1,
#'   max_iter = 10
#' )
#'
#' # We can obtain a summary using the summary() function
#' summary(fit_iClusterVB)
#'
#' @import mvtnorm MCMCpack VarSelLCM cluster clustMixType poLCA
#' @importFrom grDevices colorRampPalette colors devAskNewPage topo.colors
#' @importFrom graphics abline axis barplot text par
#' @importFrom stats aggregate kmeans model.matrix reorder setNames var
#' @importFrom utils capture.output
#' @importFrom R.utils nullfile
#' @rawNamespace import(mclust, except = dmvnorm)
#' @rawNamespace import(Rcpp, except = registerPlugin)
#' @export iClusterVB
#' @useDynLib iClusterVB, .registration=TRUE
#' @md
#'


iClusterVB <- function(
    mydata, # input data - list of length R data set (each data set is of N times p_r dimensional)
    dist, # type of data, or distribution: dist = "continuous", "multinomial", "poisson" (vector of length R)
    K = 10, # maximum number of clusters
    initial_method = "VarSelLCM", # initialization method: VarSelLCM, random, k-prototype, k-means, mclust, lca,
    VS_method = 0, # variable selection method: 0 = clustering no variable selection, 1 = clustering with variable selection
    initial_cluster = NULL, # initial cluster membership, if it is not NULL, it will overwrite the previous initial values setting for this parameter
    initial_vs_prob = NULL, # initial variable selection probability, a scalar
    initial_fit = NULL, # initial values based on previously fitted iClusterVB model (an iClusterVB object)
    initial_omega = NULL, # customized initial values for variable inclusion probabilities, if it is not NULL, it will overwrite the previous initial values setting for this parameter
    # if VS_method  = 1, initial_omega is a list with length R, each element of the list is an array with dim=c(N,p[[r]])), r = 1,...,R
    input_hyper_parameters = NULL, # input hyper-parameters of the prior distributions for the model
    max_iter = 200, # maximum iteration of the VB algorithm
    early_stop = 1, # whether stop the algorithm when converge or continue until reach max_iter: 1 - algorithm stops when converges,
    # 0 - algorithm stops when reaches the maximum iteration (regardless of converges or not)
    per = 10, # print information every per iteration
    convergence_threshold = 1e-4 # Define a convergence threshold for the change in ELBO
    ) {
  test <- 0
  if (test == 1) {
    K <- 10
    initial_method <- "VarSelLCM"
    VS_method <- 0
    initial_cluster <- NULL
    initial_vs_prob <- NULL
    initial_fit <- NULL
    initial_omega <- NULL
    input_hyper_parameters <- NULL
    max_iter <- 200
    early_stop <- 1
    per <- 10
    convergence_threshold <- 1e-4
  }

  message(paste(rep("-", 60), sep = "", collapse = ""), "\n")
  message("Pre-processing and initializing the model", "\n")
  message(paste(rep("-", 60), sep = "", collapse = ""), "\n")

  # evaluating if there is missing data
  if (sum(is.na(do.call(cbind, mydata))) > 0) {
    stop("Error: missing data is not allowed\n")
  }

  if (initial_method == "VarSelLCM" | initial_method == "kproto") {
    mydata_new <- mydata
  }

  #------
  R <- length(mydata) # number of dataset
  # function evaluating if the number of categories for the categorical variables are identical or not
  is_all_equal <- function(lst) {
    all(sapply(lst, function(x) identical(unlist(lst[[1]]), unlist(x))))
  }
  # vector store the number of categories for each categorial variable (for each dataset)
  num.categories <- list()
  for (r in 1:R) {
    if (dist[r] == "multinomial") {
      indx <- which(which(dist == "multinomial") == r)
      num.categories[[indx]] <- apply(mydata[[r]], 2, function(x) length(unique(x)))
      if (initial_method == "VarSelLCM" | initial_method == "kproto") mydata_new[[r]] <- as.data.frame(lapply(data.frame(mydata_new[[r]]), as.factor)) # convert to categorical data to factor for further analysis
    }
  }
  M <- lapply(num.categories, function(x) x[1])
  M # M is a list

  # Evaluating if the sample size of the input datasets are the same
  sample.size <- lapply(mydata, function(x) dim(x)[1]) # sample size each dataset (in a list)
  if (is_all_equal(sample.size) == "FALSE") {
    stop("Error: sample size of the input datasets are not the same \n")
  }

  if (initial_method == "VarSelLCM" | initial_method == "kproto") {
    mydata_combine <- do.call(cbind, mydata_new) # categorical data have been converted to factor
    mydata_combine <- as.data.frame(mydata_combine)
    colnames(mydata_combine) <- paste0("x", 1:dim(mydata_combine)[2])
  } else {
    mydata_combine <- do.call(cbind, mydata)
  }

  N <- sample.size[[1]]
  N

  # dimension of each dataset
  p <- lapply(mydata, function(x) dim(x)[2])
  p # dimension of each dataset (in a list)

  #---------------------------------------------------------------------------#
  p_total <- sum(unlist(p))
  p_total # total dimension of all datasets
  p_continuous_total <- sum(dist == "gaussian")
  p_categorical_total <- sum(dist == "multinomial")
  p_count_total <- sum(dist == "poisson")


  if ((initial_method == "lca") & (p_continuous_total + p_count_total > 0)) {
    stop("lca method can only be used as an initialization method when there are only categorical datasets \n")
  }

  data_summary <- list(
    N = N,
    M = M,
    p = p,
    R = R,
    p_total = p_total,
    p_continuous_total = p_continuous_total,
    p_categorical_total = p_categorical_total,
    p_count_total = p_count_total
  )

  #---------------------------------------------------------------------------#
  # Hyper-parameters
  #---------------------------------------------------------------------------#
  if (is.null(input_hyper_parameters) == TRUE) {
    # default hyper-parameter setting
    input_hyper_parameters <- list(
      alpha_00 = 0.001, # hyper-parameter for the mixing proportion
      mu_00 = 0, # mean hyper-parameter for the mean of the Gaussian distribution
      s2_00 = 100, # variance hyper-parameter for the mean of  the Gaussian distribution
      a_00 = 1, # shape parameter for Iv-Gamma for the variance of the Gaussian distribution
      b_00 = 1, # scale parameter for Iv-Gamma for the variance of the Gaussian distribution
      kappa_00 = 1, # hyper-parameter for the Dirichlet distribution, for the categorical outcomes
      u_00 = 1, # shape parameter for Iv-Gamma for the variance of the Gaussian distribution
      v_00 = 1
    ) # scale parameter for Iv-Gamma for the rate parameter of the Poisson distribution
  }

  # Hyper-parameters for dataset with continuous variables
  mu_0 <- list() # Prior mean for cluster-specific means
  s2_0 <- numeric() # Prior variance for cluster-specific means
  a_0 <- numeric() # Shape parameter for inverse-gamma prior for variances
  b_0 <- numeric() # Rate parameter for inverse-gamma prior for variances

  # Hyper-parameters for dataset with categorical variables (Dirichlet prior distribution)
  kappa_0 <- list()

  # Hyper-parameters for dataset with count variables (Gamma prior distribution)
  u_0 <- numeric() # hyper-parameter of the gamma distribution for the rate parameter
  v_0 <- numeric() # hyper-parameter of the gamma distribution for the rate parameter

  alpha_0 <- rep(input_hyper_parameters$alpha_00, K)

  for (r in 1:R) {
    #------------------------#
    if (dist[r] == "gaussian") {
      indx <- which(which(dist == "gaussian") == r) # the sequential order of the guassian distributed dataset
      # for dataset with continuous variables: specify prior for means and variances
      mu_0[[indx]] <- rep(input_hyper_parameters$mu_00, p[[r]])
      s2_0[indx] <- input_hyper_parameters$s2_00
      a_0[indx] <- input_hyper_parameters$a_00
      b_0[indx] <- input_hyper_parameters$b_00
    }
    # for dataset with categorical variables: specify prior (Dirichlet distribution) for the probability of categories of each variable
    if (dist[r] == "multinomial") {
      indx <- which(which(dist == "multinomial") == r)
      kappa_0[[indx]] <- array(input_hyper_parameters$kappa_00, dim = c(K, p[[r]], M[[indx]])) # Hyperparameters (Dirichlet distribution) for categories
    }
    if (dist[r] == "poisson") {
      # for dataset with count variables: specify prior (gamma prior) for the rate parameter of each variable
      indx <- which(which(dist == "poisson") == r)
      u_0[indx] <- input_hyper_parameters$u_00
      v_0[indx] <- input_hyper_parameters$v_00
    }
  }

  #---------------------------------------------------------------------------#
  hyper_parameters <- list(
    mu_0 = mu_0,
    s2_0 = s2_0,
    a_0 = a_0,
    b_0 = b_0,
    kappa_0 = kappa_0,
    u_0 = u_0,
    v_0 = v_0,
    alpha_0 = alpha_0
  )

  #---------------------------------------------------------------------------#
  # Define a small value to add for numerical stability
  epsilon <- 1e-10

  #---------------------------------------------------------------------------#
  # Initialize cluster allocation
  #---------------------------------------------------------------------------#
  if (is.null(initial_cluster) == TRUE & is.null(initial_fit) == TRUE) {
    #---------------------------------------------------------------------------#
    # Initialize cluster allocation (random initialization)
    #---------------------------------------------------------------------------#
    if (initial_method == "random") {
      # initial values related to the clustering
      zz <- initial_cluster <- sample(1:K, N, replace = TRUE) # random sample of the cluster allocation
    }
    #---------------------------------------------------------------------------#
    # Initialize cluster allocation (VarSelLCM initialization)
    #---------------------------------------------------------------------------#
    if (initial_method == "VarSelLCM" & VS_method == 0) {
      # fit_VarSelLCM <- suppressWarnings(VarSelCluster(mydata_combine, gvals = 1:K, vbleSelec = FALSE, crit.varsel = "BIC"))
      fit_VarSelLCM <- suppressWarnings(VarSelCluster(mydata_combine, gvals = K, vbleSelec = FALSE, crit.varsel = "BIC"))
      zz <- initial_cluster <- as.numeric(fitted(fit_VarSelLCM))
      table(zz)
    }
    if (initial_method == "VarSelLCM" & VS_method != 0) {
      # fit_VarSelLCM <- suppressWarnings(VarSelCluster(mydata_combine, gvals = 1:K, vbleSelec = TRUE, crit.varsel = "BIC", iterKeep = 20))
      fit_VarSelLCM <- suppressWarnings(VarSelCluster(mydata_combine, gvals = K, vbleSelec = FALSE, crit.varsel = "BIC", iterKeep = 50))
      zz <- initial_cluster <- as.numeric(fitted(fit_VarSelLCM))
      table(zz)
      # omega_VarSelLCM <-  as.numeric(fit_VarSelLCM@model@omega)
    }
    #----------------------------------------------------------------------------#
    # Initialize cluster allocation (K-Medoids)
    #----------------------------------------------------------------------------#
    if (initial_method == "PAM") {
      gower_dist <- daisy(mydata_combine, metric = "gower")
      fit.PAM <- pam(gower_dist, diss = TRUE, k = K, cluster.only = TRUE)
      zz <- initial_cluster <- as.numeric(fit.PAM$clustering)
      table(zz)
    }
    #----------------------------------------------------------------------------#
    # Initialize cluster allocation (kproto initialization)
    #----------------------------------------------------------------------------#
    if (initial_method == "kproto") {
      # install.packages("clustMixType")
      capture.output(
        fit.kproto <- kproto(
          x = data.frame(mydata_combine), k = K,
          lambda = rep(1, p_total),
          type = "gower"
        ),
        file = R.utils::nullfile()
      )
      zz <- initial_cluster <- as.numeric(fit.kproto$cluster)
      table(fit.kproto$cluster)
    }
    #----------------------------------------------------------------------------#
    # Initialize cluster allocation: continuous data only (k-means initialization)
    #----------------------------------------------------------------------------#
    # if (initial_method == "kmeans" & (p_categorical_total) == 0){
    if (initial_method == "kmeans") {
      fit.km <- kmeans(data.frame(mydata_combine), centers = K)
      zz <- initial_cluster <- as.numeric(fit.km$cluster)
    }
    #----------------------------------------------------------------------------#
    # Initialize cluster allocation: continuous data only (mclust initialization)
    #----------------------------------------------------------------------------#
    # if (initial_method == "mclust" & (p_categorical_total) == 0){
    if (initial_method == "mclust") {
      fit.mclust <- Mclust(as.matrix(mydata_combine), G = K, verbose = FALSE)
      zz <- initial_cluster <- as.numeric(fit.mclust$classification)
    }
    #------------------------------------------------------------------------------------#
    # Initialize cluster allocation: categorical data only	(lca initialization)
    #------------------------------------------------------------------------------------#
    if (initial_method == "lca" & (p_continuous_total + p_count_total) == 0) {
      # recode 0 to a category with positive number
      mydata_combine[mydata_combine == 0] <- max(mydata_combine) + 1
      fit.lca <- poLCA(as.matrix(mydata_combine) ~ 1, data = NULL, nclass = K, verbose = FALSE, calc.se = FALSE)
      zz <- initial_cluster <- as.numeric(fit.lca$predclass)
      table(zz)
    }
  }
  if (is.null(initial_cluster) == FALSE) {
    zz <- as.numeric(initial_cluster)
  }
  if (is.null(initial_fit) == FALSE) {
    zz <- as.numeric(initial_fit$cluster)
  } # over-write the previous class assignment

  #------------------------------------------------------------------------------------#
  # Initialize other cluster allocation parameters
  #------------------------------------------------------------------------------------#
  zz <- factor(zz, levels = 1:K)
  phi <- as.matrix(model.matrix(~ zz - 1)) # Cluster assignment probabilities
  ppi <- apply(phi, 2, mean) # Mixing proportions
  alpha <- as.numeric(table(zz))
  log_phi <- log(phi + epsilon)
  #------------------------------------------------------------------------------------#
  # Initial values related to the variable selection
  #------------------------------------------------------------------------------------#
  omega <- list() # similar to pi
  ss <- list() # similar to zz
  rho <- list()
  omega_tmp <- list()
  e <- list()
  gamma <- list()
  xi <- list()
  #------
  if (VS_method == 1 & is.null(initial_fit) == TRUE) {
    for (r in 1:R) {
      if (is.null(initial_vs_prob) == TRUE) {
        # if(N >= p_total)omega[[r]] <- array(0.9, dim=c(N,p[[r]]))
        # if(N < p_total) omega[[r]] <- array(0.1, dim=c(N,p[[r]]))
        omega[[r]] <- array(0.5, dim = c(N, p[[r]]))
      } # similar to phi
      if (is.null(initial_vs_prob) == FALSE) {
        omega[[r]] <- array(initial_vs_prob, dim = c(N, p[[r]]))
      } # similar to phi                                  }
      ss[[r]] <- array(0, dim = c(N, p[[r]])) # similar to zz
      ss[[r]][omega[[r]] >= 0.5] <- 1
      rho[[r]] <- t(apply(omega[[r]], MARGIN = 2, FUN = mean))
      rho
    }
    omega_tmp <- omega # initial value only
    e <- gamma <- rho # initial value only (for variational parameters)
    xi <- omega # initial value only
  }

  #----------------------------------------------------------------------------#
  #----------------------------------------------------------------------------#
  mu <- list() # Cluster-specific means
  sigma2 <- list() # Cluster-specific variances
  mu_tilde <- list() # Posterior mean update for mean parameter
  s2_tilde <- list() # Posterior variance update for mean parameter
  a_tilde <- b_tilde <- list() # parameter for inverse gamma distribution
  theta <- list() # parameter for dirichlet distribution of categorical data
  kappa <- list()
  theta_e <- list()
  lambda <- list()
  lambda_e <- list()
  u_tilde <- list() # parameter for poisson distribution
  v_tilde <- list() # parameter for poisson distribution
  #----------------------------------------------------------------------------#

  if (length(zz) != N) {
    stop("Please use a different initialization method or start with a lower `K` \n")
  }

  for (r in 1:R) {
    # for continuous variables
    if (dist[r] == "gaussian") {
      indx <- which(which(dist == "gaussian") == r) # the sequential order of the Gaussian distributed dataset
      mu[[indx]] <- matrix(0, ncol = p[[r]], nrow = K)
      mu[[indx]][1:length(unique(zz)), ] <- as.matrix(aggregate(mydata[[r]], list(zz), mean)[, -1]) # Cluster-specific means
      sigma2[[indx]] <- matrix(0, ncol = p[[r]], nrow = K)
      sigma2[[indx]][1:length(unique(zz)), ] <- as.matrix(aggregate(mydata[[r]], list(zz), var)[, -1]) # Cluster-specific variances
      a_tilde[[indx]] <- matrix(0.01, ncol = p[[r]], nrow = K) # initial value only
      b_tilde[[indx]] <- matrix(0.01, ncol = p[[r]], nrow = K) # initial value only
      mu_tilde[[indx]] <- mu[[indx]] # initial value only
      s2_tilde[[indx]] <- sigma2[[indx]] # initial value only
    }
    # for categorical data
    if (dist[r] == "multinomial") {
      indx <- which(which(dist == "multinomial") == r)
      tmp <- array(0, dim = c(K, p[[r]], M[[indx]]))

      theta[[indx]] <- array(0, dim = c(K, p[[r]], M[[indx]]))
      kappa[[indx]] <- array(0, dim = c(K, p[[r]], M[[indx]]))
      theta_e[[indx]] <- array(0, dim = c(K, p[[r]], M[[indx]]))

      for (k in 1:K) {
        for (j in 1:p[[r]]) {
          for (m in 1:M[[indx]]) {
            kappa[[indx]][k, j, m] <- kappa_0[[indx]][k, j, m] + sum((mydata[[r]][, j] == m) * phi[, k])
            theta_e[[indx]][k, j, m] <- sum(mydata[[r]][, j] == m) / N
          }
          theta[[indx]][k, j, ] <- kappa[[indx]][k, j, ] / sum(kappa[[indx]][k, j, ])
        }
      }
    }
    # for count data
    if (dist[r] == "poisson") {
      indx <- which(which(dist == "poisson") == r)
      lambda[[indx]] <- matrix(0, ncol = p[[r]], nrow = K)
      lambda[[indx]][1:length(unique(zz)), ] <- as.matrix(aggregate(mydata[[r]], list(zz), mean)[, -1])
      lambda
      lambda_e[[indx]] <- lambda[[indx]] # initial value only
      u_tilde[[indx]] <- lambda[[indx]] # initial value only
      v_tilde[[indx]] <- lambda[[indx]] / lambda[[indx]] # initial value only
    }
  }

  #------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------#
  # obtain initial values from the previous iClusterVB model
  if (is.null(initial_fit) == FALSE) {
    if (!(initial_fit$algo %in% c("CAVI_algorithm_standard", "CAVI_algorithm_global")) == TRUE) {
      stop("initial_fit$algo should be either 'CAVI_algorithm_standard' or 'CAVI_algorithm_global' \n")
    }

    # clustering parameters
    zz <- initial_cluster <- initial_fit$cluster
    phi <- initial_fit$model_parameters$phi
    ppi <- initial_fit$model_parameters$ppi
    alpha <- as.numeric(table(zz))
    log_phi <- log(phi + epsilon)

    # initial values for cluster-specific parameters (continuous variables)
    mu <- initial_fit$model_parameters$mu
    sigma2 <- initial_fit$model_parameters$sigma2
    mu_tilde <- initial_fit$model_parameters$mu_tilde
    s2_tilde <- initial_fit$model_parameters$s2_tilde
    a_tilde <- initial_fit$model_parameters$a_tilde
    b_tilde <- initial_fit$model_parameters$b_tilde
    # initial values for cluster-specific parameters (categorical variables)
    theta <- initial_fit$model_parameters$theta
    theta_e <- initial_fit$model_parameters$theta_e
    kappa <- initial_fit$model_parameters$kappa
    # initial values for cluster-specific parameters (count variables)
    lambda <- initial_fit$model_parameters$lambda
    lambda_e <- initial_fit$model_parameters$lambda_e
    u_tilde <- initial_fit$model_parameters$u_tilde
    v_tilde <- initial_fit$model_parameters$v_tilde

    # variable selection parameters
    # -------
    if (VS_method == 1 & initial_fit$algo == "CAVI_algorithm_standard") {
      for (r in 1:R) {
        if (is.null(initial_vs_prob) == TRUE) {
          omega[[r]] <- array(0.5, dim = c(N, p[[r]]))
        } # similar to phi
        if (is.null(initial_vs_prob) == FALSE) {
          omega[[r]] <- array(initial_vs_prob, dim = c(N, p[[r]]))
        } # similar to phi
        ss[[r]] <- array(0, dim = c(N, p[[r]])) # similar to zz
        ss[[r]][omega[[r]] >= 0.5] <- 1
        rho[[r]] <- t(apply(omega[[r]], MARGIN = 2, FUN = mean))
        rho
      }
      omega_tmp <- omega
      e <- gamma <- rho
      xi <- omega
    }
    # -------
    if (VS_method == 1 & initial_fit$algo == "CAVI_algorithm_global") {
      omega <- initial_fit$model_parameters$omega
      ss <- initial_fit$model_parameters$ss
      rho <- initial_fit$model_parameters$rho
      # omega_tmp <- initial_fit$model_parameters$omega_tmp
      # xi <- initial_fit$model_parameters$xi
      # e <- initial_fit$model_parameters$e
      # gamma <- initial_fit$model_parameters$gamma
      omega_tmp <- omega
      e <- gamma <- rho
      xi <- omega
    }
  }

  # Customized initial values for cluster membership
  # If it is not NULL, it will overwrite the previous initial values setting for this parameter
  # if (is.null(initial_cluster) == FALSE ){ zz <- initial_cluster}

  # Customized initial values for variable inclusion probabilities
  # If it is not NULL, it will overwrite the previous initial values setting for this parameter

  if (is.null(initial_omega) == FALSE) {
    omega <- initial_omega
    #----------
    if (VS_method == 1) {
      for (r in 1:R) {
        ss[[r]] <- array(0, dim = c(N, p[[r]])) # similar to zz
        ss[[r]][omega[[r]] >= 0.5] <- 1
        rho[[r]] <- t(apply(omega[[r]], MARGIN = 2, FUN = mean))
        rho
      }
      omega_tmp <- omega
      e <- gamma <- rho
      xi <- omega
    }
  }

  #------------------------------------------------------------------------------------#
  initial_values <- list(
    initial_method = initial_method,
    # initial values related to the clustering
    phi = phi,
    ppi = ppi,
    alpha = alpha,
    zz = zz,
    # initial values related to the variable selection
    omega = omega,
    ss = ss,
    rho = rho,
    omega_tmp = omega_tmp,
    xi = xi,
    e = e,
    gamma = gamma,
    # initial values for cluster-specific parameters (continuous variables)
    mu = mu,
    sigma2 = sigma2,
    mu_tilde = mu_tilde,
    s2_tilde = s2_tilde,
    a_tilde = a_tilde,
    b_tilde = b_tilde,
    # initial values for cluster-specific parameters (categorical variables)
    theta = theta,
    theta_e = theta_e,
    kappa = kappa,
    # initial values for cluster-specific parameters (count variables)
    lambda = lambda,
    lambda_e = lambda_e,
    u_tilde = u_tilde,
    v_tilde = v_tilde
  )

  # make sure each dataset is a matrix before pass to Rcpp function
  # mydata <- lapply(mydata, function(y) if(is.factor(y)) as.matrix(as.numeric(as.character(y))) else y)
  # str(mydata)

  message(paste(rep("-", 60), sep = "", collapse = ""), "\n")
  message("Running the CAVI algorithm", "\n")
  message(paste(rep("-", 60), sep = "", collapse = ""), "\n")

  #------------------------------------------------------------------------------------#
  # CAVI algorithm parameters
  #------------------------------------------------------------------------------------#
  elbo <- numeric(max_iter)

  #------------------------------------------------------------------------------------#
  # Run CAVI algorithm
  #------------------------------------------------------------------------------------#
  start_time <- Sys.time() # Record start time
  if (VS_method == 0) {
    res_CAVI <- CAVI_algorithm_standard(
      mydata,
      data_summary,
      hyper_parameters,
      initial_values,
      dist,
      K,
      max_iter,
      early_stop,
      per,
      epsilon,
      convergence_threshold
    )
    res_CAVI$algo <- "CAVI_algorithm_standard"
  }


  if (VS_method == 1) {
    res_CAVI <- CAVI_algorithm_global(
      mydata,
      data_summary,
      hyper_parameters,
      initial_values,
      dist,
      K,
      max_iter,
      early_stop,
      per,
      epsilon,
      convergence_threshold
    )
    res_CAVI$algo <- "CAVI_algorithm_global"
  }

  class(res_CAVI) <- "iClusterVB"
  # return results
  res_CAVI
}
