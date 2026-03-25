#' BaHZING_Model Function
#' This function implements the BaHZING model for microbiome data analysis.
#' @import R2jags
#' @import rjags
#' @import pscl
#' @import dplyr
#' @import tidyr
#' @import phyloseq
#' @import stringr
#' @importFrom stats quantile update
#' @importFrom bayestestR p_direction p_rope p_map
#' @param formatted_data An object containing formatted microbiome data.
#' @param x A vector of column names of the exposures.
#' @param covar An optional vector of the column names of covariates.
#' @param n.chains An optional integer specifying the number of parallel chains
#' for the model in jags.model function. Default is 3.
#' @param n.adapt An optional integer specifying the number of iterations for
#' adaptation in jags.model function. Default is 5000.
#' @param n.iter.burnin An optional integer specifying number of iterations in
#' update function. Default is 10000.
#' @param n.iter.sample An optional integer specifying the number of iterations
#' in coda.samples function. Default is 10000.
#' @param exposure_standardization Method for standardizing the exposures.
#' Should be one of "standard_normal" (the default), "quantile", or "none". If
#' "none", exposures are not standardized before analysis, and counterfactual
#' profiles must be specified by the user.
#' @param counterfactual_profiles A 2xP matrix or a vector with length of 2; P
#' is the number of exposures in x. If a 2xP matrix is provided,
#' the effect estimates for the mixture are interpreted as the estimated change
#' in the outcome when changing each exposure p in 1:P is changed from
#' `counterfactual_profiles[1,p]` to `counterfactual_profiles[2,p]`. If a vector of
#' length 2 is provided, the effect estimates for the mixture are interpreted as
#' the estimated change in the outcome when changing each exposure from
#' `counterfactual_profiles[1]` to `counterfactual_profiles[2]`. If
#' exposure_standardization = "standard_normal", then the default is c(-0.5, 0.5),
#' and the effect estimate is calculated based on increasing all exposures in
#' the mixture by one standard deviation. If exposure_standardization = "quantile",
#' then the default is c(0,1), and the effect estimate is calculated based on
#' increasing all exposures in the mixture by one quantile (where the number of
#' quantiles is based on the parameter q).
#' @param q An integer specifying the number of quantiles. Only required if
#' exposure_standardization = "quantile". If exposure_standardization =
#' "quantile" and q is not specified, then a default of q = 4 is used.
#' @param verbose If TRUE (default), function returns information a data quality
#' check.
#' @param return_all_estimates If FALSE (default), results do not include
#' the dispersion and omega estimates from the BaHZING model.
#' @param ROPE_range Region of practical equivalence (ROPE) for calculating
#' p_rope. Default is c(-0.1, 0.1).
#' @return A data frame containing results of the Bayesian analysis, with the
#' following columns:
#' - taxa_full: Full Taxa information, including all levels of the taxonomy.
#' Taxanomic levels are split by two underscores ('__').
#' - taxa_name: Taxa name, which is the last level of the taxonomy.
#' - domain: domain of the taxa.
#' - exposure: Exposure name (either one of  the individual exposures, or the
#' mixture).
#' - component: Zero inflated model estimate or the Count model estimate.
#' - estimate: Point estimate of the posterior distributions.
#' - bci_lcl: 95% Bayesian Credible Interval Lower Limit. Calculated as the
#' equal tailed interval of posterior distributions using the quantiles method.
#' - bci_ucl: 95% Bayesian Credible Interval Upper Limit. Calculated as the
#' equal tailed interval of posterior distributions using the quantiles method.
#' - p_direction: The Probability of Direction, calculated with `bayestestR`. A
#' higher value suggests a higher probability that the estimate is strictly
#' positive or negative. In other words, the closer the value to 1, the higher
#' the probability that the estimate is non-zero. Values can not be less than
#' 50%. From `bayestestR`: also known as the Maximum Probability of Effect
#' (MPE). This can be interpreted as the probability that a parameter (described
#' by its posterior distribution) is strictly positive or negative (whichever
#' is the most probable). Although differently expressed, this index is fairly
#' similar (i.e., is strongly correlated) to the frequentist p-value.
#' - p_rope: The probability that the estimate is not within the Region of
#' practical equivalence (ROPE), calculated with `bayestestR`. The proportion
#' of the whole posterior distribution that doesn't lie within the `ROPE_range`.
#' - p_map: Bayesian equivalent of the p-value, calculated with `bayestestR`.
#'  From `bayestestR`:  p_map is related to the odds that a parameter (described
#'  by its posterior distribution) has against the null hypothesis (h0) using
#'   Mills' (2014, 2017) Objective Bayesian Hypothesis Testing framework. It
#'   corresponds to the density value at the null (e.g., 0) divided by the
#'   density at the Maximum A Posteriori (MAP).
#' @export
#' @name BaHZING_Model

# Declare global variables
globalVariables(c("LibrarySize", "X2.5.", "X97.5.", "Mean",
                         "Exposure.Index", "taxa_index", "taxa_full",
                         "component", "estimate", "bci_lcl", "bci_ucl",
                         "domain", "taxa_name", "pdir","prope","pmap",
                         "name"))

BaHZING_Model <- function(formatted_data,
                          x,
                          covar = NULL,
                          exposure_standardization = NULL,
                          n.chains = 3,
                          n.adapt = 5000,
                          n.iter.burnin = 10000,
                          n.iter.sample = 10000,
                          counterfactual_profiles = NULL,
                          q = NULL,
                          verbose = TRUE,
                          return_all_estimates = FALSE,
                          ROPE_range = c(-0.1, 0.1)) {

  # 1. Check input data ----
  # Extract metadata file from formatted data
  exposure_covar_dat <- data.frame(formatted_data$Table)

  # Create covariate dataframe
  if (!is.null(covar)){
    W <- data.frame(exposure_covar_dat[covar])
    Q <- ncol(W)
  }

  # Create exposure dataframe
  if(!all(x %in% colnames(exposure_covar_dat))) {
    stop("Not all exposured are found in the formatted data")
  }
  X <- exposure_covar_dat[x]
  P <- ncol(X)

  # Set exposure_standardization if missing
  if(is.null(exposure_standardization)) {
    exposure_standardization = "standard_normal"
  }
  # Check exposure_standardization is within the bounds
  if(!(exposure_standardization %in% c("standard_normal", "quantile", "none"))){
    stop("exposure_standardization must be either standard_normal, quantile, or none")
  }

  # Give warning if exposure_standardization == "standard_normal" and q is specified
  if(exposure_standardization == "standard_normal" & !is.null(q)){
    message("Note: q is not required when exposure_standardization is standard_normal. q will be ignored.")
  }

  # If standardization is "none", then check to make sure that counterfactual_profiles is specified
  if(exposure_standardization  == "none" & is.null(counterfactual_profiles)){
    stop("counterfactual_profiles must be speficied if exposure_standardization is none.")
  }

  # 2. Counterfactual profiles -----------------------------------
  ## 2.a Set counterfactual_profiles if missing --------
  if(is.null(counterfactual_profiles)) {
    if(exposure_standardization == "standard_normal") {
      counterfactual_profiles <- c(-0.5,0.5)
    } else {
      counterfactual_profiles <- c(0,1)
    }
  }

  ## 2.b Check counterfactual profiles structure ----
  if (is.matrix(counterfactual_profiles)) { # Checks if matrix
    if (nrow(counterfactual_profiles) != 2) {
      stop("counterfactual_profiles must have 2 rows when provided as a matrix.")
    }
    if (ncol(counterfactual_profiles) != P) {
      stop(paste0("When provided as a matrix, the number of columns in counterfactual_profiles must be equal to the number of exposures in the model."))
    }
    if (!is.numeric(counterfactual_profiles)) {
      stop("counterfactual_profiles must be numeric.")
    }
  } else { # Checks if is vector
    if (is.vector(counterfactual_profiles)) {
      if (!is.numeric(counterfactual_profiles)) {
        stop("counterfactual_profiles must be numeric.")
      }
      if (length(counterfactual_profiles) != 2) {
        stop(paste0("counterfactual_profiles must have 2 elements when provided as a vector."))
      }
    } else {
      stop("counterfactual_profiles must be either a numeric 2xP matrix or a numeric vector with length P.")
    }
  }

  ## 2.c Set profiles --------------------------------
  if(is.matrix(counterfactual_profiles)) {
    profiles = counterfactual_profiles
  } else {
    profiles <- rbind(rep(counterfactual_profiles[1], P),
                      rep(counterfactual_profiles[2], P))
  }

  # Give warning if using qualtiles but counterfactuals < 0
  if(exposure_standardization=="quantiles" &
     any(counterfactual_profiles<0 |
         counterfactual_profiles > q+1)){
    warning("Note: Quantiles are used, but counterfactual_profiles includes values outside of range. Estimates will be calculated based on the specified counterfactual_profiles, but results may not be interpretable.")
  }


  # 3. Scale exposures --------------------------------
  # Set default q if not provided
  if(is.null(q) & exposure_standardization == "quantile") {
    q = 4
  }

  # If using quantiles, quantize X
  if(exposure_standardization=="quantile") {
    probs <- seq(0, 1, length.out = q + 1)
    X.q <- apply(X, 2, function(v) {
      cut(v, breaks = c(-Inf, quantile(v, probs = probs, include.lowest = FALSE)), labels = FALSE)
    })
  }

  #If not quantized and not standardized, scale X
  if(exposure_standardization=="standard_normal") {
    X.q <- scale(X)
  }

  #If none, no scaling
  if(exposure_standardization == "none") {
    X.q <- X
  }

  # 4. Format microbiome matricies ----
  #Create outcome dataframe
  Y <- exposure_covar_dat[, grep("k__", names(exposure_covar_dat))]
  N <- nrow(Y)
  R <- ncol(Y)
  #Genus
  GenusData <- as.data.frame(t(formatted_data$Species.Genus.Matrix))
  Genus.R <- ncol(GenusData)
  numGenusPerSpecies <- as.numeric(apply(GenusData, 1, sum))
  #Family
  FamilyData <- as.data.frame(t(formatted_data$Genus.Family.Matrix))
  Family.R <- ncol(FamilyData)
  numFamilyPerGenus <- as.numeric(apply(FamilyData, 1, sum))
  #Order
  OrderData <- as.data.frame(t(formatted_data$Family.Order.Matrix))
  Order.R <- ncol(OrderData)
  numOrderPerorder <- as.numeric(apply(OrderData, 1, sum))
  #Class
  ClassData <- as.data.frame(t(formatted_data$Order.Class.Matrix))
  Class.R <- ncol(ClassData)
  numClassPerOrder <- as.numeric(apply(ClassData, 1, sum))
  #Phylum
  PhylumData <- as.data.frame(t(formatted_data$Class.Phylum.Matrix))
  Phylum.R <- ncol(PhylumData)
  numPhylumPerClass <- as.numeric(apply(PhylumData, 1, sum))

  ## Create Library Size Offset
  L <- exposure_covar_dat[, grep("k__", names(exposure_covar_dat))]
  L <- L %>%
    mutate(LibrarySize=rowSums(across(everything())))
  L <- L %>%
    select(LibrarySize)

  # 3. Return "Sanity" Messages ----
  if(verbose == TRUE){
    message("#### Checking input data ####")
    message("Exposure and Covariate Data:")
    message(paste0("- Total sample size: ", N))
    message(paste0("- Number of exposures: ", P))

    message("Microbiome Data:")
    message(paste0("- Number of unique genus in data: ",  Genus.R))
    message(paste0("- Number of unique family in data: ", Family.R))
    message(paste0("- Number of unique order in data: ",  Order.R))
    message(paste0("- Number of unique class in data: ",  Class.R))
    message(paste0("- Number of unique phylum in data: ", Phylum.R))

    message("#### Running BaHZING with the following parameters #### ")
    if (exposure_standardization == "standard_normal"){
      message("Exposure standardization: Standard Normal")
    }
    if (exposure_standardization == "none"){
      message("Exposure standardization: None")
    }
    if (exposure_standardization == "quantile"){
      message(paste0("Exposure standardization: Quantiles, with q = ", q))
    }
  }

  # 4. Run Model ----
  if (!is.null(covar)) {
    # Hierarchical Model with covariates----
    BHRM.microbiome <-
      "model {
    for(r in 1:R) {
      for(i in 1:N) {
        Y[i,r] ~ dnegbin(mu[i,r], disp[r])
        mu[i,r] <- disp[r]/(disp[r]+(1-zero[i,r])*lambda[i,r]) - 0.000001*zero[i,r]
        log(lambda[i,r]) <- alpha[r] + inprod(species.beta[r,1:P], X.q[i,1:P]) + inprod(delta[r, 1:Q], W[i,1:Q]) + log(L[i,1])

        # zero-inflation
        zero[i,r] ~ dbern(pi[i,r])
        logit(pi[i,r]) <- alpha.zero[r] + inprod(species.beta.zero[r,1:P], X.q[i,1:P]) + inprod(delta.zero[r, 1:Q], W[i,1:Q]) + log(L[i,1])
      }
      # prior on dispersion parameter
      disp[r] ~ dunif(0,50)

      # prior on intercept
      alpha[r] ~ dnorm(0, 1.0E-02)
      alpha.zero[r] ~ dnorm(0, 1.0E-02)

      # prior on proportion of non-zeros
      omega[r] ~ dunif(0,1)

      # prior on covariate effects
      for(q in 1:Q) {
        delta[r,q] ~ dnorm(0, 1.0E-02)
        delta.zero[r,q] ~ dnorm(0, 1.0E-02)
      }

      # prior on exposure effects
      for(p in 1:P) {
        # species.beta.zero[r,p] ~ dnorm(0, 1.0E-02)
        species.beta[r,p] ~ dnorm(mu.species[r,p], tau[r])
        mu.species[r,p] <- inprod(genus.beta[1:Genus.R,p], GenusData[r,1:Genus.R])
        #Zero inflation component
        species.beta.zero[r,p] ~ dnorm(mu.species.zero[r,p], tau[r])
        mu.species.zero[r,p] <- inprod(genus.beta.zero[1:Genus.R,p], GenusData[r,1:Genus.R])
      }

      # prior on precision
      tau[r] <- 1/(sigma[r]*sigma[r])
      sigma[r] ~ dunif(0,3)

      # g-estimation
      species.eta.low[r] <- inprod(species.beta[r,1:P], profiles[1,1:P])
      species.eta.high[r] <- inprod(species.beta[r,1:P], profiles[2,1:P])
      species.psi[r] <- species.eta.high[r]-species.eta.low[r]
      # zero-inflation
      species.eta.low.zero[r] <- inprod(species.beta.zero[r,1:P], profiles[1,1:P])
      species.eta.high.zero[r] <- inprod(species.beta.zero[r,1:P], profiles[2,1:P])
      species.psi.zero[r] <- species.eta.high.zero[r]-species.eta.low.zero[r]
    }

    # Genus level
    for(g.r in 1:Genus.R) {
      for(p in 1:P) {
        genus.beta[g.r,p] ~ dnorm(mu.family[g.r,p],genus.tau[g.r]) # should this be shared effects across exposures at genus level? or shared effects across all genus by exposure?
        mu.family[g.r,p] <- inprod(family.beta[1:Family.R,p], FamilyData[g.r,1:Family.R])
        #Zero inflation component
        genus.beta.zero[g.r,p] ~ dnorm(mu.family.zero[g.r,p],genus.tau[g.r]) # should this be shared effects across exposures at genus level? or shared effects across all genus by exposure?
        mu.family.zero[g.r,p] <- inprod(family.beta.zero[1:Family.R,p], FamilyData[g.r,1:Family.R])
      }
      # prior on precision
      genus.tau[g.r] <- 1/(genus.sigma[g.r]*genus.sigma[g.r])
      genus.sigma[g.r] ~ dunif(0,3)

      # g-estimation
      genus.eta.low[g.r] <- inprod(genus.beta[g.r,1:P], profiles[1,1:P])
      genus.eta.high[g.r] <- inprod(genus.beta[g.r,1:P], profiles[2,1:P])
      genus.psi[g.r] <- genus.eta.high[g.r]-genus.eta.low[g.r]
      #zero inflation
      genus.eta.low.zero[g.r] <- inprod(genus.beta.zero[g.r,1:P], profiles[1,1:P])
      genus.eta.high.zero[g.r] <- inprod(genus.beta.zero[g.r,1:P], profiles[2,1:P])
      genus.psi.zero[g.r] <- genus.eta.high.zero[g.r]-genus.eta.low.zero[g.r]
    }

    # Family level
    for(f.r in 1:Family.R) {
      for(p in 1:P) {
        family.beta[f.r,p] ~ dnorm(mu.order[f.r,p], family.tau[f.r])
        mu.order[f.r,p] <- inprod(order.beta[1:Order.R,p], OrderData[f.r,1:Order.R])
        #Zero inflation component
        family.beta.zero[f.r,p] ~ dnorm(mu.order.zero[f.r,p], family.tau[f.r])
        mu.order.zero[f.r,p] <- inprod(order.beta.zero[1:Order.R,p], OrderData[f.r,1:Order.R])

      }
      # prior on precision
      family.tau[f.r] <- 1/(family.sigma[f.r]*family.sigma[f.r])
      family.sigma[f.r] ~ dunif(0,3)

      # g-estimation
      family.eta.low[f.r] <- inprod(family.beta[f.r,1:P], profiles[1,1:P])
      family.eta.high[f.r] <- inprod(family.beta[f.r,1:P], profiles[2,1:P])
      family.psi[f.r] <- family.eta.high[f.r]-family.eta.low[f.r]
      #zero inflation
      family.eta.low.zero[f.r] <- inprod(family.beta.zero[f.r,1:P], profiles[1,1:P])
      family.eta.high.zero[f.r] <- inprod(family.beta.zero[f.r,1:P], profiles[2,1:P])
      family.psi.zero[f.r] <- family.eta.high.zero[f.r]-family.eta.low.zero[f.r]
    }

    # Order level
    for(o.r in 1:Order.R) {
      for(p in 1:P) {
        order.beta[o.r,p] ~ dnorm(mu.class[o.r,p], order.tau[o.r])
        mu.class[o.r,p] <- inprod(class.beta[1:Class.R,p], ClassData[o.r,1:Class.R])
        #Zero inflation component
        order.beta.zero[o.r,p] ~ dnorm(mu.class.zero[o.r,p], order.tau[o.r])
        mu.class.zero[o.r,p] <- inprod(class.beta.zero[1:Class.R,p], ClassData[o.r,1:Class.R])
      }
      # prior on precision
      order.tau[o.r] <- 1/(order.sigma[o.r]*order.sigma[o.r])
      order.sigma[o.r] ~ dunif(0,3)

      # g-estimation
      order.eta.low[o.r] <- inprod(order.beta[o.r,1:P], profiles[1,1:P])
      order.eta.high[o.r] <- inprod(order.beta[o.r,1:P], profiles[2,1:P])
      order.psi[o.r] <- order.eta.high[o.r]-order.eta.low[o.r]
      #zero infl
      order.eta.low.zero[o.r] <- inprod(order.beta.zero[o.r,1:P], profiles[1,1:P])
      order.eta.high.zero[o.r] <- inprod(order.beta.zero[o.r,1:P], profiles[2,1:P])
      order.psi.zero[o.r] <- order.eta.high.zero[o.r]-order.eta.low.zero[o.r]
    }

    # Class level
    for(c.r in 1:Class.R) {
      for(p in 1:P) {
        class.beta[c.r,p] ~ dnorm(mu.phylum[c.r,p], class.tau[c.r])
        mu.phylum[c.r,p] <- inprod(phylum.beta[1:Phylum.R,p], PhylumData[c.r,1:Phylum.R])
        #Zero inflation component
        class.beta.zero[c.r,p] ~ dnorm(mu.phylum.zero[c.r,p], class.tau[c.r])
        mu.phylum.zero[c.r,p] <- inprod(phylum.beta.zero[1:Phylum.R,p], PhylumData[c.r,1:Phylum.R])
      }
      # prior on precision
      class.tau[c.r] <- 1/(class.sigma[c.r]*class.sigma[c.r])
      class.sigma[c.r] ~ dunif(0,3)

      # g-estimation
      class.eta.low[c.r] <- inprod(class.beta[c.r,1:P], profiles[1,1:P])
      class.eta.high[c.r] <- inprod(class.beta[c.r,1:P], profiles[2,1:P])
      class.psi[c.r] <- class.eta.high[c.r]-class.eta.low[c.r]

      #zero component
      class.eta.low.zero[c.r] <- inprod(class.beta.zero[c.r,1:P], profiles[1,1:P])
      class.eta.high.zero[c.r] <- inprod(class.beta.zero[c.r,1:P], profiles[2,1:P])
      class.psi.zero[c.r] <- class.eta.high.zero[c.r]-class.eta.low.zero[c.r]
    }

    # Phylum level
    for(p.r in 1:Phylum.R) {
      for(p in 1:P) {
        phylum.beta[p.r,p] ~ dnorm(0, phylum.tau[p.r])
        #Zero inflation component
        phylum.beta.zero[p.r,p] ~ dnorm(0, phylum.tau[p.r])
      }
      # prior on precision
      phylum.tau[p.r] <- 1/(phylum.sigma[p.r]*phylum.sigma[p.r])
      phylum.sigma[p.r] ~ dunif(0,3)

      # g-estimation
      phylum.eta.low[p.r] <- inprod(phylum.beta[p.r,1:P], profiles[1,1:P])
      phylum.eta.high[p.r] <- inprod(phylum.beta[p.r,1:P], profiles[2,1:P])
      phylum.psi[p.r] <- phylum.eta.high[p.r]-phylum.eta.low[p.r]

      #Zero inflation
      phylum.eta.low.zero[p.r] <- inprod(phylum.beta.zero[p.r,1:P], profiles[1,1:P])
      phylum.eta.high.zero[p.r] <- inprod(phylum.beta.zero[p.r,1:P], profiles[2,1:P])
      phylum.psi.zero[p.r] <- phylum.eta.high.zero[p.r]-phylum.eta.low.zero[p.r]
    }

  }"

    # Run JAGs Estimation
    jdata <- list(N=N, Y=Y, R=R, X.q=X.q, W=W, P=P, Q=Q,
                  GenusData=GenusData, Genus.R=Genus.R,
                  Family.R=Family.R, FamilyData=FamilyData,
                  Order.R=Order.R, OrderData=OrderData,
                  Class.R=Class.R, ClassData=ClassData,
                  Phylum.R=Phylum.R, PhylumData=PhylumData,
                  profiles=profiles,L=L)
    var.s <- c("species.beta", "genus.beta", "family.beta", "order.beta",
               "class.beta", "phylum.beta", "species.beta.zero",
               "genus.beta.zero", "family.beta.zero", "order.beta.zero",
               "class.beta.zero", "phylum.beta.zero","species.psi","genus.psi",
               "family.psi","order.psi","class.psi","phylum.psi",
               "species.psi.zero","genus.psi.zero","family.psi.zero",
               "order.psi.zero","class.psi.zero","phylum.psi.zero",
               "omega","disp")
    model.fit <- jags.model(file=textConnection(BHRM.microbiome), data=jdata, n.chains=n.chains, n.adapt=n.adapt, quiet=F)
    update(model.fit, n.iter=n.iter.burnin, progress.bar="text")
    model.fit <- coda.samples(model=model.fit, variable.names=var.s, n.iter=n.iter.sample, thin=1, progress.bar="text")

  } else {
    # Hierarchical Model without Covariates----
    BHRM.microbiome <-
      "model {
    for(r in 1:R) {
      for(i in 1:N) {
        Y[i,r] ~ dnegbin(mu[i,r], disp[r])
        mu[i,r] <- disp[r]/(disp[r]+(1-zero[i,r])*lambda[i,r]) - 0.000001*zero[i,r]
        log(lambda[i,r]) <- alpha[r] + inprod(species.beta[r,1:P], X.q[i,1:P]) + log(L[i,1])

        # zero-inflation
        zero[i,r] ~ dbern(pi[i,r])
        logit(pi[i,r]) <- alpha.zero[r] + inprod(species.beta.zero[r,1:P], X.q[i,1:P]) + log(L[i,1])
      }
      # prior on dispersion parameter
      disp[r] ~ dunif(0,50)

      # prior on intercept
      alpha[r] ~ dnorm(0, 1.0E-02)
      alpha.zero[r] ~ dnorm(0, 1.0E-02)

      # prior on proportion of non-zeros
      omega[r] ~ dunif(0,1)

      # # prior on covariate effects
      # for(q in 1:Q) {
      #   delta[r,q] ~ dnorm(0, 1.0E-02)
      #   delta.zero[r,q] ~ dnorm(0, 1.0E-02)
      # }

      # prior on exposure effects
      for(p in 1:P) {
        # species.beta.zero[r,p] ~ dnorm(0, 1.0E-02)
        species.beta[r,p] ~ dnorm(mu.species[r,p], tau[r])
        mu.species[r,p] <- inprod(genus.beta[1:Genus.R,p], GenusData[r,1:Genus.R])
        #Zero inflation component
        species.beta.zero[r,p] ~ dnorm(mu.species.zero[r,p], tau[r])
        mu.species.zero[r,p] <- inprod(genus.beta.zero[1:Genus.R,p], GenusData[r,1:Genus.R])
      }

      # prior on precision
      tau[r] <- 1/(sigma[r]*sigma[r])
      sigma[r] ~ dunif(0,3)

      # g-estimation
      species.eta.low[r] <- inprod(species.beta[r,1:P], profiles[1,1:P])
      species.eta.high[r] <- inprod(species.beta[r,1:P], profiles[2,1:P])
      species.psi[r] <- species.eta.high[r]-species.eta.low[r]
      # zero-inflation
      species.eta.low.zero[r] <- inprod(species.beta.zero[r,1:P], profiles[1,1:P])
      species.eta.high.zero[r] <- inprod(species.beta.zero[r,1:P], profiles[2,1:P])
      species.psi.zero[r] <- species.eta.high.zero[r]-species.eta.low.zero[r]
    }

    # Genus level
    for(g.r in 1:Genus.R) {
      for(p in 1:P) {
        genus.beta[g.r,p] ~ dnorm(mu.family[g.r,p],genus.tau[g.r]) # should this be shared effects across exposures at genus level? or shared effects across all genus by exposure?
        mu.family[g.r,p] <- inprod(family.beta[1:Family.R,p], FamilyData[g.r,1:Family.R])
        #Zero inflation component
        genus.beta.zero[g.r,p] ~ dnorm(mu.family.zero[g.r,p],genus.tau[g.r]) # should this be shared effects across exposures at genus level? or shared effects across all genus by exposure?
        mu.family.zero[g.r,p] <- inprod(family.beta.zero[1:Family.R,p], FamilyData[g.r,1:Family.R])
      }
      # prior on precision
      genus.tau[g.r] <- 1/(genus.sigma[g.r]*genus.sigma[g.r])
      genus.sigma[g.r] ~ dunif(0,3)

      # g-estimation
      genus.eta.low[g.r] <- inprod(genus.beta[g.r,1:P], profiles[1,1:P])
      genus.eta.high[g.r] <- inprod(genus.beta[g.r,1:P], profiles[2,1:P])
      genus.psi[g.r] <- genus.eta.high[g.r]-genus.eta.low[g.r]
      #zero inflation
      genus.eta.low.zero[g.r] <- inprod(genus.beta.zero[g.r,1:P], profiles[1,1:P])
      genus.eta.high.zero[g.r] <- inprod(genus.beta.zero[g.r,1:P], profiles[2,1:P])
      genus.psi.zero[g.r] <- genus.eta.high.zero[g.r]-genus.eta.low.zero[g.r]
    }

    # Family level
    for(f.r in 1:Family.R) {
      for(p in 1:P) {
        family.beta[f.r,p] ~ dnorm(mu.order[f.r,p], family.tau[f.r])
        mu.order[f.r,p] <- inprod(order.beta[1:Order.R,p], OrderData[f.r,1:Order.R])
        #Zero inflation component
        family.beta.zero[f.r,p] ~ dnorm(mu.order.zero[f.r,p], family.tau[f.r])
        mu.order.zero[f.r,p] <- inprod(order.beta.zero[1:Order.R,p], OrderData[f.r,1:Order.R])

      }
      # prior on precision
      family.tau[f.r] <- 1/(family.sigma[f.r]*family.sigma[f.r])
      family.sigma[f.r] ~ dunif(0,3)

      # g-estimation
      family.eta.low[f.r] <- inprod(family.beta[f.r,1:P], profiles[1,1:P])
      family.eta.high[f.r] <- inprod(family.beta[f.r,1:P], profiles[2,1:P])
      family.psi[f.r] <- family.eta.high[f.r]-family.eta.low[f.r]
      #zero inflation
      family.eta.low.zero[f.r] <- inprod(family.beta.zero[f.r,1:P], profiles[1,1:P])
      family.eta.high.zero[f.r] <- inprod(family.beta.zero[f.r,1:P], profiles[2,1:P])
      family.psi.zero[f.r] <- family.eta.high.zero[f.r]-family.eta.low.zero[f.r]
    }

    # Order level
    for(o.r in 1:Order.R) {
      for(p in 1:P) {
        order.beta[o.r,p] ~ dnorm(mu.class[o.r,p], order.tau[o.r])
        mu.class[o.r,p] <- inprod(class.beta[1:Class.R,p], ClassData[o.r,1:Class.R])
        #Zero inflation component
        order.beta.zero[o.r,p] ~ dnorm(mu.class.zero[o.r,p], order.tau[o.r])
        mu.class.zero[o.r,p] <- inprod(class.beta.zero[1:Class.R,p], ClassData[o.r,1:Class.R])
      }
      # prior on precision
      order.tau[o.r] <- 1/(order.sigma[o.r]*order.sigma[o.r])
      order.sigma[o.r] ~ dunif(0,3)

      # g-estimation
      order.eta.low[o.r] <- inprod(order.beta[o.r,1:P], profiles[1,1:P])
      order.eta.high[o.r] <- inprod(order.beta[o.r,1:P], profiles[2,1:P])
      order.psi[o.r] <- order.eta.high[o.r]-order.eta.low[o.r]
      #zero infl
      order.eta.low.zero[o.r] <- inprod(order.beta.zero[o.r,1:P], profiles[1,1:P])
      order.eta.high.zero[o.r] <- inprod(order.beta.zero[o.r,1:P], profiles[2,1:P])
      order.psi.zero[o.r] <- order.eta.high.zero[o.r]-order.eta.low.zero[o.r]
    }

    # Class level
    for(c.r in 1:Class.R) {
      for(p in 1:P) {
        class.beta[c.r,p] ~ dnorm(mu.phylum[c.r,p], class.tau[c.r])
        mu.phylum[c.r,p] <- inprod(phylum.beta[1:Phylum.R,p], PhylumData[c.r,1:Phylum.R])
        #Zero inflation component
        class.beta.zero[c.r,p] ~ dnorm(mu.phylum.zero[c.r,p], class.tau[c.r])
        mu.phylum.zero[c.r,p] <- inprod(phylum.beta.zero[1:Phylum.R,p], PhylumData[c.r,1:Phylum.R])
      }
      # prior on precision
      class.tau[c.r] <- 1/(class.sigma[c.r]*class.sigma[c.r])
      class.sigma[c.r] ~ dunif(0,3)

      # g-estimation
      class.eta.low[c.r] <- inprod(class.beta[c.r,1:P], profiles[1,1:P])
      class.eta.high[c.r] <- inprod(class.beta[c.r,1:P], profiles[2,1:P])
      class.psi[c.r] <- class.eta.high[c.r]-class.eta.low[c.r]

      #zero component
      class.eta.low.zero[c.r] <- inprod(class.beta.zero[c.r,1:P], profiles[1,1:P])
      class.eta.high.zero[c.r] <- inprod(class.beta.zero[c.r,1:P], profiles[2,1:P])
      class.psi.zero[c.r] <- class.eta.high.zero[c.r]-class.eta.low.zero[c.r]
    }

    # Phylum level
    for(p.r in 1:Phylum.R) {
      for(p in 1:P) {
        phylum.beta[p.r,p] ~ dnorm(0, phylum.tau[p.r])
        #Zero inflation component
        phylum.beta.zero[p.r,p] ~ dnorm(0, phylum.tau[p.r])
      }
      # prior on precision
      phylum.tau[p.r] <- 1/(phylum.sigma[p.r]*phylum.sigma[p.r])
      phylum.sigma[p.r] ~ dunif(0,3)

      # g-estimation
      phylum.eta.low[p.r] <- inprod(phylum.beta[p.r,1:P], profiles[1,1:P])
      phylum.eta.high[p.r] <- inprod(phylum.beta[p.r,1:P], profiles[2,1:P])
      phylum.psi[p.r] <- phylum.eta.high[p.r]-phylum.eta.low[p.r]

      #Zero inflation
      phylum.eta.low.zero[p.r] <- inprod(phylum.beta.zero[p.r,1:P], profiles[1,1:P])
      phylum.eta.high.zero[p.r] <- inprod(phylum.beta.zero[p.r,1:P], profiles[2,1:P])
      phylum.psi.zero[p.r] <- phylum.eta.high.zero[p.r]-phylum.eta.low.zero[p.r]
    }

  }"

    # Run JAGs Estimation
    jdata <- list(N=N, Y=Y, R=R, X.q=X.q, P=P,
                  GenusData=GenusData, Genus.R=Genus.R,
                  Family.R=Family.R, FamilyData=FamilyData,
                  Order.R=Order.R, OrderData=OrderData,
                  Class.R=Class.R, ClassData=ClassData,
                  Phylum.R=Phylum.R, PhylumData=PhylumData,
                  profiles=profiles,L=L)
    var.s <- c("species.beta", "genus.beta", "family.beta", "order.beta",
               "class.beta", "phylum.beta", "species.beta.zero",
               "genus.beta.zero", "family.beta.zero", "order.beta.zero",
               "class.beta.zero", "phylum.beta.zero","species.psi","genus.psi",
               "family.psi","order.psi","class.psi","phylum.psi",
               "species.psi.zero","genus.psi.zero","family.psi.zero",
               "order.psi.zero","class.psi.zero","phylum.psi.zero",
               "omega","disp")
    model.fit <- jags.model(file=textConnection(BHRM.microbiome), data=jdata,
                            n.chains=n.chains, n.adapt=n.adapt, quiet=F)
    update(model.fit, n.iter=n.iter.burnin, progress.bar="text")
    model.fit <- coda.samples(model=model.fit, variable.names=var.s,
                              n.iter=n.iter.sample, thin=1, progress.bar="text")
  }

  # 5. summarize results -------------------------------------------------------
  ## Calculate Mean, SD, and quantiles ----
  r <- summary(model.fit)
  results <- data.frame(round(r$statistics[,1:2],3), round(r$quantiles[,c(1,5)],3))

  ## Calculate HPD intervals (not included in current version) ------
  # x1 <- HPDinterval(model.fit[[1]], prob = 0.95)  %>% as.data.frame()

  ## Calculate p-values  ------
  post_dist <-  as.data.frame(model.fit[[1]])[, grep("beta|zero|psi", colnames(model.fit[[1]]))]

  ### p_direction ----
  pdir <- apply(post_dist, 2, function(x){
    p_direction(x = x, threshold = 0.05) %>%
      as.numeric()})

  ### p_rope ----
  prope <- apply(post_dist, 2, function(x){
    p_rope(x = x, rope = ROPE_range)$p_ROPE})

  ### p_map ----
  pmap <- apply(post_dist, 2, function(x){
    p_map(x = x) %>%
      as.numeric()})

  # Get dataframe of p-values
  p_value_df <- data.frame(name = names(pdir),
                           pdir = pdir,
                           prope = prope,
                           pmap = pmap)

  # # "Significant" example
  # bayestestR::describe_posterior(post_dist$`family.beta[1,1]`)
  # (pdirection <- bayestestR::p_direction(post_dist$`family.beta[1,1]`, as_p = TRUE)) %>% as.numeric()
  # (prope <- bayestestR::p_rope(post_dist$`family.beta[1,1]`, rope = c(-0.1, 0.1), as_p = TRUE) %>% as.numeric())
  # (pmap <- bayestestR::p_map(post_dist$`family.beta[1,1]`)) %>% as.numeric()
  # (ps <- bayestestR::p_significance(post_dist$`family.beta[1,1]`, rope = c(-0.1, 0.1), as_p = TRUE))
  #
  # # "Non-significant" example
  # bayestestR::describe_posterior(post_dist$`species.beta[189,2]`)
  # bayestestR::p_direction(post_dist$`species.beta[189,2]`, as_p = TRUE)
  # bayestestR::p_rope(post_dist$`species.beta[189,2]`, rope = c(-0.1, 0.1))

  ## Create "component" variable ----
  results <- results %>%
    mutate(
      component=case_when(
        grepl("zero",rownames(.)) ~ "Zero-inflation model coefficients",
        grepl("beta",rownames(.)) ~ "Count model coefficients",
        grepl("psi",rownames(.))  ~ "Count model coefficients",
        grepl("disp",rownames(.)) ~ "Dispersion",
        grepl("omega",rownames(.)) ~ "Omega",
        TRUE ~ "Other"))

  # Calculate significance based on Bayesian Interval, rename variables (removed- jg 02_13_25 in place of p-values)
  results <- results %>%
    # mutate(bci_inc_zero = case_when(
    #   grepl("disp",rownames(.)) ~ NA_character_,
    #   grepl("omega",rownames(.)) ~ NA_character_,
    #   (X2.5.<0 & X97.5.<0) | (X2.5.>0 & X97.5.>0) ~ "BCI ",
    #   TRUE ~ "N.S.")) %>%
    rename(estimate = Mean,
           bci_lcl = X2.5.,
           bci_ucl = X97.5.)

  # Combine results df with p-value df
  results$name = rownames(results)
  results2 <- left_join(results, p_value_df, by = "name")
  rownames(results2) <- results2$name
  results2 <- results2 %>% select(-name)

  # Calculate odds ratios-- removed --
  # results2 <- results2 %>%
  #   mutate(OR=exp(Mean),
  #          OR.ll=exp(X2.5.),
  #          OR.ul=exp(X97.5.))

  #Format output names
  phylum   <- colnames(PhylumData)
  class    <- colnames(ClassData)
  order    <- colnames(OrderData)
  family   <- colnames(FamilyData)
  genus    <- colnames(GenusData)
  species  <- colnames(Y)
  exposure <- colnames(X)
  results2$taxa_index <- str_remove(rownames(results2),"..*\\[")
  results2$taxa_index <- str_remove(results2$taxa_index,",.*$")
  results2$taxa_index <- str_remove(results2$taxa_index,"]")
  results2$taxa_index <- as.numeric(results2$taxa_index)
  results2$Exposure.Index <- str_remove(rownames(results2),"..*\\,")
  results2 <- results2 %>%
    mutate(Exposure.Index=ifelse(
      grepl("disp",Exposure.Index)|
        grepl("omega",Exposure.Index)|
        grepl("psi",Exposure.Index),
      NA,Exposure.Index))
  results2$Exposure.Index <- str_remove(results2$Exposure.Index,"]")
  results2$Exposure.Index <- as.numeric(results2$Exposure.Index)


  results2 <- results2 %>%
    mutate(taxa_full=case_when(
      grepl("phylum",rownames(results2)) ~ paste0(phylum[taxa_index]),
      grepl("class"  ,rownames(results2)) ~ paste0(class[taxa_index]),
      grepl("order"  ,rownames(results2)) ~ paste0(order[taxa_index]),
      grepl("family" ,rownames(results2)) ~ paste0(family[taxa_index]),
      grepl("genus"  ,rownames(results2)) ~ paste0(genus[taxa_index]),
      grepl("species",rownames(results2)) ~ paste0(species[taxa_index])),
      domain=case_when(
        grepl("phylum" ,rownames(results2))  ~ "Phylum",
        grepl("class"  ,rownames(results2))   ~ "Class",
        grepl("order"  ,rownames(results2))   ~ "Order",
        grepl("family" ,rownames(results2))  ~ "Family",
        grepl("genus"  ,rownames(results2))   ~ "Genus",
        grepl("species",rownames(results2)) ~ "Species"),
      exposure=paste0(exposure[Exposure.Index]))

  # Modify Exposure variable
  results2 <- results2 %>%
    mutate(exposure=case_when(
      grepl("psi",rownames(.)) ~ "Mixture",
      grepl("disp",rownames(.)) ~ "Dispersion",
      grepl("omega",rownames(.)) ~ "Omega",
      TRUE ~ exposure))

  # Get Taxa and domain information for
  results2 <- results2 %>%
    mutate(taxa_full=ifelse(grepl("disp", rownames(results2)),paste0(species[taxa_index]),taxa_full),
           taxa_full=ifelse(grepl("omega",rownames(results2)),paste0(species[taxa_index]),taxa_full),
           taxa_name = sub(".*__", "", taxa_full),
           domain = ifelse(exposure == "Dispersion" | exposure == "Omega",
                           "Species", domain))

  # Remove "disp" and "omega" estimates
  if(!return_all_estimates){
    results2 <- results2 %>%
      filter(!grepl("disp",rownames(results2)),
             !grepl("omega",rownames(results2)))
  }

  # Remove rownames
  rownames(results2) <- NULL

  # Select final variables
  results2 <- results2 %>%
    select(taxa_full, taxa_name, domain, exposure,component,
           estimate,bci_lcl,bci_ucl,pdir,prope,pmap)

  return(results2)
}

