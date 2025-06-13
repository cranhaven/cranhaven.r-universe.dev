#######################################################
#	apc package
#	Zoe Fannon, Bent Nielsen, 21 August 2020, version 1.3.6
#	Code for analyzing cross sectional and panel data
#	code written by Zoe Fannon, added to package by Bent Nielsen
#######################################################
#	Copyright 2020 Zoe Fannon, Bent Nielsen
#	Nuffield College, OX1 1NF, UK
#	bent.nielsen@nuffield.ox.ac.uk
#
#	This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#######################################################

#######################################################
#######################################################
#######################################################
# GROUP 1 Estimation
# Documented in apc.indiv.est.model.Rd	
#######################################################
#######################################################
#######################################################

apc.indiv.design.collinear <- function(data, unit=1, 
                                       n.coh.excl.start=0, n.coh.excl.end=0,
                                       n.per.excl.start=0, n.per.excl.end=0,
                                       n.age.excl.start=0, n.age.excl.end=0){
  # BN, 27 aug 2020: changed cell.name from character to factor
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function generates a collinear design matrix which encompasses all
  # possible APC submodels and covariate combinations.
  # ---------------------------------------------------------------------------
  
  ## Step 1: Checks ####
  
  # check data has appropriate variable names
  if(isTRUE(("age" %in% colnames(data)) 
            && ("period" %in% colnames(data))
            && ("cohort" %in% colnames(data))) == FALSE)
    stop("apc.error: data missing one of age, period, cohort")
  
  # add cell.name
  if (!"cell.name" %in% colnames(data)) {
    cell.name <- paste("ik", as.character(data$age), as.character(data$cohort),
                       sep="_")
	cell.name <- factor(cell.name)	# BN, 27 aug 2020, turn to factor				   
    data <- cbind(cell.name, data)
  } else {		# BN 27 aug 2020, added function name to warning
    warning("apc.indiv.design.collinear: Variable cell.name must be unique identifier of age-cohort cells; 
            if preexisting variable cell.name has alternate meaning please 
            rename")
  }
  
  ## Step 2: Get data dimensions and mu.index ####
  
  # get data minimum values 
  # first two used in construction of i.value, k.value
  age1 <- min(data$age   )
  coh1 <- min(data$cohort)
  # third used only in naming later
  per1 <- min(data$period)
  
  # use minimum values to construct i, j, and k
  i.value <- (data$age - age1)/unit + 1 + n.age.excl.start
  k.value <- (data$cohort - coh1)/unit + 1 + n.coh.excl.start
  j.value <- i.value + k.value - 1 + n.per.excl.start
  
  # get unique combinations of i, j, and k; i.e. list of ik cells
  data.wijk <- cbind(data, i.value, j.value, k.value)
  mu.index <- plyr::count(data.wijk, c("i.value", "j.value", "k.value"))
  mu.index <- mu.index[mu.index$freq>0, ]
  mu.index <- mu.index[, !names(mu.index)=="freq"]
  
  # define L and U: anchor points of plane
  L <- min(j.value) - 1     # correct bc this is the minimum observed j
  U <- as.integer((L+3)/2)
  
  # stop if something has gone wrong here
  stopifnot(exists("L")==TRUE)  
  stopifnot(exists("U")==TRUE)
  
  # get dimensionality of data
  I <- as.numeric(length(unique(data.wijk$i.value)))
  J <- as.numeric(length(unique(data.wijk$j.value)))
  K <- as.numeric(length(unique(data.wijk$k.value)))
  
  ## Step 3: create and fill design matrix - one row per ik cell ####
  
  #create space of design matrix
  n.rows <- nrow(mu.index)
  n.param.design <- I+J+K-2 # -2 rather than -3 because we allow for a period
  # slope; in the submodels with P and tP, it's easier to estimate a period 
  # slope than an equality constraint
  design <- matrix(data=0, nrow=n.rows, ncol=n.param.design+3)
  # the extra three are for index: i, j, k
  
  #begin by defining L.odd
  L.odd <- !L %% 2 == 0 
  
  #fill row-by-row
  for (row in 1:n.rows) {
    i <- mu.index[row, 1]
    j <- mu.index[row, 2]
    k <- mu.index[row, 3]
    design[row, 1] <- 1 # the intercept
    design[row, 2] <- i - U # the first (age) slope
    design[row, 4] <- k - U # the second (cohort) slope
    design[row, 3] <- design[row, 2] + design[row, 4] # period slope
    
    #age DDs
    # note 4 plane pieces => start at 4+1
    if (i < U) 
      design[row, (4 + i - n.age.excl.start):
               (4 + U - 1 - n.age.excl.start)] <- seq(1, U - i)
    # backward cumulation from DD3 to DD(U-1)
    if (i > U + 1) 
      design[row, (4 + U - n.age.excl.start):
               (4 + i - 2 - n.age.excl.start)] <- seq(i - U - 1, 1)
    # forward cumulation from DDU to DDI
    # note DDI is only the (I-2)nd column of DDs b/c start at DD3
    
    #period DDs
    # note 4 plane pieces + I-2 age DDs => start at 2+I+1
    # CASE 1: L odd
    if (L.odd && j == (L + 1))
      design[row, (2 + I + 1)] <- 1
    # single backward cumulation of DD(L+3)
    if (L.odd && j > (L + 3))
      design[row, (2 + I + 1 + L.odd):(2 + I + L.odd + j - (L + 3))] <-
      seq(j - (L + 3), 1)
    # forward cumulation from DD(L+4) to DD(L+J)
    # note DD(L+J) is the (J-3)rd column of DDs here b/c start at DD(L+4);
    # recall DD(L+3) already covered with backward cumulation
    # CASE 2: L even
    if ((!L.odd) && j > (L + 2))
      design[row, (2 + I + 1):(2 + I + j - (L + 2))] <- seq(j - (L + 2), 1)
    # forward cumulation from DD(L+3) to DD(L+J)
    # DD(L+J) is the (J-2)nd column of DDs here b/c start at DD(L+3)
    
    #cohort DDs
    # note 4 plane pieces + I-2 age + J-2 period DDs => start at I+J+1
    # also note symmetry to age DDs
    if (k < U) 
      design[row, (I + J + k - n.coh.excl.start):
               (I + J + U - 1 - n.coh.excl.start)] <- seq(1, U - k)
    # backward cumulation from DD3 to DD(U-1)
    if (k > U + 1) 
      design[row, (I + J + U - n.coh.excl.start):
               (I + J + k - 2 - n.coh.excl.start)] <- seq(k - U - 1, 1)
    # forward cumulation from DDU to DDI
    # note DDI is only the (I-2)nd column of DDs b/c start at DD3
    
    #index
    design[row, n.param.design+1] <- i
    design[row, n.param.design+2] <- j
    design[row, n.param.design+3] <- k
  }
  
  # convert matrix -> data frame
  designdf <- as.data.frame(design)
  
  # assign names to variables in design dataframe
  firstfour <- c("level", "age_slope", "period_slope", "cohort_slope")
  age.names <- paste("DD_age", seq(age1+2*unit, age1+2*unit+(I-2)*unit-1, 
                                   unit), sep="_")
  per.names <- paste("DD_period", seq(per1+2*unit, per1+2*unit+(J-2)*unit-1, 
                                      unit), sep="_")
  coh.names <- paste("DD_cohort", seq(coh1+2*unit, coh1+2*unit+(K-2)*unit-1, 
                                      unit), sep="_")
  index <- c("i.value", "j.value", "k.value")
  colnames(designdf) <- c(firstfour, age.names, per.names, coh.names, index)
  
  ## Step 4: join design matrix with full data using ijk index values ####
  
  # associates APC design matrix with each individual datapoint as needed
  data.with.design <- join(data.wijk, designdf, by=c("i.value", "j.value", 
                                                     "k.value"))													 
  
  # drop information that is redundant in future commands
  exclude.all <- c("i.value", "j.value", "k.value")
  first.exclusion <- names(data.with.design) %in% c(exclude.all)
  full.design.collinear <- data.with.design[!first.exclusion]
  
  # check join was successful
  if (anyNA(full.design.collinear)) 
    warning("NA in design matrix, check APC issues with n.excl age/per/coh,
            empty age-cohort cells, or NA in covariates")
  
  # ---------------------------------------------------------------------------
  # Return valuables
  valuables <- list(full.design.collinear = full.design.collinear, 
                    unit = unit,
                    age1 = age1,
                    per1 = per1,
                    coh1 = coh1,
                    age.max = I,
                    per.max = J,
                    coh.max = K,
                    per.zero = L,
                    per.odd = L.odd,
                    U = U)
  
  return (valuables)
  }

apc.indiv.design.model <- function(apc.indiv.design.collinear, 
                                   model.design = "APC",
                                   dep.var = NULL, covariates = NULL,
                                   plmmodel = "notplm", wt.var = NULL,
                                   id.var = NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function reduces the collinear design matrix from 
  # apc.indiv.design.collinear to the design matrix for the desired model
  # ---------------------------------------------------------------------------
  
  # Step 1: checks before loading apc.indiv.design.collinear
  # check model design
  design.list <- c("APC", "AP", "AC", "PC", "Ad", "Pd", "Cd", "A",
                   "P", "C", "t", "tA", "tP", "tC", "1",
                   "FAP", "FA", "FP", "Ft")
  if (!isTRUE(model.design %in% design.list))
    stop("model.design not recognised")
  
  within.design.list <- c("FAP", "FA", "FP", "Ft")
  if (plmmodel == "within" & !isTRUE(model.design %in% within.design.list))
    stop("model.design not suitable for within model")
  if (!(plmmodel == "within") & isTRUE(model.design %in% within.design.list))
    stop("chosen model.design only suitable for within model")
  
  # check plm
  if(!isTRUE(plmmodel %in% c("notplm", "random", "within", "pooling")))
    stop("plmmodel not recognised")
  
  if(isTRUE(plmmodel %in% c("random", "within", "pooling")) & 
     is.null(id.var))
    stop("must specify id.var for panel data")
  
  # check model feasibility
  if(plmmodel == "random" & is.null(covariates) &
     model.design %in% c("C", "tC", "1"))
    stop("Random effects requires some within-individual variation. 
         Add covariates or choose alternative APC model.")
  
  if(!plmmodel == "notplm" & !is.null(wt.var))
    stop("weights not currently implemented for panel data")
  
  # check variable names for blanks
  variables <- c(dep.var, covariates)
  if(sum(grepl(" ", variables))>0)
    stop("variable names cannot contain ' ', please replace with '_'")
  
  ##################
  # Step 2: Setup
  # Extract elements from apc.indiv.design.collinear
  full.design.collinear <- 
    apc.indiv.design.collinear$full.design.collinear
  
  # get double differences
  set.coh.DDs <- colnames(full.design.collinear)[grep(
    "^DD_cohort_", colnames(full.design.collinear))]
  set.age.DDs <- colnames(full.design.collinear)[grep(
    "^DD_age_", colnames(full.design.collinear))]
  set.per.DDs <- colnames(full.design.collinear)[grep(
    "^DD_period_", colnames(full.design.collinear))]
  
  # check covariates
  missing.covariates <- covariates[!covariates %in% 
                                     names(full.design.collinear)]
  if(!length(missing.covariates)==0)
    stop("some listed covariates are not in dataset")
  ##################
  # Step 2: Select model inclusions
  
  # intercept
  if(plmmodel %in% c("notplm", "pooling", "random")) { intercept <- TRUE  }
  if(plmmodel %in% c("within"))                      { intercept <- FALSE }
  
  # slopes and double differences
  if(model.design=="APC")	{	slopes <- c(1,0,1); difdif <- c(1,1,1);	}
  if(model.design=="AP" )	{	slopes <- c(1,0,1); difdif <- c(1,1,0);	}
  if(model.design=="AC" )	{	slopes <- c(1,0,1); difdif <- c(1,0,1);	}
  if(model.design=="PC" )	{	slopes <- c(1,0,1); difdif <- c(0,1,1);	}
  if(model.design=="Ad" )	{	slopes <- c(1,0,1); difdif <- c(1,0,0);	}
  if(model.design=="Pd" )	{	slopes <- c(1,0,1); difdif <- c(0,1,0);	}
  if(model.design=="Cd" )	{	slopes <- c(1,0,1); difdif <- c(0,0,1);	}
  if(model.design=="A"  )	{	slopes <- c(1,0,0); difdif <- c(1,0,0);	}
  if(model.design=="P"  )	{	slopes <- c(0,1,0); difdif <- c(0,1,0);	}
  if(model.design=="C"  )	{	slopes <- c(0,0,1); difdif <- c(0,0,1);	}
  if(model.design=="t"  )	{	slopes <- c(1,0,1); difdif <- c(0,0,0);	}
  if(model.design=="tA" )	{	slopes <- c(1,0,0); difdif <- c(0,0,0);	}
  if(model.design=="tP" )	{	slopes <- c(0,1,0); difdif <- c(0,0,0);	}
  if(model.design=="tC" )	{	slopes <- c(0,0,1); difdif <- c(0,0,0);	}
  if(model.design=="1"  )	{	slopes <- c(0,0,0); difdif <- c(0,0,0);	}
  
  if(model.design=="FAP") {	slopes <- c(1,0,0); difdif <- c(1,1,0);	}
  if(model.design=="FA" ) {	slopes <- c(1,0,0); difdif <- c(1,0,0);	}
  if(model.design=="FP" ) {	slopes <- c(1,0,0); difdif <- c(0,1,0);	}
  if(model.design=="Ft" ) {	slopes <- c(1,0,0); difdif <- c(0,0,0);	}
  
  # set up incl, the list of APC variables to call in this model
  incl <- vector(mode="character")
  if (slopes[1]) {incl <- c(incl, "age_slope")}
  if (slopes[2]) {incl <- c(incl, "period_slope")}
  if (slopes[3]) {incl <- c(incl, "cohort_slope")}
  if (difdif[1]) {incl <- c(incl, set.age.DDs)}
  if (difdif[2]) {incl <- c(incl, set.per.DDs)}
  if (difdif[3]) {incl <- c(incl, set.coh.DDs)}
  
  # Get dimension of included elements of xi, used in later functions
  xi.dim <- length(incl)
  # address the fact that most models automatically include an intercept
  if(intercept) {xi.dim <- xi.dim + 1} 
  
  # Produce APC part as formula for use with lm(), plm(), etc
  APC_string       <- paste(incl, collapse = " + ")
  
  # get final design matrix including APC variables and covariates
  full.inclusion <- names(full.design.collinear) %in% c(incl, covariates)
  full.inclusion.names <- names(full.design.collinear)[full.inclusion]
  final.design <- full.design.collinear[full.inclusion]
  
  # get the dependent variable and the model formula
  if (is.null(dep.var)){
    warning("Dependent variable unspecified, model formula not produced")
    DV            <- dep.var   # ie assign NULL
    model_formula <- NULL
    if(model.design == "1") {
      model_string  <- paste(c("1", full.inclusion.names), collapse = " + ")
    } else {
      model_string  <- paste(full.inclusion.names, collapse = " + ")
    }
  } else {
    DV              <- full.design.collinear[dep.var]
    if(model.design == "1") {
      RHS             <- paste(c("1", full.inclusion.names), collapse = " + ")
    } else {
      RHS             <- paste(full.inclusion.names, collapse = " + ")
    }
    model_string    <- paste(dep.var, RHS, sep = " ~ ") 
    model_formula   <- as.formula(model_string)
  }
  
  ID <- NULL; PER <- NULL; WT <- NULL
  
  # get auxiliary variables for different specifications
  if (plmmodel %in% c("within", "random", "pooling")){
    ID     <- full.design.collinear[id.var]
    PER    <- full.design.collinear["period"]
  }
  if (!is.null(wt.var)){
    WT     <- full.design.collinear[wt.var]    
  }
  ##################
  # Return valuables
  valuables <- list(full.design = final.design,
                    DV = DV,
                    ID = ID,
                    PER = PER,
                    WT = WT,
                    intercept = intercept,
                    slopes = slopes,
                    difdif = difdif,
                    xi.dim = xi.dim,
                    model.design  = model.design,
                    model.string  = model_string,
                    model.formula = model_formula,
                    plmmodel = plmmodel,
                    
                    unit = apc.indiv.design.collinear$unit,
                    age1 = apc.indiv.design.collinear$age1,
                    per1 = apc.indiv.design.collinear$per1,
                    coh1 = apc.indiv.design.collinear$coh1,
                    age.max = apc.indiv.design.collinear$age.max,
                    per.max = apc.indiv.design.collinear$per.max,
                    coh.max = apc.indiv.design.collinear$coh.max,
                    per.zero = apc.indiv.design.collinear$per.zero,
                    per.odd = apc.indiv.design.collinear$per.odd,
                    U = apc.indiv.design.collinear$U)
  return(valuables)
}

apc.indiv.fit.model <- function (apc.indiv.design.model, 
                                 model.family="gaussian",
                                 DV=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function performs estimation following construction of the design 
  # matrix using apc.indiv.design.model
  # ---------------------------------------------------------------------------
  
  # Step 1: Set up
  # Check model.design
  family.list <- c("gaussian", "binomial")
  if (!isTRUE(model.family %in% family.list))
    stop("model.family must be either 'gaussian' or 'binomial'")
  
  # Extract elements from apc.indiv.design.model
  design <- apc.indiv.design.model$full.design
  if(is.null(DV)) {
    DV <- apc.indiv.design.model$DV}
  xi.dim1 <- apc.indiv.design.model$xi.dim
  model.design <- apc.indiv.design.model$model.design
  model.string <- apc.indiv.design.model$model.string
  model.formula <- apc.indiv.design.model$model.formula 
  intercept <- apc.indiv.design.model$intercept
  difdif <- apc.indiv.design.model$difdif
  slopes <- apc.indiv.design.model$slopes
  age.max <- apc.indiv.design.model$age.max
  per.max <- apc.indiv.design.model$per.max
  coh.max <- apc.indiv.design.model$coh.max
  age1 <- apc.indiv.design.model$age1
  per1 <- apc.indiv.design.model$per1
  coh1 <- apc.indiv.design.model$coh1
  unit <- apc.indiv.design.model$unit
  plmmodel <- apc.indiv.design.model$plmmodel
  WT <- apc.indiv.design.model$WT
  
  # Get data for estimation
  if(plmmodel=="notplm" & is.null(WT)){
    gendata <- cbind(DV, design)
    rm(DV, design)
  }
  
  # Extract elements unique to panel models
  ID <- NULL; PER <- NULL; plmindex <- NULL; plmdata <- NULL
  if(plmmodel %in% c("within", "pooling", "random")){
    ID        <- apc.indiv.design.model$ID
    PER       <- apc.indiv.design.model$PER
    plmindex  <- c(names(ID), names(PER))
    plmdata   <- cbind(ID, PER, DV, design)
    rm(ID, PER, DV, design)
  }
  
  # Extract elements unique to models with weights
  svdata <- NULL
  if(!is.null(WT)){
    names(WT)           <- "weight"
    svdata_intermediate <- cbind(WT, DV, design)
    svdata              <- svydesign(ids = ~1, data = svdata_intermediate,
                                     weights = ~weight)
    rm(svdata_intermediate)
    rm(DV, design)
  }
  
  
  ##################
  # Step 2: Estimation
  if(is.null(plmindex) & is.null(WT)){
    # Normal OLS case
    if (model.family == "binomial"){
      fit <- glm(model.formula, family = binomial, data = gendata)
      lik <- -0.5*fit$deviance
    }
    if (model.family == "gaussian"){
      fit <- glm(model.formula, family = gaussian, data = gendata)
      # Get likelihood
      RSS <- sum(fit$residuals^2)
      N <- length(fit$residuals)
      lik <- -(N/2)*log(2*pi*RSS/N) - (N/2)
    }
    # Extract coefficients and covariance
    coefficients	<- summary.glm(fit)$coefficients
    covariance	<- summary.glm(fit)$cov.scaled
  }
  
  if(is.null(plmindex) & !is.null(WT)){
    # OLS with weights case
    if (model.family == "binomial")
      fit <- svyglm(model.formula, design = svdata, 
                    family=quasibinomial())
    if (model.family == "gaussian")
      fit <- svyglm(model.formula, design = svdata)
    # Report likelihood
    lik <- NULL
    # Extract coefficients and covariance
    coefficients	<- summary.glm(fit)$coefficients
    covariance	<- summary.glm(fit)$cov.scaled
  }
  
  if(!is.null(plmindex) & is.null(WT)){
    # Panel data case
    if (model.family == "gaussian")
      fit <- plm(model.formula, data = plmdata, model = plmmodel,
                 index = plmindex)
    if (model.family == "binomial")
      stop("binomial models not implemented for panel data")
    # Report likelihood
    lik <- NULL
    # Extract coefficients and covariance
    coefficients	<- summary(fit)$coefficients
    covariance	<- fit$vcov
  }
  
  if(!is.null(plmindex) & !is.null(WT))
    stop("weights not implemented for panel data")
  
  ##################
  # Step 3: Work with coefficients and covariance 
  n.coeff <- length(coefficients[, 1])
  n.coeff.canonical <- xi.dim1
  
  # Manipulate coefficients and covariance based on model design
  # First case: model design is 1
  if (model.design %in% c("1")){
    coefficients.canonical	<- t(coefficients[1, ])
    covariance.canonical	<- covariance[1,1]  
    
    if (n.coeff - n.coeff.canonical > 1)
      coefficients.covariates <- coefficients[2:n.coeff, ]
    else if (n.coeff - n.coeff.canonical == 1)
      coefficients.covariates <- t(coefficients[2:n.coeff, ])
    else
      coefficients.covariates <- NULL
    
    #	get standard errors 
    coefficients.canonical[,2]	<- sqrt(covariance.canonical)
    #	get t-statistics
    coefficients.canonical[,3]	<- coefficients.canonical[,1] 	/ 
      coefficients.canonical[,2]
    #	get p-values
    coefficients.canonical[,4]	<- 2*pnorm(abs(coefficients.canonical[  ,3]),
                                          lower.tail=FALSE)
  }
  # Second case: model design is Ft
  if (model.design %in% c("Ft")){
    coefficients.canonical	<- t(coefficients[n.coeff, ])
    covariance.canonical	<- covariance[n.coeff,n.coeff]  
    
    if (n.coeff - n.coeff.canonical > 1)
      coefficients.covariates <- coefficients[1:(n.coeff-1), ]
    else if (n.coeff - n.coeff.canonical == 1)
      coefficients.covariates <- t(coefficients[1:(n.coeff-1), ])
    else
      coefficients.covariates <- NULL
    
    #	get standard errors 
    coefficients.canonical[,2]	<- sqrt(covariance.canonical)
    #	get t-statistics
    coefficients.canonical[,3]	<- coefficients.canonical[,1] 	/ 
      coefficients.canonical[,2]
    #	get p-values
    coefficients.canonical[,4]	<- 2*pnorm(abs(coefficients.canonical[  ,3]),
                                          lower.tail=FALSE)
  }
  # Third case: fixed effects models
  if (model.design %in% c("FAP", "FA", "FP")){
    coefficients.canonical	<- coefficients[(n.coeff- n.coeff.canonical
                                            +1):n.coeff, ]
    covariance.canonical	<- covariance[(n.coeff- n.coeff.canonical
                                        +1):n.coeff,
                                       (n.coeff-n.coeff.canonical+1):n.coeff]
    
    if(n.coeff - n.coeff.canonical > 1)
      coefficients.covariates <- coefficients[1:(n.coeff-n.coeff.canonical), ]
    else if (n.coeff - n.coeff.canonical == 1)
      coefficients.covariates <- t(coefficients[1:(n.coeff-
                                                     n.coeff.canonical), ])
    else
      coefficients.covariates <- NULL
    
    #	get standard errors
    coefficients.canonical[,2]	<- sqrt(diag(covariance.canonical))
    #	get t-statistics
    coefficients.canonical[,3]	<- coefficients.canonical[,1] 	/
      coefficients.canonical[,2]
    #	get p-values
    coefficients.canonical[,4]	<- 2*pnorm(abs(coefficients.canonical[  ,3]),
                                          lower.tail=FALSE)
  }
  # Fourth case: model design is none of the above
  if (!model.design %in% c("1","FAP", "FA", "FP", "Ft")){
    coefficients.canonical	<- coefficients[c(1, (n.coeff-n.coeff.canonical
                                                 +2):n.coeff), ]
    covariance.canonical	<- covariance[c(1, (n.coeff-n.coeff.canonical
                                             +2):n.coeff), 
                                       c(1, (n.coeff-n.coeff.canonical
                                             +2):n.coeff)]   
    
    if(n.coeff - n.coeff.canonical > 1)
      coefficients.covariates <- coefficients[2:(n.coeff-n.coeff.canonical+1), ]
    else if (n.coeff - n.coeff.canonical == 1)
      coefficients.covariates <- t(coefficients[2:(n.coeff-n.coeff.canonical
                                                   +1), ])
    else
      coefficients.covariates <- NULL
    
    #	get standard errors 
    coefficients.canonical[,2]	<- sqrt(diag(covariance.canonical))
    #	get t-statistics
    coefficients.canonical[,3]	<- coefficients.canonical[,1] 	/ 
      coefficients.canonical[,2]
    #	get p-values
    coefficients.canonical[,4]	<- 2*pnorm(abs(coefficients.canonical[  ,3]),
                                          lower.tail=FALSE)
  }
  
  
  ##################
  # Step 4: Use output thus far to get dates for use in plotting later 
  
  index.age	<- NULL
  index.per	<- NULL
  index.coh	<- NULL
  
  # changes to index based on max
  start		<- intercept+sum(slopes)
  if(difdif[1])	{	index.age	<- start+seq(1,age.max-2);	start	<- start+age.max-2	}
  if(difdif[2])	{	index.per	<- start+seq(1,per.max-2);	start	<- start+per.max-2	}
  if(difdif[3])	{	index.coh	<- start+seq(1,coh.max-2);	start	<- start+coh.max-2	}
  xi.dim2		<- start
  # use index, max, unit, xi.dim2, difdif to get dates
  dates		<- matrix(data=NA,nrow=xi.dim2,ncol=1)			
  if(difdif[1])	dates[index.age,1]	<- age1+seq(2,age.max-1)*unit	
  if(difdif[2])	dates[index.per,1]	<- per1+seq(2,per.max-1)*unit
  if(difdif[3])	dates[index.coh,1]	<- coh1+seq(2,coh.max-1)*unit
  ##################  
  
  # Extract fixed effects
  fixef <- NULL
  if(plmmodel == "within"){
    fixef <- fixef(fit)
  }
  
  # Return valuables
  valuables <- c(list(fit = fit,
                      
                      coefficients.canonical = coefficients.canonical,
                      covariance.canonical = covariance.canonical,
                      dates = dates,
                      index.age = index.age,
                      index.coh = index.coh,
                      index.per = index.per,
                      
                      coefficients.covariates = coefficients.covariates, 
                      
                      model.family = model.family,
                      
                      intercept = intercept,
                      difdif = difdif,
                      slopes = slopes,
                      age1 = age1,
                      per1 = per1,
                      coh1 = coh1,
                      unit = unit,
                      age.max = age.max,
                      per.max = per.max,
                      coh.max = coh.max,
                      
                      model.design = apc.indiv.design.model$model.design,
                      per.zero = apc.indiv.design.model$per.zero,
                      per.odd = apc.indiv.design.model$per.odd,
                      U = apc.indiv.design.model$U,
                      
                      likelihood = lik,
                      fixef = fixef,
                      plmmodel = plmmodel))
  return(valuables)
}

apc.indiv.est.model <- function(data, unit=1, 
                                n.coh.excl.start=0, n.coh.excl.end=0,
                                n.per.excl.start=0, n.per.excl.end=0,
                                n.age.excl.start=0, n.age.excl.end=0,
                                model.design="APC", dep.var=NULL,
                                covariates=NULL, model.family="gaussian",
                                NR.controls=NULL,
                                existing.collinear = NULL,
                                existing.design = NULL,
                                plmmodel = "notplm", id.var = NULL,
                                wt.var = NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function can be used to estimate any APC model or the TS model
  # ---------------------------------------------------------------------------
  
  # Check that design and family are correctly specified
  design.list <- c("APC", "AP", "AC", "PC", "Ad", "Pd", "Cd", "A",
                   "P", "C", "t", "tA", "tP", "tC", "1",
                   "FAP", "FA", "FP", "Ft")
  if (!isTRUE(model.design %in% c(design.list, "TS")))
    stop("model.design not recognised")
  family.list <- c("gaussian", "binomial")
  if (!isTRUE(model.family %in% c(family.list)))
    stop("model.family not recognised")
  if(model.design == "TS" & !plmmodel=="notplm")
    stop("time-saturated model not implemented for panel data")
  if(model.design == "TS" & !is.null(wt.var))
    warning("time-saturated model not implemented with survey weights")
  
  # Estimate any of the APC model or submodels
  if (model.design %in% design.list){
    # First get collinear design matrix
    collinear <- existing.collinear
    if (is.null(collinear)){
      collinear <- apc.indiv.design.collinear(data=data, unit=unit, 
                                              n.coh.excl.start = n.coh.excl.start, 
                                              n.coh.excl.end   = n.coh.excl.end,
                                              n.per.excl.start = n.per.excl.start, 
                                              n.per.excl.end   = n.per.excl.end,
                                              n.age.excl.start = n.age.excl.start, 
                                              n.age.excl.end   = n.age.excl.end)
    }
    # Then get model-specific design matrix
    design <- existing.design
    if (is.null(design)){
      design <- apc.indiv.design.model(collinear, model.design = model.design,
                                       dep.var = dep.var, 
                                       covariates = covariates,
                                       plmmodel = plmmodel, id.var = id.var,
                                       wt.var = wt.var)
    }
    # Finally estimate the model
    model <- apc.indiv.fit.model(design, model.family = model.family)
    
  } else if (model.design=="TS"){
    # Estimate the time-saturated model
    if (model.family == "gaussian"){
      # The gaussian model is estimated analytically
      model <- apc.indiv.estimate.TS(data=data, dep.var = dep.var,
                                     covariates = covariates)
    } else if (model.family == "binomial"){
      # Estimate the TS model by Newton-Rhapson
      model <- apc.indiv.logit.TS(data = data, dep.var = dep.var,
                                  covariates = covariates,
                                  NR.controls = NR.controls)
    } 
  }
  return(model)
}

apc.indiv.estimate.TS <- function(data, dep.var, covariates=NULL){
  # BN, 27 aug 2020: changed cell.name from character to factor
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function estimates the time-saturated gaussian model
  # ---------------------------------------------------------------------------
    
  # check data has appropriate variable names
  if(isTRUE(("age" %in% colnames(data)) 
            && ("period" %in% colnames(data))
            && ("cohort" %in% colnames(data))) == FALSE)
    stop("apc.error: data missing one of age, period, cohort")
  
  # add cell.name: age-cohort cell identifier
  if (!"cell.name" %in% colnames(data)) {
    cell.name <- paste("ik", as.character(data$age), as.character(data$cohort), 
                       sep="_")
	cell.name <- factor(cell.name)	# BN, 27 aug 2020, added				   
    data <- cbind(cell.name, data)
  } else {							# BN, 27 aug 2020, added function name to warning
    warning("apc.indiv.estimate.TS: Variable cell.name must be unique identifier of age-cohort cells;
            if preexisting variable cell.name has alternate meaning please rename")
  }
  
  ######################	
  # Case with covariates (covariates)
  if(!is.null(covariates)){
    ######################	
    # Step 1: Effective regression of covariates Z on T: OLS estimator is equal
    # to within-cell mean.
    
    cell.mean.Z.withname <- ddply(data, .variables=c("cell.name"),
                                  function(subdata, colnames){colMeans(subdata[colnames])},
                                  covariates)
    colnames(cell.mean.Z.withname)[2:ncol(cell.mean.Z.withname)] <- 
      paste("cell.mean", covariates, sep=".")
    
    data.and.phi <- join(data, cell.mean.Z.withname, by="cell.name")
    cell.mean.Z <- cell.mean.Z.withname[2:ncol(cell.mean.Z.withname)]
    
    residuals.matrix <- as.data.frame(matrix(nrow=nrow(data.and.phi),
                                             ncol=length(covariates)))
    for (z in 1:length(covariates)){
      true <- data.and.phi[covariates[z]]
      cov.predicted <- paste("cell.mean", covariates[z], sep=".")
      predicted <- data.and.phi[cov.predicted]
      resid <- true - predicted
      residuals.matrix[, z] <- resid
      colnames(residuals.matrix)[z] <- paste("resid", covariates[z], sep=".")
    }
    
    data.resid.Z.on.T <- cbind(data.and.phi, residuals.matrix)
    # Contains: data, cell.mean.z, resid.z
    ######################	
    # Step 2: Regression of Y on residuals from regression of Z on T: OLS
    # estimator equals zeta.
    stage2.dep.var <- data.resid.Z.on.T[dep.var]
    stage2.evar <- residuals.matrix
    stage2.regress <- glm.fit(as.matrix(stage2.evar), 
                              as.matrix(stage2.dep.var), family = gaussian())
    zetahat <- stage2.regress$coefficients
    ######################	
    # Step 3: Effective regression of Y on T: gives rho, not kappa. Same method
    # as in Step 1.
    
    rhohat.withname <- ddply(data, .variables=c("cell.name"),
                             function(subdata, colnames){colMeans(subdata[colnames])},
                             dep.var)
    colnames(rhohat.withname)[2] <- "rhohat"
    rhohat <- rhohat.withname[, 2]
    
    ######################	
    # Step 4: Construct kappa from estimates ofphi (from step 1), zeta (from 
    # step 2), rho (from step 3)
    conformable.zeta <- (as.matrix(as.numeric(zetahat)))
    conformable.phi <- as.matrix(cell.mean.Z)
    phi.times.zeta <- conformable.phi %*% conformable.zeta
    
    kappahat <- rhohat - phi.times.zeta
    rho.and.kappa <- cbind(rhohat.withname, kappahat)
    data.all.coeffs <- join(data.resid.Z.on.T, rho.and.kappa, by="cell.name")
    # Contains: data, cell.means.Z, resid.Z, rhohat, kappahat
    ######################	
    # Step 5: Calculate residuals from main model
    predicted.y.from.covariates <- as.matrix(data.all.coeffs[covariates]
    ) %*% as.matrix(zetahat)
    y.residuals <- data.all.coeffs[dep.var] - predicted.y.from.covariates - 
      data.all.coeffs["kappahat"]
    colnames(y.residuals) <- "y.residuals"
    
    final.data <- cbind(data.all.coeffs, y.residuals)
    # Contains: data, cell.means.Z, resid.Z, rhohat, kappahat, y.residuals
    
    ######################	
    # Step 6: Calculate covariance matrix
    # Homoskedasticity assumed so can use var(beta) = (X'X)^{-1} sigma^2
    
    # get the estimate of the variance of the residuals
    small.sigma.hat <- sum(final.data["y.residuals"]^2)/nrow(final.data)
    
    # in case there are more levels than unique values observed (would be e.g.
    # if using data where cell.name already defined and it was a subset)
    final.data$cell.name <- droplevels(data$cell.name)
    
    # get parts of design matrix
    Tdiag.withname <- ddply(data, .variables=c("cell.name"),
                            function(subdata){(nrow(subdata))})
    colnames(Tdiag.withname)[2] <- "cell.counts"
    Tdiag <- Tdiag.withname[,2]
    
    cell.sum.Z.withname <- ddply(data, .variables=c("cell.name"),
                                 function(subdata, colnames){colSums(subdata[colnames])},
                                 covariates)
    colnames(cell.sum.Z.withname)[2:ncol(cell.sum.Z.withname)] <- 
      paste("cell.sum", covariates, sep=".")
    cell.sum.Z <- cell.sum.Z.withname[2:ncol(cell.sum.Z.withname)]
    
    tr <- as.matrix(cell.sum.Z)
    bl <- t(tr)
    
    Z.mat <- as.matrix(final.data[,names(final.data) %in% covariates])
    colnames(Z.mat) <- names(final.data)[names(final.data) %in% covariates]
    
    br <- t(Z.mat) %*% Z.mat
    
    # Invert top left (T'T)
    Tinv.diag <- 1/Tdiag
    inv.tl <- diag(Tinv.diag)
    
    # Schur complement
    schur <- br - bl %*% inv.tl %*% tr 
    inv.schur <- solve(schur)
    
    # top-left element of inverse X'X
    tlinvD <- inv.tl + inv.tl %*% tr %*% inv.schur %*% bl %*% inv.tl
    
    # top-right element of inverse X'X
    trinvD <- - inv.tl %*% tr %*% inv.schur
    
    # bottom-left element of inverse X'X
    blinvD <- - inv.schur %*% bl %*% inv.tl
    
    # bottom-right element of inverse X'X
    brinvD <- inv.schur
    
    top_invD <- cbind(tlinvD, trinvD)
    bot_invD <- cbind(blinvD, brinvD)
    
    invD <- rbind(top_invD, bot_invD)
    
    Sigma.hat <- invD*small.sigma.hat 
    Std.error <- sqrt(diag(Sigma.hat))
    
    coefficients.TS <- matrix(nrow=nrow(kappahat), ncol=4)
    coefficients.TS[,1] <- kappahat
    coefficients.TS[,2] <- Std.error[1:nrow(kappahat)]
    coefficients.TS[,3] <- coefficients.TS[,1]/coefficients.TS[,2]
    coefficients.TS[,4] <- 2*pnorm(abs(coefficients.TS[,3]), lower.tail=FALSE)
    colnames(coefficients.TS) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(coefficients.TS) <- rho.and.kappa[,1]
    
    coefficients.covariates <- matrix(nrow=nrow(as.matrix(zetahat)), ncol=4)
    coefficients.covariates[,1] <- zetahat
    coefficients.covariates[,2] <- Std.error[(nrow(kappahat)+1):nrow(Sigma.hat)]
    coefficients.covariates[,3] <- coefficients.covariates[,1]/coefficients.covariates[,2]
    coefficients.covariates[,4] <- 2*pnorm(abs(coefficients.covariates[,3]), lower.tail=FALSE)
    colnames(coefficients.covariates) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(coefficients.covariates) <- rownames(Sigma.hat)[(nrow(kappahat)+1):nrow(Sigma.hat)]
    
  } 
  ######################	
  ##### Case without covariates
  else{
    
    # Effective regression of Y on T
    
    rhohat.withname <- ddply(data, .variables=c("cell.name"),
                             function(subdata, colnames){colMeans(subdata[colnames])},
                             dep.var)
    colnames(rhohat.withname)[2] <- "rhohat"
    rhohat <- rhohat.withname[, 2]
    
    data.all.coeffs <- join(data, rhohat.withname, by="cell.name")
    
    # Construct residuals from regression of Y on T
    y.residuals <- data.all.coeffs[dep.var] - data.all.coeffs["rhohat"]
    colnames(y.residuals) <- "y.residuals"
    
    final.data <- cbind(data.all.coeffs, y.residuals)
    
    zetahat <- NA
    kappahat <- rhohat
    
    # Step 6: Calculate covariance matrix
    # Homoskedasticity assumed so can use var(beta) = (X'X)^{-1} sigma^2
    
    # get the estimate of the variance of the residuals
    small.sigma.hat <- sum(final.data["y.residuals"]^2)/nrow(final.data)
    
    # in case there are more levels than unique values observed (would be e.g.
    # if using data where cell.name already defined and it was a subset)
    final.data$cell.name <- droplevels(data$cell.name)
    
    # get parts of design matrix
    Tdiag.withname <- ddply(data, .variables=c("cell.name"),
                            function(subdata){(nrow(subdata))})
    colnames(Tdiag.withname)[2] <- "cell.counts"
    Tdiag <- Tdiag.withname[,2]
    
    # Invert top left (T'T)
    Tinv.diag <- 1/Tdiag
    invD <- diag(Tinv.diag)
    
    Sigma.hat <- invD*small.sigma.hat 
    Std.error <- sqrt(diag(Sigma.hat))
    
    coefficients.TS <- matrix(nrow=nrow(as.matrix(kappahat)), ncol=4)
    coefficients.TS[,1] <- kappahat
    coefficients.TS[,2] <- Std.error[1:nrow(as.matrix(kappahat))]
    coefficients.TS[,3] <- coefficients.TS[,1]/coefficients.TS[,2]
    coefficients.TS[,4] <- 2*pnorm(abs(coefficients.TS[,3]), lower.tail=FALSE)
    colnames(coefficients.TS) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(coefficients.TS) <- rhohat.withname[,1]
    
    coefficients.covariates <- NULL
  }
  
  ######################
  # Calculate likelihood and AIC
  RSS <- sum(final.data["y.residuals"]^2)
  n.coeff <- length(rhohat) + length(zetahat) - 
    as.numeric(is.na(zetahat[1])) #because length(NA)=1
  N <- nrow(final.data)
  
  lik <- -(N/2)*log(2*pi*RSS/N) - (N/2)
  aic <- -2*lik + 2*(n.coeff + 1)
  
  # Tidy names zetahat
  old.names.zetahat <- names(zetahat)
  new.names.zetahat <- gsub("^resid.", "", old.names.zetahat)
  names(zetahat) <- new.names.zetahat
  
  # Get useful things
  param.dimension <- length(kappahat)+length(zetahat)
  
  ######################	
  # Return valuables
  valuables <- list(n.cells = length(unique(final.data$cell.name)),
                    TSS = sum(final.data[dep.var]^2),
                    RSS = RSS,
                    zetahat = zetahat,
                    rhohat = rhohat,
                    kappahat = kappahat,
                    coefficients.covariates = coefficients.covariates,
                    coefficients.TS = coefficients.TS,
                    Sigmahat = Sigma.hat,
                    likelihood = lik,
                    aic = aic,
                    param.dimension = n.coeff)
  }

apc.indiv.logit.TS <- function(data, dep.var, covariates=NULL, 
                               NR.controls=NULL){
  # BN, 27 aug 2020: changed cell.name from character to factor
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function estimates the time-saturated logit model
  # ---------------------------------------------------------------------------
  
  if(is.null(NR.controls)){
    NR.controls <- list(10, 30, .002, "ols", .Machine$double.eps, .002, NULL, NULL)
    names(NR.controls) <- c("maxit.loop", "maxit.linesearch", "tolerance", "init", 
                            "inv.tol", "d1.tol", "custom.kappa", "custom.zeta")
  }
  maxit.loop <- NR.controls$maxit.loop
  maxit.linesearch <- NR.controls$maxit.linesearch
  tolerance <- NR.controls$tolerance
  init <- NR.controls$init
  inv.tol <- NR.controls$inv.tol
  d1.tol <- NR.controls$d1.tol
  custom.kappa <- NR.controls$custom.kappa
  custom.zeta <- NR.controls$custom.zeta
  
  if(!is.null(covariates)){
    
    #### Calculate things that will be used repeatedly ####
    
    ## get Y and Z
    Y <- as.matrix(data[dep.var])
    Z <- as.matrix(data[names(data) %in% covariates])
    
    ## get cell name
    # add cell.name to data: age-cohort cell identifier (if not already there)
    if (!"cell.name" %in% colnames(data)) {
      cell.name <- paste("ik", as.character(data$age), as.character(data$cohort),
                         sep="_")
	  cell.name <- factor(cell.name)	# BN, 27 aug 2020, added				   
      data <- cbind(cell.name, data)
  } else {							# BN, 27 aug 2020, added function name to warning
      warning("apc.indiv.logit.TS: Variable cell.name must be unique identifier of age-cohort cells;
              if preexisting variable cell.name has alternate meaning please 
              rename")
    }
	
    ## get first part of first derivative
    cell.sum.Y.withname <- ddply(data, .variables=c("cell.name"),
                                 function(subdata, colnames){colSums(subdata[colnames])},
                                 dep.var)
    colnames(cell.sum.Y.withname)[2:ncol(cell.sum.Y.withname)] <- 
      paste("cell.sum", dep.var, sep=".")
    cell.sum.Y <- cell.sum.Y.withname[2:ncol(cell.sum.Y.withname)]
    
    d1e1_top <- as.matrix(cell.sum.Y)
    d1e1_bot <- t(Z) %*% Y
    
    #### Get initial starting values ####
    
    if((!is.null(custom.kappa)|!is.null(custom.zeta)) & !(init=="custom"))
      warning("custom.kappa and custom.zeta are not used without init=custom")
    
    if (init == "ols"){
      # use estimate.TS once to get estimated kappa and zeta
      initial.TS <- apc.indiv.estimate.TS(data, dep.var = dep.var, 
                                          covariates = covariates)
      kappa <- as.matrix(initial.TS$kappahat)
      zeta <- as.matrix(initial.TS$zetahat) 
    } else if (init == "zero"){
      zeta <- rep(0, ncol(Z)) 
      kappa <- rep(0, nrow(cell.sum.Y))
    } else if (init == "custom"){
      zeta <- custom.zeta
      kappa <- custom.kappa
    } else stop("init must be one of 'ols', 'zero', 'custom'")
    
    # set loop iteration counter
    i <- 1
    
    # get mu, the individual predictor
    kappa.withname <- cbind(cell.sum.Y.withname[1], kappa)
    indiv.cell.name <- data["cell.name"]
    indiv.kappa <- join(indiv.cell.name, kappa.withname, by="cell.name") 
    
    mu <- indiv.kappa$kappa + Z %*% zeta
    colnames(mu) <- "mu"
    
    # evaluate the log-likelihood at the estimated values of kappa and zeta: 
    lik <- (t(Y) %*% mu) - sum(log(1+exp(mu)))
    
    # --------------------------
    
    #### The loop ####
    
    while (i <= maxit.loop){
      
      ## Step 1: Elements of updating ##
      
      # get Nx1 vector of probabilities
      pi <- exp(mu)/(1+exp(mu))
      colnames(pi) <- "pi"
      
      # get matrix W (diagonal matrix)
      w.element <- pi*(1-pi)
      colnames(w.element) <- "w.element"
      
      ## Step 2: Derivatives ##
      
      ## First
      # second part of first derivative
      data.w.pi <- cbind(data, pi, w.element)
      
      cell.sum.pi.withname <- ddply(data.w.pi, .variables=c("cell.name"),
                                    function(subdata, colnames){colSums(subdata[colnames])},
                                    "pi")
      colnames(cell.sum.pi.withname)[2:ncol(cell.sum.pi.withname)] <- 
        paste("cell.sum", "pi", sep=".")
      cell.sum.pi <- cell.sum.pi.withname[2:ncol(cell.sum.pi.withname)]
      
      d1e2_top <- as.matrix(cell.sum.pi)
      d1e2_bot <- t(Z) %*% pi
      
      # complete first derivative
      d1_top <- d1e1_top - d1e2_top
      d1_bot <- d1e1_bot - d1e2_bot
      
      ## Second
      
      # top-left element of second derivative (diagonal matrix)
      cell.sum.Welm <- ddply(data.w.pi, .variables = c("cell.name"),
                             function(dfr, colnm){sum(dfr[, colnm])}, "w.element")
      colnames(cell.sum.Welm)[2] <- paste("cell.sum", "w.element", sep=".")
      
      TWT.diag <- cell.sum.Welm[,2]
      
      # top-right element of second derivative
      WelmZ <- t(t(Z) * as.vector(w.element))
      colnames(WelmZ) <- paste("weighted", colnames(Z), sep=".")
      data.pi.Z <- cbind(data.w.pi, WelmZ)
      
      cell.sums <- as.data.frame(cell.sum.Welm)
      
      for (z in colnames(WelmZ)){
        cell.sum.Wcov <- ddply(data.pi.Z, .variables = c("cell.name"),
                               function(dfr, colnm){sum(dfr[, colnm])}, z)
        colnames(cell.sum.Wcov)[2] <- paste("cell.sum", z, sep=".")
        cell.sums <- join(cell.sums, cell.sum.Wcov, by="cell.name")
      }
      
      tr2 <- as.matrix(cell.sums[, 3:ncol(cell.sums)])
      
      # bottom-left element of second derivative
      bl2 <- t(tr2)
      
      # bottom-right element of second derivative
      br2 <- t(WelmZ) %*% Z
      
      ## Step 3: Inversion of Second derivative ##
      TWTinv.diag <- 1/TWT.diag
      inv.tl2 <- diag(TWTinv.diag)
      
      # Schur complement
      schur <- br2 - bl2 %*% inv.tl2 %*% tr2 
      inv.schur <- solve(schur, tol=inv.tol)
      
      # top-left element of second derivative inverse
      tl2inv <- inv.tl2 + inv.tl2 %*% tr2 %*% inv.schur %*% bl2 %*% inv.tl2
      
      # top-right element of second derivative inverse
      tr2inv <- - inv.tl2 %*% tr2 %*% inv.schur
      
      # bottom-left element of second derivative inverse
      bl2inv <- - inv.schur %*% bl2 %*% inv.tl2
      
      # bottom-right element of second derivative inverse
      br2inv <- inv.schur
      
      ## Step 4: Update, including linesearch ##
      
      ## Starting values
      # set linesearch parameter
      linesearch <- 1
      # set linesearch iteration counter
      j <- 1
      
      ## Loop over linesearch
      while (j <= maxit.linesearch){
        # Kappa (TS parameter)
        kappa_update <- tl2inv %*% d1_top + tr2inv %*% d1_bot
        kappa_new <- kappa + linesearch*kappa_update
        colnames(kappa_new) <- "kappa"
        
        # zeta (covariate parameter)
        zeta_update <- bl2inv %*% d1_top + br2inv %*% d1_bot
        zeta_new <- zeta + linesearch*zeta_update
        
        # mu (individual predictor)
        kappa_new.withname <- cbind(cell.sum.Y.withname[1], kappa_new)
        indiv.kappa_new <- join(indiv.cell.name, kappa_new.withname, by="cell.name") 
        
        mu_new <- indiv.kappa_new$kappa + Z %*% zeta_new
        
        # evaluate the likelihood at new values
        lik_new <- (t(Y) %*% mu_new) - sum(log(1+exp(mu_new)))
        
        # compare likelihoods: only to see if need to re-enter linesearch
        if (lik_new - lik < 0){
          # New likelihood is lower, have overstepped. Enter linesearch
          linesearch <- linesearch/2
          j <- j+1
        } else break
      }
      
      ## Update pi and first derivative
      
      # get Nx1 vector of probabilities
      pi_new <- exp(mu_new)/(1+exp(mu_new))
      colnames(pi_new) <- "pi"
      
      # get matrix W (diagonal matrix)
      w.element_new <- pi_new*(1-pi_new)
      colnames(w.element_new) <- "w.element"
      
      # get new second element of first derivative
      data.w.pi_new <- cbind(data, pi_new)
      
      cell.sum.pi_new.withname <- ddply(data.w.pi_new, .variables=c("cell.name"),
                                        function(subdata, colnames){colSums(subdata[colnames])},
                                        "pi")
      colnames(cell.sum.pi_new.withname)[2:ncol(cell.sum.pi_new.withname)] <- 
        paste("cell.sum", "pi_new", sep=".")
      cell.sum.pi_new <- cell.sum.pi_new.withname[2:ncol(cell.sum.pi_new.withname)]
      
      d1e2_top_new <- as.matrix(cell.sum.pi_new)
      d1e2_bot_new <- t(Z) %*% pi_new
      
      # complete first derivative
      d1_top_new <- d1e1_top - d1e2_top_new
      d1_bot_new <- d1e1_bot - d1e2_bot_new
      d1_new <- c(d1_top_new, d1_bot_new)    
      norm.d1 <- norm(as.matrix(d1_new), type="F")
      
      ## Step 5: Compare results of updating ##
      # Case 1: difference exceeds tolerance, update and re-enter loop
      if (lik_new - lik > tolerance){
        kappa <- kappa_new
        zeta <- zeta_new
        mu <- mu_new
        lik <- lik_new
        i <- i+1
        rm(kappa_new, zeta_new, mu_new, lik_new, pi_new, w.element_new,
           d1e2_top_new, d1e2_bot_new, d1_top_new, d1_bot_new)
        result <- "exceed lik tolerance, re-enter loop"
        r2 <- "at likelihood"
      } else if (lik_new - lik < 0){
        # Case 2: old likelihood higher, linesearch unsuccessful, terminate
        print(paste("overstepped and linesearch iteration limit reached,
                    exiting after", i-1, "iterations", sep=" "))
        i <- maxit.loop+4   # sets i too high for loop to continue
        result <- "overstep"
      } else if (lik_new - lik <= tolerance & lik_new - lik >= 0){
        # Case 3: likelihood condition satisfied, check first derivative
        # Case 3a: first derivative condition not satisfied, re-enter loop
        if (norm.d1 > d1.tol){
          kappa <- kappa_new
          zeta <- zeta_new
          mu <- mu_new
          lik <- lik_new
          i <- i+1
          rm(kappa_new, zeta_new, mu_new, lik_new, pi_new, w.element_new,
             d1e2_top_new, d1e2_bot_new, d1_top_new, d1_bot_new)
          result <- "exceed d1 tolerance, re-enter loop"
          r2 <- "at first derivative"
        } 
        # Case 3b: first derivative condition satisfied, terminate
        if (norm.d1 <= d1.tol){
          print(paste("converged after", i, "iterations", sep=" "))
          i <- maxit.loop+4 # sets i too high for loop to continue
          result <- "converge"
        }
      }
      # end of loop
    }
    
    if (i==maxit.loop+1)
      print(paste("max iterations exceeded, did not converge", r2, sep=" "))
    # at this point one ought to use the final values as the estimates and
    # also construct the SE based on these, but in the interests of expedience
    # I don't
    
    variance <- c(TWT.diag, diag(br2))
    Std.error <- sqrt(variance)
    
    coefficients.TS <- matrix(nrow=nrow(kappa), ncol=4)
    coefficients.TS[,1] <- kappa
    coefficients.TS[,2] <- Std.error[1:nrow(kappa)]
    coefficients.TS[,3] <- coefficients.TS[,1]/coefficients.TS[,2]
    coefficients.TS[,4] <- 2*pnorm(abs(coefficients.TS[,3]), lower.tail=FALSE)
    colnames(coefficients.TS) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(coefficients.TS) <- cell.sum.Welm[,1]
    
    coefficients.covariates <- matrix(nrow=nrow(zeta), ncol=4)
    coefficients.covariates[,1] <- zeta
    coefficients.covariates[,2] <- Std.error[(nrow(kappa)+1):nrow(as.matrix(Std.error))]
    coefficients.covariates[,3] <- coefficients.covariates[,1]/coefficients.covariates[,2]
    coefficients.covariates[,4] <- 2*pnorm(abs(coefficients.covariates[,3]), lower.tail=FALSE)
    colnames(coefficients.covariates) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(coefficients.covariates) <- rownames(zeta)
    }
  #### Case without covariates ####
  else{
    
    #### Calculate things that will be used repeatedly ####
    
    ## get Y and Z
    Y <- as.matrix(data[dep.var])
    
    ## get first part of first derivative
    # add cell.name to data: age-cohort cell identifier (if not already there)
    if (!"cell.name" %in% colnames(data)) {
      cell.name <- paste("ik", as.character(data$age), as.character(data$cohort),
                         sep="_")
      data <- cbind(cell.name, data)
    } else {
      warning("Variable cell.name must be unique identifier of age-cohort cells;
              if preexisting variable cell.name has alternate meaning please 
              rename")
    }
    
    cell.sum.Y.withname <- ddply(data, .variables=c("cell.name"),
                                 function(subdata, colnames){colSums(subdata[colnames])},
                                 dep.var)
    colnames(cell.sum.Y.withname)[2:ncol(cell.sum.Y.withname)] <- 
      paste("cell.sum", dep.var, sep=".")
    cell.sum.Y <- cell.sum.Y.withname[2:ncol(cell.sum.Y.withname)]
    
    d1e1 <- as.matrix(cell.sum.Y)
    
    #### Get initial starting values ####
    
    if((!is.null(custom.kappa)|!is.null(custom.zeta)) & !(init=="custom"))
      warning("custom.kappa and custom.zeta are not used without init=custom")
    
    if (init == "ols"){
      # use estimate.TS once to get estimated kappa and zeta
      initial.TS <- apc.indiv.estimate.TS(data, dep.var = dep.var)
      kappa <- as.matrix(initial.TS$kappahat)
    } else if (init == "zero"){
      kappa <- rep(0, nrow(cell.sum.Y))
    } else if (init == "custom"){
      kappa <- custom.kappa
    } else stop("init must be one of 'ols', 'zero', 'custom'")
    
    # set loop iteration counter
    i <- 1
    
    # get mu (individual predictor)
    kappa.withname <- cbind(cell.sum.Y.withname[1], kappa)
    indiv.cell.name <- data["cell.name"]
    indiv.kappa <- join(indiv.cell.name, kappa.withname, by="cell.name") 
    
    mu <- as.matrix(indiv.kappa$kappa) 
    colnames(mu) <- "mu"
    
    # evaluate the log-likelihood at the estimated values of kappa and zeta: 
    lik <- (t(Y) %*% mu) - sum(log(1+exp(mu)))
    
    # --------------------------
    
    #### The loop ####
    
    while (i <= maxit.loop){
      
      ## Step 1: Elements of updating ##
      
      # get Nx1 vector of probabilities
      pi <- exp(mu)/(1+exp(mu))
      colnames(pi) <- "pi"
      
      # get matrix W (diagonal matrix)
      w.element <- pi*(1-pi)
      colnames(w.element) <- "w.element"
      
      ## Step 2: Derivatives ##
      
      ## First
      data.w.pi <- cbind(data, pi, w.element)
      
      cell.sum.pi.withname <- ddply(data.w.pi, .variables=c("cell.name"),
                                    function(subdata, colnames){colSums(subdata[colnames])},
                                    "pi")
      colnames(cell.sum.pi.withname)[2:ncol(cell.sum.pi.withname)] <- 
        paste("cell.sum", "pi", sep=".")
      cell.sum.pi <- cell.sum.pi.withname[2:ncol(cell.sum.pi.withname)]
      
      d1e2 <- as.matrix(cell.sum.pi)
      
      # complete first derivative
      d1 <- d1e1 - d1e2
      
      ## Second
      # this is the "top-left" element in the covariate case
      cell.sum.Welm <- ddply(data.w.pi, .variables = c("cell.name"),
                             function(dfr, colnm){sum(dfr[, colnm])}, "w.element")
      colnames(cell.sum.Welm)[2] <- paste("cell.sum", "w.element", sep=".")
      
      d2.diag <- cell.sum.Welm[, 2]
      
      ## Step 3: Inversion of Second derivative ##
      
      # Invert top-left of second derivative
      d2inv.diag <- 1/d2.diag
      inv.d2 <- diag(d2inv.diag)
      
      ## Step 4: Update, including linesearch ##
      
      ## Starting values
      # set linesearch parameter
      linesearch <- 1
      # set linesearch iteration counter
      j <- 1
      
      ## Loop over linesearch
      while (j <= maxit.linesearch){
        # Kappa (TS parameter)
        kappa_update <- inv.d2 %*% d1 
        kappa_new <- kappa + linesearch*kappa_update
        colnames(kappa_new) <- "kappa"
        
        # get mu (individual predictor)
        kappa_new.withname <- cbind(cell.sum.Y.withname[1], kappa_new)
        indiv.kappa_new <- join(indiv.cell.name, kappa_new.withname, by="cell.name") 
        
        mu_new <- as.matrix(indiv.kappa_new$kappa) 
        
        # evaluate the likelihood at new values
        lik_new <- (t(Y) %*% mu_new) - sum(log(1+exp(mu_new)))
        
        # compare likelihoods: only to see if need to re-enter linesearch
        if (lik_new - lik < 0){
          # New likelihood is lower, have overstepped. Enter linesearch
          linesearch <- linesearch/2
          j <- j+1
        } else break
      }
      
      ## Update pi and first derivative
      
      # get Nx1 vector of probabilities
      pi_new <- exp(mu_new)/(1+exp(mu_new))
      colnames(pi_new) <- "pi"
      
      # get matrix W (diagonal matrix)
      w.element_new <- pi_new*(1-pi_new)
      colnames(w.element_new) <- "w.element"
      
      # get first derivative
      data.w.pi_new <- cbind(data, pi_new)
      
      cell.sum.pi_new.withname <- ddply(data.w.pi_new, .variables=c("cell.name"),
                                        function(subdata, colnames){colSums(subdata[colnames])},
                                        "pi")
      colnames(cell.sum.pi_new.withname)[2:ncol(cell.sum.pi_new.withname)] <- 
        paste("cell.sum", "pi_new", sep=".")
      cell.sum.pi_new <- cell.sum.pi_new.withname[2:ncol(cell.sum.pi_new.withname)]
      
      d1e2_new <- as.matrix(cell.sum.pi_new)
      
      # complete first derivative
      d1_new <- d1e1 - d1e2_new
      norm.d1 <- norm(as.matrix(d1_new), type="F")
      
      ## Step 5: Compare results of updating ##
      # Case 1: difference exceeds tolerance, update and re-enter loop
      if (lik_new - lik > tolerance){
        kappa <- kappa_new
        mu <- mu_new
        lik <- lik_new
        i <- i+1
        rm(kappa_new, mu_new, lik_new, pi_new, w.element_new, 
           d1e2_new)
        result <- "exceed lik tolerance, re-enter loop"
        r2 <- "at likelihood"
      } else if (lik_new - lik < 0){
        # Case 2: old likelihood higher, linesearch unsuccessful, terminate
        print(paste("overstepped and linesearch iteration limit reached,
                    exiting after", i-1, "iterations", sep=" "))
        i <- maxit.loop+4   # sets i too high for loop to continue
        result <- "overstep"
      } else if (lik_new - lik <= tolerance & lik_new - lik >= 0){
        # Case 3: likelihood condition satisfied, check first derivative
        # Case 3a: first derivative condition not satisfied, re-enter loop
        if (norm.d1 > d1.tol){
          kappa <- kappa_new
          mu <- mu_new
          lik <- lik_new
          i <- i+1
          rm(kappa_new, mu_new, lik_new, pi_new, w.element_new, 
             d1e2_new)
          result <- "exceed d1 tolerance, re-enter loop"
          r2 <- "at first derivative"
        } 
        # Case 3b: first derivative condition satisfied, terminate
        if (norm.d1 <= d1.tol){
          print(paste("converged after", i, "iterations", sep=" "))
          i <- maxit.loop+4 # sets i too high for loop to continue
          result <- "converge"
        }
      }
    }
    if (i==maxit.loop+1)
      print(paste("max iterations exceeded, did not converge", r2, sep=" "))
    # at this point one ought to use the final values as the estimates and
    # also construct the SE based on these, but in the interests of expedience
    # I don't
    
    variance <- d2inv.diag
    Std.error <- sqrt(variance)
    
    zeta <- NULL
    
    coefficients.TS <- matrix(nrow=nrow(kappa), ncol=4)
    coefficients.TS[,1] <- kappa
    coefficients.TS[,2] <- Std.error[1:nrow(kappa)]
    coefficients.TS[,3] <- coefficients.TS[,1]/coefficients.TS[,2]
    coefficients.TS[,4] <- 2*pnorm(abs(coefficients.TS[,3]), lower.tail=FALSE)
    colnames(coefficients.TS) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(coefficients.TS) <- cell.sum.Welm[,1]
    
    coefficients.covariates <- NULL
    }
  
  # --------------------------
  #### Return objects of interest ####
  
  #Define
  param.dimension <- length(kappa)+length(zeta)
  aic <- -2*lik + 2*param.dimension
  
  #Deleted values
  if(!exists("lik_new")) lik_new <- NULL
  if(!exists("kappa_new")) kappa_new <- NULL
  if(!exists("zeta_new")) zeta_new <- NULL
  
  valuables <- list(likelihood = lik[1],
                    kappa = kappa,
                    zeta = zeta,
                    n.loop.iterations = i-1,
                    n.linesearch.iterations = j-1,
                    new.likelihood = lik_new,
                    new.kappa = kappa_new,
                    new.zeta = zeta_new,
                    param.dimension = param.dimension,
                    aic = aic[1],
                    std.error = Std.error,
                    coefficients.TS = coefficients.TS,
                    coefficients.covariates = coefficients.covariates,
                    d1_new = d1_new,
                    norm.d1 = norm.d1,
                    result = result)
  
  return (valuables)
  }

#######################################################
#######################################################
#######################################################
# GROUP 2 Methods for comparing
# Documented in apc.indiv.compare.direct.Rd	
#######################################################
#######################################################
#######################################################

apc.indiv.compare.direct <- function(data, big.model, small.model, unit=1,
                                     dep.var, covariates=NULL, model.family,
                                     n.coh.excl.start=0, n.coh.excl.end=0,
                                     n.age.excl.start=0, n.age.excl.end=0,
                                     n.per.excl.start=0, n.per.excl.end=0,
                                     NR.controls=NULL, test, dist,
                                     wt.var=NULL,
                                     plmmodel="notplm", id.var=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function permits the direct comparison of any two models of APC 
  # effects for given data, dependent variable, and covariates.
  # ---------------------------------------------------------------------------
  
  
  # Check that family is correctly specified
  family.list <- c("gaussian", "binomial")
  if (!isTRUE(model.family %in% c(family.list)))
    stop("model.family not recognised")
  
  # Check the test
  test.list <- c("Wald", "LR")
  if(!isTRUE(test %in% test.list))
    stop("test must be one of: Wald, LR")
  
  # Check the dist
  dist.list <- c("F", "Chisq")
  if(!isTRUE(dist %in% dist.list))
    stop("dist must be one of: Chisq, F")
  
  # Check that all models are correctly specified
  design.list <- c("APC", "AP", "AC", "PC", "Ad", "Pd", "Cd", "A",
                   "P", "C", "t", "tA", "tP", "tC", "1",
                   "FAP", "FA", "FP", "Ft")
  if (!isTRUE(big.model %in% c(design.list, "TS")))
    stop("big.model not recognised")
  if (!isTRUE(small.model %in% design.list))
    stop("small.model not recognised; TS cannot be small.model")
  
  
  if(test=="LR" & big.model=="TS"){
    if(!is.null(wt.var))
      stop("LR not available for survey weights - not maximum likelihood")
    if(!plmmodel=="notplm")
      stop("LR not available for panel data models - not maximum likelihood")
    
    testout <- apc.indiv.LRtest.TS(data, small.model,
                                   dep.var, covariates,
                                   model.family, unit, 
                                   n.coh.excl.start, n.coh.excl.end,
                                   n.age.excl.start, n.age.excl.end,
                                   n.per.excl.start, n.per.excl.end,
                                   NR.controls = NR.controls) 
  }
  if(test=="LR" & big.model %in% design.list){
    if(!is.null(wt.var))
      stop("LR not available for survey weights - not maximum likelihood")
    if(!plmmodel=="notplm")
      stop("LR not available for panel data models - not maximum likelihood")
    testout <- apc.indiv.LRtest.fullapc(data, big.model, small.model,
                                        dep.var, covariates,
                                        model.family, unit, 
                                        n.coh.excl.start, n.coh.excl.end,
                                        n.age.excl.start, n.age.excl.end,
                                        n.per.excl.start, n.per.excl.end)
  }
  if(test=="Wald" & big.model=="TS"){
    if(!is.null(wt.var))
      stop("TS model not implemented for survey weights")
    if(!plmmodel=="notplm")
      stop("TS model not implemented for panel data")
    if(model.family=="binomial")
      stop("Wald tests not implemented for binomial TS; 
           use LR test or omit TS")
    testout <- apc.indiv.waldtest.TS(data, dist, small.model,
                                     dep.var, covariates,
                                     model.family, unit,
                                     n.coh.excl.start, n.coh.excl.end,
                                     n.age.excl.start, n.age.excl.end,
                                     n.per.excl.start, n.per.excl.end)
  }
  if(test=="Wald" & big.model %in% design.list){
    testout <- apc.indiv.waldtest.fullapc(data, dist, big.model, small.model,
                                          dep.var, covariates,
                                          model.family, unit,
                                          n.coh.excl.start, n.coh.excl.end,
                                          n.age.excl.start, n.age.excl.end,
                                          n.per.excl.start, n.per.excl.end,
                                          wt.var = wt.var, 
                                          plmmodel = plmmodel, id.var = id.var)
  }
  
  return(testout)
}

apc.indiv.waldtest.fullapc <- function(data, dist = "F", big.model="APC",
                                       small.model, dep.var, covariates=NULL,
                                       model.family="gaussian", unit=1,
                                       n.coh.excl.start=0, n.coh.excl.end=0,
                                       n.age.excl.start=0, n.age.excl.end=0,
                                       n.per.excl.start=0, n.per.excl.end=0,
                                       existing.big.model.fit=NULL, 
                                       existing.small.model.fit=NULL,
                                       existing.collinear=NULL,
                                       plmmodel="notplm", id.var=NULL,
                                       wt.var=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function permits testing between the full APC model and a chosen
  # submodel using a Wald test
  # ---------------------------------------------------------------------------
  
  # Step 1: Fit reduced APC model (the submodel)
  
  if(is.null(existing.small.model.fit)){
    if(is.null(existing.collinear)){
      collinear <- apc.indiv.design.collinear(data, unit, 
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    } else
      collinear <- existing.collinear
    design.R <- apc.indiv.design.model(collinear, 
                                       model.design = small.model,
                                       dep.var, covariates, plmmodel,
                                       wt.var, id.var)
    fitted.R <- apc.indiv.fit.model(design.R, model.family)
  } else 
    fitted.R <- existing.small.model.fit
  
  ######################	
  # Step 2: Fit full APC model
  if(is.null(existing.big.model.fit)){
    # note: can use same collinear design matrix for full model as for submodel
    if(is.null(collinear)){
      collinear <- apc.indiv.design.collinear(data, unit, 
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    } 
    design.U <- apc.indiv.design.model(collinear, 
                                       model.design = big.model,
                                       dep.var, covariates, plmmodel,
                                       wt.var, id.var)
    fitted.U <- apc.indiv.fit.model(design.U, model.family)
  } else 
    fitted.U <- existing.big.model.fit
  
  ######################	
  # Step 3: Run F-test
  
  # Special case of P and tP (because they use period slope)
  if(small.model %in% c("P", "tP")){
    # Extract elements of hypothesis test
    DD <- names(collinear$full.design.collinear
    )[grep("DD_", names(collinear$full.design.collinear))]
    allageDD <- DD[grep("DD_age", DD)]
    allcohDD <- DD[grep("DD_coh", DD)]
    allperDD <- DD[grep("DD_per", DD)]
    ageDDzero <- paste(allageDD, 0, sep = " = ")
    perDDzero <- paste(allperDD, 0, sep = " = ")
    cohDDzero <- paste(allcohDD, 0, sep = " = ")
    allDDzero <- paste(DD, 0, sep=" = ")
    
    # small model is P
    if(small.model == "P"){
      # column of reduction to P
      P.reduction.list <- list(
        c(ageDDzero, cohDDzero, "age_slope = cohort_slope"),
        c(ageDDzero, "age_slope = cohort_slope"),
        c(cohDDzero, "age_slope = cohort_slope"),
        c("age_slope = cohort_slope")
      )
      # possible big models
      big.model.list <- list("APC", "AP", "PC", "Pd")
      if(!big.model %in% big.model.list)
        stop("models not nested")
      # get appropriate big model and hypothesis for that
      number <- match(big.model, big.model.list)
      hyp    <- P.reduction.list[[number]]
      # run hypothesis test
      fresult <- linearHypothesis(fitted.U$fit, hyp, test = dist)
    } 
    
    # small model is tP
    if(small.model == "tP"){
      # column of reduction to tp
      tP.reduction.list <- list(
        c(allDDzero, "age_slope = cohort_slope"),
        c(ageDDzero, perDDzero, "age_slope = cohort_slope"),
        c(perDDzero, cohDDzero, "age_slope = cohort_slope"),
        c(perDDzero, "age_slope = cohort_slope"),
        c(perDDzero),
        c(ageDDzero, cohDDzero, "age_slope = cohort_slope"),
        c(ageDDzero, "age_slope = cohort_slope"),
        c(cohDDzero, "age_slope = cohort_slope"),
        c("age_slope = cohort_slope")
      )
      # possible big models
      big.model.list <- list("APC", "AP", "PC", "Pd", "P",
                             "AC", "Ad", "Cd", "t")
      if(!big.model %in% big.model.list)
        stop("models not nested")
      # get appropriate big model and hypothesis for that
      number <- match(big.model, big.model.list)
      hyp    <- tP.reduction.list[[number]]
      # run hypothesis test
      fresult <- linearHypothesis(fitted.U$fit, hyp, test = dist)
    }
  } else {
    # all other reductions are nested, so simple
    fresult <- waldtest(fitted.R$fit, fitted.U$fit, test = dist)
  }
  
  ######################	
  # consolidate results
  if(dist == "F"){
    test.stat <- fresult$F[2]
    p.value <- fresult$`Pr(>F)`[2]
    df <- paste("(", paste(as.character(fresult$Df[2]), 
                           as.character(fresult$Res.Df[2]), 
                           sep=", "), ")", sep="")
  }
  if(dist == "Chisq"){
    test.stat <- fresult$Chisq[2]
    p.value <- fresult$`Pr(>Chisq)`[2]
    df <- fresult$Df[2]
  }
  
  ######################	
  # valuables
  valuables <- list(test.type = "Wald",
                    dist.type = dist,
                    test.stat = test.stat,
                    df = df,
                    df.num = fresult$Df[2],
                    df.denom = fresult$Res.Df[2],
                    p.value = p.value,
                    aic.big = fitted.U$fit$aic,
                    aic.small = fitted.R$fit$aic,
                    lik.big = fitted.U$likelihood,
                    lik.small = fitted.R$likelihood)
  
  return(valuables)
}

apc.indiv.waldtest.TS <- function(data, dist = "F", small.model="APC",
                                  dep.var, covariates=NULL,
                                  model.family="gaussian", unit=1, 
                                  n.coh.excl.start=0, n.coh.excl.end = 0,
                                  n.age.excl.start=0, n.age.excl.end = 0,
                                  n.per.excl.start=0, n.per.excl.end = 0,
                                  existing.small.model.fit=NULL, 
                                  existing.big.model.fit=NULL, 
                                  existing.collinear=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function permits comparison of the TS model with the APC model or one 
  # of its submodels using a Wald test.
  # ---------------------------------------------------------------------------
  
  # Step 1: Fit APC model
  
  if(is.null(existing.small.model.fit)){
    if(is.null(existing.collinear)){
      collinear <- apc.indiv.design.collinear(data, unit,
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    } else collinear <- existing.collinear
    design <- apc.indiv.design.model(collinear, model.design = small.model, 
                                     dep.var=dep.var, covariates=covariates)
    fitted <- apc.indiv.fit.model(design, model.family)
  } else 
    fitted <- existing.small.model.fit
  
  # get RSS, number of parameters, number of observations for APC model.
  RSS.APC <- sum(fitted$fit$residuals^2)
  
  nparam.APC <- length(fitted$fit$coefficients)
  sample.N.APC <- length(fitted$fit$y)
  ######################	
  # Step 2: Fit TS model
  
  if(is.null(existing.big.model.fit)){
    TS.model <- apc.indiv.estimate.TS(data, dep.var = dep.var, 
                                      covariates = covariates)
  } else
    TS.model <- existing.big.model.fit
  
  # get RSS, number of parameters from TS model.
  RSS.TS <- TS.model$RSS
  if (!is.na(TS.model$zetahat[1])){
    nparam.TS <- TS.model$n.cells+length(TS.model$zetahat)
  } else{
    nparam.TS <- TS.model$n.cells
  }
  ######################	
  # step 3: Run F-test
  
  # Elements of F-test
  big.model.RSS <- RSS.TS
  small.model.RSS <- RSS.APC
  big.model.nparams <- nparam.TS
  small.model.nparams <- nparam.APC
  sample.N <- sample.N.APC
  
  df.num <- big.model.nparams - small.model.nparams
  df.denom <- sample.N - big.model.nparams
  
  # Combine elements in F-statistic
  fstat.num <- (small.model.RSS - big.model.RSS)/df.num
  fstat.denom <- (big.model.RSS - 0)/df.denom
  fstat <- fstat.num/fstat.denom
  
  if(dist == "F"){
    test.stat <- fstat
    # Get p-value by comparing F-statistic to F-distribution
    p.value <- pf(fstat, df1 = df.num, df2 = df.denom, lower.tail = FALSE)
    df <- paste("(", paste(as.character(df.num), as.character(df.denom), 
                           sep=", "), ")", sep="")
  }
  if(dist == "Chisq"){
    test.stat <- fstat*df.num
    p.value <- pchisq(test.stat, df=df.num, lower.tail = FALSE)
    df <- df.num
  }
  
  ######################	
  # valuables
  valuables <- list(test.type = "Wald",
                    dist.type = dist,
                    test.stat = test.stat,
                    df = df,
                    df.num = df.num,
                    df.denom = df.denom,
                    p.value = p.value,
                    aic.small = fitted$fit$aic,
                    aic.big = TS.model$aic,
                    lik.small = fitted$likelihood,
                    lik.big = TS.model$likelihood)
  return(valuables)
}

apc.indiv.LRtest.fullapc <- function(data,  big.model="APC", 
                                     small.model, dep.var, covariates=NULL, 
                                     model.family="binomial", unit=1,
                                     n.coh.excl.start=0, n.coh.excl.end=0,
                                     n.age.excl.start=0, n.age.excl.end=0,
                                     n.per.excl.start=0, n.per.excl.end=0,
                                     existing.big.model.fit=NULL,
                                     existing.small.model.fit=NULL,
                                     existing.collinear=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function permits comparison of the APC model with one 
  # of its submodels using a likelihood ratio test.
  # ---------------------------------------------------------------------------
  
  
  ######################	
  # step 1: fit reduced model
  
  if(is.null(existing.small.model.fit)){
    if(is.null(existing.collinear)){
      collinear <- apc.indiv.design.collinear(data,  unit,
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    } else
      collinear <- existing.collinear
    design.R <- apc.indiv.design.model(collinear,
                                       model.design = small.model, 
                                       dep.var, covariates)
    fitted.R <- apc.indiv.fit.model(design.R, model.family)
  } else 
    fitted.R <- existing.small.model.fit
  
  ######################	
  # step 2: fit APC model
  if(is.null(existing.big.model.fit)){
    # note: can use same collinear design matrix for full model as for submodel
    if(is.null(collinear)){
      collinear <- apc.indiv.design.collinear(data,  unit,
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    }
    design.U <- apc.indiv.design.model(collinear,
                                       model.design = big.model, 
                                       dep.var, covariates)
    fitted.U <- apc.indiv.fit.model(design.U, model.family)
  } else 
    fitted.U <- existing.big.model.fit  
  
  ######################	
  # step 3: run LR test
  LRresult <- lrtest(fitted.R$fit, fitted.U$fit)
  diff.lik <- LRresult$LogLik[2] - LRresult$LogLik[1]
  
  ######################	
  # step 4: extract valuables
  valuables <- list(test.type = "LR",
                    dist.type = "Chisq",
                    test.stat = 2*diff.lik,
                    df = LRresult$Df[2],
                    p.value = LRresult$`Pr(>Chisq)`[2],
                    aic.big = fitted.U$fit$aic,
                    aic.small = fitted.R$fit$aic,
                    lik.big = LRresult$LogLik[2],
                    lik.small = LRresult$LogLik[1])
  
  return(valuables)
}

apc.indiv.LRtest.TS <- function(data, small.model="APC", dep.var, covariates=NULL,
                                model.family="binomial", unit=1, 
                                n.coh.excl.start=0, n.coh.excl.end=0,
                                n.age.excl.start=0, n.age.excl.end=0,
                                n.per.excl.start=0, n.per.excl.end=0,
                                existing.small.model.fit=NULL, existing.big.model.fit=NULL, 
                                existing.collinear=NULL,
                                NR.controls=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function permits comparison of the TS model with the APC model or one 
  # of its submodels using a likelihood ratio test.
  # ---------------------------------------------------------------------------
  
  # step 1: fit APC model
  if(is.null(existing.small.model.fit)){
    if(is.null(existing.collinear)){
      collinear <- apc.indiv.design.collinear(data,  unit = unit,
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    } else collinear <- existing.collinear
    design <- apc.indiv.design.model(collinear, model.design = small.model,
                                     dep.var=dep.var, covariates=covariates)
    APC.model <- apc.indiv.fit.model(design, model.family)
  } else 
    APC.model <- existing.small.model.fit
  lik.APC <- APC.model$likelihood
  
  # step 2: fit TS model
  if(is.null(existing.big.model.fit) & model.family == "binomial"){
    TS.model <- apc.indiv.logit.TS(data, dep.var=dep.var, covariates=covariates,
                                   NR.controls = NR.controls)
  } else if(is.null(existing.big.model.fit) & model.family == "gaussian"){
    TS.model <- apc.indiv.estimate.TS(data, dep.var = dep.var, 
                                      covariates = covariates)
  } else
    TS.model <- existing.big.model.fit
  
  # step 3: run LR test
  diff.df <- TS.model$param.dimension - length(APC.model$fit$coefficients)
  diff.lik <- lik.APC - TS.model$likelihood
  p.value <- pchisq(-2*diff.lik, df = diff.df, lower.tail = FALSE)
  
  aic.apc <- APC.model$fit$aic
  aic.TS <- TS.model$aic
  
  if(model.family=="binomial"){
    NR.report <- list(result = TS.model$result, 
                      n.loop.iterations = TS.model$n.loop.iterations, 
                      n.linesearch.iterations = TS.model$n.linesearch.iterations, 
                      d1_new = TS.model$d1_new, 
                      norm.d1 = TS.model$norm.d1)
  } else
    NR.report <- NULL
  
  
  # step 4: extract valuables
  valuables <- list(test.type = "LR",
                    dist.type = "Chisq",
                    test.stat = -2*diff.lik,
                    df = diff.df,
                    p.value = p.value,
                    aic.small = aic.apc,
                    aic.big = aic.TS,
                    lik.small = lik.APC,
                    lik.big = TS.model$likelihood,
                    NR.report = NR.report)
  return(valuables)
}
  
#######################################################
#######################################################
#######################################################
# GROUP 3 Tables
# Documented in apc.indiv.model.table.Rd	
#######################################################
#######################################################
#######################################################

apc.indiv.model.table <- function(data, dep.var, covariates = NULL, unit=1,
                                  n.coh.excl.start = 0, n.coh.excl.end = 0,
                                  n.age.excl.start = 0, n.age.excl.end = 0,
                                  n.per.excl.start = 0, n.per.excl.end = 0,
                                  model.family, NR.controls = NULL,
                                  test, dist, TS=FALSE, wt.var=NULL,
                                  plmmodel="notplm", id.var=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function generates a table permitting the user to compare the TS 
  # model, the APC model, and all APC models for a given dependent variable and 
  # covariates. It uses a combination of AIC, likelihood, and Wald tests.
  # ---------------------------------------------------------------------------
  
  # Check that family is correctly specified
  family.list <- c("gaussian", "binomial")
  if (!isTRUE(model.family %in% c(family.list)))
    stop("model.family not recognised")
  
  # Check the test
  test.list <- c("Wald", "LR")
  if(!isTRUE(test %in% test.list))
    stop("test must be one of: Wald, LR")
  
  # Check the distribution
  dist.list <- c("F", "Chisq")
  if(!isTRUE(dist %in% dist.list))
    stop("dist must be one of: F, Chisq")
  
  if(test=="LR" & isTRUE(TS)){
    if(!is.null(wt.var))
      stop("LR not available for survey weights - not maximum likelihood")
    if(!plmmodel=="notplm")
      stop("LR not available for panel data models - not maximum likelihood")
    
    tabout <- apc.indiv.LRtable.TS(data, dep.var, covariates,
                                   model.family, unit, 
                                   n.coh.excl.start, n.coh.excl.end,
                                   n.age.excl.start, n.age.excl.end,
                                   n.per.excl.start, n.per.excl.end,
                                   NR.controls = NR.controls) 
  }
  if(test=="LR" & isFALSE(TS)){
    if(!is.null(wt.var))
      stop("LR not available for survey weights - not maximum likelihood")
    if(!plmmodel=="notplm")
      stop("LR not available for panel data models - not maximum likelihood")
    tabout <- apc.indiv.LRtable(data, dep.var, covariates,
                                model.family, unit, 
                                n.coh.excl.start, n.coh.excl.end,
                                n.age.excl.start, n.age.excl.end,
                                n.per.excl.start, n.per.excl.end)
  }
  if(test=="Wald" & isTRUE(TS)){
    if(!is.null(wt.var))
      stop("TS model not implemented for survey weights")
    if(!plmmodel=="notplm")
      stop("TS model not implemented for panel data")
    if(model.family=="binomial")
      stop("Wald tests not implemented for binomial TS; 
           use LR test or omit TS")
    tabout <- apc.indiv.waldtable.TS(data, dep.var, covariates,
                                     dist, unit, model.family,
                                     n.coh.excl.start, n.coh.excl.end,
                                     n.age.excl.start, n.age.excl.end,
                                     n.per.excl.start, n.per.excl.end)
  }
  if(test=="Wald" & isFALSE(TS)){
    tabout <- apc.indiv.waldtable(data, dep.var, covariates,
                                  dist, unit, model.family,
                                  n.coh.excl.start, n.coh.excl.end,
                                  n.age.excl.start, n.age.excl.end,
                                  n.per.excl.start, n.per.excl.end,
                                  wt.var, plmmodel, id.var)
  }
  
  return(tabout)
}

apc.indiv.waldtest.fullapc <- function(data, dist = "F", big.model="APC",
                                       small.model, dep.var, covariates=NULL,
                                       model.family="gaussian", unit=1,
                                       n.coh.excl.start=0, n.coh.excl.end=0,
                                       n.age.excl.start=0, n.age.excl.end=0,
                                       n.per.excl.start=0, n.per.excl.end=0,
                                       existing.big.model.fit=NULL, 
                                       existing.small.model.fit=NULL,
                                       existing.collinear=NULL,
                                       plmmodel="notplm", id.var=NULL,
                                       wt.var=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function permits testing between the full APC model and a chosen
  # submodel using a Wald test
  # ---------------------------------------------------------------------------
  
  # Step 1: Fit reduced APC model (the submodel)
  
  if(is.null(existing.small.model.fit)){
    if(is.null(existing.collinear)){
      collinear <- apc.indiv.design.collinear(data, unit, 
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    } else
      collinear <- existing.collinear
    design.R <- apc.indiv.design.model(collinear, 
                                       model.design = small.model,
                                       dep.var, covariates, plmmodel,
                                       wt.var, id.var)
    fitted.R <- apc.indiv.fit.model(design.R, model.family)
  } else 
    fitted.R <- existing.small.model.fit
  
  ######################	
  # Step 2: Fit full APC model
  if(is.null(existing.big.model.fit)){
    # note: can use same collinear design matrix for full model as for submodel
    if(is.null(collinear)){
      collinear <- apc.indiv.design.collinear(data, unit, 
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    } 
    design.U <- apc.indiv.design.model(collinear, 
                                       model.design = big.model,
                                       dep.var, covariates, plmmodel,
                                       wt.var, id.var)
    fitted.U <- apc.indiv.fit.model(design.U, model.family)
  } else 
    fitted.U <- existing.big.model.fit
  
  ######################	
  # Step 3: Run F-test
  
  # Special case of P and tP (because they use period slope)
  if(small.model %in% c("P", "tP")){
    # Extract elements of hypothesis test
    DD <- names(collinear$full.design.collinear
    )[grep("DD_", names(collinear$full.design.collinear))]
    allageDD <- DD[grep("DD_age", DD)]
    allcohDD <- DD[grep("DD_coh", DD)]
    allperDD <- DD[grep("DD_per", DD)]
    ageDDzero <- paste(allageDD, 0, sep = " = ")
    perDDzero <- paste(allperDD, 0, sep = " = ")
    cohDDzero <- paste(allcohDD, 0, sep = " = ")
    allDDzero <- paste(DD, 0, sep=" = ")
    
    # small model is P
    if(small.model == "P"){
      # column of reduction to P
      P.reduction.list <- list(
        c(ageDDzero, cohDDzero, "age_slope = cohort_slope"),
        c(ageDDzero, "age_slope = cohort_slope"),
        c(cohDDzero, "age_slope = cohort_slope"),
        c("age_slope = cohort_slope")
      )
      # possible big models
      big.model.list <- list("APC", "AP", "PC", "Pd")
      if(!big.model %in% big.model.list)
        stop("models not nested")
      # get appropriate big model and hypothesis for that
      number <- match(big.model, big.model.list)
      hyp    <- P.reduction.list[[number]]
      # run hypothesis test
      fresult <- linearHypothesis(fitted.U$fit, hyp, test = dist)
    } 
    
    # small model is tP
    if(small.model == "tP"){
      # column of reduction to tp
      tP.reduction.list <- list(
        c(allDDzero, "age_slope = cohort_slope"),
        c(ageDDzero, perDDzero, "age_slope = cohort_slope"),
        c(perDDzero, cohDDzero, "age_slope = cohort_slope"),
        c(perDDzero, "age_slope = cohort_slope"),
        c(perDDzero),
        c(ageDDzero, cohDDzero, "age_slope = cohort_slope"),
        c(ageDDzero, "age_slope = cohort_slope"),
        c(cohDDzero, "age_slope = cohort_slope"),
        c("age_slope = cohort_slope")
      )
      # possible big models
      big.model.list <- list("APC", "AP", "PC", "Pd", "P",
                             "AC", "Ad", "Cd", "t")
      if(!big.model %in% big.model.list)
        stop("models not nested")
      # get appropriate big model and hypothesis for that
      number <- match(big.model, big.model.list)
      hyp    <- tP.reduction.list[[number]]
      # run hypothesis test
      fresult <- linearHypothesis(fitted.U$fit, hyp, test = dist)
    }
  } else {
    # all other reductions are nested, so simple
    fresult <- waldtest(fitted.R$fit, fitted.U$fit, test = dist)
  }
  
  ######################	
  # consolidate results
  if(dist == "F"){
    test.stat <- fresult$F[2]
    p.value <- fresult$`Pr(>F)`[2]
    df <- paste("(", paste(as.character(fresult$Df[2]), 
                           as.character(fresult$Res.Df[2]), 
                           sep=", "), ")", sep="")
  }
  if(dist == "Chisq"){
    test.stat <- fresult$Chisq[2]
    p.value <- fresult$`Pr(>Chisq)`[2]
    df <- fresult$Df[2]
  }
  
  ######################	
  # valuables
  valuables <- list(test.type = "Wald",
                    dist.type = dist,
                    test.stat = test.stat,
                    df = df,
                    df.num = fresult$Df[2],
                    df.denom = fresult$Res.Df[2],
                    p.value = p.value,
                    aic.big = fitted.U$fit$aic,
                    aic.small = fitted.R$fit$aic,
                    lik.big = fitted.U$likelihood,
                    lik.small = fitted.R$likelihood)
  
  return(valuables)
}

apc.indiv.waldtable <- function(data, dep.var, covariates=NULL, dist = "F",
                                unit=1, model.family = "gaussian",
                                n.coh.excl.start=0, n.coh.excl.end=0, 
                                n.age.excl.start=0, n.age.excl.end=0, 
                                n.per.excl.start=0, n.per.excl.end=0,
                                wt.var = NULL, plmmodel = "notplm", 
                                id.var = NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function generates a table permitting the user to compare the APC 
  # model, and all APC models for a given dependent variable and 
  # covariates. It uses a combination of AIC, likelihood, and Wald tests.
  # ---------------------------------------------------------------------------
  
  # Step 1: write function that uses output from an F-test against the 
  #TS and an F-test against the full
  # APC model to generate a line suitable for the final table.
  get.table.line <- function(ftest1, first=FALSE){
    # Deal with things that are missing in survey or panel
    if(is.null(ftest1$lik.big)) ftest1$lik.big <- NA
    if(is.null(ftest1$lik.small)) ftest1$lik.small <- NA
    if(is.null(ftest1$aic.big)) ftest1$aic.big <- NA
    if(is.null(ftest1$aic.small)) ftest1$aic.small <- NA
    if(isTRUE(first)) # first line is APC model so aic only
      line <- c(NA, NA, NA, round(ftest1$aic.big, 3), 
                round(ftest1$lik.big, 3)) 
    else
      line <- c(round(ftest1$test.stat, 3), ftest1$df.num, round(ftest1$p.value,3), 
                round(ftest1$aic.small,3), round(ftest1$lik.small, 3))
    return(line)
  }
  ######################	
  # Step 2: Other setup
  
  # List of submodels to be investigated
  if(plmmodel == "within"){
    model.design.list <- c("FAP", "FA", "FP", "Ft")
  } else if(plmmodel == "random" & is.null(covariates)) {
    model.design.list	<- c("APC","AP","AC","PC","Ad","Pd","Cd","A", "P",
                           "t","tA", "tP")    
  } else {
    model.design.list	<- c("APC","AP","AC","PC","Ad","Pd","Cd","A", "P",
                           "C","t","tA", "tP", "tC", "1")    
  }
  
  # Empty table
  fit.tab <- matrix(nrow=length(model.design.list),ncol=5,data=NA)
  # Full APC model for comparison
  fullAPCcollinear <- apc.indiv.design.collinear(data, unit=unit,
                                                 n.coh.excl.start=n.coh.excl.start,
                                                 n.coh.excl.end=n.coh.excl.end,
                                                 n.age.excl.start = n.age.excl.start,
                                                 n.age.excl.end = n.age.excl.end,
                                                 n.per.excl.start = n.per.excl.start,
                                                 n.per.excl.end = n.per.excl.end)
  fullAPCdesign <- apc.indiv.design.model(fullAPCcollinear, dep.var = dep.var, 
                                          covariates = covariates,
                                          model.design=model.design.list[1],
                                          wt.var = wt.var, plmmodel = plmmodel,
                                          id.var = id.var)
  fullAPCfit <- apc.indiv.fit.model(fullAPCdesign, model.family=model.family)
  
  ######################	
  # Step 3: Generate table
  
  # First line: APC model
  APCfull.vs.sub1 <- apc.indiv.waldtest.fullapc(data, dep.var = dep.var, 
                                                covariates = covariates,
                                                unit = unit, dist = dist,
                                                model.family = model.family, 
                                                small.model = model.design.list[2],
                                                existing.big.model.fit = fullAPCfit,
                                                existing.collinear = fullAPCcollinear,
                                                wt.var = wt.var, plmmodel = plmmodel,
                                                id.var = id.var)
  # Full APC model
  first.line <- get.table.line(ftest1 = APCfull.vs.sub1, first=TRUE)
  fit.tab[1, 1:5] <- first.line
  # First submodel
  second.line <- get.table.line(ftest1 = APCfull.vs.sub1)
  fit.tab[2, 1:5] <- second.line
  # Remaining submodels
  for (i in 3:length(model.design.list)){
    againstAPC <- apc.indiv.waldtest.fullapc(data, dist = dist,
                                             small.model = model.design.list[i],
                                             dep.var=dep.var, covariates=covariates, 
                                             unit = unit, model.family=model.family,
                                             existing.big.model.fit = fullAPCfit, 
                                             existing.collinear = fullAPCcollinear,
                                             wt.var = wt.var, plmmodel=plmmodel,
                                             id.var = id.var)
    fit.tab[i, 1:5] <- get.table.line(againstAPC)
  }
  # Change table column and row names 
  if(dist=="F"){
    df.againstAPC <- paste("DF( * , ", as.character(againstAPC$df.denom), ")",
                           sep="")
    if(plmmodel == "within")
      table.colnames <- c("Wald (F) vs FAP", 
                          df.againstAPC, "p-value", "AIC", "lik")
    else
      table.colnames <- c("Wald (F) vs APC", 
                          df.againstAPC, "p-value", "AIC", "lik")
  }
  if(dist=="Chisq"){
    if(plmmodel == "within")
      table.colnames <- c("Wald (Chisq) vs FAP", 
                          "Df", "p-value", "AIC", "lik")
    else
      table.colnames <- c("Wald (Chisq) vs APC", 
                          "Df", "p-value", "AIC", "lik")
  }
  
  colnames(fit.tab) <- table.colnames
  rownames(fit.tab) <- model.design.list
  
  # drop columns that are empty
  if(!is.null(wt.var))
    fit.tab <- fit.tab[,!colnames(fit.tab) %in% c("lik")]
  if(!plmmodel=="notplm"){
    fit.tab <- fit.tab[,!colnames(fit.tab) %in% c("AIC", "lik")]
    fit.tab <- fit.tab[!rownames(fit.tab) %in% c("APC", "FAP"), ]
  }
  
  ######################	
  valuables <- list(table = fit.tab,
                    NR.report = NULL)
  return(valuables)
}

apc.indiv.waldtable.TS <- function(data, dep.var, covariates=NULL, dist = "F",
                                   unit=1, model.family = "gaussian",
                                   n.coh.excl.start=0, n.coh.excl.end=0, 
                                   n.age.excl.start=0, n.age.excl.end=0, 
                                   n.per.excl.start=0, n.per.excl.end=0){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function generates a table permitting the user to compare the TS 
  # model, the APC model, and all APC models for a given dependent variable and 
  # covariates. It uses a combination of AIC, likelihood, and Wald tests.
  # ---------------------------------------------------------------------------
  
  # Check
  if(!model.family=="gaussian")
    stop("waldtable.TS currently only available for gaussian family")
  ######################
  # Step 1: write function that uses output from an F-test against the 
  #TS and an F-test against the full
  # APC model to generate a line suitable for the final table.
  get.table.line <- function(ftest1, ftest2, first=FALSE, second=FALSE){
    if(isTRUE(first)) # first line is TS model so aic only
      line <- c(NA, NA, NA, NA, NA, NA, round(ftest1$aic.big, 3), 
                round(ftest1$lik.big, 3)) 
    else if(isTRUE(second)) 
      # second line is full APC model so not compared to itself
      line <- c(round(ftest1$test.stat, 3), ftest1$df.num, round(ftest1$p.value,3),
                NA,
                NA, NA, round(ftest1$aic.small,3), round(ftest1$lik.small, 3))
    else
      line <- c(round(ftest1$test.stat, 3), ftest1$df.num, round(ftest1$p.value,3), 
                round(ftest2$test.stat, 3),
                ftest2$df.num, round(ftest2$p.value, 3), round(ftest2$aic.small,3), 
                round(ftest2$lik.small, 3))
    return(line)
  }
  ######################	
  # Step 2: Other setup
  
  # List of submodels to be investigated
  model.design.list	<- c("TS", "APC","AP","AC","PC","Ad","Pd","Cd","A", "P",
                         "C","t","tA", "tP", "tC", "1")
  # Empty table
  fit.tab <- matrix(nrow=length(model.design.list),ncol=8,data=NA)
  # Full APC model for comparison
  fullAPCcollinear <- apc.indiv.design.collinear(data, 
                                                 unit=unit,
                                                 n.coh.excl.start=n.coh.excl.start,
                                                 n.coh.excl.end=n.coh.excl.end,
                                                 n.age.excl.start = n.age.excl.start,
                                                 n.age.excl.end = n.age.excl.end,
                                                 n.per.excl.start = n.per.excl.start,
                                                 n.per.excl.end = n.per.excl.end)
  fullAPCdesign <- apc.indiv.design.model(fullAPCcollinear, dep.var = dep.var, 
                                          covariates = covariates)
  fullAPCfit <- apc.indiv.fit.model(fullAPCdesign, model.family=model.family)
  # TS model for comparison
  TSfit <- apc.indiv.estimate.TS(data, dep.var=dep.var, covariates=covariates)
  ######################	
  # Step 3: Generate table
  
  # First line: TS model
  ftest.full.vs.TS <- apc.indiv.waldtest.TS(existing.small.model.fit = fullAPCfit, 
                                            existing.big.model.fit = TSfit,
                                            dist = dist)
  first.line <- get.table.line(ftest1 = ftest.full.vs.TS, first=TRUE)
  fit.tab[1, 1:8] <- first.line
  # Second line: full APC model
  second.line <- get.table.line(ftest1 = ftest.full.vs.TS, second=TRUE)
  fit.tab[2, 1:8] <- second.line
  # Remaining submodels
  for (i in 3:length(model.design.list)){
    againstTS <- apc.indiv.waldtest.TS(data,  dist = dist,
                                       small.model = model.design.list[i],
                                       dep.var = dep.var,covariates = covariates, 
                                       unit = unit,
                                       existing.big.model.fit = TSfit, 
                                       existing.collinear = fullAPCcollinear)
    againstAPC <- apc.indiv.waldtest.fullapc(data, dist = dist,
                                             small.model = model.design.list[i],
                                             dep.var=dep.var, covariates=covariates, 
                                             unit = unit, 
                                             existing.big.model.fit = fullAPCfit, 
                                             existing.collinear = fullAPCcollinear)
    fit.tab[i, 1:8] <- get.table.line(againstTS, againstAPC)
  }
  # Change table column and row names 
  if(dist=="F"){
    df.againstTS <- paste("DF ( * , ", as.character(againstTS$df.denom), ")", 
                          sep="")
    df.againstAPC <- paste("DF ( * , ", as.character(againstAPC$df.denom), ")",
                           sep="")
    table.colnames <- c("Wald (F) vs TS", df.againstTS, "p-value", "Wald (F) vs APC", 
                        df.againstAPC, "p-value", "AIC", "lik")
  }
  if(dist=="Chisq"){
    table.colnames <- c("Wald (Chisq) vs TS", "Df", "p-value", "Wald (Chisq) vs APC", 
                        "Df", "p-value", "AIC", "lik")
  }
  
  colnames(fit.tab) <- table.colnames
  rownames(fit.tab) <- model.design.list
  ######################	
  valuables <- list(table = fit.tab,
                    NR.report = NULL)
  return(valuables)
}

apc.indiv.LRtest.fullapc <- function(data,  big.model="APC", 
                                     small.model, dep.var, covariates=NULL, 
                                     model.family="binomial", unit=1,
                                     n.coh.excl.start=0, n.coh.excl.end=0,
                                     n.age.excl.start=0, n.age.excl.end=0,
                                     n.per.excl.start=0, n.per.excl.end=0,
                                     existing.big.model.fit=NULL,
                                     existing.small.model.fit=NULL,
                                     existing.collinear=NULL){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function permits comparison of the APC model with one 
  # of its submodels using a likelihood ratio test.
  # ---------------------------------------------------------------------------
  
  
  ######################	
  # step 1: fit reduced model
  
  if(is.null(existing.small.model.fit)){
    if(is.null(existing.collinear)){
      collinear <- apc.indiv.design.collinear(data,  unit,
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    } else
      collinear <- existing.collinear
    design.R <- apc.indiv.design.model(collinear,
                                       model.design = small.model, 
                                       dep.var, covariates)
    fitted.R <- apc.indiv.fit.model(design.R, model.family)
  } else 
    fitted.R <- existing.small.model.fit
  
  ######################	
  # step 2: fit APC model
  if(is.null(existing.big.model.fit)){
    # note: can use same collinear design matrix for full model as for submodel
    if(is.null(collinear)){
      collinear <- apc.indiv.design.collinear(data,  unit,
                                              n.coh.excl.start, n.coh.excl.end,
                                              n.per.excl.start, n.per.excl.end,
                                              n.age.excl.start, n.age.excl.end)
    }
    design.U <- apc.indiv.design.model(collinear,
                                       model.design = big.model, 
                                       dep.var, covariates)
    fitted.U <- apc.indiv.fit.model(design.U, model.family)
  } else 
    fitted.U <- existing.big.model.fit  
  
  ######################	
  # step 3: run LR test
  LRresult <- lrtest(fitted.R$fit, fitted.U$fit)
  diff.lik <- LRresult$LogLik[2] - LRresult$LogLik[1]
  
  ######################	
  # step 4: extract valuables
  valuables <- list(test.type = "LR",
                    dist.type = "Chisq",
                    test.stat = 2*diff.lik,
                    df = LRresult$Df[2],
                    p.value = LRresult$`Pr(>Chisq)`[2],
                    aic.big = fitted.U$fit$aic,
                    aic.small = fitted.R$fit$aic,
                    lik.big = LRresult$LogLik[2],
                    lik.small = LRresult$LogLik[1])
  
  return(valuables)
}

apc.indiv.LRtable <- function(data,  dep.var, covariates=NULL, 
                              model.family, unit=1, 
                              n.coh.excl.start=0, n.coh.excl.end=0, 
                              n.age.excl.start=0, n.age.excl.end=0,
                              n.per.excl.start=0, n.per.excl.end=0){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function generates a table permitting the user to compare the
  # APC model, and all APC models for a given dependent variable and 
  # covariates. It uses a combination of AIC and likelihood ratio tests.
  # ---------------------------------------------------------------------------
  
  # Step 1: function to write table lines
  get.table.line <- function(LRtest.APC, first=FALSE){
    if(isTRUE(first)) # first line is APC model so not compared to itself/APC submodel
      line <- c(NA, NA, NA, round(LRtest.APC$aic.big, 3), 
                round(LRtest.APC$lik.big, 3))
    else
      line <- c(round(LRtest.APC$test.stat, 3), LRtest.APC$df, 
                round(LRtest.APC$p.value, 3), round(LRtest.APC$aic.small, 3),
                round(LRtest.APC$lik.small, 3))
    return(line)
  }
  ######################	
  # Step 2: other setup
  
  # Submodel list
  model.design.list <- c("APC", "AP", "AC", "PC", "Ad", "Pd", "Cd", 
                         "A", "P", "C", "t", "tA", "tP", "tC", "1")
  # Empty table
  fit.tab <- matrix(nrow=length(model.design.list), ncol=5, data=NA)
  # Full APC model for comparison
  fullAPCcollinear <- apc.indiv.design.collinear(data,  unit=unit, 
                                                 n.coh.excl.start = n.coh.excl.start, 
                                                 n.coh.excl.end = n.coh.excl.end,
                                                 n.age.excl.start = n.age.excl.start,
                                                 n.age.excl.end = n.age.excl.end,
                                                 n.per.excl.start = n.per.excl.start,
                                                 n.per.excl.end = n.per.excl.end)
  fullAPCdesign <- apc.indiv.design.model(fullAPCcollinear, dep.var = dep.var, 
                                          covariates = covariates)
  fullAPCfit <- apc.indiv.fit.model(fullAPCdesign, model.family=model.family)
  
  ######################	
  # Step 3: Generate table
  
  # first line, TS model and second line, full APC model
  APCfull.vs.sub1 <- apc.indiv.LRtest.fullapc(data,  small.model = model.design.list[2],
                                              dep.var=dep.var, covariates=covariates, 
                                              model.family = model.family, unit = unit, 
                                              existing.big.model.fit = fullAPCfit, 
                                              existing.collinear = fullAPCcollinear)
  first.line <- get.table.line(LRtest.APC = APCfull.vs.sub1, first=TRUE)
  second.line <- get.table.line(LRtest.APC = APCfull.vs.sub1)
  fit.tab[1, 1:5] <- first.line
  fit.tab[2, 1:5] <- second.line
  # Remaining submodels
  for (i in 3:length(model.design.list)){
    againstAPC <- apc.indiv.LRtest.fullapc(data,  small.model = model.design.list[i],
                                           dep.var=dep.var, covariates=covariates, 
                                           model.family = model.family, unit = unit, 
                                           existing.big.model.fit = fullAPCfit, 
                                           existing.collinear = fullAPCcollinear)
    fit.tab[i, 1:5] <- get.table.line(againstAPC)
  }
  # Change table column and row names
  
  table.colnames <- c("LR-test vs APC", 
                      "df", "p-value", "AIC", "Loglihood")
  colnames(fit.tab) <- table.colnames
  rownames(fit.tab) <- model.design.list
  ####################
  valuables <- list(table = fit.tab)
  return(valuables)
}

apc.indiv.LRtable.TS <- function(data,  dep.var, covariates=NULL, 
                                 model.family, unit=1, 
                                 n.coh.excl.start=0, n.coh.excl.end=0, 
                                 n.age.excl.start=0, n.age.excl.end=0,
                                 n.per.excl.start=0, n.per.excl.end=0,
                                 NR.controls =NR.controls){
  # Zoe Fannon, 26 Jun 2020
  # ---------------------------------------------------------------------------  
  # This function generates a table permitting the user to compare the TS 
  # model, the APC model, and all APC models for a given dependent variable and 
  # covariates. It uses a combination of AIC and likelihood ratio tests.
  # ---------------------------------------------------------------------------
  
  # Step 1: function to write table lines
  get.table.line <- function(LRtest.TS, LRtest.APC, first=FALSE, second=FALSE){
    if(isTRUE(first)) # first line is TS model so not compared to itself/APC submodel
      line <- c(NA, NA, NA, NA, NA, NA, round(LRtest.TS$aic.big, 3), round(LRtest.TS$lik.big, 3))
    else if(isTRUE(second)) # second line is full APC model so not compared to itself
      line <- c(round(LRtest.TS$test.stat, 3), LRtest.TS$df, round(LRtest.TS$p.value, 3), NA,
                NA, NA, round(LRtest.TS$aic.small, 3), round(LRtest.TS$lik.small, 3))
    else
      line <- c(round(LRtest.TS$test.stat, 3), LRtest.TS$df, round(LRtest.TS$p.value, 3), 
                round(LRtest.APC$test.stat, 3),
                LRtest.APC$df, round(LRtest.APC$p.value, 3), round(LRtest.APC$aic.small, 3),
                round(LRtest.APC$lik.small, 3))
    return(line)
  }
  ######################	
  # Step 2: other setup
  
  # Submodel list
  model.design.list <- c("TS", "APC", "AP", "AC", "PC", "Ad", "Pd", "Cd", 
                         "A", "P", "C", "t", "tA", "tP", "tC", "1")
  # Empty table
  fit.tab <- matrix(nrow=length(model.design.list), ncol=8, data=NA)
  # Full APC model for comparison
  fullAPCcollinear <- apc.indiv.design.collinear(data,  unit=unit, 
                                                 n.coh.excl.start = n.coh.excl.start, 
                                                 n.coh.excl.end = n.coh.excl.end,
                                                 n.age.excl.start = n.age.excl.start,
                                                 n.age.excl.end = n.age.excl.end,
                                                 n.per.excl.start = n.per.excl.start,
                                                 n.per.excl.end = n.per.excl.end)
  fullAPCdesign <- apc.indiv.design.model(fullAPCcollinear, dep.var = dep.var, 
                                          covariates = covariates)
  fullAPCfit <- apc.indiv.fit.model(fullAPCdesign, model.family=model.family)
  # TS model for comparison
  if(model.family=="binomial"){
    TSfit <- apc.indiv.logit.TS(data, dep.var=dep.var, covariates=covariates, 
                                NR.controls = NR.controls)
  } else if(model.family=="gaussian"){
    TSfit <- apc.indiv.estimate.TS(data, dep.var=dep.var, covariates=covariates)
  } else
    stop("model.family must be binomial or gaussian")
  
  ######################	
  # Step 3: Generate table
  
  # first line, TS model and second line, full APC model
  LRtest.full.vs.TS <- apc.indiv.LRtest.TS(existing.small.model.fit = fullAPCfit, 
                                           existing.big.model.fit = TSfit,
                                           model.family=model.family)
  first.line <- get.table.line(LRtest.TS=LRtest.full.vs.TS, first=TRUE)
  second.line <- get.table.line(LRtest.TS=LRtest.full.vs.TS, second=TRUE)
  fit.tab[1, 1:8] <- first.line
  fit.tab[2, 1:8] <- second.line
  # Remaining submodels
  for (i in 3:length(model.design.list)){
    againstTS <- apc.indiv.LRtest.TS(data, small.model=model.design.list[i],
                                     dep.var = dep.var,covariates = covariates, 
                                     model.family = model.family, unit = unit,
                                     existing.big.model.fit = TSfit, 
                                     existing.collinear = fullAPCcollinear)
    againstAPC <- apc.indiv.LRtest.fullapc(data,  small.model = model.design.list[i],
                                           dep.var=dep.var, covariates=covariates, 
                                           model.family = model.family, unit = unit, 
                                           existing.big.model.fit = fullAPCfit, 
                                           existing.collinear = fullAPCcollinear)
    fit.tab[i, 1:8] <- get.table.line(againstTS, againstAPC)
  }
  # Change table column and row names
  
  table.colnames <- c("LR-test vs TS", "df", "p-value", "LR-test vs APC", 
                      "df", "p-value", "AIC", "Loglihood")
  colnames(fit.tab) <- table.colnames
  rownames(fit.tab) <- model.design.list
  ####################
  valuables <- list(table = fit.tab,
                    NR.report = againstTS$NR.report)
  return(valuables)
}
