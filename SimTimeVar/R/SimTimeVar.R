


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: PCORI Missing Data
#
# -This program creates covariates for simulated data based on existing VA data
#    (summary stats provided by Vilija at VA, see Q:\Datasets\PCORI\data simulation\real data estimates)
#
# -Using jointly_generate_binary_normal (binNor package) function by Demirtas, simulate clinical characteristics and drug exposure for
#  a large number of patients over time.
#
# -This file loads all necessary functions.
#
# USAGE NOTES
#  1.) If the type field in parameters matrix has "static" in its name, it will be overridden using S' first observation.
#  2.) Must put in "ref" as the beta for one entry in categorical parameters matrix.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TO DO:
#  - complete_parameters should use SDs to fill in variances and vice-versa (not just one-way)
# - in cat.params, do they leave it blank or set to NA? should be the latter. 

# in main doc, say it generates with exchangeable structure (i.e., same wcor for all clusters)



#' An example parameters dataframe
#'
#' An example of how to set up the parameters dataframe. 
#'
#' @docType data
#'
#' @keywords datasets
#'
"params"


#' An example dataframe for categorical variable parameters
#'
#' An example of how to set up the categorical variable parameters dataframe. 
#'
#' @docType data
#'
#' @keywords datasets
#'
"cat.params"

#' An example across-cluster correlation dataframe
#'
#' An example of how to set up the across-cluster correlation dataframe.  
#'
#' @docType data
#'
#' @keywords datasets
#'
"pcor"


#' An example within-cluster correlation dataframe
#'
#' An example of how to set up the within-cluster correlation dataframe.  
#'
#' @docType data
#'
#' @keywords datasets
#'
"wcor"




# # MAKING MAIN EXAMPLE BY SAVING DATASETS AS PART OF PACKAGE
#http://kbroman.org/pkg_primer/pages/data.html

# setwd("~/Dropbox/QSU/Mathur/PCORI/Git/PCORI/r/GENCOV/vignettes")
# 
# params = read.csv("ex1_parameters.csv", header=TRUE)
# cat.params = read.csv("ex1_categorical_parameters.csv", header=TRUE)
# pcor = read.csv("ex1_pcor.csv", header=TRUE)[,-1]
# wcor = read.csv("ex1_wcor.csv", header=TRUE)[,-1]



# save the datasets
# setwd("~/Dropbox/QSU/Mathur/PCORI/Git/PCORI/SimTimeVar/data")
# save(params, file="params.RData")
# save(cat.params, file="cat.params.RData")
# save(pcor, file="pcor.RData")
# save(wcor, file="wcor.RData")

# run the example
# sim = make_one_dataset(n=10, obs=30, n.TBins=2, pcor=pcor, wcor=wcor,
#                        parameters=complete_parameters(params, n=10), cat.parameters=cat.params)
# 







############################## MAIN FUNCTION: GENERATE ONE DATASET ##############################

#' Simulate time-varying covariates
#'
#' Simulates a dataset with correlated time-varying covariates with an exchangeable correlation
#' structure. Covariates can be normal or binary
#' and can be static within a cluster or time-varying. Time-varying normal variables can optionally 
#' have linear trajectories within each cluster. 
#' @param n The number of clusters.
#' @param obs The number of observations per cluster.
#' @param n.TBins Number of time-varying binary variables.
#' @param pcor The across-subject correlation matrix. See Details. 
#' @param wcor The within-subject correlation matrix. See Details. 
#' @param parameters A dataframe containing the general simulation parameters. See Details.
#' @param cat.parameters A dataframe containing parameters for the categorical variables. See Details. 
#' @export
#' @details
#' \strong{SPECIFYING THE PARAMETERS MATRIX}
#' 
#' The matrix \code{parameters} contains parameters required to generate all non-categorical variables.
#' It must contain column names \code{name, type, across.mean, across.SD, across.var, within.var, prop},
#' and \code{error.SD}. (To see an example, use \code{data(params)}.) Each variable to be generated requires
#' either one or two rows in \code{parameters}, depending on the variable type. 
#' 
#' The possible variable types and their corresponding specifications are:
#' \itemize{
#'  \item \strong{Static binary variables} do not change over time within a cluster. For example, if clusters
#'  are subjects, sex would be a static binary variable. Generating such a variable requires a single row
#'  of type \code{static.binary} with \code{prop} corresponding to the proportion of clusters for which the
#'  variable equals 1 and all other columns set to \code{NA}. (The correct standard deviation will automatically
#'  be computed later.) For example, if the variable is an indicator for a subject's being male, then \code{prop}
#'  specifies the proportion of males to be generated. 
#'  
#'  \item \strong{Time-varying binary variables} can change within a cluster over time, as for
#'  an indicator for whether a subject is currently taking the study drug. These variables require two rows in
#'  \code{parameters}. The first row should be of type \code{static.binary} with \code{prop} representing 
#'  the proportion of clusters for which the time-varying binary variable is 1 at least once
#'  (and all other columns set to \code{NA}). For example, this row in \code{parameters} could represent the
#'  proportion of subjects who ever take the study drug ("ever-users").
#'  
#'  The second row should be of type \code{subject.prop} with \code{across.mean} representing, for clusters
#'  that ever have a 1 for the binary variable, the proportion of observations within the cluster for which
#'  the variable is equal to 1. (All other columns should be set to \code{NA}.) For example, this this row in
#'  \code{parameters} could 
#'  represent the proportion of observations for which an ever-user is currently taking the drug. To indicate
#'  which pair of variables go together, the \code{subject.prop} should have the same name as the \code{static.binary}
#'  variable, but with the suffix \code{_s} appended (for example, the former could be named \code{drug_s} and
#'  the latter \code{drug}).
#'  
#'  \item \strong{Normal variables} are normally distributed within a cluster such that the within-cluster
#'  means are themselves also normally distributed in the population of clusters. Generating a normal variable requires
#'  specification of the population mean (\code{across.mean}) and standard deviation (\code{across.SD}) as well as of
#'  the within-cluster standard deviation (\code{within.SD}). To generate a static continuous variable, simply set
#'  \code{within.SD} to be extremely small (e.g., $1 * 10^{-7}$) and all corresponding correlations in matrix
#'  \code{wcor} to 0. 
#'  
#'  \item \strong{Time-function variables} are linear functions of time (with normal error) within each cluster such 
#'  that the within-cluster baseline values are normally distributed in the population of clusters. Generating a
#'  time-function variable requires two entries. The first entry should be of type \code{time.function} and
#'  specifies the population mean (\code{across.mean}) and standard deviation (\code{across.SD}) of the within-cluster
#'  baseline values as well as the error standard deviation (\code{error.SD}). The second entry should be of
#'  type \code{normal} and should have the same name as the \code{time.function} entry, but with the "_s" suffix.
#'  This entry specifies the mean (\code{across.mean}) and standard deviation (\code{across.SD}) of the within-cluster
#'  slopes. 
#' }
#' 
#' \strong{SPECIFYING THE CATEGORICAL PARAMETERS MATRIX}
#' 
#' The matrix \code{cat.parameters} contains parameters required to generate the single categorical variable, 
#' if any.
#' It must contain column names \code{level, parameter},
#' and \code{beta}. (To see an example, use \code{data(cat.params)}.)
#' 
#' \itemize{
#'  \item \strong{The reference level:} Each categorical variable must have exactly one "reference" level. The reference level should have one
#' row in \code{cat.parameters} for which \code{parameters} is set to \code{NA} and \code{beta} is set
#' to \code{ref}. For example, in the example file \code{cat.params} specifying parameters to generate a
#' subject's race, the reference level is \code{white}. 
#'  
#'  \item \strong{ Other levels: } Other levels of the categorical variable will have one or more rows. One row with parameter set to \code{intercept}
#' and \code{beta} set to a numeric value
#' represents the intercept term in the corresponding multinomial model. Any subsequent rows, with parameters set to 
#' names of other variables in the dataset and \code{beta} set to numeric values, 
#' represents other coefficients in the corresponding multinomial models. 
#' }
#' 
#' \strong{SPECIFYING THE POPULATION CORRELATION MATRIX}
#' 
#' Matrix \code{pcor} specifies the population (i.e., across-cluster) correlation matrix. It should have the same
#' number of rows and columns as \code{parameters} as well as the same variable names and ordering of variables.  
#' 
#' \strong{SPECIFYING THE WITHIN-CLUSTER CORRELATION MATRIX}
#' 
#' Matrix \code{wcor} specifies the within-cluster correlation matrix. The order of the variables listed in this file should be
#' consistent with the order in \code{params} and \code{pcor}. However, \code{static.binary} and \code{subject.prop} variables
#' should not be included in \code{wcor} since they are static within a cluster. Static continuous variables should be included,
#' but all the correlations should be set to zero.
#' 
#' @import
#' ICC
#' miscTools
#' car
#' plyr
#' mvtnorm
#' 
#' @examples
#' data = make_one_dataset(n=10, obs=10, n.TBins=2, pcor=pcor, wcor=wcor, 
#' parameters=complete_parameters(params, n=10), cat.parameters=cat.params)$data

# data = make_one_dataset(n=10, obs=10, n.TBins=2, pcor=pcor, wcor=wcor,
#                         parameters=complete_parameters(params, n=10), cat.parameters=cat.params)$data
# 


make_one_dataset = function(n, obs, n.TBins, pcor, wcor, parameters, cat.parameters) {
  
  ### step 1 - extract parameter vectors from given dataframe
  bin.props = parameters$prop[ parameters$type == "static.binary" ] 
  nor.means = parameters$across.mean[ parameters$type %in% c("subject.prop", "normal", "time.function") ]  
  across.vars = parameters$across.var[ parameters$type %in% c("subject.prop", "normal", "time.function") ]  
  static.var.names = parameters$name[ grep("static", parameters$type) & parameters$type != "cat.static" ]  # names of static variables, but not categoricals
  
  ### step 2 - number of different types of variables
  n.BinVars = length( bin.props )  # number binary variables (16)
  n.NormVars = length( nor.means )  # number normal variables
  n.OtherNorms = n.NormVars - n.TBins  # number of non-drug normal variables
  n.OtherBins = n.BinVars - n.TBins  # number of non-drug binary variables
  n.Vars = n.OtherBins + n.OtherNorms + n.TBins  # total number of variables in study (not double-counting drugs)
  
  ## step 3 - convert population cor matrix into vectors to appease Demirtas function
  pcor.vec = upper_tri_vec(pcor)
  

  ### step 4 - generate mu for each person
  mus0 = mod.jointly.generate.binary.normal( no.rows = n, no.bin = n.BinVars, no.nor = n.NormVars,
                                             prop.vec.bin = bin.props,
                                             mean.vec.nor = nor.means,
                                             var.nor = across.vars, corr.vec = pcor.vec )

  ### step 5 - if drug indicator is 0, then convert probability of receiving drug to 0
  mus1 = override_tbin_probs(mus0, n.TBins, n.OtherBins)
  
  ### step 6 - set aside ever-use indicators and expand them
  everUser = mus1[, ( 1:n.TBins ) ]
  everUserExp = expand_matrix(everUser, obs)
  
  ### step 7 - temporarily remove drug indicators from matrix
  mus2 = mus1[, -c( 1:n.TBins ) ]
  
  ### step 8 - "proportionize" normal drug variables (force them to be strictly between 0 and 1)
  bins = mus2[ , (n.OtherBins + 1):(n.OtherBins + n.TBins) ]  # just binaries
  bins.prop = proportionize(bins)  # proportionized version of the binaries
  mus3 = mus2
  mus3[ , (n.OtherBins + 1):(n.OtherBins + n.TBins) ] = bins.prop
  
  ### step 9 - generate time-varying data for each person
  d1 = expand_subjects(mus3, n.OtherNorms, n.OtherBins, n.TBins, wcor, obs, parameters)
  
  ### step 10 - add subject ID, ever-use indicators, and variable names
  id = rep(1:n, each=obs)
  d2 = as.data.frame( cbind(id, d1, everUserExp) )
  
  names = c( "id", as.character( parameters$name[ parameters$type=="static.binary" & !has_drug_suffix( parameters$name ) ] ),
             as.character( parameters$name[ parameters$type=="subject.prop" ] ),
             as.character( parameters$name[ parameters$type %in% c("normal", "time.function") ] ),
             as.character( parameters$name[ has_drug_suffix( parameters$name ) ] ) )
  names(d2) = names
  d3 = d2
  
  
  ### step 11 - add a single categorical variable 
  if (!is.null(cat.parameters)) d4 = add_one_categorical(d3, n, obs, cat.parameters)
  else d4 = d3

  ### step 12 - add time-function variables
  d5 = add_time_function_vars(d4, obs, parameters)
  
  ### step 13 - override static variables
  for (i in static.var.names) {
    d5 = override_static(.static.var.name=i, .id.var.name="id", .d=d5, .obs=obs)
  }
  
  sim = list( "data" = d5, "ever.user" = everUser )
  return(sim)
}


############################## FUNCTION: CHECK FOR DRUG SUFFIX ##############################

#' Checks whether string has "_s" suffix
#'
#' An internal function not intended for the user.
#' @param var.name The string to be checked 
#' @export
#' @examples
#' has_drug_suffix("myvariable_s")
#' has_drug_suffix("myvariable")
has_drug_suffix = function( var.name ){
  var.name = as.character(var.name)
  suffix = substr( var.name, nchar(var.name) - 1, nchar(var.name) )
  return(suffix == "_s")
}


############################## FUNCTION: OVERRIDE DRUG PROBS ##############################

#' Override probabilities for time-varying binary variables
#'
#' An internal function not intended for the user. For clusters assigned to have a given time-varying binary variable
#' always equal to 0,
#' overrides to 0 the corresponding proportion of observations with the binary variable equal to 1.
#' @param mus0 The matrix of cluster means. 
#' @param n.TBins Number of time-varying binary variables.
#' @param n.OtherBins The number of static binary variables. 
#' @param zero A number very close to 0, but slightly larger.  
#' @export
#' @examples
#' # make example subject means matrix for 1 static binary, 
#' #  1 time-varying binary, and 1 normal
#' #  50 subjects and 5 observations (latter plays into variance)
#' set.seed(451)
#' mus0 = mod.jointly.generate.binary.normal( no.rows = 50, no.bin = 2, no.nor = 2,
#'                                            prop.vec.bin = c( .5, .35 ),
#'                                            mean.vec.nor = c( .4, 100 ),
#'                                            var.nor = c( (0.4 * 0.6) / 5, 10 ),
#'                                            corr.vec = c(0.05, .08, 0, 0, -0.03, 0) )
#' 
#' # note that we have ever-users with non-zero propensities to be on drug: not okay
#' any( mus0[,1] == 0 & mus0[,3] != 0 )
#' 
#' # fix them
#' mus1 = override_tbin_probs( mus0, 1, 1 )
#' 
#' # all better!
#' any( mus1[,1] == 0 & mus1[,3] > 0.0001 )

override_tbin_probs = function(mus0, n.TBins, n.OtherBins, zero=0.0001) {
   # number of non-drug variables
   n.NonDrugVars = dim(mus0)[2] - n.TBins
   
   for (m in 1:n.TBins) {
     # replace each entry in the part of mus0 corresponding to the drug propensities with 0...
     # ...if the part of mus0 corresponding to the drug indicator is 0
     # otherwise leave the drug propensity alone
     mus0[, n.TBins + n.OtherBins + m] = ifelse( mus0[,m] == 0, zero, mus0[, n.TBins + n.OtherBins + m])
   }
   
   return(mus0)
 }



############################## FUNCTION: UPPER TRI VEC ##############################

#' Turn symmetric matrix into vector
#'
#' An internal function not intended for the user. Turns a matrix into a vector of the upper-triangular
#' elements (arranged by row). 
#' @param m Matrix
#' @export
#' @examples
#' # make a simple correlation matrix
#' x = rnorm(10); y = rnorm(10); z = rnorm(10)
#' mat = cor( data.frame(x,y,z) )
#' 
#' # turn into into vector
#' upper_tri_vec(mat)

upper_tri_vec = function(m) {
  v1 = as.vector( t(m) )
  keepElement = as.vector( t(upper.tri(m) ) ) #use transpose to avoid going by columns
  v2 = as.numeric( v1[keepElement] )
  return(v2)
}





############################## FUNCTION: PROPORTIONIZE ##############################

#' Turn a number into a valid proportion
#'
#' An internal function not intended for the user. Turns an arbitrary number into a valid proportion
#' by setting the number equal to the closest value in [0,1].
#' @param x The number to be turned into a proportion. 
#' @param zero A very small number that is just larger than 0.
#' @param one A number that is just smaller than 1. 
#' @export
#' @examples
#' proportionize(-0.03)
#' proportionize(1.2)
#' proportionize(.63)
proportionize = function(x, zero=0.00001, one=0.999) {
  y = ifelse(x <= 0, zero, x)
  y = ifelse(y >= 1, one, y)
  return(y)
}



############################## FUNCTION: GENERATE LINEAR PREDICTOR TO GENERATE A VARIABLE ##############################


#' Generate linear predictor from logistic model
#'
#' An internal function not intended for the user. Given a matrix of regression parameters
#' and a dataset, returns the linear predictor based on the given dataset.
#' @param m Part of the parameter matrix for the linear predictor for a single variable.  
#' @param data The dataframe from which to generate. 
#' @export
#' @examples
#' # take part of parameters matrix corresponding to single level of categorical
#' #  variable
#' m = cat.params[ cat.params$level == "black", ]
#' data = data.frame( male = rbinom(n=10, size=1, prob=0.5) )
#' make_one_linear_pred(m, data)

make_one_linear_pred = function(m, data) {
  
  # pull out relevant parameters except intercept
  if ( !any(m$parameter == "intercept") ) stop("Intercept is missing in categorical variable parameter matrix")
  m2 = m[ m$parameter!="intercept", ]  # matrix without intercept
  
  # pull out intercept
  intercept = as.numeric(as.character( m$beta[ m$parameter=="intercept" ] ) )
  
  # calculate linear predictor, not including any error term
  linear.pred = intercept
  for (p in m2$parameter) {
    beta = as.numeric( as.character( m2$beta[ m2$parameter==p ] ) ) # beta is length 1; recycled
    if ( is.null(data[[p]]) ) stop( paste("Parameter ", p, " does not appear in the dataset", sep=""))
    linear.pred = linear.pred + ( data[[p]] * beta )  # linear predictor
  }
  
  # calculate probability of missingness, pi
  pi = exp(linear.pred) / (1 + exp(linear.pred))
  
  if ( any(is.na(linear.pred)) ) warning( cat("\n\nLinear predictor to construct categorical variable
                                              has missing values.\nLinear predictor may have been too large,
                                              leading to Inf values when calculating predicted probability.\n") )
  
  return(linear.pred)
}



############################## FUNCTION: GENERATE ONE CATEGORICAL VARIABLE ##############################

# ADD TO DOC: MUST PUT "REF" IN AS BETA FOR THE REFERENCE LEVEL IN CAT PARAMS MATRIX

#' Generate linear predictor from logistic model
#'
#' An internal function not intended for the user. Given a dataset and multinomial regression parameters,
#' generates a categorical variable and adds it to the dataset. 
#' 
#' @param .d The dataset to which to add the categorical variable. 
#' @param n The number of clusters.
#' @param obs The number of observations per cluster. 
#' @param cat.parameters A dataframe of parameters for generating the categorical variable. See Details. 
#' @import
#' utils
#' @export
#' @examples
#' # mini dataset with 3 observations per person
#' data = data.frame( male = rep( rbinom(n=10, size=1, prob=0.5), each=3 ) )
#' add_one_categorical( data, 10, 3, cat.params)

add_one_categorical = function(.d, n, obs, cat.parameters) {
  
  # extract number of levels and names of levels
  n.levels = length( unique( cat.parameters$level ) )
  levels.to.model = as.character( unique( cat.parameters$level[cat.parameters$beta != "ref"] ) )
  ref.level = as.character( unique( cat.parameters$level[cat.parameters$beta == "ref"] ) )
  
  # split the parameters matrix into separate dataframes for each level to model
  # omit the reference level since it's not being modeled
  split = dlply( cat.parameters[ cat.parameters$beta != "ref", ], .(cat.parameters[ cat.parameters$beta != "ref", ]$level) )
  # below is the version used prior to CRAN submission
  # simpler syntax but generates global variable warning
  #split = dlply( cat.parameters[ cat.parameters$beta != "ref", ], .(level) )
  
  # calculate exponentiated linear predictor for each race and put in dataframe
  exp.lin.preds = as.data.frame( lapply( split, function(x) exp( make_one_linear_pred(x, .d) ) ) )
  
  # make dataframe that will hold probabilities of being each level
  probs = as.data.frame( matrix( nrow=nrow(exp.lin.preds), ncol=n.levels ) )
  names(probs) = c(levels.to.model, ref.level)
  
  # make vector of denominators for each subject's race probabilities
  # 1 + sum of exponentiated linear predictors for all levels
  denoms = 1 + apply(exp.lin.preds, MARGIN=1, FUN=sum)
  
  # for each level EXCEPT the reference, fill in subject's probability
  # note that these will NOT necessarily be sum to 1 because rmultinom internally does that
  for (i in 1:(n.levels-1) ) {
    probs[,i] = exp.lin.preds[,i] / denoms
  }
  
  # for reference level (last column)
  probs[,n.levels] = 1 / denoms
  
  # generate the categorical variable
  obs = nrow(probs)
  cat.var = vector("list", obs)   # create vector of lists for each observation's indicators
  #for (j in 1:obs)  cat.var[[j]] = rmultinom(1, 1, probs[j,])
  #cat.var = t(do.call("cbind", cat.var))
  
  # tiny helper function
  lstyle = function(x, n, size, prob){
    rmultinom(n, size, prob[x,])
  }
  
  cat.var = lapply(X = 1:obs, FUN = lstyle, n = 1, size = 1, prob = probs)
  cf <- data.frame(matrix(unlist(cat.var), nrow=length(cat.var), byrow=T))  # turn list into dataframe by row
  names(cf) = row.names(cat.var[[1]])
  
  # put new variables in dataframe
  return( cbind(.d, cf ) )
}







############################## FUNCTION: OVERRIDE A STATIC VARIABLE ##############################

# use this at the very end of repeat_sim to override sex, race, etc.
# have a column in parameters matrix where you can specify that it's static
# also in categorical params matrix

# if the categorical is supposed to only depend on baseline, this is already handled b/c
#  we are going to keep only the first observation.


#' Override static variable
#'
#' An internal function not intended for the user. For static variables, overrides any time-varying
#' values to ensure that they are actually static. 
#' 
#' @param .static.var.name Name of static variable.
#' @param .id.var.name Name of variable defining clusters in dataset.
#' @param .d Dataset
#' @param .obs The number of observations per cluster. 
#' @export
#' @examples
#' # example with 10 subjects each with 3 observations
#' # generate sex in a way where it might vary within a subject
#' data = data.frame( id = rep(1:10, each=3),
#'                    male = rbinom( n=10*3, size=1, prob=0.5 ) )
#' override_static("male", "id", data, 3)

override_static = function(.static.var.name, .id.var.name="id", .d, .obs) {
  # first observations over this variable for each subject
  .first.obs = .d[[ .static.var.name ]][ !duplicated( .d[[ .id.var.name ]] ) ]
  
  # expand for the number of observations by subject and replace in dataframe
  .d[[ .static.var.name ]] = rep(.first.obs, each=.obs)
  return(.d)
}



############################## FUNCTION: EXPAND SUBJECTS ##############################

#' Longitudinally expand a cluster
#'
#' An internal function not intended for the user. Given a matrix of cluster means for each variable to
#' be simulated, "expands" them into time-varying observations. 
#' 
#' @param mus3 A matrix of cluster means for each variable. 
#' @param n.OtherNorms The number normal variables (not counting those
#' used for generating a time-varying binary variable).
#' @param n.OtherBins The number of static binary variables. 
#' @param n.TBins The number of time-varying binary variables.  
#' @param wcor The within-cluster correlation matrix.
#' @param obs The number of observations to generate per cluster.
#' @param parameters The parameters dataframe. 
#' @param zero A small number just larger than 0. 
#' @export
#' @examples
#' # subject means matrix (normally would be created internally within make_one_dataset)
#' mus3 = structure(c(1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1e-04, 1e-04, 0.886306145591761, 
#' 1e-04, 1e-04, 1e-04, 1e-04, 0.875187001140343, 0.835990583043838, 
#' 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
#' 1e-04, 1e-04, 69.7139993804559, 61.3137637852213, 68.3375516615242, 
#' 57.7893277997516, 66.3744152975352, 63.7829561873355, 66.3864252981679, 
#' 68.8513253460358, 67.4120718557, 67.8332265185068, 192.366192293195, 
#' 128.048983102048, 171.550401133259, 120.348392753954, 158.840864356998, 
#' 170.13484760994, 113.512220330821, 162.715528382999, 138.476877345895, 
#' 159.841096973242, 115.026417822477, 109.527137142158, 117.087914485084, 
#' 121.153861460319, 109.95973584141, 122.96960673409, 90.5100006255084, 
#' 107.523229006601, 108.971677388246, 115.641818648526, -4.33184270434101, 
#' -5.45143483618415, -2.56331188314257, -1.38204452333064, -1.61744564863871, 
#' 1.83911233741448, 2.0488338883998, -0.237095062415858, -5.47497506857878, 
#' -3.53078955238741), .Dim = c(10L, 7L))
#'
#' expand_subjects( mus3 = mus3, n.OtherNorms = 4, n.OtherBins = 1, n.TBins = 2,
#'                 wcor = wcor, obs = 3, parameters = complete_parameters(params, n=10) )

expand_subjects = function(mus3, n.OtherNorms, n.OtherBins, n.TBins, wcor, obs, parameters, zero=0.0001) {
  
  # number of subjects
  n = dim(mus3)[1]
  
  # total number of variables
  n.Vars = n.OtherNorms + n.OtherBins + n.TBins
  
  dat = vector("list", n)
  
  for (s in 1:n) {
    staticBins = mus3[ s, 1:n.OtherBins ] # subset the matrix to get static (non-time-varying) binaries for subject s
    drugProbs = mus3[ s, (n.OtherBins + 1):(n.OtherBins + n.TBins) ] # subset the matrix to get binaries for subject s
    normMeans = mus3[ s, (n.OtherBins + n.TBins + 1) : ncol(mus3) ] # subset the matrix to get the non-drug normals for subject s
    
    zerodrugs = which(drugProbs==zero) # which drugs have p = 0?
    
    # within-subject correlation matrix
    wcorin2 = as.matrix(wcor)
    
    # change the correlations to 0s where pdrugs = zero
    # **SHOULD WE ALSO DO THIS WHEN PDRUGS = 1? DOES THIS EVEN HAPPEN?**
    for (r in zerodrugs){
      wcorin2[,r] = 0
      wcorin2[r,] = 0
    }
    
    # convert correlation matrix to vector of just upper-tri elements
    newwcorvec = upper_tri_vec(wcorin2)
    
    # create a list with a dataset (of length obs) for each subject
    dat[[s]] = mod.jointly.generate.binary.normal(no.rows=obs, no.bin=length(drugProbs), no.nor=length(normMeans),
                                                  prop.vec.bin=drugProbs, mean.vec.nor=normMeans,
                                                  var.nor=parameters$within.var[parameters$type == "normal" |
                                                                                  parameters$type == "time.function"],
                                                  corr.vec=newwcorvec, adjust.corrs=T)
    
    # put non-time-varying binaries back in
    dat[[s]] = cbind( staticBins, dat[[s]] )
  }
  
  dat = do.call("rbind", dat)
  return(dat)
}




######################### FUNCTION: LONGITUDINALLY EXPAND MATRIX #########################


##### Function: longitudinally expand a matrix of single observations by subject
# repeat each subject's entry in each row for obs number of times

#' Longitudinally expand a matrix of single observations by cluster
#'
#' An internal function not intended for the user. Given a matrix of single observations 
#' for a cluster, repeats each cluster's entry in each \code{.obs} times.  
#' 
#' @param .matrix The matrix of observations to be expanded.
#' @param .obs The number of observations to generate per cluster. 
#' @import plyr
#' @export
#' @examples
#' mat = matrix( seq(1:10), nrow=2, byrow=FALSE)
#' expand_matrix(mat, 4)

expand_matrix = function(.matrix, .obs) {
  #cat("Expanding a matrix\n")
  
  .n = nrow(.matrix)
  .expanded = matrix(c(NA), nrow = .n*.obs, ncol = ncol(.matrix) )
  
  adply(.matrix, 1, function(..subject, ..obs) {
    matrix(rep(..subject, ..obs), nrow = ..obs, byrow = TRUE)
  }, .obs)[ , -1]
  
}



######################### FUNCTION: COMPLETE PARAMETERS DATAFRAME GIVEN N #########################

#' Fill in partially incomplete parameters matrix
#'
#' Fills in "strategic" \code{NA} values in a user-provided parameters matrix by (1) calculating
#' SDs for proportions using the binomial distribution; (2) calculating variances based on SDs; and (3)
#' setting within-cluster variances to 1/3 of the across-cluster variances (if not already specified).
#' 
#' @param parameters Initial parameters matrix that may contain \code{NA} values. 
#' @param n The number of clusters
#' @export
#' @details
#' For binary variables, uses binomial distribution to compute across-cluster standard deviation of proportion. Where there 
#' are missing values, fills in variances given standard deviations and vice-versa. Where there are missing values in 
#' \code{within.var}, fills these in by defaulting to 1/3 of the corresponding across-cluster variance. 
#' @examples
#' complete_parameters(params, n=10)

complete_parameters = function(parameters, n) {
  
  # calculate SDs for proportions based on sample size
  parameters$across.SD[parameters$type=="subject.prop"] = sqrt( parameters$across.mean[parameters$type=="subject.prop"]
                                                                * (1-parameters$across.mean[parameters$type=="subject.prop"]) / n )
  # use SDs to calculate vars
  parameters$across.var = parameters$across.SD ^ 2
  
  # if needed, use vars to calculate SDs
  parameters$across.SD[ is.na( parameters$across.SD ) &
                          !is.na( parameters$across.var ) ] = sqrt( parameters$across.var )
  
  # set within-subject variance to 1/3 of across-subject variance if not already specified
  var.index = parameters$type %in% c("normal", "time.function")  # index of variables to consider
  within.var.vector = parameters$within.var[var.index]
  across.var.vector = parameters$across.var[var.index]
  within.var.vector[ is.na(within.var.vector) ] = across.var.vector[ is.na(within.var.vector) ] / 3
  parameters$within.var[var.index] = within.var.vector
  
  return(parameters)
}


######################### FUNCTION: MAKE A COVARIATE AS A FUNCTION OF TIME #########################

# given slope and intercept for this subject, increase the variable as a function of time
# and add random error ~ N(0, error.SD)

# change parameters matrix so that it has a normal.time option for type and an error.SD column for these


#' Creates linear time-function variables
#'
#' Given variable-specific slopes and intercepts for a cluster, creates continuous variables that
#' increase or 
#' decrease linearly in time (with normal error with standard deviation \code{error.SD}) and
#' adds them to the dataframe.
#' 
#' @param d4 The dataframe to which to add the time-function variables.
#' @param obs The number of observations per cluster. 
#' @param parameters The parameters matrix. 
#' @export
#' @details See \code{make_one_dataset} for additional information. 

add_time_function_vars = function(d4, obs, parameters) {
  
  # if no time-function variable, return dataframe unchanged
  if ( length( parameters$type[parameters$type == "time.function"] ) == 0 ) return(d4)
  
  # extract time-function variables
  time.list = parameters$name[parameters$type == "time.function"]
  first.dat = d4[!duplicated(d4$id),]
  time.vars = as.data.frame( first.dat[ , names(first.dat) %in% time.list ] )
  names(time.vars) = parameters$name[parameters$type=="time.function"]  # needed in case there's only 1 time function variable, in which case the subset above is an unnamed vector
  
  # extract parameters for time-function variables
  error.SD = parameters$error.SD[parameters$type == "time.function"]
  
  d5 = d4
  
  for ( s in unique(d4$id) ) {  # for each subject
    for ( i in 1:ncol(time.vars) ) {  # for each time-function variable
      
      # intercept is the first observation generated for this subject
      intercept = time.vars[s, i]
      
      # pull slope from first.dat
      # what variable name are we looking for?
      # assumes the slope variables are named XXX_slope where XXX is corresponding normal variable
      var.name = paste( names(time.vars)[i], "_slope", sep="" )
      # pull the generated slopes for subject s and take the first one
      subj.slope = d4[[var.name]][d4$id == s][1]
      
      # generate error vector for all obs
      # error ~ N(0, error.SD)
      errors = rnorm(n=obs, mean = 0, sd = error.SD[i])
      
      # compute vector of observations for this subject as a linear function of time
      v = intercept + subj.slope*seq( 0, (obs-1) ) + errors
      
      # put in d5
      var.index = which(names(d5)==names(time.vars)[i])  # column index for the variable we're doing
      d5[d5$id==s, var.index] = v
    }
  }
  return(d5)
}






###### Function: closest element ######
#returns whichever element of candidates vector is closest in abs value to x

#' Return closest value
#'
#' An internal function not intended for the user. Given a number \code{x} and vector of
#'  permitted values, 
#' returns the closest permitted value to \code{x} (in absolute value).
#' 
#' @param x The number to be compared to the permitted values. 
#' @param candidates A vector of permitted values. 
#' @export
#' @examples
#' closest( x = 5, candidates = c(-3, 8, 25) )

closest = function(x, candidates) {
  return( candidates[ which.min( abs(candidates - x) ) ] )
}


###### Function: binary-normal correlation bound ######
# given parameters for 1 binary and 1 normal RV, return the maximum correlation

#' Maximum correlation between binary and normal random variables
#'
#' Given parameter \code{p} for a Bernoulli random variable, returns its maximum possible
#' correlation with an arbitrary normal random variable. Used to adjust correlation matrices
#' whose entries are not theoretically possible. 
#' 
#' @param p Parameter of Bernoulli random variable. 
#' @import stats
#' @export
#' @examples
#' # find the largest possible correlation between a normal
#' #  variable and a binary with parameter 0.1
#' BN.rBound(0.1)

BN.rBound = function(p) {
  q = 1-p
  
  #compute upper bound
  hiBound = dnorm( qnorm(p) ) / sqrt(p*q)
  return( round(hiBound, 2) )
}




################# DEMIRTAS FUNCTIONS

#This function implements the algorithm that forms a basis
#for the paper entitled "Simultaneous generation of binary and normal 
#data with specified marginal and association structures".

#Load necessary libraries


#' Return closest value
#'
#' An internal function not intended for the user. Simulates correlated normal and binary
#' variables based on the algorithm of Demirtas and Doganay (2012). See references for
#' further information. 
#' 
#' @param no.rows Number of rows
#' @param no.bin Number of binary variables
#' @param no.nor Number of normal variables
#' @param prop.vec.bin Vector of parameters for binary variables
#' @param mean.vec.nor Vector of means for binary variables
#' @param var.nor Vector of variances for binary variables
#' @param corr.vec Vector of correlations
#' @param adjust.corrs Boolean indicating whether theoretically impossible correlations between 
#' a binary and a normal variable should be adjusted to their closest theoretically possible value.  
#' @import
#' corpcor
#' mvtnorm
#' @importFrom
#' psych phi2poly
#' @export
#' @references
#' Demirtas, H., & Doganay, B. (2012). Simultaneous generation of binary and
#' normal data with specified marginal and association structures. Journal of
#' Biopharmaceutical Statistics, 22(2), 223-236.
mod.jointly.generate.binary.normal=function(no.rows,no.bin,no.nor,
                                            prop.vec.bin,mean.vec.nor,var.nor,corr.vec, adjust.corrs = TRUE){
  ###########################################################
  #THIS R FUNCTION IMPLEMENTS THE METHODOLOGY IN SECTION 3
  #For bug reporting, please contact the first author
  ###########################################################
  ###########################################################
  #Definition of the arguments are as follows:
  #no.rows=Number of rows
  #no.bin=Number of binary variables
  #no.nor=Number of normally distributed variables
  #prop.vec.bin=Vector of marginal proportions for binary variables
  #mean.vec.nor=Vector of means for normal variables
  #var.nor=Vector of variances for normal variables
  #corr.vec=Specified correlations among all variables
  d=no.bin+no.nor #d is the total dimension
  #adjust.corrs=T/F. If a correlation is out of bounds, should it
  # be adjusted to the closest feasible values? 
  
  ############################################################
  #Important note 1: For convenience, binary variables are assumed 
  #to come first, then normal variables follow
  #Important note 2: Correlations are specified in vector form,
  #rather than a matrix form. If the dimension is d, d*(d-1)/2
  #non-redundant correlation terms must be specified. The order
  #in which correlations are specified is based on the upper diagonal
  #elements. For example, if there are four variables (X1,X2,X3,X4),
  #corr.vec is specified in the following form:
  #c(Corr(X1,X2),Corr(X1,X3),Corr(X1,X4),Corr(X2,X3),Corr(X2,X4),
  #Corr(X3,X4)) 
  ############################################################
  #Series of control statements to prevent obvious ARGUMENT
  #SPECIFICATION ERRORS:
  
  if ((no.rows<1)|(floor(no.rows)!=no.rows)){stop("Number of rows must be
                                                  an integer whose value is at least 1!\n")}
  if ((no.bin<1)|(floor(no.bin)!=no.bin)){stop("Number of binary variables 
                                               must be an integer whose value is at least 1!\n")}
  if ((no.nor<1)|(floor(no.nor)!=no.nor)){stop("Number of normal variables 
                                               must be an integer whose value is at least 1!\n")}
  
  if ((min(prop.vec.bin)<=0)|(max(prop.vec.bin)>=1)){
    stop("Proportions for binary variables must be between 0 and 1!\n")}
  if (length(prop.vec.bin)!=no.bin){stop("Proportion vector 
                                         is misspecified, dimension is wrong!\n")}
  
  if (length(mean.vec.nor)!=no.nor){
    stop("Mean vector for the normal part is misspecified, 
         dimension is wrong!\n")}
  if (length(var.nor)!=no.nor){
    stop("Vector of variances for the normal part is misspecified, 
         dimension is wrong!\n")}
  if (min(var.nor<=0)){stop("Variances must be positive!\n")}
  
  if(length(corr.vec)!=(d*(d-1)/2)){
    stop("Vector of correlations is misspecified, dimension is wrong!\n")}
  
  ###################################################################
  #Statements to check CORRELATION BOUND VIOLATIONS
  
  #Form a correlation matrix from the specified correlation vector
  sigma=diag(d)
  temp=1
  for(i in 1:(d-1)){
    for(j in (i+1):d){
      sigma[i,j]=sigma[j,i]=corr.vec[temp] 
      temp=temp+1        
    }
  }
  
  #Check if the specified correlation matrix is positive definite, if not
  #find the nearest positive definite matrix (Step 2 in the algorithm)
  
  if(is.positive.definite(sigma)==FALSE)
  {sigma=make.positive.definite(sigma)
   print("Specified correlation matrix is not positive definite,")
   print("Algorithm will be using the closest positive definite matrix!")}
  
  diag(sigma)=1
  
  p=prop.vec.bin
  q=1-p
  
  #Check if the correlations for binary-binary combinations are
  #in the feasible range (Step 3 in the algorithm)
  
  #Boundaries for BB =[max(-sqrt((pi*pj)/(qi*qj)),-sqrt((qi*qj)/(pi*pj))),
  #min(sqrt((pi*qj)/(qi*pj)),sqrt((qi*pj)/(pi*qj)))]
  L_BB=diag(no.bin)
  U_BB=diag(no.bin)
  
  for(i in 1:no.bin){
    for(j in 1:no.bin){
      if (i!=j) L_BB[i,j]=L_BB[j,i]=max(-sqrt((p[i]*p[j])/(q[i]*q[j])),
                                        -sqrt((q[i]*q[j])/(p[i]*p[j])))
      if (i!=j) U_BB[i,j]=U_BB[j,i]=min(sqrt((p[i]*q[j])/(q[i]*p[j])),
                                        sqrt((q[i]*p[j])/(p[i]*q[j])))
    }
  }
  
  
  for(i in 1:no.bin){
    for(j in 1:no.bin){
      if(sigma[i,j]<L_BB[i,j] | sigma[i,j]>U_BB[i,j]) {
        if (!adjust.corrs) {
          # if user does not want to adjust correlations, give error
          stop("BB corrrelation [", i,",",j,"] is out of range! Specify a feasible number!")  
        } else {
          #adjust correlation to the closest feasible value
          cat( c("BB corrrelation [", i,",",j,"],", sigma[i,j], ", is out of range! Used closest feasible correlation instead\n"))
          sigma[i,j] = sigma[j,i] = closest(sigma[i,j], c( L_BB[i,j], U_BB[i,j] ) )
        }
      }
    }
  }
  
  
  #Compute the biserial correlations for binary-normal combinations and 
  #check if they are in the feasible range (Steps 4 and 6 in the algorithm)
  
  #temporary matrix
  BN_temp=sigma
  
  # replace the BN values in BN_temp with the corresponding phi values
  for(i in (no.bin+1):d){
    for(j in 1:no.bin){
      BN_temp[i,j]=BN_temp[i,j]/(dnorm(qnorm(p[j]))/sqrt(p[j]*q[j]))
    }
  } 
  
  for(i in (no.bin+1):d){
    for(j in 1:no.bin){
      if (BN_temp[i,j]< -1 | BN_temp[i,j]> 1) {
        
        if (!adjust.corrs) {q
                            # if user does not want to adjust correlations, give error
                            stop("BN correlation [", i,",",j,"] is out of range! Specify a feasible number!")  
        } else {
          #adjust correlation to the closest feasible value
          BN_temp[i,j] = closest(BN_temp[i,j], c(-1, 1))
          
        } 
      }   
    }
  }
  
  #keep the BN part of BN_temp matrix
  BN=BN_temp[(no.bin+1):d,1:no.bin]
  
  #Compute the tetrachoric correlations for binary-binary combinations
  #(Step 5 in the algorithm)
  
  # create sigmaBB matrix by converting BB part of sigma into polychoric correlations
  sigmaBB=diag(no.bin)
  for(i in 1:no.bin){
    for(j in 1:no.bin){
      if (i > j) {
        sigmaBB[i,j] = sigmaBB[j,i] = phi2poly( sigma[i,j] ,p[i],p[j])
        #force symmetry because phi2poly is an imperfect optimization process with rounding error
      }
      #########################################################################
      ###### NOTE: ABOVE ROUNDING OF SIGMA ENTRY IS A LITTLE SKETCH!!!!! ######
      #########################################################################
      
    } 
  }
  
  
  #Biserial correlations for binary-normal combinations
  sigmaBN=BN
  
  #Combine all three types (binary-binary, binary-normal, normal-normal)
  #of correlations to construct the overall correlation matrix 
  #(Step 7 in the algorithm)
  sigma_new=sigma
  sigma_new[1:no.bin,1:no.bin]=sigmaBB
  sigma_new[(no.bin+1):d,1:no.bin]=sigmaBN
  sigma_new[1:no.bin,(no.bin+1):d]=t(sigmaBN)
  
  
  #Check if the final correlation matrix is positive definite, if not
  #find the nearest positive definite matrix (Step 8 in the algorithm)
  
  if(is.positive.definite(sigma_new)==FALSE) {
    sigma_new=make.positive.definite(sigma_new)
    print("Final correlation matrix is not positive definite,")
    print("Algorithm will be using the closest positive definite matrix!")
  }
  
  
  #Generate multivariate normal data (Step 9 in the algorithm)
  data=rmvnorm(no.rows,mean=rep(0,d), sigma=sigma_new)
  
  #Obtain binary variables by the thresholds determined by marginal proportions
  #(Step 10 in the algorithm)
  
  
  for(i in 1:no.rows){
    for(j in 1:no.bin){
      if(data[i,j]<=qnorm(1-p[j])) data[i,j]=0 else data[i,j]=1
    } 
  }
  
  
  #Go back to the original scale for normal variables by reverse centering and
  #scaling (Step 11 in the algorithm)
  
  for(i in 1:no.rows){
    temp=1    
    for(j in (no.bin+1):d){
      data[i,j]=mean.vec.nor[temp]+(data[i,j]*sqrt(var.nor[temp]))    
      temp=temp+1
    } 
  }
  
  #Output is the data matrix!
  return(data)
  }

#################################################################
#################################################################


