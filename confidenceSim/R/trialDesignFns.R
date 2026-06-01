# Functions used in the design and simulation of confidence trials


#' Get Group Sequential Design
#' @description Generate boundaries for a group sequential design using a one-sided test
#' @details To generate confidence-based thresholds, we are interested in a one-sided test and alpha is set at 0.025.
#' To generate the stopping thresholds, specify either looks or information rates, and the alpha spending function if
#' difference from O-Brien-Fleming-type.
#'
#'
#' @param info.rates Analysis times expressed as rate of information accrual. Expects a vector
#' with the last item representing the final analysis and equal to 1.
#' For example, information rates for two-stage trial with interim analysis half way through is c(0.5, 1).
#' One of two options for expressing analysis times. Either 'info.rates' or 'looks' must be specified.
#' @param looks Analysis times expressed by number of patients accrued at each point. Expects a vector
#' with the last item being equal to the maximum sample size. For example.
#' looks for a three stage trial with maximum sample size of 300 and analysis planned every 100 patients is c(100, 100, 100).
#' One of two options for expressing analysis times. Either 'info.rates' or 'looks' must be specified.
#' @param as.type Time of alpha spending function to use. Options are as outlined by 'rpact'.
#' Default is 'asOF' \(O'Brien-Fleming-type\).
#'
#' @return Returns an 'rpact' TrialDesign object.
#' @seealso [rpact::getDesignGroupSequential()]
#' @export
#'
#' @examples
#' # calculate critical values for a two-stage trial with an interim analysis half-way through
#' # Use Pocock-type alpha spending
#' design <- getGSDesign(info.rates = c(0.5, 1), as.type = 'asP')
#' critical.stagewise.alpha.levels <- design$stageLevels
#'
#' # calculate values for a 6-stage trial with a maximum sample size of 1000
#' # interim analysis begins at 500 patients accrued and continues every 100 patients after
#' design <- getGSDesign(looks = seq(500, 1000, 100))

getGSDesign <- function(info.rates=NULL, looks=NULL,  as.type="asOF"){

  if (is.null(looks) & is.null(info.rates)){
    stop("Looks or information rates is required.")
  }
  if (!is.null(looks)) {
    num.looks = length(looks)
    t = looks/max(looks) #
  }
  if (!is.null(info.rates)){
    t = info.rates
  }
  if (! as.type %in% c("OF", "P", "WT", "PT", "HP", "WToptimum", "asP", "asOF", "asKD",
                       "asHSD", "asUser", "noEarlyEfficacy")){
    message(paste0("Alpha spending option ",as.type, " not available. Changing to default (asOF)."))
    as.type = "asOF"
  }
  # using a one-sided test for confidence threshold
  alpha = 0.025
  sided = 1
  design = rpact::getDesignGroupSequential(
    sided=sided,
    alpha=alpha,
    informationRates = t,
    typeOfDesign = as.type
  )
  return(design)
}


#' Get Confidence Levels from Group Sequential Bounds
#' @description Derive confidence-based decision thresholds for efficacy and inferiority from a group sequential design.
#' @param design TrialDesign object generated from 'getGSDesign'.
#' @seealso [getGSDesign()]
#'
#' @return List of values:
#' \itemize{
#' \item{critical.values: Critical z-levels at each stage}
#' \item{alpha.spending.one.sided: One-sided alpha critical values for each stage}
#' \item{alpha.spending.cumulative: One-sided alpha accumulated by each stage}
#' \item{confidence.threshold.efficacy: Upper bounds for confidence i.e. declare efficacy if exceeded}
#' \item{confidence.threshold.inferiority: Lower bounds for confidence i.e. declare inferiority if below}
#' }
#'
#' @export
#'
#' @examples
#' # confidence bounds for a 6-stage trial with a maximum sample size of 1000
#' bounds <- getConfidenceFromBounds(getGSDesign(looks=seq(500, 1000, 100)))

getConfidenceFromBounds <- function(design){
  if (typeof(design)!="environment"){
    stop("Function expecting TrialDesign object.")
  }
  # for efficacy
  confidence.threshold = 1 - design$stageLevels
  # for inferiority, confidence thresholds are p-values
  bounds=list(critical.values=design$criticalValues,
              alpha.spending.one.sided=design$stageLevels,
              alpha.spending.cumulative=design$alphaSpent,
              confidence.threshold.efficacy=confidence.threshold,
              confidence.threshold.inferiority=design$stageLevels)
  return(bounds)
}


#' Get Critical Bounds from Confidence Thresholds
#'
#' @description
#' Derive traditional frequentist critical values from frequentist confidence thresholds for confidence in treatment benefit.
#'
#' @details
#' During a confidence trial, efficacy and inferiority is determined by the level of confidence in treatment benefit.
#' Efficacy is declared if this confidence level exceeds a pre-specified boundary,
#' and inferiority is declared if this confidence levels falls below a second pre-specified valye.
#' Given confidence-based thresholds for efficacy and inferiority, and the
#' sidedness of the test, this function returns the traditional frequentist p-value.
#'
#'
#' @param num.treat.arms Number of treatment arms (excludes control). Default is 2.
#' @param conf.lower Confidence in treatment benefit boundary for inferiority i.e. stop for inferiority if confidence in benefit is below this.
#' Default is 0.01.
#' @param conf.upper Confidence in treatment benefit boundary for efficacy i.e. stop for efficacy if confidence in benefit is above this.
#' Default is 0.99.
#' @param p.sided Sidedness of statistical test, 1 (one-sided) and 2 (two-sided). Default is 1.
#'
#' @return List of values:
#' \itemize{
#' \item{conf.lower: confidence in treatment benefit lower bound}
#' \item{z.score.lower: critical value corresponding to lower confidence bound}
#' \item{p.value.lower: p-value correponding to lower confidence bound}
#' \item{conf.upper: confidence in treatment benefit upper bound}
#' \item{z.score.upper: critical value corresponding to upper confidence bound}
#' \item{p.value.upper: p-value corresponding to upper confidence bound}
#' \item{p.value: sidedness of test}
#' }
#'
#' @export
#' @importFrom stats qnorm
#'
#' @examples
#' # Running the function on default values
#' bounds <- getBoundsFromConfidence()
#'
#' # to make adjustments for multiple arms
#' bounds <- getBoundsFromConfidence(num.treat.arms = 3)
getBoundsFromConfidence <- function(num.treat.arms=2,
                                    conf.lower=0.01,
                                    conf.upper=0.99,
                                    p.sided=1
){
  # If there are more than three treatment arms, the lower threshold is lower for each arm
  if(num.treat.arms <=2){
    conf.lower = conf.lower
  } else if (num.treat.arms >2){
    conf.lower = conf.lower/2
  }
  critical.value.upper = qnorm(conf.upper)
  critical.value.lower = qnorm(conf.lower)
  if(p.sided == 1){
    p.value = 'one-tailed'
    p.upper = 1 - conf.upper
    p.lower = conf.lower
  } else if(p.sided == 2){
    p.value = 'two-tailed'
    p.upper = (1 - conf.upper)*2
    p.lower = conf.lower * 2
  }
  bounds = list(conf.lower=conf.lower, z.score.lower=critical.value.lower,
                p.value.lower=p.lower, conf.upper=conf.upper, z.score.upper=critical.value.upper,
                p.value.upper=p.upper, p.value=p.value)
  return(bounds)
}


# get a parameter list to generate trial
#' Get Parameter List
#' @description
#' Generate a parameter list to generate a frequentist confidence trial
#'
#' @param looks Vector of analysis times expressed by either number of patients accrued at each point or by rate of information accumulated.
#' If the former, last item should be the maximum sample size.
#' If the latter, last item is 1.
#' Expects a vector with length equal to the total number of total looks.
#' @param nmax Maximum sample size, specified if information rates are used for 'looks'.
#' @param perpetual Whether to run the trial perpetually (TRUE) or not (FALSE).
#' If TRUE, new treatment arms will be added when treatment arms are dropped, until there are no more arms left.
#' All treatments are included via the 'resprate' parameter. Default is FALSE.
#' @param alloc.ratio Allocation ratios for study arms relative to each other.
#' Expects vector with length equal to number of arms including control.
#' First number corresponds to control ratio.
#' @param num.per.block Number from each arm per block, for blocked randomization to balance co-variates.
#' Block size is 'sum(num.per.block)'. If a single number is provided, it will be assume to apply to each arm.
#' @param final.visit The number of days after intervention when the response information becomes available.
#' Default assumes immediate follow-up (0).
#' @param as.type The type of alpha spending function to use in group sequential design.
#'  Default is 'asOF', O'Brien-Fleming-type.
#' @param alpha The alpha threshold to apply to each pairwise comparison to control in the final analysis.
#' Used together with the 'MONITOR FUTILITY', when an alpha spending function is not needed.
#' Default is 0.05, assuming a two-sided test.
#' @param multiarm.mode For multiple treatment arms, describes how arms are evaluated at each stage:
#' \itemize{
#' \item{"CONFIDENCE-BASED"(default): Evaluate arms against confidence-based rules}
#' \item{"DROP WORST": Drop the worst performing arm, and carry the remaining promising arms}
#' \item{"SELECT BEST": Select the best performing arm to carry forward, drop the rest}
#' \item{"ALL PROMISING": Carry forward all promising arms}
#' \item{"MONITOR FUTILITY": Only monitor for futility}
#' }
#' @param lmb.threshold Defined threshold for meaningful benefit. The direction of benefit/lacks benefit
#' depends on the data and outcome, and whether lower or higher is better. For ordinal data, lower is better and anything
#' greater than lmb.threshold lacks meaningful benefit. In that case we use a `genodds` estimator and `lmb.threshold` should be
#' below 1. For binary and continous data, higher is better and `lmb.threshold` should reflect that.
#' If the treatment effect is a ratio, it will be later converted to the logarithmic scale for confidence analysis.
#' @param lmb.conf.thresh Confidence threshold for futility. If confidence in lack of meaningful benefit (LMB)
#' is greater than this for a given treatment arm, the arm may be dropped. Default is 0.9.
#' @param outcome.type Type of primary outcome: "CONTINUOUS", "ORDINAL", or "BINARY".
#' @param estimator.type Type of estimator for binary data: "odds ratio", "risk diff", "risk ratio". Default is odds ratio.
#' For ordinal data, a generalised odds ratio 'genodds' is used. For continuous data, a difference of means is used.
#' @param resprate The response rates for control and treatment. For binary and continuous data, expects a vector which one number for each arm.
#' For ordinal data, expects a list of lists with the with list corresponding to control. If running perpetually,
#' include all treatments here, including those that will not initially be in the trial.
#' @param ppm Patients per month.While the maximum sample size for a non-perpetual trial is derived from 'looks',
#' in a perpetually setting the trial will continue to go so long as there are new treatments to add, and patients are
#' still accruing according to the length of ppm.
#' @param special Any information wishing to pass to the tag that will be added to the results dataframe under the 'misc' column.
#' @return A parameter list used to generate a trial.
#' @export
#'
#' @examples
#' # two-arm six-stage trial (PRESTO-REACH) with binary outcome measure
#'
#' parlist <- getparlist(
#' looks=seq(500,1000,100),
#' perpetual=FALSE,
#' alloc.ratio=c(1,1),
#' num.per.block=c(1,1),
#' final.visit=0,
#' as.type="asOF",
#' multiarm.mode="CONFIDENCE-BASED",
#' lmb.threshold=0.95,
#' lmb.conf.thresh=0.9,
#' outcome.type='BINARY',
#' estimator.type='odds ratio',
#' resprate=c(0.3,0.5),
#' ppm=rep(15, 300))
#'
#' # two-arm three-stage trial with 16-point ordinal outcome
#'
#' resprate <- list(
#' ctrl = rep(1/16, 16),
#' trmt=c(
#' 0.08119658, 0.07802130, 0.07502870,0.07220504, 0.06953783,0.06701574,
#' 0.06462841, 0.06236641, 0.06022113,0.05818467, 0.05624978, 0.05440984,
#' 0.05265872, 0.05099079,0.04940088, 0.04788419)
#' )
#'
#' # create a list of input parameters
#'
#' inputs <- list(
#'  lmb.threshold = 1.10,
#'  as.type = 'asOF',
#'  outcome.type = "ORDINAL",
#'  multiarm.mode='CONFIDENCE-BASED',
#'  num.per.block = c(1,1),
#'  final.visit = 180,
#'  ppm = rep(20, 300),
#'  perpetual=FALSE,
#'  resprate=resprate,
#'  looks=c(500,1000,1500)
#'  )
#'  # pass parameters in through "inputs"
#'  parlist <- do.call("getparlist", inputs)

getparlist = function(looks=seq(500,1000,100),
                      nmax=NULL,
                      perpetual=FALSE,
                      alloc.ratio=c(1,1),
                      num.per.block=c(1,1),
                      final.visit=0,
                      as.type="asOF",
                      alpha=0.05,
                      multiarm.mode="CONFIDENCE-BASED",
                      lmb.threshold=0.10,
                      lmb.conf.thresh=0.9,
                      outcome.type='BINARY',
                      estimator.type='odds ratio',
                      resprate=c(0.3,0.5),
                      ppm=rep(15, 300),
                      special=NULL) {
  # check if looks are given as time rate
  if (max(looks) == 1){
    if (!is.null(nmax)){
      looks = looks * nmax
    } else{
      stop("Maximum sample size not supplied.")
    }
  }

  nmax = max(looks)

  parlist = list(nmax = nmax, looks=looks, num.looks = length(looks))

  if (!is.null(special)){
    parlist$misc=special
  } else {parlist$misc='None'}

  parlist$alloc.ratio = alloc.ratio
  if (length(num.per.block) != length(alloc.ratio)){
    num.per.block = rep(num.per.block[1], length(alloc.ratio))
  }
  parlist$num.per.block = num.per.block

  parlist$final.visit = final.visit

  if (!toupper(multiarm.mode) %in% c("ALL PROMISING", "SELECT BEST", "DROP WORST", "CONFIDENCE-BASED", "MONITOR FUTILITY")){
    warning(paste0("Mulitarm mode option ", multiarm.mode, " not available. Changing to default (CONFIDENCE-BASED)"))
    multiarm.mode = "CONFIDENCE-BASED"
  }

  parlist$multiarm.mode = toupper(multiarm.mode)
  parlist$alpha=alpha

  # if only monitoring for futility
  if (parlist$multiarm.mode == 'MONITOR FUTILITY'){
    parlist$as.type = NULL
    parlist$critical.values = NULL
    parlist$alpha.spending = NULL
    parlist$confidence.bounds.efficacy = NULL
    parlist$confidence.bounds.inferiority = NULL
  } else {
    # decision thresholds via rpact
    if (! as.type %in% c("OF", "P", "WT", "PT", "HP", "WToptimum", "asP", "asOF", "asKD",
                         "asHSD", "asUser", "noEarlyEfficacy")){
      warning(paste0("Alpha spending option ",as.type, " not available. Changing to default (asOF)."))
      as.type = "asOF"
    }

    parlist$as.type = as.type
    design = getGSDesign(looks=looks, as.type=as.type)
    bounds = getConfidenceFromBounds(design)
    parlist$critical.values = bounds$critical.values
    parlist$alpha.spending = bounds$alpha.spending
    parlist$confidence.bounds.efficacy = bounds$confidence.threshold.efficacy
    parlist$confidence.bounds.inferiority = bounds$confidence.threshold.inferiority
  }

  # LMB // Lack of Meaningful Benefit
  parlist$lmb.threshold = lmb.threshold
  parlist$lmb.confidence.threshold = lmb.conf.thresh

  # response rates and accrual
  if (!toupper(outcome.type) %in% c("BINARY", "ORDINAL", "CONTINUOUS")){
    warning(paste0("Outcome type ", outcome.type, " not in options."))
  }
  parlist$outcome.type = toupper(outcome.type)

  # check if there are enough response rates
  if (length(resprate) < length(alloc.ratio)){
    stop("Not enough response rates to allocated arm.")
  }

  if (is.numeric(resprate)){
    # if the input is a list of numbers, then it is binary
    parlist$resprate = resprate
    parlist$outcome.type = toupper(outcome.type)
    if(outcome.type != "BINARY"){
      warning("Outcome type changed to BINARY according to numeric response rate")
      parlist$outcome.type = "BINARY"
    }
  } else if (is.list(resprate)){
    # check if each has two items (mean and std)
    if (length(resprate[1])==2){
      if (outcome.type != "CONTINUOUS") {
        warning("Outcome type changed to CONTINUOUS according to list of mean/std response rate")
        parlist$outcome.type = "CONTINUOUS"}
    }
    parlist$resprate = resprate
  }

  # check binary has estimator type
  if (parlist$outcome.type == 'BINARY'){
    if (grepl('risk', tolower(estimator.type))){
      if (grepl('diff', tolower(estimator.type))){
         parlist$estimator.type = 'risk diff'
      } else if (grepl('ratio', tolower(estimator.type))|grepl('rel', tolower(estimator.type))){
        parlist$estimator.type = 'risk ratio'
      }
    } else {
      parlist$estimator.type = 'odds ratio'
    }
  }

  # accrual
  parlist$ppm = ppm
  parlist$perpetual=perpetual

  # check if there are more response rates than allocations
  if ((length(resprate) == length(alloc.ratio)) & perpetual==TRUE){
    warning("No extra treaments provided. Turning off perpertual.")
    parlist$perpetual = FALSE
  }

  resp.str =  sapply(seq(length(parlist$resprate)), function(x){
    paste(unlist(sapply(parlist$resprate[[x]]*100, round,0)), collapse='-')})
  parlist$resprate.str = resp.str
  parlist$outfilename = paste0(
    length(parlist$alloc.ratio), "arm",
    length(looks), "stage",
    parlist$outcome.type,
    "lmb-", 100* parlist$lmb.threshold,
    parlist$as.type, "-", parlist$misc)
  return(parlist)
}



#' Get Accrual
#' @description
#' Generate patient accrual with Poisson distribution.
#'
#' @param numsubjects Maximum sample size. If perpetual is TRUE, a new maximum sample size is returned.
#' @param ppm Patients accrued per month, as an array. Length of array is the number of months in the trial.
#' @param follow.up Follow-up period in months.
#' @param cont.recruit Whether to continue recruitment while waiting for follow up (TRUE) of not (FALSE).
#' @param perpetual Whether to run trial perpetually (TRUE) or not (FALSE).
#'
#' @return Vector size of number of patients you need to get 'numsubjects' followup with values representing month of accrual.
#' @export
#' @importFrom stats rexp
#' @examples
#' ppm <- rep(15, 300)
#' monthin <- getAccrual(1000, ppm, 0)
#' # monthin is of length 1000.

getAccrual = function (numsubjects, ppm, follow.up=0,
                       cont.recruit=FALSE,
                       perpetual=FALSE){

  if (!(is.numeric(numsubjects) & is.numeric(ppm))){
    ppm = suppressWarnings(as.numeric(ppm))
    numsubjects = suppressWarnings(as.numeric(numsubjects))
    if (is.na(ppm) | is.na(numsubjects)){
      stop("Inputs numsubjects or ppm not numeric.")
    }
  }

  monthin = NULL
  ptsin = 0  # we care about this if cont.recruit = FALSE
  ptsout = 0 # we care about this is cont.recruit = TRUE
  for (i in 1:length(ppm)){
    # cumsum means that each item in the vector accumulates on the previous entry
    # this helps them represent TIME continuously
    temp = cumsum(rexp(n=((numsubjects*2)-ptsin), rate=ppm[i])) # sample extra patients
    temp = temp[temp <= 1] # only this months patients
    # combine patients in this months with total patients
    monthin = c(monthin, (i - 1) + temp)
    ptsin <- length(monthin)
    ptsout <- length(monthin[monthin > follow.up]) # will equal pts in if follow.up = 0
    if (!perpetual){
      if(ptsout >= numsubjects){
        # do we halt recruiting
        if (cont.recruit==FALSE){
          monthin <- monthin[1:numsubjects]
        } else {
          # do we continue recruiting after reaching max
          monthin <- monthin[monthin <= monthin[numsubjects] + follow.up]
        }
        break
      }
    }
  }
  return(monthin)
}

#' Get Blocked Arm
#' @description
#' Randomize patients to arms using blocked randomization.
#' @details To balance covariates, each block gets an equal distribution of treatment arms to remove the effect
#' that could come from the block characteristics (e.g. covariates). If not balancing covariates,
#' patients will be randomized according to ratios.
#'
#' @param numsubjects Number of subjects to randomize.
#' @param num.per.block Number from each arm per block. Block size is 'sum(num.per.block)'.
#' @param prob Probability of randomization to each arm. Default assumes equal probability.
#'
#' @return Return vector of arm allocations.
#' @export
#'
#' @examples
#' arm <- getBlockedArm(500, c(1,1))
getBlockedArm = function(numsubjects, num.per.block, prob=NULL){

  if (!is.numeric(numsubjects)){
    numsubjects = suppressWarnings(as.numeric(numsubjects))
    if (is.na(numsubjects)){
      stop("'numsubjects' not numeric.")
    }
  }

  num.per.block = as.vector(num.per.block)
  num.arms = length(num.per.block)

  if (num.arms <= 1 | is.na(num.arms) ){stop('Check "num.per.block".')}

  block.size = sum(num.per.block)
  # if no probabilities are supplied, make all arms equal
  if (is.null(prob)){
    prob=rep(1, num.arms)
  }
  # if some arms probabilities are set to zero change block size and need
  if (any(prob==0)){
    block.size =sum(num.per.block[prob>0])
  }
  need = ceiling(numsubjects/block.size)
  # making sure there is a particular distribution
  as.vector(replicate(
    need,sample(rep.int(1:num.arms, num.per.block),
                prob=rep.int(prob, num.per.block),
                size=block.size)))[1:numsubjects]
}


####################e
# GENERATE OUTCOMES
###################


#' Generate binary data
#' @description Given an arm allocation and response rates, this function generates a binary response.
#' @param arm Arm allocation for a single patient.Expects number in *1,2,...,n* where *n* the number of treatment arms including control.
#' @param resprate Response rates for each arm. Expects a vector of probabilities of length *n* with the first
#' corresponding to response rate of the control arm.
#'
#' @return Returns a binary value corresponding to patient response.
#' @export
#' @importFrom stats rbinom
#' @examples
#'
#' response <- getDataBin(1, c(0.5, 0.7))
#'
getDataBin = function(arm, resprate){
  if (is.na(resprate[arm])){
    stop("No response rate for this treatment.")
  }
  rbinom(n=1, size=1, prob=resprate[arm])}

#' Generate ordinal data
#' @description Given an arm allocation and response rates, this function generates response on an ordinal scale.
#' @param arm Arm allocation for a single patient. Expects number in *1,2,...,n* where *n* is the number of treatment arms including control.
#' @param resprate Response rates for each arm. Expects a list of *n* lists with the first list containing
#' probabilities for response level which correspond to the control arm.
#'
#' @return Returns an ordinal value corresponding to patient response.
#'
#' @export
#'
#' @importFrom stats rmultinom
#'
#' @examples
#' # for a three-point ordinal scale
#' response <- getDataOrd(1, list(control = c(0.3, 0.5, 0.7), treatment = (c(0.5, 0.3, 0.2))))
getDataOrd <- function(arm, resprate){
  if (is.na(resprate[arm])){
    stop("No response rate for this treatment.")
  }
  which(( rmultinom(
    n=1,
    size=1,
    prob = resprate[[arm]]))==1)
}


#' Get Continuous Data
#' @description Given an arm allocation and response rates, this function generates response from a given distribution.
#' @param arm Arm allocation for a single patient. Expects number in *1,2,...,n* where *n* is the number of treatment arms including control.
#' @param resprate Response rates for each arm. Expects a list of n lists with the first list containing
#' median and standard deviation paramaterizing control response. Expects `c(mean, sd)` for each arm.
#' @param dist Type of distribution. Default is normal (`norm`).
#'
#' @return Returns a continuous value corresponding to patient response.
#' @export
#'
#' @importFrom stats rnorm
#'
#' @examples
#' response <- getDataCont(1, list(control = c(0,1), treatment = c(0.5,1)), dist='norm')

getDataCont <- function(arm, resprate, dist='norm'){
  if (is.na(resprate[arm])){
    stop("No response rate for this treatment.")
  }
  if (dist=='norm'){
    rnorm(1,
          mean=resprate[[arm]][1],
          sd=resprate[[arm]][2])
  }

}



#' Get Current Data
#' @description Get the data available at the time of the current analysis.
#' @param datlist The entire data list generated at the start of the simulation.
#' @param looktime The time of the current analysis point.
#' @param n The number of subjects corresponding to this analysis point (`n.at.look`).
#' @param as.followup If TRUE, the true looktime is when all *n* patient reach follow-up. If FALSE, looktime remains
#' the time when the *n*th patient is *enrolled*. Default is `TRUE`.
#'
#' @return Returns a subset of the data to use in interim analysis.
#' @return \itemize{
#'  \item{subjid: Subject ID from 1 to maximum sample size}
#'  \item{arm: Treatment arm allocation for each subject}
#'  \item{dat: Response for each subject (0 or 1)}
#'  \item{arrival.day: Arrival time (days) for each subject throughout trial duration}
#' }
#' @export
#'
#' @examples
#' # using included data set
#' data(datlist)
#' # This example uses the default parlist parameters
#' looks <- seq(500,1000,100)
#'
#' # get the data available at the first interim analysis
#' n.at.look <- looks[1]
#' looktime.interim <- datlist$arrival.day[n.at.look]
#' currdatlist.interim <- getCurrentData(datlist, looktime.interim, n.at.look, as.followup=TRUE)
#' # currdatlist.interim will have a new field `KNOWN`
#' #3 which indicates if a patients response is known (TRUE) or not (FALSE)


getCurrentData = function(datlist, looktime, n, as.followup=TRUE){
  n = suppressWarnings(as.numeric(n))
  if (is.na(n)){
    stop("Num subjects (n) supplied as  NA.")
  }
  if (is.na(looktime)){
    stop("Looktime supplied as NA.")
  }

  if (as.followup){
    # the time of interest is when the 500th reaches follow up
    looktime = datlist$obstime[n]
    if (is.na(looktime)){
      stop(paste0("There is no ", n, "th subject in the dataset"))
    }
    enrolled = (datlist$arrival.day <= looktime)
  } else{
    # the time of interest is when the 500th is enrolled
    if (max(datlist$arrival.day) < looktime){
      stop("Latest arrival day is less than chosen looktime.")
    }

    enrolled = (datlist$arrival.day <= looktime) & (datlist$subjid <= n)
  }
  # create subset of data based on bool
  # x[y] is the function where x is the original list and y is the boolean array
  newdatlist = lapply(datlist, FUN=function(x,y) x[y], y=enrolled)
  # data where we have the follow-up/outcome data
  # if there is no time to follow up then all of them are known
  newdatlist$known = newdatlist$obstime<=looktime
  return(c(newdatlist, looktime=looktime))
}

#' Get Sufficient Statistics
#' @description
#' Get sufficient statistics from trial data necessary to perform primary analysis
#'
#' @details Given a data list, this checks if the responses are binary (two types of responses),
#' ordinal (more than 2 or less than 30 different response types) or continuous (other). If continuous, mean and standard
#' deviations are returned. This code is not necessarily used in singleTrial since the perpetual functionality was
#' introduced as there are additional methods to retrieve the statistics necessary to perform the analysis that take
#' into account the possibility that arms have been dropped or added.
#'
#' @param datlist The current data list at the point of analysis generated from `getCurrentData`.
#' The list must have a `KNOWN` field.
#'
#' @return List of sufficient statistics
#' @return
#' \itemize{
#' \item{num.enrolled}
#' \item{num.known}
#' \item{num.uknown}
#' \item{num.resp}
#' \item{num.fail}
#' \item{resprate}
#' \item{formattedrate}}
#' @export

#' @importFrom stats sd
#'
#' @examples
#' # load data set
#' data(datlist)
#' looks <- seq(500,1000,100)
#' # first interim analysis
#' n.at.look = looks[1]
#' looktime.interim = datlist$arrival.day[n.at.look]
#' currdatlist.interim <- getCurrentData(datlist, looktime.interim, n.at.look, as.followup=TRUE)
#' suffStats  <- getSuffStats(currdatlist.interim)
getSuffStats = function(datlist) {
  num.enrolled = table(datlist$arm)
  num.known = with(datlist, tapply(datlist$known, datlist$arm, sum))
  num.unknown = num.enrolled - num.known
  remarms = sort(unique(datlist$arm))
  if(length(unique(datlist$dat)) < 3) {
    num.resp = tapply(datlist$dat[datlist$known], datlist$arm[datlist$known], sum)
    num.fail = num.known - num.resp
    resprate = num.resp/num.known
    suffstats = list(
      num.enrolled = num.enrolled,
      num.known = num.known,
      num.unknown = num.unknown,
      num.resp = num.resp,
      num.fail = num.fail,
      resprate = resprate)
    suffstats$formattedrate = paste0(num.resp, "/", num.known,
                                     "(", resprate, ")")
  } else if (length(unique(datlist$dat)) < 30){
    # ordinal data
    num.resp.all = sapply(remarms, function(x) {
      resp = datlist$dat[datlist$known][datlist$arm[datlist$known]==x]
      do.call("rbind",
              lapply(1:length(unique(datlist$dat)),
                     function(y){length(which(resp==y))}))
    }, USE.NAMES = FALSE)
    cols = c("control",paste0("treatment", seq(ncol(num.resp.all)-1)))
    resprate = asplit(prop.table(num.resp.all ,2), MARGIN=2)
    num.resp = asplit(num.resp.all, MARGIN=2)
    names(num.resp) = cols
    names(resprate) = cols
    suffstats = list(
      num.enrolled = num.enrolled,
      num.known = num.known,
      num.unknown = num.unknown,
      num.resp = num.resp,
      resprate = resprate)
    formattedrate = lapply(1:length(resprate), function(x){sapply(resprate[[x]]*100, round,0)})
    names(formattedrate) = cols
    suffstats$formattedrate = formattedrate

  }else {
    means = sapply(remarms, function(x) {
      mean(datlist$dat[datlist$known][datlist$arm[datlist$known]==x])})
    sds =  sapply(remarms, function(x) {
      sd(datlist$dat[datlist$known][datlist$arm[datlist$known]==x])})

    suffstats=list(
      mean = means,
      sd = sds,
      arms = remarms,
      num.enrolled = num.enrolled,
      num.known = num.known,
      num.unknown = num.unknown
    )
    suffstats$formattedrate=paste0(signif(means, 2))
  }

  return(suffstats)
}
