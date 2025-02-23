#' @title Cumulative Probability Model for Big Data
#'
#' @description Fits cumulative probability models (CPMs) for big data.  CPMs
#'     can be fit with the orm() function in the rms package.  When the
#'     sample size or the number of distinct values is very large, fitting a
#'     CPM may be very slow or infeasible due to demand on CPU time or
#'     storage.  This function provides three alternative approaches.  In the
#'     divide-and-combine approach, the data are evenly divided into subsets,
#'     a CPM is fit to each subset, followed by a final step to aggregate all
#'     the information.  In the binning and rounding approaches, a new
#'     outcome variable is defined and a CPM is fit to the new outcome
#'     variable.  In the binning approach, the outcomes are ordered and then
#'     grouped into equal-quantile bins, and the median of each bin is
#'     assigned as the new outcome for the observations in the bin.  In the
#'     rounding approach, the outcome variable is either rounded to a decimal
#'     place or a power of ten, or rounded to significant digits.
#'
#' @details In the divide-and-combine approach, the data are evenly divided
#'     into subsets.  The desired number of observations in each subset is
#'     specified by 'target_num'.  As this number may not evenly divide the
#'     whole dataset, a number closest to it will be determined and used
#'     instead.  A CPM is fit for each subset with the orm() function.  The
#'     results from all subsets are then aggregated to compute the final
#'     estimates of the intercept function alpha and the beta coefficients,
#'     their standard errors, and the variance-covariance matrix for the beta
#'     coefficients.
#'
#' In the binning approach, observations are grouped into equal-quantile bins
#'     according to their outcome.  The number of bins are specified by
#'     'target_num'.  A new outcome variable is defined to takes value
#'     median[y, y in B] for observations in bin B.  A CPM is fit with the
#'     orm() function for the new outcome variable.
#'
#' In the rounding approach, by default the outcome is rounded to a decimal
#'     place or a power of ten unless the skewness of the outcome is greater
#'     than 2, in which case the outcome is rounded to significant digits.
#'     The desired number of distinct outcomes after rounding is specified by
#'     'target_num'.  Because rounding can yield too few or too many distinct
#'     values compared to the target number specified by 'target_num', a
#'     refinement step is implemented so that the final number of distinct
#'     rounded values is close to 'target_num'.  Details are in Li et
#'     al. (2021).  A CPM is fit with the orm() function for the new rounded
#'     outcome.
#'
#' @param formula a formula object
#' @param data data frame to use.  Default is the current frame.
#' @param subset logical expression or vector of subscripts defining a subset
#'     of observations to analyze
#' @param na.action function to handle NAs in the data.  Default is
#'     'na.delete', which deletes any observation having response or
#'     predictor missing, while preserving the attributes of the predictors
#'     and maintaining frequencies of deletions due to each variable in the
#'     model.  This is usually specified using
#'     options(na.action="na.delete").
#' @param target_num the desired number of observations in a subset for the
#'     'divide-and-combine' method; the target number of bins for the
#'     'binning' method; the desired number of distinct outcome values after
#'     rounding for the 'rounding' method.  Default to 10,000.  Please see
#'     Details.
#' @param approach the type of method to analyze the data.  Can take value
#'     'binning', 'rounding', and 'divide-combine'.  Default is 'binning'.
#' @param rd_type the type of round, either rounding to a decimal place or a
#'     power of ten (rd_type = 'decplace') or to significant digits (rd_type
#'     = 'signif').  Default is 'skewness', which is to determine the
#'     rounding type according to the skewness of the outcome: 'decplace' if
#'     skewness < 2 and 'signif' otherwise.
#' @param mem_limit the fraction of system memory to be used in the
#'     'divide-and-combine' method.  Default is 0.75, which is 75 percent of
#'     system memory.  Range from 0 to 1.
#' @param log a parameter for parallel::makeCluster() when the
#'     'divide-and-combine' method is used.  See the help page for
#'     \code{\link[parallel]{makeCluster}} for more detail.
#' @param model a parameter for orm().  Explicitly included here so that the
#'     'divide-and-combine' method gives the correct output.  See the help
#'     page for \code{\link[rms]{orm}} for more detail.
#' @param x a parameter for orm().  Explicitly included here so that the
#'     'divide-and-combine' method gives the correct output.  See the help
#'     page for \code{\link[rms]{orm}} for more detail.
#' @param y a parameter for orm().  Explicitly included here so that the
#'     'divide-and-combine' method gives the correct output.  See the help
#'     page for \code{\link[rms]{orm}} for more detail.
#' @param method a parameter for orm().  Explicitly included here so that the
#'     'divide-and-combine' method gives the correct output.  See the help
#'     page for \code{\link[rms]{orm}} for more detail.
#' @param ... other arguments that will be passed to \code{\link[rms]{orm}}
#'
#' @return The returned object has class 'ormBD'.  It contains the following
#'     components in addition to those mentioned under the optional arguments
#'     and those generated by orm().
#'
#' \item{call}{calling expression}
#' \item{approach}{the type of method used to analyze the data}
#' \item{target_num}{the 'target_num' argument in the function call}
#' \item{...}{others, same as for \code{\link[rms]{orm}}}
#'
#'
#' @seealso \code{\link[rms]{orm}} \code{\link[Hmisc]{na.delete}} \code{\link[benchmarkme]{get_ram}}
#'          \code{\link[doParallel]{registerDoParallel}} \code{\link[SparseM]{SparseM.solve}}
#'
#' @author Guo Chen\cr
#' Department of Computer and Data Sciences\cr
#' Case Western Reserve University \cr
#' @author Chun Li\cr
#' Department of Population and Public Health Sciences\cr
#' University of Southern California \cr
#'
#' @examples
#' ## generate a small example data and run one of the three methods 
#' set.seed(1)
#' n <- 200
#' x1 = rnorm(n); x2 = rnorm(n)
#' tmpdata = data.frame(x1 = x1, x2 = x2, y = rnorm(n) + x1 + 2*x2)
#' modbinning <- ormBD(y ~ x1 + x2, data = tmpdata, family = loglog,
#'                      approach = "binning", target_num = 100)
#' ## modrounding <- ormBD(y ~ x1 + x2, data = tmpdata, family = loglog,
#' ##                     approach = "rounding", target_num = 100)
#' ## moddivcomb <- ormBD(y ~ x1 + x2, data = tmpdata, family = loglog,
#' ##                     approach = "divide-combine", target_num = 100)
#'
#' @references Liu et
#'     al. "Modeling continuous response variables using ordinal regression."
#'     Statistics in Medicine, (2017) 36:4316-4335.
#' @references Li et
#'     al. "Fitting semiparametric cumulative probability models for big data."
#'     (2021) (to be submitted)
#'
#' @importFrom rms orm
#' @importFrom Hmisc na.delete
#' @importFrom stats model.extract
#' @importFrom benchmarkme get_ram
#' @import doParallel
#' @import parallel
#' @import foreach
#' @import iterators
#' @export

ormBD <- function(formula, data, subset = NULL, na.action = na.delete,
                  target_num = 10000,
                  approach = c("binning", "rounding", "divide-combine"),
                  rd_type = c("skewness", "signif", "decplace"),
                  mem_limit = 0.75,
                  log = NULL, model=FALSE, x=FALSE, y=FALSE,
                  method = c("orm.fit", "model.frame", "model.matrix"),...) {

  cl <- match.call()
  approach <- match.arg(approach)
  rd_type <- match.arg(rd_type)
  method <- match.arg(method)

  #shadow copy environment to avoid not found problem
  if(missing(data)) data <- list2env(as.list(environment(formula), all.names=TRUE))
  #refresh environment to avoid subset problem
  environment(formula) <- environment()
  mf <- orm(formula = formula,data = data,subset = subset,na.action = na.action,method = "model.frame",...)
  if(method == "model.frame") return(mf)
  atrx     <- attributes(mf)
  sformula <- atrx$sformula
  Terms    <- atrx$terms
  attr(Terms, "formula") <- formula
  atr <- atrx$Design
  my <- Y <- model.extract(mf, 'response')
  mx <- orm(formula = formula,data = data,subset = subset,na.action = na.action,method = "model.matrix",...)
  if(method=="model.matrix") return(mx)
  X <- as.data.frame(mx)

  if(approach == "divide-combine"){
    X$Y <- Y
    m <- orm_divide_combine(Y~.,data=X,target_num,mem_limit,log,...)
  }else{
    Y = orm_data(Y,target_num,approach,rd_type)
    X$Y <- Y
    m <- orm(Y~.,data=X, ...)
  }
  m$call <- cl
  m$approach <- approach
  m$target_num <- target_num
  if(approach == 'rounding') m$approach=c(approach, rd_type)
  if(model) m$model <- mf
  if(x) m$x <- mx
  if(y) m$y <- my
  m$yunique <- unique(my)
  m$sformula <- sformula
  m$terms <- Terms
  m$assign <- if(length(X) > 0)DesignAssign(atr,m$non.slopes,Terms)
  m$Design <- atr
  if(!is.null(atrx$na.action))m$na.action <- atrx$na.action
  # sort the attributes by name
  # m <- m[order(names(m))]
  class(m) <-  c("ormDB",class(m))
  m
}


## This function is called when approach is 'binning' or 'rounding'
orm_data <- function(res, target_num, approach, rd_type) {
  if(target_num >= length(unique(res))) {
    warning(paste("The target number specified is >= #distinct outcome values; the",
                  approach, "approach will not be applied"), immediate.=T)
    return(res)
  }
  if(target_num <= 0){
    stop("The target number should be greater than 0.")
  }
  if(length(res) == 0){
    stop("The data should have observed values.")
  }

  if(approach == "binning") {
    message("Binning the outcomes ...")
    res = orm_binning(res, target_num)
  } else if(approach == "rounding"){
    if(rd_type == "skewness"){
      skewness = mean(scale(res)^3, na.rm=T)
      message(paste("Skewness =", round(skewness, 2)))
      if(skewness >=2) {
        message("Rounding the outcome to significant digits.")
        res = orm_rounding_sig(res, target_num)
      } else {
        message("Rounding the outcome to decimal place.")
        res = orm_rounding_dec(res, target_num)
      }
    } else if(rd_type == "signif"){
      message("Rounding the outcomes to significant digits.")
      res = orm_rounding_sig(res, target_num)
    } else if(rd_type == "decplace"){
      message("Rounding the outcomes to decimal place.")
      res = orm_rounding_dec(res, target_num)
    } else {
      stop("The value of rd_type is not recognizable.")
    }
  }
  res
}

orm_rounding_dec <- function(res,target_num){
  Nround <- target_num
  dig <- 0
  output1 <- length(unique(round(res,dig)))
  output2 <- length(unique(round(res,dig+1)))
  count = 0
  #check for edge case
  while((output2 < Nround || output1 > Nround) && count <= 100){
    count = count + 1
    if(output2 < Nround)
      dig <- dig + 1
    else if(output1 > Nround)
      dig <- dig - 1

    output1 <- length(unique(round(res,dig)))
    output2 <- length(unique(round(res,dig+1)))
  }
  searchgrid = seq(1, 10, .1)
  bb <- rep(0,length(searchgrid))
  for(i in 1:length(searchgrid)){
    rl <- searchgrid[[i]]
    bb[[i]] <- length(unique(round(rl*res, dig) * (1/rl)))
  }

  rltarget = searchgrid[order(abs(log(bb)-log(Nround)))[1]]
  return(round(rltarget*res, dig) * (1/rltarget))
}

orm_rounding_sig <- function(res,target_num){
  Nround <- target_num
  dig <- 0
  #use unique here
  #need to be postive here
  output <- floor(log10(unique(abs(res))))
  my_target <- unique(res)
  range_min <- min(output)
  range_max <- max(output)
  output1 <- numeric(length(output))
  for(ll in range_min:range_max) {
    idx = (output == ll)
    output1[idx] = round(my_target[idx], dig-ll)
  }
  output1 <- length(unique(output1))

  count = 0
  while(output1 < Nround && count <= 100){
    count = count + 1
    dig <- dig+1

    output1 <- numeric(length(output))
    for(ll in range_min:range_max) {
      idx = (output == ll)
      output1[idx] = round(my_target[idx], dig-ll) ## to sdtarget+1 digits
    }
    output1 <- length(unique(output1))
  }

  dig <- dig - 1

  #find best rl
  searchgrid = seq(1, 10, .1)
  bb <- rep(0,length(searchgrid))
  for(i in 1:length(searchgrid)){
    rl <- searchgrid[[i]]
    output1 <- numeric(length(output))
    for(ll in range_min:range_max) {
      idx = (output == ll)
      output1[idx] = round(rl*my_target[idx], dig-ll)* (1/rl)
    }
    output1 <- length(unique(output1))
    bb[[i]] <- output1
  }

  rltarget = searchgrid[order(abs(log(bb)-log(Nround)))[1]]

  #apply to data
  output_temp <- floor(log10(abs(res)))
  for(ll in range_min:range_max) {
    idx = (output_temp == ll)
    res[idx] = round(rltarget*res[idx], dig-ll)* (1/rltarget)
  }
  res
}

orm_binning <- function(res,target_num){
  N = length(res)
  M = target_num
  base_size <- N %/% M
  diff_count <- N %% M
  array_size <- rep (c(base_size, base_size + 1), c(M - diff_count, diff_count))
  array_size <- sample(array_size)

  y <- sort(res)
  y_pos <- order(res)
  start_point <- 1
  for (i in 1 : M) {
    end_point <- start_point + array_size[[i]] - 1
    median_val <- stats::median(y[start_point : end_point])
    res[y_pos[start_point:end_point]] = median_val
    start_point <- end_point + 1
  }
  res
}

orm_indi <- function(formula, data, ...){
  orm0 <- orm(formula, data = data, ...)

  res = data[,1]
  len <- length(unique(res)) - 1

  coef <- orm0$coefficients
  coef[1 : len] <- -coef[1 : len]
  var <- SparseM::as.matrix(SparseM::solve(orm0$info.matrix))

  ## coefficients now has the corrent sign for alpha
  ## alphaVariance only has the diagonal values from the variance matrix
  ## betaVariance is the full matrix for beta
  orm0$coefficients <- coef
  orm0$alphaVariance <- diag(var)[1:len]
  beta <- var[(len+1):nrow(var),(len+1):ncol(var)]
  # To avoid case with only one beta
  if(!is.matrix(beta))
    beta <- as.matrix(beta)
  orm0$betaVariance <- beta
  orm0
}

orm_divide_combine <- function(formula, data, target_num,mem_limit,log, ...){
  res = data[,1]
  if(target_num >= length(unique(res))){
    warning("The target number specified is larger than the number of observations.  Only a single job will be run.")
    return(orm(formula,data, ...))
  }
  if(target_num <= 0){
    stop("The target number should be greater than 0")
  }
  if(length(res) == 0){
    stop("The data should have observed values.")
  }

  M <- target_num
  N <- length(res)
  subset_num <- round(N/M)

  #### Generate the partition
  ## find index positions with K maximum and K minimun values
  IMax <- sample(utils::head(order(res, decreasing=TRUE), subset_num))
  IMin <- sample(utils::head(order(res), subset_num))
  ## remove those K maximum and K minimum index positions before shuffling
  idx <- 1:N
  idx <- sample(idx[-c(IMax,IMin)])
  ## shuffle idx
  base_size <- N %/% subset_num
  diff_count <- N %% subset_num
  group_index <- rep(1:subset_num, each=base_size-2)
  if(diff_count > 0) group_index = c(group_index, 1:diff_count)
  subset_index <- split(idx, group_index)
  ## add the maximum and minimum index positions back
  for(i in 1:subset_num){
    subset_index[[i]] <- c(subset_index[[i]], IMax[i], IMin[i])
  }

  #### Start running individual jobs
  ## detect #cores
  # in MB
  if(mem_limit <= 0 || mem_limit > 1){
    stop("mem_limit should be between 0 and 1.")
  }
  #75% of total be default
  ram_limit <- as.numeric(get_ram())*mem_limit/1e6
  #might be data-target
  ram_need <- 7.5e-5*(max(lengths(subset_index))-1+length(data))^2
  if(is.na(ram_limit)){
    ram_limit=ram_need*2
    warning("Unable to get system memory.  Only 2 jobs will be run simultaneously.")
  }
  message(paste0("Memory limit set to ",round(ram_limit,2)," MB"))
  #if ram_limit < ram_need
  if(ram_limit < ram_need){
      stop(paste0("The memory needed for one job is ", round(ram_need,2),
                  "(MB), which is greater than the specified memory limit ",
                  round(ram_limit,2),
                  "(MB). Consider a smaller target_num or a higher mem_limit."))
  }

  #minimum of all cores-1,jobs and limit
  no_cores <- min(c(detectCores() - 1,subset_num,round(ram_limit/ram_need)))

  if(is.null(log)){
    cl <- makeCluster(no_cores)
  }
  else{
    cl <- makeCluster(no_cores, outfile = log)
  }

  registerDoParallel(cl)

  ## run individual jobs
  # if the three dots is missing, the export content should be different
  export_content <- "orm_indi"
  if(!missing(...)){
    export_content <- c(export_content,"...")
  }
  # generate data list based on sub index to save memory
  data_list <- list()
  for(i in seq(subset_num)){
    data_list[[i]] <- data[subset_index[[i]], ]
  }

  # This line only serve to avoid note in CRAN check
  subset_data <- NULL

  model_list <- foreach(subset_data=data_list, .export = export_content,
                        .packages = "rms",.inorder = TRUE) %dopar%
    orm_indi(formula, subset_data, ...)
  rm("data_list")
  ## clean up afterwards
  #stopImplicitCluster()
  stopCluster(cl)

  ## detect if any model failed
  for(m in model_list){
    if(m$fail){
      stop("One of the individual jobs failed.")
    }
  }

  #### Aggregate information
  ## unique y values for individual jobs
  output_list <- list()
  for(i in 1:subset_num){
    output_list[[i]] <- unique(res[subset_index[[i]]])
  }

  ## define relevant values
  output <- sort(unique(res))
  n_output <- length(output)
  characters <- length(data) - 1
  n_para <- n_output - 1 + characters

  ## Define K-vectors for the alpha part
  K_vector <- list()
  for (ii in 1 : subset_num) {
    N <- length(output_list[[ii]])
    p <- NULL
    for(i in 1:(n_output-1)) p[i] <- sum(output[i] >= output_list[[ii]])
    p[p==N] = 0
    K_vector[[ii]] <- p
  }
  ## add K-vectors for the beta part to the end of that for the alpha
  for (i in 1 : subset_num) {
    N <- length(output_list[[i]]) - 1
    K_vector[[i]] <- c(K_vector[[i]], N+(1:characters))
  }
  ## convert all K-vectors to a matrix
  K_vector <- do.call(rbind, K_vector)

  ## Calculate target parameters as averages over individual jobs
  meta_coef <- rep(NA, n_para)
  for (i in 1 : n_para) {
    outcome_temp <- rep(NA, subset_num)
    for (j in 1 : subset_num) {
      pos <- K_vector[j,i]
      if (pos != 0) {
        outcome_temp[j] <- model_list[[j]]$coefficients[pos]
      }
    }
    meta_coef[i] <- mean(outcome_temp, na.rm=T)
  }

  ## apply the monotonicity constraints at the ends
  if(subset_num > 1){
    for (i in subset_num : 2) {
      if (meta_coef[i-1] > meta_coef[i]) {
        meta_coef[i-1] <- meta_coef[i]
      }
    }
    for (i in (n_output - subset_num) : (n_output - 2)) {
      if (meta_coef[i+1] < meta_coef[i]) {
        meta_coef[i+1] <- meta_coef[i]
      }
    }
  }

  ## Calculate the variance of the final parameters
  ## variance of alpha (only the diagonal part)
  meta_var_alpha <- rep(NA, n_output-1)
  for (i in 1 : (n_output-1)) {
    outcome_temp <- rep(NA, subset_num)
    for (k in 1:subset_num) {
      pos_i <- K_vector[k,i]
      if (pos_i != 0) {
        outcome_temp[k] <- model_list[[k]]$alphaVariance[pos_i]
      }
    }
    count_i <- sum(!is.na(outcome_temp))
    meta_var_alpha[i] <- sum(outcome_temp, na.rm=T) / (count_i * count_i)
  }
  ## variance of beta
  meta_var_beta <- matrix(NA, nrow = characters, ncol = characters)
  for (i in 1 : characters) {
    for(j in i : characters){
      outcome_temp <- rep(NA, subset_num)
      for(k in 1:subset_num) outcome_temp[[k]] <- model_list[[k]]$betaVariance[i, j]
      meta_var_beta[i,j] <- meta_var_beta[j,i] <- sum(outcome_temp)/(subset_num * subset_num)
    }
  }

  model <- list(coefficients = meta_coef,alphaVariance = meta_var_alpha,
                betaVariance = meta_var_beta,yunique = unique(res),fail = F,
                scale.pred = model_list[[1]]$scale.pred,family = model_list[[1]]$family)
  # Not assign orm class to enable printing, might fix it later
  class(model) <- "rms"
  model$non.slopes <- length(model$yunique)-1
  model
}

# get from rms package
DesignAssign <- function(atr, non.slopes, Terms) {
  ## Given Design attributes and number of intercepts creates R
  ## format assign list.

  ll <- if(missing(Terms)) atr$name else attr(Terms,'term.labels')
  if(! length(ll)) return(list())
  nv     <- length(ll)
  params <- sapply(atr$nonlinear, length)  ## d.f. per predictor
  asc    <- atr$assume.code
  assign <- list()
  j <- non.slopes + 1
  if(length(params)) for(i in 1 : length(ll)) {
    if(asc[i] == 8) next
    assign[[ll[i]]] <- j : (j+params[i]-1)
    j <- j + params[i]
  }
  assign
}
