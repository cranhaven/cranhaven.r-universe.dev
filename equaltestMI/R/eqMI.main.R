#' The main function to test measurement invariance
#'
#' Test measurement invariance with equivalence testing, projection methods, and adjusted RMSEA cutoffs for two groups.
#'
#' @param ... The same arguments as for any lavaan model. See \code{lavaan::sem} for more information.
#'
#' Users must explicitly specify the name of the input elements for this function to catch. For example, specify 'data = HolzingerSwineford' instead just 'HolzingerSwineford'.
#' @param output If the function prints out results of covariance structure, mean structure, or both. The value of \code{output} must be \code{mean}, \code{covariance}, or \code{both}. When the tests involve mean structure (\code{output = 'mean' or 'both'}), both the strong and the strict tests of measurement invariance will be conducted.
#' @param quiet If \code{quiet=FALSE}, a summary is printed out containing an overview of the different models that are fitted, together with some model comparison tests and fit measures. The results of equivalence testing will also be printed if equivalence testing is used. If \code{quiet=TRUE} (default), no summary is printed but results will be stored in 'AnnotatedOutput'.
#' @param equivalence.test If \code{equivalence.test=TRUE}, equivalence testing is used for examining all statistics. RMSEA together with conventional or adjusted cutoff values will be used to gauge the goodness of fit.
#' @param adjRMSEA If \code{adjRMSEA=TRUE}, adjusted RMSEA cutoff values are used for equivalence testing. See details in Yuan & Chan (2016).
#' @param projection  If \code{projection=TRUE}, projection method is used to test the equality of latent factor means. The advantage of the projection method over conventional multiple-group SEM approach is that the test of latent factor means can be conducted even when the equality of intercepts do not hold.
#' @param bootstrap If \code{bootstrap=TRUE}, bootstrap is used to obtain empirical p-values for testing the equality of cross-group latent factor means.
#' @param B The number of boostrap samples used in bootstrap approach.
#' @param seed The initial seed to generate bootstrap samples. Default at 111.
#' @return A list is returned with:
#' \describe{
#' \item{\code{AnnotatedOutput}}{Annotated outout that will be printed to the console if quiet==FALSE.}
#' \item{\code{eqMI.stat}}{Test statistics, degrees of freedom, p-values, ncp, T-sizes, RMSEAs, their cutoff values, and the goodness-of-fit under equivalence testing. A formated version of \code{eqMI.stat} will be printed if \code{quiet=FALSE}.}
#' \item{\code{convention.sem}}{Results of conventional multiple-group SEM using Lavaan. Returned object of \code{\link{eqMI.semtest}}.}
#' \item{\code{projection.res}}{Results of projection methods on tests of latent means. Returned object of \code{\link{eqMI.projection}} and \code{\link{eqMI.bootstrap}}.}
#' }
#'
#' @details An all-in-one function with several added options to conduct a sequence of tests needed to evaluate MI. The chi-square statistics, except the one for testing the equality of covariance structure, are obtained based on \code{lavaan::sem} function. The test statistic of the covariance structure equality is obtained via the method of Lagrangian multiplier. Equivalence testing is enabled by setting \code{equivalence.test=TRUE} and this function will calculate T-size, RMSEA, and adjusted RMSEA cutoff values, and provide the goodness-of-fit.
#' @references Deng, L., & Yuan, K. H. (2016). Comparing Latent Means Without Mean Structure Models: A Projection-Based Approach. Psychometrika, 81(3), 802-829. https://doi.org/10.1007/s11336-015-9491-8
#' @references Jiang, G., Mai, Y., & Yuan, K. H. (2017). Advances in Measurement Invariance and Mean Comparison of Latent Variables: Equivalence Testing and A Projection-Based Approach. Frontiers in Psychology, 8, 1823.
#' @references Yuan, K. H., & Chan, W. (2016). Measurement invariance via multigroup SEM: Issues and solutions with chi-square-difference tests. Psychological methods, 21(3), 405-426. https://doi.org/10.1037/met0000080
#' @importFrom lavaan sem
#' @importFrom lavaan lav_matrix_duplication
#' @importFrom lavaan lavaanify
#' @importFrom lavaan lavTech
#' @importFrom stats pchisq
#' @export
#' @examples
#' \donttest{
#' data(HolzingerSwineford)
#' semmodel<-'
#' L1 =~ V1 + V2 + V3
#' L2 =~ V4 + V5 + V6
#' L3 =~ V7 + V8
#' L4 =~ V9 + V10 + V11
#' '
#' # If raw data are available;
#'
#' test <- eqMI.main(model = semmodel, data = HolzingerSwineford,
#'         group = "school", meanstructure = TRUE,
#'         output = 'both', quiet = FALSE,
#'         equivalence.test = TRUE, adjRMSEA = TRUE,
#'         projection = TRUE, bootstrap = FALSE)
#'
#' # when only sample statistics are available;
#' # sample.cov need to be provided for tests of covariance structure;
#' # sample.mean need to be provided for tests of mean structure;
#'
#' school1 <- subset(HolzingerSwineford, school==1)[,-12]
#' school2 <- subset(HolzingerSwineford, school==2)[,-12]
#' test <- eqMI.main(model = semmodel,
#'         sample.nobs = c(nrow(school1), nrow(school2)),
#'         sample.cov = list(cov(school1), cov(school2)),
#'         sample.mean = list(colMeans(school1), colMeans(school2)),
#'         meanstructure = TRUE, output = 'both', quiet = FALSE,
#'         equivalence.test = TRUE, adjRMSEA = TRUE,
#'         projection = TRUE, bootstrap = FALSE)
#'}
eqMI.main <- function(..., output = 'both', equivalence.test = TRUE, adjRMSEA = TRUE, projection = FALSE, bootstrap = FALSE, quiet = TRUE, B = 100, seed = 111) {

  # check the validity of commands
  output <- tolower(output)
  if(!output%in%c('mean', 'covariance', 'both')) {
    stop("output command can only take values in ('mean', 'covariance', 'both').")
  }
  dotdotdot <- list(...)
  # 0. check the positive definite of sample covariance matrices
  if(!is.null(dotdotdot$sample.cov)){
    if(sum(sapply(1:length(dotdotdot$sample.cov), function(x) det(as.matrix(dotdotdot$sample.cov[[x]])))<0)!=0) stop("All group covariance matrices need to be positive definite")}
  # 1. retain only the variables specified in the lavaan model synatx
  modTable <- lavaanify(dotdotdot$model)
  useVar <- unique(modTable[modTable$op=='=~',]$rhs)
  if (!is.null(dotdotdot$data)){
    dotdotdot$data <- dotdotdot$data[,c(useVar, dotdotdot$group)]
  } else {
    for (i in 1:length(dotdotdot$sample.cov)){
      dotdotdot$sample.cov[[i]] <- dotdotdot$sample.cov[[i]][useVar, useVar]
      if (!is.null(dotdotdot$sample.mean)){
        dotdotdot$sample.mean[[i]] <- dotdotdot$sample.mean[[i]][useVar]
      }
    }
  }
  # 2. group.equal needs to be NULL
  if(!is.null(dotdotdot$group.equal)){
    stop("lavaan ERROR: group.equal argument should not be used")
  }
  # 3. adjusted RMSEA cutoff values can only be invoked when equivalence.test is used. a warning message is given and conventional multiple group is performed
  if ( adjRMSEA == TRUE & equivalence.test == FALSE ) {
    warning("Adjusted RMSEA cutoff values is used only for equivalence testing. Please change adjRMSEA == FALSE if equivalence testing is not employed. Conventional RMSEA cutoff values will be used instead.")
  }
  # 4. projection method and bootstrap are invoked only when mean structure is of interest
  projection.res <- NULL
  if ( output == 'covariance' ) {
    if ( projection == TRUE ) {
      warning("projection method must be used when mean structure is of interest. turn off projection by setting projection = FALSE or change output command")
    } else if ( bootstrap == TRUE ) {
      warning("bootstrap resampling must be used when mean structure is of interest and projection method is enabled.")
    }
  } else if ( projection == FALSE & bootstrap == TRUE ) {
    warning("bootstrap resampling is used only for projection method. no bootstrap resampling will be performed here. ")
  } else if ( projection == TRUE ) {
    projection.res <- do.call(eqMI.projection, dotdotdot)
    if (bootstrap == TRUE) {
      projection.res$bootstrap <- do.call(eqMI.bootstrap, c(dotdotdot, B = B, seed = seed))
      projection.res$chi.stat$pvalue[2:3] <- projection.res$bootstrap
    }
  }

  #Test the equality of population covariance matrices with lagrangier multiplier tests
  fit.pop.cov <- do.call(eqMI.covtest, dotdotdot)
  AnnotatedOutput <- list()
  AnnotatedOutput <- append(AnnotatedOutput, list(fit.pop.cov))
  names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'Equality of Population Covariance Matrices under NHT'
  #  names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'NHT_TestOfEqualPopCov'
  if(!quiet) {
    message('\n', '---------- Equality of Population Covariance Matrices under NHT ---------- ', '\n\n')
    print(fit.pop.cov)
  }

  if (quiet==FALSE & equivalence.test==TRUE) {quiet2 = TRUE} else {quiet2=quiet}
  #conventional multiple-group SEM testing of MI using Lavaan
  convention.sem <- do.call(eqMI.semtest, c(dotdotdot, list(output = output, quiet = quiet2)))
  # feed the statistics to equilvalence testing
  if ( output == 'mean' ) {
    part.sem1 <- subset(convention.sem$Mean.part, select=c('Chisq', 'Df'))
    part.sem2 <- subset(convention.sem$Mean.part, select=c('Chisq.diff', 'Df.diff'))
  } else if ( output == 'covariance' ) {
    part.sem1 <- subset(convention.sem$Cov.part, select=c('Chisq', 'Df'))
    part.sem2 <- subset(convention.sem$Cov.part, select=c('Chisq.diff', 'Df.diff'))
  } else {
    part.sem1 <- rbind(subset(convention.sem$Cov.part, select=c('Chisq', 'Df')), subset(convention.sem$Mean.part, select=c('Chisq', 'Df'))[-c(1:2),])
    part.sem2 <- rbind(subset(convention.sem$Cov.part, select=c('Chisq.diff', 'Df.diff')), subset(convention.sem$Mean.part, select=c('Chisq.diff', 'Df.diff'))[-c(1:2),])
    # here 1:2 are duplicate tests of configural and metric tests
  }
  part.sem1$pvalue <- 1-pchisq(part.sem1$Chisq, part.sem1$Df)
  colnames(part.sem2) <- c('Chisq', 'Df')
  rownames(part.sem2) <- paste0(rownames(part.sem2), '.diff')
  part.sem2$pvalue <- 1-pchisq(part.sem2$Chisq, part.sem2$Df)

  # keep 'fit.configural.g1' and 'fit.configural.g2' in difference tests 'cause the chisquare are not displayed
  ## drop meaningless difference tests of 'fit.configural.g1' and 'fit.configural.g2'
  #ind <- match(c('fit.configural.g1.diff', 'fit.configural.g2.diff'), rownames(part.sem2))
  #part.sem2 <- part.sem2[-ind, ]

  # collect all statistics
  stat.feed <- rbind(fit.pop.cov, part.sem1, part.sem2, projection.res$chi.stat)

  # print results under NHT
  df1 <- cbind(stat.feed[1:(1+nrow(part.sem1)),1:3], stat.feed[c(1:4, (nrow(part.sem1)+5):(2*nrow(part.sem1)+1)),1:3])
  num.ind1 <- vapply(df1, is.numeric, FUN.VALUE = logical(1))
  num.ind1[grep('Df', names(num.ind1))] <- FALSE
  df1[,num.ind1] <- sprintf("%8.3f",unlist(df1[,num.ind1]))
  colnames(df1)[4:6] <- c('Chisq.diff', 'Df.diff', 'pvalue')
  df1[match(c('fit.pop.cov', 'fit.configural.g1', 'fit.configural.g2'), rownames(df1)), 4:6] <- ''
  df1[match('fit.combine.groups', rownames(df1)), 3:6] <- ''

  if(!quiet){
    message("\n", '-------- Chi-Square and Chi-Square-Difference Test under NHT  --------', '\n\n')
    #message("Chi Square and Chi Square Difference Test \n\n")
    print(df1)
  }
  AnnotatedOutput <- append(AnnotatedOutput, list(df1))
  names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'Chi-Square and Chi-Square-Difference Test under NHT'
  #  names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'NHT_MItest'

  if ( equivalence.test == TRUE ) {
    sample.nobs <- lavTech(convention.sem$LavaanOut$fit.combine.groups, 'nobs')
    N <- sum(sample.nobs)
    for (i in 1:nrow(stat.feed)){
      equalTest <- eqMI.ncp(stat.feed$Chisq[i], stat.feed$Df[i], N = N, m = 2)
      stat.feed$ncp[i] <- equalTest$ncp
      stat.feed$epsilon_t[i] <- equalTest$epsilon_t
      stat.feed$RMESA_t[i] <- equalTest$RMSEA_t
    }
    labels <- c('excellent', 'close', 'fair', 'mediocre', 'poor')
    if ( adjRMSEA == TRUE ) {
      RMESA_cutoffs <- data.frame(matrix(NA, nrow(stat.feed), 5))
      colnames(RMESA_cutoffs) <- c('cut.01', 'cut.05', 'cut.08', 'cut.10', 'goodness-of-fit')
      ind <- which(!(stat.feed$Df==0)) # which(!is.na(stat.feed$Df)) #neither NA nor 0 for saturated model
      for (i in ind) {
        RMESA_cutoffs[i,1:4] <- eqMI.RMSEA(N = N, df = stat.feed$Df[i], m = 2)[5:8]
        RMESA_cutoffs[i,5] <- labels[findInterval(stat.feed$RMESA_t[i], RMESA_cutoffs[i,1:4])+1]
      }
      stat.feed <- cbind(stat.feed, RMESA_cutoffs)
    } else {
      RMESA_cutoffs <- data.frame(cut.01 = 0.01, cut.05 = 0.05, cut.08 = 0.08, cut.10 = 0.10)
      stat.feed <- cbind(stat.feed, RMESA_cutoffs)
      stat.feed$'goodness of fit' <- labels[findInterval(stat.feed$RMESA_t, RMESA_cutoffs)+1]
    }
    df2 <- stat.feed[c(1:3, (nrow(part.sem1)+5):(2*nrow(part.sem1)+1)),-c(1:3)]
    num.ind2 <- vapply(df2, is.numeric, FUN.VALUE = logical(1))
    num.ind2[grep('Df', names(num.ind2))] <- FALSE
    df2[,num.ind2] <- sprintf("%8.3f",unlist(df2[,num.ind2]))
    rownames(df2) <- rownames(df1)[-4]

    AnnotatedOutput <- append(AnnotatedOutput, list(df2))
    #names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'ET_MItest'

    if ( adjRMSEA == TRUE ){
      names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'T-size epsilon, RMSEA, and Adjusted Cutoff Values under ET'
      if(!quiet){
        message("\n", '-------- T-size epsilon, RMSEA, and Adjusted Cutoff Values under ET --------', '\n\n')
        print(df2)
        message("\n")
      }
    } else {
      names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'T-size epsilon, RMSEA, and Conventional Cutoff Values under ET'
      if(!quiet){
        message("\n", '-------- T-size epsilon, RMSEA, and Conventional Cutoff Values under ET --------', '\n\n')
        print(df2)
        message("\n")
      }
    }
  }

  # print the results of projection methods on latent means here if applicable
  if(!is.null(projection.res) & !quiet){
    message('\n', '---------- Means of Latent and Specific Factors by the Projection Method and under NHT ----------', '\n\n')
    if( stat.feed["fit.metric.diff","pvalue"]<=0.05 | stat.feed["fit.metric.diff","goodness-of-fit"]=='poor'){
      warning("Metric invariance must be established before the use of projection method for testing equality of latent means ")
      message('\n')
    }
    print(projection.res$chi.stat)
    message('Validity Index is ', round(projection.res$V.index, 5), '\n')
  }
  AnnotatedOutput <- append(AnnotatedOutput, list(projection.res$chi.stat))
  names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'Projection Method: Means of Latent and Specific Factors'
  #names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'NHT_TestOfMean'
  AnnotatedOutput <- append(AnnotatedOutput, list(projection.res$V.index))
  names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'Validity Index'
  #names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'ValidityIndex'

  if (equivalence.test == TRUE) {
    if(!is.null(projection.res)){
      df3 <- stat.feed[(2*nrow(part.sem1)+2):(2*nrow(part.sem1)+4),-c(1:3)]
      num.ind3 <- vapply(df3, is.numeric, FUN.VALUE = logical(1))
      num.ind3[grep('Df', names(num.ind3))] <- FALSE
      df3[,num.ind3] <- sprintf("%8.3f",unlist(df3[,num.ind3]))
      AnnotatedOutput <- append(AnnotatedOutput, list(df3))
      names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'ET: Means of Latent and Specific Factors'
      #names(AnnotatedOutput)[length(AnnotatedOutput)] <- 'ET_TestOfMean'
    }
  }

  if(!quiet){
    #message("Projection Method Test \n\n")
    message('\n', '---------- Means of Latent and Specific Factors by the Projection Method and under ET ----------', '\n\n')
    print(df3)
    message("\n")
  }

  return(list(AnnotatedOutput = AnnotatedOutput, eqMI.stat = stat.feed, convention.sem = convention.sem, projection.res = projection.res))
}

