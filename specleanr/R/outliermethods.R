
#Adjusted boxplots

#' @title Adjust the boxplots bounding fences using medcouple to flag suspicious outliers.
#'
#' @param data \code{dataframe}. Dataframe to check for outliers.
#' @param var \code{string}. Environmental predictor considered in flagging suspicious outliers.
#' @param output \code{string} Either clean: for dataframe with no suspicious outliers or
#'   outlier: to return dataframe with only outliers.
#' @param a \code{numeric}. Constant for adjusted boxplots.
#' @param b \code{numeric}. Constant for adjusted boxplots.
#' @param coef \code{numeric}. Constant for adjusted boxplots.
#'
#' @importFrom robustbase adjboxStats
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return \code{dataframe}. Dataframe with or with no outliers.
#' @export
#'
#' @examples
#' \donttest{
#'data("efidata")
#'
#'danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#'db <- sf::st_read(danube, quiet=TRUE)
#'
#'wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#'refdata <- pred_extract(data = efidata, raster= wcd ,
#'                       lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                        colsp = "scientificName",
#'                        bbox = db,
#'                        minpts = 10)
#'
#'adout <- adjustboxplots(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'
#'}
#' @references Hubert M, Vandervieren E. 2008. An adjusted boxplot for skewed distributions.
#' Computational Statistics and Data Analysis 52:5186-5201.
#'
#'
adjustboxplots <- function(data, var, output = 'outlier', a=-4, b=3, coef=1.5,pc = FALSE, pcvar = NULL, boot = FALSE){

  options(mc_doScale_quiet=TRUE)

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  outdata <- robustbase::adjboxStats(var, coef = coef,a = a, b = b, do.conf = TRUE,
                                     do.out = TRUE)$out
  datIn <- which(!var%in%outdata)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))

}

#' @title Computes interquartile range to flag environmental outliers
#'
#' @param data Dataframe to check for outliers
#' @param var Variable considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers.
#' @param x A constant to create a fence or boundary to detect outliers.
#'
#' @details
#' Interquartile range (IQR) uses quantiles that are resistant to outliers compared
#'      to mean and standard deviation (Seo 2006). Records were considered as mild outliers
#'      if they fell outside the lower and upper bounding fences
#'      [Q1 (lower quantile) -1.5*IQR (Interquartile range); Q3 (upper quantile) +1.5*IQR]
#'      respectively \code{(Rousseeuw & Hubert 2011)}.
#'      Extreme outliers were also considered if they
#'      fell outside \code{\[Q1-3*IQR, Q3+3*IQR\]} \code{(García-Roselló et al. 2014)}.
#'      However, using the interquartile range assumes uniform lower and
#'      upper bounding fences, which is not robust to highly skewed data
#'      (Hubert & Vandervieren 2008).
#'
#'
#' @importFrom stats quantile IQR
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#'danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#'db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd , lat = 'decimalLatitude',
#'                           lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  iqrout <- interquartile(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'}
#' @references
#'  Rousseeuw PJ, Hubert M. 2011. Robust statistics for outlier detection. Wiley Interdisciplinary Reviews
#'  Data Mining and Knowledge Discovery 1:73-79.
#'
#'
interquartile <- function(data, var, output, x=1.5, pc = FALSE, pcvar = NULL, boot = FALSE){

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)

  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  iqrcal <- stats::IQR(var)

  lowbound = quantile(var, 0.25) - x* iqrcal

  upbound = quantile(var, 0.75) + x * iqrcal

  datIn <- which(var<upbound & var>lowbound)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#Semi interquartile range

#' @title  Computes semi-interquantile range to flag suspicious outliers
#'
#' @param data Dataframe to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers
#' @param x A constant to create a fence or boundary to detect outliers.
#'
#' @details
#' SemiInterquantile Ranges introduced adjusts for whiskers on either
#' side to flag suspicious outliers [Q1 – 3(Q2 (median) - Q1); Q3 + 3(Q3 - Q2)] \code{((Kimber 1990))}.
#' However, SIQR introduced the same constant values for bounding fences
#' for the lower and upper quartiles \code{(Rousseeuw & Hubert 2011)}, which leads to
#' outlier swamping and masking.
#'
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Dataframe with or with no outliers.
#' @export
#'
#' @examples
#'\donttest{
#'
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                           lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  semiout <- semiIQR(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'}
#'
#' @references
#' Kimber AC. 1990. Exploratory Data Analysis for Possibly Censored Data From Skewed Distributions.
#' Page Source: Journal of the Royal Statistical Society. Series C (Applied Statistics).
#'
semiIQR <- function(data, var, output, x=3, pc = FALSE, pcvar = NULL, boot = FALSE){

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  semiQL=  stats::quantile(var, 0.5)- stats::quantile(var, 0.25)

  #SIQRU = Q3 - Q2
  semiQU=  stats::quantile(var, 0.75) - stats::quantile(var, 0.5)

  #[Q1 - 3SIQRL ;
  lowbound  = stats::quantile(var, 0.25) - x*semiQL

  #Q3 + 3SIQRU]
  upbound = quantile(var, 0.75) + x*semiQU

  datIn <- which(var<upbound & var>lowbound)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#=
#Hampel filters

#' @title Flag suspicious outliers based on the Hampel filter method..
#'
#' @param data Data frame to check for outliers
#' @param var Environmental parameter considered in flagging suspicious outliers
#' @param output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers
#' @param x A constant to create a fence or boundary to detect outliers.
#'
#' @details
#' The Hampel filter method is a robust decision-based filter that considers
#' the median and MAD. Outliers lies beyond \deqn{[x-* lmbda*MAD; x+ lmbda*MAD]} and
#' lmbda of 3 was considered (Pearson et al. 2016).
#'
#'
#' @importFrom stats median mad
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Data frame with or with no outliers.
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                           lat = 'decimalLatitude',
#'                           lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  hampout <- hampel(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'}
#'
#' @references
#' Pearson Ronald, Neuvo Y, Astola J, Gabbouj M. 2016. The Class of Generalized Hampel Filters.
#' 2546-2550 2015 23rd European Signal Processing Conference (EUSIPCO).
#'
#'
hampel <- function(data, var, output, x=3, pc = FALSE, pcvar = NULL, boot = FALSE){

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  lowbound = stats::median(var) - x * stats::mad(var)

  upbound = stats::median(var) + x * stats::mad(var)

  datIn <- which(var<upbound & var>lowbound)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#Reverse jackknifing

#' @title Identifies outliers using Reverse Jackknifing method based on Chapman et al., (2005).
#'
#' @param data Dataframe to check for outliers
#' @param var Variable considered in flagging suspicious outliers.
#' @param output Either clean: for data frame with no suspicious outliers or outlier: to return data frame with only outliers
#' @param mode Either robust, if a robust mode is used which uses median instead of mean and median absolute deviation from median
#' or mad instead of standard deviation.
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @details
#' Reverse jackknifing was specifically developed to detect error climate profiles \code{(Chapman 1991, 1999)}.
#' The method has been applied in detecting outliers in environmental data \code{(García-Roselló et al. 2014; Robertson et al. 2016)}
#'  and incorporated in the DIVAS-GIS software \code{(Hijmans et al. 2001)}.
#'
#'
#' @return Data frame with or with no outliers.
#' @export
#'
#' @examples
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                         lat = 'decimalLatitude',
#'                         lon= 'decimalLongitude',
#'                         colsp = "scientificName",
#'                         bbox = db,
#'                         minpts = 10)
#'
#' jkout <- jknife(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'
#'}
#' @references
#'
#' \enumerate{
#'
#'   \item Chapman AD. 1991. Quality control and validation of environmental resource data in
#'   Data Quality and Standards. Pages 1-23. Canberra. Available from
#'   https://www.researchgate.net/publication/332537824.
#'   \item Chapman AD. 1999. Quality Control and Validation of Point-Sourced Environmental Resource Data. eds. .
#'   Chelsea,. Pages 409-418 in Lowell K, Jaton A, editors. Spatial accuracy assessment:
#'   Land information uncertainty in natural resources, 1st edition. MI: Ann Arbor Press., Chelsea.
#' }
#'
#'
jknife <- function(data, var, output ='outlier', mode='soft', pc = FALSE, pcvar = NULL, boot = FALSE){

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)

  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  sort_ls <- sort(unique(var))

  threshold <- 0.95*sqrt(length(var))*0.2

  totnum <- length(sort_ls)-1

  if(mode=='soft'){
    xbar <- mean(var)

    y <- c()
    xc <- c()
    for (oii in 1:totnum) {

      xi <- sort_ls [oii]

      xii <- sort_ls [oii+1]

      y[oii] <- ifelse(xi<xbar, (xii-xi)*(xbar-xi), (xii-xi)*(xii-xbar))
      xc[oii] <- xi

    }
    cvals <- y/sd(y)

    idx <- which(cvals  <threshold)

    xcv <- xc[idx]

    xvar <- var[var%in%xcv]

    datIn <- which(var%in%xvar)

  }else{
    y <- c()
    xc <- c()
    xbar <- stats::median(var)

    for (oii in 1:totnum) {

      xi <- sort_ls[oii]
      xii <- sort_ls[oii+1]

      y[oii] <- ifelse(xi<xbar, (xii-xi)*(xbar-xi), (xii-xi)*(xii-xbar))
      xc[oii] <- xi

    }
    cvals <- y/mad(y)

    idx <- which(cvals < threshold)

    xcv <- xc[idx]

    xvar <- var[var%in%xcv]

    datIn <- which(var%in%xvar)
  }
  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' @title Computes z-scores to flag environmental outliers.
#'
#' @param data Dataframe or vector to check for outliers.
#' @param var Variable considered in flagging suspicious outliers.
#' @param output Either \strong{clean}: for data frame with no suspicious outliers or outlier: to return dataframe with only outliers.
#' @param type Either \strong{mild} if zscore cut off is 2.5 or \strong{extreme} if zscore is >3.
#' @param mode Either robust, if a \strong{robust} mode is used which uses median instead of
#' mean and median absolute deviation from median.
#'
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @details
#' The method uses mean as an estimator of location and standard deviation for scale
#'     \code{(Rousseeuw & Hubert 2011)}, which both have zero breakdown point,
#'     and their influence function is unbounded (robustness of an estimator to outliers)
#'     \code{(Seo 2006; Rousseeuw & Hubert 2011)}. Because both parameters are not
#'     robust to outliers, it leads to outlier masking and swamping
#'     \code{(Rousseeuw & Hubert 2011)}. Records are flagged as outliers
#'     if their Z-score exceeds 2.5 \code{(Rousseeuw & Hubert 2011)}.
#'
#'
#' @return Data frame with or with no outliers.
#'
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                           lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  zout <- zscore(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'}
#'
zscore <- function(data, var, output = 'outlier', type = 'mild', mode = 'soft', pc = FALSE, pcvar = NULL, boot = FALSE){

  match.argc(output, choices = c('clean','outlier'))
  match.argc(type, choices = c('extreme','mild'))
  match.argc(mode, choices = c('robust', 'soft'))

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  if(mode=='robust'){

    zscores <- (var- stats::median(var))/stats::mad(var)

    datIn = switch(type, mild = which(zscores< 2.5 & zscores > (-2.5)),
                   extreme= which(zscores< 3 & zscores > (-3)))
  }else {
    zscores <- (var - base::mean(var))/stats::sd(var)

    datIn = switch(type, mild = which(zscores< 2.5 & zscores > (-2.5)),
                   extreme= which(zscores< 3 & zscores > (-3)))
  }

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#Boxplot modifications

#' @title Log boxplot based for outlier detection.
#'
#' @param data Dataframe or vector where to check outliers.
#' @param var Variable to be used for outlier detection if \strong{data} is not in a vector format.
#' @param output Either \strong{clean}: for clean data output without outliers; \strong{outliers}:
#'     for outlier data frame or vectors.
#' @param x The constant for creating lower and upper fences. Extreme is 3, but default is 1.5.
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @details
#' The loxplot for outlier detection \strong{Barbato et al. (2011)} modifies the
#' the interquartile range method to detect outlier but considering the sample sizes while indicating
#' the fences (lower and upper fences).
#'
#' \deqn{ lowerfence = [Q1 -1.5*IQR[1+0.1 * log(n/10)]}
#'
#' \deqn{upperfence = [Q3 +1.5*IQR[1+0.1 *log(n/10)]}
#'
#'      Where; Q1 is the lower quantile and Q3 is the upper quantile. The method consider the sample
#'      size in setting the fences, to address the weakness of the interquartile range method \emph{(Tukey, 1977)}.
#'      However. similar to IQR method for flagging outlier, log boxplot modification is affected by
#'      data skewness and which can be address using
#'       \link[specleanr]{distboxplot},  \code{\link{seqfences}}, \code{\link{mixediqr}} and
#'      \code{\link{semiIQR}}.
#'
#' @return Dataframe with our without outliers depending on the output.
#' \describe{
#' \item{clean}{Data without outliers.}
#' \item{outlier}{Data with outliers.}
#' }
#' @export
#'
#'
#' @references Barbato G, Barini EM, Genta G, Levi R. 2011. Features and performance of
#' some outlier detection methods. Journal of Applied Statistics 38:2133-2149
#'
#' @examples
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                         lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  logout <- logboxplot(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'
#'}
#'
logboxplot <- function(data, var, output, x=1.5, pc = FALSE, pcvar = NULL, boot = FALSE){

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)

  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  iqrcal <- stats::IQR(var)

  correction <- 1+0.1*log(length(var)/10)

  lowbound = quantile(var, 0.25) - x * iqrcal*correction

  upbound = quantile(var, 0.75) + x * iqrcal*correction

  datIn <- which(var<upbound & var>lowbound)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


#' Mixed Interquartile range and semiInterquartile range \code{Walker et al., 2018}
#'
#' @param data Dataframe or vector where to check outliers.
#' @param var Variable to be used for outlier detection if \strong{data} is not a vector file.
#' @param output Either \strong{clean}: for clean data output without outliers; \strong{outliers}:
#'     for outlier data frame or vectors.
#' @param x A constant for flagging outliers \code{Walker et al., 2018)}.
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Either clean our outliers
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                            lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  logout <- mixediqr(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'
#'}
#' @references
#' Walker ML, Dovoedo YH, Chakraborti S, Hilton CW. 2018. An Improved Boxplot for Univariate Data.
#'  American Statistician 72:348-353. American Statistical Association.
#'
mixediqr <- function(data, var, output, x=3, pc = FALSE, pcvar = NULL, boot = FALSE){

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)

  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  #semiintequantile range component
  semiQRl=  stats::quantile(var, 0.5)- stats::quantile(var, 0.25)

  #SIQRU = Q3 - Q2
  semiQRu=  stats::quantile(var, 0.75) - stats::quantile(var, 0.5)

  #Bowley’s Coefficient
  #For symmetrical data, BC is zero and the returns to IQR, which is similar to SEMIIQR
  bc <- (semiQRu-semiQRl)/(semiQRl+semiQRu)#

  numerator <- 1-bc

  denominator <- 1+bc

  frac <- numerator/denominator

  #interquantile range
  iqr <- IQR(var)


  lowbound  = stats::quantile(var, 0.25) - x*(iqr*frac)


  upbound = quantile(var, 0.75) + x*(iqr*frac)

  datIn <- which(var<upbound & var>lowbound)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' @title Median rule method
#'
#' @param data Dataframe or vector where to check outliers.
#' @param var Variable to be used for outlier detection if \strong{data} is not a vector file.
#' @param output Either \strong{clean}: for clean data output without outliers; \strong{outliers}:
#'     for outlier data frame or vectors.
#' @param x A constant for flagging outliers.
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Either clean or outliers.
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                           lat = 'decimalLatitude',
#'                           lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  medout <- medianrule(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'}
medianrule <- function(data, var, output, x=2.3, pc = FALSE, pcvar = NULL, boot = FALSE){

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)

  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  iqrcal <- stats::IQR(var)

  lowbound = quantile(var, 0.5) - x * iqrcal

  upbound = quantile(var, 0.5) + x * iqrcal

  datIn <- which(var<upbound & var>lowbound)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


#' @title Distribution boxplot
#'
#' @param data Dataframe or vector where to check outliers.
#' @param var Variable to be used for outlier detection if \strong{data} is not a vector file.
#' @param output Either \strong{clean}: for clean data output without outliers; \strong{outliers}:
#'     for outlier data frame or vectors.
#' @param p1,p2 Different pvalues for outlier detection \code{Schwertman et al. 2004)}.
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Either clean or outliers.
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#'danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                           lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  bxout <- distboxplot(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'}
#'
distboxplot <- function(data, var, output, p1=0.025, p2 = 0.975, boot = FALSE, pc = FALSE, pcvar = NULL){

  kndata <- specleanr::kdat

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)

  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  nd <- nrow(data)

  if(nd>400){
    nd = 0
  }else if(nd>=100 & nd <200){
    nd = 100
  }else if (nd>=200 && nd<300){
    nd = 200
  }else if(nd>=300 & nd<400){
    nd = 300
  }else{
    nd
  }

  kn <- kndata$kn[which(as.numeric(kndata$n) == nd)]

  #semi intequantile range component
  semiQRl=  unname(stats::quantile(var, 0.5))- unname(stats::quantile(var, 0.25))

  semiQRu=  unname(stats::quantile(var, 0.75)) - unname(stats::quantile(var, 0.5))

  alphaL <- (2*semiQRl)/kn

  alphaU <- (2*semiQRu)/kn

  stdalphaL <- qnorm(p1, lower.tail = FALSE)

  stdalphaU <- qnorm(p2, lower.tail = TRUE)

  lowbound  = stats::quantile(var, 0.5) - stdalphaL * alphaL

  upbound = quantile(var, 0.5) + stdalphaU * alphaU

  datIn <- which(var<upbound & var>lowbound)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' @title Sequential fences method
#'
#' @param data Dataframe or vector where to check outliers.
#' @param var Variable to be used for outlier detection if \strong{data} is not a vector file.
#' @param output Either \strong{clean}: for clean data output without outliers; \strong{outliers}:
#'     for outlier data frame or vectors.
#' @param gamma \code{numeric}. the p-values used to classify a record as an outlier. The lower the p-value,
#'      the extremeness is the outlier \code{Schwertman & de Silva 2007}.
#' @param mode \code{string}. Indicates the extremeness of the outlier.
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#'
#' @details
#' Sequential fences is a modification of the TUKEY boxplot, where the data is divided into groups each with its own
#' fences \code{Schwertman & de Silva 2007}. The groups can range from 1, which flags mild outliers to 6 for extreme outliers ()
#'
#'
#' @return Dataframe or vector with or without outliers
#'
#' @importFrom stats qnorm qt
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                            lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                           colsp = "scientificName",
#'                           bbox = db,
#'                           minpts = 10)
#'
#'  sqout <- seqfences(data = refdata[["Thymallus thymallus"]], var = 'bio6', output='outlier')
#'}
#'
#' @references
#'
#' \enumerate{
#'
#' \item Schwertman NC, de Silva R. 2007. Identifying outliers with sequential fences.
#' Computational Statistics and Data Analysis 51:3800-3810.
#'
#' \item Schwertman NC, Owens MA, Adnan R. 2004. A simple more general boxplot method for identifying outliers.
#' Computational Statistics and Data Analysis 47:165-174.
#'
#' \item Dastjerdy B, Saeidi A, Heidarzadeh S. 2023.
#' Review of Applicable Outlier Detection Methods to Treat Geomechanical Data. Geotechnics 3:375-396. MDPI AG.
#'
#' }
#'

seqfences <- function(data, var, output, gamma=0.95, mode='eo', pc = FALSE, pcvar = NULL, boot = FALSE){

  if(!gamma%in%c(0.75, 0.80, 0.90, 0.95, 0.975, 0.99, 0.995)){

    stop('Only 0.75, 0.80, 0.90, 0.95, 0.975, 0.99, 0.995 are allowed values for gamma.')
  }

  #standard table 1 from (Schwertman NC, de Silva R. 2007)

  kndata <- specleanr::kdat

  #standard table 2 from (Schwertman NC, de Silva R. 2007)
  mth <- specleanr::mth

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)

  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  nd <- nrow(data)

  if(nd>100) stop('Sequence fence is only computed for sample size less than 100.') else nd

  kn <- kndata$kn[which(as.numeric(kndata$n) == nd)]

  #get confidence coefficients
  df <- as.integer(7.6809524 + .5294156*nd - .00237*nd^2)

  confs <- c('m1', 'm2', 'm3', 'm4', 'm5', 'm6')

  lf <- c(); uf <- c(); tv <- c()
  for (si in seq_along(confs)) {
    m <- confs[si]

    mc <- mth[,m][which(mth$p== gamma)]

    tv <- qt(mc/length(var), df, lower.tail = FALSE)

    lf[si] <- unname(quantile(var, 0.5)) - (tv/kn)*IQR(var)
    uf[si] <- unname(quantile(var, 0.5)) + (tv/kn)*IQR(var)
  }

  indx <- c()
  for (sii in seq_along(var)){
    vals <- var[sii]
    if(mode=='eo'){
      ev <- 1
    }else if(mode=='meo'){
      ev <- 2
    }else if(mode=='leo'){
      ev <- 3
    }else if (mode == 'ao'){
      ev <- 4
    }else if(mode=='wo'){
      ev <- 5
    }else{
      ev <- 6
    }
    lv <- vals < lf[ev] | vals > uf[ev]
    if(any(lv)==TRUE)  tf <- TRUE else tf <- FALSE
    indx[sii] <- tf
  }

  datIn <- which(indx==FALSE)

  if(isTRUE(pc)) datIn <- which(varc %in% unique(varc[datIn])==TRUE) else datIn

  switch(output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' @title Identify outliers using isolation forest model.
#'
#' @param data Dataframe of environmental variables extracted from where the species was recorded present or absent.
#' @param size Proportion of data to be used in training isolation forest n´model. It ranges form 0.1 (fewer data  selected ) to 1 to all data used in
#' training isolation model.
#' @param cutoff Cut to select where the record was an outlier or not.
#' @param output Either clean: for a data set with no outliers or outlier: to output a dataframe with outliers. Default is 0.5.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#'
#' @importFrom isotree isolation.forest
#' @importFrom stats predict
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data("efidata")
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                        lat = 'decimalLatitude',
#'                        lon= 'decimalLongitude',
#'                        colsp = "scientificName",
#'                       bbox = db,
#'                        minpts = 10)
#'
#' iosd <- isoforest(data = refdata[["Thymallus thymallus"]], size = 0.7,  output='outlier',
#'                   exclude = c("x", "y"))
#' }
#'
#' @references
#' \enumerate{
#' \item Liu FeiT, Ting KaiM, Zhou Z-H. 2008. Isolation Forest. Pages 413–422 In 2008 Eighth IEEE International Conference on Data Mining.
#' Available from https://ieeexplore.ieee.org/abstract/document/4781136 (accessed November 18, 2023).
#' }
#'
isoforest <- function(data, size, cutoff =0.5, output, exclude = NULL, pc = FALSE, boot = FALSE, pcvar = NULL, var){

  match.argc(output, choices = c('clean', 'outlier'))

  if(cutoff<0 |cutoff>1)stop('cutoff should range from 0 to 1')

  if(size<0 | size>1)stop('size should range from 0 to 1')

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]
  if(isTRUE(pc)) pcdf <- pars[['pcdf']] else pcdf <- pars[['data']]

  if(!is.null(exclude))  df<- pcdf[!colnames(pcdf)%in%exclude] else df <- pcdf

  isomodel <- isolation.forest(data= df, sample_size = size, ntrees = 100, 1)

  isopred <- predict(isomodel,df)

  datIn <- which(isopred< cutoff)

  switch (output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' @title Identify outliers using One Class Support Vector Machines
#'
#' @param data Dataframe of environmental variables extracted from where the species was recorded present or absent.
#' @param kernel Either radial, linear
#' @param tune To performed a tuned version of one-class svm. High computation requirements needed.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesnot want to consider.
#' @param output Either clean: for a dataset with no outliers or outlier: to output a dataframe with outliers.
#' @param tpar A list of parameters to be varied during tunning from the normal model.
#'
#' @importFrom  e1071 svm tune.svm
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                        lat = 'decimalLatitude',
#'                        lon= 'decimalLongitude',
#'                        colsp = "scientificName",
#'                       bbox = db,
#'                        minpts = 10)
#'
#' nedata <- onesvm(data = refdata[["Thymallus thymallus"]], exclude = c("x", "y"),  output='outlier')
#'
#'}
onesvm <- function(data, kernel='radial', tune=FALSE, exclude = NULL, output,
                    tpar = list(gamma = 1^(-1:1), epislon =seq(0, 1, 0.1),
                                cost =2^2:4, nu = seq(0.05, 1, 0.1)),
                    boot = FALSE, pc = FALSE, var, pcvar = NULL){

  match.argc(kernel, choices = c('radial', 'linear')) #radial set to defualt, works well for high dimensional data

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  if(isTRUE(pc)) pcdf <- pars[['pcdf']] else pcdf <- pars[['data']]

  if(!is.null(exclude))  df<- pcdf[!colnames(pcdf)%in%exclude] else df <- pcdf

  if(isFALSE(tune)){

    svm_model <- svm(df, type = 'one-classification', nu=0.1, scale = TRUE, kernel = kernel)

    prediction <- predict(svm_model, df)


  }else{

    tunemodel =  tune.svm(x=df, y=rep(T,nrow(df)), type='one-classification', kernel= kernel,
                          nu=tpar$nu, gamma = tpar$gamma)#create TRUE class to validate the model

    modnew <- tunemodel$best.model

    prediction <- predict(modnew, df)

  }
  datIn <- which(prediction==TRUE)

  switch (output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


#' @title Flags suspicious using the local outlier factor or Density-Based Spatial Clustering of Applications with Noise.
#'
#' @param data Data frame of species records with environmental data
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for data frame with no suspicious outliers or outlier: to return dataframe with only outliers.
#' @param minPts Minimum neighbors around the records.
#' @param metric Distance-based measure to examine the distance between variables. Default \code{manhattan}.
#' @param mode Either \code{soft} if mean is used or \code{robust} if mad is used. Default \code{soft}.
#'
#'
#' @importFrom dbscan lof kNN glosh
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                        lat = 'decimalLatitude',
#'                        lon= 'decimalLongitude',
#'                        colsp = "scientificName",
#'                       bbox = db,
#'                        minpts = 10)
#'
#' lofout <- xlof(data = refdata[["Thymallus thymallus"]], exclude = c("x", "y"),
#'                 output='outlier', metric ='manhattan',
#'                 minPts = 10, mode = "soft")
#'}
#'
xlof <- function(data, output, minPts, exclude = NULL, metric = 'manhattan', mode='soft', pc = FALSE, boot = FALSE, var, pcvar = NULL){

  match.argc(mode, choices = c('robust', 'soft'))

  match.argc(metric, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary"))


  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]
  if(isTRUE(pc)) pcdf <- pars[['pcdf']] else pcdf <- pars[['data']]

  if(!is.null(exclude)) df<- pcdf[!colnames(pcdf)%in%exclude] else df <- pcdf

  dxl <- dist(x=df, method = metric)

  lscores <- dbscan::lof(dxl, minPts = minPts)

  #remove infinity values or NA values

  lclean <- lscores[!is.infinite(lscores) & !is.na(lscores)]

  lmscores <- mean(lclean)

  lsdscores <- sd(lclean)

  lmedscores <- stats::median(lclean)

  lmadscores <- stats::mad(lclean)

  lzval <- c()

  for(isl in 1:length(lscores)){

    lzval[isl] <- switch(mode, soft=(lscores[isl]-lmscores)/lsdscores,
                         robust=(lscores[isl]-lmedscores)/lmadscores)
  }

  datIn <- which(lzval<2 & lzval> (-2))

  switch (output, clean= return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' @title k-nearest neighbors for outlier detection
#'
#' @param data Data frame of species records with environmental data.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#'      latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for data frame with no suspicious outliers or outlier: to return dataframe with only outliers.
#' @param metric The different metric distances to compute the distances among the environmental predictors. See \code{dist} function and
#'        how te different distances are applied. The different measures are allowed including
#'        \code{"euclidean", "maximum", "manhattan", "canberra", "binary"}.
#' @param mode This includes \code{soft} when the outliers are removed using mean to compute the z-scores or \code{robust} when
#'      median absolute deviation.
#'
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                        lat = 'decimalLatitude',
#'                        lon= 'decimalLongitude',
#'                        colsp = "scientificName",
#'                        bbox = db,
#'                        minpts = 10)
#'
#' lofout <- xknn(data = refdata[["Thymallus thymallus"]], exclude = c("x", "y"),
#'                 output='outlier', metric ='manhattan',
#'                  mode = "soft")
#'
#'}
xknn <- function(data, output, exclude = NULL, metric = 'manhattan', mode='soft', pc = FALSE, boot = FALSE, var, pcvar = NULL){

  match.argc(mode, choices = c('robust', 'soft'))

  match.argc(metric, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary"))

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  if(isTRUE(pc)) pcdf <- pars[['pcdf']] else pcdf <- pars[['data']]

  if(!is.null(exclude)) df<- pcdf[!colnames(pcdf)%in%exclude] else df <- pcdf

  dx <- dist(x=df, method = metric)

  knx <- dbscan::kNN(dx, k=3)

  kscores <- c()

  for (iknn in 1:nrow(knx$dist)) {

    kscores[iknn] <- mean(knx$dist[iknn,])
  }

  mscores <- base::mean(kscores)

  sdscores <- stats::sd(kscores)


  medscores <- stats::median(kscores)

  madscores <- stats::mad(kscores)

  zval <- c()

  for(isc in 1:length(kscores)){

    zval[isc] <- switch(mode, soft=(kscores[isc]-mscores)/sdscores, robust=(kscores[isc]-medscores)/madscores)
  }

  datIn <- which(zval<2 & zval> (-2))

  switch (output, clean= return(data[datIn,]), outlier= return(data[-datIn,]))
}


#' @title Global-Local Outlier Score from Hierarchies
#'
#' @param data Data frame of species records with environmental data.
#' @param k The size of the neighborhood \code{(Hahsler et al 2022)}.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#'      latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for data frame with no suspicious outliers or outlier: to return dataframe with only outliers.
#' @param metric The different metric distances to compute the distances among the environmental predictors. See \code{dist} function and
#'        how te different distances are applied. The different measures are allowed including
#'        \code{"euclidean", "maximum", "manhattan", "canberra", "binary"}.
#' @param mode This includes \code{soft} when the outliers are removed using mean to compute the z-scores or \code{robust} when
#'      median absolute deviation.
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' data("efidata")
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                        lat = 'decimalLatitude',
#'                        lon= 'decimalLongitude',
#'                        colsp = "scientificName",
#'                        bbox = db,
#'                        minpts = 10)
#'
#' gloshout <- xglosh(data = refdata[["Thymallus thymallus"]], exclude = c("x", "y"),
#'                 output='outlier', metric ='manhattan', k = 3,
#'                  mode = "soft")
#'}
#
#' @references
#' \enumerate{
#' \item Campello, Ricardo JGB, Davoud Moulavi, Arthur Zimek, and Joerg Sander.
#' Hierarchical density estimates for data clustering, visualization, and outlier detection.
#' ACM Transactions on Knowledge Discovery from Data (TKDD) 10, no. 1 (2015). doi:10.1145/2733381
#' \item Hahsler M, Piekenbrock M (2022). dbscan: Density-Based Spatial Clustering of Applications with Noise (DBSCAN) and Related Algorithms. R
#' package version 1.1-11, <https://CRAN.R-project.org/package=dbscan>
#' }
#'
xglosh <- function(data, k, output, exclude = NULL, metric = 'manhattan', mode='soft', pc = FALSE, boot = FALSE, var, pcvar = NULL){

  match.argc(mode, choices = c('robust', 'soft'))

  match.argc(metric, choices = c("euclidean", "maximum", "manhattan", "canberra", "binary"))

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  if(isTRUE(pc)) pcdf <- pars[['pcdf']] else pcdf <- pars[['data']]

  if(!is.null(exclude)) df<- pcdf[!colnames(pcdf)%in%exclude] else df <- pcdf

  dx <- dist(x=df, method = metric)

  gscores <- dbscan::glosh(dx, k = k)

  gmscores <- mean(gscores)

  gsdscores <- stats::sd(gscores)


  gmedscores <- stats::median(gscores)

  gmadscores <- stats::mad(gscores)

  gzval <- c()

  for(isg in 1:length(gscores)){

    gzval[isg] <- switch(mode, soft=(gscores[isg]-gmscores)/gsdscores, robust=(gscores[isg]-gmedscores)/gmadscores)
  }

  datIn <- which(gzval<2 & gzval> (-2))

  switch (output, clean= return(data[datIn,]), outlier= return(data[-datIn,]))
}

#' @title Check for environmental outliers using species optimal ranges.
#'
#' @param data Dataframe with environmental predictors for a species or multiple species.
#' @param species The species should be indicated if the minimum \code{minval} and
#'      maximum values \code{maxval} are provided.
#' @param var Environmental parameter considered in flagging suspicious outliers.
#' @param output Either \code{clean}: for dataframe with no suspicious outliers or
#'          \code{outlier} to return dataframe with only outliers.
#' @param minval,maxval Minimum  and maximum values (ranges) for a particular that are used to flag out
#'        values outside the ranges.
#' @param optimumSettings A list of optimal parameters are provided mostly when multiple species
#'        are examined.
#'         \itemize{
#'         \item{\code{optdf}: is the dataframe with species optimal values (min, max, or ecoparam). This dataset
#'         can be generated from literature for different species.
#'         }
#'        \item{\code{optspcol}: Is the column with species names in the \code{optdf} dataset. }
#'         \item{\code{mincol}: Is the column name in the \code{optdf} with minimum values. }
#'         \item{\code{maxcol}: Is the column name in the \code{optdf} with maximum values. }
#'         \item{\code{ecoparam}: If in the \code{optdf} the minimum and maximum values are not found, then the
#'         the column with ecoparam should be provided.}
#'         \item{\code{direction}: If ecoparam is provided in the \code{optdf}, then column for direction should be provided.}
#'         }
#' @param lat,lon If the \code{checkfishbase} and \code{mode} are set, then the columns for latitude
#'        longitude should be provided.
#' @param ecoparam This parameter is used only when the lower bound (minimum) and upper bound maximum
#'                or ranges are absent. For example, if only minimum value is present for a particular
#'                species, then ecoparam is set and the direction is provided whether lower, greater,
#'                equal, less/equal or greater/equal the ecoparam value provided.
#' @param direction This indicates if the provided ecological threshold \code{ecoparam} or ranges is greater than
#'       \code{greater}, less than \code{less}, equal \code{equal}, less or equal \code{le} or
#'       greater or equal \code{ge}. Íf the minimum and maximum values are known, then the \code{ecoparam}
#'       and \code{direction} should not be used.
#' @param pct The percentage similarity of the species name provided by the user and the one  in FishBase.
#'        Only fish species names are checked with Fishbase but other taxa can be checked using
#'        \code{taxize} package.
#' @param checkfishbase Either \code{TRUE} to check for both temperatures \code{temp} and latitudinal or
#'        geographical ranges \code{geo}. If the \code{checkfishbase} is set to \code{TRUE} then the
#'        \code{mode} parameter must be set to either \code{geo or temp}. This function applies for only
#'        fish species.
#' @param mode Either \code{geo} or \code{temp} for latitudinal ranges or temperature ranges respectively.
#'        See \code{\link{thermal_ranges}} or \code{\link{geo_ranges}} on how to obtain the data.
#' @param warn Either \code{TRUE} to return warning messages or \code{FALSE} for no warning messages.
#'        the defualt is \code{FALSE}:
#' @param output output Either clean: for dataframe with no suspicious outliers or outlier: to retrun dataframe with only outliers.
#'
#' @importFrom utils adist
#'
#' @return Dataframe with or with no outliers.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#' data("jdsdata")
#'
#' datafinal <- match_datasets(datasets = list(jds = jdsdata, efi=efidata),
#'                             lats = 'lat',
#'                             lons = 'lon',
#'                             species = c('speciesname','scientificName'),
#'                             date = c('Date', 'sampling_date'),
#'                             country = c('JDS4_site_ID'))
#'
#' efidata <- check_names(data = datafinal, colsp='species', pct=90, merge=TRUE)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                          lat = 'decimalLatitude', lon= 'decimalLongitude',
#'                         colsp = "scientificName",
#'                        bbox = db,
#'                         minpts = 10)
#'
#' saldata <- refdata[["Thymallus thymallus"]]
#' #1. checking the annual maean temperature (bio1) are within the ranges in FishBase
#' salmotherange <- thermal_ranges(x = "Salmo trutta")
#'
#' sdatatemp <- ecological_ranges(data = saldata, var = 'bio1', species = "Salmo trutta",
#'                               checkfishbase = TRUE, mode = 'temp', output = 'outlier')
#' #zero record no outliers
#' #====
#' #2. geographical ranges: latitude longitude
#' #geo ranges in fishbase
#' salgeorange <- geo_ranges(data = "Salmo trutta")
#' sdatageo <- ecological_ranges(data = saldata, lat = 'y', lon = 'x', output = 'outlier',
#'                               species = "Salmo trutta",
#'                               checkfishbase = TRUE, mode = 'geo')
#' #3. GENERAL LITERATURE RANGES
#' #======
#' #1. when the min and and max are provided
#' #multiple FALSE SHOULD BE SET
#' #3.1: If only the minimum value is present: assuming minimum temperature is 6, varible: bio1
#' #direction less than 6.0 is outlier and greater is not
#' sdata <- ecological_ranges(data = saldata, ecoparam = 6.0, var = 'bio1',
#'                            direction = 'greater' )
#' #3.2
#' sdata2 <- ecological_ranges(data = saldata, var = 'bio1', minval = 2,
#'                             maxval = 24, species = "Salmo trutta" )
#'
#' #4. Multiple TRUE
#' #the optimal parameters should be provided in a dataframe format with min max, or ecoparam
#' #4.1 optimal dataset
#'
#' optdata <- data.frame(species= c("Salmo trutta", "Abramis brama"),
#'                       mintemp = c(6, 1.6),maxtemp = c(20, 21),
#'                       meantemp = c(8.5, 10.4), #ecoparam
#'                       direction = c('greater', 'greater'))
#'
#' #parameter used is annual mean temperature (WORLDCLIM)
#' #provide the column with species names in the environment dataset
#' #set optimal list parameter
#' #
#' # #optimal parameters
#' sdata3 <- ecological_ranges(data = saldata, species = 'Salmo trutta',
#'                             var = 'bio1', output = "outlier",
#'                             optimumSettings = list(optdf = optdata,maxcol = "maxtemp",
#'                                                    mincol ="mintemp",optspcol = "species"))
#' #
#' #
#' #only one ecological parameter (ecoparam is provided) and direction
#' sdata4 <- ecological_ranges(data = saldata, species = 'Salmo trutta', var = 'bio1',
#'                             output = "outlier",
#'                             optimumSettings = list(optdf = optdata,
#'                                                    ecoparam = "meantemp",
#'                                                    optspcol = "species",
#'                                                    direction= "direction"))
#' }
ecological_ranges <- function(data, var, output= "outlier", species = NULL,
                              optimumSettings = list(optdf =NULL, optspcol = NULL,
                                                     mincol = NULL, maxcol = NULL,
                                                     ecoparam = NULL, direction= NULL),
                              minval=NULL, maxval=NULL, lat = NULL, lon = NULL,
                              ecoparam=NULL, direction = NULL,
                              pct= 80,
                              checkfishbase = FALSE, mode=NULL, warn=TRUE){

  match.argc(output, choices = c('clean', 'outlier'))

  if(!is.null(mode)) match.argc(mode, choices = c('geo', 'temp'))

  if(!is.null(direction)) match.argc(direction, choices = c('equal','less','greater','le','ge'))

  if(is.null(data))stop("Environmental data for the species should be provided.")

  if(!is.null(optimumSettings$optdf)){

    if(!is(optimumSettings$optdf, "data.frame")){

      stop("A dataframe is expected for species optimal ecological parameters.")

    } else{

      if(is.null(optimumSettings$optspcol))stop('Provide column with species names in optimal data.')

      spname_opt <- unlist(optimumSettings$optdf[, optimumSettings$optspcol])

      #check if the species in the provided data has optimal ranges

      if((species%in%spname_opt)==TRUE){ #check if the species is in the dataframe for optimal parameters.

        spp <-  species

      }else if((species%in%spname_opt)==FALSE){

        xdist <- adist(species, spname_opt)

        spsel <- spname_opt[which(xdist==min(xdist))]

        if(length(spsel)>1) spfinal <- spsel[1] else spfinal <- spsel

        #use percentage similarity
        pc = (100-(min(xdist)/nchar(spfinal))*100)
        if(pc>pct) {
          spp <- spfinal
        }
        else{
          message(species, " does not have min,  max or optimal ranges and the original data will be outputted for clean data and NA for outliers.")
          switch(output, clean= return(data) , outlier = return(NA))
        }
      }else{
        message(species, " does not have min,  max or optimal ranges and the original data will be outputted and NA for outliers.")
        switch(output, clean= return(data) , outlier = return(NA))
      }
    }

    #when the multiple species and maximum(maxcol)/minimum(mincol) are provided

    if(is.null(optimumSettings$ecoparam)){
      #get min and max values

      if(!optimumSettings$maxcol%in%colnames(optimumSettings$optdf)) stop("Parameter names for species, maxcol, not in the optimal data.")

      if(!optimumSettings$mincol%in%colnames(optimumSettings$optdf)) stop("Parameter names for species, mincol, not in the optimal data.")

      parmin = optimumSettings$optdf[, optimumSettings$mincol]

      parmax = optimumSettings$optdf[, optimumSettings$maxcol]

      minv = parmin[which(optimumSettings$optdf[, optimumSettings$optspcol] ==spp)]

      maxv = parmax[which(optimumSettings$optdf[, optimumSettings$optspcol] ==spp)]


      dx = sprange(data = data, var = var, minval = minv, maxval = maxv)

      switch(output, clean = return(data[dx,]), outlier = return(data[-dx,]))

      #1. when the ecoparam is provided in the optimal dataset, for example if only mean temperature is provided then we can use

      #2. algebra as greater, less, equal, less than (le), or greater than (ge) MUST BE INDICATED

    }else if(!is.null(optimumSettings$ecoparam)){

      #when only one parameter is provided e.g., mean temperature, spi etc

      parv = optimumSettings$optdf[, optimumSettings$ecoparam]

      dirv = optimumSettings$optdf[, optimumSettings$direction]

      pvalue = parv[which(optimumSettings$optdf[, optimumSettings$optspcol] == spp)]

      direction = dirv[which(optimumSettings$optdf[, optimumSettings$optspcol] == spp)]

      dx = sprange(data = data, var = var, ecoparam = pvalue, direction = direction)

      switch(output, clean = return(data[dx,]), outlier = return(data[-dx,]))
    }else{
      stop("Provide either min/max or ecological preference value such as mean temperate.")
    }
  }else if(isTRUE(checkfishbase)){


    if(mode=="temp"){

      ranges <- thermal_ranges(x = species)

      if(nrow(ranges)>=1){
        minv <- ranges$tempmin
        maxv <- ranges$tempmax

        dx = sprange(data = data, var = var, minval = minv, maxval = maxv)

      }else{

        message("No temperature ranges for ", species ," from FishBase and original data will be output from clean data.")

        switch(output, outlier= return(NA), clean = return(data))

      }
    }else{

      geox <- geo_ranges(data = species, warn = warn )

      if(any(is.na(geox))== FALSE){

        dx = sprange(data = data, lat = lat, lon = lon, geo = geox)
      }else{
        message("No latitudinal/longitudinal ranges for the species from FishBase and orginal data will be output from clean data.")

        switch(output, outlier= return(NA), clean = return(data))
      }

    }

  }else{
    #if it is one species, and manual input of optimal parameters: min and max or ecoparam (when only one is provided.)

    dx <- sprange(data = data, var = var, minval = minval, maxval = maxval,
                  ecoparam = ecoparam , direction = direction)
  }

  switch(output, clean = return(data[dx,]), outlier = return(data[-dx,]))
}


#' @noRd
sprange <- function(data, var, minval = NULL, maxval = NULL, ecoparam = NULL, lat =NULL, lon=NULL,
                    direction = NULL, geo=NULL){

  if(!is.null(geo)){
    lat = unlist(data[, lat])
    lon = unlist(data[, lon])
  }else{
    if(is.null(var))stop("Provide the column for variable of concern in var parameter")
    var = unlist(data[, var])
  }
  #converting the latitudinal ranges to a vector
  geocodes <- c(geo)

  #
  if(is.null(ecoparam)){

    if(is.null(geo)){

      if(any(sapply(list(maxval, minval), is.null))==TRUE) stop("Can not proceeed without both maxval and minval ranges. If one is available, use ecoparam parameter and provide direction.")

      datIn  <-  which(var<=maxval & var >=minval)


    }else{
      if(any(sapply(list(lat, lon), is.null))==TRUE) stop("Can not proceeed without both latitudes and longitudes columns. If one is available, use ecoparam parameter and provide direction.")

      gcheck <- mapply(function(x, y){
        tf <- c(y<=geocodes[1], y>=geocodes[2], x>=geocodes[3], x<=geocodes[4])

        if(any(is.na(tf))){
          datIn <- NA
        } else if(all(tf==TRUE)){
          datIn <-  x
        } else{
          datIn <- NA
        }
        return(datIn)
      }, x=lon, y= lat)

      datIn <- which(lon %in%gcheck[!is.na(gcheck)])
    }
  }else{
    if(is.null(direction)) stop("For ecoparam indicate if the value is equal, less, or greater.")

    datIn = switch(direction,
                   equal=which(var==ecoparam),
                   greater = which(var>ecoparam),
                   less = which(var<ecoparam),
                   ge = which(var>=ecoparam),
                   le = which(var<=ecoparam))
  }
  return(datIn)
}

#' @title Flags outliers based on Mahalanobis distance matrix for all records.
#'
#' @param data \code{dataframe}. Dataframe to check for outliers or extract the clean data.
#' @param exclude \code{vector or string} Variables that should not be
#'   considered in the executing the Mahalanobis distance matrix. These can be
#'   coordinates such as latitude/longitude or any column that the user doesn't
#'   want to consider.
#' @param output \code{string} Either \code{clean} for a data set with no outliers or \code{outlier} to
#'   output a data frame with outliers.
#' @param mode \code{string} Either \code{robust}, if a robust mode is used which uses \code{auto} estimator to
#'  instead of mean. Default mode is \code{soft}.
#' @param pdf \code{numeric} chisqure probability distribution value used for flagging outliers
#'   \code{(Leys et al. 2018)}. Default is \code{0.95}.
#' @param tol \code{numeric} tolernce value when the inverse calculation are too
#'   small. Default \code{1e-20}.
#'
#' @importFrom stats mahalanobis qchisq cov complete.cases
#' @importFrom usdm vifcor vifstep exclude
#' @importFrom robust covRob
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Either clean or outliers dataset
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data("efidata")
#'
#'
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                        lat = 'decimalLatitude',
#'                        lon= 'decimalLongitude',
#'                        colsp = "scientificName",
#'                       bbox = db,
#'                        minpts = 10)
#'
#' #outliers
#' outliers <- mahal(data = refdata[["Thymallus thymallus"]], exclude = c("x", "y"),
#'                         output='outlier')
#'}
#' @references
#' Leys C, Klein O, Dominicy Y, Ley C. 2018. Detecting multivariate outliers:
#' Use a robust variant of the Mahalanobis distance. Journal of Experimental
#' Social Psychology 74:150-156.
#'
mahal <- function(data, exclude = NULL, output = 'outlier', mode = 'soft', pdf = 0.95, tol= 1e-20, pc = FALSE, boot = FALSE, var, pcvar = NULL){

  if(missing(data)) stop('Data not provided.')

  match.argc(mode, choices = c('soft', 'robust'))

  match.argc(output, choices = c('clean', 'outlier'))

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  if(isTRUE(pc)) pcdf <- pars[['pcdf']] else pcdf <- pars[['data']]

  dfna <- pcdf[complete.cases(pcdf),]

  if(!is.null(exclude))  df <- dfna[,!colnames(dfna) %in% exclude] else df <- dfna

  #check for multicolinearity in the data

  #check if y is in the colnames

  if(("y" %in% colnames(df))==TRUE){

    warning('vifstep uses y to fit the model and this will create an error. Rename variable y or exclude it  or vifcor will be used instead.')

    df2<- suppressWarnings(usdm::exclude(df, usdm::vifcor(df)))

    }else{

      df2<- suppressWarnings(usdm::exclude(df, usdm::vifstep(df)))
  }
  if(nrow(df2)<ncol(df2)){
    stop('Inverse matrix cannot be computed if number of variables are equal or greater than observations and NULL output generated')

  }else if(det(cov(df2))==0){
    stop('The determinant of the convariance matrix is zero and solve function failed.')

  }else{
    if(mode=='soft'){

      mdist <- stats::mahalanobis(df2 , center =  colMeans(df2), cov =  stats::cov(df2), tol= tol)

    }else{

      rob <- tryCatch(expr = robust::covRob(df2),
                      error=function(e)NULL,
                      warning = function(w)NULL)
      if(!is.null(rob)){
        cobj =  robust::covRob(df2)

        covmatrix <- cobj$cov

        centermatrix <- cobj$center

        mdist <- stats::mahalanobis(df2, center = centermatrix, cov = covmatrix,tol= tol)
      }else{
        stop("The robust mode did not succesfully execute.")
      }
    }
    cutoff <- qchisq(p=pdf, df = ncol(df2))

    datIn <- which(mdist<cutoff)

    switch(output, clean=return(data[datIn,]), outlier=return(data[-datIn,]))
  }
}

#' @title Flags outliers using kmeans clustering method
#'
#' @param data Dataframe to check for outliers
#' @param k The number of clusters to be used for optimization. It should be greater than 1. For many species k should be be greater 10
#' to ably cater for each species search for optimal k using the different optimization methods in kmethod
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for a data set with no outliers or outlier: to output a data frame with outliers.
#' @param mode Either robust, if a robust mode is used which uses median instead of mean and median absolute deviation from median.
#' @param verbose To indicate messages and the default is FALSE.
#' @param seed  An integer to fix the maintain the iterations by during the kmeans method optimisation.
#' @param method The method to be used for the kmeans clustering. Default is \code{silhouette}.
#' \code{Elbow method} can be used but user input is required, and therefore multiple outlier detection method
#' is not possible.
#'
#' @importFrom stats kmeans sd dist
#' @importFrom cluster silhouette
#' @inheritParams pcboot
#' @inheritParams boots
#' @inheritParams pca
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' refdata <- pred_extract(data = efidata, raster= wcd ,
#'                         lat = 'decimalLatitude',
#'                         lon= 'decimalLongitude',
#'                         colsp = "scientificName",
#'                         bbox = db,
#'                         minpts = 10)
#'
#' kmeansout <- xkmeans(data = refdata[["Thymallus thymallus"]],
#'                       output='outlier', exclude = c('x', 'y'), mode = 'soft', k=3)
#'}
#'
xkmeans <- function(data, k, exclude = NULL, output, mode ="soft", method="silhouette",
                    seed = 1135, verbose=FALSE, pc = FALSE, boot = FALSE, var, pcvar = NULL){

  match.argc(mode, choices = c('soft', 'robust'))

  match.argc(method, choices = c("silhouette", "elbow"))

  match.argc(output, choices = c("clean", "outlier"))

  #set seed to fix the iteration in k-means tuning and optimization
  set.seed(seed)

  pars <- pcboot(pb = data, var = var, pc = pc, boot = boot, pcvar = pcvar)
  var <-  pars[["var"]]
  data <- pars[['data']]
  varc <- pars[['varc']]

  if(isTRUE(pc)) pcdf <- pars[['pcdf']] else pcdf <- pars[['data']]

  if(!is.null(exclude)) df2 <- pcdf[,!colnames(pcdf) %in% exclude] else df2 <- pcdf

  df_scaled <- scale(df2)

  if(k<1 || k==1) stop('Increase k to atleast 2 to form clusters in the data.')

  if(nrow(df_scaled)<10) k = 3 else k = k # to avoid errors for few data.

  wcss <- c(); kcenters <- c(); sil_val <- list()

  if(method=='elbow'){

    for (ik in 2:k) {

      km <- stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4)

      wcss[ik-1] <- km$tot.withinss

      kcenters[ik-1] <- ik

      plot(x=kcenters, y=wcss, xlab='Number of clusters', ylab='Total within sum of squares',
           type = "b", main='Choose cluster number where the line forms an bend')

    }
    opt_k <- as.integer(readline(prompt = 'Enter the value of k with a bend: '))

    if(opt_k>length(kcenters)) stop('Optimal cluster centers should be within the
                                   number of cluster iteration number: ', k, call. = FALSE)
  }
  else if(method=='silhouette'){

    #lower the k value for low samples but that can not looped through the different values

     for (ik in 1:k) {

      #Check if an error is raised during the loop but when at low k values like 3, the iteration was successfully, choose a default automatically

      kx <- tryCatch(expr = stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4), error = function(e) e)

      if(inherits(kx, 'error') && nrow(df_scaled)>=10) {

        km = stats::kmeans(df_scaled, centers = 3, iter.max = 100, nstart = 4)

      }else{
        km = stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4)
      }

      sil <- cluster::silhouette(km$cluster, dist(df_scaled))

      if(length(sil)>1){

        sil_val[[ik]] <- sil[,3]

      }else{
        if(isTRUE(verbose)) message('The k centers should be greater than 1 to compute silhouette neighbour clusters')

      }
    }

    meanlst <- sapply(sil_val[-which(sapply(sil_val, is.null))], mean)

    opt_k <- which(meanlst==max(meanlst))+1 #to cater for 1-k means

    if(isTRUE(verbose))message('The optimal k value of ', opt_k, ' has been selected')

  }else{
    if(isTRUE(verbose)) message('Optimal k optimal method not selected and a default value of 3 will be used')
    opt_k = 3
  }

  km_new <- stats::kmeans(df_scaled, centers = opt_k, iter.max = 100, nstart = 4)

  eu_dist<- sqrt(rowSums(df_scaled - km_new$centers[km_new$cluster,])**2)#euclidean distances

  meaneu <- mean(eu_dist)

  sdeu <- stats::sd(eu_dist)

  medianeu <- stats::median(eu_dist)

  madeu <- stats::mad(eu_dist)

  zscores <- c()

  for(iik in 1:length(eu_dist)){

    zscores[iik] <- switch(mode, soft=(eu_dist[iik]-meaneu)/sdeu, robust=(eu_dist[iik]-medianeu)/madeu)
  }

  datIn <- which(zscores<2 & zscores> (-2))

  switch(output, clean= return(data[datIn,]), outlier=return(data[-datIn,]))
}



