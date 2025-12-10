

#identify absolute outliers, proportions and best methods.

#' @noRd
oci <- function(absoluteoutliers, absolute_propn, threshold, listofmethods,
                absolute, props, autothreshold){

  #Repeat step 2 but with only absolute outliers selected using a user-based threshold

  #the method obtained here will act as the standard since it considers all methods in a

  #multidimensional aspect

  absoluteinmethod_matrix <- matrix(NA, nrow = length(absoluteoutliers), ncol = length(listofmethods))

  for (cvii in seq_along(listofmethods)) {

    lsts2 <- listofmethods[[cvii]]

    for (cviii in seq_along(absoluteoutliers)) {

      absoluteout <- absoluteoutliers[cviii]

      absoluteinmethod_matrix[cviii, cvii] <- ifelse(absoluteout%in%lsts2, 1, 0)

    }
  }
  #Obtain absolute outlier for each method

  maximumoutlier_method <- c() #maximum outlier

  for (cvix in 1:ncol(absoluteinmethod_matrix)) maximumoutlier_method[cvix] <- sum(absoluteinmethod_matrix[, cvix])

  #total outliers for each method

  uniquetotalemethod <- c()

  for (cvx in seq_along(listofmethods))  uniquetotalemethod[cvx] <- length(unique(listofmethods[[cvx]]))


  proportion_absoluteness <- c() #proportion of absolute in each method: maximum/total

  for (cvxi in seq_along(maximumoutlier_method)) {

    maxout <- maximumoutlier_method[cvxi]

    totalout <- uniquetotalemethod[cvxi]

    proportion_absoluteness[cvxi] <- maxout/totalout

  }
  #Select best method with maximum absolute outliers
  indx <- which(maximumoutlier_method==max(maximumoutlier_method) & proportion_absoluteness == max(proportion_absoluteness))

  if(length(indx)>1){

    indx <- indx[1] #choose any

  }else if(length(indx)==0){
    indx <- which(proportion_absoluteness == max(proportion_absoluteness))

    if(length(indx)>1) indx <- indx[1]

  }else{
    'Silence'
  }

  getmethodnames <- names(listofmethods)

  bstmethod <- getmethodnames[indx]

  if(absolute==TRUE){

    if(props==FALSE){

      if(autothreshold ==TRUE) return(list(threshold, absoluteoutliers)) else return(absoluteoutliers)

    }else if(props==TRUE){

      propdata <- cbind(absoluteoutliers, absolute_propn)

      return(as.data.frame(propdata))

    }else{
      stop('Choose either TRUE or FASLE to return abolute outlier or the percentage abolutness')
    }

  }else if(absolute==FALSE){

    return(bstmethod)

  }else{
    stop('Choose either TRUE or FALSE for absolute parameter to return outliers or bestmethod. Defualt is FALSE')
  }
}

#' @title Identifies absolute outliers and their proportions for a single species.
#'
#' @param x \code{datacleaner} class for each methods used to identify outliers
#'   in \code{multidetect} function.
#' @param sp \code{string}. Species name or index if multiple species are
#'   considered during outlier detection.
#' @param threshold \code{numeric}. Maximum value to denote an absolute outlier.
#'   The threshold ranges from \code{0}, which indicates a point has not been
#'   flagged by any outlier detection method as an \code{outlier}, to \code{1},
#'   which means the record is an absolute or true outlier since all methods
#'   have identified it. At both extremes, many records are classified at low
#'   threshold values, which may be due to individual method weakness or
#'   strength and data distribution. Also, at higher threshold values, the true
#'   outliers are retained. For example, if ten methods are considered and 9
#'   methods flag a record as an outlier, If a cutoff of 1 is used, then that
#'   particular record is retained. Therefore, the \code{default} cutoff is
#'   \code{0.6}, but \code{autothreshold} can be used to select the appropriate
#'   threshold.
#' @param autothreshold \code{vector}. Identifies the threshold with mean number
#'   of absolute outliers.The search is limited within 0.51 to 1 since
#'   thresholds less than are deemed inappropriate for identifying absolute
#'   outliers. The autothreshold is used when \code{threshold} is set to
#'   \code{NULL}.
#' @param absolute \code{logical}. To output absolute outliers for a species.
#' @param props \code{dataframe}. To output the proportional absoluteness for
#'   each outlier.
#' @param warn \code{logical}. If \strong{TRUE}, warning on whether absolute
#'   outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#'
#'
#' @return \code{vector} or \code{dataframe} of absolute outliers, best outlier detection method or data frame of absolute outliers and their
#' proportions
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                       lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = "scientificName",
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#basin removed
#'
#'  #outlier detection
#'
#'outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                           exclude = c('x','y'), multiple = TRUE,
#'                           methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"))
#'
#' ociss <- ocindex(x = outliersdf, sp= 1, threshold = 0.2, absolute = TRUE)#
#' #No outliers detected in more than two methods
#'
#'}
#'
ocindex <- function(x, sp = NULL, threshold = NULL, absolute=FALSE, props=FALSE, warn = FALSE, autothreshold=FALSE){

  if(!is(x, 'datacleaner')) stop('Datacleaner class accepted')

  if(x@out!='outlier') stop('Only extracts outliers yet clean data has been produced.')

  if(is.null(threshold) && autothreshold == FALSE) stop("Either set the threshold value or change autothreshold parameter to TRUE.")

  if(!is.null(threshold) && isTRUE(autothreshold)) stop("Either set threshold value (from 0 to 1) or autothreshold to TRUE but not both.")

  if(!is.null(threshold)) if(threshold>1 | threshold<0) stop('threshold must range from 0 to 1.')

  #Get the variable used in outlier detection

  var <- x@varused

  if(length(var)>1) {

  if(is.numeric(sp)) {

    if(sp > length(var))stop("The index number provided for sp is out of bounds, the index number sholud not exceed ", length(x@result), ".", call. = FALSE)

    var <- var[sp]

  }else{
    if(length(sp)>1 || !is.atomic(sp) || !is.character(sp))stop("Indicate only one variable and must character string selected from ", paste(var, collapse = ','))

    if(isTRUE(sp%in%var)) var <- sp else stop("The ", sp, " is not among the variables used during outlier detection, select from ", paste(var, collapse = ','))
  }
}else {
    var
  }

  #For multiple species

  if(isTRUE(x@mode) == TRUE && !is.null(sp)){

    species <- x@result[[sp]]

  }else if(isTRUE(x@mode) == FALSE && is.null(sp)){

    species <- x@result

  }else{
    stop('If they are  multiple species, indicate a particular species name or index')
  }

  #first check for null values for methods that were not successful

  checknull <- sapply(species, nrow)

  #remove methods that didn't execute

  speciesNULL <- species[!sapply(checknull,is.null)]

  #check if any method returned no outliers but will be retained while computing absolute outliers.
  len <- sapply(speciesNULL, nrow)

  if(any(len==0)) y <- speciesNULL[len !=0] else y <-  speciesNULL #replace the list with empty data with y

  if(length(y)<2) stop('The methods with outliers are less than 2.')

  lstvec <- list()

  for (ciii in names(y)) {

    lstvec[[ciii]] <- sort(y[[ciii]][[var]])
  }

  #create a vector of all outliers from all methods

  all_outliers <- unique(do.call(c, lstvec))

  absoluteoutlier_matrix <- matrix(NA, nrow = length(all_outliers), ncol = length(lstvec))

  for (civ in seq_along(lstvec)) {

    lsts <- lstvec[[civ]]

    for (cv in seq_along(all_outliers)) {

      numbs <- all_outliers[cv]

      absoluteoutlier_matrix[cv, civ] <- ifelse(numbs%in%lsts, 1, 0)
    }
  }
  #compute the outlier percentage presence across the methods
  outlier_propn <- c()

  outlier_value <- c()
  #loop through the matrix with 1 for presence and 0 for absent in a particular method

  for (cvi in 1:nrow(absoluteoutlier_matrix)) {

    #instead of using only the methods with outliers, outliers are weighted across all methods

    #therefore the denominator is length(x) where x is the count of methods considered

    outlier_propn[cvi] <- sum(absoluteoutlier_matrix[cvi,])/length(speciesNULL)

    outlier_value[cvi] <- all_outliers[cvi] # identifies the real outlier value
  }


  if(autothreshold == TRUE){

    #search from >= 0.5 to 1, which threshold had the highest mean absolute outliers.

    thresholdval <- c(0.51, 0.6, 0.7, 0.8, 0.9, 1)

    numbofabs <- sapply(X = thresholdval, function(x) length(which(outlier_propn>= x)))

    #assuming the indices are more than 2

    checkZeros <- sapply(numbofabs, function(x) ifelse(x==0, 0, 1)) #If the value is 0 == YES  else No

    indzero <- which(checkZeros==0)

    if(all(checkZeros==0)==TRUE){

      stop("No absolute outliers for thresholds ranging from 0.51 to 1", call. = FALSE)

    }else if(length(indzero)>=4) { #no need to use mean

      bestthreshold <- thresholdval[1] #select the 0.51 because probably returned absolute outliers/lowest cutoff

    }else{
      diffmean <- sapply(numbofabs, function(x) abs(x-mean(numbofabs)))

      indx <- which(diffmean== min(diffmean))

      if(length(indx)>1) indx2 <- indx[1] else indx2 <- indx

      bestthreshold = thresholdval[indx2]
    }
    ido <- which(outlier_propn>= bestthreshold) #filter out only indices of absolute outliers at a best threshold

    absolute_outliers <- outlier_value[ido] #absolute outlier real values

    absolute_propn <- outlier_propn[ido]

    out <- oci(absoluteoutliers = absolute_outliers, threshold = bestthreshold, absolute_propn = absolute_propn,
               listofmethods = lstvec, absolute = absolute, props= props, autothreshold = autothreshold)
  }else{

    ido <- which(outlier_propn>= threshold) #filter out only indices of absolute outliers at a certain threshold

    absolute_outliers <- outlier_value[ido] #absolute outlier real values

    absolute_propn <- outlier_propn[ido]

    if(length(absolute_outliers)>=1 && threshold>=0.5){

      out <- oci(absoluteoutliers = absolute_outliers, threshold = threshold, absolute_propn = absolute_propn,
                 listofmethods = lstvec, absolute = absolute, props= props, autothreshold = autothreshold)

    }else if(length(absolute_outliers)>=1 && threshold<0.5){

      if(isTRUE(warn))warning('The absolute outliers found are suspicious since they are shared by less than 50% of the methods used.')

      out <- oci(absoluteoutliers = absolute_outliers, threshold = threshold, absolute_propn = absolute_propn,
                 listofmethods = lstvec, absolute = absolute, props= props, autothreshold = autothreshold)

    }else{
      stop('No absolute outliers found with a threshold of ', threshold, '. Reduce and try again or continue with the reference dataset.')
    }
  }
}


#' @title  Identifies absolute outliers for multiple species.
#'
#' @inheritParams ocindex
#'
#' @return vector or absolute outliers, best outlier detection method or data frame of absolute outliers and their
#' proportions
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                       lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = "scientificName",
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#basin removed
#'
#'  #outlier detection
#'outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                           exclude = c('x','y'), multiple = TRUE,
#'                           methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"))
#'
#' totabs_counts <- multiabsolute(x = outliersdf, threshold = 0.2)
#'
#' }
#'
#' @seealso \code{\link{ocindex}}
#'

multiabsolute <- function(x, threshold = NULL, props = FALSE, warn = FALSE, autothreshold = FALSE){

  if(!is(x, 'datacleaner')) stop('Only datacleaner class accepted')

  if(x@out!='outlier') stop('Only extracts outliers yet clean data has been produced.')

  if(is.null(threshold) && autothreshold == FALSE) stop("Either set the threshold value or change `autothreshold` parameter to TRUE.")

  if(!is.null(threshold) && isTRUE(autothreshold)) stop("Either set threshold value (from 0 to 1) or autothreshold to TRUE but not both.")

  if(!is.null(threshold)) if(threshold>1 | threshold<0) stop('threshold must range from 0 to 1.')

  if(x@mode==FALSE)stop("Instead use ocindex when single dataset or parameters considered.")

  absoluteoutliers <- c() #Absolute outliers count

  species <- c()

  #to extract the number of absolute outlier flagged by each method at a particular threshold

  if(isFALSE(props)){

    for (di in 1:length(x@result)) {

      absx <- tryCatch(
        expr = {
          if(autothreshold == TRUE){
            abscount <- ocindex(x= x, sp = di, absolute = TRUE, threshold = threshold, props = FALSE, warn=warn, autothreshold = TRUE)
          }else{

            abscount <- ocindex(x= x, sp = di, absolute = TRUE, threshold = threshold, props = FALSE, warn=warn, autothreshold = FALSE )

          }
        },
        error= function(e){

          if(isTRUE(warn))warning('No absolute outliers exist for species ', names(x@result)[di], '.')

          return(NULL)
        })

      if(length(absx) >0){

        absoluteoutliers[di] <- if(autothreshold ==TRUE) length(abscount[[2]]) else length(abscount)

      }else{
        absoluteoutliers[di] <- 0
      }
      species[di]<- names(x@result)[di]
    }

    dx <- data.frame(groups = species, absoutliers = absoluteoutliers)

    #to extract a dataframe of absolute outlier proportions for all the methods at a particular threshold
  }else if(isTRUE(props)){

    splist <- list()

    for (di in 1:length(x@result)) {

      absx <- tryCatch(
        expr = {

          absprops <- ocindex(x= x, sp = di, absolute = TRUE, threshold = threshold,
                              props = TRUE, warn=warn, autothreshold = autothreshold)

        },
        error= function(e){
          if(isTRUE(warn))warning('No absolute outliers exist for species ', names(x@result)[di], '.', call. = FALSE)
          return(NULL)
        })
      if(length(absx)>0 ){
        splist[[di]] <- absprops
        splist[[di]]$threshold <- threshold
        splist[[di]]$groups <- names(x@result)[di]
        splist[[di]]$numabs <- nrow(absprops)
      }else{
        splist[[di]] <- NULL
      }
    }
    lext <- sapply(splist, length)

    if(any(lext==0)) extfinal <- splist[lext !=0] else extfinal<- splist

    dx <- do.call(rbind, extfinal)

  }else{
    stop('Choose either FALSE or TRUE for props to extract proportion or absolute outliers')
  }
  return(dx)
}


# Presence absence dissimilarity methods

#' @title Identifies the best outlier detection method using Jaccard coefficient.
#'
#' @inheritParams ocindex
#'
#' @return \code{string} best method for identifying outliers.
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                     lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = "scientificName",
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#basin removed
#'
#'  #outlier detection
#'
#' outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                          exclude = c('x','y'), multiple = TRUE,
#'                          methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"))
#'
#' jaccardout <- jaccard(x = outliersdf, sp= 1, threshold = 0.2)#
#'
#'}
#'
jaccard <- function(x, sp = NULL, threshold = NULL, warn=FALSE, autothreshold=FALSE){


  #check if there are absolute outliers
  if(autothreshold==TRUE){

    absoutliers_list <- ocindex(x= x, sp = sp,  absolute = TRUE, threshold = threshold, warn = warn,
                                autothreshold = autothreshold)
    absoutliers <- absoutliers_list[[2]]

  }else{
    absoutliers <- ocindex(x= x, sp = sp, absolute = TRUE, threshold = threshold, warn = warn)
  }

  var <- x@varused
  if(length(var)>1) var <- sp else var

  if(x@mode == TRUE && !is.null(sp)){

    if(length(absoutliers)>=1)  species <- x@result[[sp]] else stop('No outliers')

  }else if(x@mode == FALSE && is.null(sp)){

    if(length(absoutliers)>=1) species <- x@result else stop('No outliers')

  }else{
    stop('If they are  multiple species, indicate a particular species name or index')
  }

  checkNA <- sapply(species, nrow)

  #remove methods that didnot execute
  speciesNULL <- species[!sapply(checkNA,is.null)]

  len <- sapply(speciesNULL, nrow) #check if any method returned no outliers

  if(any(len==0)) y <- speciesNULL[len !=0] else y <- speciesNULL #replace the list with empty data with y

  matj <- matrix(NA, nrow = length(y), ncol = length(y))

  for (cxii in seq_along(y)) {
    m1 <- y[[cxii]][[var]]

    for (cxiii in seq_along(y)) {

      m2 <- y[[cxiii]][[var]]

      intx <- intersect(m1, m2)

      unx <- union(m1, m2)

      matj[cxii, cxiii] <- (length(intx)/length(unx))*100

    }
  }
  #choose best method with highest mean Jaccard index value
  jdx <- c()

  for (bi in 1:nrow(matj)) {

    jadvalues <- matj[,bi]

    jdx[bi] <- mean(jadvalues)
  }
  indxj <- which(jdx==max(jdx))

  if(length(indxj)>1) indxj <- indxj[1]

  bestmethodj <- names(y)[indxj]

  return(bestmethodj)
}

#
#  @title Overlap coefficient (Szymkiewicz–Simpson coefficient)
#
#' Identifies best outlier detection method using Overlap coefficient.
#'
#' @inheritParams ocindex
#'
#' @return best method for identifying outliers.
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                     lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = "scientificName",
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#basin removed
#'
#'  #outlier detection
#'
#' outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                          exclude = c('x','y'), multiple = TRUE,
#'                          methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"))
#'
#' overlapout <- overlap(x = outliersdf, sp= 1, threshold = 0.2)#
#'}
#'
overlap <- function(x, sp = NULL, threshold = NULL, warn=FALSE, autothreshold = FALSE){


  if(isTRUE(autothreshold)){
    absoutliers_list <- ocindex(x= x, sp = sp, absolute = TRUE, threshold = threshold, warn = warn,
                                autothreshold = autothreshold)
    absoutliers <- absoutliers_list[[2]]
  }else{
    absoutliers <- ocindex(x= x, sp = sp,  absolute = TRUE, threshold = threshold, warn = warn)
  }

  var <- x@varused
  if(length(var)>1) var <- sp else var

  if(x@mode == TRUE && !is.null(sp)){

    if(length(absoutliers)>=1)  species <- x@result[[sp]] else stop('No outliers')

  }else if(x@mode == FALSE && is.null(sp)){

    if(length(absoutliers)>=1) species <- x@result else stop('No outliers')

  }else{
    stop('If they are  multiple species, indicate a particular species name or index')
  }
  checkNA <- sapply(species, nrow)

  #remove methods that didnot execute
  speciesNULL <- species[!sapply(checkNA,is.null)]

  len <- sapply(speciesNULL, nrow) #check if any method returned no outliers

  if(any(len==0)) y <- speciesNULL[len !=0] else y <- speciesNULL #replace the list with empty data with y

  matover <- matrix(NA, nrow = length(y), ncol = length(y))

  for (cxiv in seq_along(y)) {

    m3 <- y[[cxiv]][[var]]

    for (cxv in seq_along(y)) {

      m4 <- y[[cxv]][[var]]

      intx1 <- intersect(m3, m4)

      len1 <- length(unique(m3))

      len2 <- length(unique(m4))

      matover[cxiv, cxv] <- (length(intx1)/min(len1, len2))*100
    }
  }
  jdo <- c()

  for (bii in 1:nrow(matover)) {

    ovalues <- matover[,bii]

    jdo[bii] <- mean(ovalues)
  }
  indxo <- which(jdo==max(jdo))

  if(length(indxo)>1) indxo <- indxo[1]

  bestmethodo <- names(y)[indxo]

  return(bestmethodo)
}


#' @title Cosine similarity index based on (Gautam & Kulkarni 2014; Joy & Renumol 2020)
#
#' @inheritParams ocindex
#'
#' @return best method for identifying outliers.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                     lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = "scientificName",
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#basin removed
#'
#'  #outlier detection
#' outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                          exclude = c('x','y'), multiple = TRUE,
#'                          methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"))
#'
#' consineout <- cosine(x = outliersdf, sp= 1, threshold = 0.2)#
#' }
#'
cosine <- function(x, sp = NULL,threshold = NULL, warn=FALSE, autothreshold = FALSE){

  #check if there are absolute outliers
  if(isTRUE(autothreshold)){
    absoutliers_list <- ocindex(x= x, sp = sp, absolute = TRUE, threshold = threshold, warn = warn,
                                autothreshold = autothreshold)
    absoutliers <- absoutliers_list[[2]]
  }else{
    absoutliers <- ocindex(x= x, sp = sp, absolute = TRUE, threshold = threshold, warn = warn)
  }

  var <- x@varused
  if(length(var)>1) var <- sp else var


  if(x@mode == TRUE && !is.null(sp)){

    if(length(absoutliers)>=1)  species <- x@result[[sp]] else stop('No outliers')

  }else if(x@mode == FALSE && is.null(sp)){

    if(length(absoutliers)>=1) species <- x@result else stop('No outliers')

  }else{
    stop('If they are  multiple species, indicate a particular species name or index')
  }
  checkNA <- sapply(species, nrow)

  #remove methods that didnot execute
  speciesNULL <- species[!sapply(checkNA,is.null)]

  len <- sapply(speciesNULL, nrow) #check if any method returned no outliers

  if(any(len==0)) y <- speciesNULL[len !=0] else y <- speciesNULL #replace the list with empty data with y

  matcos <- matrix(NA, nrow = length(y), ncol = length(y))

  for (cxl in seq_along(y)) {

    m10 <- y[[cxl]][[var]]

    for (cxli in seq_along(y)) {

      m11 <- y[[cxli]][[var]]

      intx12 <- intersect(m10, m11)

      len11 <- sqrt(length(unique(m10)))

      len12 <- sqrt(length(unique(m11)))

      matcos[cxl, cxli] <- (length(intx12)/(len11*len12))*100
    }
  }
  jdcos <- c()

  for (bix in 1:nrow(matcos)) {

    cosv <- matcos[,bix]

    jdcos[bix] <- mean(cosv)
  }
  indxcos <- which(jdcos==max(jdcos))

  if(length(indxcos)>1) indxcos <- indxcos[1]

  bestmethodcos <- names(y)[indxcos]

  return(bestmethodcos)
}

#' @title Identifies best outlier detection method suing Sorensen Similarity Index.
#'
#' @inheritParams ocindex
#'
#' @return best method for identifying outliers.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                       lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = "scientificName",
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#basin removed
#'
#'  #outlier detection
#'
#' outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                          exclude = c('x','y'), multiple = TRUE,
#'                          methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"))
#'
#' sordata <- sorensen(x = outliersdf, sp= 1, threshold = 0.2)#
#' }
#'
#'
sorensen <- function(x, sp = NULL,  threshold=NULL, warn=FALSE, autothreshold = FALSE ){


  if(isTRUE(autothreshold)){
    absoutliers_list <- ocindex(x= x, sp = sp,  absolute = TRUE, threshold = threshold, warn = warn,
                                autothreshold = autothreshold)
    absoutliers <- absoutliers_list[[2]]
  }else{
    absoutliers <- ocindex(x= x, sp = sp,  absolute = TRUE, threshold = threshold, warn = warn)
  }

  var <- x@varused
  if(length(var)>1) var <- sp else var

  if(x@mode == TRUE && !is.null(sp)){

    if(length(absoutliers)>=1)  species <- x@result[[sp]] else stop('No outliers')

  }else if(x@mode == FALSE && is.null(sp)){

    if(length(absoutliers)>=1) species <- x@result else stop('No outliers')

  }else{
    stop('If they are  multiple species, indicate a particular species name or index')
  }

  checkNA <- sapply(species, nrow)

  #remove methods that didnot execute
  speciesNULL <- species[!sapply(checkNA,is.null)]

  len <- sapply(speciesNULL, nrow) #check if any method returned no outliers

  if(any(len==0)) y <- speciesNULL[len !=0] else y <- speciesNULL #replace the list with empty data with y

  matsor <- matrix(NA, nrow = length(y), ncol = length(y))

  for (cxvi in seq_along(y)) {

    m5 <- y[[cxvi]][[var]]

    for (cxvii in seq_along(y)) {

      m6 <- y[[cxvii]][[var]]

      intx2 <- intersect(m5, m6)

      len5 <- length(unique(m5))

      len6 <- length(unique(m6))

      matsor[cxvi, cxvii] <- ((2*length(intx2))/sum(len5, len6))*100
    }
  }
  jdsor <- c()

  for (biii in 1:nrow(matsor)) {

    sorval <- matsor[,biii]

    jdsor[biii] <- mean(sorval)
  }
  indxs <- which(jdsor==max(jdsor))

  if(length(indxs)>1) indxs <- indxs[1]

  bestmethodsor <- names(y)[indxs]

  return(bestmethodsor)
}


#Simple matching coefficient

#' @title Identify best outlier detection method using simple matching coefficient.
#'
#' @inheritParams ocindex
#'
#' @return best method for identifying outliers based on simple matching coefficient.
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                       lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = "scientificName",
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#basin removed
#'
#'#outlier detection
#'
#' outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                          exclude = c('x','y'), multiple = TRUE,
#'                          methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"))
#'
#' smcout <- smc(x = outliersdf, sp= 1, threshold = 0.2)#
#' }
#'
smc <- function(x, sp=NULL,  threshold = NULL, warn = FALSE, autothreshold = FALSE){

  #check if there are absolute outliers
  if(isTRUE(autothreshold)){

    absoutliers_list <- ocindex(x= x, sp = sp, absolute = TRUE, threshold = threshold, warn = warn,
                                autothreshold = autothreshold)
    absoutliers <- absoutliers_list[[2]]
  }else{
    absoutliers <- ocindex(x= x, sp = sp,  absolute = TRUE, threshold = threshold, warn = warn)
  }

  var <- x@varused
  if(length(var)>1) var <- sp else var

  if(x@mode == TRUE && !is.null(sp)){

    if(length(absoutliers)>=1)  species <- x@result[[sp]] else stop('No outliers')

  }else if(x@mode == FALSE && is.null(sp)){

    if(length(absoutliers)>=1) species <- x@result else stop('No outliers')

  }else{
    stop('If they are  multiple species, indicate a particular species name or index')
  }

  checkNA <- sapply(species, nrow)

  #remove methods that didnot execute
  speciesNULL <- species[!sapply(checkNA,is.null)]

  len <- sapply(speciesNULL, nrow) #check if any method returned no outliers

  if(any(len==0)) y <- speciesNULL[len !=0] else y <- speciesNULL #replace the list with empty data with y

  lstvec_sm <- list()

  for (cxx in names(y)) {

    lstvec_sm[[cxx]] <- sort(y[[cxx]][[var]])
  }

  #Compute the values that are absent in methods in a pairwise format

  #step 1. create a vector of all outliers from all methods
  all_outliers_sm <- unique(do.call(c, lstvec_sm))

  #step 2. Determine where each outlier is present or absent

  matsim <-matrix(NA, nrow = length(all_outliers_sm), ncol = length(lstvec_sm))

  for (cxxi in seq_along(lstvec_sm)) {

    lsts_sim <- lstvec_sm[[cxxi]]

    for (cxxii in seq_along(all_outliers_sm)) {

      simpm <- all_outliers_sm[cxxii]

      matsim[cxxii, cxxi] <- ifelse(simpm%in%lsts_sim, 1, 0)
    }
  }

  #step 3. compute the second matrix but identifying where each method has absences 0 and convert it to 1 for easy counting
  #use matrix columns not rows in step 2
  matrix00 <- matrix(NA, nrow = ncol(matsim), ncol = ncol(matsim))#initiate matrix
  matrix11 <- matrix(NA, nrow = ncol(matsim), ncol = ncol(matsim))#initiate matrix
  matrix01 <- matrix(NA, nrow = ncol(matsim), ncol = ncol(matsim))#initiate matrix
  matrix10 <- matrix(NA, nrow = ncol(matsim), ncol = ncol(matsim))#initiate matrix

  m00values <- c() #absences
  m11values <- c() #intersections
  m01values <- c() #only in A
  m10values <- c() #Only in B

  for (cxxiii in 1:ncol(matsim)) {

    rowss <- matsim[,cxxiii] #col1

    for (cxxiv in 1:ncol(matsim)) {

      colss <- matsim[,cxxiv] #vs col2

      for (cxxv in seq_along(rowss)) {

        outv <- rowss[cxxv] # each value in the outer column

        inv <- colss[cxxv] # each value in the outer column

        m00values[cxxv] <- ifelse(outv==0 && inv==0, 1, 0) #convert the absences to 1 for easy summation

        m11values[cxxv] <- ifelse(outv==1 && inv==1, 1, 0)

        m01values[cxxv] <- ifelse(outv==0 && inv==1, 1, 0)

        m10values[cxxv] <- ifelse(outv==1 && inv==0, 1, 0)
      }

      matrix00[cxxiii, cxxiv] <- sum(m00values)
      matrix11[cxxiii, cxxiv] <- sum(m11values)
      matrix01[cxxiii, cxxiv] <- sum(m01values)
      matrix10[cxxiii, cxxiv] <- sum(m10values)
    }
  }

  #step 5. compute the simple matching

  simfinal <- c()
  smc_v <- c()

  for (cxxiv in 1:ncol(matrix00)) {

    absencecol <- matrix00[,cxxiv]

    intscol <- matrix11[, cxxiv]

    metcolin <- matrix01[, cxxiv]

    metcolout <- matrix10[, cxxiv]


    for (cxxv in seq_along(absencecol)) {

      m00 <- absencecol[cxxv]
      m11 <- intscol[cxxv]
      m01 <- metcolin[cxxv]
      m10 <- metcolout[cxxv]

      smc_v[cxxv] <- (m00+m11)/(m00+m11+m01+m10)

    }

    simfinal[cxxiv] <- mean(smc_v)

  }

  indxsim <- which(simfinal==max(simfinal))

  if(length(indxsim)>1) indxsim <- indxsim[1]

  bestmethodsim <- names(y)[indxsim]

  return(bestmethodsim)
}


#' @title Identify best outlier detection method using Hamming distance.
#'
#' @inheritParams ocindex
#'
#' @return best method based on hamming distance
#' @export
#'
#' @examples
#'\donttest{
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                     lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = "scientificName",
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#basin removed
#'
#'  #outlier detection
#'
#' outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                          exclude = c('x','y'), multiple = TRUE,
#'                          methods = c('mixediqr', "iqr", "mahal", "iqr", "logboxplot"))
#'
#' hamout <- hamming(x = outliersdf, sp= 1, threshold = 0.2)#
#' }
#'
#'
hamming <- function(x, sp=NULL, threshold = NULL, warn = FALSE, autothreshold = FALSE){

  #check if there are absolute outliers
  if(isTRUE(autothreshold)){
    absoutliers_list <- ocindex(x= x, sp = sp, absolute = TRUE, threshold = threshold, warn = warn,
                                autothreshold = autothreshold)
    absoutliers <- absoutliers_list[[2]]
  }else{
    absoutliers <- ocindex(x= x, sp = sp, absolute = TRUE, threshold = threshold, warn = warn)
  }

  var <- x@varused
  if(length(var)>1) var <- sp else var

  if(x@mode == TRUE && !is.null(sp)){

    if(length(absoutliers)>=1)  species <- x@result[[sp]]

  }else if(x@mode == FALSE && is.null(sp)){

    if(length(absoutliers)>=1) species <- x@result

  }else{
    stop('If they are  multiple species, indicate a particular species name or index')
  }

  checkNA <- sapply(species, nrow)

  #remove methods that didn't execute
  speciesNULL <- species[!sapply(checkNA,is.null)]

  len <- sapply(speciesNULL, nrow) #check if any method returned no outliers

  if(any(len==0)) y <- speciesNULL[len !=0] else y <- speciesNULL #replace the list with empty data with y

  lstvec_ham <- list()

  for (l in names(y)) {

    lstvec_ham[[l]] <- sort(y[[l]][[var]])
  }

  #Compute the values that are absent in methods in a pairwise format

  #step 1. create a vector of all outliers from all methods
  all_outliers_ham <- unique(do.call(c, lstvec_ham))

  #step 2. Determine where each outlier is present or absent

  matham <-matrix(NA, nrow = length(all_outliers_ham), ncol = length(lstvec_ham))

  for (li in seq_along(lstvec_ham)) {

    lsts_ham <- lstvec_ham[[li]]
    for (lii in seq_along(all_outliers_ham)) {

      hampm <- all_outliers_ham[lii]

      matham[lii, li] <- ifelse(hampm%in%lsts_ham, 1, 0)
    }
  }

  matcompare <- matrix(NA, nrow = ncol(matham), ncol = ncol(matham))

  for (liii in 1:ncol(matham)) {

    m1 <- matham[,liii]

    for (liv in 1:ncol(matham)) {

      m2 <- matham[,liv]

      matcompare[liii, liv] <- Reduce(x=abs(m1-m2), f='+')/length(m1)

    }

  }

  #best method has the lowest ham difference
  hamfinal <- apply(matcompare, 2, mean)

  indxham <- which(hamfinal==min(hamfinal))

  if(length(indxham)>1) indxham <- indxham[1]

  bestmethodham <- names(y)[indxham]

  return(bestmethodham)
}

