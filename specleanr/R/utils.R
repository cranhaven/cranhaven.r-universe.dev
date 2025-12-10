
#' @noRd
#'
fishbase <- function(tables){

  check_packages(pkgs = c('rfishbase', 'curl'))

  if (!curl::has_internet()) stop('No internet connection, connect and try again later to access FishBase.')

  fb_sy <- suppressMessages(rfishbase::synonyms())

  if(nrow(fb_sy)<0) stop('The synonym table from rfishbase has not successfully loaded and names cannot be checked.')

  fb_ranges <- suppressMessages(rfishbase::stocks())

  if(nrow(fb_ranges)<0) stop('The stocks table from rfishbase has not successfully loaded and temperature/geogrpahical ranges and  cannot be determined.')

  fb_species <- suppressMessages(rfishbase::species())

  if(nrow(fb_species)<0)stop('The species table didnot load properly and species ecosystem cannot be extracted from fishbase.')

  switch(tables, synonym = return(fb_sy), ranges = return(fb_ranges), spnames = return(fb_species))
}


#' @noRd
clean_names <- function(sp){

  #convert all letters to lower
  tlw <- tolower(sp)
  #remove accents
  actr <- iconv(tlw, from = 'UTF-8', to = 'ASCII//TRANSLIT')

  sppt <- gsub("[[:punct:]]", "", actr)

  spc <- gsub("[^[:alnum:]]", " ", sppt)

  spaces <- trimws(gsub("\\s+"," " ,spc), which = 'both')

  str1 <- unlist(strsplit(spaces, " "))[1]

  strother <- paste0(unlist(strsplit(spaces, " "))[-1], collapse = ' ')

  spclean <- paste0(paste0(toupper(strtrim(str1, 1)), substring(str1, 2)),' ',strother)

  return(spclean)
}


#' @title Customized match function
#'
#' @param x The category with words to match
#' @param choices The different options or choices in a particular category that are allowed.
#' @param quiet Default \code{FALSE} not to return messages.
#'
#' @return choices
#' @export
#'
match.argc <- function(x, choices, quiet=TRUE){

  if(any(choices%in%x)==FALSE){
    stop("The value for ", deparse(substitute(x)), " is not allowed. Choose from ", paste0(choices, collapse = ', '))
  } else{
    if(isFALSE(quiet))message("The ", deparse(substitute(x)), " is not among the allowed choices ", paste0(choices, collapse = ', '))
  }
}


#' @title indicate excluded columns.
#'
#' @param x \code{dataframe} with columns to where the columns are supposed to be removed.
#' @param exclude \code{string} or \code{vector} column names  to be checked if it is in the data.
#' @param quiet TRUE if implementation messages to be shown. Default \code{FALSE}.
#'
#' @return columns that are not in the dataframe.
#'
check.exclude <- function(x, exclude, quiet=TRUE){

  indcols <- exclude%in%colnames(x)

  colsnotindf <- exclude[which(indcols==FALSE)]

  if(length(colsnotindf)>=1){

    stop("The column name/s: ", paste(colsnotindf, collapse = ', '), " to be excluded are/is not in the species-environment extracted data.")
  }else{

    if(isFALSE(quiet)) message("All indicated columns to be excluded are in the dataset.")
  }

}

#' @title get dataframe from the large dataframe.
#'
#' @param x Small dataset
#' @param y Large dataset for intersection
#' @param full Whether the whole column names are checked or not. Default \code{FALSE} where only the first column is considered.
#'      if FALSE; then the returned columns may be few or more if the considered column has less or more similar
#'      rows across the two data sets.
#'
#' @return Data to extracted from large dataset.
#'
#' @export
#'
#' @examples
#'
#' x = data.frame(id=c(1,2,3,4,5),  name=c('a','b','c', 'd','e'))
#'
#' y=data.frame(id=c(1,2,3,4,7,6,5), tens=c(10,29,37,46,58, 34, 44),
#'                  name=c('a','b','c','d','e', 'f','g'))
#'
getdiff <- function(x, y, full=FALSE){

  c1 <- colnames(x); c2 <- colnames(y)

  if(all(c1 %in% c2)==FALSE) stop("All column names are different.")

  if(identical(x, y))stop("x and y names are identical.")

  if(full==FALSE){

    #use one column name same across the two datasets
    getcol <- c1[which(c1%in%c2==TRUE)][1]

    out <- y[which(y[,getcol] %in% x[,getcol]),]

  }else{

    getcol <- c1[which(c1%in%c2==TRUE)]

    #loop through all same data sets names and extract same rows across the two data sets.

    xx <- sapply(getcol, function(cl){ y[which(y[,cl] %in% x[,cl]),] }, simplify = FALSE)

    rws <- sapply(xx, nrow)

    out <- xx[[which(rws==min(rws))]]
  }
  return(out)
}


#' Check for packages to install and respond to use
#'
#' @param pkgs list of packages to install
#'
#' @return error message for packages to install
#'
check_packages <- function(pkgs){

  pkginstall <- sapply(pkgs, requireNamespace, quietly = TRUE)

  pkgout <- pkgs[which(pkginstall==FALSE)]

  if(length(pkgout)>=1)stop('Please install ', length(pkgout), ' packages: ', paste(pkgout, collapse = ', '), ' to continue.', call. = FALSE)

  invisible(pkgs)
}


#' To check for a bounding box
#'
#' @param x raster, shapefile or list of bounding box values.
#' @param par indicate the database being queried to handing the issues of bounding box settings.
#'
#' @return extent values from raster, shapefile and bounding box
#'
extentvalues <- function(x, par= NULL){

  if(inherits(x, what = 'sf')){

    extout <- unname(sf::st_bbox(x))

  }  else if(inherits(x, "SpatRaster")){

    vc <- terra::ext(x)

    extout <- unname(c(vc[1], vc[3], vc[2], vc[4]))

  }else if(is(x, 'list')){

    v <- unlist(x)

    stdbox <- c("xmin", "ymin", "xmax", "ymax")

    if(setequal(stdbox, names(v))==FALSE) stop("the labels provided in the bounding are not standard. Please use xmin, xmax, ymin, ymax")

    extout <- as.vector(v)

    }else{

    stop('Either provide a raster layer or shapefile to extract the bounding box. If the extent is known, provide the xmin, xmax, ymin, ymax values in a list')
    }

  if(par=='inat') vf <- c(extout[2], extout[1], extout[4], extout[3]) else vf <- extout

  return(vf)
}


#' Implement principal component analysis for dimension reduction
#'
#' @param data Environmental dataframe
#' @param npc Number of principal components to be retained. Default is 2
#' @param q To show the cumulative total variance explained by the \code{npc} selected.
#'
pca <- function(data, npc, q){

  if(ncol(data)<=npc) stop('The number of columns or variabales are less than or equal to ', npc,' so either reduce the principal components needed or the data is not highly dimesional.',call. = FALSE)

  xout <- prcomp(data, center = TRUE, scale. = TRUE)
  sout <- xout$x

  if(isFALSE(q)) message('The cummulative proprotion for PCs ', npc, ' is ',summary(xout)$importance[3,npc])

  pc <- as.data.frame(sout[, 1:npc])

  return(list(pcs = pc, od =data))
}


#' To implement bootstrapping procedures. Sampling with replacement.
#'
#' @param data Environmental data
#' @param boots Number of bootstraps
#' @param seed Random seed to ensure reproduciblity
#' @param pca Whether bootstrapping is conducted on data after principal component analysis.
#'
boots <- function(data, boots, seed, pca){

  if(isTRUE(pca)){

    df <- data[[1]]

    dfo <- data[[2]]
  }else{
    df <- data
    dfo <- NULL
  }
  set.seed(seed = seed)
  boot <- seq(1, boots, 1)
  bout <- lapply(boot, function(bb){
    indx <- sample(nrow(df), size = nrow(df), replace = TRUE)
    pc <-  df[indx,]
    dfo <- dfo[indx,]
    attributes(pc)$OD <- dfo
    pc
  })
}

#' To package both principal component analysis and bootstrapping.
#'
#' @param pb the principal component or bootstrapped data
#' @param var The variable of concern, which is vital for univariate outlier detection methods
#' @param pc Whether principal component analysis will be computed. Default \code{FALSE}
#' @param boot Whether bootstrapping will be computed. Default \code{FALSE}
#' @param pcvar Principal component analysis to e used for outlier detection after PCA. Default \code{PC1}
#'
pcboot <- function(pb, var, pc, boot, pcvar){

  if(isTRUE(pc)){

    if(isTRUE(boot)){
      #attach the original data to enable forward retraction.
      varc <- (attributes(pb)$OD)[, var]

      var <- unlist(pb[, pcvar])

      data <- attributes(pb)$OD

      pcdf <- pb
    }else{

      varc <- pb[[2]][, var]
      var <- pb[[1]][, pcvar]
      data <- pb[[2]]
      pcdf <- pb[[1]] #principal component data
    }
  }else{

    var <- unlist(pb[, var])
    varc <- NULL
    pcdf <- NULL
    data <- pb

  }
  return(list(data = data, varc= varc, var = var, pcdf= pcdf))
}


#' @title Handle false flagging of records as outliers in bootstrap samples
#' @noRd
#'
bootopt <- function(x, var, nboots, th =0.6){

  if(all(sapply(x, function(xx)nrow(xx))==0)){

    out <- x[[1]] #pick one list since they all returned no data
  }else{

    xf <- Reduce(rbind, x)

    outdf <- xf[!duplicated(xf),]

    varc <- unlist(outdf[,var])

    values <- unique(varc)

    lv <- lapply(values, function(vv) length(which(varc == vv))/nboots)

    valin <- values[which(lv>=th)]

    btout <- which(varc%in%valin==TRUE)

    #get out data
    xout <- outdf[btout,]

    #retun non duplicates bootstraps

    out <- xout[!duplicated(xout[,var]),]

    #remove id column
    out$id <- NULL

    return(out)

  }
}

#' Post checks for PCA and bootstrapping
#'
#' @param y list of PCA and bootstrapped output.
#' @param nboots Number of bootstrapping
#' @param th threshold for identifying absolute outlier from bootstrapped samples.
#' @param var variable of interest.
#'
checks <- function(y, nboots, th, var){

  xx <- sapply(y, function(x) all(is.na(x)))

  xy <- y[!xx]

  if(length(xy)>=1) vv <- bootopt(x = xy, var = var, nboots = nboots, th = th) else vv <- NA

  return(vv)
}


#' Computes the empirical influence function for each values in the dataset
#'
#' @param x Outlier checked data
#' @param var variable of interest
#'
eif <- function(x, var){
  vvx <- unlist(x[,var])
  eout<- sapply(1:nrow(x), function(vx){length(vvx) * (mean(vvx) - mean((vvx[-vx])))})
  x[,'EIF'] <- eout
  return(x)
}
