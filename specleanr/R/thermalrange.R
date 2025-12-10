#' @title Collates minimum, maximum, and preferable temperatures from FishBase.
#'
#' @param x \code{dataframe} or \code{string}. species names or a dataframe of species to aid in retrieving temperature ranges from FishBase.
#' @param colsp \code{string}. if \strong{\code{x}} is a data frame, then the column species is required. Otherwise for list of species or vector, the \code{colsp} is NULL.
#' @param verbose \code{logical} To return implementation messages. Default \code{FALSE}.
#' @param pct \code{numeric}. Provide the perecentage similarity of the species name provided and the one in FishBase. The lower the \code{pct} value, the higher the chances
#'        of getting a wrong species in the standard databases (FishBase). The plausible pct value should be greater than \strong{0.9}.
#' @param sn \code{logical}. Either to output synonym or only accepted names. This parameter reduces duplication of species synonyms and old name etc. For more information
#'        see \href{https://www.fishbase.se/}{FishBase}.
#' @param ranges \code{fishbasedataframe}. A standard database for ecological ranges from FishBase. See \href{https://www.fishbase.se/}{FishBase} for more information.
#' @param synonym \code{fishbasedataframe}. A standard database for species synonym names from FishBase. See \href{https://www.fishbase.se/}{FishBase} for more information.
#'
#' @return Data table for minimum, maximum and preferable species temperatures from FishBase.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- thermal_ranges(x = "Salmo trutta")
#' }
#'
thermal_ranges <- function(x, colsp = NULL, verbose = FALSE, pct = 90, sn =FALSE,
                           synonym = fishbase(tables = 'synonym'),
                           ranges = fishbase(tables = 'ranges')){

  if(is(x, 'data.frame') && is.null(colsp)) {

    stop('Species column names is not provided', call. = FALSE)

  } else if(is(x, 'data.frame') && !is.null(colsp)){

    if(length((colnames(x)[colnames(x)==colsp]))<1){

      stop('Species column name ', colsp, ' is  not found in the ', deparse(substitute(x)), ' data provided')

    } else{

      spls <- as.list(unique(unlist(x[, colsp])))

      dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false
    }

  }else if(is(x, 'list')){

    spls <- unlist(x)

    dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false

  }else {

    dsplist <- check_names(data = x, verbose = verbose, pct = pct, sn = sn)
  }

  #remove repeated species names

  if(is(dsplist, 'vector')){

    unx <- unique(dsplist)
  }else{
    unx <- unique(unlist(dsplist$speciescheck))

  }
  if(all(is.na(unx))==FALSE){

  v <- unx[!is.na(unx)]#remove NA species from the vector data

  rc <- ranges

  sy <- synonym

  ld <- length(unx); lv <- length(v)

  if(ld<lv) message(ld-lv, ' dropped because species names are not in FishBase')

  tempmi <- c(); tempma <- c(); temppref <- c(); species <- c()

  for (ispt in seq_along(v)) {

    sp1 <- v[ispt]

    spc <- unlist(sy$synonym)%in%sp1

    if(any(spc==TRUE)){

      codesp <-  unlist(sy$SpecCode)[which( spc==TRUE)]


      #get the species temperature ranges

      tmnext <- unlist(rc$TempMin)[which(unlist(rc$SpecCode) %in% codesp)]

      tmaxext <- unlist(rc$TempMax)[which(unlist(rc$SpecCode) %in% codesp)]

      tprefext <- unlist(rc$TempPreferred)[which(unlist(rc$SpecCode) %in% codesp)]

      #check if there is no ranges

      if(all(is.na(tmnext))) {
        if(isTRUE(verbose)==TRUE) message('No minimum temperature for ', sp1, '.')
        tempminval = NA
      }else{
        tmpmnvalues <- tmnext[!is.na(tmnext)]

        if(length(tmpmnvalues)>1) tempminval <- min(tmpmnvalues) else tempminval = tmpmnvalues
      }

      if(all(is.na(tmaxext))) {
        if(isTRUE(verbose)==TRUE) message('No maximum temperature for ', sp1, '.')
        tmaxval = NA
      }else{
        tmaxvalues <- tmaxext[!is.na(tmaxext)]
        if(length(tmaxvalues)>1) tmaxval <- max(tmaxvalues) else tmaxval = tmaxvalues
      }
      if(all(is.na(tprefext))) {
        if(isTRUE(verbose)==TRUE) message('No maximum temperature for ', sp1, '.')
        tprefval = NA
      }else{
        tprefvalues <- tprefext[!is.na(tprefext)]
        if(length(tprefvalues)>1) tprefval <- max(tprefvalues) else tprefval = tprefvalues
      }
    }else{
      if(isTRUE(verbose)==TRUE) message('No minimum temperature for ', sp1, '.')
      tprefval = NA
    }

    tempma[ispt] <- tmaxval
    tempmi[ispt] <- tempminval
    temppref[ispt] <- tprefval
    species[ispt] <- sp1

    dftemp <- data.frame(species = species, tempmin= tempmi, tempmax= tempma, tempref = temppref)
  }
  }else{

    warning("The species name is not found in FishBase. If the species  name is not a fish species, remove checkfishbase from optpar parameter.")
  dftemp = data.frame(temp = NULL)
    }
  return(dftemp)
}
