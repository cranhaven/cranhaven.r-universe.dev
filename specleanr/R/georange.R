#' @title Checks for geographic ranges from FishBase
#'
#' @param data Dataframe or vector to retrieve  ranges from FishBase.
#' @param colsp Column with species names from the data set.
#' @param verbose TRUE and messages will show. Default FALSE:
#' @param pct The percentage similarity of species names during standardization from FishBase.
#' @param sn TRUE and synonyms will be generated and not accepted ones. Default is FALSE, where species accepted names will be produced.
#' @param warn FALSE, not to generate warnings and TRUE for warnings. Default is FALSE:
#' @param ranges A standard database for ecological ranges from FishBase. See \href{https://www.fishbase.se/}{FishBase} for more information.
#' @param synonym A standard database for species synonym names from FishBase. See \href{https://www.fishbase.se/}{FishBase} for more information.
#'
#' @return Dataframe with geographical corrected ranges for species from FishBase.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' gr <- geo_ranges(data= "Lates niloticus")
#'
#' }
#'
#'
geo_ranges <- function(data, colsp =NULL, verbose=FALSE, pct = 90,sn =FALSE, warn=FALSE,
                       synonym = fishbase(tables = 'synonym'),
                       ranges = fishbase(tables = 'ranges')){

  if(is(data, 'data.frame') && is.null(colsp)) {

    stop('Species column names is not provided', call. = FALSE)

  } else if(is(data, 'data.frame') && !is.null(colsp)){

    if(length((colnames(data)[colnames(data)==colsp]))<1){

      stop('Species column name ', colsp, ' is  not found in the ', deparse(substitute(data)), ' data provided')

    } else{

      spls <- as.list(unique(unlist(data[, colsp])))

      dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false
    }

  }else if(is(data, 'list')){

    spls <- unlist(data)

    dsplist <- check_names(data = spls, verbose = verbose, pct = pct, sn = sn)#merge is false

  }else {

    dsplist <- check_names(data = data, verbose = verbose, pct = pct, sn = sn)
  }

  #remove repeated species names

  if(is(dsplist, 'vector')){

    uqspeciesnames <- unique(dsplist)
  }else{
    uqspeciesnames <- unique(unlist(dsplist$speciescheck))

  }
  if(all(is.na(uqspeciesnames))==FALSE){

    speciesfinal <- uqspeciesnames[!is.na(uqspeciesnames)]#remove NA species from the vector data


    ranges_db <- ranges#dataset for ranges

    syndata <- synonym

    ld <- length(uqspeciesnames); lv <- length(speciesfinal)

    if(ld<lv) if(isTRUE(warn))warning(ld-lv, ' dropped because species names are not in FishBase', call. = FALSE)

    cordinates <- c("Northernmost", "Southermost","Westernmost", "Easternmost")

    cord <- sapply(speciesfinal, function(y){

      species  <- unlist(syndata$synonym)%in% y

      spcodes <-  unlist(syndata$SpecCode)[which( species==TRUE)]

      sapply(cordinates, function(x){

        coords <- x

        dir = switch(coords, Northernmost = "NorthSouthN",Southermost = "NorthSouthS",
                     Westernmost ="WestEastW", Easternmost = "WestEastE" )

        coordvalues<- unlist(ranges_db[,coords])[which(unlist(ranges_db$SpecCode) %in% spcodes)]

        if(length(coordvalues)>=1){

          cordvalues_no_na <- coordvalues[!is.na(coordvalues)]

          if(coords == "Southermost" | coords== "Westernmost"){
            cordvalues_final <- cordvalues_no_na[which.min(cordvalues_no_na)]
          }else{
            cordvalues_final <- cordvalues_no_na[which.max(cordvalues_no_na)]
          }
          direction_values<- unlist(ranges_db[,dir])[which(unlist(ranges_db$SpecCode) %in% spcodes)]

          dir_final <- direction_values[!is.na(direction_values)][which.max(cordvalues_no_na)]

          if(length(cordvalues_final) <1 & length(dir_final)<1){

            cordvalues_final = NA

          }else if(length(dir_final)<1){

            if(isTRUE(warn)) warning("The cordinates directions for ", y, " are not provided and cordinate coversation maybe erroneous.", call. = FALSE)

            cordvalues_final

          }else if(cordvalues_final==0 & is.na(dir_final)){

            cordvalues_final

          }else{

            if(dir_final =="S" | dir_final =="W" | dir_final =="s" | dir_final=='w') {
              if(cordvalues_final>0) cordvalues_final = cordvalues_final*-1 else cordvalues_final = cordvalues_final #some are already -ve
            } else{
              cordvalues_final
            }
          }

        }else{

          if(isTRUE(warn))warning("No latitidunal ranges found in FishBase and the original data will be outputed.", call. = FALSE)
        }

      }, simplify = TRUE)
    }, simplify = TRUE)

  }else{

    warning("The species name is not found in FishBase. If the species  name is not a fish species, remove checkfishbase from optpar parameter.")
    cord = NA
  }

  return(unlist(cord))
}

