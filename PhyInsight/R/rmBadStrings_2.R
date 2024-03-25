#' Remove Mismatched DNA Sequences
#'
#' Identify and remove bad DNA sequences within a string set. Sequentially removes mismatches until all sequences align.
#'
#' @param DNAStringSet A DNA string set object.
#' @param specimen_dataframe A dataframe with speciment data created using querySpecData().
#' @param rmOutliers A logical value to state whether to remove DNA distance outlier strings
#' @param max_Z_score A numerical value to change the max Z score when removing outliers.
#'
#' @return A list with two elements: the DNA string set with the mismatched sequences removed (1st element) and the specimen dataframe with data for the mismatched sequences removed (2nd element).
#' @export
#'
#' @examples # remove problem strings from a DNA string set
#'specdata <- querySpecData("Panthera leo")
#'
#'specdata <- subset(specdata, markercode == "COI-5P")
#'
#'DNABin_Leo <- genDNABin(specdata)
#'
#'DNAStringset_Leo <- genDNAStringSet(DNABin_Leo)
#'
#'DNAStringSet_Leo_manipulated <- ManipStringSet(DNAStringset_Leo)
#'
#'StringsAndSpecdataframe <- rmBadStrings_2(
#'
#'  DNAStringSet = DNAStringSet_Leo_manipulated,
#'  specimen_dataframe = specdata
#'
#')
#'
#'DNAStringSet_NEW <- StringsAndSpecdataframe[[1]]
#'
#'tail(DNAStringSet_NEW)
#'
#'specimen_dataframe_NEW <- StringsAndSpecdataframe[[2]]
#'
#'tail(specimen_dataframe_NEW$processid)
rmBadStrings_2 <- function(DNAStringSet, specimen_dataframe, rmOutliers = FALSE, max_Z_score = 3){

  ### function to generate symbol grid
  genSymbolGrid <- function(DNAStringSet){


    ###
    symbolGrid <- list()
    for(i in 1:length(DNAStringSet)){

      symbolGrid[i] <-                   # ORIGINAL
        strsplit(
          as.character(DNAStringSet[i]), "")

    }


    # store the split up nucleotides as a dataframe
    symbolGrid <- base::as.data.frame(symbolGrid)



    # change column names
    names(symbolGrid) <- gsub(" ", "", paste("seq",
                                             1:length(names(symbolGrid))
    ))


    # change row names   (each string)
    row.names(symbolGrid) <- gsub(" ", "", paste("p",
                                                 1:length(row.names(symbolGrid))
    ))



    #
    ## transpose
    #baseGrid <- t(baseGrid)
    #baseGrid <- base::as.data.frame(baseGrid)


    #
    return(symbolGrid)

  }


  ### use the function to create a new symbol grid
  symbGrid <- genSymbolGrid(DNAStringSet)


  ### function to detect whether the target string is mismatched with any other string it's aligned with
  is.stringMismatch <- function(SymbolGrid, SymbolGrid_stringNumber){


    # ID places where "-" are NOT located
    is_dash <- SymbolGrid[, SymbolGrid_stringNumber] != "-"      #
    #  is_dash <- SymbolGrid[, "seq41"] != "-"      # ORIGINAL
    is_dash


    # extract all strings from the set only from the base positions of the target seq
    # Target String Base Position (tsbp)
    tsbp <- SymbolGrid[is_dash, ]
    tsbp


    # return whether other strings in tsbp have no bases
    tsbp_is_nobases <- numeric()
    for(i in 1:length(tsbp[1,])){
      #for(i in 1:5){

      tsbp_is_nobases[i] <- sum((table(tsbp[, i]))["-"])

      tsbp_is_nobases[i] <- tsbp_is_nobases[i]  == length(tsbp[, i])

      tsbp_is_nobases <- as.logical(tsbp_is_nobases)

    }

    #
    return(TRUE %in% tsbp_is_nobases)

  }


  ### original stringset
  is_mismatched <- numeric()
  for(i in 1:length(symbGrid[1,])){

    is_mismatched[i] <- is.stringMismatch(SymbolGrid = symbGrid, SymbolGrid_stringNumber = i)
    is_mismatched <- as.logical(is_mismatched)

  }


  #### find number of bases in a string...
  #NBases <- nchar(specimen_dataframe$nucleotides)


  #### put the manipulated strings in order (smallest first)
  #DNAStringSet_order <- DNAStringSet[order(NBases)]


  ### put the manipulated strings in order (mismatched first)
  DNAStringSet_order <- DNAStringSet[order(is_mismatched, decreasing = TRUE)]


  ### while is.stringMismatch() sees TRUE remove the first string from the ordered string sets until it sees FALSE
  rmMismatch <- function(DNAStringSet) {

    while(length(DNAStringSet) > 0 && is.stringMismatch(genSymbolGrid(DNAStringSet), 1)) {
      DNAStringSet <- DNAStringSet[-1]
    }
    return(DNAStringSet)
  }


  ### use the function
  DNAStringSet_rmMismatch <- rmMismatch(DNAStringSet_order)


  ### need to order DNAStringSet_rmMismatch when finished while looping
  DNAStringSet_rmMismatch <- DNAStringSet_rmMismatch[order(as.numeric(names(DNAStringSet_rmMismatch)))]
  DNAStringSet <- DNAStringSet_rmMismatch # new addition 2-12-23


  ### create new specimen dataframe with the problem sequences removed
  specimen_dataframe_new <- specimen_dataframe[sort(as.numeric(names(DNAStringSet_rmMismatch))),]

  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##################################### filter out NaN strings (added 12-11-23)


  ### create DNA dist matrix
  DistMatrix <- as.matrix(ape::dist.dna(ape::as.DNAbin(DNAStringSet_rmMismatch)))


  ### ieshows 2 columns (rows/columns) which show location of each string causing NaN
  NaNlocs <- which(is.nan(DistMatrix), arr.ind = TRUE)
  NaNlocs <- table(NaNlocs)
  NaNlocs <- NaNlocs[NaNlocs > 2]
  NaNlocs <- names(NaNlocs)


  ###
  if(is.character(NaNlocs) == TRUE) {

    #DNAStringSet_rmMismatch <- DNAStringSet_rmMismatch[-as.numeric(NaNlocs)]
    #DNAStringSet <- DNAStringSet_rmMismatch
    DNAStringSet <- DNAStringSet_rmMismatch[-as.numeric(NaNlocs)]
    #DNAStringSet <- DNAStringSet[-as.numeric(NaNlocs)]
    specimen_dataframe_new <- specimen_dataframe_new[-as.numeric(NaNlocs), ]

  }


  ###
  #DNAStringSet <- DNAStringSet[-c(as.numeric(NaNlocs)), ]    <<< original line


  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ######################## filter out outlier distance strings (added 14-11-23)


  ### function to calculate Z scores
  calculate_Zscore <- function(x){
    z <- (x - mean(x)) / stats::sd(x)

    z <- abs(z)

    z <- as.matrix(z)

    return(z)
  }


  ### use which() to find locations of outlier dna distances
  outLocs <- which(
    calculate_Zscore(DistMatrix) > max_Z_score,
    arr.ind = TRUE
  )



  ###
  outLocs <- table(outLocs)
  outLocs <- outLocs[outLocs > 3]
  outLocs <- names(outLocs)




  ###
  #  #if (is.null(outLocs) == FALSE) {
  #  if (!is.null(outLocs)) {
  #    DNAStringSet <- DNAStringSet[-as.numeric(outLocs)]
  #
  #    DF_NEW   <- specimen_dataframe
  #    DF_NEW <- DF_NEW[row.names(DF_NEW) %in% names(DNAStringSet), ]
  #
  #    specimen_dataframe_new <- DF_NEW
  #
  #  }

  if(rmOutliers == TRUE){

    while (!is.null(outLocs)) {

      DNAStringSet <- DNAStringSet[-as.numeric(outLocs), ]
      specimen_dataframe_new <- specimen_dataframe_new[-as.numeric(outLocs), ]

      #   following lines removed 26-11-2023
      #      DF_NEW <- specimen_dataframe_new
      #      DF_NEW <- DF_NEW[row.names(DF_NEW) %in% names(DNAStringSet), ]
      #      specimen_dataframe_new <- DF_NEW


      print(paste("Outlier strings detected and removed: ", outLocs))


      ###
      DistMatrix <- as.matrix(ape::dist.dna(ape::as.DNAbin(DNAStringSet)))
      ### use which() to find locations of outlier dna distances
      outLocs <- which(
        calculate_Zscore(DistMatrix) > max_Z_score,
        arr.ind = TRUE
      )
      ###
      outLocs <- table(outLocs)
      outLocs <- outLocs[outLocs > 3]
      outLocs <- names(outLocs)
      outLocs
    }
  }

  ##############################################################################
  ##############################################################################

  ###
  #  return(list(DNAStringSet_rmMismatch, specimen_dataframe_new))  # original
  return(list(DNAStringSet, specimen_dataframe_new))

}
