#' @import utils
## quiets concerns of R CMD check re: the .'s that appear in pipelines
#if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",">"))
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",":=",".x",">"))



#' ReadData
#'
#' This function read the one file, select interesting columns and rename them.
#' @param ListN The name of the list of files where the file to read belongs or NULL if there is only one list of files
#' @param FileN The name of the file to read
#' @param VarN A named list containing the column names in the file corresponding to the variables below: MARKER, CHR, POS, EFFECT, PVAL. (optional: FREQ, ALLELE0, ALLELE1)
#' @param MinFreq A numeric value allowing to filter to keep markers with MAF > MinFreq
#' @return  A tibble with the interesting columns selected and renamed.
#' @import stringr dplyr
#' @importFrom data.table fread
#' @importFrom tibble as_tibble



ReadData <- function(ListN, FileN, VarN, MinFreq=0){

  ## Declare variable names
  if(is.null(ListN)){
    Name <- names(FileN)
  } else {
    Name <- stringr::str_c(ListN, "_", names(FileN))
  }
  VarsToSelect <- c(VarN$CHR, VarN$POS, VarN$MARKER, VarN$FREQ, VarN$EFFECT, VarN$PVAL)
  ## Is a FREQ variable provided ?
  if('FREQ'%in%names(VarN)){
    names(VarsToSelect) <- c("CHR", "POS", "MARKER",
                             str_c(c("FREQ.", "EFFECT.","PVAL."), Name))
  } else {
    names(VarsToSelect) <- c("CHR", "POS", "MARKER",
                             str_c(c("EFFECT.","PVAL."), Name))
  }
  ## Add allele encoding if present
  if(!is_empty(VarN$ALLELE0) & !is_empty(VarN$ALLELE1)){
    Alleles <- c(VarN$ALLELE0, VarN$ALLELE1)
    names(Alleles) <- c("ALLELE0","ALLELE1")
    VarsToSelect <- c(VarsToSelect,Alleles)
  }

  ### Add Weight if present
  if(!is_empty(VarN$WEIGHT)){
    Weight <- VarN$WEIGHT
    names(Weight) <- str_c("WEIGHT." , Name)
    VarsToSelect <- c(VarsToSelect,Weight)
  }

  ### Add Effect_Se if present
  if(!is_empty(VarN$EFFECT_SE)){
    Effect_se <- VarN$EFFECT_SE
    names(Effect_se) <- str_c("EFFECT_SE." , Name)
    VarsToSelect <- c(VarsToSelect,Effect_se)
  }

  ## Read the dataset, select interesting columns and rename them
  dt_temp <- data.table::fread(FileN) %>% as_tibble() %>%
    dplyr::select(!!VarsToSelect)

  ## If requested, filter markers based on MAF
  if(MinFreq>0){
    Maf.ok <- (dt_temp[[str_c('FREQ.',Name)]] >= MinFreq)&(dt_temp[[str_c('FREQ.',Name)]] <= 1-MinFreq)
    dt_temp <- dt_temp[Maf.ok,]
  }
  return(dt_temp)
}

#' Collect the results of GWAS data from different files
#'
#' @description This function merges files containing the summary statistics of GWAS in different environments (one file per environment).
#' @details Each file MUST contain the variables below:
#' \itemize{
#' \item MARKER: the marker name
#' \item CHR: the chromosome
#' \item POS: the position of the marker
#' \item EFFECT: the mean effect of the marker
#' \item PVAL: the pvalue
#' }
#' 
#' Each file might contain the variables:
#' \itemize{
#' \item FREQ: MAF
#' \item ALLELE0: Allele coding for allele 0
#' \item ALLELE1: Allele coding for allele 1
#' }
#' 
#' 
#' @param FileNames A list containing the file paths to merge (one trait only) or a list of such lists
#' @param VariableNames A named list containing the column names in the original files corresponding 
#' to the variables : MARKER, CHR, POS, EFFECT, PVAL (optional: FREQ, ALLELE0, ALLELE1) ; or a list of such lists.
#' @param MinFreq A numeric value allowing to filter markers based on the maf. (optional)
#' @param DropDuplicates A boolean indicating whether duplicate markers should be removed or not. (\code{TRUE} by default)
#' @param Verbose A boolean indicating whether progression messages should be printed or not. (\code{FALSE} by default)
#' @param NA.rmv  A boolean indicating if the \code{NA} should be removed or not (\code{TRUE} by default)
#' @return A list with the following elements:
#'\tabular{ll}{
#' \code{Data} \tab A tibble containing all the columns of interest of all the files from FileNames.\cr
#' \code{RemovedMarkers} \tab Same kind of tibble, but containing the markers that have been removed 
#' due to unclear allele coding, maf filtering or duplicates dropping.
#' }
#' 


#' @examples
#' require(dplyr)
#' require(tibble)
#' require(stringr)
#' RepData <- system.file("extdata", package = "metaGE")
#' # Get the complete list of association files
#' File.list <- list.files(RepData ,full.names = TRUE) %>%
#'             tibble(Names = .) %>%
#'             mutate(ShortNames = Names %>%
#'                   str_remove(pattern = paste0(RepData,"/")) %>%
#'                   str_remove(pattern = "_DF.txt"))  %>%
#'             select(ShortNames,Names) %>%
#'             deframe
#' ###Build the dataset
#' ## First provide the list of variable names
#' Names.list <- list(MARKER="Marker_Name",
#'                   CHR="Chromosome",
#'                   POS="Marker_Position",
#'                   FREQ="Maf",
#'                   EFFECT="SNP_Weight",
#'                   PVAL="Pvalue",
#'                   ALLELE0="Allele1",
#'                   ALLELE1="Allele2")
#'
#' MinFreq <- 0.07
#'
#' ## Now collect
#' metaData <- metaGE.collect(File.list, Names.list,MinFreq = MinFreq)

#' @export
#' @import purrr dplyr stringr tibble
#' @importFrom data.table fread
#'


metaGE.collect <- function(FileNames, VariableNames, MinFreq = 0, DropDuplicates=TRUE, Verbose = FALSE,NA.rmv=TRUE){

  ### Checkings

  ## Checks on FileNames
  if(!is.list(FileNames) & !is.character(FileNames)){
    stop('FileNames should be a vector of file names or a list of such vectors.')
  }
  if(is.list(FileNames)){
    if(purrr::map(FileNames,class) %>% `!=`('character') %>% any){
      stop('Each element of FileNames should be a vector of file names.')
    }
  }
  if(is.list(FileNames)&(length(FileNames)>1)&(is.null(names(FileNames)))){
    names(FileNames) <- paste0('ListEnv',1:length(FileNames))
  }

  ## Checks on VariableNames
  if(!is.list(VariableNames) & !is.character(VariableNames)){
    stop('VariableNames should be a list of variable names or a list of such lists.')
  }

  ## Cross checks on FileNames and VariableNames
  if(is.list(FileNames)){
    if(!is.list(VariableNames[[1]])){
      message('Assuming VariableNames works for all items in  FileNames...')
      VariableNames <- rep(VariableNames,length(FileNames))
    }
  }

  ## Checks on MinFreq
  if(MinFreq>0){
    if(is.character(VariableNames)){
      IsFreqProvided <- 'FREQ'%in%names(VariableNames)
    } else if(is.character(VariableNames[[1]])){
      IsFreqProvided <- 'FREQ'%in%names(VariableNames)
    } else {
      IsFreqProvided <- suppressWarnings(purrr::map(VariableNames, ~'FREQ'%in%names(.x)) %>% all)
    }
    if(!IsFreqProvided){
      stop('A FREQ variable is required to perform filtering on MAF...')
    }
  }

  ### Create the table with the first file as basis.
  if (!is.list(FileNames)){
    FileNames <- list(FileNames)
  }
  if (!is.list(VariableNames[[1]])){
    VariableNames <- list(VariableNames)
  }

  ## Initialize by creating a DF with CHR, POS and MARKER from the first file
    # If requested, filter markers based on MAF
  Data <- data.table::fread(FileNames[[1]][[1]]) %>%
    dplyr::rename(CHR = VariableNames[[1]]$CHR, POS = VariableNames[[1]]$POS, MARKER = VariableNames[[1]]$MARKER)
  if(MinFreq>0){
    Data <- Data %>% rename(FREQ = VariableNames[[1]]$FREQ)
    Data <- Data %>% filter(.data$FREQ > MinFreq & .data$FREQ < 1-MinFreq )
  }

  Data <- Data %>%  select(.data$CHR, .data$POS, .data$MARKER) %>%
    mutate(REMOVE = 0) %>% as_tibble()
  First <- TRUE

  ## Now loop over lists of files
  #i=1
  for(i in 1:length(FileNames)){
    NbEnv <- length(FileNames[[i]])
    ListN <- names(FileNames)[i]
    # if(is.null(names(FileNames[[1]]))){
    #   names(FileNames[[1]]) <- paste0('Env',1:NbEnv)
    # }
    if(is.null(names(FileNames[[i]]))){
      names(FileNames[[i]]) <- paste0('Env',1:NbEnv)
    }

    for(j in 1:NbEnv){
      ## Load the columns of interest only
      FileN <- FileNames[[i]][j]
      if(Verbose){message(paste0('Loading ',FileN,'...'))}
      Data <- ReadData(ListN,FileN,VariableNames[[i]],MinFreq = MinFreq) %>%
        full_join(Data, ., by =c('CHR','POS','MARKER'))
      ## Check allele encoding
      if('ALLELE0.x'%in%colnames(Data) & 'ALLELE0.y'%in%colnames(Data)){
        if(is.null(ListN)){
          Name <- names(FileN)
        } else {
          Name <- stringr::str_c(ListN, "_", names(FileN))
        }
        Data <- Data %>%
          mutate(.,SWAPPED = ifelse(!(is.na(.data$ALLELE0.x) | is.na(.data$ALLELE0.y)), yes = ifelse(test = (.data$ALLELE0.x == .data$ALLELE1.y) & (.data$ALLELE1.x == .data$ALLELE0.y), yes = -1, no = 1), no = 1)) %>%
          mutate(!!paste0("EFFECT." , Name)
                 := eval(expr = parse(text=str_c("EFFECT.",Name,'*SWAPPED')))) %>%
          mutate(ALLELEMATCH = pmap_lgl(list(.data$ALLELE0.x,.data$ALLELE1.x,.data$ALLELE0.y,.data$ALLELE1.y), ~ !(((..3==..1)|(..3==..2)) & ((..4==..1)|(..4==..2))))) %>%
          mutate(REMOVE = .data$REMOVE + .data$ALLELEMATCH) %>%
          select(-.data$ALLELEMATCH, -.data$ALLELE0.y, -.data$ALLELE1.y, -.data$SWAPPED) %>%
          rename(ALLELE0 = .data$ALLELE0.x) %>%
          rename(ALLELE1 = .data$ALLELE1.x )
      }

      ### Message according the allele encoding detection
      if('ALLELE0'%in%colnames(Data) & First){
        if(Verbose){message(str_c("Taking the allele coding found in ", names(FileNames[[i]])[j], " as reference"))}
        First <- FALSE
      } else if (!'ALLELE0'%in%colnames(Data) & First){
        if(Verbose){message("No ALLELE given in VariableNames, assuming a same coding for all files.")}
        First <- FALSE
      }
    }
  }
  if(Verbose){message("Merging done")}

  ### Removing lines with unclear allele coding
  RemovedMarkers <- filter(Data, Data$REMOVE == 1) %>%
    select(-.data$REMOVE)
  if(length(RemovedMarkers[[1]] > 0)){
    message(str_c(length(RemovedMarkers[[1]]), " markers removed due to unclear allele coding. Removed markers are returned as Results[RemovedMarkers]."))
    Data <- filter(Data, !Data$REMOVE)
  }

  ### Filtering to keep only maf > MinFreq or maf < 1-MinFreq
  if('FREQ' %in% names(VariableNames)){
    for(freq in names(select(Data, contains('FREQ')))){
      Data[[freq]] <- replace(Data[[freq]], is.na(Data[[freq]]), 0.5) # If all markers are not the same between studies, a new line with NA will be created with the merge
      Data <- filter(Data, Data[[freq]] > MinFreq & Data[[freq]] < 1-MinFreq)
    }
  }

  ### Remove perfect duplicates
  if(DropDuplicates){
    GetDupl <-tibble::as_tibble(Data) %>%
      select(-.data$CHR,-.data$POS,-.data$MARKER) %>%
      duplicated() %>%
      which
    if(length(GetDupl)>0){
      message(paste0(length(GetDupl),' duplicated markers removed'))
    }
    Data <- filter(Data, !(.data$MARKER %in% Data[c(GetDupl),]$MARKER)) %>%
      select(-.data$REMOVE)
  } else {
    Data <- select(Data,-.data$REMOVE)
  }

  ### Remove NAs
  if(NA.rmv){
    Before <- nrow(Data)
    Data <- Data %>% drop_na()
    if(nrow(Data)<Before){
      message(str_c(Before-nrow(Data),' markers with NAs removed'))
    }
  }

  Results <- list(Data = Data, RemovedMarkers = RemovedMarkers)

  return(Results)
}
