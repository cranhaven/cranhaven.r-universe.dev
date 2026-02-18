#' Search for genera and species in the Eschmeyer's Catalog of Fishes
#' 
#' @param query Character or Character Vector containing the name or names of the taxon to search. Note, you can not mix common and scientific names as a query.
#' @param type Character either "Genus" or "Species" to search for genera and species respectively.
#' Note that only one of these options can be chosen.
#' @param unavailable Logical. Should the query be run with unavailable names include? Default is FALSE.
#' @param taxon.history Should a detailed history of taxonomic changes per taxa be returned (i.e. synonymization, raised to validity, authority, etc.). Default is FALSE.
#' @param resolve Logical. If a match for the query isn't found, should an attempt be made to resolve the name using taxize?
#' @param sleep.time Numeric. Time in seconds to sleep between query calls to the California Academy of Sciences page. This is set by default to 10 seconds, which is in their robots.txt. Adjust at your own risk.
#' @param phrase Logical. Should query be passed as a quoted phrase (e.g. "Synonym of Cyprinus carpio"). Default is FALSE.
#' @param verbose Logical. Should query progress be messaged to the screen? Default is TRUE.
#' @param common.name Is the query a common name? Common names will be converted to scientific names for searching through rfishbase. Note, you can not mix common and scientific names as a query. This likely will only work for species searches. Default is FALSE.
#' @param language Language to perform common name search. Default is English.
#' @details This function searches for genera or species in the Catalog of Fishes and returns its
#' valid status, synonyms, and taxonomic history as well as reference numbers for the authority of
#' the citations. By default, the function returns basic information on a taxon, such as who described it, its current taxonomic status, type locality,
#' gender of the name, etc. If users choose taxon.history = TRUE, a detailed list of taxonomic information regarding nomenclature acts associated with the taxa is also returned.
#' Note that the function will take longer to run, sometimes twice as long if taxon.history = TRUE.
#' 
#' One problem a user may encounter using the Catalog of Fishes website is that the input taxon name must match directly to a term in the database 
#' or the database will not return any information. While this remains true using this package, users can attempt to resolve names by setting resolve = TRUE.
#' When resolve = TRUE, rcatfish_search will use the Global Names Verifier (GNverifier) in an attempt to resolve the name, which will then be passed to downstream function calls.
#' This is meant to be useful, but we recommend using this option be cautious about what the GNverifier returns. A message will print to the screen notifying you what 
#' name the GNverifier resolved to be the best match and will be used, though we strongly recommend users check the resolved name does not deviate from their expectations (i.e. a homonym or similar name for a different group is not returned).
#' @return Data frames. If taxon.history = TRUE, a list of two data frames. In this case, the first data frame TaxonSummary contains information on the description and current
#' status of the taxa in the query, references to descriptions, and information on the type locality, types, family/subfamily, distribution, and habitat for species and type species
#' gender, status, and authorities for genera searches. The second data frame, TaxonHistory contains detailed information on the taxonomic history 
#' of a taxon, such as which authorities have viewed it as a synonym or valid since its description. An itemized list describing the contents in the columns of the data returned is described below.
#' \itemize{
#'   \item Query - Character. The submitted query.
#'   \item Nominal Taxa - Character. Nominal taxonomic names.
#'   \item Author - Character. Authorship of the species/Genus description.
#'   \item DescriptionRef - Character. Eschmeyer Catalog of Fishes reference number for the genus or species description.
#'   \item DescriptionYear - Numeric. Year in which taxon was described.
#'   \item Status - Character. Current status of the nominal taxon.
#'   \item CurrentNomenclature - Character. Currently recognized taxonomic name of the taxon.
#'   \item CurrentAuthority - Character. Current authority for the valid name of the taxon.
#'   \item Holotype - Character. Catalog number of the holotype.
#'   \item Paratype - Character. Catalog number(s) of the paratypes.
#'   \item Lectotype - Character. Catalog number of the lectotype.
#'   \item Paralectotype - Character. Catalog number(s) of the paralectotype.
#'   \item Neotype - Character. Catalog number of the neotype.
#'   \item Syntype - Character. Catalog number(s) of the syntypes.
#'   \item NoTypes - Character. Specifies entries with currently no known types.
#'   \item TypeLocality - Character. Type locality of the taxon.
#'   \item Family - Character. Family the taxon belongs to.
#'   \item Subfamily - Character. Subfamily the taxon belongs to.
#'   \item Distribution - Character. Distribution of the species.
#'   \item Fresh - Numeric. Binary presence (1) or absence (0) in freshwater.
#'   \item Brackish - Numeric. Binary presence (1) or absence (0) in brackish water.
#'   \item Marine - Numeric. Binary presence (1) or absence (0) in marine water.
#'   \item IUCNYear - Numeric. Year in which IUCN status was assessed.
#'   \item IUCNStatus - Character. Status in the IUCN list of threatened species.
#'   \item NomenclatureNotes - Character. Descriptive tags identifying status as a homonym, hybrid, nomen protectum, etc.
#'   \item TypeSpecies - Character. Type Species of the genus.
#'   \item Gender - Character. Gender of the taxon.
#'   \item TypeBy - Character. Type designation.
#'   \item Notes - Character. Any notes related to the taxon or taxonomic history (e.g. treated as a subspecies, availability of name, authorship issues, etc.).
#'   \item AsSubgenus - Character. If the taxon was described as a subgenus of another genus, provides information on the genus.
#'   \item Infrasubspecific - Character. Infrasubspecific designation if it exists. 
#'   \item Miscellaneous - Character. Additional information not captured in other columns.
#'   \item ResultCode - Character. A unique identifier for each nominal taxa, useful for linking table rows when homonyms are present.
#' }
#' @examples
#' #Note that for Windows OS, OpenSSL must be used as a backend for curl. 
#' #Please see vignette on how to do this with vignette('rcatfish').
#' 
#' #Search for Abactochromis and return taxon history
#' 
#' if((.Platform$OS.type == "windows") & (grepl(pattern = "\\(OpenSSL", 
#'   curl::curl_version()$ssl_version) == TRUE)){
#' cat("openSSL backend for curl is required for the Windows version of this package, but it not 
#' detected as being active. Please see the vignette on how to configure curl with openSSL for this
#' function to work. You can access the vignette with the following: vignette('rcatfish').")
#' }else{
#' my.search <- rcatfish_search(type = "Species", query = "Abactochromis", 
#'   taxon.history = FALSE, resolve = FALSE, sleep.time = 0)
#' }
#' 
#' #Search for the genera Astatheros and Gasteropelecidae
#' \donttest{
#' if((.Platform$OS.type == "windows") & (grepl(pattern = "\\(OpenSSL", 
#'   curl::curl_version()$ssl_version) == TRUE)){
#' cat("openSSL backend for curl is required for the Windows version of this package, but it not 
#' detected as being active. Please see the vignette on how to configure curl with openSSL for this
#' function to work. You can access the vignette with the following: vignette('rcatfish').")
#' }else{
#'   my.search <- rcatfish_search(type = "Genus", query = c("Astatheros","Gasteropelecidae"), 
#'     taxon.history = TRUE, resolve = FALSE, sleep.time = 10)
#'   }
#' 
#' #Perform a species search for two different taxa
#' 
#' if((.Platform$OS.type == "windows") & (grepl(pattern = "\\(OpenSSL", 
#'   curl::curl_version()$ssl_version) == TRUE)){
#' cat("openSSL backend for curl is required for the Windows version of this package, but it not 
#' detected as being active. Please see the vignette on how to configure curl with openSSL for this
#' function to work. You can access the vignette with the following: vignette('rcatfish').")
#' }else{
#' my.search<-rcatfish_search(type = "Species", query = c("Ctenopharynx",
#'   "Pseudocrenilabrus multicolor victoriae"), taxon.history = TRUE, resolve = FALSE, 
#'   sleep.time = 10)
#'   }
#' }
#' 
#' @references 
#' Fricke, R., Eschmeyer, W.N. & van der Laan, R. (Year Accessed). Eschmeyer's Catalog of Fishes: Genera, Species, References. https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp
#' 
#' Mozzherin, D. (2025). GNverifier -- a reconciler and resolver of scientific names against more than 100 data sources. (v1.3.0). Zenodo. https://doi.org/10.5281/zenodo.17245658
#' @author Samuel R. Borstein, Brandon E. Dominy, Brian C. O'Meara 
#' @export

rcatfish_search<-function(query, type, unavailable = FALSE, taxon.history = FALSE, resolve = FALSE, sleep.time = 10, phrase = FALSE, verbose = TRUE, common.name = FALSE, language = "English"){
  if(length(type) > 1){#Check length of type. Must be 1.
    stop("Length of argument type must be 1")
  }
  
  stopifnot("Argument type must be either Genus or Species" = type %in% c("Species", "Genus", "species", "genus"))#Error if type is not a valid option
  type <- paste(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), sep="")#force lower case first letter to upper so it works.
  
  if(type != "Species" && common.name == TRUE) {
    stop("Common names can only be searched by species")
  }
  
  if(type == "Species") {
    output <- rcatfish_search_species(query, unavailable, taxon.history, resolve, sleep.time, phrase, verbose, common.name, language)
  } else if (type == "Genus") {
    output <- rcatfish_search_genus(query, unavailable, taxon.history, resolve, sleep.time, phrase, verbose)
  }  
  return(output)
}

####################################################################################
####################################################################################
####################################################################################
#
#  rcatfish_search_species
#
####################################################################################
####################################################################################
####################################################################################

rcatfish_search_species <- function(query, unavailable = FALSE, taxon.history = FALSE, resolve = FALSE, sleep.time = 10, phrase = FALSE, verbose = TRUE, common.name = FALSE, language = "English") {
  ##### Check Arguments for Parameters #####
  
  ##### Check that Windows is Using OpenSSL ##### 
  if((.Platform$OS.type == "windows") & (grepl(pattern = "\\(OpenSSL", curl::curl_version()$ssl_version) == TRUE)){
    stop("openSSL backend for curl is required for the Windows version of this package, but is not detected as being active. Please see the vignette on how to configure curl with openSSL for this function to work. You can access the vignette with the following: vignette('rcatfish')")
  }
  
  ##### Retrieve Scientific Names from Common Names #####
  if (common.name == T) {
    all.common.names <- as.data.frame(GetSci(query = query, language = language))
    colnames(all.common.names) <- c("Query", "Species", "CommonName")
    query <- unique(all.common.names$Species)
    if(length(query)==0){
      stop("No scientific names found for common name(s) in query")
    }
  }
  
  sleep.time <- ifelse(length(query)>1, sleep.time, 0) # Set sleep time for queries longer than 1
  
  #Make Tables to store results
  tax_sum_columns <- c("Query",	"NominalTaxa",	"Author",	"DescriptionRef", "RefPage", "DescriptionYear", "Status", "CurrentNomenclature", "CurrentAuthority",	"Holotype",	"Paratype",	"Lectotype",	"Paralectotype",	"Neotype",	"Syntype", "NoTypes", "TypeLocality",	"Family","Subfamily",	"Distribution", "Fresh","Brackish", "Marine", "IUCNYear", "IUCNStatus", "NomenclatureNotes", "Infrasubspecific", "Miscellaneous", "ResultCode")
  all.tax.sum <- data.frame(matrix(nrow = 0, ncol = length(tax_sum_columns)),stringsAsFactors = FALSE)
  colnames(all.tax.sum) <- tax_sum_columns
  
  if (taxon.history == T) {
    tax_hist_columns <- c("Query","NominalTaxa","Status","RecognizedNomenclature","Authority","RefNo","Notes", "ResultCode")
    all.taxon.history <- data.frame(matrix(nrow = 0, ncol = length(tax_hist_columns)))
    colnames(all.taxon.history) <- tax_hist_columns 
  }
  
  for (TaxonIndex in seq_along(query)){
    if (verbose == TRUE) {
      message(paste0("Now on query ",TaxonIndex, " of ", length(query), " ",query[TaxonIndex]))
    }
    
    local.result  <- catalog_search(query[TaxonIndex], type = "Species", unavailable, resolve, phrase)
    ptime_Start <- Sys.time()
    if (length(local.result) > 0) {
      
      current.query.dat <- data.frame(matrix(nrow = length(local.result),ncol = length(tax_sum_columns)),stringsAsFactors = FALSE)#Set up data storage
      colnames(current.query.dat) <- tax_sum_columns
      
      current.query.dat$Query <- query[TaxonIndex]#Fill current query information
      for(result.index in 1:length(local.result)){
        #loop through results and parse data
        current.query.dat$Distribution[result.index] <- get_distribution(local.result = local.result[result.index])#parse distribution data 
        current.query.dat[result.index, 21:23] <- get_habitat(local.result = local.result[result.index])#parse and store habitat data
        families <- get_family(local.result = local.result[result.index])#obtain family/subfamily
        ifelse(length(families)<2, current.query.dat[result.index,18:19] <- c(families, NA),current.query.dat[result.index,18:19] <- families)#check and store family daa
        current.query.dat[result.index,7:9] <- get_current_status(local.result = local.result[result.index])#obtain status of taxon
        current.query.dat[result.index, 10:16] <- get_types(local.result = local.result[result.index])#obtain and store type information
        current.query.dat[result.index, c(2:4,6)] <- get_focal_name(local.result = local.result[result.index])#Obtain focal species, author, year, and description reference
        current.query.dat$TypeLocality[result.index]<-get_type_locality(local.result = local.result[result.index])#Obtian type locality
        current.query.dat[result.index, 24:25] <- get_iucn(local.result = local.result[result.index]) #Get IUCN date and status
        current.query.dat[result.index, 26] <- get_nomenclature_notes(local.result = local.result[result.index]) #Get nomenclature notes (tags in bold)
        current.query.dat[result.index, c(3, 27)] <- get_infrasub(local.author = current.query.dat[result.index, 3]) #Get infrasubspecific tags
        current.query.dat[result.index, 5] <- get_page_number(local.result = local.result[result.index], processed.input = current.query.dat[result.index, 3])
        current.query.dat[result.index, 28] <- get_miscellanea(local.result[result.index], processed.input = current.query.dat[result.index,])
        current.query.dat[result.index, 29] <- paste(TaxonIndex, result.index, sep = "_")
        wrongNAindex <- which(current.query.dat == "NA", arr.ind = T)
        current.query.dat[wrongNAindex] <- NA
      }
      all.tax.sum <- rbind.data.frame(all.tax.sum,current.query.dat)#store current data in data to return
      
      if (taxon.history == TRUE) {
        for (result.index in seq_along(local.result)) {
          tryCatch(expr = {
            ##### Set Up Data Storage Tables #####
            taxon.history.dat <- data.frame(matrix(nrow = 0, ncol = length(tax_hist_columns))) # Taxon history overall storage (will contain taxon.history.status and parsed.history.dat)
            taxon.history.status <- data.frame(matrix(nrow = 2, ncol = length(tax_hist_columns))) # Nominal and current history storage
            parsed.history.dat <- data.frame(matrix(nrow = 0, ncol = length(tax_hist_columns))) # Parsed taxon history by catalog entry
            colnames(taxon.history.dat) <- colnames(taxon.history.status) <- colnames(parsed.history.dat) <- tax_hist_columns
            
            ##### Process Nominal and Current Taxon History #####
            taxon.history.status$Query <- current.query.dat$Query[result.index] # Add query info
            taxon.history.status$NominalTaxa <-current.query.dat$NominalTaxa[result.index] # Add focal name
            taxon.history.status$ResultCode <- current.query.dat$ResultCode[result.index]
            if (is.na(current.query.dat$Status[result.index])) {
              taxon.history.status$Status <- c("Nominal Species", "Currently no status")
            } else if (current.query.dat$Status[result.index] == "Uncertain") {
              taxon.history.status$Status <- c("Nominal Species", "Currently uncertain")
            } else if (current.query.dat$Status[result.index] == "Unknown") {
              taxon.history.status$Status <- c("Nominal Species", "Currently unknown")
            } else if (current.query.dat$Status[result.index] == "Synonym" && is.na(current.query.dat$CurrentNomenclature[result.index])) {
              taxon.history.status$Status <- c("Nominal Species", "Currently synonym")
            } else {
              taxon.history.status$Status <- c("Nominal Species","Current") # Add status types
            }
            taxon.history.status$RecognizedNomenclature <- c(current.query.dat$NominalTaxa[result.index],current.query.dat$CurrentNomenclature[result.index]) # Add in recognized nomenclature
            taxon.history.status$Authority <- c(current.query.dat$Author[result.index],current.query.dat$CurrentAuthority[result.index]) # Add authority info
            taxon.history.status$RefNo <- c(current.query.dat$DescriptionRef[result.index], NA) # Add in description ref 
            
            ##### Process Taxon History #####
            history <- qdapRegex::ex_between(local.result[result.index], c("\\u2022","\\u2022"), c(". <b>"," <b>")) # Extract bulleted information from catalog
            if(is.na(history)) {
              history <- qdapRegex::ex_between(local.result[result.index], c("\\u2022","\\u2022","\\u2022"), c("[A-z]+idae",". Distribution",". Habitat"), fixed = FALSE) # If history is not found due to lack of status, base off of family, distribution, or habitat
            }
            changes <- unlist(stringr::str_split(history, "\\u2022")) # Split history records on bullets
            changes <- gsub("et al,","et al.",changes) # Handle issues of commas in et al.
            
            for (change.index in seq_along(changes)) { # Process each bullet one by one
              tax.status <- qdapRegex::ex_between(changes[change.index],left = c("",""),right = c("<i>"," --"))[[1]][1] # Obtain taxa status for current bullet
              if (grepl("<a", tax.status, fixed = T)) {tax.status <- str_remove_all(tax.status, "\\<a.*?\\>|\\<\\/a\\>")}
              nom.status <- qdapRegex::ex_between(changes[change.index],left = "<i>",right = "</i>") # Obtain the nomenclature status
              if (grepl("[Hh]ybrid", tax.status)) {
                nom.status <- nom.status[[1]][1:2]
                if (is.na(nom.status[2])) {
                  nom.status <- nom.status[1]
                }
              } else {
                nom.status <- nom.status[[1]][1]
              }
              # Find any notes before authorship information for current bullet
              notes <- gsub(pattern = "^[12]\\d\\d\\d\\)?\\,?| --|<i>|</i>", replacement = "",
                            x = stringr::str_extract(string = changes[change.index], pattern = "[12]\\d\\d\\d\\)?.* --"))
              if (grepl("[Hh]ybrid", tax.status)) {
                if (grepl(paste0("^ and ", nom.status[2], " \\(.*?\\)$"), notes)) {
                  notes <- NA
                } else if (grepl(paste0("^ x ", nom.status[2], " \\(.*?\\)$"), notes)) {
                  notes <- NA
                } 
              }
              references <- tax_hist_changes(changes[change.index])
              nom.refs <- references[[1]]
              nom.refs.no <- references[[2]]
              
              current.taxon.history <- data.frame(matrix(nrow = ifelse(length(nom.refs) > 0, length(nom.refs), 0),ncol = length(tax_hist_columns)),stringsAsFactors = FALSE)#for the current history store data
              colnames(current.taxon.history) <- tax_hist_columns
              
              current.taxon.history$Query <-query[TaxonIndex]#fill with query info
              current.taxon.history$NominalTaxa <- current.query.dat$NominalTaxa[result.index]#fill in focal name
              current.taxon.history$ResultCode <- current.query.dat$ResultCode[result.index]
              current.taxon.history$Status <- tax.status#add status of taxon
              current.taxon.history$Notes <- ifelse(notes == "",NA, notes)#add in notes on status
              current.taxon.history$RecognizedNomenclature <- ifelse(length(nom.status)==2, paste(nom.status[1], "x", nom.status[2]), nom.status)#add in recognized name
              current.taxon.history$Authority <- ifelse(nchar(nom.refs) > 0, nom.refs, NA)#add in authority info
              current.taxon.history$RefNo <- nom.refs.no#put in reference number
              parsed.history.dat<-rbind(parsed.history.dat,current.taxon.history)#combine with other taxon history data
            }
            taxon.history.dat<-rbind(taxon.history.status[1,],parsed.history.dat,taxon.history.status[2,])#combine current taxon history with current/nominal data
            if(taxon.history == T) {all.taxon.history <- rbind.data.frame(all.taxon.history, taxon.history.dat)}#combine current history with all history to be returned
          },
          error = function(cond) {
            message(paste("Error when parsing taxon history for", current.query.dat$NominalTaxa[result.index]))
          })
        }
        del <- which(is.na(all.taxon.history$Status))
        RowsToDelete <- c()
        for (i in seq_along(del)) {if (is.na(all.taxon.history[del[i], 4]) && is.na(all.taxon.history[del[i], 5]) && is.na(all.taxon.history[del[i], 6]) && is.na(all.taxon.history[del[i], 7])) {RowsToDelete <- c(RowsToDelete, del[i])}}
        if(length(RowsToDelete) > 0) {all.taxon.history <- all.taxon.history[-RowsToDelete,]}
      }
    } else if (length(local.result) == 0) {
      warning(paste0("No results found for supplied taxon ",query[TaxonIndex]))#Warn users if query is not found
    }
    ptime_End <- Sys.time()
    ptime_Total <- ptime_End - ptime_Start
    sleep.time <- ifelse(ptime_Total < 10, 10-ptime_Total, 0) #Adjust sleep time between server calls based processing time
    if (TaxonIndex != length(query)) {Sys.sleep(sleep.time)} #System sleep between calls
  }
  
  if (taxon.history == TRUE) {
    all.taxon.history.cleaned <- lapply(all.taxon.history, function(x) gsub("amp;", "", x))#kill &
    all.taxon.history.cleaned <- as.data.frame(all.taxon.history.cleaned)#make data frame
    if (common.name == TRUE) {
      all.results <- list(all.tax.sum, all.taxon.history.cleaned, all.common.names)
      names(all.results) <- c("TaxonSummary", "TaxonHistory", "CommonNames")
      return(all.results)
    } else if (common.name == FALSE) {
      all.results <- list(all.tax.sum, all.taxon.history.cleaned)
      names(all.results) <- c("TaxonSummary", "TaxonHistory")
      return(all.results)
    }
  } else if (common.name == TRUE) {
    all.results <- list(all.tax.sum, all.common.names)
    names(all.results) <- c("TaxonSummary", "CommonNames")
    return(all.results)
  } else {
    return(all.tax.sum)
  }
  
}

####################################################################################
####################################################################################
####################################################################################
#
#  rcatfish_search_genus
#
####################################################################################
####################################################################################
####################################################################################

rcatfish_search_genus <- function(query, unavailable = FALSE, taxon.history = FALSE, resolve = FALSE, sleep.time = 10, phrase = FALSE, verbose = TRUE) {
  ##### Check Arguments for Parameters #####
  
  ##### Check that Windows is Using OpenSSL ##### 
  if((.Platform$OS.type == "windows") & (grepl(pattern = "\\(OpenSSL", curl::curl_version()$ssl_version) == TRUE)){
    stop("openSSL backend for curl is required for the Windows version of this package, but is not detected as being active. Please see the vignette on how to configure curl with openSSL for this function to work. You can access the vignette with the following: vignette('rcatfish')")
  }
  sleep.time <- ifelse(length(query)>1, sleep.time, 0) # Set sleep time for queries longer than 1
  
  #Make Tables to store results
  tax_sum_columns <- c("Query",	"NominalTaxa",	"Author",	"DescriptionRef", "DescriptionYear", "Status", "CurrentNomenclature", "CurrentAuthority",	"TypeSpecies", "Gender", "TypeBy",	"Family","Subfamily","Notes", "NomenclatureNotes", "AsSubgenus", "ResultCode")
  all.tax.sum <- data.frame(matrix(nrow = 0, ncol = length(tax_sum_columns)),stringsAsFactors = FALSE)
  colnames(all.tax.sum) <- tax_sum_columns
  
  if (taxon.history == T) {
    tax_hist_columns <- c("Query","NominalTaxa","Status","RecognizedNomenclature","Authority","RefNo","Notes", "ResultCode")
    all.taxon.history <- data.frame(matrix(nrow = 0, ncol = length(tax_hist_columns)))
    colnames(all.taxon.history) <- tax_hist_columns 
  }
  
  for (TaxonIndex in seq_along(query)) {
    if (verbose == TRUE) {
      message(paste0("Now on query ",TaxonIndex, " of ", length(query), " ",query[TaxonIndex]))
    }
    
    local.result  <- catalog_search(query[TaxonIndex], type = "Genus", unavailable, resolve, phrase)
    ptime_Start <- Sys.time()
    
    if (length(local.result) > 0) {
      current.query.dat <- data.frame(matrix(nrow = length(local.result),ncol = length(tax_sum_columns)),stringsAsFactors = FALSE)#Set up data storage
      colnames(current.query.dat) <- tax_sum_columns
      
      current.query.dat$Query <- query[TaxonIndex]#Fill current query information
      for(result.index in 1:length(local.result)){
        #loop through results and parse data
        current.query.dat[result.index, c(2:5,16)] <- get_focal_genus(local.result = local.result[result.index])#add in the focal genus, author and year
        current.query.dat[result.index,6:8] <- get_current_status(local.result = local.result[result.index])#get genus status
        current.query.dat$TypeSpecies[result.index] <- get_type_species(local.result = local.result[result.index])#get type species of genus
        current.query.dat$Gender[result.index] <- get_gender(local.result = local.result[result.index])#get the gender of the genus
        current.query.dat$TypeBy[result.index] <- get_type_by(local.result = local.result[result.index])#get what the type is by
        families <- get_family_genus(local.result = local.result[result.index])#Get the family of the genus
        ifelse(length(families)<2, current.query.dat[result.index,12:13] <- c(families, NA),current.query.dat[result.index,12:13] <- families)#check for subfamilies and write to table
        current.query.dat$Notes[result.index] <- get_genus_notes(local.result = local.result[result.index])#Get notes on genus
        current.query.dat$NomenclatureNotes[result.index] <- get_nomenclature_notes(local.result = local.result[result.index]) #Add in nomenclature notes (bold tags in search results)
        current.query.dat$ResultCode[result.index] <- paste(TaxonIndex, result.index, sep = "_")
        wrongNAindex <- which(current.query.dat == "NA", arr.ind = T)
        current.query.dat[wrongNAindex] <- NA
      }
      all.tax.sum <- rbind.data.frame(all.tax.sum,current.query.dat)#store current data in data to return
      
      if (taxon.history == TRUE) {
        for (result.index in seq_along(local.result)) {
          tryCatch(expr = {
            ##### Set Up Data Storage Tables #####
            taxon.history.dat <- data.frame(matrix(nrow = 0, ncol = length(tax_hist_columns))) # Taxon history overall storage (will contain taxon.history.status and parsed.history.dat)
            taxon.history.status <- data.frame(matrix(nrow = 2, ncol = length(tax_hist_columns))) # Nominal and current history storage
            parsed.history.dat <- data.frame(matrix(nrow = 0, ncol = length(tax_hist_columns))) # Parsed taxon history by catalog entry
            colnames(taxon.history.dat) <- colnames(taxon.history.status) <- colnames(parsed.history.dat) <- tax_hist_columns
            
            ##### Process Nominal and Current Taxon History #####
            taxon.history.status$Query <- current.query.dat$Query[result.index] # Add query info
            taxon.history.status$NominalTaxa <-current.query.dat$NominalTaxa[result.index] # Add focal name
            taxon.history.status$ResultCode <- current.query.dat$ResultCode[result.index]
            taxon.history.status$Status <- c("Nominal Species","Current") # Add status types
            taxon.history.status$RecognizedNomenclature <- c(current.query.dat$NominalTaxa[result.index],current.query.dat$CurrentNomenclature[result.index]) # Add in recognized nomenclature
            taxon.history.status$Authority <- c(current.query.dat$Author[result.index],current.query.dat$CurrentAuthority[result.index]) # Add authority info
            taxon.history.status$RefNo <- c(current.query.dat$DescriptionRef[result.index], NA) # Add in description ref 
            
            ##### Process Taxon History #####
            history <- qdapRegex::ex_between(local.result[result.index], c("\\u2022","\\u2022"), c(". <b>"," <b>")) # Extract bulleted information from catalog
            if(is.na(history)) {
              history <- qdapRegex::ex_between(local.result[result.index], c("\\u2022","\\u2022","\\u2022"), c("[A-z]+idae",". Distribution",". Habitat"), fixed = FALSE) # If history is not found due to lack of status, base off of family, distribution, or habitat
            }
            changes <- unlist(stringr::str_split(history, "\\u2022")) # Split history records on bullets
            changes <- gsub("et al,","et al.",changes) # Handle issues of commas in et al.      
            
            for (change.index in seq_along(changes)) { # Process each bullet one by one
              tax.status <- qdapRegex::ex_between(changes[change.index],left = c("",""),right = c("<i>"," --"))[[1]][1] # Obtain taxa status for current bullet
              nom.status <- qdapRegex::ex_between(changes[change.index],left = "<i>",right = "</i>")[[1]][1] # Obtain the nomenclature status
              # Find any notes before authorship information for current bullet
              notes <- gsub(pattern = "^[12]\\d\\d\\d\\)?\\,?| --|<i>|</i>", replacement = "",
                            x = stringr::str_extract(string = changes[change.index], pattern = "[12]\\d\\d\\d\\)?.* --"))
              
              references <- tax_hist_changes(changes[change.index])
              nom.refs <- references[[1]]
              nom.refs.no <- references[[2]]
              
              current.taxon.history <- data.frame(matrix(nrow = ifelse(length(nom.refs) > 0, length(nom.refs), 0),ncol = length(tax_hist_columns)),stringsAsFactors = FALSE)#for the current history store data
              colnames(current.taxon.history) <- tax_hist_columns
              
              current.taxon.history$Query <-query[TaxonIndex]#fill with query info
              current.taxon.history$NominalTaxa <- current.query.dat$NominalTaxa[result.index]#fill in focal name
              current.taxon.history$ResultCode <- current.query.dat$ResultCode[result.index]
              current.taxon.history$Status <- tax.status#add status of taxon
              current.taxon.history$Notes <- ifelse(notes == "",NA, notes)#add in notes on status
              current.taxon.history$RecognizedNomenclature <- nom.status#add in recognized name
              current.taxon.history$Authority <- ifelse(nchar(nom.refs) > 0, nom.refs, NA)#add in authority info
              current.taxon.history$RefNo <- nom.refs.no#put in reference number
              parsed.history.dat<-rbind(parsed.history.dat,current.taxon.history)#combine with other taxon history data
            }
            taxon.history.dat<-rbind(taxon.history.status[1,],parsed.history.dat,taxon.history.status[2,])#combine current taxon history with current/nominal data
            if(taxon.history == T) {all.taxon.history <- rbind.data.frame(all.taxon.history, taxon.history.dat)}#combine current history with all history to be returned
            RowsToDelete <- which(is.na(all.taxon.history$TaxonHistory$Status) & is.na(all.taxon.history$TaxonHistory$RecognizedNomenclature) & is.na(all.taxon.history$TaxonHistory$Authority) & is.na(all.taxon.history$TaxonHistory$RefNo) & is.na(all.taxon.history$TaxonHistory$Notes))
            if (length(RowsToDelete) > 0) {all.taxon.history <- all.taxon.history[-RowsToDelete,]}
          },
          error = function(cond) {
            message(paste("Error when parsing taxon history for", current.query.dat$NominalTaxa[result.index]))
          })
        }
        del <- which(is.na(all.taxon.history$Status))
        RowsToDelete <- c()
        for (i in seq_along(del)) {if (is.na(all.taxon.history[del[i], 4]) && is.na(all.taxon.history[del[i], 5]) && is.na(all.taxon.history[del[i], 6]) && is.na(all.taxon.history[del[i], 7])) {RowsToDelete <- c(RowsToDelete, del[i])}}
        if(length(RowsToDelete) > 0) {all.taxon.history <- all.taxon.history[-RowsToDelete,]}
      }
    } else if (length(local.result) == 0) {
      warning(paste0("No results found for supplied taxon ",query[TaxonIndex]))#Warn users if query is not found
    }
    ptime_End <- Sys.time()
    ptime_Total <- ptime_End - ptime_Start
    sleep.time <- ifelse(ptime_Total < 10, 10-ptime_Total, 0)
    if (TaxonIndex != length(query)) {Sys.sleep(sleep.time)} #System sleep between calls
  }
  if (taxon.history == TRUE) {
    all.taxon.history.cleaned <- lapply(all.taxon.history, function(x) gsub("amp;", "", x))#kill &
    all.taxon.history.cleaned <- as.data.frame(all.taxon.history.cleaned)#make data frame
    all.results <- list(all.tax.sum, all.taxon.history.cleaned)
    names(all.results) <- c("TaxonSummary", "TaxonHistory")
    return(all.results)
  } else {
    return(all.tax.sum)
  }
}