#' Find number of available fish names, number of valid species, and number of descriptions in the last 10 years by family or subfamily
#' @param query A character vector of taxa to search for. For users wishing to obtain the information for all fishes please query "Totals".
#' @param verbose Logical. Should query progress be messaged to the screen? Default is TRUE.
#' @return A list with each element containing a data frame for each query. List elements are named by the queried taxa. 
#' The list contains information on the number of genera and species available, valid, and described in the last 10 years. An itemized list describing the contents in the columns of the data frame(s) returned is described below.
#' #' \itemize{
#'   \item Class - Character. Taxonomic class.
#'   \item Order - Character. Taxonomic order.
#'   \item Family - Character. Taxonomic family
#'   \item Subfamily - Character. Taxonomic subfamily.
#'   \item Available.Genera - Integer. Number of available genera for the taxonomic rank.
#'   \item Valid.Genera - Integer. Number of valid genera for the taxonomic rank.
#'   \item Genera.Last.Ten.Years - Integer. Number of genera described over the past ten years for the taxonomic rank.
#'   \item Available.Species - Integer. Number of available species for the taxonomic rank.
#'   \item Valid.Species - Integer. Number of valid species for the taxonomic rank.
#'   \item Species.Last.Ten.Years - Integer. Number of species described over the past ten years for the taxonomic rank.
#'   }
#' @description This function retrieves and parses data on the number of genera and species per family from the 
#' Eschmeyer's Catalog of Fishes. It returns a table containing the number of genera and species that are available, valid,
#' and described in the last 10 years in its columns. Each row represents either a class, order, family, or subfamily, so 
#' some columns for higher taxonomic groups may have NA for lower taxonomic groups (e.g. if you Search for Cypriniformes, the row 
#' containing Cypriniformes will have NA for family and subfamily as it is above those taxonomic levels).
#' 
#' Given the function reads in the page containing the table, for speed, it is highly recommended that if a user wishes to obtain
#' information on numerous orders, families, or subfamilies that they pass them in as a single query. To do this, see the last example.
#' @author Samuel R. Borstein
#' @references 
#' Fricke, R., Eschmeyer, W.N. & Fong J.D. (Year Accessed). Eschmeyer’s Catalog of Fishes: Species by family/subfamily in the Catalog of Fishes. https://researcharchive.calacademy.org/research/ichthyology/catalog/SpeciesByFamily.asp.
#' @examples
#' #Search for the number of described Cypriniformes
#' MySearch <- rcatfish_species_by(query = "Cypriniformes") 
#' #Obtain the total number of genera and species available, valid, and described in the last 10 years
#' CofF_totals <- rcatfish_species_by(query = "Totals")
#' #Search for more than one family
#' MySearch <- rcatfish_species_by(query = c("Cichlidae","Embiotocidae"))
#' @export

rcatfish_species_by <- function(query, verbose = TRUE) {
  rcatfish_taxa <- GetTaxonTable()
  TaxaSummary <- list()
  for (query.index in seq_along(query)){
    if (verbose == TRUE) {
      message(paste0("Now on query ",query.index, " of ", length(query),", ",query[query.index]))
    }
    if(query[query.index] == "Totals"){
      TotalHit <- nrow(rcatfish_taxa)
      Totals <- rcatfish_taxa[TotalHit,]
      Totals[1,2:4] <- "Totals"
      TaxaSummary[[query.index]] <- data.frame(Totals, stringsAsFactors=FALSE)
      names(TaxaSummary)[query.index] <- query[query.index]
      }else{
      found <- which(apply(X = rcatfish_taxa, MARGIN = 2, FUN = is.element, el = query[query.index])==TRUE)[1]#Find the taxon in table
      if(is.na(found)){
        warning(paste0("No results found for supplied taxon ",query[query.index]))
      }else{
        Focal.Tax <- which(as.character(rcatfish_taxa[,found])==query[query.index])#Find Children of taxa
        Focal.Tax.Dat <- rcatfish_taxa[Focal.Tax,]#Subset children of taxa
        rownames(Focal.Tax.Dat) <- nrow(Focal.Tax)#renumber rows
        Taxa2Sum <- suppressWarnings(which(is.na(as.numeric(Focal.Tax.Dat$`Valid Genera`))))#Find group to summarize
        SubFam2Kill <- which(!is.na(Focal.Tax.Dat$Subfamily))
        if(length(SubFam2Kill) > 0){
          Fams2Sum <- Focal.Tax.Dat[-SubFam2Kill,]#Kill off subfamilies, which are already summed at family level
          Fams2Sum <- Fams2Sum[!is.na(Fams2Sum$Family),]
        }else{
          Fams2Sum <- Focal.Tax.Dat[!is.na(Focal.Tax.Dat$Family),]
        }
        MissingClass <- Focal.Tax.Dat[which(is.na(Focal.Tax.Dat$Order) & is.na(Focal.Tax.Dat$Family)),]
        MissingOrder <-  Focal.Tax.Dat[which(!is.na(Focal.Tax.Dat$Order) & is.na(Focal.Tax.Dat$Family)),]
        #Fix Duplicate Name Issues
        if(nrow(MissingOrder) > 0){
          for(order.index in 1:length(unique(MissingOrder$Order))){
            Dat2Store.Order <- match(MissingOrder$Order[order.index], Focal.Tax.Dat$Order)
            Current.Order <- Fams2Sum[Fams2Sum$Order == MissingOrder$Order[order.index],]
            Focal.Tax.Dat[Dat2Store.Order,5:10] <- colSums(Current.Order[,5:10])
          }
        }
        if(nrow(MissingClass) > 0){
          for(class.index in 1:length(unique(MissingClass$Class))){
            Dat2Store.Class <- match(MissingClass$Class[class.index], Focal.Tax.Dat$Class)
            Current.Class <- Fams2Sum[Fams2Sum$Class == MissingClass$Class[class.index],]
            Focal.Tax.Dat[Dat2Store.Class,5:10] <- colSums(Current.Class[,5:10])
          }
        }
        TaxaSummary[[query.index]] <- data.frame(Focal.Tax.Dat, stringsAsFactors=FALSE)
        names(TaxaSummary)[query.index] <- query[query.index]
      }
    }
    }
  return(TaxaSummary)
}


#' Get the table with species numbers by taxonomy
#' 
#' @return Object of class data.frame of taxonomy and number of described/valid species
#' @author Samuel R. Borstein
#' @noRd

GetTaxonTable <- function() {
  webpage <- xml2::read_html("https://researcharchive.calacademy.org/research/ichthyology/catalog/SpeciesByFamily.asp")
  tbls <- rvest::html_elements(webpage, "table")#capture table
  speciesTable<- rvest::html_table(x = tbls[5])[[1]]
  classes <- c("Totals","Myxini","Petromyzonti","Elasmobranchii","Holocephali","Cladistii","Actinopteri","Coelacanthi","Dipneusti")#Make vector of classes to find to keep
  speciesTable$Class[!speciesTable$Class%in%classes]<- NA#kill off non-classes in class column
  speciesTable$Order[which(!grepl("iformes",speciesTable$Order))] <- NA#kill off non-orders in order column
  speciesTable$Family[which(!grepl("idae",speciesTable$Family))] <- NA#kill off non-families in family column
  speciesTable$Subfamily[which(!grepl("inae",speciesTable$Subfamily))] <- NA#kill off non-subfamily in subfamily column
  #colnames(speciesTable) <- speciesTable[1,]#Rename columns into useful variables
  #speciesTable <- speciesTable[-1,]#Kill header lines
  #filled<-as.data.frame(apply(speciesTable, 2, function(x) gsub("^$|^ $", NA, x)),stringsAsFactors = FALSE)#Replace empty with NA
  speciesTable <- tidyr::fill(speciesTable, "Family", .direction = c("down"))#Fill in NA with the family/order
  speciesTable$Family[is.na(speciesTable$`Available Genera`)]<-NA
  speciesTable <- tidyr::fill(speciesTable, "Order", .direction = c("down"))#Fill in NA with the family/order
  speciesTable$Order[which(!is.na(speciesTable$Class)&is.na(speciesTable$`Available Genera`))]<-NA
  speciesTable <- tidyr::fill(speciesTable, "Class", .direction = c("down"))#Fill in NA with the family/order
  speciesTable[grep("Totals",speciesTable$Class),2:4]<-NA#for Totals, make it only Totals
  speciesTable <- as.data.frame(speciesTable)
  return(speciesTable)
}
