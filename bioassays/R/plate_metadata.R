#' @name plate_metadata
#' @aliases plate_metadata
#' @title Combining Plate Specific Information with Metadata
#'
#' @description plate_metadata combine the plate specific information (like compounds used, standard concentration, dilution of samples, etc) and metadata, to produce a plate specific metadata.
#'
#' @usage plate_metadata (plate_details, metadata, mergeby = "type")
#'
#' @param plate_details plate specific information that need to be added to metadata
#' @param metadata column that is common to both metadata and plate_meta (as a string in "")
#'
#'@param mergeby column that is common to both metadata and plate_meta (as a string in "")
#'
#'@details plate_details need to be in a list format. Metadata should have a 'row' and 'col' columns representing the row and column names of the corresponding multi well plate.
#'
#'@return A dataframe. Each element of 'plate_details' will appear as a new column to the left of 'metadata'
#'
#'@author A.A Palakkan
#'
#'@examples
#'## loading data
#'data(metafile96)
#'plate_details <- list("compound" = "Taxol",
#'                      "concentration" = c(0.00,0.01,0.02,0.05,0.10,1.00,5.00,10.00),
#'                      "type" = c("S1","S2","S3","S4","S5","S6","S7","S8"),
#'                      "dilution" = 1)
#'
#'## eg:1 filling metadata96 using plate_details
#'plate_meta<-plate_metadata(plate_details,metafile96,mergeby="type")
#'head(plate_meta)
#'
#'@keywords manip
#'
#'
#'@importFrom magrittr %>%
#'@importFrom dplyr arrange select rename inner_join
#'@export

plate_metadata <- function (plate_details, metadata,mergeby="type"){

# Initial setup #
position <- NULL
plate_meta <- plate_details
plate_meta<-as.data.frame(plate_meta)


plate_metadata<-merge(metadata,plate_meta,by=mergeby, all=TRUE)

plate_metadata$concentration.y <- as.numeric(plate_metadata$concentration.y)
plate_metadata$concentration.x <- as.numeric(plate_metadata$concentration.x)
plate_metadata$dilution.x <- as.numeric(plate_metadata$dilution.x)
plate_metadata$dilution.y <- as.numeric(plate_metadata$dilution.y)


plate_metadata<- dplyr::arrange(plate_metadata,position)
row.names(plate_metadata)<-c(seq(1,nrow(plate_metadata),1))



for (c in seq_along(rownames(plate_metadata))) {

  if(is.na(plate_metadata[c,("dilution.x")]))
    {plate_metadata[c,("dilution.x")] <- plate_metadata[c,("dilution.y")]}


  if(is.na(plate_metadata[c,("concentration.x")]))
  {plate_metadata[c,("concentration.x")]<-plate_metadata[c,("concentration.y")]}
}

plate_metadata<-plate_metadata[(!is.na(plate_metadata$position)),]

plate_metadata<- plate_metadata %>%
  dplyr::select(c("row","col","type","position","id","dilution.x","concentration.x","compound"))
plate_metadata <- dplyr::rename(plate_metadata, c("dilution" = "dilution.x", "concentration" = "concentration.x"))
plate_metadata <- plate_metadata[order(plate_metadata$position),]
row.names(plate_metadata) <- seq(1,nrow(plate_metadata),1)

return(plate_metadata)
}
