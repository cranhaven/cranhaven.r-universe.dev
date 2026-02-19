#' Filter occurrences in environmental space
#'
#' Filter the occurrence with the most realible species identification in the
#' environmental space. This function is based in the function envSample
#' provided by Varela et al. (2014) and were adapted to the naturaList package to
#' select the occurrence with the most realible species identification in each
#' environmental grid.
#'
#' @param occ.cl data frame with occurrence records information already
#'   classified by \code{\link{classify_occ}} function.
#' @param env.data data frame with rows for occurrence observation and columns for
#'   each environmental variable
#' @param grid.res numeric vector. Each value represents the width of each bin
#'   in the scale of the environmental variable. The order in this vector is
#'   assumed to be the same order in the of the variables in the \code{env.data}
#'   data frame.
#' @param occurrence.id column name of \code{occ} with link or code for the
#'  occurrence record. See in
#'  \href{https://dwc.tdwg.org/terms/#dwc:occurrenceID}{Darwin Core Format}
#' @param species column name of \code{occ} with the species names.
#' @param identified.by column name of \code{occ.cl} with the name of who
#'  determined the species.
#' @param decimal.longitude column name of \code{occ.cl} longitude in decimal
#'  degrees.
#' @param decimal.latitude column name of \code{occ.cl} latitude in decimal
#'  degrees.
#' @param basis.of.record column name with the specific nature of the data
#'  record. See details.
#' @param media.type column name of \code{occ.cl} with the media type of recording.
#'  See details.
#' @param institution.code column name of \code{occ.cl} with the name (or acronym)
#'  in use by the institution having custody of the object(s) or information
#'  referred to in the record.
#' @param collection.code column name of \code{occ.cl} with The name, acronym,
#'  code, or initials identifying the collection or data set from which the
#'  record was derived.
#' @param catalog.number column name of \code{occ.cl} with an identifier
#'  (preferably unique) for the record within the data set or collection.
#' @param year Column name of \code{occ.cl} the four-digit year in which the
#'  Event occurred, according to the Common Era Calendar.
#' @param date.identified Column name of \code{occ.cl} with the date on which the
#'  subject was determined as representing the Taxon.
#'
#' @return Data frame with the same columns of \code{occ.cl}.
#'
#' @seealso \code{\link[naturaList]{classify_occ}}
#'
#' @examples
#'
#' \dontrun{
#' library(naturaList)
#' library(tidyverse)
#'
#' data("cyathea.br")
#' data("speciaLists")
#' data("r.temp.prec")
#'
#' occ <- cyathea.br %>%
#'   filter(species == "Cyathea atrovirens")
#'
#' occ.cl <- classify_occ(occ, speciaLists, spec.ambiguity = "is.spec")
#'
#' # temperature and precipitaion data
#' env.data <- raster::extract(
#'   r.temp.prec,
#'   occ.cl[,c("decimalLongitude", "decimalLatitude")]
#' ) %>% as.data.frame()
#'
#' # the bins for temperature has 5 degrees each and for precipitation has 100 mm each
#' grid.res <- c(5, 100)
#'
#' occ.filtered <- env_grid_filter(
#'   occ.cl,
#'   env.data,
#'   grid.res
#' )
#'
#' }
#'
#' @export
#'
#' @references Varela et al. (2014). Environmental filters reduce the effects
#'   of sampling bias and improve predictions of ecological niche models.
#'   *Ecography*. 37(11) 1084-1091.

env_grid_filter <- function(
    occ.cl,
    env.data,
    grid.res,
    institution.code = "institutionCode",
    collection.code = "collectionCode",
    catalog.number = "catalogNumber",
    year = "year",
    date.identified = "dateIdentified",
    species = "species",
    identified.by = "identifiedBy",
    decimal.latitude = "decimalLatitude",
    decimal.longitude = "decimalLongitude",
    basis.of.record = "basisOfRecord",
    media.type = "mediaType",
    occurrence.id = "occurrenceID"
){

  # initial checkings -------------------------------------------------------

  naturaList_levels = "naturaList_levels"
  natList_column <- "naturaList_levels" %in% colnames(occ.cl)
  if(!natList_column){
    stop("occurrences must has 'naturaList_levels' classification.")
  }

  spp <- unique(occ.cl[,species])

  if(length(spp) > 1){
    stop("there is more than 1 species in 'occ.cl'. Please, use 'env_grid_filter' with one species at a time. You can create a loop for filter more species.")
  }

  if(nrow(occ.cl) == 1){
    return(occ.cl)
  }

  if(nrow(occ.cl) != nrow(env.data)){
    stop("'occ.cl' and 'env.data' must have the same number of rows")
  }

  if(ncol(env.data) != length(grid.res)){
    stop("'grid.res' must have the same length as the number of variables in the 'env.data'")
  }


  input.env.data <- env.data
  n <- ncol(env.data)

  range.df <-
    sapply(env.data, range, na.rm = T)  %>%
    t()  %>%
    as.data.frame()

  names(range.df) <- c("min", "max")

  l.env.bin.steps <-
    lapply(1:n, function(i) {
      min <- range.df[i,"min"] - (grid.res[i]/2)
      max <- range.df[i,"max"]

      seq(min, max, by = grid.res[i])

    })

  mv.grid.bgn <- expand.grid(l.env.bin.steps)
  mv.grid.end <- NULL
  for (i in 1:n){
    ends <- mv.grid.bgn[,i] + grid.res[i]
    mv.grid.end <- cbind(mv.grid.end, ends)
  }

  mv.grid.end <- as.data.frame(mv.grid.end)

  names(mv.grid.bgn) <- paste0("begin_var_", 1:n)
  names(mv.grid.end) <- paste0("end_var_", 1:n)
  cond_df <- cbind(mv.grid.bgn, mv.grid.end)

  names(env.data) <- paste0("var_", 1:n)

  ### input data frame
  input_data <- occ.cl %>%
    dplyr::bind_cols(env.data) %>%
    dplyr::mutate(naturaList_levels = as.character(naturaList_levels),
                  row_id = 1:nrow(occ.cl)) %>%
    dplyr::arrange(naturaList_levels)


  conditions <- character()
  for(i in 1:n){
    conditions <- paste(conditions,
                        paste0(" cond_df$begin_var_", i, " <= i_data$var_", i,
                               " &", "cond_df$end_var_", i, " > i_data$var_", i, " "),
                        sep = "& ")
  }


  conditions <- substr(conditions, 2, nchar(conditions))

  group_ID <-
    sapply(1:nrow(input_data), function(i){
      i_data <- input_data[i,]
      eval(parse(text = paste0("which(", conditions, ")")))
    })

  input_data <- input_data %>%
    mutate(group_ID)

  cols.occ <- names(occ.cl)

  output_data <-
    input_data %>%
    dplyr::arrange(
      .data$group_ID,
      .data$naturaList_levels,
      desc(.data[[date.identified]]),
      desc(.data[[year]])
    ) %>%
    dplyr::group_by(.data$group_ID) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(cols.occ)) %>%
    as.data.frame()

  return(output_data)


}
