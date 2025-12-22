#' @importFrom data.table fread
#' @importFrom plyr ddply
#' @importFrom dplyr select
#' @importFrom dplyr case_when
NULL

#' Efficiently loads an EDGE-produced taxonomic assignment from a file.
#' An assumption has been made -- since EDGE tables are generated in an automated fashion,
#' they should be properly formatted -- thus the code doesn't check for any inconsistencies except
#' for the very file existence. Note however, the unassigned to taxa entries are removed.
#' This implementation fully relies on the fread function from data.table package
#' gaining performance over traditional R techniques.
#'
#' @param filepath the path to EDGE-generated tab-delimited taxonomy assignment file.
#'
#' @param type the assignment type. Following types are recognized: 'bwa', 'diamond',
#'             'gottcha', 'gottcha2', 'kraken', 'metaphlan', and 'pangia'.
#'
#' @return a data frame containing four columns: TAXA, LEVEL, COUNT, and ABUNDANCE, representing
#'         taxonomically anchored sequences from the sample.
#'
#' @examples
#' pa_fpath <- system.file("extdata", "HMP_even//allReads-pangia.list.txt", package="MetaComp")
#' pangia_assignment = load_edge_assignment(pa_fpath, type = "pangia")
#'
#' table(pangia_assignment$LEVEL)
#'
#' pangia_assignment[pangia_assignment$LEVEL == "phylum",]
#'
#' @export
load_edge_assignment <- function(filepath, type) {

  levels <- TAXA <- LEVEL <- COUNT <- ABUNDANCE <- NULL
  ROLLUP <- NAME <- ASSIGN <- REL_ABU <- READ_COUNT <- NULL
  REL_ABUNDANCE <- NORM_COV <- READ_COUNT_RNR <- ASSIGNED <- NULL

  # check for the file existence
  if ( !file.exists(filepath) ) {
    stop(paste("Specified file \"", filepath, "\" doesn't exist!", sep = ""))
  }

  # if file is empty, return an empty table
  file_info <- file.info(filepath)
  if ( 0 == file_info$size ) {
    data.frame( LEVEL = character(), TAXA = character(), COUNT = integer(), ABUNDANCE = double())
  } else {

    # read the file
    df <- data.table::fread(filepath, sep = "\t", header = T)
    # remove empty (non-assigned) lines
    df <- df[df$LEVEL != "", ]

    # it's in the memory, let's extract columns according to the file type
    if ( 'bwa' == type ){
      # BWA output is never empty, but has a title...
      if (0 == length(df$LEVEL) ) {
        data.frame( LEVEL = character(), TAXA = character(), COUNT = integer(), ABUNDANCE = double())
      } else {
        #
        # normalize ...
        levels <- plyr::ddply(df, "LEVEL", function(x) { sum(x$ROLLUP) })
        names(levels) <- c("LEVEL", "SUM")
        df <- base::merge.data.frame(df, levels, by = c("LEVEL"))
        df$ABUNDANCE <- df$ROLLUP / df$SUM * 100
        #
        # return results, "as a data frame" to avoid any confusion...
        as.data.frame( dplyr::select(df, LEVEL, TAXA, COUNT = ROLLUP, ABUNDANCE))
      }
    } else if ( 'diamond' == type ){
      as.data.frame( dplyr::select(df, LEVEL, TAXA = NAME, COUNT = ASSIGN, ABUNDANCE = REL_ABU))
    } else if ( 'gottcha2' == type ){
      as.data.frame( dplyr::select(df, LEVEL, TAXA = NAME, COUNT = READ_COUNT, ABUNDANCE = REL_ABUNDANCE))
    } else if ( 'gottcha' == type ){
      as.data.frame( dplyr::select(df, LEVEL, TAXA, COUNT = READ_COUNT, ABUNDANCE = NORM_COV))
    } else if ( 'kraken' == type ){
      # add a normalized abundance
      max_rollup <- df[df$LEVEL == "root", ]$ROLLUP
      df$ABUNDANCE <- df$ROLLUP / max_rollup * 100
      as.data.frame( dplyr::select(df, LEVEL, TAXA, COUNT = ROLLUP, ABUNDANCE))
    } else if ( 'metaphlan' == type ){
      as.data.frame( dplyr::select(df, LEVEL, TAXA, COUNT = ASSIGNED, ABUNDANCE = ROLLUP))
    } else if ( 'pangia' == type ){
      df$READ_COUNT_RNR = as.integer(df$READ_COUNT_RNR)
      as.data.frame( dplyr::select(df, LEVEL, TAXA = NAME, COUNT = READ_COUNT_RNR, ABUNDANCE = REL_ABUNDANCE))
    } else {
      data.frame( LEVEL = character(), TAXA = character(), COUNT = integer(), ABUNDANCE = double())
    }
  }
}


