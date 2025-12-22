#' @importFrom data.table fread
#' @importFrom plyr dlply
NULL

#' Efficiently loads a BWA (or other EDGE-like taxonomic assignment) tables from a list
#' of files. Outputs a named list of assignments.
#'
#' @param filepath the path to tab delimited, two-column file whose first column is a project_id
#' (which will be used to name this assignment) and the second column is the assignment filename.
#'
#' @param type the type of assignments to be loaded. Following types are recognized: 'bwa',
#' 'diamond', 'gottcha', 'gottcha2', 'kraken', 'metaphlan', and 'pangia'.
#'
#' @return a list of all read assignments.
#'
#' @examples
#' hmp_even_fp <- system.file("extdata", "HMP_even", package="MetaComp")
#' hmp_stagger_fp <- system.file("extdata", "HMP_stagger", package="MetaComp")
#' data_files <- data.frame(V1 = c("HMP_even", "HMP_stagger"),
#'                          V2 = c(file.path(hmp_even_fp, "allReads-gottcha2-speDB-b.list.txt"),
#'                                 file.path(hmp_stagger_fp, "allReads-gottcha2-speDB-b.list.txt")))
#' write.table(data_files, file.path(tempdir(), "assignments.txt"),
#'                                                  row.names = FALSE, col.names = FALSE)
#' gottcha2_assignments = load_edge_assignments(file.path(tempdir(), "assignments.txt"),
#'                                                                             type = "gottcha2")
#'
#' names(gottcha2_assignments)
#' table(gottcha2_assignments[[1]]$LEVEL)
#'
#' @export
load_edge_assignments <- function(filepath, type) {


  # check for the file existence
  if ( !file.exists(filepath) ) {
    stop(paste("Specified file \"", filepath, "\" doesn't exist!", sep = ""))
  }

  # read the assignments table file
  df <- utils::read.table(filepath, header = F, row.names = NULL, as.is = T,
                   colClasses = c("character", "character"), stringsAsFactors = F)

  # read files, and yield the results
  res = list()
  for (prj_name in df$V1) {
    cur_asgnmt = load_edge_assignment(df[prj_name == df$V1, ]$V2, type)
    res = c(res, list(cur_asgnmt))
  }
  names(res) <- df$V1

  res

}
