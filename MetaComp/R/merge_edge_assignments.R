#' @importFrom dplyr select
NULL

#' Merges two or more EDGE-like taxonomical assignments. The input data frames are assumed to
#' have the following columns: LEVEL, TAXA, and ABUNDANCE -- these will be used in the merge
#' procedure, all other columns will be ignored.
#'
#' @param assignments A named list of assignments (the list element's name will be used as a
#'                      resulting data frame column name).
#'
#' @return A merged table, which is a data frame whose rows are taxonomical ids and columns are
#'          the input assignments ids.
#' @examples
#' \dontrun{
#' hmp_even_fp <- system.file("extdata", "HMP_even", package="MetaComp")
#' hmp_stagger_fp <- system.file("extdata", "HMP_stagger", package="MetaComp")
#' data_files <- data.frame(V1 = c("HMP_even", "HMP_stagger"),
#'                          V2 = c(file.path(hmp_even_fp, "allReads-gottcha2-speDB-b.list.txt"),
#'                                 file.path(hmp_stagger_fp, "allReads-gottcha2-speDB-b.list.txt")))
#' write.table(data_files, file.path(tempdir(), "assignments.txt"),
#'                                                  row.names = FALSE, col.names = FALSE)
#' gottcha2_assignments = merge_edge_assignments(
#'                          load_edge_assignments(
#'                            file.path(tempdir(), "assignments.txt"), type = "gottcha2"))
#' }
#'
#' @export
merge_edge_assignments <- function(assignments) {

  # fix CRAN notes
  #
  LEVEL <- TAXA <- ABUNDANCE <- NULL # fix the CRAN note

  # extract only rows wich correspond to the desired taxonomy level and name the first column
  #
  res <- dplyr::select(assignments[[1]], LEVEL, TAXA, ABUNDANCE)
  names(res) <- c(names(res)[1:2], names(assignments)[1])

  # iterate over the rest of the input list whilst merging the resulting table with
  # the current list's element
  #
  if (length(assignments) > 1) {
    for (i in 2:length(assignments)) {
      res  <- base::merge.data.frame(res, dplyr::select(assignments[[i]], LEVEL, TAXA, ABUNDANCE),
                    by = c("LEVEL", "TAXA"), all = T)
      names(res) <- c(names(res)[1:(length(names(res)) - 1)], names(assignments)[i])
    }
  }

  # merge produces NAs when there is no corresponding value in to-be-merged column, fix these with
  # zeros
  #
  res[is.na(res)] <- 0

  # and voila
  #
  res
}
