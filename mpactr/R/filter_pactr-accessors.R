#' Return the summary for a single mpactr filter.
#'
#' @description
#' `filter_summary()` is a wrapper function to return the summary
#' from a single filter within the given mpactr object.
#'
#' @param mpactr_object The mpactr object that is created by calling
#' the import_data() function.
#'
#' @param filter The name of a filter whose summary is to be extracted.
#' Must be one of: "mispicked", "group", "replicability", or "insource".
#' @param group If filter = "group", the name of the Biological_Group
#' used to filter.
#'
#' @return a `list` reporting 1) compound ids for compounds which failed
#' the filter and 2) compound ids for compounds which passed the filter.
#' @export
#'
#' @examples
#' data <- import_data(example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_mispicked_ions(data)
#'
#' mispicked_summary <- filter_summary(data_filter, filter = "mispicked")
#' mispicked_summary
#'
filter_summary <- function(mpactr_object, filter, group = NULL) {
  return(mpactr_object$get_log(filter = filter, group = group))
}

#' Get similar ion groups.
#'
#' @description
#' `get_similar_ions()` is a wrapper function to return similar ion groups
#' determined with the [filter_mispicked_ions()].
#'
#' @param mpactr_object The mpactr object that is created by calling the
#' import_data() function.
#'
#' @return a `data.table` reporting the main ion and those found to be
#' similar with [filter_mispicked_ions()].
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_mispicked_ions(data)
#'
#' mispicked_ion_groups <- get_similar_ions(data_filter)
#' mispicked_ion_groups
#'
get_similar_ions <- function(mpactr_object) {
  return(mpactr_object$get_mispicked_ions())
}

#' Get groups averages.
#'
#' @description
#' `get_group_averages()` is a wrapper function to return group averages
#' for the filtered peak table.
#'
#' @param mpactr_object The mpactr object that is created by calling the
#' import_data() function.
#'
#' @return a `data.table` reporting the average and relative standard
#' deviation across biological groups and technical replicates within
#' each group.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_group(data, group_to_remove = "Blanks")
#'
#' group_averages <- get_group_averages(data_filter)
#' head(group_averages)
#'
get_group_averages <- function(mpactr_object) {
  return(mpactr_object$get_group_averages())
}

#' Get CV values.
#'
#' @description
#' `get_cv_data()` is a wrapper function to return cv (coefficient of
#' variation) calculated with [filter_cv()].
#'
#' @param mpactr_object The mpactr object that is created by calling
#' the import_data() function.
#'
#' @return a `data.table` reporting the mean and median coefficient
#' of variation for each input ion.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_cv(data,
#'   cv_threshold = 0.01,
#' )
#'
#' cv <- get_cv_data(data_filter)
#' head(cv)
#'
get_cv_data <- function(mpactr_object) {
  return(mpactr_object$get_cv())
}
