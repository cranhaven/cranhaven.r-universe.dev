#' @title Merge method for odf tibbles.
#'
#' @description Merge two odf tibbles in R while keeping
#' attributes with metadata.
#'
#' @import data.table
#'
#' @param x,y
#' odf tibbles, or objects to be coerced to one
#'
#' @param by
#' A vector of shared column names in x and y to merge on. This defaults to the
#' shared key columns between the two tables. If y has no key columns, this
#' defaults to the key of x.
#'
#' @param by.x,by.y
#' Vectors of column names in x and y to merge on.
#'
#' @param all
#' logical; all = TRUE is shorthand to save setting both all.x = TRUE and
#' all.y = TRUE.
#' @param all.x
#' logical; if TRUE, rows from x which have no matching row in y are included.
#' These rows will have 'NA's in the columns that are usually filled with values
#' from y. The default is FALSE so that only rows with data from both x and y
#' are included in the output.
#' @param all.y
#' logical; analogous to all.x above.
#' @param  sort
#' logical. If TRUE (default), the rows of the merged data.table are sorted by
#' setting the key to the by / by.x columns. If FALSE, unlike base R's merge for
#' which row order is unspecified, the row order in x is retained (including
#' retaining the position of missings when all.x=TRUE), followed by y rows that
#' don't match x (when all.y=TRUE) retaining the order those appear in y.
#'
#' @param suffixes
#' A character(2) specifying the suffixes to be used for making non-by column
#' names unique. The suffix behaviour works in a similar fashion as the
#' merge.data.frame method does.
#'
#' @param no.dups
#' logical indicating that suffixes are also appended to non-by.y column names
#' in y when they have the same column name as any by.x.
#'
#' @param allow.cartesian
#' See allow.cartesian in \code{\link[data.table]{data.table}}.
#'
#' @param incomparables
#' values which cannot be matched and therefore are excluded from by columns.
#'
#' @param ...
#' Not used at this time.
#'
#' @details
#' \code{\link{merge}} is a generic function in base R. It dispatches
#' to either the merge.data.frame method, merge.odf_tbl or merge.data.table method
#' depending on the class of its first argument. merge.odf_tbl uses the
#' merge.data.table to join data.frame and adds the attributes containing
#' metadata from the two original odf data.frames.
#' Note that, unlike SQL join, NA is matched against NA (and NaN against NaN)
#' while merging.
#' For a more data.table-centric way of merging two data.tables, see
#' \code{\link[data.table]{data.table}}. See FAQ 1.11 for a detailed comparison of
#' merge.
#'
#' @return A new odf tibble build from the two input data.frames with the
#' variable attributes from the original data.frames. Sorted by the columns set
#' (or inferred for) the by argument if argument sort is set to TRUE.
#' For variables/columns occurring in both x and y, attributes are taken from x.
#'
#' @export
#' @examples
#' # get path to example data from the opendataformat package (data.zip)
#' path  <-  system.file("extdata", "data.odf.zip", package = "opendataformat")
#'
#' # read four columns of example data specified as ODF from ZIP file
#' df  <-  read_odf(file = path, select = 1:4)
#'
#' # read other columns of example data specified as ODF from ZIP file
#' df2  <-  read_odf(file = path, select = 4:7)
#'
#' # generate a variable for joining both datasets:
#' df$id<-1:20
#' df2$id<-1:20
#'
#' # merge both datasets by id column
#' merged_df<-merge(df, df2)
#'
#' #merge both datasets by shared key columns between the two tables
#' merged_df2<-merge(df, df2)
#'
#' @export
merge.odf_tbl <- function(x, y,
                          by = NULL,
                          by.x = NULL,
                          by.y = NULL,
                          all = FALSE,
                          all.x = all,
                          all.y = all,
                          sort = TRUE,
                          suffixes = c(".x", ".y"),
                          no.dups = TRUE,
                          allow.cartesian = getOption(
                            "datatable.allow.cartesian"),
                          incomparables = NULL,
                          ...) {

  if (is.null(by.x) && is.null(by.y)) {
    data_out <- data.table::merge.data.table(x = as.data.table(x), y =  as.data.table(y),
                                 by = by,
                                 all.x = all.x, all.y = all.y,
                                 sort = sort,
                                 suffixes = no.dups,
                                 no.dups = no.dups,
                                 allow.cartesian = allow.cartesian,
                                 incomparables = incomparables)
  } else {
    if (is.null(by)) {
      data_out <- data.table::merge.data.table(x = as.data.table(x), y = as.data.table(y),
                                   by.x = by.x, by.y = by.y,
                                   all.x = all.x, all.y = all.y,
                                   sort = sort,
                                   suffixes = no.dups,
                                   no.dups = no.dups,
                                   allow.cartesian = allow.cartesian,
                                   incomparables = incomparables)
    } else {
      data_out <- data.table::merge.data.table(x = as.data.table(x), y = as.data.table(y),
                                   by = by, by.x = by.x,
                                   by.y = by.y, all.x = all.x,
                                   all.y = all.y,
                                   sort = sort,
                                   suffixes = no.dups,
                                   no.dups = no.dups,
                                   allow.cartesian = allow.cartesian,
                                   incomparables = incomparables)
    }
  }

  data_out <- as_tibble(data_out)
  class(data_out) <- c("odf_tbl", class(data_out))

  xname <- gsub("dataset merged from other datasets ", "",
                attributes(x)["name"])
  xname <- gsub("dataset merged from other datasets", "", xname)
  if (xname == "NULL") xname <- NULL
  yname <- gsub("dataset merged from other datasets ", "",
                attributes(y)["name"])
  yname <- gsub("dataset merged from other datasets", "", yname)
  if (yname == "NULL") yname <- NULL
  xyname <- paste(xname, yname)
  if (length(xyname) > 0) {
    attr(data_out, "name") <- paste0("dataset merged from other datasets",
                                     " ", xyname)
  } else {
    attr(data_out, "name") <- "dataset merged from other datasets"
  }
  attr(data_out, "languages") <- unique(c(attr(x, "languages"),
                                          attr(y, "languages")))
  attr(data_out, "lang") <- unique(c(attr(x, "lang"), attr(y, "lang")))[1]
  for (var in colnames(data_out)) {
    if (var %in% colnames(x)) {
      attributes(data_out[[var]]) <- attributes(x[[var]])
    } else {
      if (var %in% colnames(y)) {
        attributes(data_out[[var]]) <- attributes(y[[var]])
      } else {
        if (var %in% paste0(colnames(x), suffixes[1])) {
          attributes(data_out[[var]]) <- attributes(y[[gsub(suffixes[1],
                                                         "", var)]])
        }
        if (var %in% paste0(colnames(y), suffixes[2])) {
          attributes(data_out[[var]]) <- attributes(y[[gsub(suffixes[2],
                                                         "", var)]])
        }
      }
    }
  }
  data_out <- setlanguage_odf(data_out, attr(data_out, "lang"))
  return(data_out)
}
