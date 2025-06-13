#' Returns a list of tables a variable appears in
#'
#' @param all_tables A list from \code{\link{crsra_import_course}} or
#' \code{\link{crsra_import}}
#' @param col_name The name of the column/variable to look for
#' @return A list of tables that a specific variable appears in
#' @examples
#' crsra_whichtable(example_course_import, "assessment_id")
#' @export
crsra_whichtable <- function(
    all_tables,
    col_name){

    all_tables = crsra_import_as_course(all_tables)
    inside = lapply(all_tables, function(L) {
        res = sapply(L, function(x) {
            any(col_name %in% colnames(x))
        })
        names(res)[res]
    })
    inside
}
