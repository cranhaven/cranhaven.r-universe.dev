#' Deletes a specific user from all tables in the data in case Coursera data
#' privacy laws require you to delete a specific (or set of) user(s) from your
#' data.
#' @param all_tables A list from \code{\link{crsra_import_course}} or
#' \code{\link{crsra_import}}
#' @param users A vector of user ids to delete
#' @examples
#' del_user = example_course_import$users$jhu_user_id[1]
#' del_user %in% example_course_import$users$jhu_user_id
#' res = crsra_delete_user(example_course_import, users = del_user)
#' del_user %in% res$users$jhu_user_id
#'
#' @return A list that contains all the tables within each course.
#' @export
crsra_delete_user <- function(
    all_tables,
    users){

    if (!inherits(all_tables, "coursera_course_import")) {
        stop(paste0("Object is not a coursera_course_import, stopping. ",
                    "If this is a multi-course import, use lapply"))
    }
    col_to_delete <- attributes(all_tables)$partner_user_id

    delete_users <- function(x) {
        if (col_to_delete %in% colnames(x)) {
            vec = x[, col_to_delete, drop = TRUE]
            keep = !(vec %in% users)
            x = x[keep, , drop = FALSE]
        }
        return(x)
    }

    all_tables.deleted <- purrr::map(all_tables, delete_users)
    return(all_tables.deleted)

}


#crsra_delete_user(course_data, users = "e921665b")
