#' Imports all the .csv files into one list consisting of all
#' the courses and all the tables within each course.
#'
#' @param workdir A character string vector indicating the directory
#' where all the unzipped course directories are stored.
#' @param ... Additional arguments to pass to
#' \code{\link{crsra_import_course}}
#' @examples
#' zip_file = system.file("extdata", "fake_course_7051862327916.zip",
#' package = "crsra")
#' bn = basename(zip_file)
#' bn = sub("[.]zip$", "", bn)
#' res = unzip(zip_file, exdir = tempdir(), overwrite = TRUE)
#' example_import = crsra_import(workdir = tempdir(),
#' check_problems = FALSE)
#'
#' @export
#' @importFrom purrr map
crsra_import <- function(workdir = ".", ...) {


    dn <- list.files(path = workdir,
                     pattern = "course_branch_grades.csv",
                     recursive = TRUE,
                     full.names = TRUE)
    dirnames = unique(dirname(dn))

    all_tables = purrr::map(dirnames, crsra_import_course, ...)
    coursenames = sapply(all_tables, function(x){
        attributes(x)$course_name
    })
    names(all_tables) = coursenames

    partner_user_ids = sapply(all_tables, function(x){
        attributes(x)$partner_user_id
    })
    partner_user_id = unique(partner_user_ids)
    attr(all_tables, "partner_user_id") = partner_user_id

    class(all_tables) = "coursera_import"
    return(all_tables)

}
