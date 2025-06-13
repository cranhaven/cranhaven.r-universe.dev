#' Imports all the .csv files into one list consisting of all
#' the tables within the course.
#'
#' @param workdir A character string vector indicating the
#' directory where the unzipped course is stored.
#' @param add_course_name Should a column of the course name
#' be added to all the \code{data.frame}s
#' @param change_pid_column Should the \code{partner_user_id}
#' column be changed to simply say \code{"partner_user_id"}?
#' @param check_problems Should problems with reading in the
#' data be checked?
#'
#' @examples
#' zip_file = system.file("extdata", "fake_course_7051862327916.zip",
#' package = "crsra")
#' bn = basename(zip_file)
#' bn = sub("[.]zip$", "", bn)
#' res = unzip(zip_file, exdir = tempdir(), overwrite = TRUE)
#' workdir = file.path(tempdir(), bn)
#' course_tables = crsra_import_course(workdir,
#' check_problems = FALSE)
#' @export
#' @importFrom purrr map_lgl
#' @importFrom dplyr select_
#' @importFrom readr problems read_csv cols
crsra_import_course <- function(
    workdir = ".",
    add_course_name = FALSE,
    change_pid_column = FALSE,
    check_problems = TRUE) {

    dircheck <- file.path(workdir, "course_branch_grades.csv")
    if (!file.exists(dircheck)) {
        msg = paste0("Did not find course_branch_grades.csv.",
                     "Please make sure you have set your ",
                     "working directory to where the Coursera ",
                     "data dump is located.")
        stop(msg)
    }

    ########################################################################
    files = list.files(pattern = ".csv$", path = workdir, full.names = TRUE)
    stubs <- sub(pattern = "[.]csv$", replacement = "", basename(files))
    suppressWarnings({
        suppressMessages({
            dfs = purrr::map(files, read_csv, progress = FALSE,
                             col_types = cols())
        })
    })
    names(dfs) = stubs
    if (check_problems) {
        any_probs = purrr::map(dfs, problems)
        any_probs = purrr::map_lgl(any_probs, function(x) {
            nrow(x) > 0
        })

        if (any(any_probs)) {
            suppressWarnings({
                suppressMessages({
                    dfs[ any_probs] = lapply(
                        files[any_probs],
                        read_csv, guess_max = Inf,
                        progress = FALSE, col_types = cols())
                })
            })
        }
        any_probs = purrr::map(dfs, problems)
        any_probs = purrr::map_lgl(any_probs, function(x) {
            nrow(x) > 0
        })
        if (any(any_probs)) {
            ind = stubs[which(any_probs)]
            warning(paste0("Data set ", paste(ind, collapse = ", "),
                           " may still have some data errors"))
        }
    }

    dfs$peer_review_part_free_responses =
        dfs$peer_review_part_free_responses %>%
        dplyr::select_("peer_assignment_id",
                       "peer_assignment_review_schema_part_id",
                       "peer_review_id",
                       "peer_review_part_free_response_text")
    course_name = dfs$courses$course_name
    if (add_course_name) {
        dfs = lapply(dfs, function(x) {
            if (nrow(x) == 0) {
                return(x)
            }
            x$course_name = course_name
            x
        })
    }
    cn = colnames(dfs$users)
    ind = grep("user_id", cn)
    if (length(ind) > 1) {
        warning("More than 1 column in users has a user_id name!")
        ind = ind[1]
    }
    partner_user_id = cn[ind]
    if (change_pid_column) {
        dfs = lapply(dfs, function(x) {
            cn = colnames(x)
            if (partner_user_id %in% cn) {
                cn[cn %in% partner_user_id] = "partner_user_id"
            }
            colnames(x) = cn
            x
        })
    }

    attr(dfs, "course_name") = course_name
    attr(dfs, "partner_user_id") = partner_user_id
    class(dfs) = "coursera_course_import"

    return(dfs)
}
