#' Anonymizes ID variables (such as Partner hashed user ids) throughout
#' the data set. The function is based on the function \code{digest} from the
#' package \code{digest}.
#'
#' This function will still keep the relationship between tables, i.e. it will
#' change a specific id across all tables to the same id.
#' @param all_tables A list from \code{\link{crsra_import_course}} or
#' \code{\link{crsra_import}}
#' @param col_to_mask The name of id column to mask.
#' @param algorithm The algorithms to be used for anonymization;
#' for currently available choices, see \code{\link{digest}}.
#' @examples
#' res = crsra_anonymize(example_course_import,
#' col_to_mask = "jhu_user_id",
#' algorithm = "crc32")
#' @return A list that contains all the tables within each course.
#' @export
#' @importFrom digest digest
crsra_anonymize <- function(
    all_tables,
    col_to_mask = attributes(all_tables)$partner_user_id,
    algorithm = "crc32") {

    if (!inherits(all_tables, "coursera_course_import")) {
        stop(paste0("Object is not a coursera_course_import, stopping. ",
                    "If this is a multi-course import, use lapply"))
    }

    anonymize <- function(x){
        x = as.character(x)
        ux = unique(x)
        unq_hashes <- vapply(ux, function(object) {
            digest(object, algo = algorithm)
        }, FUN.VALUE = "", USE.NAMES = TRUE)
        unname(unq_hashes[x])
    }

    makesample <- function(x) {
        cn = colnames(x)
        if (col_to_mask %in% cn) {
            col_run = x[, col_to_mask, drop = TRUE]
            col_run = anonymize(col_run )
            x[, col_to_mask] = col_run
        }
        return(x)
    }

    all_tables.encrypted <- purrr::map(
        all_tables, .f = makesample)
    return(all_tables.encrypted)
}

