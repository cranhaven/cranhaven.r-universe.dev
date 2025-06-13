#' Convert a Coursera Course to Coursera Import
#'
#' @param x object of class \code{coursera_import} or
#' \code{coursera_course_import}
#'
#' @return object of class \code{coursera_import}
#' @export
crsra_import_as_course =  function(x) {
    if (inherits(x, "coursera_import")) {
        return(x)
    }
    if (inherits(x, "coursera_course_import")) {
        course_name = attributes(x)$course_name
        partner_user_id = attributes(x)$partner_user_id
        x = list(x)
        names(x) = course_name
        attr(x, "partner_user_id") = partner_user_id
        class(x) = "coursera_import"
        return(x)
    }
    stop("Object class not of coursera_course_import or coursera_import")
}
