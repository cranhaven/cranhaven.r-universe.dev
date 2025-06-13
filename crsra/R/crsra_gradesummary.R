#' The average course grade across different groups
#'
#' @param all_tables A list from \code{\link{crsra_import_course}} or
#' \code{\link{crsra_import}}
#' @param groupby A character string indicating the how to break down
#' grades. The default is set to \code{total} and returns the grade
#' summary for each course. Other values are \code{gender} (for
#' grouping by gender), \code{education} (for grouping by education
#' level), \code{stustatus} (for grouping by student status),
#' \code{empstatus} (for grouping by employment status), and
#' \code{country} (for grouping by country). Note that this
#' grouping uses the entries in the table \code{users} that is
#' not fully populated so by grouping you lose some observations.
#' @return A table which indicates the average grade across specified
#'  groups for each course
#' @examples
#' crsra_gradesummary(example_course_import)
#' crsra_gradesummary(example_course_import, groupby = "education")
#' @export crsra_gradesummary
#'
crsra_gradesummary <- function(
    all_tables,
    groupby = c("total", "country", "language", "gender",
                "empstatus", "education", "stustatus")
) {
    partner_user_id = attributes(all_tables)$partner_user_id
    all_tables = crsra_import_as_course(all_tables)
    numcourses = length(all_tables)
    message("Note that maximum grade possible is 1.")


    groupby = match.arg(groupby)
    coursenames = names(all_tables)
    varname = switch(groupby,
                     "total" = "",
                     "country" = "country_cd",
                     "language" = "profile_language_cd",
                     "gender" = "reported_or_inferred_gender",
                     "empstatus" = "employment_status",
                     "education" = "educational_attainment",
                     "stustatus" = "student_status")
    course_grade_overall = AvgGrade = NULL
    rm(list = c("AvgGrade", "course_grade_overall"))

    grading <- function(x, y, z) {
        temp <- z %>%
            dplyr::left_join(x, by=partner_user_id, `copy`=TRUE) %>%
            dplyr::left_join(y, by=partner_user_id, `copy`=TRUE) %>%
            dplyr::filter(!is.na(course_grade_overall))
        if (groupby == "total") {
            temp = temp %>%
                dplyr::summarise(AvgGrade=mean(course_grade_overall))
            return(temp)
        } else {
            temp$y = temp[[varname]]
            temp = temp %>%
                dplyr::filter(!is.na(y)) %>%
                dplyr::group_by(y) %>%
                dplyr::summarise(AvgGrade=mean(course_grade_overall)) %>%
                dplyr::arrange(desc(AvgGrade))
            return(temp)
        }
    }

    gradetable <- purrr::map(1:numcourses,
                             ~ grading(all_tables[[.x]][["course_memberships"]],
                                       all_tables[[.x]][["course_grades"]],
                                       all_tables[[.x]][["users"]]))
    names(gradetable) <- coursenames
    return(gradetable)
}
