#' Ordered list of course items and the number and share of learners who have completed the item
#'
#' @param all_tables A list from \code{\link{crsra_import_course}} or
#' \code{\link{crsra_import}}
#'
#' @return A table which lists all the item within a course and the total number of learners and the share of learners who have completed the item.
#' @examples
#' crsra_progress(example_course_import)
#' @export
#' @importFrom dplyr row_number
#'
#'
crsra_progress <- function(all_tables){
    partner_user_id = attributes(all_tables)$partner_user_id
    all_tables = crsra_import_as_course(all_tables)
    numcourses = length(all_tables)
    coursenames = names(all_tables)

    course_module_order = course_lesson_order = NULL
    course_item_order = NULL
    rm(list = c("course_module_order", "course_lesson_order",
                "course_item_order"))

    # this is for ordering course items withing lessons and modules
    ordering <- function(x, y, z) {
        temp <- x %>%
            dplyr::left_join(y, by = "course_lesson_id", `copy`=TRUE) %>%
            dplyr::left_join(z, by = "course_module_id", `copy`=TRUE) %>%
            dplyr::arrange(course_module_order, course_lesson_order, course_item_order) %>%
            dplyr::mutate(item_rank = row_number())
    }

    itemorder <- purrr::map(
        1:numcourses,
        ~ ordering(all_tables[[.x]][["course_items"]],
                   all_tables[[.x]][["course_lessons"]],
                   all_tables[[.x]][["course_modules"]]))

    course_progress_ts = item_rank = NULL
    Total = Share = course_item_name = NULL
    course_lesson_name = course_module_name = NULL
    rm(list = c("course_progress_ts", "item_rank", "Total",
                "Share", "course_item_name",
                "course_lesson_name", "course_module_name"))
    progress <- function(x, y){

        temp <- x %>%
            dplyr::filter(!is.na(course_progress_ts)) %>%
            dplyr::group_by_(partner_user_id) %>% # 3 is the index of the column referring to partner_user_id
            dplyr::filter(course_progress_ts == max(course_progress_ts)) %>% # only to record the last activity
            dplyr::left_join(y, by = "course_item_id", `copy`=TRUE) %>% # so that we know the name of the course item
            dplyr::group_by(item_rank) %>%
            dplyr::summarise(Total=n()) %>%
            dplyr::arrange(desc(Total)) %>%
            dplyr::mutate(Share=round(Total/sum(Total), 2)) %>%
            dplyr::left_join(y, by = "item_rank") %>%
            dplyr::select(item_rank, Total, Share, course_item_name, course_lesson_name, course_module_name)
    }

    last_activity <- purrr::map(
        1:numcourses,
        ~ progress(all_tables[[.x]][["course_progress"]],
                   itemorder[[.x]]))
    names(last_activity) <- coursenames
    return(last_activity)
}





