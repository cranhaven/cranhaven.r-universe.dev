# To be added in future releases
#' new_cervical_summary <- function(.data, .summary, data_name, level) {
#'   orgs <- n_distinct(.data[[level]])
#'   period <- .data$period
#'   min_period <- min(period)
#'   max_period <- max(period)
#'   months <- ((min_period %--% max_period) /months(1)) + 1
#'   sources <- .data %>%
#'     distinct(source) %>%
#'     pull(source)
#'   categories <- .data %>%
#'     distinct(age_group) %>%
#'     pull(age_group)
#'
#'   vctrs::new_data_frame(
#'     df_list(.summary),
#'     class = c('screening_summary', 'tbl'),
#'     data_orgs = orgs,
#'     data_level = level,
#'     data_type = data_type(.data),
#'     data_period = str_c(format(min_period, '%B %Y'), ' to ', format(max_period, '%B %Y')),
#'     data_months = str_c(months, ' months'),
#'     data_sources = str_c(sources, collapse = ', '),
#'     data_categories = str_c(categories, collapse = ', '),
#'     data_name = data_name
#'   )
#' }
#'
#' #' @export
#' summary.cacx_screened <- function(object, ...) {
#'   if (is.null(object)) {
#'     cancerscreening_abort('x' = 'dataframe is null.')
#'   }
#'
#'   data_name <- deparse(substitute(object))
#'   level <- data_level(object)
#'   ou_level <- ifelse(level %in% c('subcounty', 'ward', 'facility'), 'county', level)
#'   period <- object$period
#'   min_period <- min(period)
#'   max_period <- max(period)
#'   months <- ((min_period %--% max_period) /months(1)) + 1
#'   target <- get_cervical_target_population(year(max_period), ou_level)
#'
#'   object_summary <- object %>%
#'     filter(age_group == '25-49') %>%
#'     group_by(across(any_of(ou_level))) %>%
#'     summarise(
#'       screened = sum(value)
#'     ) %>%
#'     left_join(target, by=ou_level) %>%
#'     mutate(
#'       target = target/12 * months,
#'       coverage = screened/target
#'     )
#'
#'   new_cervical_summary(object, object_summary, data_name, level)
#' }
#'
#' #' @export
#' summary.cacx_positive <- function(object, ...) {
#'   if (is.null(object)) {
#'     cancerscreening_abort('x' = 'dataframe is null.')
#'   }
#'
#'   data_name <- deparse(substitute(object))
#'   level <- data_level(object)
#'   ou_level <- ifelse(level %in% c('subcounty', 'ward', 'facility'), 'county', level)
#'
#'   object_summary <- object %>%
#'     filter(age_group == '25-49') %>%
#'     group_by(across(any_of(ou_level))) %>%
#'     summarise(
#'       positive = sum(value)
#'     )
#'
#'   new_cervical_summary(object, object_summary, data_name, level)
#' }
#'
#' #' @export
#' summary.cacx_treated <- function(object, ...) {
#'   if (is.null(object)) {
#'     cancerscreening_abort('x' = 'dataframe is null.')
#'   }
#'
#'   data_name <- deparse(substitute(object))
#'   level <- data_level(object)
#'   ou_level <- ifelse(level %in% c('subcounty', 'ward', 'facility'), 'county', level)
#'
#'   object_summary <- object %>%
#'     filter(age_group == '25-49') %>%
#'     group_by(across(any_of(ou_level))) %>%
#'     summarise(
#'       treated = sum(value)
#'     )
#'
#'   new_cervical_summary(object, object_summary, data_name, level)
#' }
#'
#' #' @export
#' tbl_sum.screening_summary <- function(x, ...) {
#'   default_header <- NextMethod()
#'   summary_values <- get_summary_values(x)
#'   names(summary_values) <- get_summary_dnames(x)
#'
#'   c(
#'     default_header,
#'     summary_values
#'   )
#' }
#'
#' get_summary_dnames <- function(summary_object) {
#'   c(
#'     'Data name',
#'     'Data type',
#'     'Organisations level',
#'     'Number of organisations',
#'     'Period',
#'     'Months',
#'     'Sources',
#'     'Categories'
#'   )
#' }
#'
#' get_summary_values <- function(summary_object) {
#'   c(
#'     data_name(summary_object),
#'     data_type(summary_object),
#'     data_level(summary_object),
#'     data_orgs(summary_object),
#'     data_period(summary_object),
#'     data_months(summary_object),
#'     data_sources(summary_object),
#'     data_categories(summary_object)
#'   )
#' }
#'
#' data_name <- function(object) {
#'   attr(object, 'data_name')
#' }
#'
#' data_orgs <- function(object) {
#'   attr(object, 'data_orgs')
#' }
#'
#' data_level <- function(object) {
#'   attr(object, 'data_level')
#' }
#'
#' data_period <- function(object) {
#'   attr(object, 'data_period')
#' }
#'
#' data_months <- function(object) {
#'   attr(object, 'data_months')
#' }
#'
#' data_sources <- function(object) {
#'   attr(object, 'data_sources')
#' }
#'
#' data_categories <- function(object) {
#'   attr(object, 'data_categories')
#' }
#'
#' data_type <- function(object) {
#'   attr(object, 'data_type')
#' }
