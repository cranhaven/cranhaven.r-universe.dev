##' Export glance data to Excel Workbook
##'
##' It is similar to \code{\link{glance_data}} but instead of creating
##' a single dataframe, it creates a list of seven dataframes:
##' \describe{
##' \item{\code{all}}{Same output as \code{\link{glance_data}}.}
##' \item{\code{summary}}{A tally of columns by type. A dataframe with
##' two columns: column type and count.}
##' \item{\code{all_nas}}{Summary of columns with only NAs.}
##' \item{\code{single_value}}{Summary of columns with a single value
##' besides NAs. It might be numeric or character.}
##' \item{\code{binary}}{Summary of columns with two values besides
##' NAs. It might be numerical or categorical.}
##' \item{\code{numerical}}{Summary of all numerical columns when
##' there are more than two possible values.}
##' \item{\code{categorical}}{Summary of all categorical columns when
##' there are more than two possible values.}
##' }
##'
##' If a XLSX file name is provided, it will create a file with seven
##' sheets, corresponding to the seven dataframes above.
##'
##' Finally, the last elements of the list (and the last five sheets
##' if the filename is provided), are disjoint. That is, if a the
##' summary of a column is included in "binary", it will not be
##' included in "numerical".
##' @param dataframe Dataframe to be summarized
##' @param filename File name of the Excel file, e.g.,
##'     "mydataglance.xlsx". By default, no file name is provided and,
##'     therefore, no Excel is created.
##' @inheritParams glance_data
##' @return A list
##' @importFrom dplyr select mutate filter %>% case_when group_by
##'     tally lst
##' @importFrom openxlsx write.xlsx
##' @examples
##' ## Create a list of dataframes. If you provide the `filename`
##' ## parameter to be equal to, say, "myglance.xlsx", then it will
##' ## create an Excel workbook and place the content of each
##' ## dataframe in a separate sheet.
##' glance_data_in_workbook(iris)
##' @author Guillermo Basulto-Elias
##' @export
glance_data_in_workbook <- function(dataframe,
                                        filename = NULL,
                                        limit2tally = 20) {
    .data <- NULL

    ## All
    all <- glance_data(dataframe, limit2tally)

    ## Summary of variables
    summary  <- all %>%
        select(.data$type, .data$distinct_values) %>%
        mutate(cat =
                   case_when(
                       .data$distinct_values == 0 ~ "all nas",
                       .data$distinct_values == 1 ~ "single value",
                       .data$distinct_values == 2 ~ "binary",
                       .data$type == "numerical" ~ "numerical",
                       TRUE ~ "categorical")) %>%
        group_by(cat) %>%
        tally()

    ## All NAs
    all_nas  <- all %>%
        filter(.data$distinct_values == 0) %>%
        select(-"minimum", -"median", -"maximum", -"mean", -"sd")

    ## Single Value
    single_value <-  filter(all, .data$distinct_values == 1)

    ## Binary
    binary  <- filter(all, .data$distinct_values == 2)

    ## Numerical
    numerical <- all %>%
        filter(.data$type == "numerical" &
               .data$distinct_values > 2) %>%
        select(-.data$count)

    ## Categorical
    categorical <- all %>%
        filter(.data$type %in%  c("categorical", "factor"),
               .data$distinct_values > 2) %>%
        select(- "type", - "minimum", - "median",
               - "maximum", - "mean", - "sd")

    out <- lst(all, summary, all_nas, single_value,
               binary, numerical, categorical)

    ## Save XLSX if required
    if (!is.null(filename)) {
        if (!grepl("\\.xlsx$", filename)) {
            stop("Output file must be 'xlsx'")
        }
        write.xlsx(out, file = filename)
    }

    out

}
