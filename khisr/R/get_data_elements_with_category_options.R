#' Get Data Elements with Category Options
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `get_data_elements_with_category_options()` fetches data elements metadata with the
#' category options from the DHIS2 API server.
#'
#' @param element_ids The data element identifiers whose details being retrieved
#' @param auth The authentication object
#' @param call The caller environment
#'
#' @return A tibble containing the following columns:
#'
#' * element_id - The unique identifier for the data element.
#' * element    - The name of the data element.
#' * category   - The category options for the elements
#' * category_id - The unique identifier for the category options
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#'
#' # Fetch the data element metadata for particular element id
#' elements <- get_data_elements_with_category_options('htFuvGJRW1X')
#' elements

get_data_elements_with_category_options <-function(element_ids, auth = NULL, call = caller_env()) {

    name = categoryCombo = categoryOptionCombos = co = co_name = co_id = NULL # due to NSE notes in R CMD check

    check_string_vector(element_ids, call = call)

    filter <- splice(list2(filter = NULL))
    if (!is.null(element_ids)) {
        filter <- id %.in% element_ids
    }

    data <- get_data_elements(filter,
                              fields = c('id','name','categoryCombo[categoryOptionCombos[id,name]]'),
                              auth = auth,
                              call = call)

    if (is_empty(data)) {
        return(NULL)
    }

    data  %>%
        hoist(categoryCombo, 'categoryOptionCombos') %>%
        unnest_longer(categoryOptionCombos, values_to = 'co') %>%
        unnest_wider(co, names_sep = '_') %>%
        rename(
            element_id = id,
            element = name,
            category = co_name,
            category_id = co_id
        )
}
