#' Get Organisations by Level
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `get_organisations_by_level()` is an experimental function that retrieves
#' the organisation units along with their parent units.
#'
#' @param level An integer specifying the desired organisation level (default level 1).
#' @param org_ids Optional. A vector of organisation identifiers whose details
#'   are being retrieved.
#' @param auth Optional. The authentication object
#' @param call The call environment.
#'
#' @return A tibble containing the organisation units and their parent units up
#'   to the specified level.
#'
#' @export
#'
#' @examplesIf khis_has_cred()
#' # Fetch all the organisation units metadata
#' organisations <- get_organisations_by_level(level = 2)
#' organisations

get_organisations_by_level <- function(level = 1,
                                       org_ids = NULL,
                                       auth = NULL,
                                       call = caller_env()) {

    name = parent = NULL

    check_integerish(level, call = call)
    org_levels <- check_level_supported(level, auth = auth, call = call)

    if (!is.null(org_ids)) {

        check_string_vector(org_ids, call = call)

        filters <- split(unique(org_ids), ceiling(seq_along(unique(org_ids))/500))
        orgs <- map(filters,
                    ~ get_organisation_units(id %.in% .x,
                                             level %.eq% level,
                                             fields = generate_fields_string(level),
                                             auth = auth,
                                             call = call))
        orgs <- bind_rows(orgs)

    } else {
        orgs <- get_organisation_units(level %.eq% level,
                                       fields = generate_fields_string(level),
                                       auth = auth,
                                       call = call)
    }

    if (is_empty(orgs)) {
        return (NULL)
    }

    level_name <- org_levels %>%
        filter(level == !!level) %>%
        pull(name) %>%
        tolower()

    hoist_columns <- generate_hoist_columns(level, org_levels)

    if (!is.null(hoist_columns)) {
        orgs <- orgs %>%
            hoist(parent, !!!hoist_columns) %>%
            select(-any_of('parent'))
    }

    orgs %>%
        rename_with(~ level_name, starts_with('name')) %>%
        clean_names() %>%
        relocate(id)
}

generate_fields_string <- function(level) {
    if (level <= 1) {
        return('id,name')
    }

    parent_str <- str_dup(',parent[name', level - 1)
    parent_str <- str_c(parent_str, str_dup(']', level - 1))

    return(paste0('id,name', parent_str))
}

generate_hoist_columns <- function(level, org_levels) {

    name = NULL

    # Determine the hierarchy of the levels
    levels_hierarchy <- org_levels %>%
        arrange(level) %>%
        filter(level < !!level) %>%
        pull(name) %>%
        tolower()

    # Generate the hoist column list dynamically
    if (length(levels_hierarchy) == 0) {
        return(NULL)
    }

    columns <- list2()
    current_level <- NULL
    for (lev in rev(levels_hierarchy)) {
        if (is.null(current_level)) {
            columns[[lev]] <- 'name'
            current_level <- 'parent'
        } else {
            columns[[lev]] <- list2(!!!current_level, 'name')
            current_level <- list2(!!!current_level, 'parent')
        }
    }

    return(columns)
}
