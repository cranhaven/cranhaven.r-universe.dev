##############################
## Utils
##############################

#' Get configuration data from remote URL
#' 
#' @return A list of project configurations
get_config_data <- function() {
    url <- "https://raw.githubusercontent.com/Jiefei-Wang/EpochData/refs/heads/main/config.json"
    
    repos <- osf_retrieve_node("v9qyb")
    config_json <- repos$meta[[1]][["attributes"]][["description"]]
    config <- fromJSON(config_json)
    config <- config[[.config_version]]
    config
}



##############################
## functions
##############################

#' Update downloader repositories configuration
#' 
#' @description
#' Manually update the project list from the remote repository.
#' This function will attempt to download the latest configuration
#' from the GitHub repository. If it fails, the current configuration
#' will remain unchanged.
#' 
#' @param verbose Logical indicating whether to show messages
#' @return `updateRepos`: No return value, called for side effects.
#' @rdname EpochDownloader-method
#' @export
updateRepos <- function(verbose = FALSE) {
    .message(verbose, "Attempting to update project repository configuration...")

    config <- get_config_data()
    pkg_global$.project_list <- config
    .message(verbose, 
    "Success. Available projects: ", paste(names(pkg_global$.project_list), collapse = ", "))
    
}

#' Get the list of available projects
#' 
#' @description
#' This function returns the list of available projects.
#' If the configuration has not been fetched yet, it will
#' automatically update the project list from the remote repository.
#' @return `EpochRepos`: A list of project names and their corresponding OSF project IDs.
#' @examples EpochRepos()
#' @rdname EpochDownloader-method
#' @export
EpochRepos <- function(verbose = TRUE) {
    if (!pkg_global$.config_fetched) {
        .message(verbose, "Configuration not fetched yet. Fetching now...")
        updateRepos(verbose = verbose) 
        pkg_global$.config_fetched <- TRUE
    }else{
        .message(verbose, "Configuration already fetched.")
    }
    pkg_global$.project_list
}
