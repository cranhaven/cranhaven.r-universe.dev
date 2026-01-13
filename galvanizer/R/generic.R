# Documentation for API at: https://docs-apis.highbond.com/

#' Highbond Authentication credentials
#' 
#' Assembles all the core authentication needed to connect to a Highbond instance.
#' 
#' @details 
#' 
#'   Requires a Highbond API token. Activate a token from
#'   \url{https://help.highbond.com/helpdocs/highbond/en-us/Content/launchpad/getting_started/managing_access_tokens.html}.
#'
#'   The Instance (Organization) number and datacenter can both be found from the Highbond launchpad, then accessing Options and Organization.
#'   \url{https://accounts.highbond.com/orgs/<ORG_ID>/details}
#'
#' @param apikey Highbond API token
#' @param instance The HighBond instance ID, also known as the organization number
#' @param datacenter The region code. Can be \code{'us', 'ca', 'eu', 'ap', 'au'}
#'
#' @return A Highbond Authentication credentials object to pass to your requests
#' @export
setup_highbond <- function(apikey, instance, datacenter){

  # Check data center id
  `%!in%` = Negate(`%in%`)
  
  if (datacenter %!in% c('us', 'ca', 'eu', 'ap', 'au')){
    stop("Unknown data center region.")
  }
  
  # Make sure the org ID is a number
  if (as.numeric(instance) != instance){
    stop("Instance/Organization should be a numeric value.")
  }
  
  # Create stable class of highbond authentication object
  structure(
    list(key = apikey,
         org = instance,
         dc = datacenter),
    class = 'hb_auth')
}

hb_checkauth <- function(auth){
  stopifnot(class(auth) == 'hb_auth')
}

hb_api_get <- function(auth, url, params = NULL){
  # The function that actually pulls Highbond data
  hb_checkauth(auth)
  
  apikey <- auth$key
  
  skip_retry <- TRUE
  
  while(skip_retry){
    if (is.null(params)){
      hb_api_get <- httr::GET(url, 
                              hb_headers(auth))
    } else {
      # We have to do this because having a params of blank but passing a fully encoded url will clear all the parameters
      hb_api_get <- httr::GET(url, 
                            hb_headers(auth),
                            query = params)
    }
    
    # Force a retry if it fails due to time out. These two catches will bypass the validate, preventing a complete stop
    if (httr::status_code(hb_api_get) == 429){
      message("Rate limit exceeded, waiting to retry")
      Sys.sleep(0.1)
    } else if (httr::status_code(hb_api_get) == 504){
      message("Gateway timeout, waiting to retry")
      Sys.sleep(0.2)
    } 
    else {
      check_download <- hb_validateDownload(hb_api_get)
      if (check_download){skip_retry <- FALSE}
    }
  }
  return(hb_api_get)
}

hb_headers <- function(auth){
  hb_checkauth(auth)
  
  apikey <- auth$key
  
  hb_headers <- httr::add_headers(Authorization = paste("Bearer", apikey),
                            `Content-Type` = 'application/vnd.api+json',
                            `Accept-Encoding` = '')
  
  return(hb_headers) 
}

hb_validateDownload <- function(content){
  httr::warn_for_status(content)
  httr::stop_for_status(content)
  
  if (httr::http_type(content) != 'application/json' & httr::http_type(content) != 'application/vnd.api+json'){
    stop("API did not return application/json or application/vnd.api+json", call. = FALSE)
  }
  
  return(TRUE)
}

hb_url <- function(auth){
  # See https://docs-apis.highbond.com/#section/Making-requests
  hb_checkauth(auth)
  
  regionurl <- hb_url_base(auth) # Moved to enable hb_url_base functionality to accomodate projects. Same syntax kept to allow for Results to continue without re-engineering
  
  url <- paste0(regionurl, '/v1/orgs/', auth$org, '/')
  
  return(url)
}

hb_url_base <- function(auth){
  # This needs to be separate due to needing a base URL for Projects and next page handler
  hb_checkauth(auth)

  datacenter <- auth$dc
    
  if (datacenter == 'us'){
    regionurl <- 'https://apis.highbond.com'
  } else if (datacenter == 'ca'){
    regionurl <- 'https://apis-ca.highbond.com'
  } else if (datacenter == 'eu'){
    regionurl <- 'https://apis-eu.highbond.com'
  } else if (datacenter == 'ap'){
    regionurl <- 'https://apis-ap.highbond.com'
  } else if (datacenter == 'au'){
    regionurl <- 'https://apis-au.highbond.com'
  } else {
    message("Unspecified datacenter - defaulting to 'us'")
    regionurl <- 'https://apis.highbond.com'
  }
  
  return(regionurl)
}

api_jsonParseDf <- function(apicontent){
  # Flattens data into a list
  
  parsed <- jsonlite::fromJSON(httr::content(apicontent, 'text'),
                               simplifyVector = FALSE,
                               simplifyDataFrame = TRUE,
                               flatten = TRUE)
  
  return(parsed)
}

hb_url_component <- function(component, primary = NULL, secondary = NULL, primary_parent = NULL){
  # Builds the second part of the URL, the resource itself
  # This needs to break out GET A and GET ALL 
  
  # Primary parent for projects/framworks, very limited use case currently
  # Primary is the primary ID of the resource - for example, a high level project number
  # Secondary is the secondary ID of the resource - for example, specific files or controls
  
  # Future - perhaps put plural switch here?
  
  url <- NULL
  
  ### PART ONE - Substitution based on components.
  # These are components that are considered top level - i.e. /orgs/{org_id}/projects
  
  if (component %in% c('projects', 'project_types', 'entities', 'entity_categories', 'collections')){
    if (is.null(primary)) {
      url <- paste0(component, '/') # Get all projects
    } else {
      url <- paste0(component, '/', primary) # Get one specific project
    }
    
    return(url) # Early return
  }
  
  # These components have no secondary component
  if(component %in% c('control_test_plans', 'walkthroughs', 'control_tests', 'mitigations', 'request_items')){
    url <- paste0(component, '/', primary)
    
    return(url) # Early return
  }
  
  if (is.null(primary) & is.null(secondary)) # If the project or objective isn't specified, then at least one of the components needs to be
  {
    stop("Primary and secondary IDs must be specified for the requested component")
  }
  
  ### PART TWO - Components that belong to a specific parent
  
  # Projects - Project based components
  # FUTURE These may be switched as well with a 'framework' parent component - Planning Files, Objectives, Collaborators
  if(component %in% c('planning_files', 'results_files', 'objectives', 'issues', 'collaborators')){
    if (is.null(secondary)){
      if (is.null(primary_parent)){
        primary_parent <- 'projects'
      }
      url <- paste0(primary_parent, '/', primary, '/', component)
    } else {
      url <- paste0(component, '/', secondary)
    }
  }
  
  # Projects - Objective based components
  # FUTURE Narrative projects must be Internal Control
  if(component %in% c('narratives', 'risks', 'controls')){
    if (is.null(secondary)){
      url <- paste0('objectives/', primary, '/', component)
    } else {
      url <- paste0(component, '/', secondary)
    }
  }
  
  # Projects - Issue based components
  if(component %in% c('actions')){
    if (is.null(secondary)){
      url <- paste0('issues/', primary, '/', component)
    } else {
      url <- paste0(component, '/', secondary)
    }
  }
  
  # Projects - Project Types based components
  if(component %in% c('custom_attributes')){
    if (is.null(secondary)){
      url <- paste0('project_types/', primary, '/', component)
    } else {
      url <- paste0(component, '/', secondary)
    }
  }
  
  # Results - Collections-based components
  
  if(component %in% c('analyses')){
    if (is.null(secondary)){
      url <- paste0('collections/', primary, '/', component)
    } else {
      url <- paste0(component, '/', secondary)
    }
  }
  
  # Results - Analyses-based components
  
  if(component %in% c('tables')){
    if (is.null(secondary)){
      url <- paste0('analyses/', primary, '/', component)
    } else {
      url <- paste0(component, '/', secondary)
    }
  }
  
  # Results - Tables-based components
  
  if(component %in% c('columns', 'records')){
    if (is.null(secondary)){
      url <- paste0('tables/', primary, '/', component)
    } else {
      stop("Results Columns and Records should be specified with a 'primary' table reference.")
    }
  }
  
  # Final check
  if (is.null(url)){
    stop("Something broke, check if component/parent/primary/secondary matches")
  }
  
  return(url)
}

hb_parse_content <- function(content, plural){
  content_data <- if(plural){content$data} else {content} # This is important for many
  
  if (length(content_data) == 0){
    warning('No data returned. ')
    return(NULL)
  }
  
  core <- hb_prj_parse_standard(content_data) # Returns the three primary tables in all - header, attributes, relationships
  
  # Custom fields get - relevant to most except...
  custom <- hb_prj_parse_custom(content_data)
  tags <- hb_prj_parse_tags(content_data)
  relationships <- hb_prj_parse_rel(content_data)
  
  # First page, finished
  combined_data <- hb_prj_coljoin_data(core, custom, tags, relationships)
}


