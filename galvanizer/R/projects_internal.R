hb_project_one_only <- function(id1, id2){
  # at least one ID must be specified, but not both:
  
  check1 <- is.null(id1) 
  check2 <- is.null(id2) 
  
  # If check1 is not empty (FALSE) and check2 is not empty (FALSE)
  if (check1 + check2 == 0) {stop("Only one id value may be specified.", call. = FALSE)}
  
  # If check1 is empty (TRUE) and check2 is empty (TRUE)
  if (check1 + check2 == 2) {stop("At least one id value must be specified.", call. = FALSE)}
  
  return(TRUE)
}

hb_prj_set_params <- function(component, pagesize = 50, fields = NULL){
  # Creates a list of eligible parameters based on the generalized requirements
  # The check for using this should occur at the component-check level
  
  # ATTEMPT 2 - Inspired by Chel Hee Lee's advice
  # Use this method that helps force generally illegal names to be assigned to the list, which keeps its format when calling GET query later (via params)
  # Modify by attempting to force names as list name, as the substitution/eval method doesn't allow for NAs, which are possible
  # txt1 <- paste("param_test <- list(
  #   `fields[", component, "] = '", fields, "',
  #  `page[size]` = 2
  #   )")
  # 
  # print(eval(parse(text=txt1)))
  
  # Make fields friendly for download
  if (length(fields) > 0) {
    fields <- paste0(fields, collapse = ',')
  }
  
  param_list <- list()
  param_name_pagesize <- paste0('page[size]')
  param_name_fields <- paste0('fields[',component,']')
  param_list[param_name_pagesize] <- pagesize
  param_list[param_name_fields] <- fields
  
  return(param_list)
}

#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson spread_values enter_object gather_object spread_all jstring
hb_prj_parse_standard <- function(content_raw){
  # This will parse the standard data fields that exist in all get points
  
  # Suppress notes, as these are JSON elements within the returned object
  attributes <- NULL
  id <- NULL
  type <- NULL
  
  content_header <- content_raw %>%
    tidyjson::spread_values(id = tidyjson::jstring(id),
                            type = tidyjson::jstring(type)) %>%
    #  spread_all %>%
    select(.data$document.id, .data$id, .data$type) %>%
    as_tibble()
  
  content_attributes <- content_raw %>% 
    tidyjson::enter_object(attributes) %>% 
    spread_all %>%
    as_tibble()
  
  content_parts <- tibble(
    header = content_header,
    attributes = content_attributes
  )
  
  return(content_parts)
}

#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson spread_values enter_object gather_object gather_array spread_all append_values_string
hb_prj_parse_custom <- function(content_raw){
  # This parses custom only

  # Suppress notes, as these are JSON elements within the returned object
  id <- NULL
  term <- NULL
  document.id <- NULL
  value <- NULL
  attributes <- NULL
  custom_attributes <- NULL

  content_attributes_custom <- content_raw %>% 
    enter_object(attributes) %>% # Have not been able to find a way to suppress this note
    enter_object(custom_attributes)
  
  if (nrow(content_attributes_custom) == 0){return (NULL)} # Check for blanks before going
  
  content_attributes_custom <- content_attributes_custom %>%
    gather_array() %>%
    spread_values(id = jstring(id),
                  term = jstring(term)) %>%
    enter_object('value') %>%
    gather_array('value') %>%
    append_values_string("value") %>%
    as_tibble() %>%
    select(document.id, id, term, value) %>%
    nest(custom_attributes = c(id, term, value))
  
  if (nrow(content_attributes_custom) == 0){return (NULL)}
  
  return(content_attributes_custom)
}

#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson spread_values enter_object gather_object gather_array spread_all append_values_string
hb_prj_parse_tags <- function(content_raw){
  # This only processes tags
  
  # Suppress notes, as these are JSON elements within the returned object
  attributes <- NULL
  tag_list <- NULL
  
  content_tags <- content_raw %>% 
    tidyjson::enter_object(attributes) %>% 
    tidyjson::enter_object(tag_list)
  
  if (nrow(content_tags) == 0){return (NULL)} # Check for blanks before going
  
  content_tags <- content_tags %>%
    gather_array() %>% 
    append_values_string() %>%
    select(.data$document.id, tag_list = .data$string) %>% 
    as_tibble() %>%
    nest(tag_list = c(.data$tag_list))
  
  if (nrow(content_tags) == 0){return (NULL)}
  
  return(content_tags)
}

#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows count slice rename
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyjson json_types spread_values enter_object gather_object gather_array spread_all append_values_string
hb_prj_parse_rel <- function(content_raw){
  # This only processes tags
  
  # Suppress notes, as these are JSON elements within the returned object
  name <- NULL
  type <- NULL
  id <- NULL
  document.id <- NULL
  relationship_name <- NULL
  relationships <- NULL
  
  rel_check <- content_raw %>%
    tidyjson::enter_object(relationships)
  
  if (nrow(rel_check) == 0){return (NULL)} # This occurs here because select statement will fail if these columns don't exist
  
  relationships_provided <- content_raw %>%
    tidyjson::enter_object(relationships) %>%
    gather_object %>%
    json_types #%>% # Remove as it caused a bug where only the first relationship for each type was being generated
    #dplyr::count(name, type)
  
  combined_relationships <- list()
  
  for (i in 1:nrow(relationships_provided)){
    # Choose a single relationship
    one_rel <- content_raw %>%
      tidyjson::enter_object(relationships) %>%
      gather_object %>%
      slice(i) %>%
      rename(relationship_name = .data$name) %>%
      gather_object %>%
      select(-name) %>%
      json_types %>%
      rename(json_type = type)
    
    if (one_rel$json_type == 'array'){
      combined_relationships[[i]] <- one_rel %>%
        gather_array %>%
        tidyjson::spread_values(id = tidyjson::jstring(id),
                                type = tidyjson::jstring(type)) %>%
        as_tibble %>%
        select(document.id, relationship_name, id, type)
    } else if (one_rel$json_type == 'object') {
      combined_relationships[[i]] <- one_rel %>%
        tidyjson::spread_values(id = tidyjson::jstring(id),
                                type = tidyjson::jstring(type)) %>%
        as_tibble %>%
        select(document.id, relationship_name, id, type)
    } else {
      combined_relationships[[i]] <- data.frame(
        document.id = one_rel$document.id,
        relationship_name = one_rel$relationship_name,
        id = NA, 
        type = NA)
    }
  }
  
  content_rel <- bind_rows(combined_relationships)  %>%
    rename(name = relationship_name) %>%
    nest(relationships = c(name, id, type))
  
  return(content_rel)
}

#' @importFrom rlang .data
#' @importFrom dplyr select left_join
hb_prj_coljoin_data <- function(core, custom, tags, relationships){
  . <- NULL
  
  # Combine all the data together
  
  joined <- core$header %>%
    left_join(core$attributes, by = 'document.id') %>% {
      if(!is.null(custom)){
        left_join(., custom, by = 'document.id')
      } else . } %>% {
        if(!is.null(tags)){
          left_join(., tags, by = 'document.id')
        } else .} %>% {
          if(!is.null(relationships)){
            left_join(., relationships, by = 'document.id')
          } else .} %>%
    select(-.data$document.id)
  
  return(joined)
}

#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
hb_prj_get_controller <- function(auth, url, params, plural, waittime = 0.2){
  # Performs the GET and loop
  
  i <- 1 # pointer
  
  # Download the first page, specifying parameters
  download <- hb_api_get(auth, url, params = params)
  waittime_last <- Sys.time()
  json <- httr::content(download, 'text')
  content <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  
  next_page <- content$links$`next` # Save next page reference
  
  # The Plural flag needs to be set if there are multiple id and types per data source.
  # If data comes back in a $data list, then it is TRUE
  # otherwise, it comes back straight as is, FALSE

  combined_data <- hb_parse_content(content, plural)
  
  # Pagination
  while (!is.null(next_page)){
    waittime_now <- Sys.time() # Wait time is required as Highbond limits rates to about three queries per second
    waittime_elapsed <- as.numeric(difftime(waittime_now, waittime_last, units = 'secs'))
    if (waittime_elapsed < waittime){
      message("Delaying until param wait time has reached")
      Sys.sleep(waittime - waittime_elapsed)
    }
    
    combined_data_next <- NULL # Just a clean reset
    i <- i + 1 # increment to save into next page
    
    url <- paste0(hb_url_base(auth),next_page) # Use the next page as provided
    download <- hb_api_get(auth, url) # Don't use parameters anymore
    waittime_last <- Sys.time()
    
    json <- httr::content(download, 'text')
    content <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    
    next_page <- content$links$`next` # Save next page reference
    
    combined_data_next <- hb_parse_content(content, plural)

    combined_data <- bind_rows(combined_data, combined_data_next)
  }
  
  return(combined_data)
}





# All the specific data below only works with active projects

## Project based queries

#/orgs/{org_id}/projects/{project_id}/planning_files
#/orgs/{org_id}/planning_files/{id} ### -- PROJECT ID
#/orgs/{org_id}/projects/{project_id}/results_files
#/orgs/{org_id}/results_files/{id}
#/orgs/{org_id}/projects/{project_id}/objectives # THIS ACTS AS BASIS FOR OBJECTIVE BASED QUERIES
#/orgs/{org_id}/objectives/{id} 
#/orgs/{org_id}/projects/{project_id}/issues
#/orgs/{org_id}/issues/{id}

## Objective based queries (can be sections, processes, cycles, functional areas, application systems)

#/orgs/{org_id}/objectives/{objective_id}/narratives # INTERNAL CONTROLS ONLY
#/orgs/{org_id}/narratives/{id} # INTERNAL CONTROLS ONLY
#/orgs/{org_id}/objectives/{objective_id}/risks
#/orgs/{org_id}/risks/{id}
#/orgs/{org_id}/objectives/{objective_id}/controls #IC Controls, Workplan Procedures
#/orgs/{org_id}/controls/{id}  ## THIS LINKS OUT TO CONTROL_TESTS via:
## data -- relationships
### -- walkthrough, control_test_plan, control_tests, mitigations

## Controls ID relationship based queries
#/orgs/{org_id}/mitigations
#/orgs/{org_id}/control_test_plans/{id} 
#/orgs/{org_id}/walkthroughs/{id}  # IC Walkthroughts, Workplan Execute Procedure
#/orgs/{org_id}/control_tests/{id}  # INTERNAL CONTROLS ONLY

## Issue ID based queries (Issues is above)
#/orgs/{org_id}/issues/{issue_id}/actions
#/orgs/{org_id}/actions/{id}

### CAN NOT TELL HOW TO GET REQUEST ITEM BY PROJECT OR ID.
### Only one way out of requests, but you need to know the ID first
#/orgs/{org_id}/request_items/{id}

### Generic Highbond Settings
#/orgs/{org_id}/entities
#/orgs/{org_id}/entities/{id}
#/orgs/{org_id}/entity_categories
#/orgs/{org_id}/entity_categories/{id}

# data - relationships - project - data - id

# when posting, how to tell which project to associate it to?

# get_projecttype <- function(){
#   #/orgs/{org_id}/project_types
#   #/orgs/{org_id}/project_types/{id}
# }

#https://docs-apis.highbond.com/#operation/getProjects
