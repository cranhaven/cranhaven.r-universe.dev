# Some useful functions to get clean outputs from the databases

# Organizations -------------------------------------------------------

#' Organizations
#' 
#' Retrieve Organizations that you have access to.
#' 
#' @param id (Optional) An integer if you want information on a particular entity. Returns all entities unless this argument is provided.
#' 
#' @return A data.frame of organization information.
#'  
#' @export
get_orgs <- function(id){
  orgs <- api.get('organizations')
  
  orgs <- orgs$data
  
  if (!missing(id)) {
    
    id = as.integer(id)
    
    id = as.data.frame(id) %>%
      filter(id!='')
    
    if(nrow(id)>=1){
      orgs <- semi_join(orgs, id,
                        by = 'id')
    }
  }
  return(orgs)
}


# Buildings ---------------------------------------------------------------

#' Buildings
#' 
#' Retrieve buildings that you have access to.  
#' 
#' @inheritParams get_orgs
#' 
#' @return A data.frame of all building information.
#' 
#' @export
get_buildings <-function(id){
  
  buildings <- api.get('buildings')
  
  if(!missing(id)){
    
    id = as.integer(id)
    id = as.data.frame(id) %>%
      filter(id!='')
    
    if(nrow(id)>=1){
      
      buildings <- semi_join(buildings,id,
                             by='id')
    }
  }
  return(buildings)
}


# Users -------------------------------------------------------------------

#' Users
#' 
#' Retrieve all user info in your organization.
#' 
#' @inheritParams get_orgs
#' 
#' @return A data.frame of all user information.
#' 
#' @export

get_users <- function(id){
  
  #Get roles db
  roles <- api.get('roles')
  
  roles <- roles$data %>%
    select(.data$id, role = .data$name)
  
  #Get user db
  users <- api.get('users')
  
  #Format users
  
  users <- users$data %>%
    select(.data$id, .data$org_id, .data$org_name, .data$roles, 
           .data$email, .data$username, .data$first_name, .data$last_name, 
           .data$last_login, .data$created, .data$password_reset, 
           .data$active) %>% 
    mutate(across(c(.data$password_reset, .data$last_login, .data$created),
                  ~ as_datetime(as.numeric(substr(., 1, 10))),
                  tz = 'America/New_York')) %>% 
    mutate(across(.data$roles,
                  ~gsub('c\\(|\\)','',.))) %>%  
    mutate(across(.data$roles,
                  ~gsub(':',', ',.))) %>% 
    tidyr::separate(col = 'roles',
             into = c('role1','role2','role3','role4'),
             sep = ', ',fill = 'right') %>%
    tidyr::pivot_longer(cols = c(4:7), values_to ='role_id') %>%
    filter(!is.na(.data$role_id)) %>%
    mutate(across(.data$role_id, ~as.integer(.)))  %>%
    left_join(roles,
              by = c('role_id' = 'id')) %>% 
    select(.data$id, .data$org_id, .data$org_name, .data$role, .data$email, 
           .data$username, .data$first_name, .data$last_name, .data$last_login, 
           .data$created, .data$password_reset, .data$active)
  
  if(!missing(id)){
    id = as.integer(id)
    id = as.data.frame(id)
    
    if(nrow(id)>=1){
      users <- semi_join(users, id,
                         by = 'id')
      
    }
    
  }
  
  return(users)
}


# Deployments -------------------------------------------------------------

#' Deployments
#' 
#' Get all deployments in your organization.
#' 
#' @param org_id organization id
#' 
#' @return A data.frame of all deployments.
#' 
#' @export
get_deployments <- function(org_id){
  api_key <- api_url <- last_heartbeat <- wg_pubkey <- 
  deployments <- api.get('deployment')
  
  deployments <- deployments %>%
    mutate(across(last_heartbeat,
                  ~ as_datetime(as.numeric(substr(., 1, 10)),
                                tz = 'America/New_York'))) %>%
    select(-.data$api_key,-.data$wg_pubkey)
  
  if(!missing(org_id)){
    org_id = as.integer(org_id)
    org_id = as.data.frame(org_id)
    
    if(nrow(org_id)>=1){
      deployments <-semi_join(deployments,org_id,
                              by='org_id')
    }
  }
  
  return(deployments)
}