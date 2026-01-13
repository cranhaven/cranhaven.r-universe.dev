#' Retrieve Highbond Projects - Projects
#'
#' @description Downloads the primary details of one or all projects
#'
#' @details Fields allowed: name, state, status, created_at, updated_at,
#'   description, background, budget, position, certification,
#'   control_performance, risk_assurance, management_response, max_sample_size,
#'   number_of_testing_rounds, opinion, opinion_description, purpose, scope,
#'   start_date, target_date, tag_list, project_type, entities
#'
#' @inheritParams get_results_records
#'
#' @param project_id Project ID number. \code{NULL} will default to all items.
#' @param fields OPTIONAL. A character vector each field requested within the
#'   project. NULL will default to all fields.
#' @param pagesize Defaults to 50. Maximum is 100.
#' @param waittime Time in seconds to wait between requests.
#'
#' @return A tibble of projects
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- get_projects(auth)
#' projects <- get_projects(auth, fields = c('name', 'state', 'status'))
#' }
get_projects <- function(auth, project_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- project_id
  component <- 'projects' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- is.null(primary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Projects - Project Types
#'
#' @description Downloads the primary details of one or all project types.
#'
#' @details Possible fields include: name, description, workflow, project_terms,
#'   certification_terms, control_terms, finding_terms, control_test_terms,
#'   finding_action_terms, narrative_terms, objective_terms, planning_terms,
#'   results_terms, risk_terms, test_plan_terms, walkthrough_terms
#'
#' @inheritParams get_projects
#'
#' @param project_type_id \code{NULL} Defaults to all project types
#'
#' @export
#' @return A tibble of project types
get_project_types <- function(auth, project_type_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- project_type_id
  component <- 'project_types' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- is.null(primary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Planning Files
#'
#' @description Downloads the primary details of one or multiple planning files
#'   for a project. 
#'
#' @details possible fields: name, reference_id, description, position,
#'   created_at, updated_at, custom_attributes, project
#'
#' @inheritParams get_projects
#'
#' @param project_id Required if other parameter is blank. May obtain multiple rows.
#' @param planning_file_id Required if other parameter is blank. Will get only one row.
#' 
#' @export
#' @return A tibble of planning files.
get_project_planning_files <- function(auth, project_id = NULL, planning_file_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){

  primary <- project_id
  secondary <- planning_file_id
  component <- 'planning_files' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download

  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Result Files
#'
#' @description Downloads the primary details of one or multiple result files
#'   for a project.
#'
#' @details possible fields: name, reference_id, description, position,
#'   created_at, updated_at, custom_attributes, project
#'
#' @inheritParams get_projects
#'
#' @param project_id Required if other parameter is blank. May obtain multiple rows.
#' @param results_file_id Required if other parameter is blank. Will get only one row.
#' @export
#' @return A tibble of result files
get_project_result_files <- function(auth, project_id = NULL, results_file_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- project_id
  secondary <- results_file_id
  component <- 'results_files' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Objectives
#'
#' @description Downloads the primary details of one or multiple objectives for
#'   a project. Also known as sections, processes, cycles, functional areas,
#'   application systems, or another custom term.
#'
#' @details possible fields: title, description, reference, division_department,
#'   owner, executive_owner, created_at, updated_at, project, assigned_user,
#'   custom_attributes, position, risk_control_matrix_id,
#'   walkthrough_summary_id, testing_round_1_id, testing_round_2_id,
#'   testing_round_3_id, testing_round_4_id, entities
#'
#' @inheritParams get_projects
#'
#' @param project_id Required if other parameter is blank. May obtain multiple rows.
#' @param objective_id Required if other parameter is blank. Will get only one row.
#' @export
#' @return A tibble of objectives
get_project_objectives <- function(auth, project_id = NULL, objective_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- project_id
  secondary <- objective_id
  component <- 'objectives' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Narratives
#'
#' @description "A narrative is a description of a business process or area under
#'   review. Narratives are also known as policies, process narratives, process
#'   descriptions, or control guides."
#'   
#' @details possible fields: title, description, created_at, updated_at, objective
#'
#' @inheritParams get_projects
#'
#' @param objective_id Required if other parameter is blank. May obtain multiple rows.
#' @param narrative_id Required if other parameter is blank. Will get only one row.
#' @export
#' @return A tibble of narratives
get_project_narratives <- function(auth, objective_id = NULL, narrative_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- objective_id
  secondary <- narrative_id
  component <- 'narratives' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download

  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Risks
#'
#' @description "A narrative is a description of a business process or area
#'   under review. Narratives are also known as policies, process narratives,
#'   process descriptions, or control guides."
#'
#' @details possible fields: title, description, risk_id, position, impact,
#'   likelihood, custom_attributes, created_at, updated_at, objective,
#'   mitigations, entities
#'
#' @inheritParams get_projects
#'
#' @param objective_id Required if other parameter is blank. May obtain multiple rows.
#' @param risk_id Required if other parameter is blank. Will get only one.
#' @export
#' @return A tibble of Risks
get_project_risks <- function(auth, objective_id = NULL, risk_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- objective_id
  secondary <- risk_id
  component <- 'risks' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Controls
#'
#' @description "A control is a program, policy, routine, or activity that is
#'   intended to mitigate a risk. Controls are organized by objectives, and can
#'   be associated with one or more risks. The combination of identified risks
#'   and corresponding controls defines the Risk Control Matrix. Controls are
#'   also known as procedures."
#'
#' @details possible fields: title, description, control_id, owner, frequency,
#'   control_type, prevent_detect, method, status, position, created_at,
#'   updated_at, custom_attributes, objective, walkthrough, control_test_plan,
#'   control_tests, mitigations, owner_user, entities
#'
#' @inheritParams get_projects
#'
#' @param objective_id Required if other parameter is blank. May obtain multiple rows.
#' @param control_id Required if other parameter is blank. Will get only one.
#'  
#' @export
#' @return A tibble of controls
get_project_controls <- function(auth, objective_id = NULL, control_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- objective_id
  secondary <- control_id
  component <- 'controls' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Mitigations
#'
#' @description The mapping between controls and risks
#'
#' @inheritParams get_projects
#'
#' @param mitigation_id Will get only one.
#'  
#' @export
#' @return A tibble of mitigations
get_project_mitigations <- function(auth, mitigation_id, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- mitigation_id
  component <- 'mitigations' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Test Plans
#'
#' Only applicable to test plans with Internal Control types
#'
#' @description "A test plan is a document that details how controls are
#'   assessed. Test plans identify the testing method or type of evidence
#'   obtained, specify the total sample size (split amongst testing rounds), and
#'   illustrate test steps or attributes."
#'
#' @details possible fields: testing_round_number, not_applicable, sample_size,
#'   testing_results, testing_conclusion, created_at, updated_at, control,
#'   assigned_user
#'
#' @inheritParams get_projects
#'
#' @param control_test_plan_id Will get only one.
#'
#' @export
#' @return A tibble of control test plans
get_project_control_test_plans <- function(auth, control_test_plan_id, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- control_test_plan_id
  component <- 'control_test_plans' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Control Tests
#' 
#' Only applicable to test plans with Internal Control types
#'
#' @description "Control tests evaluate the operating effectiveness of a control."
#'
#' @inheritParams get_projects
#'
#' @param control_test_id Will get only one.
#'
#' @export
#' @return A tibble of control tests
get_project_control_tests <- function(auth, control_test_id, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- control_test_id
  component <- 'control_tests' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Walkthroughs / Execute Procedures
#'
#' @description "A walkthrough is a series of steps you perform to establish the
#'   reliability of controls and test the design of controls. Each control you
#'   define has a corresponding walkthrough that is used to verify that the
#'   control is designed appropriately. In a Workplan workflow project, a
#'   walkthrough is called an execute procedure."
#'
#' @inheritParams get_projects
#'
#' @param walkthrough_id Will get only one.
#'
#' @export
#' @return A tibble of walkthroughs
get_project_walkthroughs <- function(auth, walkthrough_id, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- walkthrough_id
  component <- 'walkthroughs' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Issues
#'
#' @description "An issue is a problem, control gap, or exception that has been
#'   identified within a project. Adding an issue involves recording basic
#'   information about the issue and assigning the issue to an owner. Issues may
#'   also be known as deficiencies, findings, or another customized term."
#'
#' @details possible fields: title, description, creator_name, created_at,
#'   updated_at, position, owner, recommendation, deficiency_type, severity,
#'   published, identified_at, reference, reference_prefix, risk, scope,
#'   escalation, cause, effect, cost_impact, executive_summary, executive_owner,
#'   project_owner, closed, remediation_status, remediation_plan,
#'   remediation_date, actual_remediation_date, retest_deadline_date,
#'   actual_retest_date, retesting_results_overview, custom_attributes, project,
#'   entities
#'
#' @inheritParams get_projects
#'
#' @param project_id Required if other parameter is blank. May obtain multiple rows.
#' @param issue_id Required if other parameter is blank. Will get only one.
#' @export
#' @return A tibble of issues
get_project_issues <- function(auth, project_id = NULL, issue_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- project_id
  secondary <- issue_id
  component <- 'issues' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Actions
#'
#' @description "An action is a specific follow-up measure that is associated
#'   with an identified issue. You can add actions and assign action owners. You
#'   can also set up reminders for yourself to retest issues or track hours
#'   spent on retesting by self-assigning actions."
#'
#' @details possible fields: title, created_at, updated_at, owner_name,
#'   owner_email, send_recurring_reminder, include_issue_details,
#'   include_remediation_details, description, due_date, priority, closed,
#'   completed_date, status, submitted_on, custom_attributes, issue,
#'   assigned_by, cc_users
#'
#' @inheritParams get_projects
#'
#' @param issue_id Required if other parameter is blank. May obtain multiple rows.
#' @param action_id Required if other parameter is blank. Will get only one.
#' @export
#' @return A tibble of actions
get_project_actions <- function(auth, issue_id = NULL, action_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- issue_id
  secondary <- action_id
  component <- 'actions' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  plural <- is.null(secondary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Project - Request Items
#'
#' @description Capture what you asked for
#'
#' @inheritParams get_projects
#'
#' @param request_id Will get only one.
#'
#' @export
#' @return A tibble of walkthroughs
get_project_request_items <- function(auth, request_id, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- request_id
  component <- 'request_items' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- FALSE
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Projects - Entities
#'
#' @description Get all the entities
#'
#' @details Fields allowed: title, description, created_at, updated_at, parent, children_count, entity_category
#' 
#' @inheritParams get_projects
#'
#' @param entity_id \code{NULL} will default to all entities.
#'
#' @return A tibble of entities
#' @export
get_project_entities <- function(auth, entity_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- entity_id
  component <- 'entities' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- is.null(primary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Projects - Entity Categories
#'
#' @description Get all the entity categories
#'
#' @details Fields allowed: title, description, position, entities_count, created_at, updated_at, entities
#' 
#' @inheritParams get_projects
#'
#' @param entity_category_id \code{NULL} will default to all entity categories
#'
#' @return A tibble of entities
#' @export
get_project_entity_categories <- function(auth, entity_category_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- entity_category_id
  component <- 'entity_categories' # Set up the project
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary)) # Set up the query parameters
  plural <- is.null(primary) # Flag if its one or many. I.e. plural download
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  
  return(data) 
}

#' Retrieve Highbond Projects - Collaborators
#' 
#' @description Get the collaborators, aka user roles in Project or Framework.
#' 
#' @details Fields allowed: project, framework, user, project_role, effective_role, organization_role, created_at, updated_at, group
#' 
#' @inheritParams get_projects
#' 
#' @param project_id Required if other parameter is blank.
#' @param encoded_uid Required if the other parameter is blank. Base64 encoded parent resource id (project or framework) and user uid, encoded from format \code{parent_resource_id:user_uid.}.
#' 
#' @return A tibble of collaborators
#' @export
get_project_collaborators <- function(auth, project_id = NULL, encoded_uid = NULL, fields = NULL, pagesize = 50, waittime = 0.2){

  primary <- project_id
  secondary <- encoded_uid
  component <- 'collaborators' # Set up the project
  hb_project_one_only(primary, secondary) # Checks

  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters

  # A project id will return MULTIPLE users for one project
  # Whereas an encoded UID will return a single
  plural <- is.null(secondary)

  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data

  return(data)
}

#' Retrieve Highbond Projects - Project Type Custom Attributes
#'
#' @description Get the custom attributes set within a project type. Note these
#'   are different than the custom terms used to rename fields. For those, see
#'   \link[galvanizer]{get_project_types}
#'
#' @inheritParams get_projects
#'
#' @param project_type_id Required if other parameter is blank.
#' @param custom_attributes_id Required if the other parameter is blank. Base64
#'   encoded parent resource id (project or framework) and user uid, encoded
#'   from format \code{parent_resource_id:user_uid.}.
#'
#' @return A tibble of custom attributes
#' @export
get_project_type_custom_attributes <- function(auth, project_type_id = NULL, custom_attributes_id = NULL, fields = NULL, pagesize = 50, waittime = 0.2){
  
  primary <- project_type_id
  secondary <- custom_attributes_id
  component <- 'custom_attributes' # Set up the project
  hb_project_one_only(primary, secondary) # Checks
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary)) # Set up the query parameters
  
  plural <- is.null(custom_attributes_id) 
  
  params <- hb_prj_set_params(component, pagesize, fields) # Set up parameters
  data <- hb_prj_get_controller(auth, url, params, plural, waittime) # Download the data
  # MAY REQUIRE ENHANCEMENT DUE TO OPTIONS NESTED
  
  return(data) 
}