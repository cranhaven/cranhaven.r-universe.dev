check_api <- function(){
  key <- Sys.getenv('highbond_openapi') 
  if (!nzchar(key)){
    skip('API not available')
  }
}

test_that("Highbond Projects - GET projects (many)", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  highbond_fields <- c('name', 'state', 'status', 'created_at', 'updated_at', 'description', 'background', 'budget', 'position', 'certification', 'control_performance', 'risk_assurance', 'management_response', 'max_sample_size', 'number_of_testing_rounds', 'opinion', 'opinion_description', 'purpose', 'scope', 'start_date', 'target_date', 'tag_list', 'project_type', 'entities', 'collaborators')
  
  projects <- get_projects(hb_creds, project_id = NULL, pagesize = 2, fields = highbond_fields)
  
  expect_true(nrow(projects) > 1)
})

test_that("Highbond Projects - Relationships exist for more than first project", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  highbond_fields <- c('name', 'state', 'status', 'created_at', 'updated_at', 'description', 'background', 'budget', 'position', 'certification', 'control_performance', 'risk_assurance', 'management_response', 'max_sample_size', 'number_of_testing_rounds', 'opinion', 'opinion_description', 'purpose', 'scope', 'start_date', 'target_date', 'tag_list', 'project_type', 'entities', 'collaborators')
  
  projects <- get_projects(hb_creds, project_id = NULL, pagesize = 2, fields = highbond_fields)
  
  no_relationships <- length(which(sapply(projects$relationships, is.null)))

  expect_true(no_relationships == 0)
})

test_that("Highbond Projects - GET projects (one)", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  project_id <- Sys.getenv('prj_project_workflow_id')
  projects <- get_projects(hb_creds, project_id = project_id)
  
  expect_true(nrow(projects) == 1)
})

test_that("Highbond Projects - GET project types", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_types(hb_creds, NULL)
  b <- get_project_types(hb_creds, a$id[1], fields = c('name', 'description', 'workflow')) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET project planning files", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_planning_files(hb_creds, project_id = Sys.getenv('prj_project_workflow_id')) 
  b <- get_project_planning_files(hb_creds, planning_file_id = a$id[1]) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET project result files", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_result_files(hb_creds, project_id = Sys.getenv('prj_project_workflow_id'))
  b <- get_project_result_files(hb_creds, results_file_id = a$id[1]) 
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET objectives", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_objectives(hb_creds, project_id = Sys.getenv('prj_project_workflow_id'))
  b <- get_project_objectives(hb_creds, objective_id = a$id[1]) 
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET narratives", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_objectives(hb_creds, project_id = Sys.getenv('prj_project_sox_id'))
  b <- get_project_narratives(hb_creds, objective_id = a$id[1]) 
  c <- get_project_narratives(hb_creds, narrative_id = b$id[1]) 
    
  expect_true(nrow(b) >= 1)
  expect_true(nrow(c) >= 1)
})

test_that("Highbond Projects - GET risks", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_objectives(hb_creds, project_id = Sys.getenv('prj_project_workflow_id'))
  b <- get_project_risks(hb_creds, objective_id = a$id[1]) 
  c <- get_project_risks(hb_creds, risk_id = b$id[1]) 
    
  expect_true(nrow(b) >= 1)
  expect_true(nrow(c) >= 1)
})

test_that("Highbond Projects - GET controls", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_objectives(hb_creds, project_id = Sys.getenv('prj_project_workflow_id'))
  b <- get_project_controls(hb_creds, objective_id = a$id[1]) 
  c <- get_project_controls(hb_creds, control_id = b$id[1]) 
    
  expect_true(nrow(b) >= 1)
  expect_true(nrow(c) >= 1)
})

test_that("Highbond Projects - Controls - Check Arrays", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  fields <- c('title', 'description', 'control_id', 'owner', 'frequency', 'control_type', 'prevent_detect', 'method', 
                       'status', 'position', 'created_at', 'updated_at', 'custom_attributes', 'objective', 'walkthrough', 'control_test_plan', 
                       'control_tests', 'mitigations', 'entities', 'owner_user')
  
  a <- get_project_objectives(hb_creds, project_id = Sys.getenv('prj_project_sox_id'))
  b <- get_project_controls(hb_creds, fields = fields, objective_id = a$id[1]) 
  
  # control_tests, mitigations, and entities should be arrays
  
  b_count <- b$relationships[[1]] %>% 
    dplyr::filter(name == 'control_tests')
  
  expect_true(nrow(b_count) == 2) # interim/final phases
})

test_that("Highbond Projects - GET control test plans", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  fields <- c('title', 'description', 'control_id', 'owner', 'frequency', 'control_type', 'prevent_detect', 'method', 
              'status', 'position', 'created_at', 'updated_at', 'custom_attributes', 'objective', 'walkthrough', 'control_test_plan', 
              'control_tests', 'mitigations', 'entities', 'owner_user')
  
  a <- get_project_objectives(hb_creds, project_id = Sys.getenv('prj_project_sox_id'))
  b <- get_project_controls(hb_creds, fields = fields, objective_id = a$id[1]) 
  
  b_relationships <- b$relationships[[1]] %>% 
    dplyr::filter(name == 'control_test_plan')
  
  c <- get_project_control_test_plans(hb_creds, control_test_plan_id = b_relationships$id[1])
    
  expect_true(nrow(c) >= 1)
})

test_that("Highbond Projects - GET control tests", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  fields <- c('title', 'description', 'control_id', 'owner', 'frequency', 'control_type', 'prevent_detect', 'method', 
              'status', 'position', 'created_at', 'updated_at', 'custom_attributes', 'objective', 'walkthrough', 'control_test_plan', 
              'control_tests', 'mitigations', 'entities', 'owner_user')
  
  a <- get_project_objectives(hb_creds, project_id = Sys.getenv('prj_project_sox_id'))
  b <- get_project_controls(hb_creds, fields = fields, objective_id = a$id[1]) 
  
  b_relationships <- b$relationships[[1]] %>% 
    dplyr::filter(name == 'control_tests')
  
  c <- get_project_control_tests(hb_creds, control_test_id = b_relationships$id[1])

  expect_true(nrow(c) >= 1)
})

test_that("Highbond Projects - GET walkthroughs", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  fields <- c('title', 'description', 'control_id', 'owner', 'frequency', 'control_type', 'prevent_detect', 'method', 
              'status', 'position', 'created_at', 'updated_at', 'custom_attributes', 'objective', 'walkthrough', 'control_test_plan', 
              'control_tests', 'mitigations', 'entities', 'owner_user')

  a <- get_project_objectives(hb_creds, project_id = Sys.getenv('prj_project_sox_id'))
  b <- get_project_controls(hb_creds, fields = fields, objective_id = a$id[1]) 
  
  b_relationships <- b$relationships[[1]] %>% 
    dplyr::filter(name == 'walkthrough')
  
  c <- get_project_walkthroughs(hb_creds, walkthrough_id = b_relationships$id[1])
    
  expect_true(nrow(c) >= 1)
})

test_that("Highbond Projects - GET issues", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_issues(hb_creds, project_id = Sys.getenv('prj_project_workflow_id'))
  b <- get_project_issues(hb_creds, issue_id = a$id[1])
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET actions", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_issues(hb_creds, project_id = Sys.getenv('prj_project_workflow_id'))
  b <- get_project_actions(hb_creds, issue_id= a$id[1]) 
  c <- get_project_actions(hb_creds, action_id = b$id[1])
  
  expect_true(nrow(b) >= 1)
  expect_true(nrow(c) >= 1)
})

test_that("Highbond Projects - GET request items", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_request_items(hb_creds, request_id = Sys.getenv('prj_project_request_id'))

  expect_true(nrow(a) >= 1)
})

test_that("Highbond Projects - GET entities", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_entities(hb_creds, NULL)
  b <- get_project_entities(hb_creds, entity_id = Sys.getenv('prj_project_entities_id'))
    
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET entity categories", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_entity_categories(hb_creds, NULL)
  b <- get_project_entity_categories(hb_creds, entity_category_id = Sys.getenv('prj_project_entity_categories_id'))
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})

test_that("Highbond Projects - GET Project Type Custom Attributes", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  a <- get_project_type_custom_attributes(hb_creds, project_type_id = Sys.getenv('prj_project_types_id'))
  b <- get_project_type_custom_attributes(hb_creds, custom_attributes_id = a$id[1])
  
  expect_true(nrow(a) >= 1)
  expect_true(nrow(b) >= 1)
})


test_that("Highbond Projects - GET collaborators", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))

  a <- get_project_collaborators(hb_creds, project_id = Sys.getenv('prj_project_workflow_id'))
  #b <- get_project_collaborators(hb_creds, encoded_uid = 'MTQ3OTcwOndwaGpwUHNfUS02ZVI3VlBBUm1G')

  expect_true(nrow(a) >= 1)
  #expect_true(nrow(b) >= 1)
})