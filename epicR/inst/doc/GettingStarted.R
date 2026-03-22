## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE  # Set to FALSE to prevent long simulations during package check
)

## ----installation, eval=FALSE-------------------------------------------------
# # Install from GitHub
# pak::pkg_install("resplab/epicR")

## ----setup--------------------------------------------------------------------
# library(epicR)

## ----simple-simulate, eval=FALSE----------------------------------------------
# # Run with defaults - that's it!
# results <- simulate()
# 
# # Access basic results
# print(results$basic)
# 
# # Custom parameters
# results <- simulate(
#   jurisdiction = "us",
#   time_horizon = 10,
#   n_agents = 100000
# )
# 
# # Quick test with fewer agents (faster for testing)
# results <- simulate(n_agents = 10000)
# 
# # By default, you get both basic and extended results
# results <- simulate()
# print(results$basic)
# print(results$extended)  # Included by default
# 
# # Get basic output only (faster, less memory)
# results <- simulate(extended_results = FALSE)
# 
# # Get event history (automatically sets record_mode)
# results <- simulate(return_events = TRUE)
# head(results$events)

## ----jurisdiction-------------------------------------------------------------
# # For Canadian population (default)
# results_canada <- simulate(jurisdiction = "canada")
# 
# # For US population
# results_us <- simulate(jurisdiction = "us")

## ----explore_inputs-----------------------------------------------------------
# inputs <- get_input()
# 
# # Top-level structure
# names(inputs)
# # [1] "values" "help" "references"
# 
# # Value categories
# names(inputs$values)
# 
# # Example: global parameters
# names(inputs$values$global_parameters)
# inputs$values$global_parameters$time_horizon  # Simulation duration in years

## ----modify_inputs_simple-----------------------------------------------------
# # Change time horizon
# results <- simulate(time_horizon = 20)
# 
# # Change jurisdiction and time horizon
# results <- simulate(jurisdiction = "us", time_horizon = 15)
# 
# # For quick testing
# results <- simulate(n_agents = 10000, time_horizon = 5)

## ----explore_inputs_advanced--------------------------------------------------
# # Explore available inputs
# input <- get_input()
# names(input$values)  # See categories
# 
# # View specific parameters
# input$values$cost$exac_dcost  # Exacerbation costs by severity
# input$values$global_parameters$time_horizon

## ----settings-----------------------------------------------------------------
# settings <- get_default_settings()
# names(settings)

## ----n_agents-----------------------------------------------------------------
# # Quick test run (10,000 agents)
# results <- simulate(n_agents = 1e4)
# 
# # Standard run (60,000 agents - default)
# results <- simulate()
# 
# # Production run (1,000,000 agents)
# results <- simulate(n_agents = 1e6)
# 
# # Check memory requirements before running large simulations
# estimate_memory_required(n_agents = 1e6, record_mode = 0, time_horizon = 20)

## ----extended_output----------------------------------------------------------
# # By default, you get both basic and extended results
# results <- simulate()
# 
# # Access basic results
# print(results$basic)
# 
# # Access detailed output tables
# names(results$extended)

## ----individual_data----------------------------------------------------------
# # Get event history (automatically sets record_mode = 2)
# # Keep n_agents small due to memory requirements
# results <- simulate(
#   n_agents = 1e4,
#   time_horizon = 5,
#   return_events = TRUE
# )
# 
# # Access events data frame
# head(results$events)
# 
# # Get everything including events
# results <- simulate(
#   n_agents = 1e4,
#   extended_results = TRUE,  # TRUE by default
#   return_events = TRUE
# )
# # Returns: results$basic, results$extended, results$events

## ----closed_cohort------------------------------------------------------------
# # Run closed cohort analysis
# results <- simulate(closed_cohort = TRUE)
# 
# # Combine with other parameters
# results <- simulate(
#   closed_cohort = TRUE,
#   jurisdiction = "us",
#   time_horizon = 10,
#   n_agents = 50000
# )

## ----scenarios----------------------------------------------------------------
# # Baseline scenario
# results_baseline <- simulate(
#   jurisdiction = "canada",
#   time_horizon = 20,
#   n_agents = 100000
# )
# 
# # Intervention scenario (e.g., different time horizon or jurisdiction)
# results_intervention <- simulate(
#   jurisdiction = "us",
#   time_horizon = 20,
#   n_agents = 100000
# )
# 
# # Compare outcomes
# cost_diff <- results_intervention$basic$total_cost -
#   results_baseline$basic$total_cost
# qaly_diff <- results_intervention$basic$total_qaly -
#   results_baseline$basic$total_qaly
# icer <- cost_diff / qaly_diff

## ----advanced_inputs----------------------------------------------------------
# # Get and modify inputs
# input <- get_input()
# 
# # Modify specific parameters
# input$values$global_parameters$time_horizon <- 5
# input$values$agent$p_female <- 0.55
# 
# # Run with custom inputs
# results <- simulate(input = input$values)

## ----multiple_sims------------------------------------------------------------
# # Simulation 1
# results1 <- simulate(n_agents = 50000, time_horizon = 10, seed = 123)
# 
# # Simulation 2 with different parameters
# results2 <- simulate(n_agents = 50000, time_horizon = 20, seed = 123)
# 
# # Compare results
# comparison <- data.frame(
#   time_horizon = c(10, 20),
#   n_deaths = c(results1$basic$n_deaths, results2$basic$n_deaths)
# )

## ----error_handling_advanced--------------------------------------------------
# results <- tryCatch({
#   simulate(n_agents = 50000)
# }, error = function(e) {
#   message("Simulation failed: ", e$message)
#   NULL
# })

