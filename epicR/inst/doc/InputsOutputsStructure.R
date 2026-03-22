## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

oldopt <- options(rmarkdown.html_vignette.check_title = FALSE)
on.exit(options(oldopt))


## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Get default inputs for Canada (default)
library(epicR)
input <- get_input()

# Get inputs for US
input_us <- get_input(jurisdiction = "us")

# The function returns a nested list with three components:
# - values: The actual parameter values used in simulation
# - help: Descriptions of what each parameter means
# - ref: Literature references for each parameter

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Example: Modify global parameters
input <- get_input()
input$values$global_parameters$time_horizon <- 10
input$values$global_parameters$discount_cost <- 0.015
input$values$global_parameters$discount_qaly <- 0.015

results <- simulate(input = input$values)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Example: Change population sex distribution
input <- get_input()
input$values$agent$p_female <- 0.52  # 52% female

results <- simulate(input = input$values)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Example: Increase smoking cessation rate
input <- get_input()
# The first coefficient is the intercept - increasing it increases cessation rate
input$values$smoking$ln_h_ces_betas[1] <- input$values$smoking$ln_h_ces_betas[1] + 0.5

results <- simulate(input = input$values)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Example: Reduce exacerbation rate
input <- get_input()
# Reduce the baseline rate (first coefficient is intercept)
input$values$exacerbation$ln_rate_betas[1] <- input$values$exacerbation$ln_rate_betas[1] - 0.2

results <- simulate(input = input$values)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Example: Enable case detection program
input <- get_input()
input$values$diagnosis$case_detection_start_end_yrs <- c(0, 20)  # Active years 0-20
input$values$diagnosis$p_case_detection <- rep(0.1, 20)  # 10% annual probability

results <- simulate(input = input$values)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Example: View and modify costs
input <- get_input()

# View default exacerbation costs
print(input$values$cost$exac_dcost)
# Columns: mild, moderate, severe, very_severe

# Double the cost of severe exacerbations
input$values$cost$exac_dcost[3] <- input$values$cost$exac_dcost[3] * 2

results <- simulate(input = input$values)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Example: View utility values
input <- get_input()

# Background utilities by stage
print(input$values$utility$bg_util_by_stage)
# Typical values: No COPD=0.86, GOLD1=0.81, GOLD2=0.72, GOLD3=0.68, GOLD4=0.58

# Exacerbation disutility matrix
print(input$values$utility$exac_dutil)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
input <- get_input()

# Get help text for a parameter
input$help$exacerbation$ln_rate_betas
# Returns: "Regression coefficients for the random-effects log-hazard model..."

# Get literature reference
input$ref$smoking$mortality_factor_current
# Returns: "Meta-analysis. doi:10.1001/archinternmed.2012.1397"

# List all help for a category
names(input$help$cost)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Complete workflow example
input <- get_input(jurisdiction = "canada")

# Modify multiple parameters
input$values$global_parameters$time_horizon <- 10
input$values$agent$p_female <- 0.52
input$values$smoking$ln_h_ces_betas[1] <- input$values$smoking$ln_h_ces_betas[1] + 0.3

# Run simulation
results <- simulate(input = input$values, n_agents = 50000)

# Access results
print(results$basic)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
results <- simulate(n_agents = 50000)

# Access basic results
basic <- results$basic
cat("Total agents:", basic$n_agents, "\n")
cat("Total deaths:", basic$n_deaths, "\n")
cat("Mild exacerbations:", basic$total_exac[1], "\n")
cat("Moderate exacerbations:", basic$total_exac[2], "\n")
cat("Severe exacerbations:", basic$total_exac[3], "\n")
cat("Very severe exacerbations:", basic$total_exac[4], "\n")
cat("Total cost:", basic$total_cost, "\n")
cat("Total QALYs:", basic$total_qaly, "\n")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
results <- simulate(n_agents = 50000)

# Access extended results
extended <- results$extended

# Calculate COPD prevalence over time
copd_prevalence <- rowSums(extended$n_COPD_by_ctime_sex) /
                   rowSums(extended$n_alive_by_ctime_sex)
plot(copd_prevalence, type = "l",
     xlab = "Year", ylab = "COPD Prevalence",
     main = "COPD Prevalence Over Time")

# Calculate annual exacerbation rates by severity
annual_exac_rates <- extended$n_exac_by_ctime_severity /
                     rowSums(extended$n_COPD_by_ctime_sex)

# Analyze costs by GOLD stage
costs_by_gold <- colSums(extended$cumul_cost_gold_ctime)
names(costs_by_gold) <- c("No COPD", "GOLD 1", "GOLD 2", "GOLD 3", "GOLD 4")
barplot(costs_by_gold, main = "Total Costs by GOLD Stage")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Run simulation with event recording
results <- simulate(
  n_agents = 10000,
  return_events = TRUE
)

# Access events data frame
events <- results$events
head(events)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Analyze events for a specific agent
results <- simulate(n_agents = 10000, return_events = TRUE)
events <- results$events

# Get all events for agent #5
agent_5_events <- events[events$id == 5, ]
print(agent_5_events[, c("local_time", "event", "gold", "exac_status")])

# Count events by type
event_counts <- table(events$event)
names(event_counts) <- c("Start", "Annual", "Birthday", "Smoking",
                         "COPD", "Exac", "Exac_end", "Exac_death",
                         "Doctor", "Med_change", "", "", "",
                         "Bgd_death", "End")[as.numeric(names(event_counts)) + 1]
print(event_counts)

# Find all exacerbation events
exac_events <- events[events$event == 5, ]
cat("Total exacerbations:", nrow(exac_events), "\n")
cat("By severity:\n")
print(table(exac_events$exac_status))

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Using settings directly
settings <- get_default_settings()
settings$record_mode <- 2  # Full event recording
settings$n_base_agents <- 10000

results <- simulate(settings = settings, return_events = TRUE)

# Or simply use return_events = TRUE (automatically sets record_mode = 2)
results <- simulate(n_agents = 10000, return_events = TRUE)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
# Baseline scenario
input_baseline <- get_input(jurisdiction = "canada")
results_baseline <- simulate(
  input = input_baseline$values,
  n_agents = 50000,
  seed = 12345  # For reproducibility
)

# Intervention scenario: Enhanced smoking cessation
input_intervention <- get_input(jurisdiction = "canada")
input_intervention$values$smoking$ln_h_ces_betas[1] <-
  input_intervention$values$smoking$ln_h_ces_betas[1] + 0.5

results_intervention <- simulate(
  input = input_intervention$values,
  n_agents = 50000,
  seed = 12345  # Same seed for comparability
)

# Calculate incremental cost-effectiveness
delta_cost <- results_intervention$basic$total_cost - results_baseline$basic$total_cost
delta_qaly <- results_intervention$basic$total_qaly - results_baseline$basic$total_qaly
icer <- delta_cost / delta_qaly

cat("Baseline total cost:", results_baseline$basic$total_cost, "\n")
cat("Intervention total cost:", results_intervention$basic$total_cost, "\n")
cat("Baseline total QALYs:", results_baseline$basic$total_qaly, "\n")
cat("Intervention total QALYs:", results_intervention$basic$total_qaly, "\n")
cat("Incremental cost:", delta_cost, "\n")
cat("Incremental QALYs:", delta_qaly, "\n")
cat("ICER:", icer, "per QALY\n")

