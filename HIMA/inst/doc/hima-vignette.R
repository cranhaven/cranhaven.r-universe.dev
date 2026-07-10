## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  echo = TRUE # Ensures all code is displayed by default
)
library(HIMA)

## ----hima-interface-----------------------------------------------------------
# hima(
#   formula,          # The model formula specifying outcome, exposure, and covariate(s)
#   data.pheno,       # Data frame with outcome, exposure, and covariate(s)
#   data.M,           # Data frame or matrix of high-dimensional mediators
#   mediator.type,    # Type of mediators: "gaussian", "negbin", or "compositional"
#   penalty = "DBlasso",  # Penalty method: "DBlasso", "MCP", "SCAD", or "lasso"
#   quantile = FALSE, # Use quantile mediation analysis (default: FALSE)
#   efficient = FALSE,# Use efficient mediation analysis (default: FALSE)
#   longitudinal = FALSE, # Enable longitudinal-survival mediation (requires Surv(tstart, tstop, status))
#   id.var = NULL,    # Subject ID column in data.pheno when longitudinal = TRUE
#   scale = TRUE,     # Scale data (default: TRUE)
#   sigcut = 0.05,    # Significance cutoff for mediator selection
#   contrast = NULL,  # Named list of contrasts for factor covariate(s)
#   subset = NULL,    # Optional subset of observations
#   verbose = FALSE   # Display progress messages (default: FALSE)
# )

## ----parallel-----------------------------------------------------------------
# hima(..., parallel = TRUE, ncore = 4)

## ----load-HIMA----------------------------------------------------------------
# library(HIMA)

## ----continuous-example-------------------------------------------------------
# data(ContinuousOutcome)
# pheno_data <- ContinuousOutcome$PhenoData
# mediator_data <- ContinuousOutcome$Mediator
# 
# hima_continuous.fit <- hima(
#   Outcome ~ Treatment + Sex + Age,
#   data.pheno = pheno_data,
#   data.M = mediator_data,
#   mediator.type = "gaussian",
#   penalty = "DBlasso",
#   scale = FALSE # Demo data is already standardized
# )
# summary(hima_continuous.fit, desc=TRUE)
# # `desc = TRUE` option to show the description of the output results

## ----efficient-example--------------------------------------------------------
# hima_efficient.fit <- hima(
#   Outcome ~ Treatment + Sex + Age,
#   data.pheno = pheno_data,
#   data.M = mediator_data,
#   mediator.type = "gaussian",
#   efficient = TRUE,
#   penalty = "MCP",
#   scale = FALSE # Demo data is already standardized
# )
# summary(hima_efficient.fit, desc=TRUE)
# # Note that the efficient HIMA is controlling FDR

## ----binary-example-----------------------------------------------------------
# data(BinaryOutcome)
# pheno_data <- BinaryOutcome$PhenoData
# mediator_data <- BinaryOutcome$Mediator
# 
# hima_binary.fit <- hima(
#   Disease ~ Treatment + Sex + Age,
#   data.pheno = pheno_data,
#   data.M = mediator_data,
#   mediator.type = "gaussian",
#   penalty = "MCP",
#   scale = FALSE # Demo data is already standardized
# )
# summary(hima_binary.fit)

## ----survival-example---------------------------------------------------------
# data(SurvivalData)
# pheno_data <- SurvivalData$PhenoData
# mediator_data <- SurvivalData$Mediator
# 
# hima_survival.fit <- hima(
#   Surv(Time, Status) ~ Treatment + Sex + Age,
#   data.pheno = pheno_data,
#   data.M = mediator_data,
#   mediator.type = "gaussian",
#   penalty = "DBlasso",
#   scale = FALSE # Demo data is already standardized
# )
# summary(hima_survival.fit)

## ----survival-longitudinal-example--------------------------------------------
# data(SurvivalLongData)
# pheno_data <- SurvivalLongData$PhenoData
# mediator_data <- SurvivalLongData$Mediator
# 
# hima_survival_long.fit <- hima(
#   Surv(Tstart, Tstop, Status) ~ Treatment + Sex + Age,
#   data.pheno = pheno_data,
#   data.M = mediator_data,
#   mediator.type = "gaussian",
#   longitudinal = TRUE,
#   id.var = "ID",
#   penalty = "lasso",
#   scale = FALSE # Demo data is already standardized
# )
# summary(hima_survival_long.fit)

## ----microbiome-example-------------------------------------------------------
# data(MicrobiomeData)
# pheno_data <- MicrobiomeData$PhenoData
# mediator_data <- MicrobiomeData$Mediator
# 
# hima_microbiome.fit <- hima(
#   Outcome ~ Treatment + Sex + Age,
#   data.pheno = pheno_data,
#   data.M = mediator_data,
#   mediator.type = "compositional",
#   penalty = "DBlasso"
# )
# summary(hima_microbiome.fit)

## ----quantile-example---------------------------------------------------------
# data(QuantileData)
# pheno_data <- QuantileData$PhenoData
# mediator_data <- QuantileData$Mediator
# 
# hima_quantile.fit <- hima(
#   Outcome ~ Treatment + Sex + Age,
#   data.pheno = pheno_data,
#   data.M = mediator_data,
#   mediator.type = "gaussian",
#   quantile = TRUE,
#   penalty = "MCP",
#   tau = c(0.3, 0.5, 0.7),
#   scale = FALSE # Demo data is already standardized
# )
# summary(hima_quantile.fit)

