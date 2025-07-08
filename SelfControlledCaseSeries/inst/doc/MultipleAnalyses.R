## ----echo = FALSE, message = FALSE, warning = FALSE---------------------------
library(SelfControlledCaseSeries)
library(EmpiricalCalibration)
outputFolder <- "e:/temp/vignetteSccs2"
diclofenac <- 1124300
giBleed <- 77

## ----eval=TRUE,eval=FALSE-----------------------------------------------------
# connectionDetails <- createConnectionDetails(
#   dbms = "postgresql",
#   server = "localhost/ohdsi",
#   user = "joe",
#   password = "supersecret"
# )
# 
# outputFolder <- "s:/temp/sccsVignette2"
# 
# cdmDatabaseSchema <- "my_cdm_data"
# cohortDatabaseSchema <- "my_cohorts"
# options(sqlRenderTempEmulationSchema = NULL)

## ----eval=FALSE---------------------------------------------------------------
# giBleed <- 77
# cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(giBleed)

## ----eval=FALSE---------------------------------------------------------------
# connection <- DatabaseConnector::connect(connectionDetails)
# cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)
# CohortGenerator::createCohortTables(connection = connection,
#                                     cohortDatabaseSchema = cohortDatabaseSchema,
#                                     cohortTableNames = cohortTableNames)
# counts <- CohortGenerator::generateCohortSet(connection = connection,
#                                              cdmDatabaseSchema = cdmDatabaseSchema,
#                                              cohortDatabaseSchema = cohortDatabaseSchema,
#                                              cohortTableNames = cohortTableNames,
#                                              cohortDefinitionSet = cohortDefinitionSet)
# DatabaseConnector::disconnect(connection)

## ----eval=TRUE----------------------------------------------------------------
diclofenac <- 1124300
negativeControls <- c(
  705178, 705944, 710650, 714785, 719174, 719311, 735340, 742185,
  780369, 781182, 924724, 990760, 1110942, 1111706, 1136601,
  1317967, 1501309, 1505346, 1551673, 1560278, 1584910, 19010309,
  40163731
)
giBleed <- 77

exposuresOutcomeList <- list()
exposuresOutcomeList[[1]] <- createExposuresOutcome(
  outcomeId = giBleed,
  exposures = list(createExposure(exposureId = diclofenac))
)
for (exposureId in c(negativeControls)) {
  exposuresOutcome <- createExposuresOutcome(
    outcomeId = giBleed,
    exposures = list(createExposure(exposureId = exposureId, trueEffectSize = 1))
  )
  exposuresOutcomeList[[length(exposuresOutcomeList) + 1]] <- exposuresOutcome
}

## ----eval=TRUE----------------------------------------------------------------
getDbSccsDataArgs <- createGetDbSccsDataArgs(
  deleteCovariatesSmallCount = 100,
  exposureIds = c(),
  maxCasesPerOutcome = 100000
)

createStudyPopulationArgs <- createCreateStudyPopulationArgs(
  naivePeriod = 180,
  firstOutcomeOnly = FALSE
)

covarExposureOfInt <- createEraCovariateSettings(
  label = "Exposure of interest",
  includeEraIds = "exposureId",
  start = 1,
  end = 0,
  endAnchor = "era end",
  profileLikelihood = TRUE,
  exposureOfInterest = TRUE
)

createSccsIntervalDataArgs1 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = covarExposureOfInt
)

fitSccsModelArgs <- createFitSccsModelArgs()

## ----eval=TRUE----------------------------------------------------------------
sccsAnalysis1 <- createSccsAnalysis(
  analysisId = 1,
  description = "Simplest model",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs1,
  fitSccsModelArgs = fitSccsModelArgs
)

## ----eval=TRUE----------------------------------------------------------------
ppis <- c(911735, 929887, 923645, 904453, 948078, 19039926)

covarPreExp <- createEraCovariateSettings(
  label = "Pre-exposure",
  includeEraIds = "exposureId",
  start = -30,
  end = -1,
  endAnchor = "era start",
  preExposure = TRUE
)

covarProphylactics <- createEraCovariateSettings(
  label = "Prophylactics",
  includeEraIds = ppis,
  start = 1,
  end = 0,
  endAnchor = "era end"
)

createSccsIntervalDataArgs2 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarProphylactics
  )
)

sccsAnalysis2 <- createSccsAnalysis(
  analysisId = 2,
  description = "Including prophylactics and pre-exposure",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs2,
  fitSccsModelArgs = fitSccsModelArgs
)

seasonalitySettings <- createSeasonalityCovariateSettings(seasonKnots = 5)

calendarTimeSettings <- createCalendarTimeCovariateSettings(calendarTimeKnots = 5)

createSccsIntervalDataArgs3 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarProphylactics
  ),
  seasonalityCovariateSettings = seasonalitySettings,
  calendarTimeCovariateSettings = calendarTimeSettings
)

sccsAnalysis3 <- createSccsAnalysis(
  analysisId = 3,
  description = "Including prophylactics, season, calendar time, and pre-exposure",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs3,
  fitSccsModelArgs = fitSccsModelArgs
)

covarAllDrugs <- createEraCovariateSettings(
  label = "Other exposures",
  includeEraIds = c(),
  excludeEraIds = "exposureId",
  stratifyById = TRUE,
  start = 1,
  end = 0,
  endAnchor = "era end",
  allowRegularization = TRUE
)

createSccsIntervalDataArgs4 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarAllDrugs
  ),
  seasonalityCovariateSettings = seasonalitySettings,
  calendarTimeCovariateSettings = calendarTimeSettings
)

sccsAnalysis4 <- createSccsAnalysis(
  analysisId = 4,
  description = "Including all other drugs",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs4,
  fitSccsModelArgs = fitSccsModelArgs
)

createSccsIntervalDataArgs5 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarProphylactics
  ),
  eventDependentObservation = TRUE
)

sccsAnalysis5 <- createSccsAnalysis(
  analysisId = 5,
  description = "Adjusting for event-dependent obs. end",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs5,
  fitSccsModelArgs = fitSccsModelArgs
)

## ----eval=TRUE----------------------------------------------------------------
sccsAnalysisList <- list(
  sccsAnalysis1,
  sccsAnalysis2,
  sccsAnalysis3,
  sccsAnalysis4,
  sccsAnalysis5
)

## ----eval=TRUE----------------------------------------------------------------
sccsAnalysesSpecifications <- createSccsAnalysesSpecifications(
  sccsAnalysisList = sccsAnalysisList,
  exposuresOutcomeList = exposuresOutcomeList,
  controlType = "exposure"
)

## ----eval=FALSE---------------------------------------------------------------
# multiThreadingSettings <- createDefaultSccsMultiThreadingSettings(
#   parallel::detectCores() - 1
# )
# 
# referenceTable <- runSccsAnalyses(
#   connectionDetails = connectionDetails,
#   cdmDatabaseSchema = cdmDatabaseSchema,
#   exposureDatabaseSchema = cdmDatabaseSchema,
#   exposureTable = "drug_era",
#   outcomeDatabaseSchema = cohortDatabaseSchema,
#   outcomeTable = cohortTable,
#   outputFolder = outputFolder,
#   sccsMultiThreadingSettings = multiThreadingSettings,
#   sccsAnalysesSpecifications = sccsAnalysesSpecifications
# )

## ----eval=FALSE---------------------------------------------------------------
# sccsModelFile <- referenceTable$sccsModelFile[result$exposureId == diclofenac &
#                                                 referenceTable$outcomeId == giBleed &
#                                                 referenceTable$analysisId == 1]
# sccsModel <- readRDS(file.path(outputFolder, sccsModelFile))
# sccsModel

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (file.exists(file.path(outputFolder, "outcomeModelReference.rds"))) {
  referenceTable <- getFileReference(outputFolder)
  sccsModelFile <- referenceTable$sccsModelFile[referenceTable$exposureId == diclofenac &
                                                  referenceTable$outcomeId == giBleed &
                                                  referenceTable$analysisId == 1]
  sccsModel <- readRDS(file.path(outputFolder, sccsModelFile))
  sccsModel
}

## ----eval=FALSE---------------------------------------------------------------
# referenceTable <- getFileReference(outputFolder)

## ----eval=FALSE---------------------------------------------------------------
# resultsSum <- getResultsSummary(outputFolder)
# head(resultsSum)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (file.exists(file.path(outputFolder, "outcomeModelReference.rds"))) {
  resultsSum <- getResultsSummary(outputFolder)
  head(resultsSum)
}

## ----eval=FALSE---------------------------------------------------------------
# diagnosticsSum <- getDiagnosticsSummary(outputFolder)
# head(diagnosticsSum)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (file.exists(file.path(outputFolder, "diagnosticsSummary.rds"))) {
  diagnosticsSum <- getDiagnosticsSummary(outputFolder)
  head(diagnosticsSum)
}

## ----eval=FALSE---------------------------------------------------------------
# install.packages("EmpiricalCalibration")
# library(EmpiricalCalibration)
# 
# # Analysis 1: Simplest model
# analysis1Estimates <-  resultsSum |>
#   inner_join(diagnosticsSum) |>
#   filter(analysisId == 1 & unblind == 1)
# 
# nrow(analysis1Estimates)

## ----echo=FALSE,message=FALSE,warning=FALSE,eval=TRUE-------------------------
if (file.exists(file.path(outputFolder, "outcomeModelReference.rds"))) {
  analysis1Estimates <-  resultsSum |>
    inner_join(diagnosticsSum) |>
    filter(analysisId == 1 & unblind == 1)
  
  nrow(analysis1Estimates)
}

## ----eval=FALSE---------------------------------------------------------------
# # Analysis 2: Including prophylactics and pre-exposure
#   analysis2Estimates <-  resultsSum |>
#     inner_join(diagnosticsSum) |>
#     filter(analysisId == 2 & unblind == 1)
# 
#   nrow(analysis2Estimates)
# )

## ----echo=FALSE,message=FALSE,warning=FALSE,eval=TRUE-------------------------
if (file.exists(file.path(outputFolder, "outcomeModelReference.rds"))) {
  analysis2Estimates <-  resultsSum |>
    inner_join(diagnosticsSum) |>
    filter(analysisId == 2 & unblind == 1)
  
  nrow(analysis2Estimates)
}

## ----eval=FALSE---------------------------------------------------------------
# # Analysis 3: Including prophylactics, season, calendar time, and pre-exposure
#   analysis3Estimates <-  resultsSum |>
#     inner_join(diagnosticsSum) |>
#     filter(analysisId == 3 & unblind == 1)
# 
#   negCons <- analysis3Estimates |>
#     filter(eraId != diclofenac)
#   ei <- negCons <- analysis3Estimates |>
#     filter(eraId == diclofenac)
#   plotCalibrationEffect(
#     logRrNegatives = negCons$logRr,
#     seLogRrNegatives = negCons$seLogRr,
#     logRrPositives = ei$logRr,
#     seLogRrPositives = ei$seLogRr
#   )

## ----echo=FALSE,message=FALSE,warning=FALSE,eval=TRUE-------------------------
if (file.exists(file.path(outputFolder, "outcomeModelReference.rds"))) {
  analysis3Estimates <-  resultsSum |>
    inner_join(diagnosticsSum) |>
    filter(analysisId == 3 & unblind == 1)
  
  negCons <- analysis3Estimates |>
    filter(eraId != diclofenac)
  ei <- analysis3Estimates |>
    filter(eraId == diclofenac)
  plotCalibrationEffect(
    logRrNegatives = negCons$logRr,
    seLogRrNegatives = negCons$seLogRr,
    logRrPositives = ei$logRr,
    seLogRrPositives = ei$seLogRr
  )
}

## ----eval=FALSE---------------------------------------------------------------
# # Analysis 4: Including all other drugs
# negCons <- resultsSum[resultsSum$analysisId == 4 & resultsSum$eraId != diclofenac, ]
# ei <- resultsSum[resultsSum$analysisId == 4 & resultsSum$eraId == diclofenac, ]
# null <- fitNull(
#   negCons$logRr,
#   negCons$seLogRr
# )
# plotCalibrationEffect(
#   logRrNegatives = negCons$logRr,
#   seLogRrNegatives = negCons$seLogRr,
#   logRrPositives = ei$logRr,
#   seLogRrPositives = ei$seLogRr,
#   null
# )

## ----echo=FALSE,message=FALSE,warning=FALSE,eval=TRUE-------------------------
if (file.exists(file.path(outputFolder, "outcomeModelReference.rds"))) {
  analysis4Estimates <-  resultsSum |>
    inner_join(diagnosticsSum) |>
    filter(analysisId == 4 & unblind == 1)
  
  negCons <- analysis4Estimates |>
    filter(eraId != diclofenac)
  ei <- analysis4Estimates |>
    filter(eraId == diclofenac)
  plotCalibrationEffect(
    logRrNegatives = negCons$logRr,
    seLogRrNegatives = negCons$seLogRr,
    logRrPositives = ei$logRr,
    seLogRrPositives = ei$seLogRr
  )
}

## ----eval=FALSE---------------------------------------------------------------
# # Analysis 5: Adjusting for event-dependent obs. end
# nrow(analysis5Estimates)

## ----echo=FALSE,message=FALSE,warning=FALSE,eval=TRUE-------------------------
if (file.exists(file.path(outputFolder, "outcomeModelReference.rds"))) {
   analysis5Estimates <-  resultsSum |>
    inner_join(diagnosticsSum) |>
    filter(analysisId == 5 & unblind == 1)
  
  nrow(analysis5Estimates)
}

## ----eval=TRUE----------------------------------------------------------------
citation("SelfControlledCaseSeries")

## ----eval=TRUE----------------------------------------------------------------
citation("Cyclops")

