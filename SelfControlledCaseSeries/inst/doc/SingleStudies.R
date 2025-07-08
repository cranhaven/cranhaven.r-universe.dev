## ----echo = FALSE, message = FALSE, warning = FALSE---------------------------
library(SelfControlledCaseSeries)
outputFolder <- "e:/temp/vignetteSccs"
folderExists <- dir.exists(outputFolder)
if (folderExists) {
  options(andromedaTempFolder = "e:/andromedaTemp")
}

## ----eval=FALSE---------------------------------------------------------------
# install.packages("remotes")
# library(remotes)
# install_github("ohdsi/SelfControlledCaseSeries")

## ----eval=FALSE---------------------------------------------------------------
# connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                              server = "localhost/ohdsi",
#                                              user = "joe",
#                                              password = "supersecret")
# 
# cdmDatabaseSchema <- "my_cdm_data"
# cohortDatabaseSchema <- "my_results"
# cohortTable <- "my_cohorts"
# options(sqlRenderTempEmulationSchema = NULL)

## ----eval=FALSE---------------------------------------------------------------
# epistaxis <- 356
# cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(epistaxis)

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

## ----eval=FALSE---------------------------------------------------------------
# aspirin <- 1112807
# 
# sccsData <- getDbSccsData(
#   connectionDetails = connectionDetails,
#   cdmDatabaseSchema = cdmDatabaseSchema,
#   outcomeDatabaseSchema = cohortDatabaseSchema,
#   outcomeTable = cohortTable,
#   outcomeIds = epistaxis,
#   exposureDatabaseSchema = cdmDatabaseSchema,
#   exposureTable = "drug_era",
#   getDbSccsDataArgs = createGetDbSccsDataArgs(
#     studyStartDates = "20100101",
#     studyEndDates = "21000101",
#     maxCasesPerOutcome = 100000,
#     exposureIds = aspirin
#   )
# )
# sccsData

## ----echo=FALSE,message=FALSE,eval=TRUE---------------------------------------
aspirin <- 1112807
if (folderExists) {
  sccsData <- loadSccsData(file.path(outputFolder, "data1.zip"))
} 

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  sccsData
}

## ----eval=FALSE---------------------------------------------------------------
# summary(sccsData)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  summary(sccsData)
}

## ----eval=FALSE---------------------------------------------------------------
# saveSccsData(sccsData, "sccsData.zip")

## ----eval=FALSE---------------------------------------------------------------
# studyPop <- createStudyPopulation(
#   sccsData = sccsData,
#   outcomeId = epistaxis,
#   createStudyPopulationArgs = createCreateStudyPopulationArgs(
#     firstOutcomeOnly = FALSE,
#     naivePeriod = 180
#   )
# )

## ----eval=FALSE---------------------------------------------------------------
# getAttritionTable(studyPop)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  studyPop <- readRDS(file.path(outputFolder, "studyPop.rds"))
  getAttritionTable(studyPop)
}

## ----eval=FALSE---------------------------------------------------------------
# covarAspirin <- createEraCovariateSettings(
#   label = "Exposure of interest",
#   includeEraIds = aspirin,
#   start = 0,
#   end = 0,
#   endAnchor = "era end"
# )
# 
# sccsIntervalData <- createSccsIntervalData(
#   studyPopulation = studyPop,
#   sccsData,
#   createSccsIntervalDataArgs =  createCreateSccsIntervalDataArgs(
#     eraCovariateSettings = covarAspirin
#   )
# )
# 
# summary(sccsIntervalData)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  sccsIntervalData <- loadSccsIntervalData(file.path(outputFolder, "intervalData1.zip"))
  summary(sccsIntervalData)
}

## ----eval=FALSE---------------------------------------------------------------
# model <- fitSccsModel(
#   sccsIntervalData = sccsIntervalData,
#   fitSccsModelArgs = createFitSccsModelArgs()
# )

## ----eval=FALSE---------------------------------------------------------------
# model

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists){
  model <- readRDS(file.path(outputFolder, "simpleModel.rds"))
  model
}

## ----eval=FALSE---------------------------------------------------------------
# covarPreAspirin <- createEraCovariateSettings(
#   label = "Pre-exposure",
#   includeEraIds = aspirin,
#   start = -30,
#   end = -1,
#   endAnchor = "era start",
#   preExposure = TRUE
# )
# 
# sccsIntervalData <- createSccsIntervalData(
#   studyPopulation = studyPop,
#   sccsData,
#   createSccsIntervalDataArgs = createCreateSccsIntervalDataArgs(
#     eraCovariateSettings = list(covarAspirin, covarPreAspirin)
#   )
# )
# model <- fitSccsModel(
#   sccsIntervalData = sccsIntervalData,
#   fitSccsModelArgs = createFitSccsModelArgs()
# )

## ----eval=FALSE---------------------------------------------------------------
# model

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  model <- readRDS(file.path(outputFolder, "preExposureModel.rds"))
  model
}

## ----eval=FALSE---------------------------------------------------------------
# seasonalityCovariateSettings <- createSeasonalityCovariateSettings()
# 
# calendarTimeSettings <- createCalendarTimeCovariateSettings()
# 
# sccsIntervalData <- createSccsIntervalData(
#   studyPopulation = studyPop,
#   sccsData = sccsData,
#   createSccsIntervalDataArgs = createCreateSccsIntervalDataArgs(
#     eraCovariateSettings = list(covarAspirin,
#                                 covarPreAspirin),
#     seasonalityCovariateSettings = seasonalityCovariateSettings,
#     calendarTimeCovariateSettings = calendarTimeSettings
#   )
# )
# 
# model <- fitSccsModel(
#   sccsIntervalData = sccsIntervalData,
#   fitSccsModelArgs = createFitSccsModelArgs()
# )

## ----eval=FALSE---------------------------------------------------------------
# model

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  model <- readRDS(file.path(outputFolder, "seasonCalendarTimeModel.rds"))
  model
}

## ----eval=FALSE---------------------------------------------------------------
# plotSeasonality(model)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  plotSeasonality(model)
}

## ----eval=FALSE---------------------------------------------------------------
# plotCalendarTimeEffect(model)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  plotCalendarTimeEffect(model)
}

## ----eval=FALSE---------------------------------------------------------------
# plotEventToCalendarTime(studyPopulation = studyPop,
#                         sccsModel = model)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  plot <- readRDS(file.path(outputFolder, "stabilityPlot.rds"))
  print(plot)
}

## ----eval=FALSE---------------------------------------------------------------
# sccsData <- getDbSccsData(
#   connectionDetails = connectionDetails,
#   cdmDatabaseSchema = cdmDatabaseSchema,
#   outcomeDatabaseSchema = cohortDatabaseSchema,
#   outcomeTable = cohortTable,
#   outcomeIds = epistaxis,
#   exposureDatabaseSchema = cdmDatabaseSchema,
#   exposureTable = "drug_era",
#   getDbSccsDataArgs = createGetDbSccsDataArgs(
#     exposureIds = aspirin,
#     maxCasesPerOutcome = 100000,
#     studyStartDates = c("20100101", "20220101"),
#     studyEndDates = c("20191231", "21001231")
#   )
# )
# studyPop <- createStudyPopulation(
#   sccsData = sccsData,
#   outcomeId = epistaxis,
#   createStudyPopulationArgs = createCreateStudyPopulationArgs(
#     firstOutcomeOnly = FALSE,
#     naivePeriod = 180
#   )
# )
# sccsIntervalData <- createSccsIntervalData(
#   studyPopulation = studyPop,
#   sccsData = sccsData,
#   createSccsIntervalDataArgs = createCreateSccsIntervalDataArgs(
#     eraCovariateSettings = list(covarAspirin,
#                                 covarPreAspirin),
#     seasonalityCovariateSettings = seasonalityCovariateSettings,
#     calendarTimeCovariateSettings = calendarTimeSettings
#   )
# )
# model <- fitSccsModel(
#   sccsIntervalData = sccsIntervalData,
#   fitSccsModelArgs = createFitSccsModelArgs()
# )

## ----eval=FALSE---------------------------------------------------------------
# plotEventToCalendarTime(studyPopulation = studyPop,
#                         sccsModel = model)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  plot <- readRDS(file.path(outputFolder, "stabilityPlot2.rds"))
  print(plot)
}

## ----eval=FALSE---------------------------------------------------------------
# sccsIntervalData <- createSccsIntervalData(
#   studyPopulation = studyPop,
#   sccsData = sccsData,
#   createSccsIntervalDataArgs = createCreateSccsIntervalDataArgs(
#     eraCovariateSettings = list(covarAspirin,
#                                 covarPreAspirin),
#     eventDependentObservation = TRUE)
# )
# 
# model <- fitSccsModel(
#   sccsIntervalData = sccsIntervalData,
#   fitSccsModelArgs = createFitSccsModelArgs()
# )

## ----eval=FALSE---------------------------------------------------------------
# model

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  model <- readRDS(file.path(outputFolder, "eventDepModel.rds"))
  model
}

## ----eval=FALSE---------------------------------------------------------------
# ssris <- c(715939, 722031, 739138, 751412, 755695, 797617, 40799195)
# sccsData <- getDbSccsData(
#   connectionDetails = connectionDetails,
#   cdmDatabaseSchema = cdmDatabaseSchema,
#   outcomeDatabaseSchema = cohortDatabaseSchema,
#   outcomeTable = cohortTable,
#   outcomeIds = epistaxis,
#   exposureDatabaseSchema = cdmDatabaseSchema,
#   exposureTable = "drug_era",
#   getDbSccsDataArgs = createGetDbSccsDataArgs(
#     maxCasesPerOutcome = 100000,
#     exposureIds = c(aspirin, ssris),
#     studyStartDates = c("20100101", "20220101"),
#     studyEndDates = c("20191231", "21001231")
#   )
# )
# sccsData

## ----echo=FALSE,message=FALSE,eval=TRUE---------------------------------------
ssris <- c(715939, 722031, 739138, 751412, 755695, 797617, 40799195)
if (folderExists) {
  sccsData <- loadSccsData(file.path(outputFolder, "data2.zip"))
} 

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  sccsData
}

## ----eval=FALSE---------------------------------------------------------------
# studyPop <- createStudyPopulation(
#   sccsData = sccsData,
#   outcomeId = epistaxis,
#   createStudyPopulationArgs = createCreateStudyPopulationArgs(
#     firstOutcomeOnly = FALSE,
#     naivePeriod = 180
#   )
# )
# covarSsris <- createEraCovariateSettings(label = "SSRIs",
#                                          includeEraIds = ssris,
#                                          stratifyById = FALSE,
#                                          start = 1,
#                                          end = 0,
#                                          endAnchor = "era end")
# sccsIntervalData <- createSccsIntervalData(
#   studyPopulation = studyPop,
#   sccsData = sccsData,
#   createSccsIntervalDataArgs = createCreateSccsIntervalDataArgs(
#     eraCovariateSettings = list(covarAspirin,
#                                 covarPreAspirin,
#                                 covarSsris),
#     seasonalityCovariateSettings = seasonalityCovariateSettings,
#     calendarTimeCovariateSettings = calendarTimeSettings
#   )
# )
# 
# model <- fitSccsModel(
#   sccsIntervalData = sccsIntervalData,
#   fitSccsModelArgs = createFitSccsModelArgs()
# )

## ----eval=FALSE---------------------------------------------------------------
# model

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  model <- readRDS(file.path(outputFolder, "ssriModel.rds"))
  model
}

## ----eval=FALSE---------------------------------------------------------------
# sccsData <- getDbSccsData(
#   connectionDetails = connectionDetails,
#   cdmDatabaseSchema = cdmDatabaseSchema,
#   outcomeDatabaseSchema = cohortDatabaseSchema,
#   outcomeTable = cohortTable,
#   outcomeIds = epistaxis,
#   exposureDatabaseSchema = cdmDatabaseSchema,
#   exposureTable = "drug_era",
#   getDbSccsDataArgs = createGetDbSccsDataArgs(
#     exposureIds = c(),
#     maxCasesPerOutcome = 100000,
#     studyStartDates = c("19000101", "20220101"),
#     studyEndDates = c("20191231", "21001231")
#   )
# )

## ----eval=FALSE---------------------------------------------------------------
# studyPop <- createStudyPopulation(
#   sccsData = sccsData,
#   outcomeId = epistaxis,
#   createStudyPopulationArgs = createCreateStudyPopulationArgs(
#     firstOutcomeOnly = FALSE,
#     naivePeriod = 180
#   )
# )
# covarAllDrugs <- createEraCovariateSettings(
#   label = "Other exposures",
#   includeEraIds = c(),
#   excludeEraIds = aspirin,
#   stratifyById = TRUE,
#   start = 1,
#   end = 0,
#   endAnchor = "era end",
#   allowRegularization = TRUE
# )
# sccsIntervalData <- createSccsIntervalData(
#   studyPopulation = studyPop,
#   sccsData = sccsData,
#   createSccsIntervalDataArgs = createCreateSccsIntervalDataArgs(
#     eraCovariateSettings = list(covarAspirin,
#                                 covarPreAspirin,
#                                 covarAllDrugs),
#     seasonalityCovariateSettings = seasonalityCovariateSettings,
#     calendarTimeCovariateSettings = calendarTimeSettings
#   )
# )
# model <- fitSccsModel(
#   sccsIntervalData = sccsIntervalData,
#   fitSccsModelArgs = createFitSccsModelArgs()
# )

## ----eval=FALSE---------------------------------------------------------------
# estimates <- getModel(model)
# estimates[estimates$originalEraId == aspirin, ]

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  model <- readRDS(file.path(outputFolder, "allDrugsModel.rds"))
  estimates <- getModel(model)
  estimates[estimates$originalEraId == aspirin, ]
}

## ----eval=FALSE---------------------------------------------------------------
# estimates[estimates$originalEraId %in% ssris, ]

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  estimates[estimates$originalEraId %in% ssris, ]
}

## ----eval=FALSE---------------------------------------------------------------
# checkRareOutcomeAssumption(studyPop)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  checkRareOutcomeAssumption(studyPopulation = studyPop)
}

## ----eval=FALSE---------------------------------------------------------------
# checkEventExposureIndependenceAssumption(sccsModel = model)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  checkEventExposureIndependenceAssumption(sccsModel = model)
}

## ----eval=FALSE---------------------------------------------------------------
# checkEventObservationIndependenceAssumption(sccsModel = model)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  checkEventObservationIndependenceAssumption(sccsModel = model)
}

## ----eval=FALSE---------------------------------------------------------------
# checkTimeStabilityAssumption(studyPopulation = studyPop,
#                              sccsModel = model)

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  readRDS(file.path(outputFolder, "stability2.rds"))
}

## ----eval=FALSE---------------------------------------------------------------
# computeMdrr(sccsIntervalData,
#             exposureCovariateId = 1000,
#             alpha = 0.05,
#             power = 0.8,
#             twoSided = TRUE,
#             method = "binomial")

## ----echo=FALSE,message=FALSE-------------------------------------------------
if (folderExists) {
  computeMdrr(sccsIntervalData,
              exposureCovariateId = 1000,
              alpha = 0.05,
              power = 0.8,
              twoSided = TRUE,
              method = "binomial")
}

## ----eval=FALSE---------------------------------------------------------------
# plotExposureCentered(studyPop, sccsData, exposureEraId = aspirin)

## ----echo=FALSE,message=FALSE,warning=FALSE,fig.width=6.5, fig.height=5-------
if (folderExists) {
  plotExposureCentered(studyPop, sccsData, exposureEraId = aspirin)
}

## ----eval=FALSE---------------------------------------------------------------
# plotAgeSpans(studyPop)

## ----echo=FALSE,message=FALSE,warnings=FALSE----------------------------------
if (folderExists) {
  plotAgeSpans(studyPop)
}

## ----eval=TRUE----------------------------------------------------------------
citation("SelfControlledCaseSeries")

## ----eval=TRUE----------------------------------------------------------------
citation("Cyclops")

