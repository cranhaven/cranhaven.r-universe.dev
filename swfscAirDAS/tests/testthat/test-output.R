test_that("airdas_read output has expected column names and classes", {
  y.read <- airdas_read(system.file("airdas_sample.das", package = "swfscAirDAS"))
  y.read2 <- as_airdas_dfr(data.frame(y.read))
  
  # Same as in as_airdas_dfr()
  exp.class <- list(
    Event = "character",
    EffortDot = "logical",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    EventNum = "character",
    file_das = "character",
    line_num = "integer",
    file_type = "character"
  )
  
  expect_identical(exp.class, lapply(y.read, class))
  expect_identical(exp.class, lapply(y.read2, class))
})


test_that("airdas_process output has expected column names and classes", {
  y.proc <- airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))
  y.proc2 <- as_airdas_df(data.frame(y.proc))
  
  # Same as in as_airdas_df()
  exp.class <- list(
    Event = "character",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    OnEffort = "logical",
    Trans = "character",
    Bft = "numeric",
    CCover = "numeric",
    Jelly = "numeric",
    HorizSun = "numeric",
    VertSun = "numeric",
    HKR = "character",
    Haze = "logical",
    Kelp = "logical",
    RedTide = "logical",
    AltFt = "numeric",
    SpKnot = "numeric",
    ObsL = "character",
    ObsB = "character",
    ObsR = "character",
    Rec = "character",
    VLI = "character",
    VLO = "character",
    VB = "character",
    VRI = "character",
    VRO = "character",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    EffortDot = "logical", 
    EventNum = "character",
    file_das = "character",
    line_num = "integer",
    file_type = "character"
  )
  
  expect_identical(exp.class, lapply(y.proc, class))
  expect_identical(exp.class, lapply(y.proc2, class))
})


test_that("airdas_sight output has expected column names and classes", {
  y.proc <- airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))
  y.sight <- airdas_sight(y.proc)
  
  exp.name <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort", 
    "Trans", "Bft", "CCover", "Jelly", "HorizSun", "VertSun", 
    "HKR", "Haze", "Kelp", "RedTide", "AltFt", "SpKnot",  
    "ObsL", "ObsB", "ObsR", "Rec", "VLI", "VLO", "VB", "VRI", "VRO", 
    "EffortDot", "EventNum", "file_das", "line_num", "file_type", 
    "SightNo", "Obs", "Angle", "ObsStd", "SightStd", 
    "Mixed", "SpCode", "GsTotal", "GsSp", 
    "TurtleSize", "TurtleDirection", "TurtleTail"
  )
  
  expect_identical(exp.name, names(y.sight))
})


test_that("airdas_sight output has expected column names and classes with an extra column", {
  y.proc <- airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))
  y.proc$testrr <- 4
  y.sight <- airdas_sight(y.proc)
  
  exp.name <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort", 
    "Trans", "Bft", "CCover", "Jelly", "HorizSun", "VertSun", 
    "HKR", "Haze", "Kelp", "RedTide", "AltFt", "SpKnot",  
    "ObsL", "ObsB", "ObsR", "Rec", "VLI", "VLO", "VB", "VRI", "VRO", 
    "EffortDot", "EventNum", "file_das", "line_num", "file_type", "testrr", 
    "SightNo", "Obs", "Angle", "ObsStd", "SightStd", 
    "Mixed", "SpCode", "GsTotal", "GsSp", 
    "TurtleSize", "TurtleDirection", "TurtleTail"
  )
  
  expect_identical(exp.name, names(y.sight))
})

