y.read <- das_read(system.file("das_sample.das", package = "swfscDAS"))
y.proc <- das_process(y.read)


exp.proc.name <- c(
  "Event", "DateTime", "Lat", "Lon", "OnEffort",
  "Cruise", "Mode", "OffsetGMT", "EffType", "ESWsides", "Course", "SpdKt",
  "Bft", "SwellHght", "WindSpdKt",
  "RainFog", "HorizSun", "VertSun", "Glare", "Vis",
  "ObsL", "Rec", "ObsR", "ObsInd",
  "EffortDot", "EventNum", "file_das", "line_num"
)

exp.sight.name <- c(
  "SightNo", "Subgroup", "SightNoDaily", "Obs", "ObsStd", "Bearing", "Reticle", "DistNm",
  "Cue", "Method", "Photos", "Birds", "CalibSchool", "PhotosAerial", "Biopsy",
  "Prob", "nSp", "Mixed"
)

exp.sight.name.default <- c(
  "SpCode", "SpCodeProb",
  "GsSchoolBest", "GsSchoolHigh", "GsSchoolLow",
  "GsSpBest", "GsSpHigh", "GsSpLow",
  "CourseSchool",
  "TurtleJFR", "TurtleAge", "TurtleCapt", "PerpDistKm"
)

names.wide <- c(
  "GsSpBest1", "GsSpBest2", "GsSpBest3", "GsSpBest4",
  "GsSpHigh1", "GsSpHigh2", "GsSpHigh3", "GsSpHigh4",
  "GsSpLow1", "GsSpLow2", "GsSpLow3", "GsSpLow4"
)
exp.sight.name.wide <- c(
  "ObsEstimate", "SpCode1", "SpCode2","SpCode3", "SpCode4",
  "SpCodeProb1", "SpCodeProb2", "SpCodeProb3", "SpCodeProb4",
  "SpPerc1", "SpPerc2", "SpPerc3", "SpPerc4",
  "GsSchoolBest", "GsSchoolHigh", "GsSchoolLow",
  names.wide, "CourseSchool",
  "TurtleSp", "TurtleGs", "TurtleJFR", "TurtleAge", "TurtleCapt",
  "PinnipedSp", "PinnipedGs", "BoatType", "BoatGs", "PerpDistKm"
)
exp.sight.name.complete <- setdiff(exp.sight.name.wide, names.wide)


test_that("das_read output has expected column names and classes", {
  y.read2 <- as_das_dfr(data.frame(y.read))

  # Same as in as_das_dfr()
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
    Data8 = "character",
    Data9 = "character",
    Data10 = "character",
    Data11 = "character",
    Data12 = "character",
    EventNum = "character",
    file_das = "character",
    line_num = "integer"
  )

  expect_identical(exp.class, lapply(y.read, class))
  expect_identical(exp.class, lapply(y.read2, class))
})


test_that("das_process output has expected column names and classes", {
  y.proc2 <- as_das_df(data.frame(y.proc))

  # Same as in as_das_df()
  exp.class <- list(
    Event = "character",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    OnEffort = "logical",
    Cruise = "numeric",
    Mode = "character",
    OffsetGMT = "integer",
    EffType = "character",
    ESWsides = "numeric",
    Course = "numeric",
    SpdKt = "numeric",
    Bft = "numeric",
    SwellHght = "numeric",
    WindSpdKt = "numeric",
    RainFog = "numeric",
    HorizSun = "numeric",
    VertSun = "numeric",
    Glare = "logical",
    Vis = "numeric",
    ObsL = "character",
    Rec = "character",
    ObsR = "character",
    ObsInd = "character",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    Data8 = "character",
    Data9 = "character",
    Data10 = "character",
    Data11 = "character",
    Data12 = "character",
    EffortDot = "logical",
    EventNum = "character",
    file_das = "character",
    line_num = "integer"
  )

  expect_identical(exp.class, lapply(y.proc, class))
  expect_identical(exp.class, lapply(y.proc2, class))
})


test_that("das_sight output has expected column names and classes", {
  y.sight <- das_sight(y.proc)
  y.sight.wide <- das_sight(y.proc, return.format = "wide")
  y.sight.complete <- das_sight(y.proc, return.format = "complete")

  expect_identical(c(exp.proc.name, exp.sight.name, exp.sight.name.default),
                   names(y.sight))
  expect_identical(c(exp.proc.name, exp.sight.name, exp.sight.name.wide),
                   names(y.sight.wide))
  expect_identical(c(exp.proc.name, exp.sight.name, exp.sight.name.complete),
                   names(y.sight.complete))
})


test_that("das_sight output has expected column names and classes with extra column", {
  y.proc$testrr <- 4
  y.sight <- das_sight(y.proc)
  y.sight.wide <- das_sight(y.proc, return.format = "wide")
  y.sight.complete <- das_sight(y.proc, return.format = "complete")

  expect_identical(c(exp.proc.name, "testrr", exp.sight.name, exp.sight.name.default),
                   names(y.sight))
  expect_identical(c(exp.proc.name, "testrr", exp.sight.name, exp.sight.name.complete),
                   names(y.sight.complete))

})
