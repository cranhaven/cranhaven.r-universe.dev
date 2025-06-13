PupilPreEnv <- new.env()

.onLoad <- function(...) {
  # Check R version
if(getRversion() >= "2.15.1")
  # Global vars
  utils::globalVariables(c("is_NA", "isNA", "NNA", "PercentNA",
  "NBaseline", "event", "Pupil", "Gaze_X", "Gaze_Y", "PupilClean", "Pupil_Previous",
  "Baseline", "In_Blink", "TRIAL_INDEX", "ContainsBlink", "Blink",
  "N_Blinks", "N_Trials", "BaselinePeriod", "CleanedPupil", "values",
  "MahaOutlier", "InArtifValue", "end", "start", "InBlinkValue",
  "Preview", "is_same", "Gaze_X_Previous", "Gaze_Y_Previous",
  "Compared", "Percent", "Column", "PUPIL", "Datapoint", "Missing", "SD",
  "N", "PupilPlot", "SE", "left", "right", "Mean", "Lev1", "Lev2",
  "Columnmean", "binomial", "RIGHT_PUPIL_SIZE", "LEFT_PUPIL_SIZE",
  "RIGHT_VELOCITY_X", "LEFT_VELOCITY_X", "RIGHT_VELOCITY_Y", "LEFT_VELOCITY_Y",
  "RIGHT_ACCELERATION_X", "LEFT_ACCELERATION_X", "RIGHT_ACCELERATION_Y",
  "LEFT_ACCELERATION_Y", "RIGHT_IN_BLINK", "LEFT_IN_BLINK", "RIGHT_IN_SACCADE",
  "LEFT_IN_SACCADE", "Eye", "Pupil_Data", "Data", ".", "Cond", "Cond1", "CustCond1",
  "CustCond2", "DC1m", "DC1sd", "DC2m", "DC2sd", "DS", "Diff", "EYE_TRACKED",
  "Event", "EyeRecorded", "EyeSelected", "IA", "Item", "LEFT_GAZE_X",
  "LEFT_GAZE_Y", "Present", "RIGHT_GAZE_X", "RIGHT_GAZE_Y", "Subject",
  "Time", "VALUE", "Variable", "degfree", "error_lower", "error_upper",
  "meanDiff", "n1", "n2", "sd", "se", "BaselinePercentNA", "BaselineQuality",
  "CriticalQuality", "CrititalPercentNA", "FilterSkip", "Filtered",
  "InterpPupilSkip", "InterpXYSkip", "Original", "PupilRevert", "Screen",
  "Trimmed", "XRevert", "YRevert", "density"), add = FALSE)
}


.onAttach <- function(...) {
	name <- utils::packageDescription("PupilPre", fields = c("Package", "Version"))[[1]]
	version <- utils::packageDescription("PupilPre", fields = c("Package", "Version"))[[2]]
	hello <- paste("This is ", name, " version ", version, ". See NEWS for important changes. \nFor package information, type 'help(package=\"PupilPre\")'.", " ", "\nTo cite this package, type 'citation(package=\"PupilPre\")'.",sep="")
    packageStartupMessage(hello)
}

