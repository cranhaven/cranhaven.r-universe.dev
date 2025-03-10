library(D4TAlink.light)

# Load task
task <- loadTask(project="%PROJECT%", package="%PACKAGE%", taskname="%TASK%")
# if task does not exist on repository, create it
if(is.null(task)) {
  task <- initTask(project="%PROJECT%",
                   package="%PACKAGE%",
                   taskname="%TASK%",
                   sponsor="%SPONSOR%",
                   author="%AUTHOR%")
}

# Load dependencies
library(stats)

## =================================================== ##
## == Input ========================================== ##

# Code to load input data

# ta <- loadTask(task$project,task$package,"20220102_loadAssays)
# ass <- readBinary(ta,"assays")

## =================================================== ##
## == Process ======================================== ##

# Chunk that contains the analysis R code.

## =================================================== ##
## == Output ========================================= ##


# Code to save data

# saveBinary(ass,ta,"assays")

## =================================================== ##
## == Session information ============================ ##

# In the appendix the session information is being printed for reproducibility.

print(sessionInfo())

