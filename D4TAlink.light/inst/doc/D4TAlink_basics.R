## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# NA

## ----eval=FALSE---------------------------------------------------------------
# install.packages("D4TAlink.light")

## ----eval=FALSE---------------------------------------------------------------
# if (!require("devtools", quietly = TRUE))
#     install.packages("devtools")
# devtools::install_bitbucket("SQ4/d4talink.light",subdir="D4TAlink.light")

## ----eval=FALSE---------------------------------------------------------------
# if (!require("D4TAlink.light", quietly = TRUE)) install.packages("D4TAlink.light")
# library(D4TAlink.light)

## ----eval=FALSE---------------------------------------------------------------
# setTaskAuthor("Doe Johns")
# setTaskSponsor("myClient")
# setTaskRoot("~/myDataRepository", dirCreate = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# task1 <- initTask(project = "DiseaseABC",
#                   package = "myStudy",
#                   taskname = "2022-09-01_myFirstAnalysis")
# task2 <- initTask(project = "DiseaseABC",
#                   package = "myStudy",
#                   taskname = "2022-09-05_mySecondAnalysis")

## ----eval=FALSE---------------------------------------------------------------
# print(listTasks())

## ----eval=FALSE---------------------------------------------------------------
# mytask <- loadTask(project = "DiseaseABC",
#                   package = "myStudy",
#                   taskname = "2022-09-05_mySecondAnalysis")

## ----eval=FALSE---------------------------------------------------------------
# d <- list(letters = data.frame(a = LETTERS, b = letters, c = 1:length(letters)),
#           other = data.frame(a = 1:3, b = 11:13))
# saveBinary(d, mytask, "myTables")

## ----eval=FALSE---------------------------------------------------------------
# e  <- readBinary(mytask, "myTables")

## ----eval=FALSE---------------------------------------------------------------
# excelfilename <- saveReportXls(d, mytask, "tables")
# 
# pdffilename <- pdfReport(mytask, "myPlot", dim = c(150, 150)) # 150mm x 150mm
# plot(pi)
# dev.off()
# 
# csvfile <- reportFn(mytask, "someData", "csv")
# p <- data.frame(a = LETTERS, b = letters, c = 1:length(letters))
# write.table(p, csvfile)
# print(csvfile)

## ----eval=FALSE---------------------------------------------------------------
# rmdfile <- initTaskRmd(mytask)
# print(rmdfile)

## ----eval=FALSE---------------------------------------------------------------
# # May require having run 'tinytex::install_tinytex()'
# docfile <- renderTaskRmd(mytask)
# if (require("Biobase", quietly = TRUE)) Biobase::openPDF(docfile)

## ----eval=FALSE---------------------------------------------------------------
# print(listTaskFiles(mytask))

## ----eval=FALSE---------------------------------------------------------------
# library(D4TAlink.light)
# 
# setTaskAuthor("Doe Johns")
# setTaskSponsor("mySponsor")

## ----eval=FALSE---------------------------------------------------------------
# setTaskRoot(file.path(tempdir(),"D4TAlink_example001"),dirCreate=TRUE)

## ----eval=FALSE---------------------------------------------------------------
# setTaskRmdTemplate("/SOME/WHERE/my.Rmd")
# setTaskStructure(pathsDefault)

## ----eval=FALSE---------------------------------------------------------------
# # Set the base parameters
# library(D4TAlink.light)
# setTaskAuthor("Doe Johns")
# setTaskSponsor("mySponsor")
# setTaskRoot(file.path(tempdir(),"D4TAlink_example001"),dirCreate=TRUE)
# 
# # create a task
# task <- initTask(project="myProject",
#                  package="myPackage",
#                  taskname=sprintf("%s_myTask",format(Sys.time(),"%Y%m%d")))
# print(listTasks())

## ----eval=FALSE---------------------------------------------------------------
# listTaskFiles(task)

## ----eval=FALSE---------------------------------------------------------------
# listTasks()

## ----eval=FALSE---------------------------------------------------------------
# file <- initTaskRmd(task)
# print(file)

## ----eval=FALSE---------------------------------------------------------------
# file <- renderTaskRmd(task) # may require having run 'tinytex::install_tinytex()'
# Biobase::openPDF(file)

## ----eval=FALSE---------------------------------------------------------------
# file <- initTaskRscript(task)
# print(file)

## ----eval=FALSE---------------------------------------------------------------
# d <- list(letters=data.frame(a=LETTERS,b=letters,c=1:length(letters)),
#           other=data.frame(a=1:3,b=11:13))
# file <- saveReportXls(d,task,"tables")
# print(file)

## ----eval=FALSE---------------------------------------------------------------
# file <- pdfReport(task,c("plots",1),dim=c(100,100))
# hist(rnorm(100))
# dev.off()
# Biobase::openPDF(file)

## ----eval=FALSE---------------------------------------------------------------
# file <- pngReport(task,c("plots",1),dim=c(300,300))
# hist(rnorm(100))
# dev.off()
# print(file)

## ----eval=FALSE---------------------------------------------------------------
# file <- jpegReport(task,c("plots",1),dim=c(300,300))
# hist(rnorm(100))
# dev.off()
# print(file)

## ----eval=FALSE---------------------------------------------------------------
# file <- reportFn(task,"someData","csv")
# d <- data.frame(a=LETTERS,b=letters,c=1:length(letters))
# write.table(d,file)
# print(file)

## ----eval=FALSE---------------------------------------------------------------
# d <- list(letters = data.frame(a=LETTERS,b=letters,c=1:length(letters)),
#           other   = data.frame(a=1:3,b=11:13))
# task <- initTask(project="myProject",
#                  package="myPackage",
#                  taskname="20220801_parentTask")
# file <- saveBinary(d,task,"someData")
# print(file)

## ----eval=FALSE---------------------------------------------------------------
# newtask <- initTask(project="myProject",
#                     package="myPackage",
#                     taskname="20220801_childTask")
# 
# oldtask <- loadTask(newtask$project, newtask$package, "20220801_parentTask")
# e <- readBinary(oldtask,"someData")
# print(lapply(e,head))

## ----eval=FALSE---------------------------------------------------------------
# setTaskRoot(file.path(tempdir(),"D4TAlink_exampleFrom"),dirCreate=TRUE)
# task <- initTask(project="myProject",
#                  package="myPackage",
#                  taskname="20220501_myTask")
# file <- tempfile(fileext=".zip")
# archiveTask(task,file)
# print(reportDir(task))

## ----eval=FALSE---------------------------------------------------------------
# setTaskRoot(file.path(tempdir(),"D4TAlink_exampleTo"),dirCreate=TRUE)
# restoreTask(file)
# newtask <- loadTask(project="myProject",
#                     package="myPackage",
#                     taskname="20220501_myTask")
# print(reportDir(newtask))

## ----eval=FALSE---------------------------------------------------------------
# setTaskRmdTemplate("/SOME/WHERE/my.Rmd")
# setTaskRscriptTemplate("/SOME/WHERE/my.R")

## ----eval=FALSE---------------------------------------------------------------
# fun <- function(project,package,taskname,sponsor) {
#   basePath <- file.path("%ROOT%",sponsor,project,package)
#   paths <- list(
#     root = "%ROOT%",
#     datasrc = file.path(basePath, "raw", "data_source"),
#     data = file.path(basePath, "output","adhoc",taskname),
#     bin  = file.path(basePath, "output","adhoc",taskname,"bin"),
#     code = file.path(basePath, "progs"),
#     doc  = file.path(basePath, "docs"),
#     log  = file.path(basePath, "output","log")
#   )
# }
# 
# setTaskStructure(fun)

