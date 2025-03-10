## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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

