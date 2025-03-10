---
title: "D4TAlink.light - Quick start"
author: "Grégoire Thomas"
date: "October 17, 2022"
output: rmarkdown::html_vignette
vignette: |
  %\VignetteIndexEntry{D4TAlink.light - Quick start} 
  %\VignetteEngine{knitr::knitr} 
  %\VignetteEncoding{UTF-8}
---



1. Load [D4TAlink.light](https://bitbucket.org/SQ4/d4talink.light/)

```r
if (!require("D4TAlink.light", quietly = TRUE)) install.packages("D4TAlink.light")
library(D4TAlink.light)
```

2. Parametrise 

```r
setTaskAuthor("Doe Johns")
setTaskSponsor("myClient")
setTaskRoot("~/myDataRepository", dirCreate = TRUE)
```

3. Create two tasks (```package``` refers here to a _work package_)

```r
task1 <- initTask(project = "DiseaseABC", 
                  package = "myStudy", 
                  taskname = "2022-09-01_myFirstAnalysis")
task2 <- initTask(project = "DiseaseABC", 
                  package = "myStudy", 
                  taskname = "2022-09-05_mySecondAnalysis")
```

4. List the tasks in repository

```r
print(listTasks())             
```

5. Load a task from the repository 

```r
mytask <- loadTask(project = "DiseaseABC", 
                  package = "myStudy", 
                  taskname = "2022-09-05_mySecondAnalysis")
```

6. Add data to a task

```r
d <- list(letters = data.frame(a = LETTERS, b = letters, c = 1:length(letters)), 
          other = data.frame(a = 1:3, b = 11:13))
saveBinary(d, mytask, "myTables")
```

7. Load data from a task

```r
e  <- readBinary(mytask, "myTables")
```

8. Add reports to a task

```r
excelfilename <- saveReportXls(d, mytask, "tables")

pdffilename <- pdfReport(mytask, "myPlot", dim = c(150, 150)) # 150mm x 150mm
plot(pi)
dev.off()

csvfile <- reportFn(mytask, "someData", "csv")
p <- data.frame(a = LETTERS, b = letters, c = 1:length(letters))
write.table(p, csvfile)
print(csvfile)
```

9. Add R markdown file from template to a task 

```r
rmdfile <- initTaskRmd(mytask)
print(rmdfile)
```

10. Render a task's R markdown file

```r
# May require having run 'tinytex::install_tinytex()'
docfile <- renderTaskRmd(mytask) 
if (require("Biobase", quietly = TRUE)) Biobase::openPDF(docfile)
```

11. List content of task 

```r
print(listTaskFiles(mytask))
```

