---
title: "D4TAlink.light - Manual"
author: "Grégoire Thomas"
date: "October 19, 2022"
output: rmarkdown::html_vignette
vignette: |
  %\VignetteIndexEntry{D4TAlink.light - Manual} 
  %\VignetteEngine{knitr::knitr} 
  %\VignetteEncoding{UTF-8}
---




<!--ts-->
   1. [Introduction](#introduction)
   2. [Installation](#installation)
   3. [Quick start](#quick-start)
   4. [Usage](#usage)
      * [Parametrisation](#parametrisation)
      * [Analysis task](#analysis-task)
      * [Task content](#task-content)
      * [Create and render an R markdown file](#create-and-render-an-r-markdown-file)
      * [Create an R script](#create-an-r-script)
      * [Report data](#report-data)
      * [Transfer data between tasks](#transfer-data-between-tasks)
      * [Share a task](#share-a-task)
      * [R markdown and script templates](#r-markdown-and-script-templates)
      * [Change directory structure](#change-directory-structure)
<!--te-->


# 1. Introduction

[D4TAlink.light](https://bitbucket.org/SQ4/d4talink.light/) is an [R](https://www.r-project.org/)
package integrating D4TAlink's [R](https://www.r-project.org/) methods. [D4TAlink.light](https://bitbucket.org/SQ4/d4talink.light/) enables seamless 
compliance with FAIR data and ALCOA principles. 

[D4TAlink.light](https://bitbucket.org/SQ4/d4talink.light/)'s key features:

* speed up data analytics and statistics projects,
* reduce resources and lower costs, 
* enable traceability and reproducibility seamlessly, 
* unclog data analysts' life, 
* ease collaboration, 
* facilitate validation and review processes, 
* open, easy and light weight.

See also: 

1. FAIR principles: Jacobsen et al., 2017 ([doi:10.1162/dint_r_00024](https://doi.org/10.1162/dint_r_00024))
2. ALCOA principles: Food & Drug Administration, 2018 ([Data Integrity and Compliance With Drug CGMP - Questions and Answers Guidance for Industry](<https://www.regulations.gov/document/FDA-2018-D-3984-0002>)).

# 2. Installation #

Install from [CRAN](https://CRAN.R-project.org/package=D4TAlink.light): 


```r
install.packages("D4TAlink.light")
```

Install latest version from [Bitbucket](https://bitbucket.org/SQ4/d4talink.light):


```r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_bitbucket("SQ4/d4talink.light",subdir="D4TAlink.light")
```

Note that you may need to install: 
- the Bioconductor package `Biobase`  ([instructions](https://bioconductor.org/install/)), and
- Rtools ([cran.r-project.org/bin/windows](https://cran.r-project.org/bin/windows/)).

# 3. Quick start #

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

# 4. Usage #

## 4.1. Parametrisation ##

Once the R package loaded, user must set D4TAlink's global parameters, namely 
the name of the data analyst and the name of the study sponsor.


```r
library(D4TAlink.light)

setTaskAuthor("Doe Johns")
setTaskSponsor("mySponsor")
```

The location of the data file repository, must then be defined. Indeed, D4TAlink 
manages data and information in flat files within a structured directory tree. 



```r
setTaskRoot(file.path(tempdir(),"D4TAlink_example001"),dirCreate=TRUE)
```

As described further below, other parameters can be defined.


```r
setTaskRmdTemplate("/SOME/WHERE/my.Rmd")
setTaskStructure(pathsDefault)
```

Note that D4TAlink's parameters can be set via the [`.Renviron`](https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Invoking-R) 
file located in the system home directory.

```
D4TAlink_author="Dow Johns"
D4TAlink_sponsor="CompanyA"
D4TAlink_root="/SOME/WHERE/D4TAlink_example001"
D4TAlink_rmdtempl="/SOME/WHERE/my.Rmd"
D4TAlink_rscripttempl="/SOME/WHERE/my.R"
D4TAlink_pathgen="pathsDefault"
```

## 4.2. Analysis task ##

A data analysis workflow usually comprises a succession of distinct analyses tasks. 
A typical analysis workflow would comprise the following tasks:

1. exporting and loading of source data, 
2. data transformation (e.g., normalization and imputation), 
3. descriptive statistics, and 
4. statistical modelling. 

Coding these successive tasks using a single analysis script is bad practice for 
multiple reasons. Firstly, the analysis scripts become lengthy and thus difficult 
to write, review and maintain. Further, this prevents code reuse and it hinders 
project agility. Finally, this complexifies collaboration between stake holders. 

D4TAlink defines the 'analysis task' as a central concept. A data analysis workflow 
consists of a succession of tasks that could be arborescent. 

Each task is assigned to a **work package**, which is assigned to a **project**, and
each project is assigned to a **sponsor**. 

To create an analysis task in R use the following calls.


```r
# Set the base parameters 
library(D4TAlink.light)
setTaskAuthor("Doe Johns")
setTaskSponsor("mySponsor")
setTaskRoot(file.path(tempdir(),"D4TAlink_example001"),dirCreate=TRUE)

# create a task
task <- initTask(project="myProject",
                 package="myPackage",
                 taskname=sprintf("%s_myTask",format(Sys.time(),"%Y%m%d")))
print(listTasks())             
```

## 4.3. Task content ##

Each task has it's own directory structure. The task contains storage for five types of 
data: 

1. **output data**: typically, the data produced by the script in for of excel files, graphic files, ..., 
2. **source data**: local storage for the input data provided by third parties, 
3. **analysis scripts**: data analysis scripts in R, SAS, python, ..., 
4. **documentation**: documentation of the analysis task, 
5. **binary data**: output data for the task stored in binary format for follow-up task. 

The location of these data can be obtained using respectively the functions `reportDir`, 
`datasourceDir`, `progDir`, `docDir`, and `binaryDir`.   

For traceability, the files within a task have specifically the format `[TASK_NAME]_[DATA_TYPE].[EXTENSTION]`, where `DATA_TYPE` is a short string 
describing the content of the file, and `EXTENSION` the file tyle (e.g., `pdf` or 
`xlsx`). By convention `TASK_NAME` has a date as prefix with format `%Y%m%d_`, 
and `DATA_TYPE` does not contain underscores or dots, `_` or `.`.

The function `listTaskFiles` returns a list of files associated with a task: 


```r
listTaskFiles(task)
```

Similarly, the function `listTasks` returns a list of all tasks in the repository: 


```r
listTasks()
```

## 4.4. Create and render an R markdown file ##

Documentation of a task is typically authored using 
[R markdown](https://rmarkdown.rstudio.com/) files (Rmd). D4TAlink precognises having one Rmd file per task. `D4TAlink.light` provides functions to create and render these files. 

Creation of an R markdown file from template:


```r
file <- initTaskRmd(task)
print(file)
```

Rendering of the markdown file into the task documentation directory:


```r
file <- renderTaskRmd(task) # may require having run 'tinytex::install_tinytex()'
Biobase::openPDF(file)
```

## 4.5. Create an R script ##

For some tasks an R script may also be needed. A task script can be created from the default template: 


```r
file <- initTaskRscript(task)
print(file)
```

## 4.6. Report data ##

To output a report file in the output directory of a task, use the following.

**XLSX**

```r
d <- list(letters=data.frame(a=LETTERS,b=letters,c=1:length(letters)),
          other=data.frame(a=1:3,b=11:13))
file <- saveReportXls(d,task,"tables")
print(file)
```

**PDF**

```r
file <- pdfReport(task,c("plots",1),dim=c(100,100))
hist(rnorm(100))
dev.off()
Biobase::openPDF(file)
```

**PNG**

```r
file <- pngReport(task,c("plots",1),dim=c(300,300))
hist(rnorm(100))
dev.off()
print(file)
```

**JPEG**

```r
file <- jpegReport(task,c("plots",1),dim=c(300,300))
hist(rnorm(100))
dev.off()
print(file)
```

**Other**

```r
file <- reportFn(task,"someData","csv")
d <- data.frame(a=LETTERS,b=letters,c=1:length(letters))
write.table(d,file)
print(file)
```

## 4.7. Transfer data between tasks ##

Tasks each constituting an element in a stepwise analysis process. Data can be transferred 
from a task to another. To do so, R objects must be stored by the parent task 
using the call `saveBinary(object,task,"ojectType")`. The child task may then load the data 
from the parent task using the call `saveBinary(loadTask(...),"ojectType")`.  

Saving data in a parent task:


```r
d <- list(letters = data.frame(a=LETTERS,b=letters,c=1:length(letters)),
          other   = data.frame(a=1:3,b=11:13))
task <- initTask(project="myProject",
                 package="myPackage",
                 taskname="20220801_parentTask")
file <- saveBinary(d,task,"someData")
print(file)
```

Loading data from a child task: 


```r
newtask <- initTask(project="myProject",
                    package="myPackage",
                    taskname="20220801_childTask")

oldtask <- loadTask(newtask$project, newtask$package, "20220801_parentTask")
e <- readBinary(oldtask,"someData")
print(lapply(e,head))
```

## 4.8. Share a task ##

In order to share a task with coworkers, D4TAlink.light has functions to archive 
and restore tasks. This permits easily transferring data, scripts and documentation 
associated with a given task.

Note this can be used for a range of other purposes, such as transferring tasks from a 
local repository to a shared repository, and vice versa. 

Archiving a task: 


```r
setTaskRoot(file.path(tempdir(),"D4TAlink_exampleFrom"),dirCreate=TRUE)
task <- initTask(project="myProject",
                 package="myPackage",
                 taskname="20220501_myTask")
file <- tempfile(fileext=".zip")
archiveTask(task,file)
print(reportDir(task))
```

Restoring a task in a different location:


```r
setTaskRoot(file.path(tempdir(),"D4TAlink_exampleTo"),dirCreate=TRUE)
restoreTask(file)
newtask <- loadTask(project="myProject",
                    package="myPackage",
                    taskname="20220501_myTask")
print(reportDir(newtask))
```

## 4.9. R markdown and script templates ##

The R markdown and script templates can be set using the functions `setTaskRmdTemplate` and `setTaskRscriptTemplate` as follows.  


```r
setTaskRmdTemplate("/SOME/WHERE/my.Rmd")
setTaskRscriptTemplate("/SOME/WHERE/my.R")
```

The available path generation functions are `pathsDefault`, `pathsGLPG`, and `pathsPMS`.

Further, the path path th the template can be set in the [`.Renviron`](https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Invoking-R) file:

```
D4TAlink_rmdtempl="/SOME/WHERE/my.Rmd"
D4TAlink_rscripttempl="/SOME/WHERE/my.R"
```
## 4.10. Change directory structure ##

The directory structure can be customized, by creating a directory using the command `setTaskStructure` as follows.  


```r
fun <- function(project,package,taskname,sponsor) {
  basePath <- file.path("%ROOT%",sponsor,project,package)
  paths <- list(
    root = "%ROOT%",
    datasrc = file.path(basePath, "raw", "data_source"),
    data = file.path(basePath, "output","adhoc",taskname),
    bin  = file.path(basePath, "output","adhoc",taskname,"bin"),
    code = file.path(basePath, "progs"),
    doc  = file.path(basePath, "docs"),
    log  = file.path(basePath, "output","log")
  )
}

setTaskStructure(fun)
```

The available path generation functions are `pathsDefault`, `pathsGLPG`, and `pathsPMS`.

Further, the path generator can be set in the [`.Renviron`](https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Invoking-R) file, the available functions being 'pathsDefault', 'pathsGLPG', and 'pathsPMS':

```
D4TAlink_pathgen="pathsDefault"
```

