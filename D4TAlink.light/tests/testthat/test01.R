{
  ## ========================================================
  library(D4TAlink.light)
  ## ========================================================
  for(pfun in list(pathsDefault,pathsPMS)) {
    ## ============================
    ## set globals
    setTaskRoot(tempfile(),dirCreate=TRUE)
    #setTaskRoot("~/dtest1",dirCreate=TRUE)
    setTaskAuthor("Doe Johns")
    setTaskSponsor("MySponsor")
    setTaskStructure(pfun)
    ## ============================
    ## create first task
    tn <- sprintf("%s_myTask",format(Sys.time(),"%Y%m%d"))
    task <- initTask("myProject","myPackage",tn)
    ## ============================
    ## create a pdf report in first task
    file <- pdfReport(task,c("plots",1),dim=c(100,100))
    plot(pi)
    dev.off()
    if(FALSE) utils::browseURL(file)
    ## ============================
    ## create an Excel report in first task
    d <- list(letters=data.frame(a=LETTERS,b=letters,c=1:length(letters)),
              other=data.frame(a=1:3,b=11:13))
    if(FALSE) {
      file <- saveReportXls(d,task,"table")
      if(FALSE) utils::browseURL(file)
    }
    ## ============================
    ## save an R object report in first task
    saveBinary(d,task,"data")
    e <- readBinary(task,"data")
    if(!all(names(e)%in%names(d))) stop("error [1]")
    ## ============================
    ## create an R script for first task
    file <- initTaskRscript(task,overwrite=TRUE)
    ## ============================
    ## create an R markdown file for first task and render it
    initTaskRmd(task,overwrite=TRUE)
    file <- renderTaskRmd(task) # requires having run 'tinytex::install_tinytex()'
    if(FALSE) utils::browseURL(file)
    ## ============================
    ## create an R markdown file for first task with another template and render it
    setTaskRmdTemplate("SQU4RE")
    initTaskRmd(task,overwrite=TRUE)
    file <- renderTaskRmd(task) # requires having run 'tinytex::install_tinytex()'
    if(FALSE) utils::browseURL(file)
    ## ============================
    ## create an R markdown file for first task with yet another template and render it
    file <- tempfile()
    file.copy(getTaskRmdTemplate(),file)
    setTaskRmdTemplate(file)
    initTaskRmd(task,overwrite=TRUE)
    file <- renderTaskRmd(task) # requires having run 'tinytex::install_tinytex()'
    if(FALSE) utils::browseURL(file)
    ## ============================
    ## create a second task; list tasks and files
    newtask <- initTask("myProject","myPackage","20220202_otherTask")
    testthat::expect_equal(length(listTaskFiles(newtask)$bin),2)
    testthat::expect_equal(nrow(listTasks()),2)
    ## ============================
    ## archive the first task and delete the first directory
    file <- tempfile(fileext=".zip")
    archiveTask(task,file)
    unlink(getTaskRoot(),recursive=TRUE)
    ## ============================
    ## restore the first task into another repository and check it
    setTaskRoot(tempfile(),dirCreate=TRUE)
    #setTaskRoot("~/dtest2",dirCreate=TRUE)
    retask <- restoreTask(file)
    print(list.files(getTaskRoot(),recursive=TRUE,full.names=TRUE))
    testthat::expect_equal(retask$task,tn)
    ## ============================
    ## delete the second directory
    unlink(getTaskRoot(),recursive=TRUE)
  }
  ## ========================================================
}
