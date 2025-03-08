## ----options, echo = FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------

library(knitr)
opts_chunk$set(
    echo = TRUE, 
    results = 'asis', 
    warning = FALSE, 
    error = FALSE, message = FALSE, cache = FALSE,
    fig.width = 8, fig.height = 7,
    fig.path = "./figures_vignette/",
    fig.align = 'center')
options(width = 170)
options(warn = 1)#instead of warn = 0 by default -> to have place where warnings occur in the call to Sweave function


## ----loadLibraries------------------------------------------------------------------------------------------------------------------------------------------------------

library(clinDataReview)


## ----exampleReport-createClinDataReviewReportSkeleton, eval = FALSE-----------------------------------------------------------------------------------------------------
#  createClinDataReviewReportSkeleton()

## ----exampleReport-render_clinDataReviewReport, eval = FALSE------------------------------------------------------------------------------------------------------------
#  render_clinDataReviewReport()

## ----render_clinDataReviewReport, eval = FALSE--------------------------------------------------------------------------------------------------------------------------
#  
#  clinDataReview::render_clinDataReviewReport()
#  

## ----createRedirectPage, eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------
#  
#  clinDataReview::createRedirectPage()
#  

## ----convertReportToAspx, eval = FALSE----------------------------------------------------------------------------------------------------------------------------------
#  
#  clinDataReview::convertReportToAspx(reportDir = ".")
#  

## ----zipclinDataReview, eval = FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  clinDataReview::zipClinDataReview()
#  

## ----metadata, eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------
#  
#  clinDataReview::getMetadata()
#  

## ----render_clinDataReviewReport-configFiles, eval = FALSE--------------------------------------------------------------------------------------------------------------
#  # run one specific report
#  clinDataReview::render_clinDataReviewReport(configFiles = "config-AE_timeprofile.yml")
#  # only run the listings:
#  clinDataReview::render_clinDataReviewReport(configFiles = list.files(pattern = "listing", "config"))

## ----postProcessReport, eval = FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  postProcessReport()
#  

## ----render_clinDataReviewReport-devel, eval = FALSE--------------------------------------------------------------------------------------------------------------------
#  
#  # get parameters from the general 'config.yml' and the specified config file
#  params <- getParamsFromConfig(configFile = "config-AE_timeprofile.yml")
#  
#  # extract template from package if specified
#  if(params$templatePackage == "clinDataReview"){
#    pathTemplate <- clinDataReview::getPathTemplate(file = params$template)
#    file.copy(from = pathTemplate, to = ".")
#  }
#  
#  # run a current chapter (without clinical data Js libraries)
#  # Note that Js library to have the functionality to download patient profiles is not imported
#  rmarkdown::render(input = params$template)
#  
#  # preview a specific chapter (with clinical data Js libraries)
#  bookdown::render_book(input = params$template, preview = TRUE)
#  # include the index file:
#  bookdown::render_book(input = c("index.Rmd", params$template), preview = TRUE)
#  

## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
#  clinDataReview::render_clinDataReviewReport(nCores = 4)

## ----sessionInfo, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------

print(sessionInfo())


