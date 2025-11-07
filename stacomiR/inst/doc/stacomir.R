## ----setup, include = FALSE---------------------------------------------------
require(knitr)
knitr::opts_chunk$set(
		collapse = TRUE,
		comment = "#>",
		fig.path = "../man/figures/README-"
)
opts_knit$set(message=FALSE, warnings=FALSE,error=FALSE,include=FALSE) 

## ----echo=FALSE, include=FALSE------------------------------------------------
library(stacomiR)


## ---- eval=FALSE--------------------------------------------------------------
#  # get the package from CRAN
#  install.packages("stacomiR")
#  # get the development version
#  install.packages("stacomiR", repos="http://R-Forge.R-project.org")

## ---- eval=FALSE--------------------------------------------------------------
#  stacomi()

## ---- eval=FALSE--------------------------------------------------------------
#  #here is an example to connect with interactive use
#  options(
#  		stacomiR.dbname = "bd_contmig_nat",
#  		stacomiR.host = readline(prompt = "Enter host: "),
#  		stacomiR.port = "5432",
#  		stacomiR.user = readline(prompt = "Enter user: "),
#  		stacomiR.password = readline(prompt = "Enter password: ")
#  )
#  # the schema of connection is passed by sch
#  stacomi(database_expected = TRUE, sch= "iav")

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
## launches the application in the command line without connection to the database
stacomi(database_expected=FALSE) 

## ----eval=FALSE,echo=TRUE-----------------------------------------------------
#  require(stacomiR)
#  stacomi(
#  		database_expected=TRUE)	
#  r_mig_mult=new("report_mig_mult")
#  r_mig_mult=choice_c(r_mig_mult,
#  		dc=c(5,6,12),
#  		taxa=c("Anguilla anguilla"),
#  		stage=c("AGG","AGJ","CIV"),
#  		datedebut="2011-01-01",
#  		datefin="2011-12-31")
#  r_mig_mult<-charge(r_mig_mult)
#  # launching charge will also load classes associated with the report
#  # e.g. report_ope, report_df, report_dc
#  r_mig_mult<-connect(r_mig_mult)
#  # calculations
#  r_mig_mult<-calcule(r_mig_mult,silent=TRUE)

## ----rmmstd,eval=FALSE,echo=TRUE,message=FALSE,fig.height=6,fig.with=8--------
#  # Without a connection at the database we can launch these lines to generate the graph
#  # To obtain titles in french use Sys.setenv(LANG = "fr")
#  # the
#  require(stacomiR)
#  stacomi(
#  		database_expected=FALSE)	
#  data("r_mig_mult")
#  data("r_mig_mult_ope")
#  assign("report_ope",r_mig_mult_ope,envir=envir_stacomi)
#  data("r_mig_mult_df")
#  assign("report_df",r_mig_mult_df,envir=envir_stacomi)
#  data("r_mig_mult_dc")
#  assign("report_dc",r_mig_mult_dc,envir=envir_stacomi)
#  # The two lines below avoid an error in MacOSX, the r_mig_mult currently has CET
#  # which for some reason provides a failure when testing the vignette in MaxOSX (twice the same date in seq.POSIXt)
#  attr(r_mig_mult@data$ope_date_debut, "tzone") <- "UTC"
#  attr(r_mig_mult@data$ope_date_fin, "tzone") <- "UTC"
#  r_mig_mult <- calcule(r_mig_mult,silent=TRUE)
#  
#  
#  # restrict to glass eel
#  r_mig_mult@stage@stage_selected <- r_mig_mult@stage@stage_selected[3]
#  r_mig_mult@dc@dc_selected <- r_mig_mult@dc@dc_selected[3]
#  r_mig_mult <- calcule(r_mig_mult,silent=TRUE)
#  plot(r_mig_mult, plot.type="standard", silent=TRUE)

## ----rmmmult,eval=TRUE,echo=TRUE,fig.height = 4, fig.width = 6----------------

plot(r_mig_mult,plot.type="multiple",silent=TRUE)

## ----silver,eval=TRUE,echo=TRUE,message=FALSE,warning=FALSE,fig.height = 4, fig.width = 6,fig.keep="all"----
require(stacomiR)
data("coef_durif")
# load a dataset of class report_silver_eel with data slot already prepared
# here is an example of output 
data("r_silver")
r_silver <- calcule(r_silver)
plot(r_silver, plot.type="3")
#######################################
# To use the function fun_stage_durif manually
# create a matrix with columns BL","W","Dv","Dh","FL"
#############################################
# here it is extracted from the data at hand
silver_eel<-as.matrix(r_silver@calcdata[[1]][,c("BL","W","Dv","Dh","FL")])
head(silver_eel) # to see the first lines
stage <- fun_stage_durif(silver_eel) # apply the function to the matrix
stage[1:10] # look at the first 10 elements in vector silver

