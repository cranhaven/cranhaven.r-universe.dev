## ----libs_tut, echo = FALSE, message = FALSE----------------------------------
library(knitr)
library(flowr)

## ----example1, cache = FALSE, echo=FALSE, eval=FALSE--------------------------
#  s <- file.path(system.file(package = 'flowr'), 'pipelines/sleep_pipe.R')
#  knitr::read_chunk(s)

## -----------------------------------------------------------------------------
sleep=c('sleep 5', 'sleep 5')

tmp=c('cat $RANDOM > tmp1', 
			'cat $RANDOM > tmp2')
			
merge='cat tmp1 tmp2 > tmp'
			
size='du -sh tmp'


## -----------------------------------------------------------------------------
## create a table of all commands
library(flowr)
lst = list( sleep=sleep, 
           create_tmp=tmp, 
           merge=merge,
           size=size)

flowmat = to_flowmat(lst, "samp1")
kable(flowmat)

## ----plot_skeleton_def, message=FALSE-----------------------------------------
## create a skeleton flow definition
def = to_flowdef(flowmat) 
suppressMessages(plot_flow(def))

## ----message=FALSE------------------------------------------------------------
##               sleep     create tmp   merge     size
def$sub_type = c("scatter", "scatter", "serial", "serial")
def$dep_type = c("none", "serial", "gather", "serial")
kable(def)

## ----plot_tweaked_def, message=FALSE, echo = FALSE----------------------------
suppressMessages(plot_flow(def))

## ---- message=FALSE-----------------------------------------------------------
fobj = to_flow(flowmat, def, flowname = "sleep_pipe")

## ----eval=FALSE---------------------------------------------------------------
#  plot_flow(fobj)
#  submit_flow(fobj) ## dry run
#  fobj2 = submit_flow(fobj, execute = TRUE) ## submission to LSF cluster
#  
#  ## after submission, we can use the following:
#  status(fobj2) ## check status
#  rerun(fobj2)  ## re-run from a intermediate step
#  kill(fobj2)   ## kill it!

## ----define_modules, echo=TRUE------------------------------------------------


## ----define_pipeline----------------------------------------------------------


## ----eval=FALSE---------------------------------------------------------------
#  ## 1. Single step submission:
#  fobj = run("sleep_pipe", execute = TRUE);
#  
#  ## 2
#  ## change wd, so that we can source the files downloaded in the previous step
#  setwd("~/flowr/pipelines")
#  
#  ## 2a. optionally, load default parameters
#  load_opts("sleep_pipe.conf")
#  
#  ## 2b. get sleep_pipe() function
#  source("sleep_pipe.R")
#  ## create a flowmat
#  flowmat = sleep_pipe()
#  
#  ## 2c. read a flow definition.
#  flowdef = as.flowdef("sleep_pipe.def")
#  
#  ## 2d. create flow and submit to cluster
#  fobj = to_flow(flowmat, flowdef, execute = TRUE)

## ----picard_merge, echo=TRUE, comment=""--------------------------------------
picard_merge <- function(x, 
                        samplename = opts_flow$get("samplename"),
                         mergedbam,
                         java_exe = opts_flow$get("java_exe"),
                         java_mem = opts_flow$get("java_mem"),
                         java_tmp = opts_flow$get("java_tmp"),
                         picard_jar = opts_flow$get("picard_jar")){
	## Make sure all args have a value (not null)
	## If a variable was not defined in a conf. file opts_flow$get, will return NULL
	check_args()  
  
  bam_list = paste("INPUT=", x, sep = "", collapse = " ")
  ## create a named list of commands
  cmds = list(merge = sprintf("%s %s -Djava.io.tmpdir=%s -jar %s MergeSamFiles %s OUTPUT=%s ASSUME_SORTED=TRUE VALIDATION_STRINGENCY=LENIENT CREATE_INDEX=true USE_THREADING=true",java_exe, java_mem, java_tmp, picard_jar, bam_list, mergedbam))
  
  ## Create a flowmat
  flowmat = to_flowmat(cmds, samplename)
  
  ## return a list, flowmat AND outfiles
  return(list(outfiles = mergedbam, flowmat = flowmat))
}

## -----------------------------------------------------------------------------
## check_args(), checks ALL the arguments of the function, and throws a error. use ?check_args for more details.
opts_flow$get("my_new_tool")

