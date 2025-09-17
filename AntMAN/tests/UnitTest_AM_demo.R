#######################################################################################
###############
############### AntMAN Package : Tests and Examples
###############
###############
#######################################################################################

##############################################
### Load the AntMan package
##############################################

library("AntMAN")
set.seed(123)

##############################################
### Run
##############################################

res_mvn_poi = AM_demo_mvn_poi () 
res_mvb_poi = AM_demo_mvb_poi () 
res_uvn_poi = AM_demo_uvn_poi () 

##############################################
### Verify
##############################################

stopifnot(is.numeric(res_mvn_poi$input))
stopifnot(is.integer(res_mvb_poi$input))
stopifnot(is.numeric(res_uvn_poi$input))

stopifnot(is.matrix(res_mvn_poi$input))
stopifnot(is.matrix(res_mvb_poi$input))
stopifnot(is.vector(res_uvn_poi$input))

stopifnot(is.vector(res_mvn_poi$clusters))
stopifnot(is.vector(res_mvb_poi$clusters))
stopifnot(is.vector(res_uvn_poi$clusters))

stopifnot(is.list(res_mvn_poi$fit))
stopifnot(is.list(res_mvb_poi$fit))
stopifnot(is.list(res_uvn_poi$fit))





