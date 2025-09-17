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

##############################################
### Prepare the data
##############################################

set.seed(123)
res_mvn_poi = AM_demo_mvn_poi () 

set.seed(123)
res_mvb_poi = AM_demo_mvb_poi () 

set.seed(123)
res_uvn_poi = AM_demo_uvn_poi () 


##############################################
### Run
##############################################



CI_uvn = AM_extract(res_uvn_poi$fit,c("CI"))
df_uvn = AM_extract(res_uvn_poi$fit,c("mu","sig2","W","YPRED","U","M","K","gamma","lambda"))

CI_mvn = AM_extract(res_mvn_poi$fit,c("CI"))
df_mvn = AM_extract(res_mvn_poi$fit,c("mu","Sig","W","U","M","K","gamma","lambda"))

CI_mvb = AM_extract(res_mvb_poi$fit,c("CI"))
df_mvb = AM_extract(res_mvb_poi$fit,c("theta","W","U","M","K","gamma","lambda"))



##############################################
### Verify
##############################################

# stopifnot(is.data.frame(df_uvn))
# stopifnot(is.data.frame(df_mvn))
# stopifnot(is.data.frame(df_mvb))

# stopifnot(is.data.frame(CI_uvn))
# stopifnot(is.data.frame(CI_mvn))
# stopifnot(is.data.frame(CI_mvb))
stopifnot(is.list(df_uvn))
stopifnot(is.list(df_mvn))
stopifnot(is.list(df_mvb))

stopifnot(is.list(CI_uvn))
stopifnot(is.list(CI_mvn))
stopifnot(is.list(CI_mvb))

# stopifnot(is.integer(df_uvn[,"K"]))
# stopifnot(is.integer(df_uvn[,"M"]))
# stopifnot(is.numeric(df_uvn[,"gamma",]))
# stopifnot(is.numeric(df_uvn[,"lambda"]))
# stopifnot(is.numeric(df_uvn[,"W_1"]))
# stopifnot(is.numeric(df_uvn[,"U"]))


# stopifnot(is.integer(df_mvn[,"K"]))
# stopifnot(is.integer(df_mvn[,"M"]))
# stopifnot(is.numeric(df_mvn[,"gamma",]))
# stopifnot(is.numeric(df_mvn[,"lambda"]))
# stopifnot(is.numeric(df_mvn[,"W_1"]))
# stopifnot(is.numeric(df_mvn[,"U"]))
