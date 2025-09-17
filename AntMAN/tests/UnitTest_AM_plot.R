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

AM_plot_pairs(res_mvn_poi$fit, tags = c("K","M"))
AM_plot_density(res_mvn_poi$fit, tags = c("K","M"))
AM_plot_traces(res_mvn_poi$fit, tags = c("K","M"))
AM_plot_values(res_mvn_poi$fit, tags = c("K","M"))
AM_plot_chaincor(res_mvn_poi$fit, tags = c("K","M"))
AM_plot_similarity_matrix(res_mvn_poi$fit, "binder")


AM_plot_pairs(res_mvb_poi$fit, tags = c("K","M"))
AM_plot_density(res_mvb_poi$fit, tags = c("K","M"))
AM_plot_traces(res_mvb_poi$fit, tags = c("K","M"))
AM_plot_values(res_mvb_poi$fit, tags = c("K","M"))
AM_plot_chaincor(res_mvb_poi$fit, tags = c("K","M"))
AM_plot_similarity_matrix(res_mvb_poi$fit, "binder")


##############################################
### Verify
##############################################

