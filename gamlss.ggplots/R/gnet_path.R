################################################################################
################################################################################
################################################################################
################################################################################
# 
################################################################################
################################################################################
################################################################################
################################################################################
gnet_path <- function(model, parameter="mu")
{
  if (!(class(getSmo(model, parameter)[[1]])[1]%in%c("elnet", "lars")))
    stop("this is not a relevant model")
  if (class(getSmo(model, parameter)[[1]])[1]=="elnet")
  {
    plot(getSmo(model, parameter)[[1]], xvar="lambda")
    if (!is.null(getSmo(model,parameter)[[2]]$optlambda)) abline(v=log(getSmo(model,parameter)[[2]]$optlambda),  col="gray")
  } else 
  {
    plot(getSmo(model, parameter)[[1]], xvar="df")
    abline(v=getSmo(model,parameter)[[2]]$df,  col="red")
  }  
}
################################################################################
################################################################################
################################################################################
################################################################################
gnet_terms <- function(model, parameter="mu")
{
  lout = length(getSmo(model, parameter))
  if (!(class(getSmo(model, parameter)[[1]])[1]%in%c("elnet", "lars")))
    stop("this is not a relevant model")
  getSmo(model, parameter)[[lout]]$beta[getSmo(model, parameter)[[lout]]$beta!=0]
}
################################################################################
################################################################################
################################################################################\
################################################################################
gnet_coef <- function(model, parameter="mu")
{
  lout = length(getSmo(model, parameter))
  if (!(class(getSmo(model, parameter)[[1]])[1]%in%c("elnet", "lars")))
    stop("this is not a relevant model")
  getSmo(model, parameter)[[lout]]$beta
}
################################################################################
################################################################################
################################################################################
################################################################################
gnet_df <- function(model, parameter="mu")
{
  lout = length(getSmo(model, parameter))
  if (!(class(getSmo(model, parameter)[[1]])[1]%in%c("elnet", "lars")))
    stop("this is not a relevant model")
  getSmo(model, parameter)[[lout]]$df
}
################################################################################
################################################################################
################################################################################
################################################################################
# old code
# X <- as.matrix(model.frame(flow~time+month+nino+soi+eof1+eof2+logf1+logf2+ logf3+logf4+logf5 , data=da1)[,-1])
#X1 <- model.matrix(flow ~time+month+nino+soi+eof1+eof2+logf1+logf2+ logf3+logf4+logf5 , data=da1)[,-1]
#identical(X,X1)
# the problem is that we do not want to scale the factors  
# pp contains the position of the factor in the 
#    pp <- unlist(sapply(theFactors, grep, colnames(XX)))
# scale only the continuous variables 
#     if (scale) XX[,-pp] <- scale(XX[,-pp])
# if (type=="second.order")
#      {
#       formula <- as.formula(paste(paste0(response_t,"~"), 
#                     paste0(paste0("(",paste(x_Names, collapse='+')), ")^3"))) 
#      }   
################################################################################
################################################################################
################################################################################
################################################################################
get_kfolds <- function(data, K=6, setseed=123 )
{
  set.seed(setseed)
  nfolds <- K
  n <- dim(data)[1]
  # folds for cross-validation 
  CVfolds <-  lapply(
    as.data.frame(
      t(
        sapply(
          sample(rep_len(1:nfolds,length.out=n),replace=FALSE)
          ,"!=", 1:nfolds)
      )
    )
    , which )   
  CVfolds
}
################################################################################
################################################################################
################################################################################
################################################################################ 

