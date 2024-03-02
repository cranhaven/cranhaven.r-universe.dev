
survivalSL <- function(methods, metric="ci",  data, times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL,
                     cv=10, param.tune=NULL, pro.time=NULL,  optim.local.min=FALSE,
                     ROC.precision=seq(.01,.99,.01), param.weights.fix=NULL,
                     param.weights.init=NULL, keep.predictions=TRUE,
                     progress=TRUE) {

  #####################
  ### Quality tests ###
  #####################

  if(length(methods)<=1)
  { stop("Number of methods need to be greater or equal to 2 to estimate a SuperLearner")   }

  if(length(metric)>1){
    warning(paste0("SuperLearner is currently developped for one metric. Results for metric ",metric[1]))
    metric=metric[1]
  }

  if(min(metric%in%c("ci","bs","loglik","ibs","ibll","bll", "ribs","ribll","auc"))==0){
    stop("The argument \"metric\" must be Brier score (bs),
         Concordance index (ci),
         Integrated Brier score (ibs), the binomilar log-likelihood (bll),
         the Integrated binomial log-likelihood (ibll), the restricted ibs (ribs),
         the restricted ibll (ribll), the log-likelihood (loglik), or
         the area under the ROC curve (auc)")
  }

  if(!is.data.frame(data) & !is.matrix(data)){
    stop("The argument \"data\" need to be a data.frame or a matrix") }


  if( is.null(group)==F){
    if(length(group)>1){
      stop("Only one variable can be use as group")
    }
    if(min(group %in%colnames(data))==0 & is.character(group)==T){
      stop("Group name is not present in data")
    }
  }

  if( is.null(cov.quanti)==F){
    if(min(group %in%colnames(data))==0 & is.character(cov.quanti)==T){
      stop("At least one name of quantitative covariate is not present in data")
    }
  }

  if( is.null(cov.quali)==F){
    if(min(cov.quali %in%colnames(data))==0 & is.character(cov.quali)==T){
      stop("At least one name of qualitative covariate is not present in data")
    }
  }

  if(is.null(group)==T&is.null(cov.quanti)==T&is.null(cov.quali)==T){
    stop("SuperLearner need at least one group or one quantitative or one qualitative covariate")
  }

  if(!is.null(group)==T){
    data<-data[,c(times,failures,group,cov.quanti,cov.quali)]
  }
  if(is.null(group)==T){
    data<-data[,c(times,failures,cov.quanti,cov.quali)]
  }

  if (any(is.na(data))){
    data<-na.omit(data)
    warning("Data need to be without NA. NA is removed")
  }

  if(!(is.null(group))){
    if(!is.character(group) & !is.numeric(group) ){
      stop("The argument \"group\" need to be scalar or a character string") }

    mod <- unique(data[,group])
    if(length(mod) != 2 | ((mod[1] != 0 & mod[2] != 1) & (mod[1] != 1 & mod[2] != 0))){
      stop("Two modalities encoded 0 (for non-treated/non-exposed patients) and 1 (for treated/exposed patients) are required in the argument \"group\" ")
    }

  }

  if(length(data[,times])!=length(data[,failures])){
    stop("The length of the times must be equaled to the length of the events in the training data") }

  mod2 <- unique(data[,failures])
  if(length(mod2) != 2 | ((mod2[1] != 0 & mod2[2] != 1) & (mod2[1] != 1 & mod2[2] != 0))){
    stop("Two modalities encoded 0 (for censored patients) and 1 (for dead patients) are required in the argument \"failures\" ")
  }

  if (!is.numeric(data[,times])){
    stop("Time variable is not numeric")}

  if (min(data[,times])<=0){
    stop("Time variable need to be positive")
  }

  if (cv < 3 | !is.numeric(cv)) {
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  }

  .meth_rm=c()
  if(sum(methods %in% "LIB_AFTgamma")>=2){
    .meth_rm=c(.meth_rm,which(methods=="LIB_AFTgamma")[-1])
    warning("SuperLearner can use only one LIB_AFTgamma method. We remove the others.")
  }
  if(sum(methods %in% "LIB_AFTllogis")>=2){
    .meth_rm=c(.meth_rm,which(methods=="LIB_AFTllogis")[-1])
    warning("SuperLearner can use only one LIB_AFTllogis method. We remove the others.")
  }
  if(sum(methods %in% "LIB_AFTggamma")>=2){
    .meth_rm=c(.meth_rm,which(methods=="LIB_AFTggamma")[-1])
    warning("SuperLearner can use only one LIB_AFTggamma method. We remove the others.")
  }
  if(sum(methods %in% "LIB_AFTweibull")>=2){
    .meth_rm=c(.meth_rm,which(methods=="LIB_AFTweibull")[-1])
    warning("SuperLearner can use only one LIB_AFTweibull method. We remove the others.")
  }
  if(sum(methods %in% "LIB_PHexponential")>=2){
    .meth_rm=c(.meth_rm,which(methods=="LIB_PHexponential")[-1])
    warning("SuperLearner can use only one LIB_PHexponential method. We remove the others.")
  }
  if(sum(methods %in% "LIB_PHgompertz")>=2){
    .meth_rm=c(.meth_rm,which(methods=="LIB_PHgompertz")[-1])
    warning("SuperLearner can use only one LIB_PHgompertz method. We remove the others.")
  }
  if(sum(methods %in% "LIB_PHspline")>=2){
    .meth_rm=c(.meth_rm,which(methods=="LIB_PHspline")[-1])
    warning("SuperLearner can use only one LIB_PHspline method. We remove the others.")
  }

  if(sum(methods %in% "LIB_COXlasso")==1){
    if(!(is.null(param.tune[[which(methods=="LIB_COXlasso")]]))){
      if(!is.list(param.tune[[which(methods=="LIB_COXlasso")]])){
        stop("Argument param.tune for LIB_COXlasso need to be a list")
      }
      if(sum((names(param.tune[[which(methods=="LIB_COXlasso")]])%in%"lambda"))==0){
        stop("Tune parameters for LIB_COXlasso need to have lambda")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_COXlasso")]]$lambda)|
           is.null(param.tune[[which(methods=="LIB_COXlasso")]]$lambda))){
        stop("Lambda tune parameters for LIB_COXlasso need to be a scalar or a vector or NULL")
      }
    }
  }

  if(sum(methods %in% "LIB_COXlasso")>=2){
    if(length(param.tune[which(methods=="LIB_COXlasso")])!=length(unique(param.tune[which(methods=="LIB_COXlasso")]))){
      stop("Tune parameters for LIB_COXlasso methods need to be unique")
    }
    for (i in 1:sum(methods %in% "LIB_COXlasso")){
      if(!(is.null(param.tune[[which(methods=="LIB_COXlasso")[i]]]))){
        if(!is.list(param.tune[[which(methods=="LIB_COXlasso")[i]]])){
          stop(paste("Argument param.tune for the ",i,"th LIB_COXlasso need to be a list"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_COXlasso")[i]]])%in%"lambda"))==0){
          stop(paste("Tune parameters for the ",i,"th LIB_COXlasso need to have lambda"))
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_COXlasso")[i]]]$lambda)|
             is.null(param.tune[[which(methods=="LIB_COXlasso")[i]]]$lambda))){
          stop(paste("Lambda tune parameters for the ",i,"th LIB_COXlasso need to be a scalar or a vector or NULL"))
        }
      }
    }
  }



  if(sum(methods %in% "LIB_PHspline")==1){
    if(!(is.null(param.tune[[which(methods=="LIB_PHspline")]]))){
      if(!is.list(param.tune[[which(methods=="LIB_PHspline")]])){
        stop("Argument param.tune for LIB_PHspline need to be a list")
      }
      if(sum((names(param.tune[[which(methods=="LIB_PHspline")]])%in%"k"))==0){
        stop("Tune parameters for LIB_PHspline need to have k")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_PHspline")]]$k)|
           is.null(param.tune[[which(methods=="LIB_PHspline")]]$k))){
        stop("Lambda tune parameters for LIB_PHspline need to be a scalar or a vector or NULL")
      }
    }
  }

  if(sum(methods %in% "LIB_PHspline")>=2){
    if(length(param.tune[which(methods=="LIB_PHspline")])!=length(unique(param.tune[which(methods=="LIB_PHspline")]))){
      stop("Tune parameters for LIB_PHspline methods need to be unique")
    }
    for (i in 1:sum(methods %in% "LIB_PHspline")){
      if(!(is.null(param.tune[[which(methods=="LIB_PHspline")[i]]]))){
        if(!is.list(param.tune[[which(methods=="LIB_PHspline")[i]]])){
          stop(paste("Argument param.tune for the ",i,"th LIB_PHspline need to be a list"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_PHspline")[i]]])%in%"k"))==0){
          stop(paste("Tune parameters for the ",i,"th LIB_PHspline need to have k"))
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_PHspline")[i]]]$k)|
             is.null(param.tune[[which(methods=="LIB_PHspline")[i]]]$k))){
          stop(paste("Lambda tune parameters for the ",i,"th LIB_PHspline need to be a scalar or a vector or NULL"))
        }
      }
    }
  }


  if(sum(methods %in% "LIB_COXridge")==1){
    if(!(is.null(param.tune[[which(methods=="LIB_COXridge")]]))){
      if(!is.list(param.tune[[which(methods=="LIB_COXridge")]])){
        stop("Argument param.tune for LIB_COXridge need to be a list")
      }
      if(sum((names(param.tune[[which(methods=="LIB_COXridge")]])%in%"lambda"))==0){
        stop("Tune parameters for LIB_COXridge need to have lambda")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_COXridge")]]$lambda)|
           is.null(param.tune[[which(methods=="LIB_COXridge")]]$lambda))){
        stop("Lambda tune parameters for LIB_COXridge need to be a scalar or a vector or NULL")
      }
    }
  }
  if(sum(methods %in% "LIB_COXridge")>=2){
    if(length(param.tune[which(methods=="LIB_COXridge")])!=length(unique(param.tune[which(methods=="LIB_COXridge")]))){
      stop("Tune parameters for LIB_COXridge methods need to be unique")
    }
    for (i in 1:sum(methods %in% "LIB_COXridge")){
      if(!(is.null(param.tune[[which(methods=="LIB_COXridge")[i]]]))){
        if(!is.list(param.tune[[which(methods=="LIB_COXridge")[i]]])){
          stop(paste("Argument param.tune for the ",i,"th LIB_COXridge need to be a list"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_COXridge")[i]]])%in%"lambda"))==0){
          stop(paste("Tune parameters for the ",i,"th LIB_COXridge need to have lambda"))
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_COXridge")[i]]]$lambda)|
             is.null(param.tune[[which(methods=="LIB_COXridge")[i]]]$lambda))){
          stop(paste("Lambda tune parameters for the ",i,"th LIB_COXridge need to be a scalar or a vector or NULL"))
        }
      }
    }
  }

  if(sum(methods %in% "LIB_COXen")==1){
    if(!(is.null(param.tune[[which(methods=="LIB_COXen")]]))){
      if(!is.list(param.tune[[which(methods=="LIB_COXen")]])){
        stop("Argument param.tune for LIB_COXen need to be a list")
      }
      if(sum((names(param.tune[[which(methods=="LIB_COXen")]])%in%"lambda"))==0){
        stop("Tune parameters for LIB_COXen need to have lambda")
      }
      if(sum((names(param.tune[[which(methods=="LIB_COXen")]])%in%"alpha"))==0){
        stop("Tune parameters for LIB_COXen need to have alpha")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_COXen")]]$lambda)|
           is.null(param.tune[[which(methods=="LIB_COXen")]]$lambda))){
        stop("Lambda tune parameters for LIB_COXen need to be a scalar or a vector or NULL")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_COXen")]]$alpha)|
           is.null(param.tune[[which(methods=="LIB_COXen")]]$alpha))){
        stop("alpha tune parameters for LIB_COXen need to be a scalar or a vector or NULL")
      }
      if(min(param.tune[[which(methods=="LIB_COXen")]]$alpha)<0 | max(param.tune[[which(methods=="LIB_COXen")]]$alpha)>1){
        stop("tune parameters for LIB_COXen alpha need to be in ]0;1[")
      }
    }
  }
  if(sum(methods %in% "LIB_COXen")>=2){
    if(length(param.tune[which(methods=="LIB_COXen")])!=length(unique(param.tune[which(methods=="LIB_COXen")]))){
      stop("Tune parameters for LIB_COXen methods need to be unique")
    }
    for (i in 1:sum(methods %in% "LIB_COXen")){
      if(!(is.null(param.tune[[which(methods=="LIB_COXen")[i]]]))){
        if(!is.list(param.tune[[which(methods=="LIB_COXen")[i]]])){
          stop(paste("Argument param.tune for the ",i,"th LIB_COXen need to be a list"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_COXen")[i]]])%in%"lambda"))==0){
          stop(paste("Tune parameters for the ",i,"th LIB_COXen need to have lambda"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_COXen")[i]]])%in%"alpha"))==0){
          stop(paste("Tune parameters for the ",i,"th LIB_COXen need to have alpha"))
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_COXen")[i]]]$lambda)|
             is.null(param.tune[[which(methods=="LIB_COXen")[i]]]$lambda))){
          stop(paste("Lambda tune parameters for the ",i,"th LIB_COXen need to be a scalar or a vector or NULL"))
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_COXen")[i]]]$alpha)|
             is.null(param.tune[[which(methods=="LIB_COXen")[i]]]$alpha))){
          stop(paste("Alpha tune parameters for the ",i,"th LIB_COXen need to be a scalar or a vector or NULL"))
        }
        if(min(param.tune[[which(methods=="LIB_COXen")[i]]]$alpha)<0 | max(param.tune[[which(methods=="LIB_COXen")[i]]]$alpha)>1){
          stop("tune parameters for LIB_COXen alpha need to be in ]0;1[")
        }
      }
    }
  }

  if(sum(methods %in% "LIB_COXaic")==1){
    if(!(is.null(param.tune[[which(methods=="LIB_COXaic")]]))){
      if(!is.list(param.tune[[which(methods=="LIB_COXaic")]])){
        stop("Argument param.tune for LIB_COXaic need to be a list")
      }
      if(sum((names(param.tune[[which(methods=="LIB_COXaic")]])%in%"final.model"))==0){
        stop("Tune parameters for LIB_COXaic need to have final.model")
      }
      if(sum((names(param.tune[[which(methods=="LIB_COXaic")]])%in%"model.min"))==0){
        stop("Tune parameters for LIB_COXaic need to have model.min")
      }
      if(sum((names(param.tune[[which(methods=="LIB_COXaic")]])%in%"model.max"))==0){
        stop("Tune parameters for LIB_COXaic need to have model.max")
      }
    }
  }
  if(sum(methods %in% "LIB_COXaic")>=2){
    if(length(param.tune[which(methods=="LIB_COXaic")])!=length(unique(param.tune[which(methods=="LIB_COXaic")]))){
      stop("Tune parameters for LIB_COXaic methods need to be unique")
    }
    for (i in 1:sum(methods %in% "LIB_COXaic")){
      if(!(is.null(param.tune[[which(methods=="LIB_COXaic")[i]]]))){
        if(!is.list(param.tune[[which(methods=="LIB_COXaic")[i]]])){
          stop(paste("Argument param.tune for the ",i,"th LIB_COXaic need to be a list"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_COXaic")[i]]])%in%"finl.model.cov"))==0){
          stop(paste("Tune parameters for the ",i,"th LIB_COXaic need to have finl.model.cov"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_COXaic")[i]]])%in%"model.min"))==0){
          stop(paste("Tune parameters for the ",i,"th LIB_COXaic need to have model.min"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_COXaic")[i]]])%in%"model.max"))==0){
          stop("Tune parameters for LIB_COXaic need to have model.max")
        }
      }
    }
  }


  if(sum(methods %in% "LIB_RSF")==1){
    if(!(is.null(param.tune[[which(methods=="LIB_RSF")]]))){
      if(!is.list(param.tune[[which(methods=="LIB_RSF")]])){
        stop("Argument param.tune for LIB_RSF need to be a list")
      }
      if(sum((names(param.tune[[which(methods=="LIB_RSF")]])%in%"nodesize"))==0){
        stop("Tune parameters for LIB_RSF need to have nodesize")
      }
      if(sum((names(param.tune[[which(methods=="LIB_RSF")]])%in%"mtry"))==0){
        stop("Tune parameters for LIB_RSF need to have mtry")
      }
      if(sum((names(param.tune[[which(methods=="LIB_RSF")]])%in%"ntree"))==0){
        stop("Tune parameters for LIB_RSF need to have ntree")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_RSF")]]$nodesize)|
           is.null(param.tune[[which(methods=="LIB_RSF")]]$nodesize))){
        stop("nodesize tune parameters for LIB_RSF need to be a scalar or a vector or NULL")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_RSF")]]$mtry)|
           is.null(param.tune[[which(methods=="LIB_RSF")]]$mtry))){
        stop("mtry tune parameters for LIB_RSF need to be a scalar or NULL")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_RSF")]]$ntree)|
           is.null(param.tune[[which(methods=="LIB_RSF")]]$ntree))){
        stop("ntree tune parameters for LIB_RSF need to be a scalar or NULL")
      }
    }
  }
  if(sum(methods %in% "LIB_RSF")>=2){
    if(length(param.tune[which(methods=="LIB_RSF")])!=length(unique(param.tune[which(methods=="LIB_RSF")]))){
      stop("Tune parameters for LIB_RSF methods need to be unique")
    }
    for (i in 1:sum(methods %in% "LIB_RSF")){
      if(!is.list(param.tune[[which(methods=="LIB_RSF")[i]]])){
        stop(paste("Argument param.tune for the ",i,"th LIB_RSF need to be a list"))
      }
      if(!(is.null(param.tune[[which(methods=="LIB_RSF")[i]]]))){
        if(sum((names(param.tune[[which(methods=="LIB_RSF")[i]]])%in%"nodesize"))==0){
          stop("Tune parameters for LIB_RSF need to have nodesize")
        }
        if(sum((names(param.tune[[which(methods=="LIB_RSF")[i]]])%in%"mtry"))==0){
          stop("Tune parameters for LIB_RSF need to have mtry")
        }
        if(sum((names(param.tune[[which(methods=="LIB_RSF")[i]]])%in%"ntree"))==0){
          stop("Tune parameters for LIB_RSF need to have ntree")
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_RSF")[i]]]$nodesize)|
             is.null(param.tune[[which(methods=="LIB_RSF")[i]]]$nodesize))){
          stop("nodesize tune parameters for LIB_RSF need to be a scalar or a vector or NULL")
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_RSF")[i]]]$mtry)|
             is.null(param.tune[[which(methods=="LIB_RSF")[i]]]$mtry))){
          stop("mtry tune parameters for LIB_RSF need to be a scalar or NULL")
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_RSF")[i]]]$ntree)|
             is.null(param.tune[[which(methods=="LIB_RSF")[i]]]$ntree))){
          stop("ntree tune parameters for LIB_RSF need to be a scalar or NULL")
        }
      }
    }
  }

  if(sum(methods %in% "LIB_SNN")==1){
    if(!(is.null(param.tune[[which(methods=="LIB_SNN")]]))){
      if(!is.list(param.tune[[which(methods=="LIB_SNN")]])){
        stop("Argument param.tune for LIB_SNN need to be a list")
      }
      if(sum((names(param.tune[[which(methods=="LIB_SNN")]])%in%"n.nodes"))==0){
        stop("Tune parameters for LIB_SNN need to have n.nodes")
      }
      if(sum((names(param.tune[[which(methods=="LIB_SNN")]])%in%"decay"))==0){
        stop("Tune parameters for LIB_SNN need to have decay")
      }
      if(sum((names(param.tune[[which(methods=="LIB_SNN")]])%in%"batch.size"))==0){
        stop("Tune parameters for LIB_SNN need to have batch.size")
      }
      if(sum((names(param.tune[[which(methods=="LIB_SNN")]])%in%"epochs"))==0){
        stop("Tune parameters for LIB_SNN need to have epochs")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_SNN")]]$n.nodes)|
           is.null(param.tune[[which(methods=="LIB_SNN")]]$n.nodes))){
        stop("n.nodes tune parameters for LIB_SNN need to be a scalar, a vector or NULL")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_SNN")]]$decay)|
           is.null(param.tune[[which(methods=="LIB_SNN")]]$decay))){
        stop("decay tune parameters for LIB_SNN need to be a scalar, a vector or NULL")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_SNN")]]$batch.size)|
           is.null(param.tune[[which(methods=="LIB_SNN")]]$batch.size))){
        stop("batch.size tune parameters for LIB_SNN need to be a scalar, a vector or NULL")
      }
      if(!(is.numeric(param.tune[[which(methods=="LIB_SNN")]]$epochs)|
           is.null(param.tune[[which(methods=="LIB_SNN")]]$epochs))){
        stop("epochs tune parameters for LIB_SNN need to be a scalar, a vector or NULL")
      }
    }
  }
  if(sum(methods %in% "LIB_SNN")>=2){
    if(length(param.tune[which(methods=="LIB_SNN")])!=length(unique(param.tune[which(methods=="LIB_SNN")]))){
      stop("Tune parameters for LIB_SNN methods need to be unique")
    }
    for (i in 1:sum(methods %in% "LIB_SNN")){
      if(!(is.null(param.tune[[which(methods=="LIB_SNN")[i]]]))){
        if(!is.list(param.tune[[which(methods=="LIB_SNN")[i]]])){
          stop(paste("Argument param.tune for the ",i,"th LIB_SNN need to be a list"))
        }
        if(sum((names(param.tune[[which(methods=="LIB_SNN")[i]]])%in%"n.nodes"))==0){
          stop("Tune parameters for LIB_SNN need to have n.nodes")
        }
        if(sum((names(param.tune[[which(methods=="LIB_SNN")[i]]])%in%"decay"))==0){
          stop("Tune parameters for LIB_SNN need to have decay")
        }
        if(sum((names(param.tune[[which(methods=="LIB_SNN")[i]]])%in%"batch.size"))==0){
          stop("Tune parameters for LIB_SNN need to have batch.size")
        }
        if(sum((names(param.tune[[which(methods=="LIB_SNN")[i]]])%in%"epochs"))==0){
          stop("Tune parameters for LIB_SNN need to have epochs")
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_SNN")[i]]]$n.nodes)|
             is.null(param.tune[[which(methods=="LIB_SNN")[i]]]$n.nodes))){
          stop("nodesize tune parameters for LIB_SNN need to be a scalar or a vector or NULL")
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_SNN")[i]]]$decay)|
             is.null(param.tune[[which(methods=="LIB_SNN")[i]]]$decay))){
          stop("decay tune parameters for LIB_SNN need to be a scalar, a vector or NULL")
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_SNN")[i]]]$batch.size)|
             is.null(param.tune[[which(methods=="LIB_SNN")[i]]]$batch.size))){
          stop("batch.size tune parameters for LIB_SNN need to be a scalar, a vector or NULL")
        }
        if(!(is.numeric(param.tune[[which(methods=="LIB_SNN")[i]]]$epochs)|
             is.null(param.tune[[which(methods=="LIB_SNN")[i]]]$epochs))){
          stop("epochs tune parameters for LIB_SNN need to be a scalar, a vector or NULL")
        }
      }
    }
  }

  if(length(.meth_rm)>=1){
    methods=methods[-.meth_rm]
    param.tune=param.tune[-.meth_rm]
  }

  if((max(ROC.precision)==1) | (min(ROC.precision)==0)){
    stop("values for ROC.precision need to be in ]0;1[")
  }


  if(is.null(param.weights.fix)==FALSE & is.null(param.weights.init)==FALSE){
    warning("Weights can not be fix and initial at the same time. SuperLearner ignored initial values")
    param.weights.init<-NULL
  }

  if(is.null(param.weights.fix)==FALSE | is.null(param.weights.init)==FALSE){
    if(is.null(param.weights.fix)==FALSE){
      if(is.numeric(param.weights.fix)==FALSE){
        stop("param.weights.fix need to be numeric")
      }
    }

    if(is.null(param.weights.init)==FALSE){
      if(is.numeric(param.weights.init)==FALSE){
        stop("param.weights.init need to be numeric")
      }

        if(length(param.weights.init)!=(length(methods)-1)){
          stop("wrong lenth for param.weights.init")
        }

    }
  }
  if(is.null(param.weights.fix)==TRUE & is.null(param.weights.init)==TRUE){
      param.weights.init<-rep(0,length(methods)-1)
  }

  ######################################################
  ### Loss functions used for the weigths estimation ###
  ######################################################

  ibs<-function(par, FitCV, timeVector, obj_surv, ot, csurv, csurv_btime, time){

    .pred<-array(dim = c(dim(FitCV[[1]]),length(FitCV)))
    .par<-exp(c(par))/(1+sum(exp(par)))


    .par<-c(.par,1-sum(.par))
    for (i in 1:length(FitCV)){
      .pred[,,i]<-FitCV[[i]]*.par[i]
    }
    .pred<-rowSums(.pred, dims=2)

    survs <- t(.pred)[,ot]


    bsc<-sapply(1:length(timeVector), FUN = function(j)
    {
      help1 <- as.integer(time <= timeVector[j] & obj_surv[ot,2] == 1)
      help2 <- as.integer(time > timeVector[j])
      return(mean((0 - survs[j, ])^2 * help1 * (1/csurv) +
                    (1 - survs[j, ])^2 * help2 * (1/csurv_btime[j])))
    })

    idx <- 2:length(timeVector)
    RET <- diff(timeVector) %*% ((bsc[idx - 1] + bsc[idx])/2)
    RET <- RET/diff(range(timeVector))
    RET=as.matrix(RET)

    return(RET)
  }

  brs<-function(par, FitCV, timeVector,
                obj_surv, ot, csurv, csurv_btime, time, pro.time){

    .pred<-array(dim = c(dim(FitCV[[1]]),length(FitCV)))

    .par<-exp(c(par))/(1+sum(exp(par)))

    .par<-c(.par,1-sum(.par))

    for (i in 1:length(FitCV)){
      .pred[,,i]<-FitCV[[i]]*.par[i]
    }
    .pred<-rowSums(.pred, dims=2)

    survs <- t(.pred)[,ot]


    j=length(timeVector[which(timeVector<=pro.time)])


    help1 <- as.integer(time <= timeVector[j] &  obj_surv[ot,2] == 1)
    help2 <- as.integer(time > timeVector[j])
    bs=mean((0 - survs[j, ])^2 * help1 * (1/csurv) +
              (1 - survs[j, ])^2 * help2 * (1/csurv_btime[j]))

    bs=as.numeric(bs)
    return(bs)
  }

  minus.roc <- function(par, FitCV, timeVector,
                        obj_surv, ot, time, pro.time, ROC.precision) {

    .pred<-array(dim = c(dim(FitCV[[1]]),length(FitCV)))

    .par<-exp(c(par))/(1+sum(exp(par)))

    .par<-c(.par,1-sum(.par))

    for (i in 1:length(FitCV)){ .pred[,,i]<-FitCV[[i]]*.par[i] }
    .pred<-rowSums(.pred, dims=2)

    survs <- t(.pred)[,ot]

    j=length(timeVector[which(timeVector<=pro.time)])

    .data <- data.frame(times = time, failures = obj_surv[ot,2], predictions=1-survs[j, ])

    return(-1*roc(times="times", failures="failures", variable="predictions", confounders=~1, data=.data,
             pro.time=pro.time, precision=ROC.precision)$auc)

  }


  ibll <- function(par, FitCV, timeVector, obj_surv, ot, csurv, csurv_btime, time){

    .pred<-array(dim = c(dim(FitCV[[1]]),length(FitCV)))
    .par<-exp(c(par))/(1+sum(exp(par)))


    .par<-c(.par,1-sum(.par))
    for (i in 1:length(FitCV)){
      .pred[,,i]<-FitCV[[i]]*.par[i]
    }
    .pred<-rowSums(.pred, dims=2)

    survs <- t(.pred)[,ot]
    survs[which(survs==0)]<-10**-7
    survs[which(survs==1)]<-1-10**-7

    bll<-sapply(1:length(timeVector), FUN = function(j)
    {
      help1 <- as.integer(time <= timeVector[j] & obj_surv[ot,2] == 1)
      help2 <- as.integer(time > timeVector[j])
      return(-mean(log(1 - survs[j, ]) * help1 * (1/csurv) +
                     log( survs[j, ]) * help2 * (1/csurv_btime[j])))
    })

    idx <- 2:length(timeVector)
    RET <- diff(timeVector) %*% ((bll[idx - 1] + bll[idx])/2)
    RET <- RET/diff(range(timeVector))
    RET=as.matrix(RET)

    return(RET)
  }

  bll<-function(par, FitCV, timeVector, obj_surv, ot, csurv, csurv_btime, time, pro.time){

    .pred<-array(dim = c(dim(FitCV[[1]]),length(FitCV)))

    .par<-exp(c(par))/(1+sum(exp(par)))

    .par<-c(.par,1-sum(.par))

    for (i in 1:length(FitCV)){
      .pred[,,i]<-FitCV[[i]]*.par[i]
    }
    .pred<-rowSums(.pred, dims=2)

    survs <- t(.pred)[,ot]
    survs[which(survs==0)]<-10**-7
    survs[which(survs==1)]<-1-10**-7

    j=length(timeVector[which(timeVector<=pro.time)])


    help1 <- as.integer(time <= timeVector[j] &  obj_surv[ot,2] == 1)
    help2 <- as.integer(time > timeVector[j])
    bll=-mean(log(1- survs[j, ]) * help1 * (1/csurv) +
                log(survs[j, ]) * help2 * (1/csurv_btime[j]))

    bll=as.numeric(bll)
    return(bll)
  }

  ribs<-function(par, FitCV, timeVector, obj_surv, ot, csurv, csurv_btime, time, pro.time){

    .pred<-array(dim = c(dim(FitCV[[1]]),length(FitCV)))
    .par<-exp(c(par))/(1+sum(exp(par)))


    .par<-c(.par,1-sum(.par))
    for (i in 1:length(FitCV)){
      .pred[,,i]<-FitCV[[i]]*.par[i]
    }
    .pred<-rowSums(.pred, dims=2)

    .pred=.pred[,timeVector<=pro.time]

    survs <- t(.pred)[,ot]

    timeVector=timeVector[timeVector<=pro.time]

    bsc<-sapply(1:length(timeVector), FUN = function(j)
    {
      help1 <- as.integer(time <= timeVector[j] & obj_surv[ot,2] == 1)
      help2 <- as.integer(time > timeVector[j])
      return(mean((0 - survs[j, ])^2 * help1 * (1/csurv) +
                    (1 - survs[j, ])^2 * help2 * (1/csurv_btime[j])))
    })

    idx <- 2:length(timeVector)
    RET <- diff(timeVector) %*% ((bsc[idx - 1] + bsc[idx])/2)
    RET <- RET/diff(range(timeVector))
    RET=as.matrix(RET)

    return(RET)
  }


  ribll<-function(par, FitCV, timeVector,  obj_surv, ot, csurv, csurv_btime, time, pro.time){

    .pred<-array(dim = c(dim(FitCV[[1]]),length(FitCV)))
    .par<-exp(c(par))/(1+sum(exp(par)))


    .par<-c(.par,1-sum(.par))
    for (i in 1:length(FitCV)){
      .pred[,,i]<-FitCV[[i]]*.par[i]
    }
    .pred<-rowSums(.pred, dims=2)

    .pred=.pred[,timeVector<=pro.time]

    survs <- t(.pred)[,ot]

    timeVector=timeVector[timeVector<=pro.time]

    bll<-sapply(1:length(timeVector), FUN = function(j)
    {
      help1 <- as.integer(time <= timeVector[j] & obj_surv[ot,2] == 1)
      help2 <- as.integer(time > timeVector[j])
      return(-mean(log(1 - survs[j, ]) * help1 * (1/csurv) +
                     log( survs[j, ]) * help2 * (1/csurv_btime[j])))
    })

    idx <- 2:length(timeVector)
    RET <- diff(timeVector) %*% ((bll[idx - 1] + bll[idx])/2)
    RET <- RET/diff(range(timeVector))
    RET=as.matrix(RET)

    return(RET)
  }

  minus.ci <- function(par, FitCV, data.times, data.failures, time.pred,
                        pro.time) {

    .pred<-array(dim = c(dim(FitCV[[1]]),length(FitCV)))
    .par<-exp(c(par))/(1+sum(exp(par)))

    .par<-c(.par,1-sum(.par))
    for (i in 1:length(FitCV)){
      .pred[,,i]<-FitCV[[i]]*.par[i]
    }
    .pred <- rowSums(.pred, dims=2)

    predicted <- .pred[,time.pred>=pro.time][,1]

    timeVector<-sort(unique(time.pred))

    obj_surv <- Surv(data.times, data.failures)
    time <- obj_surv[, 1]
    status <- obj_surv[, 2]

    permissible <- 0
    concord <- 0
    par_concord <- 0

    n <- length(time)
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if ((time[i] < time[j] &
             status[i] == 0) | (time[j] < time[i] & status[j] == 0)) {
          next
        }

        if (time[i] == time[j] & status[i] == 0 & status[j] == 0) {
          next
        }

        permissible <- permissible + 1

        if (time[i] != time[j]) {
          if ((time[i] < time[j] &
               predicted[i] < predicted[j]) |
              (time[j] < time[i] & predicted[j] < predicted[i])) {
            concord <- concord + 1
          } else if (predicted[i] == predicted[j]) {
            par_concord <- par_concord + 0.5
          }
        }

        if (time[i] == time[j] & status[i] == 1 & status[j] == 1) {
          if (predicted[i] == predicted[j]) {
            concord <- concord + 1
          } else {
            par_concord <- par_concord + 0.5
          }
        }

        if (time[i] == time[j] &
            ((status[i] == 1 &
              status[j] == 0) | (status[i] == 0 & status[j] == 1))) {
          if ((status[i] == 1 &
               predicted[i] < predicted[j]) |
              (status[j] == 1 & predicted[j] < predicted[i])) {
            concord <- concord + 1
          } else {
            par_concord <- par_concord + 0.5
          }
        }
      }
    }

    return( -1 * (concord + par_concord) / permissible)
  }


  ###################################################
  ### Initialisation et recuperation param.tune ###
  ###################################################

  if(sum(!(methods %in% c("LIB_COXlasso", "LIB_COXridge", "LIB_RSF", "LIB_SNN", "LIB_COXen",
                          "LIB_AFTweibull","LIB_AFTweibull","LIB_AFTggamma","LIB_AFTgamma",
                          "LIB_PHgompertz","LIB_PHexponential",
                          "LIB_AFTllogis","LIB_COXaic","LIB_COXall", "LIB_PHspline")))>=1){
    stop("New method is not yet implemented") }

  M <-length((methods))
  N <- length(data[,times])

  if(progress==TRUE){
  max.progess <- M + cv * M + 4
  pb <- txtProgressBar(min = 0, max = max.progess, style = 3, width = 50, char = "=")
  ip <- 0
  setTxtProgressBar(pb, ip)
  }

  names.meth=c(rep(NA,M))
  for(i in unique(methods)){
    if(length(which(methods==i))==1){
      names.meth[which(methods==i)]=i
    }
    else{
      names.meth[which(methods==i)]=paste0(i,1:length(which(methods==i)))
    }
  }

  time.pred <- sort(unique(data[,times]))

  if(is.null(param.tune)==FALSE){
    if(length(param.tune)!=M){
      stop("Param.tune need to have one element per method. Please modifiy param.tune or set it = NULL")
    }
    for (me in 1:M){
      if(is.null(param.tune[[me]])==T & !(methods[me] %in% c("LIB_AFTgamma",
                            "LIB_AFTggamma","LIB_AFTweibull","LIB_AFTllogis","LIB_PHexponential","LIB_PHgompertz"))){
        if(methods[me] %in%"LIB_PHspline"){
          param.tune[[me]]=list(k=1:4)
        }
        if(methods[me] %in%"LIB_COXen"){
          param.tune[[me]]=list(alpha=seq(.1,.9,.1), lambda=NULL)
        }
        if(methods[me] %in%"LIB_COXaic"){
          param.tune[[me]]=list(final.model=NA, model.min=NULL, model.max=NULL)
        }
        if(methods[me] %in%"LIB_SNN"){
          param.tune[[me]]=list(n.nodes=c(2, 3, 4, 6, 10, 20),
                                decay=c(0, 0.01, 0.1),
                                batch.size=256L,
                                epochs=1L)
        }
        if(methods[me] %in% "LIB_COXlasso"){
          param.tune[[me]]=list(lambda=NULL)
        }
        if(methods[me] %in%"LIB_COXridge"){
          param.tune[[me]]=list(lambda=NULL)
        }
        if(methods[me] %in%"LIB_RSF"){
          param.tune[[me]]=list(mtry=(length(group)+length(cov.quanti)+length(cov.quali))/2+2,
                                nodesize=c(2, 4, 6, 10, 20, 30, 50, 100),
                                ntree=500)
        }
      }
    }
  }


  if(is.null(param.tune)){
    param.tune=vector("list",M)
    for (me in 1:M){
      if(methods[me] %in%"LIB_COXen"){
        param.tune[[me]]=list(alpha=seq(.1,.9,.1), lambda=NULL)
      }
      if(methods[me] %in%"LIB_COXaic"){
        param.tune[[me]]=list(final.model=NA, model.min=NULL,model.max=NULL)
      }
      if(methods[me] %in%"LIB_SNN"){
        param.tune[[me]]=list(n.nodes=c(2, 3, 4, 6, 10, 20),
                              decay=c(0, 0.01, 0.1),
                              batch.size=256L,
                              epochs=1L)
      }
      if(methods[me] %in% "LIB_COXlasso"){
        param.tune[[me]]=list(lambda=NULL)
      }
      if(methods[me] %in% "LIB_COXridge"){
        param.tune[[me]]=list(lambda=NULL)
      }
      if(methods[me] %in% "LIB_PHspline"){
        param.tune[[me]]=list(k=1:4)
      }
      if(methods[me] %in% "LIB_RSF"){
        param.tune[[me]]=list(mtry=seq(1,(length(group)+length(cov.quanti)+length(cov.quali))/2+2),
                              nodesize=c(2, 4, 6, 10, 20, 30, 50, 100), ntree=500)
      }
    }
  }

  if(is.null(pro.time)==T & sum(metric %in% c("ci","bs","bll","ribs","ribll","roc"))){
    pro.time=median(data[,times])
  }

  .model<-vector("list",M)
  names(.model)<-names.meth

  .tune.optimal<-vector("list",M)
  names(.tune.optimal)<-names.meth

  .tune.results<-vector("list",M)
  names(.tune.results)<-names.meth


  for (me in 1:M){

    if(methods[me] == "LIB_AFTweibull" ){
      .tune.optimal[[me]]=NA

      .LIB_AFTweibull <- LIB_AFTweibull(times=times, failures=failures, group=group,
                                  cov.quanti=cov.quanti, cov.quali=cov.quali, data=data)

      .model[[me]]<-.LIB_AFTweibull

      if(progress==TRUE){
        ip <- ip+1
        setTxtProgressBar(pb, ip)
        }

      rm(.LIB_AFTweibull) }

    if(methods[me] == "LIB_AFTggamma"){
      .tune.optimal[[me]]=NA

      .LIB_AFTggamma <- LIB_AFTggamma(times=times, failures=failures, group=group,
                                cov.quanti=cov.quanti, cov.quali=cov.quali, data=data)

      .model[[me]]<-.LIB_AFTggamma

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_AFTggamma) }

    if(methods[me] == "LIB_AFTgamma" ){
      .tune.optimal[[me]]=NA

      .LIB_AFTgamma <- LIB_AFTgamma(times=times, failures=failures, group=group,
                              cov.quanti=cov.quanti, cov.quali=cov.quali, data=data)

      .model[[me]]<-.LIB_AFTgamma

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_AFTgamma)  }


    if(methods[me] == "LIB_AFTllogis" ){
      .tune.optimal[[me]]=NA

      .LIB_AFTllogis <- LIB_AFTllogis(times=times, failures=failures, group=group,
                              cov.quanti=cov.quanti, cov.quali=cov.quali, data=data)

      .model[[me]]<-.LIB_AFTllogis
      rm(.LIB_AFTllogis)    }

    if(methods[me] == "LIB_PHgompertz" ){
      .tune.optimal[[me]]=NA

      .LIB_PHgompertz <- LIB_PHgompertz(times=times, failures=failures, group=group,
                                  cov.quanti=cov.quanti, cov.quali=cov.quali, data=data)

      .model[[me]]<-.LIB_PHgompertz

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_PHgompertz) }

    if(methods[me] == "LIB_PHexponential"){
      .tune.optimal[[me]]=NA

      .LIB_PHexponential <- LIB_PHexponential(times=times, failures=failures, group=group,
                                        cov.quanti=cov.quanti, cov.quali=cov.quali, data=data)

      .model[[me]]<-.LIB_PHexponential

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_PHexponential) }

    if(methods[me] == "LIB_COXall"){
      .tune.optimal[[me]]=NA

      .LIB_COXall <- LIB_COXall(times=times, failures=failures, group=group,
                                        cov.quanti=cov.quanti, cov.quali=cov.quali, data=data)

      .model[[me]]<-.LIB_COXall

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_COXall) }

    if(methods[me] == "LIB_PHspline"){

      if(is.null(param.tune[[me]]$k)==T | length(param.tune[[me]]$k)>1){

        .tune<- tunePHspline(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                               cov.quali=cov.quali, data=data, cv=cv,
                               k=param.tune[[me]]$k)
        .tune.optimal[[me]]=.tune$optimal
        .tune.results[[me]]=.tune$results
        rm(.tune)  }

      else{ .tune.optimal[[me]]=list(lambda=param.tune[[me]]$k) }


      .LIB_PHspline <- LIB_PHspline(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                              cov.quali=cov.quali, data=data, k=.tune.optimal[[me]]$k)

      .model[[me]]<-.LIB_PHspline


      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_PHspline)    }

    if(methods[me] == "LIB_COXlasso"){

      if(is.null(param.tune[[me]]$lambda)==T | length(param.tune[[me]]$lambda)>1){

        .tune<- tuneCOXlasso(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                               cov.quali=cov.quali, data=data, cv=cv,
                               parallel=FALSE, lambda=param.tune[[me]]$lambda)

        .tune.optimal[[me]]=.tune$optimal
        .tune.results[[me]]=.tune$results
        rm(.tune)  }

      else{ .tune.optimal[[me]]=list(lambda=param.tune[[me]]$lambda) }


      .LIB_COXlasso <- LIB_COXlasso(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                              cov.quali=cov.quali, data=data, lambda=.tune.optimal[[me]]$lambda)

      .model[[me]]<-.LIB_COXlasso

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_COXlasso)  }

    if(methods[me] == "LIB_COXridge"){

      if(is.null(param.tune[[me]]$lambda)==T | length(param.tune[[me]]$lambda)>1){
        .tune<- tuneCOXridge(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                               cov.quali=cov.quali,  data=data, cv=cv,
                               parallel = FALSE, lambda=param.tune[[me]]$lambda)
        .tune.optimal[[me]]<-.tune$optimal
        .tune.results[[me]]<-.tune$results
        rm(.tune)
      }
      else{
        .tune.optimal[[me]]=list(lambda=param.tune[[me]]$lambda)
      }

      .LIB_COXridge <- LIB_COXridge(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                              cov.quali=cov.quali, data=data,
                              lambda=.tune.optimal[[me]]$lambda)
      .model[[me]]<-.LIB_COXridge

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_COXridge)    }

    if(methods[me] == "LIB_COXen"){

      if(length(param.tune[[me]]$alpha)==1 & length(param.tune[[me]]$lambda)==1){
        .tune.optimal[[me]]=list(alpha=param.tune[[me]]$alpha, lambda=param.tune[[me]]$lambda)
      }
      else{
        .tune <- tuneCOXen(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                             cov.quali=cov.quali, data=data, cv=cv,
                             parallel=FALSE,
                             alpha=param.tune[[me]]$alpha,
                             lambda=param.tune[[me]]$lambda)
        .tune.optimal[[me]]<-.tune$optimal
        .tune.results[[me]]<-.tune$results

        rm(.tune) }

      .LIB_COXen <- LIB_COXen(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                        cov.quali=cov.quali, data=data, alpha=.tune.optimal[[me]]$alpha,
                        lambda=.tune.optimal[[me]]$lambda)

      .model[[me]]<-.LIB_COXen

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_COXen) }

    if(methods[me] == "LIB_COXaic"){

      if(is.na(param.tune[[me]]$final.model)==FALSE){
        .tune.optimal[[me]]=list(final.model=param.tune[[me]]$final.model)
      }
      else{

        .tune <- tuneCOXaic(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                             cov.quali=cov.quali, data=data,
                             model.min=param.tune[[me]]$model.min,
                             model.max=param.tune[[me]]$model.max)
        .tune.optimal[[me]]<-.tune$optimal
        .tune.results[[me]]<-.tune$results

        rm(.tune) }

      .LIB_COXaic<- LIB_COXaic(times=times, failures=failures, group=group, data=data,
                       cov.quanti=cov.quanti, cov.quali=cov.quali,
                       final.model = .tune.optimal[[me]]$final.model)

      .model[[me]]<-.LIB_COXaic

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_COXaic)  }

    if (methods[me] == "LIB_RSF"){

      if(length(param.tune[[me]]$nodesize)!=1 | length(param.tune[[me]]$mtry)!=1 |
         length(param.tune[[me]]$ntree)!=1){
        .tune<-tuneRSF(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                            cov.quali=cov.quali, data=data,
                            nodesize=param.tune[[me]]$nodesize,
                            mtry=param.tune[[me]]$mtry,
                            ntree=param.tune[[me]]$ntree)

        .tune.optimal[[me]]<-.tune$optimal
        .tune.results[[me]]<-.tune$results
        rm(.tune)
      }
      else{
        .tune.optimal[[me]]<-list(nodesize=param.tune[[me]]$nodesize,
                                  mtry=param.tune[[me]]$mtry,
                                  ntree=param.tune[[me]]$ntree)
      }
      .LIB_RSF <-LIB_RSF(times=times, failures=failures,
                         group=group, cov.quanti=cov.quanti, cov.quali=cov.quali, data=data,
                         nodesize=.tune.optimal[[me]]$nodesize,
                         mtry=.tune.optimal[[me]]$mtry, ntree=.tune.optimal[[me]]$ntree)

      .model[[me]]<-.LIB_RSF

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_RSF) }


    if (methods[me] == "LIB_SNN"){
      torch<-reticulate::import("torch")
      torch$set_num_threads(1L)

      if(length(param.tune[[me]]$n.nodes)!=1 | length(param.tune[[me]]$decay)!=1 |
         length(param.tune[[me]]$batch.size)!=1 |length(param.tune[[me]]$epochs)!=1 ){

        .tune <- tuneSNN(times=times, failures=failures, group=group,
                               cov.quanti=cov.quanti, cov.quali=cov.quali,
                               data=data, cv=cv,
                               n.nodes=param.tune[[me]]$n.nodes,
                               decay=param.tune[[me]]$decay,
                               batch.size=param.tune[[me]]$batch.size,
                               epochs=param.tune[[me]]$epochs)
        .tune.optimal[[me]]<-.tune$optimal
        .tune.results[[me]]<-.tune$results
        rm(.tune)
      }
      else{
        .tune.optimal[[me]]<-list(n.nodes=param.tune[[me]]$n.nodes,
                                  decay=param.tune[[me]]$decay,
                                  batch.size=param.tune[[me]]$batch.size,
                                  epochs=param.tune[[me]]$epochs)
      }

      .LIB_SNN <-LIB_SNN(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                         cov.quali=cov.quali, data=data,
                         n.nodes=as.numeric(.tune.optimal[[me]]$n.nodes),
                         decay=as.numeric(.tune.optimal[[me]]$decay),
                         batch.size=as.integer(.tune.optimal[[me]]$batch.size),
                         epochs=as.integer(.tune.optimal[[me]]$epochs))


      .model[[me]]<-.LIB_SNN

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

      rm(.LIB_SNN)    }
  }

  ########################
  ### Cross-Validation  ##
  ########################

  sample_id=sample(nrow(data))
  folds <- cut(seq(1,nrow(data)), breaks=cv, labels=FALSE)
  folds_id=folds[sample_id]
  data$folds=folds_id

  CV<-vector("list",cv*M)
  j<-1
  for(m in 1:M){
    for (k in 1:cv){
      CV[[j]]<-list(train= data[data$folds!=k, ],valid=data[data$folds==k, ], num_method=m)
      j<-j+1
    }
  }

  CV_all_method<-function(CV, method, Tune,
                          times, failures, group, cov.quanti, cov.quali,time.pred){
    num_method<-CV$num_method
    meth<-method[num_method]
    if(meth == "LIB_AFTweibull"){
      fit<-LIB_AFTweibull(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                       cov.quali=cov.quali, data=CV$train)
      pred=predict(fit,  newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth == "LIB_AFTggamma"){
      fit<-LIB_AFTggamma(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                      cov.quali=cov.quali, data=CV$train)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth == "LIB_AFTgamma"){
      fit<-LIB_AFTgamma(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                     cov.quali=cov.quali, data=CV$train)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth == "LIB_AFTllogis"){
      fit<-LIB_AFTllogis(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                     cov.quali=cov.quali, data=CV$train)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth == "LIB_PHgompertz"){
      fit<-LIB_PHgompertz(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                       cov.quali=cov.quali, data=CV$train)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth == "LIB_PHexponential"){
      fit<-LIB_PHexponential(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                          cov.quali=cov.quali, data=CV$train)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth == "LIB_PHspline"){
      fit<-LIB_PHspline(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                     cov.quali=cov.quali, data=CV$train,
                     k=Tune[[num_method]]$k)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth == "LIB_COXlasso"){
      fit<-LIB_COXlasso(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                     cov.quali=cov.quali, data=CV$train,
                     lambda=Tune[[num_method]]$lambda)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth == "LIB_COXen"){
      fit<-LIB_COXen(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                 cov.quali=cov.quali, data=CV$train,
                  alpha=Tune[[num_method]]$alpha, lambda=Tune[[num_method]]$lambda)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    if(meth =="LIB_COXridge"){
      fit<-LIB_COXridge(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                    cov.quali=cov.quali, data=CV$train, lambda=Tune[[num_method]]$lambda)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions

    }
    if(meth =="LIB_COXaic"){
      fit<-LIB_COXaic(times=times, failures=failures, group=group, data=data,
                   final.model = Tune[[num_method]]$final.model, cov.quanti=cov.quanti,
                   cov.quali=cov.quali)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions

    }
    if(meth =="LIB_COXall"){
      fit<-LIB_COXall(times=times, failures=failures, group=group,
                   cov.quanti=cov.quanti, cov.quali=cov.quali, data=data)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions

    }
    if(meth =="LIB_RSF"){
      fit<-LIB_RSF(times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                   cov.quali=cov.quali,  data=CV$train,
                   nodesize=Tune[[num_method]]$nodesize, mtry=Tune[[num_method]]$mtry,
                   ntree=Tune[[num_method]]$ntree)
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions

    }
    if(meth =="LIB_SNN"){
      fit<-LIB_SNN(times=times, failures=failures, group=group,  cov.quanti=cov.quanti, cov.quali=cov.quali, data=CV$train,
                     n.nodes=as.numeric(Tune[[num_method]]$n.nodes),
                     decay=as.numeric(Tune[[num_method]]$decay),
                     batch.size=as.integer(Tune[[num_method]]$batch.size),
                     epochs=as.integer(Tune[[num_method]]$epochs))
      pred<-predict(fit,newtimes=time.pred, newdata=CV$valid)$predictions
    }
    return(pred)
  }

  preFitCV<-lapply(CV, CV_all_method,method=methods,
                   Tune=.tune.optimal, times=times, failures=failures, group=group, cov.quanti=cov.quanti,
                   cov.quali=cov.quali, time.pred=time.pred)

  FitCV<-vector("list", M)
  for(m in 1:M){
    FitCV[[m]]<-matrix(nrow=dim(.model[[1]]$predictions)[1], ncol= dim(.model[[1]]$predictions)[2])
    for (j in 1:cv){
      FitCV[[m]][data$folds==j,]<-preFitCV[[(m-1)*cv+j]]

      if(progress==TRUE){
      ip <- ip+1
      setTxtProgressBar(pb, ip)
      }

    }
  }
  names(FitCV)<-names.meth

  rm(preFitCV, CV)

  ################
  # OPTIMISATION #
  ################

  data.times <- data[,times]
  data.failures <- data[,failures]
  timeVector <- survfit(Surv(data[,times],data[,failures])~ 1 )$time

  obj_surv <- Surv(data.times, data.failures)

  time <- obj_surv[, 1]
  ot <- order(time)
  cens <- obj_surv[ot, 2]
  time <- time[ot]

  .temp <- survfit(Surv(time, cens==0) ~ 1)
  csurv <- summary(.temp, times=time, extend=TRUE)$surv

  csurv[csurv == 0] <- Inf

  csurv_btime <- summary(.temp, times=timeVector, extend=TRUE)$surv

  csurv_btime[is.na(csurv_btime)] <- min(csurv_btime, na.rm = TRUE)
  csurv_btime[csurv_btime == 0] <- Inf

  .optim.method="Nelder-Mead"
  if(M==2){  .optim.method="BFGS"  }

  if(is.null(param.weights.fix)==TRUE){

      switch(metric,
             ibs={
               estim<-optim(par=param.weights.init, fn=ibs, FitCV = FitCV,
                            timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                            csurv_btime=csurv_btime,time=time,hessian=F,
                            method=.optim.method)

               if(optim.local.min==T){
                 start_par=estim$par
                 estim<-optim(par=start_par, fn=ibs, FitCV = FitCV,
                              timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                              csurv_btime=csurv_btime, time=time,hessian=F,
                              method=.optim.method)
               }
             },
             bs={
               if(is.null(pro.time)){
                 pro.time=median(data[,times])
               }
               estim<-optim(par=param.weights.init, fn=brs, FitCV = FitCV,
                            timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                            csurv_btime=csurv_btime,time=time,pro.time=pro.time,hessian=F,
                            method=.optim.method)

               if(optim.local.min==T){
                 start_par=estim$par
                 estim<-optim(par=start_par, fn=brs, FitCV = FitCV,
                              timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                              csurv_btime=csurv_btime,time=time,pro.time=pro.time,hessian=F,
                              method=.optim.method)
               }
             },
             auc={
               if(is.null(pro.time)){
                 pro.time=median(data[,times])
               }
               estim<-optim(par=param.weights.init, fn=minus.roc, FitCV = FitCV,
                            timeVector=timeVector,  obj_surv=obj_surv, ot=ot, ROC.precision = ROC.precision,
                            time=time,pro.time=pro.time,hessian=F,
                            method=.optim.method)

               if(optim.local.min==T){
                 start_par=estim$par
                 estim<-optim(par=start_par, fn=minus.roc, FitCV = FitCV,  ROC.precision = ROC.precision,
                              timeVector=timeVector,  obj_surv=obj_surv, ot=ot,
                              time=time,pro.time=pro.time,hessian=F,
                              method=.optim.method)
               }
             },
             ibll={
               estim<-optim(par=param.weights.init, fn=ibll, FitCV = FitCV,
                            timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                            csurv_btime=csurv_btime,time=time,hessian=F,
                            method=.optim.method)

               if(optim.local.min==T){
                 start_par=estim$par
                 estim<-optim(par=start_par, fn=ibll, FitCV = FitCV,
                              timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                              csurv_btime=csurv_btime, time=time,hessian=F,
                              method=.optim.method)
               }
             },
             bll={
               if(is.null(pro.time)){
                 pro.time=median(data[,times])
               }
               estim<-optim(par=param.weights.init, fn=bll, FitCV = FitCV,
                            timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                            csurv_btime=csurv_btime,time=time,pro.time=pro.time,hessian=F,
                            method=.optim.method)

               if(optim.local.min==T){
                 start_par=estim$par
                 estim<-optim(par=start_par, fn=bll, FitCV = FitCV,
                              timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                              csurv_btime=csurv_btime,time=time,pro.time=pro.time,hessian=F,
                              method=.optim.method)
               }
             },
             ribs={
               if(is.null(pro.time)){
                 pro.time=median(data[,times])
               }
               estim<-optim(par=param.weights.init, fn=ribs, FitCV = FitCV,
                            timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                            csurv_btime=csurv_btime,time=time,pro.time=pro.time,hessian=F,
                            method=.optim.method)

               if(optim.local.min==T){
                 start_par=estim$par
                 estim<-optim(par=start_par, fn=ribs, FitCV = FitCV,
                              timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                              csurv_btime=csurv_btime,time=time,pro.time=pro.time,hessian=F,
                              method=.optim.method)
               }
             },
             ribll={
               if(is.null(pro.time)){
                 pro.time=median(data[,times])
               }
               estim<-optim(par=param.weights.init, fn=ribll, FitCV = FitCV,
                            timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                            csurv_btime=csurv_btime,time=time,pro.time=pro.time,hessian=F,
                            method=.optim.method)

               if(optim.local.min==T){
                 start_par=estim$par
                 estim<-optim(par=start_par, fn=ribll, FitCV = FitCV,
                              timeVector=timeVector,  obj_surv=obj_surv, ot=ot, csurv=csurv,
                              csurv_btime=csurv_btime,time=time,pro.time=pro.time,hessian=F,
                              method=.optim.method)
               }
             },
             ci={
               if(is.null(pro.time)){
                 pro.time=median(data[,times])
               }
               estim<- optim(par=param.weights.init, fn=minus.ci, FitCV = FitCV,
                             data.times =  data.times, data.failures = data.failures,
                             time.pred=time.pred, pro.time=pro.time, hessian=F, method=.optim.method)
               if(optim.local.min==T){
                 start_par=estim$par
                 estim<-optim(par=start_par,fn=minus.ci, FitCV = FitCV, data.times =  data.times,
                              data.failures = data.failures, time.pred=time.pred,
                              pro.time=pro.time, hessian=F, method=.optim.method)
               }
             }
      )
  }

  if(progress==TRUE){
  ip <- ip+3
  setTxtProgressBar(pb, ip)
  }

  ############################
  # Compute Survival from SL #
  ############################

  if(is.null(param.weights.fix)==FALSE){
    estim=list()
    estim$par=param.weights.fix
  }

  FitALL<-vector("list",M)
  names(FitALL)<-names.meth

  for(me in 1:M){
    FitALL[[me]]<-.model[[me]]$predictions
  }

w.sl <- c(exp(c(estim$par,0)) / ( 1+sum(exp(estim$par))) )
.SL<-array(dim = c(dim(FitALL[[1]]),length(FitALL)))
for (i in 1:length(FitCV)){ .SL[,,i]<-.model[[i]]$predictions*w.sl[i]  }
surv.SL <-rowSums(.SL, dims=2)


  ######################
  # PREPARATION RETURN #
  ######################

  if(keep.predictions==TRUE) {
    FitALL<-vector("list",M)
    names(FitALL)<-names.meth

    for(me in 1:M){
      FitALL[[me]]<-.model[[me]]$predictions
    }

    FitALL$sl<-surv.SL
    temp.predictions <- FitALL}

  else {
    temp.predictions <- surv.SL
    temp.predictions<-as.data.frame(temp.predictions)
  }

if(progress==TRUE){
ip <- ip+1
setTxtProgressBar(pb, ip)
}

  res<-list(times=time.pred,
            predictions=temp.predictions,
            data=data.frame(times=data[,times], failures=data[,failures],
                            data[, !(dimnames(data)[[2]] %in% c(times, failures))]),
            outcomes=list(times=times, failures=failures),
            predictors=list(group=group, cov.quanti=cov.quanti, cov.quali=cov.quali),
            ROC.precision=ROC.precision,
            cv=cv,
            pro.time=pro.time,
            methods=methods,
            models=.model,
            weights=list(coefficients=estim$par, values=w.sl),
            metric=list(metric=metric, value=estim$value),
            param.tune=list(optimal=.tune.optimal, results=.tune.results))

  class(res) <- "sltime"

  if(progress==TRUE){ close(pb) }

  return(res)
}

