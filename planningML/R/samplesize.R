#' Sample size determination
#'
#' This function determine the optimal sample size based on the performance evaluation metric and number of selected features.
#'
#'
#' @param features feature selection results from the featureselection function in the package.
#' @param sample.size sample size grid
#' @param method default is HCT method, sample size dependent performance metric based on HCT method (HCT) or DS method (DS).
#' @param m the number of features involved in the sample size determination. Default is NULL, which means
#'          the number of features are determined by the featureselection results based on the iHCT method.
#'          Otherwise, users can select the number based on their needs. The self-defined m should be smaller
#'          than the optimal number of features determined by the featureselection function.
#' @param effectsize common effect size the the m features. NULL means the effect size is directly calculated from the
#'                   data. Users can also provide the effect sizes based on historical data.
#' @param class.prob probability of the event
#' @param totalnum_features total number of features
#' @param threshold default = 0.1. Threshold needed to determine the sample size.
#' @param metric default = "MCC". The target performance estimation metric that you want to optimize. Other choices
#'               can be AUC.
#' @param target target MCC/AUC that you want to achieve
#' @return \code{samplesize()} returns sample size needed to achieve corresponding performance measurements.
#' @import caret lubridate
#' @importFrom MESS auc
#' @importFrom stats cor pnorm pt qnorm qt quantile rbeta rnorm runif sd var
#'
#' @examples
#' # first compute the results of featureselection function:
#' ## load data
#' ## Please be noted that the "pilot_sub.rds" dataset is only include 500 features out
#' ## of 10108 features of the whole dataset.
#' ## It is created for R package testing only. To access the full dataset, please use the
#' ##"pilotdata.rds" instead.
#'
#' pilot.data = readRDS(system.file("extdata", "pilotdata_sub.rds", package = "planningML"))
#' x = pilot.data[,-ncol(pilot.data)]
#' y = pilot.data$DEPRESSION
#'
#' # select important features
#' # features = featureselection(x = x, y = y)
#'
#' # determine sample size
#'
#'    #output = samplesize(features=features,
#'    #               method="HCT", m=length(features$features),
#'    #               effectsize=NULL, class.prob = NULL, totalnum_features = NULL,
#'    #               threshold=0.1, metric="MCC", target = NULL)
#'    #output
#'    #summary(output)
#'    #plot(output) # Plot sample size dependent AUC or MCC based on number of selected features
#' }
#'
#' @export


samplesize = function(features = NULL,sample.size=seq(10,1000,20),
                      method="HCT", m=NULL, effectsize=NULL, class.prob = NULL, totalnum_features = NULL,
                      threshold=0.1, metric="MCC", target = NULL){

  if (!is.null(features)) {
    # obtain information from featureselection
    x = features$x
    y = features$y
    pilot.data = features$data
    index = features$index
    selectnum = features$selectnum
    selected_features = features$features

    delta.pilot = features$delta.pilot


  } else {

    if (method != "HCT"){
      warning("Sample size determination method must be HCT if the feature selection step is skipped!")
    }

    if (!is.null(effectsize)){
      delta.pilot = effectsize/2
    } else {
      warning("Effect size is missing!")
    }
  }

  weights<-function(zscores,lambda)
  {
    w<-ifelse(abs(zscores) > lambda , (1*sign(zscores)),0)
    return(weights=w)
  }



  # Function for calculating pooled variance
  pooled.variance<-function(df.1,df.2)
  {
    #calculate sample size of each group
    n1 <- nrow(df.1)
    n2 <- nrow(df.2)
    #calculate sample variance of each group
    var1 <- apply(df.1, 2, var)
    var2 <- apply(df.2, 2, var)
    #calculate pooled variance between the two groups
    pooled <- ((n1-1)*var1 + (n2-1)*var2) / (n1+n2-2)
    return(data.frame(pooled.variance = pooled, pooled.sd = sqrt(pooled)))
  }

  # determine interested number of features
  if (is.null(m)){
    num_m = length(selected_features)
  } # otherwise, use the self-determined m

  # Sample size determination with DS method

  if (method == "DS"){

    # Compute sd's for positive and negative data
    #pos<-vx_new[which(vx_new[,ncol(vx_new)]==1),-ncol(vx_new)]
    #neg<-vx_new[which(vx_new[,ncol(vx_new)]==0),-ncol(vx_new)]
    pos<-pilot.data[which(pilot.data[,ncol(pilot.data)]==1),-ncol(pilot.data)]
    neg<-pilot.data[which(pilot.data[,ncol(pilot.data)]==0),-ncol(pilot.data)]

    #Calculate sigma.delta as a function of m
    sigma<-vector(length=length(m))
    delta<-vector(length=length(m))
    effect.sizes<-vector(length=length(m))
    #eigen.val<-vector(length=length(m))
    PCC.inf.balanced<-vector(length=length(m))
    for(i in 1:length(m))
    {
      sigma[i]=quantile(pooled.variance(pos,neg)[index[1:m[i]],2],probs=0.50, na.rm = TRUE)
      #delta[i] = quantile(delta.pilot[1:m[i]],probs = 0.50)
      delta[i] = mean(delta.pilot[1:m[i]])
      effect.sizes[i] = 2*delta[i]/sigma[i]
      #  eigen.val[i] = max(tau_estimate(as.matrix(pilot.data[,index[1:m[i]]])))
      #  PCC.inf.balanced[i] = pnorm(((delta[i]/sigma[i]) *(m[i]/eigen.val[i])), 0,1,lower.tail=TRUE)
    }

    class.prob=length(which(y==1))/nrow(pilot.data)
    class.prob.grid = c(10^(-5),seq(0.01,1,0.01))

    weights<-function(zscores,lambda)
    {
      w<-ifelse(abs(zscores) > lambda , (1*sign(zscores)),0)
      return(weights=w)
    }
    #####################################################################################
    #Calculation using DS method
    # source("calculate_PCC_by_DS_Updated.R")
    # load_all()
    eigen.value =1

    k <- sample.size

    if (metric=="MCC"){
      MCC.mn<-matrix(NA, nrow= length(k), ncol=length(m), byrow= TRUE)
      for(i in 1: length(k))
      {
        for(j in 1: length(m))
        {
          MCC.mn[i,j]<- calculate_MCC_by_DS(n = k[i], m= m[j], p=ncol(x),eigen.val = eigen.value,
                                            delta = delta[j], sigma =sigma[j],
                                            alpha.grid = seq(0.01,0.5,0.01), class.prob = class.prob)[2]
        }
      }
      metric_out <- MCC.mn
    }


    if (metric=="AUC"){
      AUC.mn<-matrix(NA, nrow= length(k), ncol=length(m), byrow= TRUE)

      for(i in 1: length(k))
      {
        for(j in 1: length(m))
        {
          AUC.mn[i,j]<- calculate_AUC_by_DS(n = k[i], m= m[j], p=ncol(x),eigen.val = eigen.value,
                                            delta = delta[j], sigma =sigma[j],
                                            alpha.grid = seq(0.01,0.5,0.01),
                                            class.prob.grid = class.prob.grid)[3]
        }
      }
      metric_out <- AUC.mn
    }

    # if (metric=="AUPRC"){
    #   AUPRC.mn<-matrix(NA, nrow= length(k), ncol=length(m), byrow= TRUE)
    #
    #   for(i in 1: length(k))
    #   {
    #     for(j in 1: length(m))
    #     {
    #       AUPRC.mn[i,j]<- calculate_AUC_by_DS(n = k[i], m= m[j], p=ncol(x),eigen.val = eigen.value,
    #                                           delta = delta[j], sigma =sigma[j],
    #                                           alpha.grid = seq(0.01,0.5,0.01),
    #                                           class.prob.grid = (class.prob.grid))[4]
    #
    #     }
    #   }
    #   metric_out <- AUPRC.mn
    # }

  }


  # Sample size determination with HCT method

  if (method == "HCT"){

    if (is.null(class.prob)){
      class.prob=length(which(y==1))/nrow(pilot.data)
    }
    class.prob.grid = c(10^(-5),seq(0.01,1,0.01))

    HCT.simulation<-function(n,m,p,delta.pilot,alpha_0=0.1, class.prob)
    {
      kappa=0.5*log((1-class.prob)/class.prob)
      mu_0 = mean(delta.pilot[1:m])
      #mu_0=0.4
      denom = sqrt((1/ceiling(n*(1-class.prob))) + (1/floor(n*(class.prob))))
      #z<-rnorm(m,(2*mu_0)/denom,1)
      z<-sapply(delta.pilot[1:m], function(x) rnorm(1,( 2*x/denom),1))
      #pp<-2*(1-pnorm(abs(z),0,1,lower.tail = TRUE))
      pp <- mapply(function(x,y) 2*(1-pnorm(abs(x),y,1,lower.tail = TRUE)), z,delta.pilot[1:m])
      u<-rbeta(1,ceiling(p*alpha_0),p-m+1-ceiling(p*alpha_0))
      nu=runif(ceiling(p*alpha_0)-1,0,u)
      pvalue <-sort(c(pp,nu,u),decreasing = FALSE)
      k <-seq(1,ceiling(p*alpha_0),1)
      val = sqrt(p)*(((k/p) - pvalue[1: ceiling(p*alpha_0)]) / sqrt ((k/p)*(1-(k/p))))
      l= which.max(val)
      lambda = abs(qnorm(pvalue[l]/2,lower.tail = TRUE))
      #Calculate weights of m features using t-scores and lambda
      weights.HCT = weights(z,lambda=lambda)
      tot.weight = sum(ifelse( pvalue[1:ceiling(p*alpha_0)] < 2* (1-pnorm(abs(lambda))),1,0))
      PCC = class.prob* pnorm((mu_0*sum(weights.HCT) -kappa)/sqrt(tot.weight)) +
        (1-class.prob) * pnorm((mu_0*sum(weights.HCT) +kappa)/sqrt(tot.weight))
      TPR = pnorm((mu_0*sum(weights.HCT) -kappa)/sqrt(tot.weight))
      TNR = pnorm((mu_0*sum(weights.HCT) +kappa)/sqrt(tot.weight))
      PPV = (class.prob*TPR)/ (( class.prob*TPR) + ((1-class.prob)*(1-TNR)))
      NPV = ((1-class.prob)* TNR) /(((1-class.prob)* TNR) + (class.prob*(1-TPR)))
      FScore = (2*PPV*TPR) /(TPR + PPV)
      MCC = sqrt(PPV*TPR*NPV*TNR) - sqrt((1-PPV)*(1-TPR)*(1-TNR)*(1-NPV))
      class.prob.grid = seq(0.01,1.00,0.01)
      kappa.grid = sapply(class.prob.grid, function(x) 0.5*log((1-x)/x))
      TPR.grid =  sapply( kappa.grid, function(x) pnorm((mu_0*sum(weights.HCT) - x)/sqrt(tot.weight)))
      TNR.grid = sapply(kappa.grid, function(x) pnorm((mu_0*sum(weights.HCT) + x)/sqrt(tot.weight)))
      PPV.grid = (class.prob.grid*TPR.grid)/ (( class.prob.grid*TPR.grid) + ((1-class.prob.grid)*(1-TNR.grid)))
      AUC = auc(x=1-TNR.grid, y=TPR.grid, type="spline")
      #AUPRC = MESS::auc(x= TPR.grid, y= PPV.grid, type="spline")
      return(c(PCC=PCC,FScore =  FScore, AUC= AUC, #AUPRC=AUPRC
               MCC=MCC))
    }

    # Calculate HCT_PCC and FScore for different choices of m and n
    N=1000
    #sample.size <- seq(10,1000,20) # sample size determined by user?
    imp.features <- m
    # imp.features <- c(5,10,20,length(index))
    if (is.null(totalnum_features)){
      num_features = ncol(x)
    } else {
      num_features = totalnum_features
    }

    AUC.HCT.simulation.mean<-matrix(NA,ncol= length(imp.features), nrow= length(sample.size), byrow=TRUE)
    MCC.HCT.simulation.mean<-matrix(NA,ncol= length(imp.features), nrow= length(sample.size), byrow=TRUE)
    for(j in 1: length(sample.size))
    {
      for(k in 1:length(imp.features))
      {

        calc_score <-replicate(N, HCT.simulation(n=sample.size[j],m=imp.features[k],p=num_features,
                                                 delta.pilot=delta.pilot,class.prob=class.prob), simplify=FALSE)
        AUC.HCT.simulation.mean[j,k] = mean(sapply(calc_score, "[[",3),na.rm=TRUE)
        MCC.HCT.simulation.mean[j,k] = mean(sapply(calc_score, "[[",4),na.rm=TRUE)
      }
    }

    if (metric == "MCC"){
      metric_out <- MCC.HCT.simulation.mean
    }
    if (metric == "AUC"){
      metric_out <- AUC.HCT.simulation.mean
    }
  }

  column_names = paste("m =", m)
  colnames(metric_out) = column_names

  row_names = paste("n =", sample.size)
  rownames(metric_out) = row_names

  ## determine sample size:

  optimalindex = apply(as.matrix(metric_out), 2, function(x) which.max(x[length(x)] - x < threshold))
  # out = rbind(metric_out,  sample.size[optimalindex])
  # rownames(out)[nrow(out)] = "Minimum sample size"
  out = metric_out

  minimumsamplesize = sample.size[optimalindex]
  names(minimumsamplesize) = column_names



  if (is.null(target)){
    output = list(m = m,
                  metric = metric,
                  samplesize = sample.size,
                  outtable = out,
                  minimum.samplesize = minimumsamplesize)
  } else {
    targetsize = c()
    for (i in 1:ncol(out)){
      out1 <- out[,i]
      targetsize <- c(targetsize,out1[which(out1 > target)[1]])
    }

    output = list(m = m,
                  metric = metric,
                  samplesize = sample.size,
                  outtable = out,
                  minimum.samplesize = minimumsamplesize,
                  Sample.size.for.target.metric = targetsize)
  }

  class(output)<-"planningML"

  return(output)



}



