#' Rank the regression models based on the confidence envelope for minimum ZIC
#'
#' @param data a matrix of \eqn{n} by \eqn{(m+1)} where \eqn{m} is the number of independent variables. First column should be the dependent variable and the rest of the \eqn{m} columns should be the independent variables of the dataset. Maximum of \eqn{m} should be 10.
#' @param alphaval confidence limit of the confidence envelope (Default is 0.95).
#' @param model_ZIC type of the information criterion, it can be "AIC", "BIC", or "AICc" (Default is the "AIC").
#'
#' @return A list containing at least the following components.
#' \item{Ranked_Models}{A set of top ranked models which lie in the confidence envelop \eqn{CE(\alpha)} (with variables list and the ranked ZIC values ("AIC", "BIC", or "AICc")) for regression data.
#' \eqn{0} represents the coefficient while \eqn{1,2,...,m} give the corresponding columns of independent variables \eqn{X_1,X_2,...,X_m} respectively.}
#'
#' \item{Confidence_Envelope}{gives the confidence envelope \eqn{CE(\alpha)} for the minimum ZIC.}
#'
#' \item{Confidence_Limit}{the confidence limit, \eqn{1-\alpha}.}
#'
#' \item{Total_Models}{number of total fitted models.}
#' @export
#'
#' @description Narrow down the number of models to look at in model selection using the confidence envelope based on the minimum ZIC values for regression data. Here, we compute the ZIC values ("AIC", "BIC", or "AICc") for regression data, confidence envelope for the minimum ZIC values for the given confidence limit, and rank the best models which lie in the confidence envelope.
#'
#' @details This program involves the computation of multivariate normal-probabilities with covariance matrices based on minimum ZIC inverting the CDF of the minimum ZIC. It involves both the computation of singular and nonsingular probabilities. The methodology is described in Genz (1992).
#' @details Let \eqn{X_j} be the ZIC value for the \eqn{j^{th}} fitted model. Compute the cdf values of the minimum ZIC, \eqn{F_{X_{(1)}}(\cdot)} numerically and then obtain the \eqn{100\cdot (1-\alpha)\%} confidence envelope:
#' \deqn{CE(\alpha)=F^{-1}_{X_{(1)}}(1-\alpha)}
#' @details See details:
#' @details Jayaweera I.M.L.N, Trindade A.A., ``How Certain are You in Your Minimum AIC and BIC Values?", Sankhya A (2023+)
#'
#' @usage RankReg(data,alphaval=0.95, model_ZIC="AIC")
#'
#' @importFrom cmna bisection
#' @importFrom stats lm
#' @importFrom MuMIn dredge
#' @importFrom mvtnorm pmvnorm
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#'
#' @references Genz, A. (1992). Numerical computation of multivariate normal probabilities. Journal of computational and graphical statistics, 1(2), 141-149.
#'
#' @examples
#' \donttest{
#' library("ConfZIC")
#' data(Concrete)
#' x=Concrete
#' Y=x[,9] #dependent variable
#' #independent variables
#' X1=x[,1];X2=x[,2];X3=x[,3];X4=x[,4];
#' X5=x[,5];X6=x[,6];X7=x[,7];X8=x[,8];
#' mydata=cbind(Y,X1,X2,X3,X4,X5,X6,X7,X8) #data matrix
#' RankReg(mydata,0.95,"BIC")
#' }


RankReg=function(data,alphaval=0.95,model_ZIC="AIC"){



  if (alphaval<0 || alphaval>1) {stop("Confidence limit should be between 0 and 1!")}
  if (ncol(data)>=10){stop("No of data points are out of range: m should be less than 10")}
  if (model_ZIC!= "AIC" && model_ZIC!= "BIC" && model_ZIC!= "AICc"){stop("model_ZIC should be either AIC,BIC, or AICc")}


  ExactReg<-function(AIC_v,sigmahat,alphaval,m)
  {
    FX=NULL
    m=2^m

    FX=function(x){
      a=x
      pmvnorm(lower = rep(a,m),
              upper = rep(Inf,m),
              mean = AIC_v[1:m],
              sigma = sigmahat[1:m,1:m],maxpts = 25000, abseps = 0.001,
              releps = 0)
    }


    CDFfinal=function(x){
      ans=1-FX(x)-alphaval

      return(ans)
    }
    ####################################################################

    ## Find the value of x_q for nonlinear equation

    uval=bisection(f=CDFfinal,a=-10,b=max(GIC)+100, tol = 0.001,m=100)

    return(uval)
  }

  k=ncol(data)
  Y=data[,1]
  X=data[,2:k]
  Datanew=as.data.frame(cbind(Y,X))
  X=as.matrix(X)
  m=ncol(X) # number of independent variables

  #change the global options
  old=options()
  on.exit(options(old))

  options(na.action = "na.fail")

  #fit the linear regression
  model <- lm(Y ~., data = Datanew)

  #get the all the subset combinations (2^m)
  combinations <- dredge(model)
  dataM=as.data.frame(combinations)
  dataModel=as.data.frame(combinations)

  file=tempfile()
  write.csv(dataModel,file)
  RankData=read.csv(file)

  RankData=as.matrix(RankData)

  FinalData=RankData[order(RankData[,1],decreasing=FALSE),]
  FinalData1=RankData[order(RankData[,1],decreasing=FALSE),]
  FinalData[is.na(FinalData)] <- 0
  Beta=FinalData[,2:(m+2)] # get the fitted coefficients
  LogLikeVal=FinalData[,(m+4)] # get lo likelihood values
  df=FinalData[,(m+3)]
  k=ncol(FinalData)

  num=nrow(dataM)
  n=nrow(X)

  pt=df # number of parameters
  #calculate the information criteria
  GIC_AICc=(-2/n) * LogLikeVal +2*pt/(n-pt-1)
  GIC_AIC=(-2/n) * LogLikeVal +2*pt/n
  GIC_BIC=(-2/n) * LogLikeVal +2*pt*log(n)/(2*n)



  if (model_ZIC=="AIC"){
    GIC=GIC_AIC
  }

  if (model_ZIC=="BIC"){
    GIC=GIC_BIC
  }

  if (model_ZIC=="AICc"){
    GIC=GIC_AICc
  }



  A=matrix(rep(1,n),nrow=n,ncol=1)
  M=cbind(A,X)

  tot=2^m
  #get the covariance matrix
  sigma2hat=rep(0,tot);
  for (i in 1:tot){
    sigma2hat[i]=(1/n)*sum((Y-M%*%Beta[i,])^2)
  }

  ########################################

  LogL=matrix(rep(0,tot*n),nrow=n,ncol=tot);


  for (k in 1:tot){
    LogL[,k]=(-1/2)*log(2*pi)-((1/(2*(sigma2hat[k])))*((Y-M%*%Beta[k,])^2))-log(sqrt(sigma2hat[k]));
  }

  VarianceReg=matrix(rep(0,tot^2),nrow=tot,ncol=tot)
  for (k in 1:tot){
    for (j in 1:tot){
      VarianceReg[k,j]=(1/n)*sum(LogL[k]%*%LogL[j])-((1/n)*sum(LogL[k]))*((1/n)*sum(LogL[j]))

      #VarianceReg[k,j]=(1/n)*sum(LogL[,k]%*%LogL[,j])-((1/n)*sum(LogL[,k]))*((1/n)*sum(LogL[,j]))
    }
  }


  # sort the GIC
  S=sort(GIC)
  R=order(GIC)

  alphaval=alphaval
  #calculate the 1-alpha confidence envelope
  pern=ExactReg(GIC,(1/n)*VarianceReg,alphaval,m)

  #### Get the model
  Kmodel=seq(1,tot,1)
  ModelNum=matrix(rep(0, tot*(m+1)),nrow=tot)

  k=m+1
  Beta1=FinalData1[,2:(m+2)]
  for (u in 1: tot){
    for (v in 1: k){
      if (Beta[u,v]>0|| Beta[u,v]<0)
        ModelNum[u,v]=v
    }
  }

  listModels=list()
  for (u in 1:tot){
    listModels[[u]]=which(ModelNum[u,]!=0)
    listModels[[u]]=listModels[[u]]-1
  }


  GIC=round(GIC,digits = 7)
  OUT=cbind(GIC,Kmodel)

  result=as.matrix(OUT[order(OUT[,1],decreasing=FALSE),])
  result_new=result[result[,1]<pern,]


  rankM=result_new[,2]
  #ModelNum(c(rankM),)
  Rank_Models=listModels[c(rankM)]


  kmodels=length(Rank_Models)
  Models=matrix(rep(0,kmodels), nrow=kmodels)


  for (u in 1:kmodels){
    Models[u,]=toString(paste0("'",Rank_Models[u],"'") )

  }



  result_final=cbind(result_new[,1],Models)

  if (model_ZIC=="AIC"){ colnames(result_final)=c("AIC","Models")}
  if (model_ZIC=="BIC"){ colnames(result_final)=c("BIC","Models")}
  if (model_ZIC=="AICc"){ colnames(result_final)=c("AICc","Models")}


  # listdata=list(Ranked_Models=Rank_Models,ZIC_models=result_final, Confidence_Envelope=pern, Confidence_Limit=alphaval, Total_Models=tot)
  listdata=list(Ranked_Models=result_final, Confidence_Envelope=pern, Confidence_Limit=alphaval, Total_Models=tot)


  return(listdata)

}
