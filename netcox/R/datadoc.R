#' A simulated demo dataset \code{sim}
#' 
#' @name sim
#' @keywords datasets
#' @usage data(sim)
#' @format A simulated data frame that is used to illustrate the use of the netcox package. The max follow-up time for each subject is set to be 5. The total number of subject is 50.
#' \describe{
#'   \item{Id}{The ID of each subject.}
#'   \item{Event}{During the time from \code{Start} to \code{Stop}, if the subject experience the event. We use the function \code{permalgorithm} in the \code{R} package \code{PermAlgo} to generate the Event.}
#'   \item{Start}{Start time.}
#'   \item{Stop}{Stop time.}
#'   \item{Fup}{The total follow-up time for the subject.}
#'   \item{Covariates}{A1, A2, C1, C2, B, A1B, A2B, C1B, C2B. The dataset contains 5 variables (9 columns after one-hot encoding). Variable A is a e 3-level categorical variable, which results in 2 binary variables (A1 and A2), the same with the variable C. B is a continuous variable. The interaction term AB and CB are also two 3-level categorical variables. The code for generating the covariates is given below.}
#' }
#' @examples
#' # generate B
#'gen_con=function(m){
#'  X=rnorm(m/5)
#'  XX=NULL
#'  for (i in 1:length(X)) {
#'    if (length(XX)<m){
#'    X.rep=rep(X[i],round(runif(1,5,10),0))
#'    XX=c(XX,X.rep)
#'    }
#'  }
#'  return(XX[1:m])
#'}
#' # generate A and C
#'gen_cat=function(m){
#'  X=sample.int(3, m/5,replace = TRUE)
#'  XX=NULL
#'  for (i in 1:length(X)) {
#'    if (length(XX)<m){
#'      X.rep=rep(X[i],round(runif(1,5,10),0))
#'      XX=c(XX,X.rep)
#'    }
#'  }
#'  return(XX[1:m])
#'}
#'
#' # generate covariate for one subject
#'gen_X=function(m){
#'  A=gen_cat(m);B=gen_con(m);C=gen_cat(m)
#'  A1=ifelse(A==1,1,0);A2=ifelse(A==2,1,0)
#'  C1=ifelse(C==1,1,0);C2=ifelse(C==2,1,0)
#'  A1B=A1*B;A2B=A2*B
#'  C1B=C1*B;C2B=C2*B
#'  return(as.matrix(cbind(A1,A2,C1,C2,B,A1B,A2B,C1B,C2B)))
#'}
#'
#' # generate covariate for all subject
#'gen_X_n=function(m,n){
#'  Xn=NULL
#'  for (i in 1:n) {
#'    X=gen_X(m)
#'    Xn=rbind(Xn,X)
#'  }
#'  return(Xn)
#'}
#'
#'  n=50;m=5
#'  covariates=gen_X_n(m,n)
#'  # generate outcomes
#'  # library(PermAlgo)
#'  # data <- permalgorithm(n, m, covariates, 
#'  #                       XmatNames = c("A1","A2","C1","C2","B","A1B","A2B","C1B","C2B"),
#'  #                       #change according to scenario 1/2
#'  #                       betas = c(rep(log(3),2),rep(0,2), log(4), rep(log(3),2),rep(0,2)),
#'  #                       groupByD=FALSE )
#'  # fit.original = coxph(Surv(Start, Stop, Event) ~ . ,data[,-c(1,3)])
#' @seealso
#' \code{PermAlgo}
"sim"
