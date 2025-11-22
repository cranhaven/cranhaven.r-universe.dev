#' Variational Bayesian Mixture of Factor Analyzers (VB-MoFA)
#'
#'@description An implementation of the Variational Bayesian Mixutre of Factor
#' Analysers \insertCite{ghahramani2000variational}{autoMFA}. This code is an
#' R port of the MATLAB code which was written by M.J.Beal and released alongside
#' their paper.
#'
#' @references \insertRef{ghahramani2000variational}{autoMFA}
#' @param Y An \emph{n} by \emph{p} (normalised) data matrix (i.e. the result of
#' a call to the function \code{preprocess}), where \emph{n} is the number of observations and
#' \emph{p} is the number of dimensions of the data.
#' @param qmax Maximum factor dimensionality (default \emph{p}-1).
#' @param maxtries The maximum number of times the algorithm will attempt to split each component.
#' @param verbose Whether or not verbose output should be printed during the 
#' model fitting process (defaults to false).  
#' @param varimax Boolean indicating whether the output factor loading matrices should be constrained
#' using varimax rotation or not. 
#' 
#' @return A list containing the following elements:
#' \itemize{
#' \item{\code{model}:}{ A list specifying the final MFA model. This contains: \itemize{
#'            \item{\code{B}:}{ A \emph{p} by \emph{p} by \emph{q} array containing the factor loading matrices for each component.}
#'             \item{\code{D}:}{ A \emph{p} by \emph{p} by \emph{g} array of error variance matrices.}
#'            \item{\code{mu}:}{  A \emph{p} by \emph{g} array containing the mean of each cluster.}
#'           \item{\code{pivec}:}{ A 1 by \emph{g} vector containing the mixing
#'             proportions for each FA in the mixture.}
#'           \item{\code{numFactors}:}{ A 1 by \emph{g} vector containing the number of factors for each FA.}}
#'           }
#'\item{\code{clustering}:}{ A list specifying the clustering produced by the final model. This contains: \itemize{
#'             \item{\code{responsibilities}:}{ A \emph{n} by \emph{g} matrix containing the probability
#'               that each point belongs to each FA in the mixture.}
#'             \item{\code{allocations}:}{ A \emph{n} by 1 matrix containing which
#'               FA in the mixture each point is assigned to based on the responsibilities.}}}
#'\item{\code{diagnostics}:}{ A list containing various pieces of information related to the fitting process of the algorithm. This contains: \itemize{
#'             \item{\code{bic}:}{ The BIC of the final model.}
#'             \item{\code{logL}:}{ The log-likelihood of the final model.}
#'             \item{\code{Fhist}:}{The values of \emph{F} at each iteration of the algorithm. \emph{F} is defined in \insertCite{ghahramani2000variational}{autoMFA}.}
#'             \item{\code{times}:}{ The time taken for each loop in the algorithm.}
#'             \item{\code{totalTime}:}{ The total time taken to fit the final model.}}}
#'}
#' @export
#'
#'
#' @examples
#' RNGversion('4.0.3'); set.seed(3)
#' Yout <- preprocess(MFA_testdata)
#' MFA.fit <- vbmfa(Yout$Yout, maxtries = 2)
#'
#'@seealso \code{\link{preprocess}} for centering and scaling data prior to using \code{vbmfa}.
#'
vbmfa <- function(Y, qmax = NULL, maxtries = 3, verbose = FALSE, varimax = FALSE){
  
  k <- qmax
  if(is.null(k)){
    k = dim(Y)[2] 
    qmax <- k-1
  } else {
    k <- qmax + 1 
  }
  
  stopifnot("verbose should be either TRUE or FALSE"={verbose %in% c(TRUE,FALSE)})
  stopifnot("varimax should be either TRUE or FALSE"={varimax %in% c(TRUE,FALSE)})
  stopifnot("Y should be an n by p numeric matrix"={is.numeric(Y)&is.matrix(Y)})
  stopifnot("qmax should be numeric"={is.numeric(qmax)})
  stopifnot("qmax should be an integer"={qmax%%1==0})
  stopifnot("qmax must be greater than zero and less than or equal to p-1" = {(qmax>0) & (qmax <= k-1)} )
  
cat(sprintf("Variational Bayesian MFA fitting process begun...\n"))  
  
#Variational Mixture of Factor Analysers model.
#
#net = vbmfa(Y,k,pcaflag,Fflag,dsp,net);
#
# Y - N x p matrix of (normalised) observations 
# k - maximum factor dimensionality (default p-1)
#
#Matthew J. Beal GCNU 15/02/01


start.time <- Sys.time()

pcaflag=0
Fflag=0
dsp=pi

Y <- t(Y)
dims <- dim(Y)
p <- dims[1]; n <- dims[2]

selection_method = 4;
getbeta = NULL; #Required to stop an undefined variable note from showing
#but has no impact on code. Only needed if we changed selection method to 3.

net = list(
  model = "variational MFA",
  hparams = list(mcl = NULL,psii=NULL,pa=NULL,pb=NULL,alpha=NULL),
  params = list(Lm = NULL, Lcov = NULL, u = NULL),
  hidden = list(Xm = NULL, Xcov = NULL, Qns = NULL, a = NULL, b = NULL),
  Fhist = NULL
)
pa = 1; pb = 1; alpha = 1; psii = matrix(rep(1,p), nrow = p)
Lm = vector(mode = "list")
Lcov = vector(mode = "list")


### Random initialisation
Lm[[1]] = matrix(rnorm(p*k), nrow = p)
### Changing to deterministic for testing purposes
#Lm[[1]] = matrix(1/(p*k)*1:(p*k), nrow = p)


Lcov[[1]] = array(rep(diag(k),p),dim = c(k,k,p))
u = 1;
mean_mcl = matrix(rowMeans(Y),ncol = 1);
#Need to check that this works as expected!!!!
nu_mcl = (1/matrix(sqrt(rowSums((Y - matrix(rep(mean_mcl,n),ncol=n))^2/(n-1))),ncol=1))^2;
Xcov = vector(mode = "list")
Xcov[[1]] = array(0,dim = c(k,k,n));

s = length(Lm); 
pu = alpha*matrix(rep(1,s),nrow = 1)/s;

# Tweakables and initialisations
psimin = 1e-5;
maintol = 6; # the greater these numbers,
finetol = 8; #  the finer learning
tries = 0;
removal = 1; # if on, insists removal of components that have close to zero responsibility
F_ = -Inf;
Fhist = NULL;
it = 0; I = diag(k-1);

# Infer hidden variables and calculate initial F
tol = exp(-maintol);
Qns = matrix(rep(1,n*s),nrow = n)/s;

#### 4 scripts here
b <- list()
Xm = vector(mode = "list")
trXm = vector(mode = "list")
#inferQX: This script calculates the sufficient statistics for the
# variational posterior over hidden factors.

tempOut <- inferQX(Y,Lm,Lcov,psii,Xcov,trXm,Xm)
Xcov = tempOut$Xcov
trXm = tempOut$trXm
Xm = tempOut$Xm

candorder = matrix()
cophd = NULL
tempOut <- inferQns(Y,Lm,Qns,psii,Lcov,Xm,Xcov,candorder,b,u,pu,removal,parent,pos,cophd, verbose)
Qns = tempOut$Qns; Ps = tempOut$Ps; Lm = tempOut$Lm; Lcov = tempOut$Lcov;
Xm = tempOut$Xm; Xcov = tempOut$Xcov; b = tempOut$b; u = tempOut$u;
pu = tempOut$pu; s = tempOut$s; dQns_sagit = tempOut$dQns_sagit; candorder = tempOut$candorder;
cophd = tempOut$cophd;

tempOut <- inferQnu(Lm,pa,pb,Lcov,b)
s = tempOut$s; b = tempOut$b; a = tempOut$a
num = vector(mode = "list")

tempOut <- learn(it, Y,Lm,Lcov,psii,Xcov,trXm,Xm,
                 mean_mcl,nu_mcl,a,b,Qns,candorder,u,pu,
                 removal, alpha, pa, pb, pcaflag, psimin, num, parent, pos, cophd, verbose)
it = tempOut$it; Xm=tempOut$Xm; trXm= tempOut$trXm; Xcov = tempOut$Xcov;
mean_Lambda = tempOut$mean_Lambda; num = tempOut$num; Lcov = tempOut$Lcov;
Lm = tempOut$Lm; Qns = tempOut$Qns; Ps = tempOut$Ps; b = tempOut$b;
u = tempOut$u; pu = tempOut$pu; s = tempOut$s; dQns_sagit = tempOut$dQns_sagit;
a = tempOut$a; psi = tempOut$psi; psii = tempOut$psii; zeta = tempOut$zeta;
mean_mcl = tempOut$mean_mcl; nu_mcl = tempOut$nu_mcl
candorder = tempOut$candorder; cophd = tempOut$cophd;
####

workspace = list(psii = psii, Lm = Lm, Lcov = Lcov, Qns = Qns, a = a, b = b, u = u)

candorder = cbind(1:length(Lm),0); pos = 1; parent = candorder[pos];
epoch = 0;

tempOut = Fcalc(Lm,Y,Qns,F_,a,u,pu,Xm,Xcov,Ps,psii,pi,b,pa,pb,nu_mcl,mean_mcl,it,alpha,Lcov,Fhist);
Fmatrix = tempOut$Fmatrix; pu = tempOut$p; Ps = tempOut$Ps;
F_old = tempOut$F_old;Qnsmod = tempOut$Qnsmod; FmatrixKLpi = tempOut$FmatrixKLpi;
F_ = tempOut$F_; dF = tempOut$dF; Fhist = tempOut$Fhist;

Ftarg = F_;

cumul_time = 0;
time.vec <- c(0)
birthFail = FALSE
while(parent != 0){
loop.start.time <- Sys.time()
epoch = epoch+1; cophd = 0;

tempOut = Fcalc(Lm,Y,Qns,F_,a,u,pu,Xm,Xcov,Ps,psii,pi,b,pa,pb,nu_mcl,mean_mcl,it,alpha,Lcov,Fhist);
Fmatrix = tempOut$Fmatrix; pu = tempOut$p; Ps = tempOut$Ps;
F_old = tempOut$F_old;Qnsmod = tempOut$Qnsmod; FmatrixKLpi = tempOut$FmatrixKLpi;
F_ = tempOut$F_; dF = tempOut$dF; Fhist = tempOut$Fhist;

Ftarg = F_; dF = 0;

if(length(Lm) < parent){birthFail = TRUE; break}

tempOut = dobirth(Y,Lm,psii,parent,Qns,Lcov,b,u,pu,alpha, Xcov, trXm, Xm, mean_mcl,nu_mcl,a,num, verbose)
Lcov = tempOut$Lcov; Lm = tempOut$Lm; b=tempOut$b; u=tempOut$u; pu = tempOut$pu;
mean_Lambda = tempOut$mean_Lambda; num = tempOut$num;
Xcov = tempOut$Xcov; trXm = tempOut$trXm; Xm = tempOut$Xm
Qns = tempOut$Qns


tol = maintol;
dQns_sagit = tol;

counter = 0

while (length(which(dQns_sagit>=exp(-tol))) > 0 ) {

  
  tempOut <- learn(it, Y,Lm,Lcov,psii,Xcov,trXm,Xm,
                   mean_mcl,nu_mcl,a,b,Qns,candorder,u,pu,
                   removal, alpha, pa, pb, pcaflag, psimin, num, parent, pos, cophd, verbose)
  it = tempOut$it; Xm=tempOut$Xm; trXm= tempOut$trXm; Xcov = tempOut$Xcov;
  mean_Lambda = tempOut$mean_Lambda; num = tempOut$num; Lcov = tempOut$Lcov;
  Lm = tempOut$Lm; Qns = tempOut$Qns; Ps = tempOut$Ps; b = tempOut$b;
  u = tempOut$u; pu = tempOut$pu; s = tempOut$s; dQns_sagit = tempOut$dQns_sagit;
  a = tempOut$a; psi = tempOut$psi; psii = tempOut$psii; zeta = tempOut$zeta;
  mean_mcl = tempOut$mean_mcl; nu_mcl = tempOut$nu_mcl
  candorder = tempOut$candorder; cophd = tempOut$cophd;
  counter = counter + 1
  if(Fflag == 1){
  dF_old = dF;
  tempOut = Fcalc(Lm,Y,Qns,F_,a,u,pu,Xm,Xcov,Ps,psii,pi,b,pa,pb,nu_mcl,mean_mcl,it,alpha, Lcov, Fhist);
  Fmatrix = tempOut$Fmatrix; pu = tempOut$p; Ps = tempOut$Ps;
  F_old = tempOut$F_old;Qnsmod = tempOut$Qnsmod; FmatrixKLpi = tempOut$FmatrixKLpi;
  F_ = tempOut$F_; dF = tempOut$dF; Fhist = tempOut$Fhist;
  if(verbose){
  print(paste('Iteration: ',it, ', F-value: ', F_, ', Change in F: ', dF, '.'))}
  
  }
  
}

# Analyse whether to accept Epoch

### Script here
tempOut = Fcalc(Lm,Y,Qns,F_,a,u,pu,Xm,Xcov,Ps,psii,pi,b,pa,pb,nu_mcl,mean_mcl,it,alpha, Lcov, Fhist);
Fmatrix = tempOut$Fmatrix; pu = tempOut$p; Ps = tempOut$Ps;
F_old = tempOut$F_old;Qnsmod = tempOut$Qnsmod; FmatrixKLpi = tempOut$FmatrixKLpi;
F_ = tempOut$F_; dF = tempOut$dF; Fhist = tempOut$Fhist;


if(F_ > Ftarg){
  if(verbose){
print(paste('Accepting F= ', F_, ' (', Ftarg,')'))};
# If child of parent has died do not reorder components
if(cophd == 1){
  if(verbose){
  print('Child of parent has died, no reordering.')};
  pos = pos+1;
} else {

### Script here
tempOut <- ordercands(Y,Lm,Fmatrix,Qns,getbeta,selection_method, verbose)
pos = tempOut$pos; candorder = tempOut$candorder
# sets "candorder", with last entry as a ZERO (termination)


tries = 0; #% resets the tries back to zero as well as the pos.
}
workspace = list( psii=psii, Lm = Lm, Lcov = Lcov, Qns = Qns, a = a, b = b,u= u);

tempOut <- inferQX(Y,Lm,Lcov,psii,Xcov,trXm,Xm)
Xcov = tempOut$Xcov; trXm = tempOut$trXm; Xm = tempOut$Xm

tempOut <- infermcl(Lm,Lcov)
if(class(tempOut) != "character"){
mean_mcl = tempOut$mean_mcl; nu_mcl = tempOut$nu_mcl}

tempOut <- learn(it, Y,Lm,Lcov,psii,Xcov,trXm,Xm,
                 mean_mcl,nu_mcl,a,b,Qns,candorder,u,pu,
                 removal, alpha, pa, pb, pcaflag, psimin, num, parent, pos, cophd, verbose)
it = tempOut$it; Xm=tempOut$Xm; trXm= tempOut$trXm; Xcov = tempOut$Xcov;
mean_Lambda = tempOut$mean_Lambda; num = tempOut$num; Lcov = tempOut$Lcov;
Lm = tempOut$Lm; Qns = tempOut$Qns; Ps = tempOut$Ps; b = tempOut$b;
u = tempOut$u; pu = tempOut$pu; s = tempOut$s; dQns_sagit = tempOut$dQns_sagit;
a = tempOut$a; psi = tempOut$psi; psii = tempOut$psii; zeta = tempOut$zeta;
mean_mcl = tempOut$mean_mcl; nu_mcl = tempOut$nu_mcl
candorder = tempOut$candorder; cophd = tempOut$cophd;


tempOut = Fcalc(Lm,Y,Qns,F_,a,u,pu,Xm,Xcov,Ps,psii,pi,b,pa,pb,nu_mcl,mean_mcl,it,alpha, Lcov, Fhist);
Fmatrix = tempOut$Fmatrix; pu = tempOut$p; Ps = tempOut$Ps;
F_old = tempOut$F_old;Qnsmod = tempOut$Qnsmod; FmatrixKLpi = tempOut$FmatrixKLpi;
F_ = tempOut$F_; dF = tempOut$dF; Fhist = tempOut$Fhist;

Ftarg = F_;



} else {
  if(verbose){
  print(paste('Rejecting F= ', F_, ' (', Ftarg , ')'))};

  psii = workspace$psii
  Lm = workspace$Lm
  Lcov = workspace$Lcov
  Qns = workspace$Qns
  a = workspace$a
  b = workspace$b
  u = workspace$u


  ###  3 scripts here
  tempOut <- inferQX(Y,Lm,Lcov,psii,Xcov,trXm,Xm)
  Xcov = tempOut$Xcov; trXm = tempOut$trXm; Xm = tempOut$Xm

  tempOut <- infermcl(Lm,Lcov)
  if(class(tempOut) != "character"){
  mean_mcl = tempOut$mean_mcl; nu_mcl = tempOut$nu_mcl}

  tempOut <- learn(it, Y,Lm,Lcov,psii,Xcov,trXm,Xm,
                   mean_mcl,nu_mcl,a,b,Qns,candorder,u,pu,
                   removal, alpha, pa, pb, pcaflag, psimin, num, parent, pos, cophd, verbose)
  it = tempOut$it; Xm=tempOut$Xm; trXm= tempOut$trXm; Xcov = tempOut$Xcov;
  mean_Lambda = tempOut$mean_Lambda; num = tempOut$num; Lcov = tempOut$Lcov;
  Lm = tempOut$Lm; Qns = tempOut$Qns; Ps = tempOut$Ps; b = tempOut$b;
  u = tempOut$u; pu = tempOut$pu; s = tempOut$s; dQns_sagit = tempOut$dQns_sagit;
  a = tempOut$a; psi = tempOut$psi; psii = tempOut$psii; zeta = tempOut$zeta;
  mean_mcl = tempOut$mean_mcl; nu_mcl = tempOut$nu_mcl
  candorder = tempOut$candorder; cophd = tempOut$cophd;

  pos = pos +1;
}

if(verbose){
print(paste("Iteration: ", it, " Completion: ", ( tries/maxtries+(pos-1)/(dim(candorder)[2]-1)/maxtries )*100 , '%'))
} else {
    cat(sprintf(paste('\rIteration: ',it, ', F-value: ', round(F_,6),'.', sep = "")))
  }

if(pos > dim(candorder)[2]){
  pos <- dim(candorder)[2]; #Fixing a very rare bug where pos was > length(candorder)'
}
parent = candorder[pos];
if(parent == 0){
  tries = tries+1;
  if(tries != maxtries){
    if(verbose){  print('End of ordering reached, reordering and trying more splits')}
  tempOut <- inferQX(Y,Lm,Lcov,psii,Xcov,trXm,Xm)
  Xcov = tempOut$Xcov; trXm = tempOut$trXm; Xm = tempOut$Xm

  tempOut <- inferQns(Y,Lm,Qns,psii,Lcov,Xm,Xcov,candorder,b,u,pu,removal,parent,pos,cophd, verbose)
  Qns = tempOut$Qns; Ps = tempOut$Ps; Lm = tempOut$Lm; Lcov = tempOut$Lcov;
  Xm = tempOut$Xm; Xcov = tempOut$Xcov;
  b = tempOut$b; u = tempOut$u; pu = tempOut$pu; s = tempOut$s;
  dQns_sagit = tempOut$dQns_sagit; candorder = tempOut$candorder; cophd = tempOut$cophd;

  tempOut = Fcalc(Lm,Y,Qns,F_,a,u,pu,Xm,Xcov,Ps,psii,pi,b,pa,pb,nu_mcl,mean_mcl,it,alpha, Lcov, Fhist);
  Fmatrix = tempOut$Fmatrix; pu = tempOut$p; Ps = tempOut$Ps;
  F_old = tempOut$F_old;Qnsmod = tempOut$Qnsmod; FmatrixKLpi = tempOut$FmatrixKLpi;
  F_ = tempOut$F_; dF = tempOut$dF; Fhist = tempOut$Fhist;

  tempOut = ordercands(Y,Lm,Fmatrix,Qns,getbeta,selection_method, verbose)
  pos = tempOut$pos; candorder = tempOut$candorder

  parent = candorder[pos];
  }
}


loop.end.time <- Sys.time()
loop.time <- loop.end.time - loop.start.time
time.vec <- c(time.vec, loop.time)
cumul_time <- cumul_time + loop.time
if(verbose){
print(paste("Time since last update: ", loop.time, "s. Cumulative running time: ", cumul_time, 's.'))}

}

if(birthFail == TRUE){
  cat(sprintf(paste('Problem encountered while fitting. Consider refitting model.')))
}

end.time <- Sys.time()
cat(sprintf(paste('\nOptimisation complete with ', length(Lm), 'components.\n')))
totaltime = difftime(end.time,start.time,units = "secs")
cat(sprintf(paste('The total time taken for completion was ',totaltime , 's.\n')))

#script here.
tempOut = Fcalc(Lm,Y,Qns,F_,a,u,pu,Xm,Xcov,Ps,psii,pi,b,pa,pb,nu_mcl,mean_mcl,it,alpha, Lcov, Fhist);
Fmatrix = tempOut$Fmatrix; pu = tempOut$p; Ps = tempOut$Ps;
F_old = tempOut$F_old;Qnsmod = tempOut$Qnsmod; FmatrixKLpi = tempOut$FmatrixKLpi;
F_ = tempOut$F_; dF = tempOut$dF; Fhist = tempOut$Fhist;


g <- length(Lm)
Mu <- vector(mode = 'list'); Lambda <- vector(mode = 'list');
for(i in 1:g){
  Mu[[i]] <- matrix(Lm[[i]][,1],ncol = 1)
  Lambda[[i]] <- matrix(Lm[[i]][,2:dim(Lm[[i]])[2]], ncol = dim(Lm[[i]])[2] - 1)
}



final.Lambda <- do.call(cbind, Lambda);
final.Mu <- do.call(cbind, Mu)
final.Pi <- matrix((1/n)*colSums(Qns), nrow = 1)
final.Psi <- matrix(rep(1/psii, g), ncol = g)
numFactors <- matrix(ncol = g)
for(i in 1:g){
  numFactors[1,i] = dim(Lambda[[i]])[2]
}

tempLL <- logL(t(Y),g,final.Pi,final.Lambda,final.Mu,final.Psi,numFactors)
ll <- tempLL$logL
q <- max(numFactors);

#No. of params for common error matrix formulation
d <- (g - 1) + 2 * g * p + sum(p * as.vector(numFactors) -
                                 as.vector(numFactors)* (as.vector(numFactors) - 1)/2)

bic <- log(n)*d -2*ll;

D <- array(dim = c(p,p,g));
D[,,] <-diag(1/as.vector(psii))

out <- vector(mode = 'list')
#Model parameters
out$model$pivec = (1/n)*colSums(Qns)
out$model$mu = abind::abind(Mu,along = 3)
out$model$B = abind::abind(Lambda,along=3)
out$model$D = D
out$model$numFactors <- numFactors
#Diagnostic quantities
out$diagnostics$bic = bic
out$diagnostics$logL = ll
out$diagnostics$Fhist = Fhist
out$diagnostics$times = time.vec #Timing of each while(parent != 0) loop iteration
out$diagnostics$totalTime = totaltime
#Clustering information
out$clustering$responsibilities = Qns; #Class conditional responsibilities
out$clustering$allocations = Rfast::rowMaxs(Qns); #Clustering allocations
#Other quantities, not currently being included in the output.

if(birthFail){
  out$diagnostics$model_complete = FALSE
}else{ 
  out$diagnostics$model_complete = TRUE
}

if(varimax){
  p <- dim(out$model$B)[1]
  no.comp <- dim(out$model$B)[3]
  for(i in 1:no.comp){
    out$model$B[,,i] <- matrix(out$model$B[,,i], nrow = p) %*% stats::varimax(matrix(out$model$B[,,i], nrow = p))$rotmat
  }
}

return(out)

}
