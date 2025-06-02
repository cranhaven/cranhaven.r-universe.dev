\name{est_multi_poly_between}
\alias{est_multi_poly_between}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Estimate latent class item response theory (LC-IRT)  models for dichotomous and polytomous responses under between-item multidimensionality}

\description{The function performs maximum likelihood estimation of the parameters of the IRT models
assuming a discrete distribution for the ability and between-item multidimensionality. Every ability level corresponds to a latent
class of subjects in the reference population. The class of models is based on a between-item multidimensional formulation with each item loading on a dimension of a given latent variable. Maximum likelihood estimation is based on Expectation-
Maximization algorithm.}

\usage{est_multi_poly_between(S, yv = rep(1, ns), k, X = NULL, start = c("deterministic",
                       "random","external"), link = c("global","local"), disc = FALSE,
                       difl = FALSE, multi = 1:J, Phi = NULL, gat = NULL, De = NULL,
                       fort = FALSE, tol = 10^-10, maxitc = 10^4, disp = FALSE,
                       output = FALSE, out_se = FALSE, glob = FALSE, Zth=NULL,zth=NULL,
                       Zbe=NULL, zbe=NULL,Zga=NULL,zga=NULL)}
        
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
           (use NA for missing responses)}
  \item{yv}{vector of the frequencies of every response configuration in \code{S}}
  \item{k}{number of ability levels (or latent classes) for the latent variable}
  \item{X}{matrix of covariates that affects the weights}
  \item{start}{method of initialization of the algorithm}
  \item{link}{type of link function ("global" for global logits, "local" for local logits); with global logits a graded response model results; with local logits a  partial credit model results (with dichotomous responses, global logits is the same as using local logits resulting in the Rasch or the 2PL model depending on the value assigned to disc)}
  \item{disc}{indicator of constraints on the discriminating indices (FALSE = all equal to one, TRUE = free)}
  \item{difl}{indicator of constraints on the difficulty levels (FALSE = free, TRUE = rating scale parametrization); difl = TRUE is only admitted in the presence of items with the same number of categories}
  \item{multi}{matrix with a number of rows equal to the number of dimensions and elements in each row
               equal to the indices of the items measuring the dimension corresponding to that row for the latent variable}
  \item{Phi}{initial value of the matrix of the conditional response probabilities (if start="external")}
  \item{gat}{initial value of the vector of free discriminating indices (if start="external")}
  \item{De}{initial value of regression coefficients for the covariates (if start="external")}
  \item{fort}{to use Fortran routines when possible}
  \item{tol}{tolerance level for checking convergence of the algorithm as relative difference between 
  	         consecutive log-likelihoods}
  \item{maxitc}{maximum number of iterations of the algorithm}	         
  \item{disp}{to display the likelihood evolution step by step}
  \item{output}{to return additional outputs (Piv,Pp,lkv, Xlabel, XXdis)}
  \item{out_se}{to return standard errors}
  \item{glob}{to use global logits in the covariates} 
  \item{Zth}{matrix for the specification of constraints on the support points}  
  \item{zth}{vector for the specification of constraints on the support points}  
  \item{Zbe}{matrix for the specification of constraints on the item difficulty parameters}  
  \item{zbe}{vector for the specification of constraints on the item difficulty parameters}  
  \item{Zga}{matrix for the specification of constraints on the item discriminating indices}  
  \item{zga}{vector for the specification of constraints on the item discriminating indices}  
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{piv}{estimated vector of weights of the latent classes (average of the weights in case of model with covariates)}
\item{fv}{vector indicating the reference item chosen for each latent dimension of the latent variable}
\item{tht}{estimated matrix of free ability levels for each dimension}
\item{Th}{complete matrix of free and constrained ability levels for each dimension and latent class of the latent variable}
\item{bet}{estimated vector of free difficulty levels for every item (split in two vectors if difl=TRUE)}
\item{Bec}{complete vector of free and constrained difficulty levels for every item (split in two vectors if difl=TRUE)}
\item{gat}{estimated vector of free discriminating indices for every item (with all elements equal to 1
	       if disc=FALSE)}
\item{gac}{complete vector of free and constrained discriminating indices for every item (with all elements equal to 1
	       if disc=FALSE)}
\item{De}{matrix of regression coefficients for the multinomial (or global if glob=TRUE) logit model on the class weights}	       
\item{Phi}{array of the conditional response probabilities for every item and each of the k latent classes}
\item{lk}{log-likelhood at convergence of the EM algorithm}
\item{np}{number of free parameters}
\item{aic}{Akaike Information Criterion index}
\item{bic}{Bayesian Information Criterion index}
\item{ent}{entropy index to measure the separation of classes}
\item{pivs}{estimated vector of (ordered) weights of the latent classes (average of the weights in case of model with covariates)}
\item{Ths}{standardized ability levels}
\item{Becs}{standardized values of item difficulty parameters}
\item{gacs}{standardized values of item discriminating indices}
\item{call}{call of function}
\item{Pp}{matrix of the posterior probabilities for each response configuration and latent class
	      (if output=TRUE)}
\item{lkv}{vector to trace the log-likelihood evolution across iterations (if output=TRUE)}
\item{Xlabel}{structure of the design matrix, for internal use (if output=TRUE)}
\item{XXdis}{design matrix for the covariates affecting the latent variable (if output=TRUE)}
\item{Piv}{matrix of the weights for every response configuration (if output=TRUE)}
\item{setht}{standard errors for vector tht (if out_se=TRUE)}
\item{seTh}{standard errors for vector Th (if out_se=TRUE)}  
\item{sebet}{standard errors for vector bet (if out_se=TRUE)}
\item{seBec}{standard errors for vector Bec (if out_se=TRUE)}
\item{segat}{standard errors for vector gat (if out_se=TRUE)}
\item{segac}{standard errors for vector gac (if out_se=TRUE)}
\item{seDe}{standard errors for vector De (if out_se=TRUE)}
\item{Vnt}{estimated variance-covariance matrix for free parameter estimates (if out_se=TRUE)}
\item{Vn}{estimated variance-covariance matrix for all parameter estimates (if out_se=TRUE)}
}

\references{
Bartolucci, F. (2007), A class of multidimensional IRT models for testing unidimensionality and clustering
items, \emph{Psychometrika}, \bold{72}, 141-157.

Bacci, S., Bartolucci, F. and Gnaldi, M. (2014), A class of Multidimensional Latent Class IRT models for 
ordinal polytomous item responses, \emph{Communications in Statistics - Theory and Methods}, \bold{43}, 787-800.
}

\author{Francesco Bartolucci, Silvia Bacci - University of Perugia (IT)}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{

\dontrun{
# Fit a Graded response model with two dimensions (free discrimination
# and difficulty parameters; three latent classes):
data(SF12_nomiss)
S = SF12_nomiss[,1:12]
X = SF12_nomiss[,13]
multi0 = rbind(c(1:5, 8), c(6:7,9:12))
k=3

out1 =  est_multi_poly_between(S=S,k=k,X=X,link="global",disc=TRUE,
                               multi=multi0,fort=TRUE,disp=TRUE,out_se=TRUE) 

# Display output:
summary(out1)
out1$lk
out1$Th
out1$piv
out1$De
}

\dontrun{
## Fit the model under different external constraints on abilities and/or item parameters
# Fixed ability levels; all item parameters can be free  
S1 = pmin(as.matrix(S),2) # all items have the same number of categories
Zth = matrix(0,nrow(multi0)*k,0)
zth = c(rep(-1, times=nrow(multi0)), rep(0, times=nrow(multi0)),  rep(1, times=nrow(multi0)))
Zbe = diag(ncol(S1)*2)  # free item difficulties: 12*2  = 24 (12 items with 3 categories)
Zga = diag(ncol(S1));  # free item discriminating parameters = 12 items loading on U
outc1 = est_multi_poly_between(S=S1,k=k,X=X,link="global",disc=TRUE, multi=multi0,disp=TRUE,
                               out_se=TRUE,Zth=Zth,zth=zth,Zbe=Zbe,Zga=Zga) 
outc1$Th
outc1$tht
outc1$Bec                                                          

# Add equality constraints on item parameters                         
# Same difficulties for pairs of items 1-7, 2-8, 3-9, 4-10, 5-11, 6-12; 
# same discriminating indices for items 2 and 3;
# free ability levels
Zbe = (matrix(1,2,1)\%x\%diag(12))[,-1]
Zga = as.matrix(rep(0, times=12)); Zga[2,1] = 1; Zga[3,1] = 1; 
Zga1p1 = matrix(0, nrow=3, ncol=9); Zga1p2 = diag(9); Zga1p = rbind(Zga1p1, Zga1p2)
Zga = cbind(Zga, Zga1p)
# discriminating index of item 1 constrained to 1 for the model identifiability
zga = rep(0,nrow(Zga)); zga[1] = 1 
outc2 = est_multi_poly_between(S=S1,k=k,X=X,link="global",disc=TRUE, 
                             multi = multi0,disp=TRUE,tol=10^-4,
                             out_se=TRUE,Zbe=Zbe, Zga=Zga, zga=zga)
outc2$tht
outc2$Th
outc2$Ths
outc2$Bec
outc2$Becs
outc2$gac 
outc2$gacs
}

}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{maximum likelihood estimation}
\keyword{Expectation-Maximization algorithm}% __ONLY ONE__ keyword per line
