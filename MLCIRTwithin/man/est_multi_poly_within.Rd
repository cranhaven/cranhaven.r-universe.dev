\name{est_multi_poly_within}
\alias{est_multi_poly_within}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Estimate latent class item response theory (LC-IRT) models for dichotomous and polytomous responses under within-item multidimensionality}

\description{The function performs maximum likelihood estimation of the parameters of the two-tier IRT models
assuming a discrete distribution for the ability and within-item multidimensionality. Every ability level corresponds to a latent
class of subjects in the reference population. The class of models is based on a particular within-item multidimensional formulation with each item loading on at most two uncorrelated latent variables. Maximum likelihood estimation is based on the Expectation-
Maximization algorithm.}

\usage{est_multi_poly_within(S, yv = rep(1, ns), k1, k2, X = NULL,
                      start = c("deterministic","random","external"), link = c("global",
                      "local"), disc = FALSE, difl = FALSE, multi1, multi2, Phi = NULL,
                      ga1t = NULL, ga2t = NULL, De1 = NULL, De2 = NULL, fort = FALSE,
                      tol = 10^-10, maxitc = 10^4, disp = FALSE, output = FALSE,
                      out_se = FALSE, glob = FALSE, Zth1 = NULL, zth1 = NULL, Zth2=NULL,
                      zth2=NULL, Zbe=NULL, zbe=NULL, Zga1=NULL, zga1=NULL, Zga2=NULL,
                      zga2=NULL)}
%- maybe also 'usage' for other objects documented here.


\arguments{
  \item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
           (use NA for missing responses)}
  \item{yv}{vector of the frequencies of every response configuration in \code{S}}
  \item{k1}{number of ability levels (or latent classes) for the 1st latent variable}
  \item{k2}{number of ability levels (or latent classes) for the 2nd latent variable}
  \item{X}{matrix of covariates that affects the weights}
  \item{start}{method of initialization of the algorithm}
  \item{link}{type of link function ("global" for global logits, "local" for local logits);
               with global logits a graded response model results; with local logits a  partial credit model results (with dichotomous responses, global logits is the same as using local logits resulting in the Rasch or the 2PL model depending on the value assigned to disc)}
  \item{disc}{indicator of constraints on the discriminating indices (FALSE = all equal to one, TRUE = free)}
  \item{difl}{indicator of constraints on the difficulty levels (FALSE = free, TRUE = rating scale parametrization); difl = TRUE is only admitted in the presence of items with the same number of categories}
  \item{multi1}{matrix with a number of rows equal to the number of dimensions and elements in each row
               equal to the indices of the items measuring the dimension corresponding to that row for the 1st latent variable}
  \item{multi2}{matrix with a number of rows equal to the number of dimensions and elements in each row
               equal to the indices of the items measuring the dimension corresponding to that row for the 2nd latent variable}
  \item{Phi}{initial value of the matrix of the conditional response probabilities (if start="external")}
  \item{ga1t}{initial value of the vector of free discriminating indices (if start="external") for the 1st latent variable}
  \item{ga2t}{initial value of the vector of free discriminating indices (if start="external") for the 2nd latent variable}
  \item{De1}{initial value of regression coefficients for the covariates (if start="external") affecting the 1st latent variable}
  \item{De2}{initial value of regression coefficients for the covariates (if start="external") affecting the 2nd latent variable}
  \item{fort}{to use Fortran routines when possible}
  \item{tol}{tolerance level for checking convergence of the algorithm as relative difference between 
  	         consecutive log-likelihoods}
  \item{maxitc}{maximum number of iterations of the algorithm}  	         
  \item{disp}{to display the likelihood evolution step by step}
  \item{output}{to return additional outputs (Piv1, Piv2, Pp1, Pp2, lkv, Xlabel, XX1dis, XX2dis)}
  \item{out_se}{to return standard errors}
  \item{glob}{to use global logits in the covariates}  
  \item{Zth1}{matrix for the specification of constraints on the support points for the 1st latent variable}  
  \item{zth1}{vector for the specification of constraints on the support points for the 1st latent variable}  
  \item{Zth2}{matrix for the specification of constraints on the support points for the 2nd latent variable}  
  \item{zth2}{vector for the specification of constraints on the support points for the 2nd latent variable}  
  \item{Zbe}{matrix for the specification of constraints on the item difficulty parameters}  
  \item{zbe}{vector for the specification of constraints on the item difficulty parameters}  
  \item{Zga1}{matrix for the specification of constraints on the item discriminating indices for the 1st latent variable}  
  \item{zga1}{vector for the specification of constraints on the item discriminating indices for the 1st latent variable}  
  \item{Zga2}{matrix for the specification of constraints on the item discriminating indices for the 2nd latent variable}  
  \item{zga2}{vector for the specification of constraints on the item discriminating indices for the 2nd latent variable}  
}

\details{
In order to ensure the model identifiability,  the following conditions must hold. First, suitable  constraints on the item parameters are required: one discriminanting index must be equal to 1 and one difficulty parameter must be equal to 0 for each dimension. The constrained items  may be  chosen  in an arbitrary way: by default the algorithm selects the first element of each row of \code{multi1} and \code{multi2}. As a consequence, the user must pay attention to specify matrices \code{multi1} and \code{multi2} so that different items are constrained for each dimension. Second, the maximum number of items shared by the two latent variables is equal to the total number of items minus one, that is, the union of rows of \code{multi1} must differ from the union of rows of \code{multi2}.  
These conditions may be skipped specifying in a suitable way the entries of \code{Zth1}, \code{zth1}, \code{Zth2}, \code{zth2}, \code{Zbe}, \code{zbe}, \code{Zga1}, \code{zga1}, \code{Zga2}, and \code{zga2}, according to the following equations: 

Th1  =  Zth1 \%*\% th1t + zth1 

Th2  =  Zth2 \%*\% th2t + zth2 

Bec  =  Zbe \%*\% bet + zbe 

ga1c  =  Zga1 \%*\% ga1t + zga1 

ga2c  =  Zga2 \%*\% ga2t + zga2, 

where Th1, Th2, Bec, ga1c, ga2c denote the complete matrices/vectors of support points (Th1, Th2), item difficulties (Bec), and item discriminating indices (ga1c, ga2c), whereas th1t, th2t, bet, ga1t, ga2t are the corresponding matrices/vectors of free (i.e., unconstrained) parameters. 
}

\value{
\item{piv1}{estimated vector of weights of the latent classes (average of the weights in case of model with covariates) for the 1st latent variable}
\item{piv2}{estimated vector of weights of the latent classes (average of the weights in case of model with covariates) for the 2nd latent variable}
\item{fv1}{vector indicating the reference item chosen for each latent dimension for the 1st latent variable}
\item{fv2}{vector indicating the reference item chosen for each latent dimension for the 2nd latent variable}
\item{th1t}{estimated matrix of free ability levels for each dimension and for the 1st latent variable}
\item{th2t}{estimated matrix of free ability levels for each dimension and for the 2nd latent variable}
\item{Th1}{complete matrix of free and constrained ability levels for each dimension and latent class for the 1st latent variable}
\item{Th2}{complete matrix of free and constrained ability levels for each dimension and latent class for the 2nd latent variable}
\item{bet}{estimated vector of free difficulty levels for every item (split in two vectors if difl=TRUE)}
\item{Bec}{complete vector of free and constrained difficulty levels for every item (split in two vectors if difl=TRUE)}
\item{ga1t}{estimated vector of free discriminating indices for every item (with all elements equal to 1
	       if disc=FALSE) for the 1st latent variable}
\item{ga2t}{estimated vector of free discriminating indices for every item (with all elements equal to 1
	       if disc=FALSE) for the 2nd latent variable}
\item{ga1c}{complete vector of free and constrained discriminating indices for every item  for the 1st latent variable (with all 			elements equal to 1 if disc=FALSE and NA for items that do not load on the 1st latent variable)}
\item{ga2c}{complete vector of free and constrained discriminating indices for every item  for the 2nd latent variable (with all 			elements equal to 1 if disc=FALSE and NA for items that do not load on the 2nd latent variable)}
\item{De1}{matrix of regression coefficients for the multinomial (or global if glob=TRUE) logit model on the class weights for the 1st latent variable}
\item{De2}{matrix of regression coefficients for the multinomial (or global if glob=TRUE) logit model on the class weights for the 2nd latent variable}
\item{Phi}{array of the conditional response probabilities for every item and each of the k1*k2 latent classes}
\item{lk}{log-likelihood at convergence of the EM algorithm}
\item{np}{number of free parameters}
\item{aic}{Akaike Information Criterion index}
\item{bic}{Bayesian Information Criterion index}
\item{ent}{entropy index to measure the separation of classes}
\item{piv1s}{estimated vector of (ordered) weights of the latent classes (average of the weights in case of model with covariates) for the 1st standardized latent variable}
\item{piv2s}{estimated vector of (ordered) weights of the latent classes (average of the weights in case of model with covariates) for the 2nd standardized latent variable}
\item{Th1s}{standardized ability levels for the 1st latent variable, ordered according to the first dimension}
\item{Th2s}{standardized ability levels for the 2nd latent variable, ordered according to the first dimension}
\item{Becs}{standardized values of item difficulty parameters}
\item{ga1cs}{standardized values of item discriminating indices for the 1st latent variable}
\item{ga2cs}{standardized values of item discriminating indices for the 2nd latent variable}
\item{call}{call of function}  
\item{Pp1}{matrix of the posterior probabilities for each response configuration and latent class for the 1st latent variable
	      (if output=TRUE)}
\item{Pp2}{matrix of the posterior probabilities for each response configuration and latent class for the 2nd latent variable
	      (if output=TRUE)}	      
\item{lkv}{vector to trace the log-likelihood evolution across iterations (if output=TRUE)}
\item{Xlabel}{structure of the design matrix, for internal use (if output=TRUE)}
\item{XX1dis}{design matrix for the covariates affecting the 1st latent variable (if output=TRUE)}
\item{XX2dis}{design matrix for the covariates affecting the 2nd latent variable  (if output=TRUE)}	      
\item{Piv1}{matrix of the weights for every covariate pattern configuration for the 1st latent variable (if output=TRUE)}
\item{Piv2}{matrix of the weights for every covariate pattern configuration for the 2nd latent variable (if output=TRUE)}
\item{seth1t}{standard errors for vector th1t (if out_se=TRUE)}
\item{seth2t}{standard errors for vector th2t (if out_se=TRUE)}
\item{seTh1}{standard errors for vector Th1 (if out_se=TRUE)}
\item{seTh2}{standard errors for vector Th2 (if out_se=TRUE)}
\item{sebet}{standard errors for vector bet (if out_se=TRUE)} 
\item{seBec}{standard errors for vector Bec (if out_se=TRUE)}
\item{sega1t}{standard errors for vector ga1t (if out_se=TRUE)}
\item{sega2t}{standard errors for vector ga2t (if out_se=TRUE)}
\item{sega1c}{standard errors for vector ga1c (if out_se=TRUE)}
\item{sega2c}{standard errors for vector ga2c (if out_se=TRUE)}
\item{seDe1}{standard errors for vector De1 (if out_se=TRUE)}
\item{seDe2}{standard errors for vector De2 (if out_se=TRUE)}
\item{Vnt}{estimated variance-covariance matrix for free parameters (if out_se=TRUE)}
\item{Vn}{complete variance-covariance matrix for all parameters (if out_se=TRUE)}
}

\references{
Bacci, S.  and Bartolucci, F. (2015), A multidimensional finite mixture SEM for non-ignorable missing responses to test items, \emph{Structural Equation Modeling}, \bold{22}, 352-365.

Bacci, S., Bartolucci, F., and Gnaldi, M. (2014), A class of Multidimensional Latent Class IRT models for 
ordinal polytomous item responses, \emph{Communications in Statistics - Theory and Methods}, \bold{43}, 787-800.

Bartolucci, F. (2007), A class of multidimensional IRT models for testing unidimensionality and clustering
items, \emph{Psychometrika}, \bold{72}, 141-157.

Bonifay, W. E. (2015), An illustration of the two-tier item factor analysis model, in S. P. Reise and D. A. Revicki (eds), Handbook of Item Response Theory Modeling, p. 207-225, Routledge.

Cai, L. (2010), A two-tier full-information item factor analysis model with applications, Psychometrika, 75, 581-612.

Cai, L., Yang, J. S., and Hansen, M. (2011), Generalized full-information item bifactor analysis, Psychological Methods, 16, 221-248.
}

\author{Francesco Bartolucci, Silvia Bacci - University of Perugia (IT)}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~
%\section{Warning }{
%}

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{

\dontrun{
# Fit the model under different within-item multidimensional structures
# for SF12_nomiss data
data(SF12_nomiss)
S = SF12_nomiss[,1:12]
X = SF12_nomiss[,13]

# Graded response model with two latent variables sharing six items (free
# discrimination and difficulty parameters; two latent classes for each
# latent variable; one covariate):
multi1 = c(1:5, 8:12)
multi2 = c(6:12, 1)
tol = 10^-6  # decrease tolerance to obtain more reliable results
out1 = est_multi_poly_within(S=S,k1=2,k2=2,X=X,link="global",disc=TRUE,
                             multi1=multi1,multi2=multi2,disp=TRUE,
                             out_se=TRUE,tol=tol) 
                             
# Partial credit model with two latent variables sharing eleven items
# (free discrimination and difficulty parameters; two latent classes for
# the 1st latent variable and three latent classes for the 2nd latent
# variable; one covariate):
multi1 = 1:12
multi2 = 2:12
out2 = est_multi_poly_within(S=S,k1=2,k2=3,X=X,link="local",disc=TRUE,
                             multi1=multi1,multi2=multi2,disp=TRUE,tol=tol)
                               
# Display output:
summary(out2)
out2$lk
out2$Th1 
out2$Th1s
out2$piv1
out2$Th2
out2$Th2s
out2$piv2
out2$De1
out2$De2
}

\dontrun{
## Fit the model under different situations for RLMS data
# Example of use of the function to account for non-ignorable missing
# item responses 
data(RLMS)
X = RLMS[,1:4]
Y = RLMS[,6:9]
YR = cbind(Y,1*(!is.na(Y)))
multi1 = 1:4
multi2 = 5:8
tol = 10^-6  # decrease tolerance to obtain more reliable results

# MAR model
out0 = est_multi_poly_within(YR,k1=3,k2=2,X=X,link="global",
                 disc=TRUE,multi1=multi1,multi2=multi2,disp=TRUE,
                 out_se=TRUE,glob=TRUE,tol=tol) 
                 
# NMAR model
multi1 = 1:8
out1 = est_multi_poly_within(YR,k1=3,k2=2,X=X,link="global",
                 disc=TRUE,multi1=multi1,multi2=multi2,disp=TRUE,
                 out_se=TRUE,glob=TRUE,tol=tol)
                   
# testing effect of the latent trait on missingness
c(out0$bic,out1$bic)
(test1 = out1$ga1c[-1]/out1$sega1c[-1])
}

\dontrun{
## Fit the model under different external constraints on abilities and/or item parameters
data(SF12_nomiss)
S = SF12_nomiss[,1:12]
X = SF12_nomiss[,13]
multi1m = rbind(1:5, 8:12) # two dimensions for the 1st latent variable 
multi2m = rbind(6:9, c(10:12, 1)) # two dimensions for the 2nd latent variable 
k1 = 2
k2 = 2

# Fixed ability levels; all item parameters can be free
Zth1 = matrix(0,nrow(multi1m)*k1,0)
zth1 = c(rep(-1, times=nrow(multi1m)),  rep(1, times=nrow(multi1m)))
Zth2 = matrix(0,nrow(multi2m)*k2,0)
zth2 = c(rep(-1, times=nrow(multi2m)),  rep(1, times=nrow(multi2m)))  
# item difficulties: 10*4 + 2*2 = 44 (10 items with 5 categories plus 2 items with 3 categories)
Zbe = diag(44)
# item discriminating parameters = 10 items loading on the 1st latent variable plus 8 items loading
# on the 2nd latent variable
Zga1 = diag(10); Zga2 = diag(8) 
zga1 = rep(0,nrow(Zga1)); zga1[1] = 1
zga2 = rep(0,nrow(Zga2)); zga2[1] = 1
out1c = est_multi_poly_within(S=S,k1=k1,k2=k2,X=X,link="global",disc=TRUE,multi1=multi1m,
                              multi2=multi2m,disp=TRUE,out_se=TRUE,Zth1=Zth1,zth1=zth1,Zth2=Zth2,
                              zth2=zth2,Zbe=Zbe,Zga1=Zga1,zga1=zga1,Zga2=Zga2,zga2=zga2)   
summary(out1c)
out1c$Bec                             

# Constraint difficulties of the first threshold to be equal for all items 
# and difficulties of the second threshold to be equal for all items; 
# free ability levels
multi1u = c(1:3, 6:10) # one dimension for the 1st latent variable 
multi2u = c(4:10, 1)  # one dimension for the 2nd latent variable
S1 = pmin(as.matrix(S[, -c(2,3)]),2)  # all items have the same number of categories
Zbe = as.matrix((matrix(1,10,1)\%x\%diag(2))[,-1])  
out2c = est_multi_poly_within(S=S1,k1=2,k2=2,X=X,link="global",disc=TRUE,
                             multi1=multi1u,multi2=multi2u,disp=TRUE,
                             out_se=TRUE,Zbe=Zbe)
out2c$Bec   

# Same difficulties for pairs of items 1-6, 2-7, 3-8, 4-9, 5-10; 
# free ability levels
Zbe = (matrix(1,2,1)\%x\%diag(10))[,-1]
out3c = est_multi_poly_within(S=S1,k1=2,k2=2,X=X,link="global",disc=TRUE,
                             multi1=multi1u,multi2=multi2u,disp=TRUE,
                             out_se=TRUE,Zbe=Zbe)
out3c$Bec  

# Add equality constraints on some discriminating indices for the 1st latent variable
Zbe = (matrix(1,2,1)\%x\%diag(10))[,-1]
Zga1 = diag(length(multi1u)); 
# discriminating index of item 1 constrained to 1 for the model identifiability
# discriminating index of item 3 equal to discriminating index of item 2
Zga1 = Zga1[, -c(1, 3)];
Zga1[3, 1] = 1 
zga1 = rep(0,nrow(Zga1)); zga1[1] = 1
out4c = est_multi_poly_within(S=S1,k1=2,k2=2,X=X,link="global",disc=TRUE,
                             multi1=multi1u,multi2=multi2u,disp=TRUE,tol=10^-4,
                             out_se=TRUE,Zbe=Zbe, Zga1=Zga1, zga1=zga1)   
out4c$Bec 
out4c$ga1c
out4c$ga1t                                                           
}
}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{maximum likelihood estimation}
\keyword{Expectation-Maximization algorithm}% __ONLY ONE__ keyword per line
