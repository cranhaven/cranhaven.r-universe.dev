\name{search.model_within}
\alias{search.model_within}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Search for the global maximum of the log-likelihood of within-item muldimensional models}

\description{It searches for the global maximum of the log-likelihood of within-item muldimensional models given a vector of possible number of classes to try for.}

\usage{search.model_within(S, yv = rep(1, ns), kv1, kv2, X = NULL, 
                    link = c("global","local"), disc = FALSE, difl = FALSE, 
                    multi1, multi2, fort = FALSE, tol1 = 10^-6, tol2 = 10^-10,
                    glob = FALSE, disp = FALSE, output = FALSE, out_se = FALSE, 
                    nrep = 2, Zth1 = NULL, zth1 = NULL, Zth2=NULL, zth2=NULL, 
                    Zbe=NULL, zbe=NULL, Zga1=NULL, zga1=NULL, Zga2=NULL, 
                    zga2=NULL)}
        
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
           (use NA for missing responses)}
  \item{yv}{vector of the frequencies of every response configuration in \code{S}}
  \item{kv1}{vector of the possible numbers of ability levels (or latent classes) for the 1st latent variable}
  \item{kv2}{vector of the possible numbers of ability levels (or latent classes) for the 2nd latent variable}
  \item{X}{matrix of covariates affecting the weights}
  \item{link}{type of link function ("global" for global logits, "local" for local logits);
              with global logits a graded response model results; with local logits a  partial credit model results
              (with dichotomous responses, global logits is the same as using local logits resulting in the Rasch or
              the 2PL model depending on the value assigned to disc)}
  \item{disc}{indicator of constraints on the discriminating indices (FALSE = all equal to one, TRUE = free)}
  \item{difl}{indicator of constraints on the difficulty levels (FALSE = free, TRUE = rating scale parametrization)}
  \item{multi1}{matrix with a number of rows equal to the number of dimensions and elements in each row equal to
  	                 the indices of the items measuring the dimension corresponding to that row for the 1st latent variable}
  \item{multi2}{matrix with a number of rows equal to the number of dimensions and elements in each row equal to
  	                 the indices of the items measuring the dimension corresponding to that row for the 2nd latent variable}
  \item{fort}{to use Fortran routines when possible}
  \item{tol1}{tolerance level for checking convergence of the algorithm as relative difference between 
  	          consecutive log-likelihoods (initial check based on random starting values)}
  \item{tol2}{tolerance level for checking convergence of the algorithm as relative difference between 
  	          consecutive log-likelihoods (final convergence)}
  \item{glob}{to use global logits in the covariates}  
  \item{disp}{to display the likelihood evolution step by step}
  \item{output}{to return additional outputs (Piv,Pp,lkv)}
  \item{out_se}{to return standard errors}
  \item{nrep}{number of repetitions of each random initialization}
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

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{out.single}{output of each single model for each \code{k} in \code{kv1} and \code{kv2}; it is similar to output from est_multi_poly_within, with the addition of values of number of latent classes for the 1st latent variable (\code{k1}) and the 2nd latent variable (\code{k2}) and the sequence of log-likelihoods (\code{lktrace}) for the deterministic start, for each random start, and for the final estimation obtained with  a tolerance level equal to \code{tol2}}	
\item{aicv}{Akaike Information Criterion index  for each  \code{k} in \code{kv1} and \code{kv2}}
\item{bicv}{Bayesian Information Criterion index  for each \code{k}  in \code{kv1} and \code{kv2}}
\item{entv}{Entropy index for each \code{k} in \code{kv1} and \code{kv2}}
\item{necv}{NEC index for each \code{k} in \code{kv1} and \code{kv2}}
\item{lkv}{log-likelihood at convergence of the EM algorithm  for each  \code{k}  in \code{kv1} and \code{kv2}}
\item{errv}{trace of any errors occurred during the estimation process for each  \code{k}  in \code{kv1} and \code{kv2}}
}

\references{
Bartolucci, F., Bacci, S. and Gnaldi, M. (2014), MultiLCIRT: An R package for multidimensional latent class item response models,
\emph{Computational Statistics & Data Analysis}, \bold{71}, 971-985.
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
# Fit the model under different within-item multidimensional structures
# for SF12_nomiss data
data(SF12_nomiss)
S = SF12_nomiss[,1:12]
X = SF12_nomiss[,13]

# Partial credit model with two latent variables sharing six items 
# (free difficulty parameters and constrained discriminating parameters; 
# 1 to 3 latent classes for the 1st latent variable and 1 to 2 classes for the 2nd latent variable; 
# one covariate):
multi1 = c(1:5, 8:12)
multi2 = c(6:12, 1)
out1 = search.model_within(S=S,kv1=1:3,kv2=1:2,X=X,link="global",disc=FALSE,
                             multi1=multi1,multi2=multi2,disp=TRUE,
                             out_se=TRUE,tol1=10^-4, tol2=10^-7, nrep=1)
                             
# Main output
out1$lkv 
out1$aicv
out1$bicv 
# Model with 2 latent classes for each latent variable
out1$out.single[[4]]$k1 
out1$out.single[[4]]$k2 
out1$out.single[[4]]$Th1          
out1$out.single[[4]]$Th2 
out1$out.single[[4]]$piv1 
out1$out.single[[4]]$piv2   
out1$out.single[[4]]$ga1c
out1$out.single[[4]]$ga2c   
out1$out.single[[4]]$Bec            
}

}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{maximum likelihood estimation}
\keyword{Expectation-Maximization algorithm}% __ONLY ONE__ keyword per line
