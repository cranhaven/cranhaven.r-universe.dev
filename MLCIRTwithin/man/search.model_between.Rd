\name{search.model_between}
\alias{search.model_between}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Search for the global maximum of the log-likelihood of between-item muldimensional models}

\description{It searches for the global maximum of the log-likelihood of between-item muldimensional models given a vector of possible number of classes to try for.}

\usage{search.model_between(S, yv = rep(1, ns), kv, X = NULL,
                     link = c("global","local"), disc = FALSE, difl = FALSE,
                     multi = 1:J, fort = FALSE, tol1 = 10^-6, tol2 = 10^-10,
                     glob = FALSE, disp = FALSE, output = FALSE,
                     out_se = FALSE, nrep = 2, Zth=NULL,zth=NULL,
                     Zbe=NULL, zbe=NULL,Zga=NULL,zga=NULL)}
        
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
           (use NA for missing responses)}
  \item{yv}{vector of the frequencies of every response configuration in \code{S}}
  \item{kv}{vector of the possible numbers of latent classes}
  \item{X}{matrix of covariates affecting the weights}
  \item{link}{type of link function ("global" for global logits, "local" for local logits);
              with global logits a graded response model results; with local logits a  partial credit model results
              (with dichotomous responses, global logits is the same as using local logits resulting in the Rasch or
              the 2PL model depending on the value assigned to disc)}
  \item{disc}{indicator of constraints on the discriminating indices (FALSE = all equal to one, TRUE = free)}
  \item{difl}{indicator of constraints on the difficulty levels (FALSE = free, TRUE = rating scale parametrization)}
  \item{multi}{matrix with a number of rows equal to the number of dimensions and elements in each row
               equal to the indices of the items measuring the dimension corresponding to that row for the latent variable}
  \item{fort}{to use Fortran routines when possible}
  \item{tol1}{tolerance level for checking convergence of the algorithm as relative difference between 
  	          consecutive log-likelihoods (initial check based on random starting values)}
  \item{tol2}{tolerance level for checking convergence of the algorithm as relative difference between 
  	          consecutive log-likelihoods (final converngece)}
  \item{glob}{to use global logits in the covariates}  
  \item{disp}{to display the likelihood evolution step by step}
  \item{output}{to return additional outputs (Piv,Pp,lkv)}
  \item{out_se}{to return standard errors}
  \item{nrep}{number of repetitions of each random initialization}
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
\item{out.single}{output of each single model for each \code{k} in \code{kv}; it is similar to output from est_multi_poly_between, with the addition of values of number of latent classes  (\code{k})  and the sequence of log-likelihoods (\code{lktrace}) for the deterministic start, for each random start, and for the final estimation obtained with  a tolerance level equal to \code{tol2}}	
\item{aicv}{Akaike Information Criterion index for each \code{k} in \code{kv}}
\item{bicv}{Bayesian Information Criterion index for each \code{k} in \code{kv}}
\item{entv}{Entropy index for each \code{k} in \code{kv}}
\item{necv}{NEC index for each \code{k} in \code{kv}}
\item{lkv}{log-likelihood at convergence of the EM algorithm  for each \code{k} in \code{kv}}
\item{errv}{trace of any errors occurred during the estimation process for each \code{k} in \code{kv}}
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
# Fit a Graded response model with two latent variables (free discrimination
# and difficulty parameters; two latent classes):
data(SF12_nomiss)
S = SF12_nomiss[,1:12]
X = SF12_nomiss[,13]
multi0 = rbind(c(1:5, 8), c(6:7,9:12))
out1 = search.model_between(S=S,kv=1:3,X=X,link="global",disc=TRUE,
                               multi=multi0,fort=TRUE,disp=TRUE,out_se=TRUE) 

# Display output
out1$lkv
out1$bicv

# Display output with 2 classes:
out1$out.single[[2]]
out1$out.single[[2]]$lktrace
out1$out.single[[2]]$Th
out1$out.single[[2]]$piv
out1$out.single[[2]]$gac
out1$out.single[[2]]$Bec

}

}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{maximum likelihood estimation}
\keyword{Expectation-Maximization algorithm}% __ONLY ONE__ keyword per line
