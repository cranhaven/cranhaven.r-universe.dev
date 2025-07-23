\name{createCon}
\alias{createCon}
\title{
%%  ~~function to do ... ~~
Create a list of constraints for co-data weight estimation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create a list of constraints to be used by \code{\link{ecpc}} in estimating G co-data weights. Combine constraints with p-splines to estimate shape-constrained functions, e.g. positive, monotone increasing and/or convex functions.
}
\usage{
createCon(G, shape = "positive+monotone.i+convex")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{G}{
%%     ~~Describe \code{G} here~~
Number of co-data weights that should be estimated subject to constraints. 
}
  \item{shape}{
%%     ~~Describe \code{shape} here~~
Common type of shapes, including `positive', 'monotone.i' ('monotone.d') for monotonically increasing (decreasing), 'convex' ('concave'), or any combination thereof by attaching multiple with a '+' sign.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of the form \code{list(M.ineq = M.ineq, b.ineq = b.ineq)} with the matrix M.ineq and vector b.ineq containing the inequality constraints corresponding to the given shape.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
The relation between the prior variance and co-data may be estimated with a shape-constrained spline, see \code{\link{createZforSplines}} and \code{\link{createS}} for creating a spline basis and difference penalty matrix for a co-data variable. See \code{\link{ecpc}} for an example.
}
\examples{
#create constraints for positivity
Con1 <- createCon(G=10, shape="positive") 
#create constraints for positive and monotonically increasing weights
Con2 <- createCon(G=10, shape="positive+monotone.i")  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
