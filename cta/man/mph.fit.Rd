\name{mph.fit}
\alias{mph.fit}
\title{
Fitting MPH and HLP Models
}
\description{
Computes maximum likelihood estimates and fit statistics for
multinomial-Poisson homogeneous (MPH) and homogeneous linear predictor
(HLP) models for contingency tables.

More detailed \href{https://homepage.divms.uiowa.edu/~jblang/mph.fitting/}{DOCUMENTATION and EXAMPLES} of \code{mph.fit} are \href{https://homepage.divms.uiowa.edu/~jblang/mph.fitting/}{online}.
}
\usage{
mph.fit(y, h.fct = constraint, constraint = NULL, h.mean = FALSE,
        L.fct = link, link = NULL, L.mean = FALSE, X = NULL,
        strata = rep(1, length(y)), fixed.strata = "all",
        check.homog.tol = 1e-9, check.HLP.tol = 1e-9, maxiter = 100,
        step = 1, change.step.after = 0.25 * maxiter, y.eps = 0,
        iter.orig = 5, m.initial = y, norm.diff.conv = 1e-6,
        norm.score.conv = 1e-6, max.score.diff.iter = 10,
        derht.fct = NULL, derLt.fct = NULL, pdlambda = 2/3,
        verbose = FALSE)
}
\arguments{
  \item{y}{Vector (not matrix) of table counts.}
  \item{h.fct}{Function object that defines the constraint function \eqn{h(\cdot)}.
               It must return a column vector. \code{h.fct} can also be set to
               \code{0}, in which case \eqn{h(\cdot)} is viewed as the \eqn{0}
               function, so no constraints are imposed.

               By default, \eqn{h(\cdot)} is viewed as a function of the table
               probabilities \eqn{p}. If \code{h.mean} is set to \code{h.mean = TRUE},
               then \eqn{h(\cdot)} is viewed as a function of the expected counts
               \eqn{m}.

               Default: \code{h.fct = NULL}. If both \code{h.fct = NULL}
               and \code{L.fct = NULL}, then \code{h.fct} is set to \code{0} and
               no constraints are imposed. If \code{h.fct = NULL} and \code{L.fct}
               is not \code{NULL}, then \code{h.fct} will be computed as
               \code{t(U) \%*\% L.fct}.}
  \item{constraint}{Alias for the argument \code{h.fct}. Argument \code{constraint}
                    is secondary to the primary argument \code{h.fct} in the                           following senses: If \code{constraint} and \code{h.fct} are not
                    equal, \code{h.fct} is used.}
  \item{h.mean}{Logical argument. If \code{h.mean = FALSE} (the default), \code{h.fct}
                is treated as a function of \eqn{p}. If \code{h.mean = TRUE}, then
                \code{h.fct} is treated as a function of \eqn{m}.}
  \item{L.fct}{Function object that defines the link \eqn{L(\cdot)} in the HLP model;
               it must return a column vector. Or ... \code{L.fct = keyword},
               where candidate keywords include \code{"logp"} and \code{"logm"}.

               Entering \code{L.fct = "logp"} tells the program to create the
               function object as \code{L.fct <- function(p) {log(p)}}.
               \code{L.fct = "logm"} tells the program to (i) create the
               function object as \code{L.fct <- function(m) {log(m)}} and
               (ii) set \code{L.mean = TRUE}.

               By default, \code{L.fct} is treated as a function of the table
               probabilities \eqn{p} (even if the argument in the \code{L.fct}
               function object is \code{m} ). If \code{L.mean} is set to
               \code{L.mean = TRUE}, then \code{L.fct} is treated as a function
               of the expected counts \eqn{m}. Default: \code{L.fct = NULL} means
               no constraints of the form \eqn{L(p) = X\beta} are imposed.}
  \item{link}{Alias for the argument \code{L.fct}. Argument \code{link} is
              secondary to the primary argument \code{L.fct} in the following
              senses: If \code{link} and \code{L.fct} are not equal, \code{L.fct}
              is used.}
  \item{L.mean}{Logical argument. If \code{L.mean = FALSE} (the default), \code{L.fct}
                is treated as a function of \eqn{p}. If \code{L.mean = TRUE},
                \code{L.fct} is treated as a function of \eqn{m}.}
  \item{X}{HLP model design matrix, assumed to be full rank. Default:
           \code{X = NULL}; i.e., it is left unspecified and unused.}
  \item{strata}{Vector of the same length as \code{y} that gives the stratum
                membership identifier. Default: \code{strata = rep(1, length(y))};
                i.e. the default is the single stratum (non-stratified) setting.
                Examples: \code{strata = A}, or
                \code{strata = c(1,1,1,2,2,2,3,3,3)}, or
                \code{strata = paste(sep = "", "A=", A, ", B=", B)}.}
  \item{fixed.strata}{The object that gives information on which stratum have
                      fixed sample sizes. It can equal one of the keywords,
                      \code{fixed.strata = "all"} or \code{fixed.strata = "none"},
                      or it can be a vector of stratum membership identifiers,
                      e.g. \code{fixed.strata = c(1,3)} or
                      \code{fixed.strata = c("pop1", "pop5")}. Default:
                      \code{fixed.strata = "all"}.}
  \item{check.homog.tol}{Round-off tolerance for \eqn{Z} homogeneity check.
                         Default: \code{check.homog.tol = 1e-9}.}
  \item{check.HLP.tol}{Round-off tolerance for HLP link status check. Default:
                       \code{check.HLP.tol = 1e-9}.}
  \item{maxiter}{Maximum number of iterations. Default: \code{maxiter = 100}.}
  \item{step}{Step-size value. Default: \code{step = 1}.}
  \item{change.step.after}{If the score value increases for more than
                           \code{change.step.after} iterations in a row, then
                           the initial step size is halved. Default:
                           \code{change.step.after = 0.25 * maxiter}.}
  \item{y.eps}{Non-negative constant to be temporarily added to the original
               counts in \code{y}. Default: \code{y.eps = 0}.}
  \item{iter.orig}{Iteration at which the original counts will be used. Default:
                   \code{iter.orig = 5}.}
  \item{m.initial}{Initial estimate of \eqn{m}. Default: \code{m.initial = y}.
                   See Input Note 6 below.}
  \item{norm.diff.conv}{Convergence criteria value; see \code{norm.diff} in the
                        \bold{Value} section. Default: \code{norm.diff.conv = 1e-6}.}
  \item{norm.score.conv}{Convergence criteria value; see \code{norm.score} in the
                        \bold{Value} section. Default:
                         \code{norm.score.conv = 1e-6}.}
  \item{max.score.diff.iter}{The variable \code{score.diff.iter} keeps track of
                             how long \code{norm.score} is smaller than
                             \code{norm.score.conv}, but \code{norm.diff} is
                             greater than \code{norm.diff.conv}. If this is the
                             case too long (i.e.
                             \code{score.diff.iter >= max.score.diff.iter}),
                             then stop the iterations because the solution likely
                             includes at least one ML fitted value of \eqn{0}.
                             Default: \code{max.score.diff.iter = 10}.}
  \item{derht.fct}{Function object that computes analytic derivative of the
                   transpose of the constraint function \eqn{h(\cdot)} with respect
                   to \eqn{m}. If \eqn{h(\cdot)} maps from \eqn{R^p} to \eqn{R^q}
                   (i.e. there are \eqn{q} constraints), then \code{derht.fct}
                   returns a \eqn{p}-by-\eqn{q} matrix of partial derivatives.
                   Default: \code{derht.fct = NULL}. This means that the
                   derivative is calculated numerically.}
  \item{derLt.fct}{Function object that computes analytic derivative of the
                   transpose of the link function \eqn{L(\cdot)} with respect to
                   \eqn{m}. If \eqn{L(\cdot)} maps from \eqn{R^p} to \eqn{R^q} (i.e.
                   there are \eqn{q} link components), then \code{derLt.fct}
                   returns a \eqn{p}-by-\eqn{q} matrix of partial derivatives.
                   Default: \code{derLt.fct = NULL}, i.e. by default this
                   derivative is calculated numerically.}
  \item{pdlambda}{The index parameter \eqn{\lambda} in the power-divergence
                  statistic.}
  \item{verbose}{Logical argument. If \code{verbose = FALSE}, do not print out
                 iteration information. If \code{verbose = TRUE}, then
                 iteration information is printed out. Default:
                 \code{verbose = FALSE}.}
}
\details{
In the following, let \eqn{y} be the vector of contingency table counts, \eqn{p}
be the unknown vector of contingency table probabilities, \eqn{s} be a vector of
strata identifiers, and \eqn{F} be the set of strata with a priori fixed sample
sizes.

Although \code{mph.fit} can fit more general models (see below), two important
special cases include:
\itemize{
  \item MPH (Special-Case): \eqn{y} is
a realization of random vector \eqn{Y}, where \eqn{Y \sim MP(\gamma, p | \texttt{strata} = s, \texttt{fixed} = F)}, \eqn{h(p) = 0}.
  \item HLP (Special-Case): \eqn{y} is
a realization of random vector \eqn{Y}, where \eqn{Y \sim MP(\gamma, p | \texttt{strata} = s, \texttt{fixed} = F)}, \eqn{L(p) = X \beta}.
}

Here, \eqn{h(\cdot)} is a smooth constraint function and \eqn{L(\cdot)} is a smooth link function. It is assumed that the constraints in \eqn{h(p) = 0} are non-redundant so that the Jacobian, \eqn{\partial h'(p) / \partial p}, is full column rank.

The link \eqn{L(\cdot)} is allowed to be many-to-one and row-rank deficient, so this
HLP model is quite general. It is only required that the implied constraints,
\eqn{U'L(p) = 0}, where \eqn{null(U') = span(X)}, are non-redundant.

Here, MP stands for the multinomial-Poisson distribution. The parameters are
\eqn{\gamma}, the vector of expected sample sizes, and \eqn{p}, the vector of table
probabilities.

The notation \deqn{Y \sim MP(\gamma, p | \texttt{strata} = s, \texttt{fixed} = F)} means that the random vector \eqn{Y} is composed of independent blocks of multinomial and/or Poisson random variables. The strata vector \eqn{s} determines the blocks and \eqn{F} determines which blocks are multinomial and which blocks comprise independent Poisson random variables. More specifically, suppose there are \eqn{K} strata, so \eqn{s} contains \eqn{K} distinct strata identifiers. The components in
\eqn{Y} corresponding to \eqn{s = \texttt{identifier[k]}} make up a block. If
\code{identifier[k]} is included in \eqn{F}, then this block has a multinomial distribution and \eqn{\gamma_{k}} is the a priori known, i.e. fixed, sample size. If \code{identifier[k]} is not in \eqn{F}, then this block comprises independent Poisson random variables and \eqn{\gamma_{k}} is an unknown expected sample size.

Note: Given the observed counts \eqn{y}, the pair \eqn{\texttt{(strata, fixed)} = (s, F)}
contains the same information as the sampling plan triple \eqn{(Z, Z_{F}, n_{F})} described in Lang (2004, 2005). Specifically, \eqn{Z = Z(s)}, the strata/population matrix, is determined by \eqn{s}. \eqn{Z_{F} = Z_{F}(s, F)}, the sampling constraint matrix, is determined by \eqn{s} and \eqn{F}. \eqn{n_{F} = Z_{F}'y} is the vector of a priori fixed sample sizes.

Special case MP distributions include...
\itemize{
  \item Full Multinomial:
\eqn{MP(\gamma, p | \texttt{strata = 1, fixed = "all"})}.
A simple random sample of fixed size \eqn{\gamma} is taken from a single strata
(population).
  \item Product Multinomial:
\eqn{MP(\gamma, p | \texttt{strata = s, fixed = "all"})}.
A stratified random sample of fixed sample sizes \eqn{\gamma = (\gamma_{1}, \ldots, \gamma_{K})'} is taken from the \eqn{K} strata determined by \eqn{s}.
  \item Full Poisson:
\eqn{MP(\gamma, p | \texttt{strata = 1, fixed = "none"})}.
A simple random sample is taken from a single strata (population). The sample size is random and follows a Poisson distribution with unknown mean \eqn{\gamma}.
  \item Product Poisson:
\eqn{MP(\gamma, p | \texttt{strata = s, fixed = "none"})}.
A stratified random sample is taken from \eqn{K} strata. The sample sizes are all random and distributed as Poissons with unknown means in
\eqn{\gamma = (\gamma_{1}, \ldots, \gamma_{K})'}.
}

Specifying the MP distribution in \code{mph.fit}...

The user need only enter \code{(strata, fixed.strata)}, the input variables corresponding to \eqn{(s, F)}. Keywords, \code{fixed.strata = "all"} [\code{"none"}] means that all [none] of the strata have a priori fixed sample sizes.

To fit MPH (Special Case), the user must enter the counts \code{y}, the constraint function \code{h.fct} (alias \code{constraint}), and the sampling plan variables, \code{strata} and \code{fixed.strata}. Note: The user can omit the sampling plan variables if the default, multinomial sampling \code{(strata = 1, fixed = "all")}, can be assumed.

To fit HLP (Special Case), the user must enter the counts \code{y}, the link function \code{L.fct} (alias \code{link}), the model matrix \code{X}, and the sampling plan variables, \code{strata} and \code{fixed.strata}. Note: The user can omit the sampling plan variables if the default, multinomial sampling, can be assumed.

IMPORTANT: When specifying the model and creating the input objects for
\code{mph.fit}, keep in mind that the interpretation of the table probabilities \eqn{p} depends on the sampling plan!

Specifically, if the \eqn{i^{th}} count \eqn{y_{i}} is in block \eqn{k} (i.e. corresponds with strata \code{identifier[k]}), then the \eqn{i^{th}} table probability \eqn{p_{i}} is the conditional probability defined as \eqn{p_{i}} = probability of a Type \eqn{i} outcome GIVEN that the outcome is one of the types in stratum \eqn{k}.

For example, in an \eqn{I}-by-\eqn{J} table with row variable \eqn{A} and column variable \eqn{B}, if row-stratified sampling is used, the table probabilities have the interpretation, \eqn{p_{ij} =} prob of a Type \eqn{(i, j)} outcome GIVEN that the outcome is one of the types in stratum \eqn{i} (i.e. one of \eqn{(i, 1), \ldots, (i, J)}) \eqn{ = P(A = i, B = j | A = i)} \eqn{ = P(B = j | A = i)}. For column-stratified sampling, \eqn{p_{ij} = P(A = i | B = j)}. And for non-stratified sampling, \eqn{p_{ij} = P(A = i, B = j)}.

Log-Linear Models: Log-linear models specified as \eqn{\log(p) = X\beta}, are HLP models.

As with any HLP model, \eqn{\log(p) = X\beta} can be restated as a collection of constraints; specifically, \eqn{\log(p) = X\beta} is equivalent to \eqn{h(p) = U'\log(p) = 0}, where \eqn{null(U') = span(X)}. Noting that \eqn{Z'p = 1}, we see that to avoid redundant constraints, \eqn{span(X)} should contain \eqn{span(Z)}. Loosely, fixed-by-sampling-design parameters should be included.

Log-linear models of the form \eqn{\log(p) = X\beta} are simple to fit using \code{mph.fit}. For example,\cr \code{> mph.fit(y, link = "logp", X = model.matrix(~ A + B))},\cr
or, equivalently,\cr \code{> mph.fit(y, link = function(p) {log(p)}, X = model.matrix(~ A + B))}.


MORE GENERAL MPH and HLP MODELS...

Instead of \eqn{(\gamma, p)}, the MP distribution can alternatively be parameterized in terms of the vector of expected table counts, \eqn{m = E(Y)}. Formally, \eqn{(\gamma, p)} and \eqn{m} are in one-to-one correspondence and satisfy:
\deqn{m = Diag(Z\gamma)p,} and \deqn{\gamma = Z'm, p = Diag^{-1}(ZZ'm)m.} Here, \eqn{Z = Z(s)} is the \eqn{c}-by-\eqn{K} strata/population matrix determined by strata vector \eqn{s}. Specifically, \eqn{Z_{ik}  =  I\{s_{i} = \texttt{identifier[k]}\}}.

The MPH (Special-Case) Model given above is a special case because it constrains the expected counts \eqn{m} only through the table probabilities \eqn{p}. Similarly, the HLP (Special-Case) Model given above is a special case because it uses a link function that depends on \eqn{m} only through the table probabilities \eqn{p}.

More generally, \code{mph.fit} computes maximum likelihood estimates and fit
statistics for MPH and HLP models of the form...
\itemize{
  \item  MPH: \eqn{y} is
a realization of random vector \eqn{Y}, where \eqn{Y \sim MP(\gamma, p | \texttt{strata} = s, \texttt{fixed} = F), h(m) = 0}.
  \item  HLP: \eqn{y} is
a realization of random vector \eqn{Y}, where \eqn{Y \sim MP(\gamma, p | \texttt{strata} = s, \texttt{fixed} = F), L(m) = X\beta}.
}

Here, \eqn{h(\cdot)} is a smooth constraint function that must also be \eqn{Z} (i.e. strata \eqn{s}) homogeneous. \eqn{L(\cdot)} is a smooth link function that must also satisfy the HLP conditions with respect to \eqn{Z} (i.e. strata \eqn{s}) and \eqn{X}.
That is,
\itemize{
  \item (1) \eqn{L(\cdot)} has HLP link status with respect to \eqn{Z}, and
  \item (2) The implied constraint function \eqn{h(m) = U'L(m)} is \eqn{Z} homogeneous. Here, \eqn{null(U') = span(X)}.
}

Here, (1) \eqn{L(\cdot)} has HLP link status with respect to \eqn{Z} if, for \eqn{m = Diag(Z\gamma) p},
\itemize{
  \item (1)(a) \eqn{L(m) = a(\gamma) + L(p)}, where
  \eqn{a(\gamma_{1}/\gamma_{2}) - a(1) = a(\gamma_{1}) - a(\gamma_{2})}, i.e. \eqn{a(\gamma)} has the form
  \eqn{C \log \gamma + \texttt{constant}};
               or
  \item (1)(b) \eqn{L(m) = G(\gamma) L(p)}, where \eqn{G(\gamma)} is a diagonal matrix with
  diagonal elements that are powers of the \eqn{\gamma} elements, i.e. \eqn{L(\cdot)} is
  \eqn{Z} homogeneous (see Lang (2004));
               or
  \item (1)(c) The components of \eqn{L(\cdot)} are a mixture of types (a) and (b):
  \eqn{L_{j}(m) = a_{j}(\gamma) + L_{j}(p)} or \eqn{L_{j}(m) = G_{j}(\gamma) L_{j}(p)},
  \eqn{j = 1, \ldots, l}.
}

Lang (2005) described HLP models that satisfied (1)(a) and (2), but the definition
of HLP models can be broadened to include those models satisfying (1) and (2).
That is, HLP models can be defined so they also include models that satisfy (1)(b)
and (2) or (1)(c) and (2). \code{mph.fit} uses this broader definition
of HLP Model.

Note: The input variable \code{h.mean} must be set to \code{TRUE} to fit this more general MPH model. Similarly, the input variable \code{L.mean} must be set to
\code{TRUE} to fit this more general HLP model. (An exception: If the link function
is specified using the keyword \code{"logm"} then \code{L.mean} is automatically
set to \code{TRUE}.)

Note: \code{mph.fit} carries out "necessary-condition" checks
of \eqn{Z} homogeneity of \eqn{h(\cdot)} and HLP link status of \eqn{L(\cdot)} for these general models.

Log-Linear Models: Log-linear models of the form \eqn{\log(m) = X\beta} are HLP models
provided the \eqn{span(X)} contains the \eqn{span(Z)}. Loosely, provided fixed-by-design
parameters are included, the log-linear model is a special case HLP model.

Log-linear models of the form \eqn{\log(m) = X\beta} are simple to fit using
\code{mph.fit}. For example,\cr
\code{> mph.fit(y, link = "logm", X = model.matrix(~ A + B))},\cr
                       or, equivalently,\cr
\code{> mph.fit(y, link = function(m) {log(m)}, L.mean = TRUE, X = model.matrix(~ A + B))}.

Note: Most reasonable generalized log-linear models, which have the form
\eqn{L(m) = C \log Mm  = X\beta}, are also HLP models. See Lang (2005).
}
\value{
\code{mph.fit} returns a list, which includes the following objects:
\item{y}{Vector of counts used in the algorithm for ML estimation. Usually, this
         vector is identical to the observed table counts.}
\item{m}{Vector of ML fitted values.}
\item{covm}{Approximate covariance matrix of fitted values.}
\item{p}{Vector of cell probability ML estimates.}
\item{covp}{Approximate covariance matrix of cell probability estimators.}
\item{lambda}{Vector of Lagrange multiplier ML estimates.}
\item{covlambda}{Approximate covariance matrix of multiplier estimators.}
\item{resid}{Vector of raw residuals (i.e. observed minus fitted counts).}
\item{presid}{Vector of Pearson residuals.}
\item{adjresid}{Vector of adjusted residuals.}
\item{covresid}{Approximate covariance matrix of raw residuals.}
\item{Gsq}{Likelihood ratio statistic for testing \eqn{H_{0}: h(m) = 0} vs.
           \eqn{H_{1}:} not \eqn{H_{0}}.}
\item{Xsq}{Pearson's score statistic (same as Lagrange multiplier statistic)
           for testing \eqn{H_{0}: h(m) = 0} vs. \eqn{H_{1}:} not \eqn{H_{0}}.}
\item{Wsq}{Generalized Wald statistic for testing \eqn{H_{0}: h(m) = 0} vs.
           \eqn{H_{1}:} not \eqn{H_{0}}.}
\item{PD.stat}{Power-divergence statistic (with index parameter \code{pdlambda})
               for testing \eqn{H_{0}: h(m) = 0} vs. \eqn{H_{1}:} not \eqn{H_{0}}.}
\item{df}{Degrees of freedom associated with \code{Gsq}, \code{Xsq}, and \code{PD.stat}.
          \eqn{df = \dim(h)}.}
\item{beta}{Vector of HLP model parameter ML estimates.}
\item{covbeta}{Approximate covariance matrix of HLP model parameter estimators.}
\item{L}{Vector of HLP model link ML estimates.}
\item{Lobs}{Vector of HLP model observed link values, \eqn{L(y)}.}
\item{covL}{Approximate covariance matrix of HLP model link estimators.}
\item{Lresid}{Vector of adjusted link residuals of the form
              \deqn{(L(\texttt{obs}) - L(\texttt{fitted})) / ase(L(\texttt{obs}) - L(\texttt{fitted})).}}
\item{iter}{Number of update iterations performed.}
\item{norm.diff}{Distance between last and second last \code{theta} iterates
                 (\code{theta} is the vector of log fitted values and Lagrange
                 multipliers).}
\item{norm.score}{Distance between the score vector and zero.}
\item{h.fct}{Constraint function used in algorithm.}
\item{h.input.fct}{Constraint function as originally input.}
\item{h.mean}{Logical variable. If \code{h.mean = FALSE}, \code{h.fct} is treated as a
              function of \eqn{p}. If \code{h.mean = TRUE}, \code{h.fct} is treated as
              a function of \eqn{m}.}
\item{derht.fct}{Analytic function used in algorithm that computes derivative of
                 transpose of constraint function \eqn{h}.}
\item{L.fct}{Link function used in algorithm.}
\item{L.input.fct}{Link function as originally input.}
\item{L.mean}{Logical variable. If \code{L.mean = FALSE}, \code{L.fct} is treated as a
              function of \eqn{p}. If \code{L.mean = TRUE}, \code{L.fct} is treated as
              a function of \eqn{m}.}
\item{derLt.fct}{Analytic function used in algorithm that computes derivative of
                 transpose of link function \eqn{L}.}
\item{X}{HLP model design matrix used in algorithm.}
\item{U}{Orthogonal complement of design matrix \eqn{X}.}
\item{triple}{A list object containing the sampling plan triple \eqn{(Z, Z_{F}, n)},
              where \eqn{Z} is the population (or strata) matrix, \eqn{Z_{F}} is the
              sampling constraint matrix, and \eqn{n} is the collection of fixed
              sample sizes.}
\item{strata}{\code{strata} variable used as input.}
\item{fixed.strata}{The strata corresponding to fixed sample sizes.}
\item{warn.message}{Message stating whether or not the original counts were used.}
\item{warn.message.score}{Message stating whether or not the norm score convergence
                          criterion was met.}
\item{warn.message.diff}{Message stating whether or not the norm diff convergence
                         criterion was met.}
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.

Lang, J. B. (2005) Homogeneous linear predictor models for contingency tables, \emph{Journal of the American Statistical Association}, \bold{100}, 121--134.
}
\author{Joseph B. Lang}
\note{
Input Notes:
\enumerate{
 \item CONSTRAINT FUNCTION.

       \code{constraint} is an alias for \code{h.fct}. If \code{h.fct} is a function object, it must return a column vector.

       By default, \code{h.fct} is treated as a function of the table probabilities
       \eqn{p}. To treat \code{h.fct} as a function of the expected counts \eqn{m},
       you must set \code{h.mean = TRUE} (by default, \code{h.mean = FALSE}).

       The fitting algorithm will fail if the constraints in \code{h.fct} \eqn{= 0}
       are redundant.

 \item MODEL WITH NO CONSTRAINTS.

       The model with no constraints can be specified using \code{h.fct = 0}. The
       no-constraint model is the default when neither \code{h.fct} nor \code{L.fct}        are input (i.e. when \code{h.fct = NULL} and \code{L.fct = NULL}).

 \item HLP MODEL SPECIFICATION.

       \code{link} is an alias for \code{L.fct}. For HLP models, both \code{L.fct} and \code{X} must be specified. The design matrix \eqn{X} must be of full column rank. \code{mph.fit} recognizes two keywords for link specification, \code{"logp"} and \code{"logm"}. These are convenient for log-linear modeling. If \code{L.fct} is a function object, it must return a column vector.

       By default, \code{L.fct} is treated as a function of the table probabilities
       \eqn{p}. To treat \code{L.fct} as a function of the expected counts \eqn{m},
       you must set \code{L.mean = TRUE} (by default, \code{L.mean = FALSE}).

       The constraint function \code{h.fct} is typically left unspecified for HLP
       models, but it need not be.

       If \code{h.fct} is left unspecified, it is created within the program as
       \code{h.fct(m) <- function(m) {t(U) \%*\% L.fct(m)}}, where matrix
       \eqn{U} is an orthogonal complement of \eqn{X}. If \code{h.fct} is specified, the constraints implied by \code{L.fct}\eqn{(p) = X\beta}, or \code{L.fct}\eqn{(m) = X\beta}, are ignored.

       Note: Although the HLP constraints are ignored when \code{h.fct} is
       specified, estimates of \eqn{\beta} and the link are computed under the
       model with constraints \code{h.fct}\eqn{(p) = 0} or
       \code{h.fct}\eqn{(m) = 0}.

       The fitting algorithm will fail to converge if the implied constraints,
       \eqn{U' }\code{ L.fct }\eqn{ = 0}, include redundancies.

 \item EXTENDED ML ESTIMATES.

       When ML estimates are non-existent owing to zero counts, \code{norm.diff}
       will not converge to zero, instead it tends to level off at some constant
       positive value. This is because at least one ML fitted value is \eqn{0},
       which on the log scale is \eqn{\log(0) = -\infty}, and the log-scale iterates
       slowly move toward \eqn{-\infty}. One solution to this problem is to set the
       convergence value \code{norm.diff.conv} to some large number so only the
       score convergence criterion is used. In this case, the algorithm often
       converges to a solution that can be viewed as an extended ML estimate, for
       which \eqn{0} estimates are allowed. \code{mph.fit} automates the
       detection of such problems. See the description of the input variable
       \code{max.score.diff.iter} above and the MISC COMPUTATIONAL NOTES in \href{https://homepage.divms.uiowa.edu/~jblang/mph.fitting/mph.fit.documentation.htm}{
       \code{mph.fit} online documentation}.

 \item CONVERGENCE PROBLEMS / FINE TUNING ITERATIONS.

       First check to make sure that the model is correctly specified and redundant
       constraints are avoided.

       When ML estimates exist, but there are non-convergence problems (perhaps
       caused by zero counts), a modification to the tuning parameters \code{step},
       \code{change.step.after}, \code{y.eps}, and/or \code{iter.orig} will often
       lead to convergence.

       With zero counts, it might help to set \code{y.eps = 0.1} (or some other
       positive number) and \code{iter.orig = 5} (the default). This tells the
       program to initially use \code{y + y.eps} rather than the original counts
       \code{y}. At iteration \eqn{5} \eqn{=} \code{iter.orig}, after the algorithm
       has had time to move toward a non-boundary solution, the original counts are
       again used.

       To further mitigate non-convergence problems, the parameter \code{step} can
       be set to a smaller value (default: \code{step = 1}) so the iterates do not
       change as much.

 \item The initial estimate of \eqn{m} is actually
       \code{m.initial + y.eps + 0.01 * ((m.initial + y.eps) == 0)}. The program
       defaults are \code{m.initial = y} and \code{y.eps = 0}. Note: If
       \code{m.initial > 0} and \code{y.eps = 0}, then the initial estimate of
       \eqn{m} is simply \code{m.initial}.
}

Output Notes:
\enumerate{
  \item ITERATION HISTORY.

        An iteration history is printed out when \code{verbose} is set equal to
        \code{TRUE}. A single line of the history looks like the following:

        "\code{iter= 18[0] norm.diff= 3.574936e-08 norm.score= 1.901705e-15}".

        Here, \code{iter} is the number of update iterations performed. The number
        in \code{[]} gives the number of step size searches required within each
        iteration. \code{norm.diff} and \code{norm.score} are defined above.
        Finally, the time elapsed is output. Note: For the model with no
        restrictions (\code{h.fct = 0}), there are no step size changes.

  \item STORING and VIEWING RESULTS.

        To store the results of \code{mph.fit}, issue a command like the following
        example

        \code{>  results <-  mph.fit(y, h.fct = h.fct)}

        Use program \code{mph.summary} to view the results of \code{mph.fit}.
        Specifically, if the results of \code{mph.fit} are saved in object
        \code{results}, submit the command \code{mph.summary(results)}
        [or \code{mph.summary(results, TRUE)} or \code{mph.summary(results, TRUE, TRUE)}
        depending on how much of the output you need to see.]

  \item  The output objects \code{beta}, \code{covbeta}, \code{L}, \code{covL},
         and \code{Lresid} will be set to \code{NA} unless an HLP model is
         specified (i.e. \code{L.fct} and \code{X} are input).
}
}

\seealso{
\code{\link{mph.summary}}, \code{\link{create.Z.ZF}}, \code{\link{create.U}}, \code{\link{num.deriv.fct}}
}
\examples{
# Listed below is a collection of Basic Examples:
# https://homepage.divms.uiowa.edu/~jblang/mph.fitting/mph.basic.numerical.examples.htm

# Another collection of Less Basic Examples is online:
# https://homepage.divms.uiowa.edu/~jblang/mph.fitting/mph.numerical.examples.htm


# EXAMPLE 1. Test whether a binomial probability equals 0.5.
#
# y = (15, 22) <- Y ~ MP(gamma, p | strata = 1, fixed = "all");
# i.e. Y ~ multinomial.
#
# In other symbols,
#
# y = (15, 22) <- Y = (Y[1], Y[2]) ~ multinomial(37, p = (p[1], p[2])).
#
# GOAL: Test H0: p[1] = 0.5 vs. H1: not H0.

a1 <- mph.fit(y = c(15, 22), constraint = function(p) {p[1] - 0.5})

# Alternative specifications...
a2 <- mph.fit(y = c(15, 22), constraint = function(p) {p[1] - p[2]})
a3 <- mph.fit(y = c(15, 22), constraint = function(p) {log(p[1] / p[2])})
a4 <- mph.fit(y = c(15, 22), constraint = function(m) {m[1] - m[2]},
              h.mean = TRUE)
a5 <- mph.fit(y = c(15, 22), link = function(p) {p}, X = matrix(1, 2, 1))
a6 <- mph.fit(y = c(15, 22), link = "logm", X = matrix(1, 2, 1))

# Alternatively, assume that
#
# y = (15, 22) <- Y ~ MP(gamma, p | strata = 1, fixed = "none");
# i.e. Y ~ indep Poisson.
#
# In other symbols,
#
# y = (15, 22) <- Y = (Y[1], Y[2]), where
# Y[i] indep ~ Poisson(gamma * p[i]), i = 1, 2.
#
# GOAL: Test H0: p[1] = 0.5 vs. H1: not H0.

b1 <- mph.fit(y = c(15, 22), constraint = function(p) {p[1] - 0.5},
              fixed.strata = "none")

mph.summary(a1, TRUE)
mph.summary(b1, TRUE)


# EXAMPLE 2. Test whether a multinomial probability vector is uniform.
#            Test whether a multinomial probability vector equals a
#            specific value.
#
# y <- Y = (Y[1], ..., Y[6]) ~ MP(gamma, p | strata = 1, fixed = "all");
# i.e. Y ~ multinomial.
#
# In other symbols,
#
# y <- Y ~ multinomial(15, p = (p[1], ..., p[6]))
#
# GOAL: Test H0: p[1] = p[2] = ... = p[6] vs. H1: not H0.

y <- rmultinom(1, 15, rep(1, 6))
a1 <- mph.fit(y, L.fct = function(p) {p}, X = matrix(1, 6, 1),
              y.eps = 0.1)

# Alternative specification...
a2 <- mph.fit(y, h.fct = function(p) {as.matrix(p[-6] - 1/6)},
              y.eps = 0.1)

mph.summary(a1, TRUE)
mph.summary(a2, TRUE)

# Test whether p = (1, 2, 3, 1, 2, 3) / 12 .

p0 <- c(1, 2, 3, 1, 2, 3) / 12
b <- mph.fit(y, h.fct = function(p) {as.matrix(p[-6] - p0[-6])},
             y.eps = 0.1)
mph.summary(b, TRUE)


# EXAMPLE 3. Test whether a multinomial probability vector satisfies a
#            particular constraint.
#
# Data Source: Agresti 25:2002.
#
# y = (30, 63, 63) <- Y ~ MP(gamma, p | strata = 1, fixed = "all");
# i.e. Y ~ multinomial.
#
# In other symbols,
#
# y = (30, 63, 63) <- Y ~ multinomial(156, p = (p[1], p[2], p[3]))
#
# GOAL: Test H0: p[1] + p[2] = p[1] / (p[1] + p[2]) vs. H1: not H0.

y <- c(30, 63, 63)
h.fct <- function(p) {
    (p[1] + p[2]) - p[1] / (p[1] + p[2])
}
a <- mph.fit(y, h.fct = h.fct)
mph.summary(a, TRUE)


# EXAMPLE 4. Test of Independence in a 2-by-2 Table.
#
# y = (y[1, 1], y[1, 2], y[2, 1], y[2, 2]) = (25, 18, 13, 21)
#   <- Y ~ MP(gamma, p | strata = 1, fixed = "all");
# i.e. Y ~ multinomial.
#
# In other symbols,
# y = (y[1, 1], y[1, 2], y[2, 1], y[2, 2])
#   <- Y ~ multinomial(77, p = (p[1, 1], p[1, 2], p[2, 1], p[2, 2]))
#
# GOAL: Test H0: p[1, 1] * p[2, 2] / p[1, 2] / p[2, 1] = 1
#        vs. H1: not H0.

d <- data.frame(A = c(1, 1, 2, 2), B = c(1, 2, 1, 2),
                count = c(25, 18, 13, 21))

a1 <- mph.fit(y = d$count, h.fct = function(p)
              {log(p[1] * p[4] / p[2] / p[3])})

# Alternative specifications of independence....
a2 <- mph.fit(y = d$count, h.fct = function(p)
              {p <- matrix(p, 2, 2, byrow = TRUE);
               log(p[1, 1] * p[2, 2] / p[1, 2] / p[2, 1])})
a3 <- mph.fit(y = d$count, h.fct = function(p)
              {p[1] * p[4] / p[2] / p[3] - 1})
a4 <- mph.fit(y = d$count, h.fct = function(p)
              {p[1] / (p[1] + p[2]) - p[3] / (p[3] + p[4])})
a5 <- mph.fit(y = d$count, L.fct = "logm",
              X = model.matrix(~ A + B, data = d))

# Suppose we wished to output observed and fitted values of
# log OR, OR, and P(B = 1 | A = 1) - P(B = 1 | A = 2)...

L.fct <- function(p) {
  L <- as.matrix(c(
    log(p[1] * p[4] / p[2] / p[3]),
    p[1] * p[4] / p[2] / p[3],
    p[1] / (p[1] + p[2]) - p[3] / (p[3] + p[4])
  ))
  rownames(L) <- c("log OR", "OR",
                   "P(B = 1 | A = 1) - P(B = 1 | A = 2)")
  L
}

a6 <- mph.fit(y = d$count, h.fct = function(p)
              {log(p[1] * p[4] / p[2] / p[3])},
              L.fct = L.fct, X = diag(3))

# Unrestricted Model...
b <- mph.fit(y = d$count, L.fct = L.fct, X = diag(3))

mph.summary(a6, TRUE)
mph.summary(b, TRUE)


# EXAMPLE 5. Test of Independence in a 4-by-4 Table.
#            (Using Log-Linear Model.)
#
# Data Source: Table 2.8, Agresti, 57:2002.
#
# y <- Y ~ MP(gamma, p | strata = 1, fixed = "all");
# i.e. Y ~ multinomial.
#
# In other symbols,
# y <- Y ~ multinomial(96, p = (p[1, 1], p[1, 2], p[2, 1], p[2, 2]))
#
# GOAL: Test H0: p[1, 1] * p[2, 2] / p[1, 2] / p[2, 1] = 1 vs. H1: not H0.

d <- data.frame(Income = c("<15", "<15", "<15", "<15", "15-25", "15-25",
                           "15-25", "15-25", "25-40", "25-40", "25-40",
                           "25-40", ">40", ">40", ">40", ">40"),
                JobSatisf = c("VD", "LD", "MS", "VS", "VD", "LD", "MS", "VS",
                              "VD", "LD", "MS", "VS", "VD", "LD", "MS", "VS"),
                count = c(1, 3, 10, 6, 2, 3, 10, 7, 1, 6, 14, 12, 0, 1, 9, 11))

a <- mph.fit(y = d$count, link = "logp",
             X = model.matrix(~ Income + JobSatisf, data = d))
mph.summary(a)

# Alternatively,
b <- mph.fit(y = d$count, link = "logm",
             X = model.matrix(~ Income + JobSatisf, data = d))
mph.summary(b)


# EXAMPLE 6. Test Marginal Homogeneity in a 3-by-3 Table.
#
# Data Source: Table 10.16, Agresti, 445:2002.
#
# y <- Y ~ MP(gamma, p | strata = 1, fixed = "all");
# i.e. Y ~ multinomial.
#
# Specifically,
# y <- Y ~ multinomial(160, p = (p[1, 1], ..., p[3, 3]))
#
# GOAL: Test H0: p[1, +] = p[+, 1], p[2, +] = p[+, 2], p[3, +] = p[+, 3]
#        vs. H1: not H0.

d <- data.frame(Siskel = c("Pro", "Pro", "Pro", "Mixed", "Mixed",
                           "Mixed", "Con", "Con", "Con"),
                Ebert = c("Pro", "Mixed", "Con", "Pro", "Mixed",
                          "Con", "Pro", "Mixed", "Con"),
                count = c(64, 9, 10, 11, 13, 8, 13, 8, 24))

h.fct <- function(p){
    p.Siskel <- M.fct(d$Siskel) \%*\% p
    p.Ebert  <- M.fct(d$Ebert) \%*\% p
    as.matrix(c(p.Siskel[-3] - p.Ebert[-3]))
}
a1 <- mph.fit(y = d$count, h.fct = h.fct)
mph.summary(a1, TRUE)

# Suppose that we wish to report on the observed and fitted
# marginal probabilities.

L.fct <- function(p) {
    p.Siskel <- M.fct(d$Siskel) \%*\% p
    p.Ebert <- M.fct(d$Ebert) \%*\% p
    L <- as.matrix(c(p.Siskel, p.Ebert))
    rownames(L) <- c(paste(sep = "", "P(Siskel=", levels(as.factor(d$Siskel)), ")"),
                     paste(sep = "", "P(Ebert=", levels(as.factor(d$Ebert)), ")"))
    L
}
a2 <- mph.fit(y = d$count, h.fct = h.fct, L.fct = L.fct, X = diag(6))
mph.summary(a2, TRUE)

# M.fct(factor) \%*\% p gives the marginal probabilities corresponding to
# the levels of 'factor'. The marginal probabilities are ordered by the
# levels of 'factor'.
#
# Alternatively, in this rectangular table setting, we can find the
# marginal probabilities using the apply(...) function. In this case,
# the marginal probabilities are ordered as they are entered in the
# data set.

h.fct <- function(p) {
    p <- matrix(p, 3, 3, byrow = TRUE)
    p.Siskel <- apply(p, 1, sum)
    p.Ebert <- apply(p, 2, sum)
    as.matrix(c(p.Siskel[-3] - p.Ebert[-3]))
}

L.fct <- function(p) {
    p <- matrix(p, 3, 3, byrow = TRUE)
    p.Siskel <- apply(p, 1, sum)
    p.Ebert <- apply(p, 2, sum)
    L <- as.matrix(c(p.Siskel, p.Ebert))
    rownames(L) <- c("P(Siskel=Pro)", "P(Siskel=Mixed)",
                     "P(Siskel=Con)", "P(Ebert=Pro)",
                     "P(Ebert=Mixed)", "P(Ebert=Con)")
    L
}
b <- mph.fit(y = d$count, h.fct = h.fct, L.fct = L.fct, X = diag(6))


# EXAMPLE 7. Log-Linear Model for 2-by-2-by-2 Table.
#
# Data Source: Table 8.16, Agresti 347:2002
#
# y <- Y ~ MP(gamma, p | strata = 1, fixed = "all");
# i.e. Y ~ multinomial.
#
# Specifically,
#
# y <- Y ~ multinomial(621, p).
#
# The counts in y are cross-classification counts for variables
# G = Gender, I = Information Opinion, H = Health Opinion.
#
# GOAL: Fit the loglinear models [GI, GH, IH] and [G, IH].

d <- data.frame(G = c("Male", "Male", "Male", "Male",
                      "Female", "Female", "Female", "Female"),
                I = c("Support", "Support", "Oppose", "Oppose",
                      "Support", "Support", "Oppose", "Oppose"),
                H = c("Support", "Oppose", "Support", "Oppose",
                      "Support", "Oppose", "Support", "Oppose"),
                count = c(76, 160, 6, 25, 114, 181, 11, 48))

# Fit loglinear model [GI, GH, IH]...

a1 <- mph.fit(y = d$count, link = "logm",
              X = model.matrix(~ G + I + H + G:I + G:H + I:H, data = d))

# Fit loglinear model [G, IH]...

a2 <- mph.fit(y = d$count, link = "logm",
              X = model.matrix(~ G + I + H + I:H, data = d))

# Different Sampling Distribution Assumptions:
#
# Alternatively, assume
# y <- Y ~ MP(gamma, p | strata = 1, fixed = "none");
# that is, Y ~ indep Poisson.
#
# In other symbols,
# y <- Y, where Y[i] indep ~ Poisson(m[i] = gamma * p[i]).
# Here, gamma is the unknown expected sample size.

b2 <- mph.fit(y = d$count, link = "logm",
              X = model.matrix(~ G + I + H + I:H, data = d),
              fixed = "none")

# Alternatively, assume
# y <- Y ~ MP(gamma, p | strata = Gender, fixed = "all");
# that is, Y ~ prod multinomial.
#
# In other symbols,
# y <- Y = (Y[1, 1, 1], Y[1, 1, 2], ..., Y[2, 2, 2]),
# where (Y[i, 1, 1], ..., Y[i, 2, 2]) indep ~ multinomial(n[i], p[i, , ]).
# Here, p[i, j, k] = P(I = j, H = k | G = i) and n[1] = 267 and
# n[2] = 354 are the a priori fixed sample sizes for males and females.

c2 <- mph.fit(y = d$count, link = "logm",
              X = model.matrix(~ G + I + H + I:H, data = d),
              strata = d$G)

# Alternatively, assume
# y <- Y ~ MP(gamma, p | strata = Gender, fixed = "none");
# that is, Y ~ prod Poisson.
#
# In other symbols,
# y <- Y = (Y[1, 1, 1], Y[1, 1, 2], ..., Y[2, 2, 2]),
# where Y[i, j, k] indep ~ Poisson(m[i, j, k] = gamma[i] * p[i, j, k]).
# Here, p[i, j, k] = P(I = j, H = k | G = i) and gamma[1] and gamma[2] are the
# unknown expected sample sizes for males and for females.

d2 <- mph.fit(y = d$count, link = "logm",
              X = model.matrix(~ G + I + H + I:H, data = d),
              strata = d$G, fixed = "none")

cbind(a2$m, b2$m, c2$m, d2$m, sqrt(diag(a2$covm)), sqrt(diag(b2$covm)),
      sqrt(diag(c2$covm)), sqrt(diag(d2$covm)))
cbind(a2$p, b2$p, c2$p, d2$p, sqrt(diag(a2$covp)), sqrt(diag(b2$covp)),
      sqrt(diag(c2$covp)), sqrt(diag(d2$covp)))


# EXAMPLE 8. Fit Linear-by-Linear Log-Linear Model
#
# Data Source: Table 8.15, Agresti, 345:2002
#
# y <- Y ~ MP(gamma, p | strata = 1, fixed = "all");
# i.e. Y ~ multinomial.
#
# Specifically,
# y <- Y ~ multinomial(1425, p)
#
# GOAL: Assess the fit of the linear-by-linear log-linear model.

d <- list(Schooling = c("<HS", "<HS", "<HS", "HS", "HS", "HS", ">HS", ">HS", ">HS"),
          Abortion = c("Disapprove", "Middle", "Approve", "Disapprove", "Middle",
                       "Approve", "Disapprove", "Middle", "Approve"),
          count = c(209, 101, 237, 151, 126, 426, 16, 21, 138))

Schooling.score <- -1 * (d$Schooling == "<HS") +
                    0 * (d$Schooling == "HS") +
                    1 * (d$Schooling == ">HS")
Abortion.score  <- -1 * (d$Abortion == "Disapprove") +
                    0 * (d$Abortion == "Middle") +
                    1 * (d$Abortion == "Approve")

d <- data.frame(d, Schooling.score, Abortion.score)

a <- mph.fit(y = d$count, link = "logm",
             X = model.matrix(~ Schooling + Abortion +
             Schooling.score : Abortion.score, data = d))
mph.summary(a, TRUE)


# EXAMPLE 9. Marginal Standardization of a Contingency Table.
#
# Data Source: Table 8.15, Agresti 345:2002.
#
# GOAL: For a two-way table, find the standardized values of y, say y*,
# that satisfy (i) y* has the same odds ratios as y, and
#             (ii) y* has row and column totals equal to 100.
#
# Note: This is equivalent to the problem of finding the fitted values
# for the following model...
# x <- Y ~ multinomial(n, p = (p[1, 1], ..., p[3, 3]))
#      p[1, +] = p[2, +] = p[3, +] = p[+, 1] = p[+, 2] = p[+, 3] = 1/3
#      p[1, 1] * p[2, 2] / p[2, 1] / p[1, 2] = or[1, 1]
#      p[1, 2] * p[2, 3] / p[2, 2] / p[1, 3] = or[1, 2]
#      p[2, 1] * p[3, 2] / p[3, 1] / p[2, 2] = or[2, 1]
#      p[2, 2] * p[3, 3] / p[3, 2] / p[2, 3] = or[2, 2],
# where or[i, j] = y[i, j] * y[i + 1, j + 1] / y[i + 1, j] / y[i, j + 1]
# are the observed (y) odds ratios.
# If m is the vector of fitted values, then y* = m * 300 / sum(m)
# are the standardized values of y.
# Here x can be any vector of 9 counts.
# Choosing x so that the sum is 300 leads to sum(m) = 300, so that
# y* = m in this case.

d <- data.frame(Schooling = c("<HS", "<HS", "<HS", "HS", "HS", "HS", ">HS", ">HS", ">HS"),
                Abortion = c("Disapprove", "Middle", "Approve", "Disapprove", "Middle",
                             "Approve", "Disapprove", "Middle", "Approve"),
                count = c(209, 101, 237, 151, 126, 426, 16, 21, 138))

h.fct <- function(p) {
   p.Schooling <- M.fct(d$Schooling) \%*\% p
   p.Abortion  <- M.fct(d$Abortion) \%*\% p
   p <- matrix(p, 3, 3, byrow = TRUE)
   as.matrix(c(
     p.Schooling[-3] - 1/3, p.Abortion[-3] - 1/3,
     p[1, 1] * p[2, 2] / p[2, 1] / p[1, 2] - 209 * 126 / 151 / 101,
     p[1, 2] * p[2, 3] / p[2, 2] / p[1, 3] - 101 * 426 / 126 / 237,
     p[2, 1] * p[3, 2] / p[3, 1] / p[2, 2] - 151 * 21 / 16 / 126,
     p[2, 2] * p[3, 3] / p[3, 2] / p[2, 3] - 126 * 138 / 21 / 426
   ))
}

b <- mph.fit(y = d$count, h.fct = h.fct)
ystar <- b$m * 300 / sum(b$m)
matrix(round(ystar, 1), 3, 3, byrow = TRUE)

x <- c(rep(33, 8), 36)
b <- mph.fit(y = x, h.fct = h.fct)
ystar <- b$m
matrix(round(ystar, 1), 3, 3, byrow = TRUE)


# EXAMPLE 10. Cumulative Logit Model.
#
# Data Source: Table 7.19, Agresti, 306:2002.
#
# y <- Y ~ MP(gamma, p | strata = Therapy * Gender, fixed = "all");
# i.e. Y ~ prod multinomial.
#
# Here, y[i, j, k] is the cross-classification count corresponding to
# Therapy = i, Gender = j, Response = k.
#
# The table probabilities are defined as
# p[i, j, k] = P(Response = k | Therapy = i, Gender = j).
#
# Goal: Fit the cumulative logit proportional odds model that includes
# the main effect of Therapy and Gender.

d <- data.frame(Therapy = c("Sequential", "Sequential", "Sequential", "Sequential",
                            "Sequential", "Sequential", "Sequential", "Sequential",
                            "Alternating", "Alternating", "Alternating", "Alternating",
                            "Alternating", "Alternating", "Alternating", "Alternating"),
                Gender = c("Male", "Male", "Male", "Male", "Female", "Female",
                           "Female", "Female", "Male", "Male", "Male", "Male",
                           "Female", "Female", "Female", "Female"),
                Response = c("Progressive", "NoChange", "Partial", "Complete",
                             "Progressive", "NoChange", "Partial", "Complete",
                             "Progressive", "NoChange", "Partial", "Complete",
                             "Progressive", "NoChange", "Partial", "Complete"),
                count = c(28, 45, 29, 26, 4, 12, 5, 2, 41, 44, 20, 20, 12, 7, 3, 1))

strata <- paste(sep = "", d$Therapy, ".", d$Gender)
d <- data.frame(d, strata)

d3 <- subset(d, Response != "Complete")
levels(d3$Response) <- c(NA, "NoChange", "Partial", "Progressive")

L.fct <- function(p) {
   p <- matrix(p, 4, 4, byrow = TRUE)
   clogit <- c()
   for (s in 1:4) {
     clogit <- c(clogit,
                 log(sum(p[s, 1])   / sum(p[s, 2:4])),
                 log(sum(p[s, 1:2]) / sum(p[s, 3:4])),
                 log(sum(p[s, 1:3]) / sum(p[s, 4]))
     )
   }
   L <- as.matrix(clogit)
   rownames(L) <- c(paste(sep = "", "log odds(R < ", 2:4, "|",
                          d3$strata, ")"))
   L
}

a <- mph.fit(d$count, link = L.fct,
             X = model.matrix(~ -1 + Response + Therapy + Gender,
                              data = d3),
             strata = strata)

# Fit the related non-proportional odds cumulative logit model
b <- mph.fit(d$count, link = L.fct,
             X = model.matrix(~ Response + Response * Therapy +
                                Response * Gender - 1 - Therapy - Gender,
                              data = d3),
             strata = strata)

mph.summary(a, TRUE)
mph.summary(b, TRUE)

}
\keyword{htest}% use one of  RShowDoc("KEYWORDS")

