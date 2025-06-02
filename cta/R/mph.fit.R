######################   Begin mph.fit ##########################################

mph.fit  <- function(y, h.fct=constraint,constraint=NULL, h.mean=FALSE,
                     L.fct=link, link=NULL, L.mean=FALSE, X=NULL,
                     strata=rep(1,length(y)),fixed.strata="all",
                     check.homog.tol=1e-9, check.HLP.tol=1e-9,
                     maxiter=100, step=1, change.step.after=0.25*maxiter,
                     y.eps=0,   iter.orig=5,    m.initial=y,
                     norm.diff.conv=1e-6,norm.score.conv=1e-6, max.score.diff.iter=10,
                     derht.fct=NULL,derLt.fct=NULL, pdlambda = 2/3,
                     verbose=FALSE)
{
  # mph.fit, version 4.1
  # Revised: 8/15/21
  #
  # Originally Created: August 25, 2000 (last revised: 2/12/07, 4/23/09, 5/20/09, 3/27/13, 4/17/13)
  #
  # Author:   Joseph B. Lang,
  #           Dept. of Statistics and Actuarial Science
  #           Univ. of Iowa, Iowa City, IA 52242
  #           joseph-lang@uiowa.edu
  #
  # References:? Lang, J.B. (2004).? "Multinomial-Poisson Homogeneous
  #???????????????  Models for Contingency Tables,"?Annals of Statistics,
  #                 32, 340-383.
  #
  #????????????? Lang, J.B. (2005). "Homogeneous Linear Predictor Models
  #??????????????? for Contingency Tables,"? JASA, 100, 121-134
  #
  # DESCRIPTION:
  #
  #   This program computes maximum likelihood estimates and fit statistics for
  #   multinomial-Poisson homogeneous (MPH) and homogeneous linear
  #   predictor (HLP) models for contingency tables (see Lang 2004, 2005).
  #
  #   In the following,  let y  be the vector of  c table counts,  p be the
  #   unknown vector of c table probabilities, s be a vector of c strata identifiers,
  #   and F be the set of strata with a priori fixed sample sizes.
  #
  #   Although mph.fit can fit more general models (see below), two important special
  #   cases include:
  #
  #     MPH (Special-Case):    y <- Y ~ MP(nu, p | strata=s, fixed=F),   h(p) = 0.
  #
  #     HLP (Special-Case):    y <- Y ~ MP(nu, p | strata=s, fixed=F),   L(p) = X beta.
  #
  #   Here,  h(.) is a smooth constraint function and L(.) is a smooth link function.
  #   It is assumed that the constraints in h(p) = 0 are non-redundant so that
  #   the Jacobian, d h(p)'/d p,   is full column rank.
  #
  #   The link L(.) is allowed to be many-to-one and row-rank deficient, so
  #   this HLP model is quite general.    It is only required that the implied
  #   constraints, U'L(p) = 0, where null(U') = span(X), are non-redundant.
  #
  #   Here, MP stands for the Multinomial-Poisson distribution.  The parameters
  #   are nu, the vector of expected sample sizes, and p, the vector of
  #   table probabilities.
  #
  #   The notation     y <- Y ~ MP(nu, p| strata=s, fixed=F)      means that
  #   y is a realization of random vector Y, which is composed of
  #   independent blocks of multinomial and/or Poisson random variables.
  #   The strata vector s determines the blocks and F determines which blocks
  #   are multinomial and which blocks comprise independent Poisson random variables.
  #   More specifically, suppose there are K strata, so s contains K distinct strata
  #   identifiers.   The components in Y corresponding to s=identifier[k]
  #   make up a block.  If identifier[k] is included in F, then this block has a
  #   multinomial distribution and nu[k] is the a priori known, i.e. fixed, sample size.
  #   If identifier[k] is not in F, then this block comprises independent Poisson random
  #   variables and nu[k] is an unknown expected sample size.
  #
  #   Note:  Given the observed counts y, the pair (strata,fixed)=(s, F)
  #          contains the same information as the sampling plan triple (Z,ZF,nF)
  #          described in Lang (2004,2005).  Specifically,
  #
  #          Z=Z(s),  the strata/population matrix, is determined by s.
  #          ZF = ZF(s,F), the sampling constraint matrix, is determined by s and F.
  #          nF  = ZF'y  is the vector of a priori fixed sample sizes.
  #
  #   Special case MP distributions include...
  #
  #     Full Multinomial:    MP(nu,p| strata=1, fixed="all")
  #                             A simple random sample of fixed size nu is taken
  #                             from a single strata (population).
  #
  #     Product Multinomial: MP(nu,p| strata=s, fixed="all")
  #                             A stratified random sample of fixed sample sizes
  #                             nu=(nu[1],...,nu[K]) is taken from the K strata
  #                             determined by s.
  #
  #     Full Poisson:        MP(nu,p| strata=1, fixed="none")
  #                             A simple random sample is taken from a single
  #                             strata (population).  The sample size is random and
  #                             follows a Poisson distribution with unknown mean nu.
  #
  #     Product Poisson:     MP(nu,p| strata=s, fixed="none")
  #                             A stratified random sample is taken from K strata.
  #                             The sample sizes are all random and distributed
  #                             as Poissons with unknown means in nu=(nu[1],...,nu[K]).
  #
  #   Specifying the MP distribution in 'mph.fit'...
  #
  #         In older versions of mph.fit (2.1 and lower),  the user
  #         had to enter the sampling plan triple (Z,ZF,n).
  #         In 'mph.fit', version 3.0 and higher, the user need only
  #         enter ('strata', 'fixed.strata'), the input variables
  #         corresponding to (s, F).  Keywords, fixed.strata="all" ["none"]
  #         means that all [none] of the strata have a priori fixed sample sizes.
  #
  #   To fit MPH (Special Case),  the user must enter the counts 'y', the
  #   constraint function 'h.fct' (alias 'constraint'),  and the sampling plan
  #   variables, 'strata' and 'fixed.strata'.  Note:   The user can omit the sampling plan
  #   variables if the default, multinomial sampling (strata=1,fixed="all"), can be assumed.
  #
  #   To fit HLP (Special Case),  the user must enter the counts 'y', the
  #   link function 'L.fct' (alias 'link'),  the model matrix 'X', and the sampling
  #   plan variables, 'strata' and 'fixed.strata'.  Note:  The user can omit the
  #   sampling plan variables if the default, multinomial sampling, can be assumed.
  #
  #   IMPORTANT:  When specifying the model and creating the input objects for 'mph.fit',
  #   keep in mind that the interpretation of the table probabilities p depends on the
  #   sampling plan!
  #
  #     Specifically, if  the  i^{th} count y[i] is in block k (i.e. corresponds with
  #     strata identifier[k]),  then the i^{th} table probability p[i] is the conditional
  #     probability defined as
  #       p[i] = probability of a Type i outcome GIVEN that the outcome is one
  #              of the types in stratum k.
  #
  #     For example, in an IxJ table with row variable A and column variable B,
  #     if row-stratified sampling is used, the table probabilities have the interpretation,
  #     p[i,j] = prob of a Type (i,j) outcome GIVEN that the outcome is one
  #              of the types in stratum i (i.e. one of (i,1),...,(i,J)).
  #            = P(A=i,B=j|A=i)
  #            = P(B=j|A=i).
  #     For column-stratified sampling,  p[i,j] = P(A=i|B=j).   And for non-stratified
  #     sampling,  p[i,j] = P(A=i,B=j).
  #
  #   Log-Linear Models:  Log-Linear models specified as log(p) = X beta,  are  HLP models.
  #
  #       As with any HLP model, log(p) = X beta can be restated as a collection of
  #       constraints; specifically,  log(p) = X beta is equivalent to h(p) = U'log(p) = 0,
  #       where null(U') = span(X).   Noting that Z'p = 1,  we see that to avoid redundant
  #       constraints,   span(X) should contain  span(Z).  Loosely, fixed-by-sampling-design
  #       parameters should be included.
  #
  #          Log-Linear models of the form log(p) = X beta are simple to fit using 'mph.fit'.
  #          For example,
  #
  #               mph.fit(y, link="logp",X=model.matrix(~A+B))
  #                   or, equivalently,
  #               mph.fit(y, link=function(p){log(p)},X=model.matrix(~A+B))
  #
  #
  #
  #
  # DESCRIPTION (continued):  MORE GENERAL MPH and HLP MODELS...
  #
  #   Instead of  (nu, p), the MP distribution can alternatively be parameterized in terms of
  #   the vector of expected table counts, m = E(Y).   Formally,  (nu, p) and m
  #   are in one-to-one correspondence and satisfy:
  #
  #            m = D(Z nu)p  and    nu= Z'm, p = D^{-1}(ZZ'm)m.
  #
  #   Here, Z = Z(s) is the cxK strata/population matrix determined by strata vector s.
  #   Specifically,  Z[i,k]  =  1(s[i] = identifier[k]).
  #
  #   The MPH (Special-Case) Model given above is a special case because it constrains the
  #   expected counts m  only through the table probabilities p.
  #   Similarly, the HLP (Special-Case) Model given above is a special case because
  #   it uses a link function that depends on m only through the table probabilities p.
  #
  #   More generally, 'mph.fit' computes maximum likelihood estimates and fit statistics for
  #   MPH and HLP models of the form...
  #
  #     MPH.     y <- Y ~ MP(nu, p | strata=s, fixed=F),     h(m) = 0.
  #     HLP:     y <- Y ~ MP(nu, p | strata=s, fixed=F),     L(m) = X beta.
  #
  #   Here,  h(.) is a smooth constraint function that must also be Z (i.e. strata s)
  #   homogeneous.  L(.) is a smooth link function that must also satisfy the
  #   HLP conditions with respect to Z (i.e. strata s) and X.
  #   That is,
  #      (1) L(.) has HLP link status wrt Z   AND
  #      (2) The implied constraint function h(m)=U'L(m) is Z homogeneous.
  #            Here,  null(U') = span(X).
  #
  #      L(.) has HLP link status wrt Z if, for m = D(Z nu)p,
  #
  #      (1)(a)  L(m) = a(nu) + L(p),  where a(nu1/nu2)-a(1) = a(nu1) - a(nu2),
  #                                  i.e. a(nu) has the form  C log nu + constant.
  #                OR
  #
  #         (b)  L(m) = G(nu)L(p),  where G(nu) is a diagonal matrix with diagonal
  #                                 elements that are powers of the nu elements,
  #                                 i.e. L(.) is Z homogeneous (see Lang 2004).
  #                OR
  #
  #         (c)  The components of L(.) are a mixture of types (a) and (b):
  #              L[j](m) = a[j](nu) + L[j](p)  or  L[j](m) = G[j](nu)L[j](p),
  #                       j=1,...,l.
  #
  #   Lang (2005) described HLP models that satisfied (1)(a) and (2), but
  #   the definition of HLP models can be broadened to include those models
  #   satisfying  (1) and (2).  That is, HLP models can be defined so
  #   they also include models that satisfy (1)(b) and (2) or (1)(c) and (2).
  #   Version 3.1+ of mph.fit  uses this broader definition of HLP Model.
  #
  #   Note:    The input variable 'h.mean' must be set to TRUE to fit this more general MPH
  #            model.  Similarly, the input variable 'L.mean' must be set to TRUE to fit
  #            this more general HLP model.  (An exception:  If the link function
  #            is specified using the keyword "logm" then 'L.mean' is automatically
  #            set to TRUE.)
  #
  #   Note:   'mph.fit', version 3.0 and above, carries out "necessary-condition" checks
  #            of Z homogeneity of h(.) and HLP link status of L(.) for these general models.
  #
  #   Log-Linear Models:   Log-Linear models of the form   log(m) = X beta   are  HLP models
  #            provided the span(X) contains the span(Z).  Loosely, provided  fixed-by-design
  #            parameters are included, the log-linear model is a special case HLP model.
  #
  #            Log-Linear models of the form log(m) = X beta are simple to fit using 'mph.fit'.
  #            For example,
  #
  #                 mph.fit(y, link="logm",X=model.matrix(~A+B))
  #                        or, equivalently,
  #                 mph.fit(y, link=function(m){log(m)}, L.mean=T, X=model.matrix(~A+B))
  #
  #   Note:    Most reasonable generalized loglinear models, which have the form
  #            L(m) = C log Mm  = X beta,     are also HLP models  (see Lang 2005).
  #
  # SUBROUTINES and COMPLEMENTARY FUNCTIONS:
  #
  #   Subroutines:     'mph.fit' uses 5 other short subroutines:
  #                          'num.deriv.fct', 'create.U', 'create.Z.ZF',
  #                          'check.homog', and 'check.HLP'.
  #
  #   Complementary Function:
  #                    'mph.summary'  summarizes and displays the  results from 'mph.fit'.
  #
  #
  # INPUT:
  #
  #   Required Input:
  #            y     =  vector (not matrix) of table counts
  #
  #   Optional Input:
  #            h.fct = function object that defines the constraint function h(.).
  #                    It must return a column vector.
  #                    h.fct can also be set to 0, in which case h(.) is viewed as the
  #                    0 function, so no constraints are imposed.
  #
  #                    By default,  h(.) is viewed as a function of the
  #                    table probabilities p.  If h.mean is set to h.mean=T,
  #                    then h(.) is viewed as a function of the expected counts m.
  #                    Default:  h.fct = NULL  (If both h.fct=NULL and  L.fct = NULL,
  #                    then h.fct is set to 0 and no constraints are imposed.
  #                    If h.fct = NULL and  L.fct is not= NULL then
  #                    h.fct will be computed as U'L.fct.)
  #
  #            constraint   is an alias for the argument 'h.fct'.
  #                    Argument 'constraint' is secondary to the primary argument 'h.fct'
  #                    in the following senses:
  #                         If 'constraint' and 'h.fct' are not equal,  'h.fct' is used.
  #
  #            h.mean = logical argument.  If h.mean=F (the default), h.fct is treated as
  #                     a function of p.  If h.mean=T then h.fct is treated  as a function of m.
  #
  #            L.fct = function object  that defines the link L(.) in the HLP model;
  #                    it must return a column vector.
  #
  #                    Or...  L.fct = keyword,  where
  #                    candidate keywords include "logp" and "logm".
  #
  #                    Entering L.fct="logp" tells the program to create the
  #                    function object as L.fct <- function(p){log(p)}.
  #                    L.fct="logm" tells the program to (i) create the
  #                    function object as L.fct <- function(m){log(m)} and
  #                    (ii) set L.mean = T.
  #
  #                    By default,  L.fct is treated as a function of the
  #                    table probabilities p (even if the argument in the
  #                    L.fct function object is "m").  If L.mean is set to L.mean=T,
  #                    then L.fct is treated as a function of the expected counts m.
  #                    Default:  L.fct = NULL  means no constraints of the form L(p)=X beta
  #                              are imposed.
  #
  #            link    is an alias for the argument 'L.fct'.
  #                    Argument 'link' is secondary to the primary argument 'L.fct'
  #                    in the following senses:
  #                         If 'link' and 'L.fct' are not equal,  'L.fct' is used.
  #
  #            L.mean = logical argument. If L.mean=F (the default), L.fct is treated
  #                    as a function of p.  If L.mean=T,  L.fct is treated
  #                    as a function of m.
  #
  #
  #            X     = HLP model design matrix, assumed to be full rank.
  #                    (default: X = NULL; i.e., it is left unspecified and unused.)
  #
  #            strata = vector of the same length as y that gives the stratum
  #                     membership identifier.  Default: strata = rep(1,length(y));
  #                     i.e. the default is the single stratum (non-stratified) setting.
  #                     Examples, strata=A or strata=c(1,1,1,2,2,2,3,3,3) or
  #                     strata=paste(sep="","A=",A,",B=",B)
  #
  #            fixed.strata = the object that gives information on which stratum have
  #                     fixed sample sizes.  It can equal one of the keywords,
  #                     fixed.strata = "all" or   fixed.strata = "none",  or it can
  #                     be a vector of stratum membership identifiers, e.g.
  #                     fixed.strata=c(1,3) or fixed.strata=c("pop1","pop5").
  #                     (default: fixed.strata="all")
  #
  #            check.homog.tol = round-off tolerance for Z homogeneity check
  #                              (default: check.homog.tol=1e-9)
  #            check.HLP.tol= round-off tolerance for HLP Link status check
  #                              (default: check.HLP.tol=1e-9)
  #
  #            print.iter = F,  Do not print out iteration information.  If print.iter=T
  #                         then iteration information is printed out.
  #
  #            maxiter = maximum number of iterations (default: maxiter=100)
  #            step  = step-size value  (default: step=1)
  #            change.step.after:  If the score value increases for more than
  #                    change.step.after iterations in a row, then the
  #                    initial step size is halved.  (default:
  #                    change.step.after = 0.25*maxiter)
  #            y.eps  = non-negative constant to be temporarily added
  #                     to the original counts in y  (default: y.eps=0)
  #            iter.orig = iteration at which the original counts will
  #                        be used (default: iter.orig=5).
  #            m.initial = initial estimate of m (default: m.initial=y)
  #                     See Input Note 7 below.
  #            norm.diff.conv  = convergence criteria value; see norm.diff
  #                              in the Output section
  #                              (default: norm.diff.conv=1e-6)
  #            norm.score.conv = convergence criteria value; see norm.score in
  #                              the Output section
  #                              (default: norm.score.conv = 1e-6)
  #            max.score.diff.iter,    The variable score.diff.iter
  #                    keeps track of how long norm.score is
  #                    smaller than norm.score.conv, but norm.diff is
  #                    greater than norm.diff.conv.  If this is the
  #                    case too long (i.e. score.diff.iter >= max.score.diff.iter),
  #                    then stop the iterations because the solution likely
  #                    includes at least one ML fitted value of 0.
  #                    (default:  max.score.diff.iter = 10)
  #            derht.fct = NULL, function object that computes analytic derivative of
  #                        the transpose of the constraint function h(.) with
  #                        respect to m.
  #                        If h(.) maps from Rp to Rq (i.e. there are q
  #                        constraints), then derht.fct returns a
  #                        pxq matrix of partial derivatives.
  #                        (default: derht.fct=NULL.  This means that the
  #                        derivative is calculated numerically.)
  #            derLt.fct = NULL, function object that computes analytic derivative of
  #                        the transpose of the link function L(.) with
  #                        respect to m.
  #                        If L(.) maps from Rp to Rq (i.e. there are q
  #                        link components), then derLt.fct returns a
  #                        pxq matrix of partial derivatives.
  #                        (default: derLt.fct =NULL, i.e. by default this
  #                        derivative is calculated numerically.)
  #
  #    Input Notes:
  #
  #                1. CONSTRAINT FUNCTION.
  #
  #                   'constraint' is an alias for 'h.fct'.
  #
  #                   If h.fct is a function object, it must return a column vector.
  #
  #                   By default, h.fct is treated as a function of the table
  #                   probabilities p.  To treat h.fct as a function of the expected
  #                   counts m, you must set h.mean=T (by default, h.mean=F).
  #
  #                   The fitting algorithm will fail if the constraints in h.fct=0
  #                   are redundant.  (The redundancy results in a rank-deficient
  #                   Jacobian, d h'/d m.)
  #
  #                2. MODEL WITH NO CONSTRAINTS.
  #
  #                   The model with no constraints can be specifed using
  #                   h.fct = 0.   The no-constraint model is the default
  #                   when neither h.fct nor L.fct are input (i.e. when
  #                   h.fct=NULL and L.fct=NULL).
  #
  #                3. HLP MODEL SPECIFICATION.
  #
  #                   'link'  is an alias for 'L.fct'.
  #
  #                   For HLP models, both L.fct and X must be specified.
  #
  #                   The design matrix X must be of full column rank.
  #
  #                   'mph.fit' recognizes two keywords for link specification,
  #                   'logp' and 'logm'.  These are convenient for log-linear modeling.
  #
  #                   If L.fct is a function object, it must return a column vector.
  #
  #                   By default, L.fct is treated as a function of the table
  #                   probabilities p.  To treat L.fct as a function of the expected
  #                   counts m, you must set L.mean=T (by default, L.mean=F).
  #
  #                   The constraint function h.fct is typically left
  #                   unspecified for HLP models, but it need not be.
  #
  #                      If h.fct is left unspecified, it is created within the program as
  #                      h.fct(m) <- function(m){ t(U)%*%L.fct(m)}, where matrix
  #                      U is an orthogonal complement of X.
  #
  #                      If h.fct is specified, the constraints implied by
  #                      L.fct(p) =X beta, or L.fct(m)=X beta,  are ignored.
  #
  #                      Note:  Although the HLP constraints are ignored when
  #                      h.fct is specified,  estimates of beta and the link are
  #                      computed under the model with constraints
  #                      h.fct(p)=0  or h.fct(m)=0.
  #
  #                   The fitting algorithm will fail to converge if the implied
  #                   constraints, U'L.fct = 0, include redundancies.
  #
  #                4. EXTENDED ML ESTIMATES.
  #                   When ML estimates are non-existent owing to
  #                   zero counts,  norm.diff will not converge to
  #                   zero, instead it tends to level off at some constant
  #                   positive value.  This is because at least one ML
  #                   fitted value is 0, which on the log scale is
  #                   log(0)=-Inf, and the log-scale iterates slowly
  #                   move toward -Inf.  One solution to this problem is
  #                   to set the convergence value norm.diff.conv
  #                   to some large number so only the score convergence
  #                   criterion is used.  In this case, the algorithm
  #                   often converges to a solution that can be viewed
  #                   as an extended ML estimate, for which 0 estimates
  #                   are allowed.   Versions 2.0 and higher  automates the
  #                   detection of such problems.  See the description of the
  #                   input variable  'max.score.diff.iter' above and the
  #                   MISC COMPUTATIONAL NOTES below.
  #
  #                5. CONVERGENCE PROBLEMS / FINE TUNING ITERATIONS.
  #
  #                   First check to make sure that the model is correctly
  #                   specified and redundant constraints are avoided.
  #
  #                   When ML estimates exist, but there are
  #                   non-convergence problems (perhaps caused by zero
  #                   counts), a modification to the tuning
  #                   parameters `step', 'change.step.after', `y.eps',
  #                   and/or `iter.orig' will often lead to convergence.
  #
  #                   With zero counts, it might help to set
  #                   y.eps = 0.1 (or some other positive number)
  #                   and iter.orig=5 (the default). This tells
  #                   the program to initially use y+y.eps rather than
  #                   the original counts y.  At iteration 5=iter.orig,
  #                   after the algorithm has had time to move toward a
  #                   non-boundary solution, the original counts are again
  #                   used.
  #
  #                   To further mitigate non-convergence problems,
  #                   the parameter `step' can be set to a smaller value
  #                   (default: step=1) so the iterates do not change as much.
  #
  #                6. The initial estimate of m is actually
  #                      m.initial + y.eps + 0.01*((m.initial+y.eps)==0)
  #                   The program defaults are m.initial=y and y.eps=0.
  #                   Note: If m.initial > 0 and y.eps=0, then the
  #                         initial estimate of m is simply m.initial.
  #
  #
  # OUTPUT
  #            y         = vector of counts used in the algorithm
  #                        for ML estimation.  Usually, this vector
  #                        is identical to the observed table counts
  #            m         = vector of ML fitted values
  #            covm      = approximate covariance matrix of fitted values
  #            p         = vector of cell probability ML estimates
  #            covp      = approximate covariance matrix of cell probability estimators
  #            lambda    = vector of Lagrange multiplier ML estimates
  #            covlambda = approximate covariance matrix of multiplier estimators
  #            resid     = vector of raw residuals (i.e. observed minus fitted counts)
  #            presid    = vector of Pearson residuals
  #            adjresid  = vector of adjusted residuals
  #            covresid  = approximate covariance matrix of raw residuals
  #            Gsq       = likelihood ratio stat for testing Ho: h(m)=0 vs. Ha:not Ho
  #            Xsq       = Pearson's score stat (same as Lagrange multiplier stat)
  #                        for testing Ho: h(m)=0 vs. Ha: not H0.
  #            Wsq       = Generalized Wald stat for testing Ho: h(m)=0 vs.
  #                        Ha: not H0.
  #            df        = degrees of freedom associated with Gsq and Xsq (df= dim(h))
  #            beta      = vector of HLP model  parameter ML estimates
  #            covbeta   = approximate covariance matrix of HLP
  #                        model parameter estimators
  #            L         = vector of HLP model link ML estimates
  #            Lobs      = vector of HLP model observed link values, L(y).
  #            covL      = approximate covariance matrix of HLP model
  #                        link estimators
  #            Lresid    = vector of adjusted link residuals of the form
  #                        (L(obs) ? L(fitted))/ase(L(obs)-L(fitted)).
  #            covLres   = var matrix of L(obs) - L(fitted)
  #            iter      = number of update iterations performed
  #            norm.diff = distance between last and second last `theta' iterates
  #                        (`theta' is the vector of log fitted values and
  #                        Lagrange multipliers)
  #            norm.score= distance between the score vector and zero
  #            h.fct     = constraint function used in algorithm
  #            h.input.fct = constraint function as originally input
  #            h.mean    = logical variable (h.mean=F, h.fct treated as function of p,
  #                                          h.mean=T, h.fct treated as function of m)
  #            derht.fct = analytic function used in algorithm that computes
  #                        derivative of transpose of constraint function h
  #            L.fct     = link function used in algorithm
  #            L.input.fct = link function as originally input
  #            L.mean    = logical variable (L.mean=F, L.fct treated as function of p,
  #                                          L.mean=T, L.fct treated as function of m)
  #            derLt.fct = analytic function used in algorithm that computes
  #                        derivative of transpose of link function L
  #            X         = HLP model design matrix used in algorithm
  #            U         = orthogonal complement of design matrix X
  #            triple    = a list object containing the sampling plan triple (Z,ZF,n),
  #                        where Z is the population (or strata) matrix,
  #                        ZF is the sampling constraint matrix,
  #                        and n is the collection of fixed sample sizes.
  #            strata    = strata variable used as input
  #            fixed.strata = strata corresponding to fixed sample sizes
  #            version   = `mph.fit' version number
  #            warn.message = message stating whether or not the original
  #                           counts were used.
  #            warn.message.score = message stating whether or not the norm score
  #                        convergence criterion was met.
  #            warn.message.diff  = message stating whether or not the norm diff
  #                        convergence criterion was met.
  #
  #    Output Notes
  #                 1.  ITERATION HISTORY.
  #                     An iteration history is printed out when 'print.iter' is set
  #                     equal to TRUE.  A single line of the history looks like the following:
  #                     "iter= 18[0] norm.diff= 3.574936e-08 norm.score= 1.901705e-15"
  #                     Here, iter is the number of update iterations performed.
  #                     The number in [] gives the number of step size searches
  #                     required within each iteration.
  #                     norm.diff and norm.score are defined above.
  #                     Finally, the time elapsed is output.
  #                     Note:  For the model with no restrictions (h.fct = 0)
  #                     there are no step size changes.
  #
  #                 2.  STORING and VIEWING RESULTS.
  #                     To store the results of mph.fit, issue a command
  #                     like the following example
  #                        >  results <-  mph.fit(y, h.fct=h.fct)
  #                     Use program "mph.summary"  to view the results
  #                     of mph.fit.   Specifically, if the results of mph.fit
  #                     are saved in object "results",  submit the command
  #                     mph.summary(results)  [or mph.summary(results,T) or
  #                     mph.summary(results,T,T) depending on how much of
  #                     the output you need to see.]
  #
  #                 3.  The output objects beta, covbeta, L, covL, and
  #                     Lresid will be set to "NA" unless an HLP model is
  #                     specified (i.e. L.fct and X are input)
  #
  # MISC COMPUTATIONAL NOTES:
  #
  #        For computational efficiency (in particular, to speed up
  #        multiplications involving diagonal matrices), the expression
  #        (A*c(x)) is used in place of diag(c(x))%*%A
  #
  #        Version 1.2 Major Updates:
  #             1. "<-" replaces "_" to reflect changes in R, version 1.8.1
  #             2. Input variable  "chscore.criterion" added.  This variable
  #                is used to restrict the updating move so that the
  #                norm of the score changes by no more than a specified
  #                amount.  This restriction is used to fine tune the
  #                iterative algorithm so that convergence is more likely.
  #        Version 1.3 Update:
  #             1. Allows user to input initial estimates of m.  See input
  #                variate m.initial.
  #             2. 1.3.1.  Fixed bug regarding warning message about original
  #                        counts NOT being used...
  #                   if ((iter <  iter.orig) & (y.eps != 0)) changed to
  #                   if ((iter <= iter.orig) & (y.eps != 0))
  #        Version 2.0 Update:
  #             1. Z = matrix(1,length(y),1) is now the default; i.e. the
  #                         "single strata setting" is the default.
  #             2. Iterations are fine-tuned a bit so the algorithm is
  #                    more robust, especially in sparse count settings.
  #                    The 'chscore.criterion' has been replaced.
  #             3. h.fct, L.fct, and X now have NULL as the default value.
  #                    is.null checks are now made in the code.
  #             4. Added max.score.diff.iter criterion to better handle ML
  #                    fitted values of 0.  score.diff.iter
  #                    keeps track of how long norm.score is
  #                    smaller than norm.score.conv, but norm.diff is
  #                    greater than norm.diff.conv.  If this is the
  #                    case too long (i.e. score.diff.iter >= max.score.diff.iter),
  #                    then stop the iterations because the solution likely
  #                    includes at least one ML fitted value of 0.
  #             5. Added warn.message.score and warn.message.diff to the
  #                    output.  These messages state whether or not the
  #                    norm score and norm diff convergence criteria were met.
  #             6. Added change.step.after option.   Default:
  #                    change.step.after=0.25*maxiter.
  #                    If score value increases for more than change.step.after
  #                    iterations in a row, the initial step size is halved.
  #
  #       Version 3.0:  Substantial changes were made in version 3.0.
  #
  #             1.  The sampling plan matrices Z and ZF are no longer entered
  #                 directly.    With version 3.0, the user specifies
  #                 a strata variable that gives the stratum membership identifiers
  #                 associated with each count in y.  The user can specify the stratum
  #                 membership identifiers that correspond to fixed sample
  #                 sizes using the 'fixed.strata' input variable.
  #
  #             2.  Specification of model constraints is simplified.
  #                 By default constraint function h(.) is viewed as a function
  #                 of table probabilities p, rather than expected counts m.
  #                 Similarly, by default, the link function L(.) is viewed as a function
  #                 of table probabilities rather than expected counts.
  #                 In this case,  Z homogeneity is guaranteed.  That is, h(p)= 0
  #                 and L(p) = X beta  are both MPH models.
  #
  #             3.  The user can override the default of the previous item.
  #                 That is,  constraints [links] can be specified in terms of
  #                 expected counts, rather than table probabilities.  The override
  #                 is accomplished by changing the default h.mean=F [L.mean=F] to h.mean=T
  #                 [L.mean=T].
  #
  #                 When h.mean=T...Whereas,  h(p) is Z homogeneous, there is no
  #                 guarantee that h(m) is Z homogeneous.  Version 3.0 now runs
  #                 a necessary-condition check to make sure the constraint function
  #                 h(m) is Z homogeneous.  If it is not, a message to that effect
  #                 is returned  and the model is not fitted.
  #
  #                 When L.mean=T...Whereas, L(p) satisfies the conditions for
  #                 being an HLP link,  there is no guarantee that L(m) does.
  #                 Version 3.0 now runs a necessary-condition check to make
  #                 sure L(m) is an HLP link.   If it is not, a message to that effect
  #                 is returned  and the model is not fitted.
  #
  #             4.  The output from mph.fit has been expanded to include
  #                 more information about the sampling plan.  When available,
  #                 dimension labels are used.   Correspondingly,  mph.summary
  #                 has been modified.
  #
  #             5.  Aliases for h.fct and L.fct are available.  The user can
  #                 use 'constraint' instead of 'h.fct'  and 'link' instead of 'L.fct'.
  #
  #             6.  Keywords for log-linear modeling are available:  Rather
  #                 than creating a function object,  the user can enter  L.fct="logp"
  #                 or L.fct="logm"
  #
  #             7.  By default (print.iter=F), iteration information is not printed.
  #
  #             8.  The iteration process is initiated using the counts y+y.eps, rather
  #                 than y.   In version 3.0, when y.eps != 0,  convergence will not
  #                 occur until after the counts are reset to their original values.
  #                 This avoids the problem of convergence based on the adjusted
  #                 counts y+y.eps.  (If the user wants convergence based on
  #                 adjusted counts, they will need to enter the adjusted counts
  #                 directly into the y input variable.)
  #                 On a related note,  if iter.orig >= maxiter,  it is reset in the
  #                 program as:  iter.orig <-  maxiter - 1.
  #
  #       Version 3.1:
  #
  #             1.  The definition of HLP model has been broadened to include
  #                 models with links L(.) that are Z homogeneous of any order.
  #                 See the relevant discussion above in the section entitled,
  #                 "DESCRIPTION (cont'd):  MORE GENERAL MPH and HLP MODELS..."
  #                 The HLP link status check has been modified accordingly.
  #
  #       version 3.2:
  #
  #             1.  The computation of the Generalized Wald GOF statistic has
  #                 been changed. It now correctly accommodates different sampling plans.
  #                 And it now uses the original counts as the unrestricted MLe's
  #                 (with 0.01 added to 0 counts).  In older versions, the Wald GOF
  #                 statistic used initial estimates of restricted MLe's,  which
  #                 could be very different from the unrestricted MLe's, e.g. when
  #                 y.eps and/or m.initial values were used to initiate the algorithm.
  #
  #             2.  covLres = cov(L(obs) - L(fitted))  is now output.
  #
  #
  #
  start.time <- proc.time()[3]
  inv <- solve

  y.orig <- as.matrix(c(t(y)))
  y <- y.orig + y.eps
  if (y.eps != 0) {is.original.y <- FALSE} else {is.original.y <- TRUE}

  if (iter.orig >= maxiter) {iter.orig <- maxiter - 1}

  ##############  Create Sampling Plan Matrices  Z and ZF ###########################

  Z.ZF <- create.Z.ZF(strata=strata,nrowZ=length(y),fixed.strata=fixed.strata)
  Z <- Z.ZF$Z
  ZF <- Z.ZF$ZF


  ###############################################################


  ################  Create h.fct   and L.fct   functions ####################
  #
  #  If the user enters h.fct [similarly, L.fct] as a function of the table
  #  probabilities p, which is the case by default (see  h.mean=F [L.mean=F]), rather than
  #  the expected counts m (see h.mean=T [L.mean=T]), it is re-expressed as a function of m.
  #
  #  If h.fct=NULL and the user enters L.fct and X then the constraint
  #  function h.fct is created and defined as h.fct = U'L.fct.
  #
  #  Note:  If the user enters h.fct AND also enters L.fct and X,  the constraints
  #  implied by "L=X beta" are ignored.  In this case, beta and L are estimated
  #  under the model that uses the constraint function h.fct only.
  #

  h.input.fct <- h.fct
  L.input.fct <- L.fct


  if (!is.null(L.input.fct)) {
    if (is.null(X)) {return("Design matrix X must be specified when L function is specified!")}
    if (!is.function(L.input.fct)) {
      # if (L.input.fct == "logp") {L.input.fct <- function(p){log(p)}}
      # else {L.input.fct <- function(m) {log(m)}; L.mean <- TRUE}
      if (L.input.fct == "logp") {L.input.fct <- function(x){log(x)}}
      else {L.input.fct <- function(x) {log(x)}; L.mean <- TRUE}
    }
    if (!(L.mean)) {
      L.fct <- function(m) {
        p <- m*c(1/Z%*%t(Z)%*%m)
        as.matrix(L.input.fct(p))
      }
    }
    if (L.mean) {
      L.fct <- L.input.fct
      if (verbose) {
        cat("CHECKING whether L(.) is an HLP link...")
      }
      chk1 <- check.HLP(L.fct,Z,check.HLP.tol)
      chk2 <- check.homog(L.fct,Z,check.HLP.tol)
      if ((chk1!= "")&(chk2!="")) {
        warning(paste("\n","Link L(m) did not satisfy the sufficient conditions\n for HLP link status [based on tol=",check.HLP.tol,"].\n",
            "The model may or may not be an HLP model.\n\n  Proceed with CAUTION!\n\n"))
        #return(msg)
      }
      else {
        if (verbose) {
          cat(sep="","Necessary condition [tol=",check.HLP.tol,"] passed.\n")
        }
      }
    }
  }


  U <- "Not created within the program."

  if (!is.null(h.input.fct)) {
    if (!is.function(h.input.fct)) {h.fct <- 0}
    if (is.function(h.input.fct)) {
      if (!(h.mean)) {
        h.fct <- function(m) {
          p <- m*c(1/Z%*%t(Z)%*%m)
          as.matrix(h.input.fct(p))
        }
      }
      if (h.mean) {
        h.fct <- h.input.fct
        if (verbose) {
          cat("CHECKING whether h(.) is Z homogeneous...")
        }
        chk <- check.homog(h.fct,Z,check.homog.tol)
        if (chk!= "") {
          msg <- paste("h(m) is not Z homogeneous [based on tol=",check.homog.tol,"]!")
          stop(msg)
        }
        else {
          if (verbose) {
            cat(sep="","Necessary condition [tol=",check.homog.tol,"] passed.\n")
          }
        }
      }
    }
  }
  else if (!is.null(L.input.fct))
  {
    U <- create.U(X)
    if (sum(abs(U)) == 0) {h.fct <- 0}
    else {
      h.fct <- function(m) {
        t(U)%*%L.fct(m)
      }
      if (L.mean) {
        h.mean <- TRUE
        if (verbose) {
          cat("CHECKING whether h(.) is Z homogeneous...")
        }
        chk <- check.homog(h.fct,Z,check.homog.tol)
        if (chk!= "") {
          msg <- paste("h(m) is not Z homogeneous [based on tol=",check.homog.tol,"]!");
          stop(msg)
        }
        else {
          if (verbose) {
            cat(sep="","Necessary condition [tol=",check.homog.tol,"] passed.\n")
          }
        }
      }
    }
  }
  else {h.fct <- 0}



  ##########BEGIN FITTING WHEN THERE ARE CONSTRAINTS (i.e. is.function(h.fct)==T) #####################

  if (is.function(h.fct)) {

    lenm <- length(y);

    m.obs <- y.orig; m.obs[m.obs==0] <- 0.01;       #ver. 3.2+,  used for Wsq
    if (!is.function(derht.fct)) {
      H.obs <- num.deriv.fct(h.fct,m.obs)
    }
    else {
      H.obs <- derht.fct(m.obs)
    }                                               #ver. 3.2+,  used for Wsq
    covm.unadj.obs <- covm.obs <-  diag(c(m.obs))   #ver. 3.2+,  used for Wsq

    #Initialize iterates...
    m <- as.matrix(c(m.initial)) + y.eps
    m[m==0] <- 0.01
    xi <- log(m)
    h <- hobs <- h.fct(m)
    lenh <- length(h)
    lam <- matrix(0,lenh,1)
    Dm <- diag(c(m))
    Dminv <- diag(c(1/m))
    if (!is.function(derht.fct)) {
      H <- num.deriv.fct(h.fct,m)
    }
    else {
      H <- derht.fct(m)
    }
    HtDHinv <- inv(t(H)%*%(H*c(m))) #HtDHinv <- inv(t(H)%*%Dm%*%H)
    HHtDHinv <- H%*%HtDHinv
    s <- rbind(y-m+(H*c(m))%*%lam,h)              #s <- rbind(y-m+Dm%*%H%*%lam,h)
    norm.score <- sqrt(sum(s*s))
    theta <- rbind(xi,lam)
    lentheta <- length(theta)
    iter <- 0
    score.diff.iter <- 0
    norm.diff <-  10
    step.start <- step
    number.increase <- 0
    if (verbose) {
      cat("\n")
      cat("mph.fit running...\n")
    }


    #######################  BEGIN ITERATING ###################################

    while ( ((norm.diff > norm.diff.conv)||(norm.score > norm.score.conv)||(is.original.y == FALSE) )
            &(iter< maxiter)&(score.diff.iter < max.score.diff.iter) )
    {
      if (iter == iter.orig) {y <- y.orig; is.original.y <- TRUE}
      A <- cbind(Dminv - HHtDHinv%*%t(H),-HHtDHinv)
      A <- rbind(A,cbind(  -t(HHtDHinv),-HtDHinv))


      ####################### FINE-TUNE UPDATING STEP #######################
      step.temp <- step.start
      step.iter <- 0
      step.out <- FALSE

      ##############  BEGIN while ###################################
      while ((step.iter < 5)&(step.out==FALSE)) {

        theta.temp <- theta + step.temp*A%*%s
        norm.diff  <- sqrt(sum((theta-theta.temp)*(theta-theta.temp)))
        m  <- as.matrix(exp(theta.temp[1:lenm]))
        lam  <- as.matrix(theta.temp[(lenm+1):lentheta])
        h  <- h.fct(m)
        Dm  <- diag(c(m))
        Dminv <- diag(c(1/m))
        if (!is.function(derht.fct)) {
          H <- num.deriv.fct(h.fct,m)
        }
        else {
          H <- derht.fct(m)
        }
        HtDHinv <- inv(t(H)%*%(H*c(m)))     #HtDHinv <- inv(t(H)%*%Dm%*%H)
        HHtDHinv <- H%*%HtDHinv
        s.temp <- rbind(y-m+(H*c(m))%*%lam,h)    #s <- rbind(y-m+Dm%*%H%*%lam,h)
        norm.score.temp <- sqrt(sum(s.temp*s.temp))


        if (is.nan(norm.score.temp)) {
          step.iter <- step.iter+1
          step.temp <- max(step.temp/2,0.01)
        }
        else {
          if (norm.score.temp >  norm.score) {
            number.increase <- number.increase + 1
            if (norm.score.temp > max(2*norm.score,norm.score.conv))   {
              step.iter <- step.iter+1
              step.temp <- max(step.temp/2,0.01)
            }
            else {step.out <- TRUE}
          }
          else {step.out <- TRUE}
        }
      }
      #################  END while ((step.iter < 5)&(step.out==F)) ####


      if (number.increase > change.step.after) {
        step.start <- max(step.start/2,0.01); number.increase <- 0
        if (verbose) {
          cat("\nInitial step value has been halved to", step.start,".\n");
        }
      }

      ####################  END FINE-TUNE UPDATING STEP #######################

      theta <- theta.temp
      s <- s.temp
      norm.score <- norm.score.temp
      iter <- iter + 1
      if (verbose) {
        cat("  iter=",iter, "[",step.iter,"]",
            " norm.diff=",norm.diff," norm.score=", norm.score,"\n")
      }
      if ((norm.score < norm.score.conv)&(norm.diff > norm.diff.conv)) {
        score.diff.iter <- score.diff.iter + 1
      }
    }

    ########################  END ITERATING ##################################

    p <- m*c(1/Z%*%t(Z)%*%y)        #Ninv <- diag(c(1/Z%*%t(Z)%*%y)); p <- Ninv%*%m
    resid <- y-m
    covresid <- (H*c(m))%*%HtDHinv%*%t(H*c(m))
    #covresid <- Dm%*%H%*%HtDHinv%*%t(H)%*%Dm
    covm.unadj <- covm <- Dm -  covresid
    if (sum(ZF) != 0) {
      covm <- covm.unadj -  ((ZF*c(m))%*%t(ZF*c(m)))*c(1/Z%*%t(Z)%*%y)
      #covm <- covm.unadj - Ninv%*%Dm%*%ZF%*%t(ZF)%*%Dm
      covm.obs <- covm.unadj.obs - ((ZF*c(m.obs))%*%t(ZF*c(m.obs)))*c(1/Z%*%t(Z)%*%y) #ver 3.2+
    }
    covp <- t(t((covm.unadj-((Z*c(m))%*%t(Z*c(m)))*c(1/Z%*%t(Z)%*%y))*
                  c(1/Z%*%t(Z)%*%y))* c(1/Z%*%t(Z)%*%y))
    #covp <- Ninv%*%(covm.unadj-Ninv%*%Dm%*%Z%*%t(Z)%*%Dm)%*%Ninv

    covlam <- HtDHinv

    ####################### COMPUTE ADJUSTED AND PEARSON RESIDUALS ##############

    dcovresid <- diag(covresid)
    dcovresid[abs(dcovresid)<1e-8] <- 0
    adjresid <- resid
    adjresid[dcovresid > 0] <- resid[dcovresid>0]/sqrt(dcovresid[dcovresid>0])
    presid <- resid/sqrt(m)

    ##################  END COMPUTE ADJUSTED AND PEARSON RESIDUALS ###############

    #######################  COMPUTE FIT STATISTICS  #############################

    Gsq <- as.matrix(2*sum(y[y>0]*(log(y[y>0])-log(m[y>0]))))
    Xsq <- as.matrix(t(y-m)%*%((y-m)*c(1/m)))
    # Wsq <- as.matrix(t(hobs)%*%HtDHinvobs%*%hobs)
    Wsq <- as.matrix(t(hobs)%*%solve(t(H.obs)%*%covm.obs%*%H.obs)%*%hobs)   #ver 3.2+
    ## compute power divergence statistic, when lambda = pdlambda
    if ((pdlambda != 0) & (pdlambda != -1)) {
      PD.stat <- sum(y*((y/m)^pdlambda-1))*2/pdlambda/(pdlambda+1)
    }
    else {
      if (pdlambda == 0) {
        PD.stat <- Gsq
      }
      if (pdlambda == -1) {
        ymod <- y; ymod[ymod==0] <- 1e-3
        PD.stat <- 2*sum(m*log(m/ymod))
      }
    }
    PD.stat <- as.matrix(PD.stat)

    #######################  END COMPUTE FIT STATISTICS  #########################


    beta <- "NA"
    covbeta <-  "NA"
    covL <-  "NA"
    L <- "NA"
    Lobs <- "NA"
    Lresid <- "NA"
    covLres <- "NA"

    ############ COMPUTE LINEAR PREDICTOR ESTIMATES if (!is.null(L.fct)) #########

    if (!is.null(L.fct) ) {
      L <- as.matrix(L.fct(m))
      Lobs <- as.matrix(L.fct(y))
      if (!is.function(derLt.fct)) {
        derLt <- num.deriv.fct(L.fct,m)
      }
      else {
        derLt <- derLt.fct(m)
      }
      covL <- t(derLt)%*%covm%*%derLt

      if (sum(abs(X)) != 0) {
        PX <- inv(t(X)%*%X)%*%t(X)
        beta <- PX%*%L
        covbeta <- PX%*%covL%*%t(PX)
      }
      else {beta <- 0; covbeta <- 0}

      ##############COMPUTE LINK RESIDUALS#####################
      Lres <- Lobs - L
      covLres <- t(derLt)%*%covresid%*%derLt
      dcovLres <-   diag(covLres)
      dcovLres[abs(dcovLres)<1e-8] <- 0
      Lresid <- Lres
      Lresid[dcovLres > 0] <- Lres[dcovLres>0]/sqrt(dcovLres[dcovLres>0])
      ###########################################################

      if (is.null(dimnames(X)[[2]])) {
        lbeta <-  c()
        for (i in 1:length(beta)) {
          lbeta <- c(lbeta,paste("beta",i,sep=""))
        }
      }
      else {lbeta <- dimnames(X)[[2]]}
      if (is.null(dimnames(L)[[1]])) {
        ll <- c()
        for (i in 1:length(L)) {
          ll <- c(ll,paste("link",i,sep=""))
        }
      }
      else {ll <- dimnames(L)[[1]]}
      dimnames(beta) <- list(lbeta,"BETA")
      dimnames(covbeta) <- list(lbeta,lbeta)
      dimnames(L) <- list(ll,"ML LINK")
      dimnames(Lobs) <- list(ll,"OBS LINK")
      dimnames(covL) <- list(ll,ll)
      dimnames(Lresid) <- list(ll,"LINK RESID")
      dimnames(covLres) <- list(ll,ll)
    }

    #####  END COMPUTE LINEAR PREDICTOR ESTIMATES if (!is.null(L.fct)) ###########
  }

  ##########END FITTING WHEN THERE ARE CONSTRAINTS (i.e. is.function(h.fct)==T) ###########################

  ##########BEGIN FITTING WHEN THERE ARE NO CONSTRAINTS (i.e. is.function(h.fct)==F) ######################

  else {                #i.e.,  is.function(h.fct)==F
    lenh <- 0
    lenm <- length(y)
    m <- as.matrix(c(m.initial))+y.eps
    m[m==0] <- 0.01
    xi <- log(m)
    Dm <- diag(c(m))
    Dminv <- diag(c(1/m))
    s <- y-m
    norm.score <- sqrt(sum(s*s))
    theta <- xi
    lentheta <- length(theta)
    iter <- 0
    score.diff.iter <- 0
    norm.diff <- 10
    if (verbose) {
      cat("\n", "mph.fit running...\n")
    }

    #######################  BEGIN ITERATING ###################################

    while ( ((norm.diff > norm.diff.conv)||(norm.score > norm.score.conv)||(is.original.y==FALSE) )
            &(iter< maxiter)&(score.diff.iter < max.score.diff.iter))
    {
      if (iter == iter.orig) {y <- y.orig; is.original.y <- TRUE}
      A <- Dminv
      thetanew <- theta + step*(s*c(1/m))   #thetanew <- theta + step*A%*%s
      norm.diff <- sqrt(sum((theta-thetanew)*(theta-thetanew)))
      theta <- thetanew
      m <- exp(theta)
      Dm <- diag(c(m))
      Dminv <- diag(c(1/m))
      s <- y-m
      norm.score <- sqrt(sum(s*s))
      iter <- iter + 1
      if (verbose) {
        cat("  iter=",iter, " norm.diff=",norm.diff," norm.score=", norm.score,"\n")
      }
      if ((norm.score < norm.score.conv)&(norm.diff > norm.diff.conv)) {
        score.diff.iter <- score.diff.iter + 1
      }
    }

    #######################  END ITERATING ###################################

    p <- m*c(1/Z%*%t(Z)%*%y)         #Ninv <- diag(c(1/Z%*%t(Z)%*%y)); p <- Ninv%*%m
    resid <- 0*y
    covm.unadj <- covm <- Dm
    covresid <- 0*covm
    if (sum(ZF) != 0) {
      covm <- covm.unadj -  ((ZF*c(m))%*%t(ZF*c(m)))*c(1/Z%*%t(Z)%*%y)
      #covm <- covm.unadj - Ninv%*%Dm%*%ZF%*%t(ZF)%*%Dm
    }
    covp <- t(t((covm.unadj-((Z*c(m))%*%t(Z*c(m)))*c(1/Z%*%t(Z)%*%y))*
                  c(1/Z%*%t(Z)%*%y))* c(1/Z%*%t(Z)%*%y))
    #covp <- Ninv%*%(covm.unadj-((Z*c(m))%*%t(Z*c(m)))*c(1/Z%*%t(Z)%*%y))%*%Ninv
    #covp <- Ninv%*%(covm.unadj-Ninv%*%Dm%*%Z%*%t(Z)%*%Dm)%*%Ninv
    covlam <- as.matrix(0);lam <- as.matrix(0)

    ##################  COMPUTE ADJUSTED and PEARSON RESIDUALS #################
    adjresid <-  0*y
    presid <- 0*y
    ##################  END COMPUTE ADJUSTED and PEARSON RESIDUALS #############

    ##################  COMPUTE FIT STATISTICS #################################

    Gsq <- as.matrix(2*sum(y[y>0]*(log(y[y>0])-log(m[y>0]))))
    Xsq <- as.matrix(t(y-m)%*%((y-m)*c(1/m)))
    Wsq <- as.matrix(0)
    ## compute power divergence statistic, when lambda = pdlambda
    if ((pdlambda != 0) & (pdlambda != -1)) {
      PD.stat <- sum(y*((y/m)^pdlambda-1))*2/pdlambda/(pdlambda+1)
    }
    else {
      if (pdlambda == 0) {
        PD.stat <- Gsq
      }
      if (pdlambda == -1) {
        ymod <- y; ymod[ymod==0] <- 1e-3
        PD.stat <- 2*sum(m*log(m/ymod))
      }
    }
    PD.stat <- as.matrix(PD.stat)

    ##################  END COMPUTE FIT STATISTICS #############################


    beta <- "NA"
    covbeta <-  "NA"
    covL <-   "NA"
    L <-  "NA"
    Lresid <- "NA"
    Lobs <- "NA"
    covLres <- "NA"

    ####### COMPUTE LINEAR PREDICTOR ESTIMATES if (!is.null(L.fct)) ############
    if (!is.null(L.fct)) {
      L <- as.matrix(L.fct(m))
      Lobs <- as.matrix(L.fct(y))
      if (!is.function(derLt.fct))  {
        derLt <- num.deriv.fct(L.fct,m)
      }
      else {
        derLt <- derLt.fct(m)
      }
      covL <- t(derLt)%*%covm%*%derLt
      covLres <- 0*covL   #var(Lobs - Lfit) = 0 in this no-constraints case, ver 3.2.

      if (sum(abs(X)) != 0) {
        PX <- inv(t(X)%*%X)%*%t(X)
        beta <- PX%*%L
        covbeta <- PX%*%covL%*%t(PX)
      }
      else {beta <- covbeta <- 0}

      Lresid <- 0*L

      if (is.null(dimnames(X)[[2]])) {
        lbeta <-  c()
        for (i in 1:length(beta)) {
          lbeta <- c(lbeta,paste("beta",i,sep=""))
        }
      }
      else {lbeta <- dimnames(X)[[2]]}
      if (is.null(dimnames(L)[[1]])) {
        ll <- c()
        for (i in 1:length(L)) {
          ll <- c(ll,paste("link",i,sep=""))
        }
      }
      else {ll <- dimnames(L)[[1]]}

      dimnames(beta) <- list(lbeta,"BETA")
      dimnames(covbeta) <- list(lbeta,lbeta)
      dimnames(Lobs) <- list(ll,"OBS LINK")
      dimnames(L) <- list(ll,"ML LINK")
      dimnames(covL) <- list(ll,ll)
      dimnames(Lresid) <- list(ll,"LINK RESID")
      dimnames(covLres) <- list(ll,ll)
    }

    ######## END COMPUTE LINEAR PREDICTOR ESTIMATES if (!is.null(L.fct)) #########

  }            # end is.function(h.fct)==F

  ########END FITTING WHEN THERE ARE NO CONSTRAINTS (i.e. is.function(h.fct)==F) ########################




  ########################### ASSIGN LABELS...################################

  lm <- ly <- lp <- lbeta <- lr <- lar <- lpr <- ll <- llam <- c()
  for (i in 1:lenm) {
    lm <- c(lm,paste("m",i,sep=""))
    ly <- c(ly,paste("y",i,sep=""))
    lp <- c(lp,paste("p",i,sep=""))
    lr <- c(lr,paste("r",i,sep=""))
    lar <- c(lar,paste("adj.r",i,sep=""))
    lpr <- c(lpr,paste("pearson.r",i,sep=""))
  }

  for (i in 1:length(lam)) {
    llam <- c(llam,paste("lambda",i,sep=""))
  }
  dimnames(y) <- list(ly,"OBS")
  dimnames(m) <- list(lm,"FV")
  dimnames(p) <- list(lp,"PROB")
  dimnames(resid) <- list(lr,"RAW RESIDS")
  dimnames(presid) <- list(lpr,"PEARSON RESIDS")
  dimnames(adjresid) <- list(lar, "ADJUSTED RESIDS")
  dimnames(lam) <- list(llam,"LAGRANGE MULT")
  dimnames(covm) <- list(lm,lm)
  dimnames(covp) <- list(lp,lp)
  dimnames(covresid) <- list(lr,lr)
  dimnames(covlam) <- list(llam,llam)
  dimnames(Xsq) <- list("","PEARSON SCORE STATISTIC")
  dimnames(Gsq) <- list("","LIKELIHOOD RATIO STATISTIC")
  dimnames(Wsq) <- list("","GENERALIZED WALD STATISTIC")
  dimnames(PD.stat) <- list("", "POWER DIVERGENCE STATISTIC")
  #######################  end ASSIGN LABELS  ###################################

  #cat("\n")
  if (norm.score > norm.score.conv) {
    warn.message.score <- paste(sep="", "Did NOT meet norm score convergence criterion [", norm.score.conv,"]!\n")
    if (verbose) {
      cat("  ",warn.message.score)
    }
  }
  else {
    warn.message.score <- paste(sep="", "Norm score convergence criterion [", norm.score.conv,"] was met.\n")
  }

  if (norm.diff > norm.diff.conv) {
    warn.message.diff <- paste(sep="", "Did NOT meet norm diff convergence criterion [", norm.diff.conv,"]!\n")
    if (verbose) {
      cat("  ",warn.message.diff)
    }
    if (norm.score <= norm.score.conv) {
      if (verbose) {
        cat("  ",warn.message.score)
      }
    }
  }
  else {
    warn.message.diff <- paste(sep="", "Norm diff convergence criterion [", norm.diff.conv,"] was met.\n")
  }

  if ((norm.score <= norm.score.conv)&(norm.diff <= norm.diff.conv)&(is.original.y==TRUE)) {
    if (verbose) {
      cat("   Convergence criteria have been met.\n")
    }
  }

  if (is.original.y == FALSE) {
    warn.message <- "Original counts NOT used!"
    if (verbose) {
      cat("\n"); cat("  ",warn.message); cat("\n\n")
    }
  }
  else warn.message <- "Original counts used."

  if (sum(ZF)==0) {n.fixed <- 0}
  else {n.fixed <- t(ZF)%*%y}

  triple <- list(Z=Z,ZF=ZF,n=n.fixed)


  if (is.function(derht.fct)==FALSE) {derht.fct <- "Numerical derivatives used."}
  if (is.function(derLt.fct)==FALSE) {derLt.fct <- "Numerical derivatives used."}

  time.elapsed <- proc.time()[3] - start.time
  if (verbose) {
    cat("   Time Elapsed:", time.elapsed,"seconds",",   Iterations: ",iter)
    cat("\n\n")
  }


  list(y=y,m=m,covm=covm,p=p,covp=covp,
       lambda=lam,covlambda=covlam,
       resid=resid,presid=presid,adjresid=adjresid,covresid=covresid,
       Gsq=Gsq,Xsq=Xsq,Wsq=Wsq,PD.stat=PD.stat, df=lenh,
       beta=beta,covbeta=covbeta, Lobs=Lobs, L=L,covL=covL,Lresid=Lresid,covLres=covLres,
       iter=iter, norm.diff=norm.diff,norm.score=norm.score,
       h.fct=h.fct,h.input.fct=h.input.fct, h.mean=h.mean, derht.fct=derht.fct,
       L.fct=L.fct,L.input.fct=L.input.fct, L.mean=L.mean, derLt.fct=derLt.fct,X=X,
       U=U,triple=triple,strata=strata,fixed.strata=fixed.strata,
       warn.message=warn.message,warn.message.score=warn.message.score,
       warn.message.diff=warn.message.diff,time.elapsed=time.elapsed)
}
######################   end mph.fit   ##########################################
#################################################################################



#######################  end of file #################################################
