

#' @title Fit Bayesian SITAR growth curve model
#'
#' @description The \strong{bsitar()} is the main function that fits the
#'   Bayesian version of the super imposition by translation and rotation
#'   (\emph{SITAR}) model. The \emph{SITAR} model is a nonlinear mixed effects
#'   model that has been used extensively to summarize growth processes (such as
#'   height and weight) from early childhood through the adulthood. The
#'   frequentist version of the \emph{SITAR} model can be fit by using an
#'   already available R package, \pkg{sitar} \insertCite{R-sitar}{bsitar}.
#'   Besides Bayesian implementation, the \pkg{bsitar} package greatly enhances
#'   the modelling capabilities of the \emph{SITAR}. For example, in addition to
#'   the univariate analysis (i.e, modelling a single outcome), the \pkg{bsitar}
#'   allows univariate-by-subgroup and multivariate model fitting. In
#'   univariate-by-subgroup analysis, a single outcome is simultaneously
#'   analysed for subgroups defined by a factor variable such as gender. An
#'   advantage of univariate-by-subgroup analysis is that posterior draws for
#'   each sub group are part of a single model object that makes it possible to
#'   compare coefficients across groups and also test various hypotheses. The
#'   multivariate analysis involves simultaneous joint modelling of two or more
#'   outcomes.
#'  
#' @details The \emph{SITAR} is a shape-invariant nonlinear mixed effect growth
#'   curve model that fits a population average (i.e., mean average) curve to
#'   the data, and aligns each individual's growth trajectory to the underlying
#'   population average curve via a set of (typically) three random effects: the
#'   \code{size}, \code{timing} and \code{intensity}. Additionally, a slope
#'   parameter can be included as a random effect to estimate the variability in
#'   adult growth rate (See [sitar::sitar()] for details). The concept of shape
#'   invariant model (SIM) was first described by
#'   \insertCite{Lindstrom1995;textual}{bsitar} and later used by
#'   \insertCite{Beath2007;textual}{bsitar} to model infant growth data (birth
#'   to 2 years). The current version of the \emph{SITAR} model is developed by
#'   \insertCite{Cole2010;textual}{bsitar} and has been used extensively for
#'   modelling growth data
#'  \insertCite{@see @nembidzaneUsingSITARMethod2020 and @Sandhu2020}{bsitar}. 
#'  
#'  The frequentist version of the \emph{SITAR} model can be fit by using an
#'  already available R package, the \pkg{sitar} \insertCite{R-sitar}{bsitar}.
#'  The framework of Bayesian implementation of the \emph{SITAR} model in
#'  \pkg{bsitar} package is same as the \pkg{sitar} package with the exception
#'  that unlike the \pkg{sitar} package which uses B spline basis for the
#'  natural cubic spline design matrix (by calling the [splines::ns()]), the
#'  \pkg{bsitar} package uses the truncated power basis approach (see
#'  \insertCite{harrell2001regression;textual}{bsitar}, and 
#'  \insertCite{R-Hmisc;textual}{bsitar} for details) to construct the spline
#'  design matrix. Note that \pkg{bsitar} package builds the spline design
#'  matrix on the fly which is then included in the \code{functions} block of
#'  the **Stan** program and hence compiled (via the c++) during the model fit.
#'  
#'  Like \pkg{sitar} package \insertCite{Cole2010}{bsitar}, the \pkg{bsitar}
#'  package fits \emph{SITAR} model with (usually) up to three random effects:
#'  the size (parameter defined as \code{a}), the timing (parameter defined as
#'  \code{b}) and the intensity (parameter defined as \code{c}). In addition,
#'  there is a slope parameter (defined as \code{d}) that models the variability
#'  in the adult slope of the growth curve (See [sitar::sitar()] for details).
#'  Please note that author of the \pkg{sitar} package
#'  \insertCite{Cole2010}{bsitar} enforces the inclusion of parameter
#'  \code{d} as a random effects only and therefore excludes it from the 
#'  fixed fixed structure of the model. However, the \pkg{bsitar} package allows
#'  inclusion of parameter \code{d} in fixed and/or in the random effects
#'  structures of the \emph{SITAR} model. For the three parameter version of the
#'  \emph{SITAR} model (default), the fixed effects structure (i.e., population
#'  average trajectory) is specified as \code{fixed = 'a+b+c'}, and the
#'  random effects structure that captures the deviation of individual
#'  trajectories from the population average curve is specified as \code{random
#'  = 'a+b+c'}. Note that user need not to include all the three parameters in
#'  the fixed or the random effect structure. For example, a fixed effect
#'  version of the \emph{SITAR} model can be fit by setting randoms as an empty 
#'  string i.e., \code{random = ''}. Furthermore, the fixed effect structure may 
#'  include only a sub set of the parameters e.g., size and timing parameters 
#'  (\code{fixed = 'a+b'}) or the size and the intensity parameters 
#'  (\code{fixed = 'a+c'}). The four parameters version of the \emph{SITAR} 
#'  model is fit by including parameter \code{d} in the \code{fixed} and/or the 
#'  \code{random} arguments. Similar to the three parameter \emph{SITAR} model, 
#'  user can fit model with a sub set of the fixed and/or the random effects.
#'  
#'  The \pkg{sitar} package internally depends on the \pkg{brms} 
#'  package \insertCite{@see @R-brms; @brms2021}{bsitar}. The \pkg{brms} can
#'  fit a wide range of hierarchical linear and nonlinear regression models
#'  including multivariate models. The \pkg{brms} itself depends on the **Stan**
#'  software program full Bayesian inference
#'  \insertCite{@see @teamStanReferenceManual; @gelman2015}{bsitar}. Like 
#'  \pkg{brms}, the \pkg{bsitar} package allows a wide range of prior
#'  specifications that encourage the users to specify priors that
#'  actually reflect their prior knowledge about the human growth processes,
#'  (such as timing and intensity of the growth spurt). For prior
#'  specification, we follow the carefully crafted approaches used in the
#'  \pkg{brms} and \pkg{rstanarm} packages. For example, we follow the
#'  \pkg{brms} package in using the \code{student_t} distribution for the
#'  regression coefficients as well as the standard deviation for group
#'  level random effects, but set \code{exponential} distribution for the
#'  residual standard deviation as used in the \pkg{rstanarm} package. 
#'  Like \pkg{brms} and \pkg{rstanarm} packages, the \pkg{bsitar} package allows
#'  for auto scaling of the scale parameter for the location-scale based
#'  distributions such as \code{normal} and \code{student_t}. While
#'  \pkg{rstanarm} earlier used to set \code{autosclae} as \code{2.5} (recently
#'  authors changed this behavior to \code{FALSE}), the \pkg{brms} package sets
#'  it as \code{1.0} or \code{2.5} depending on the standard deviation of the
#'  response variable (See [brms::prior()]). The \pkg{bsitar} package, on the
#'  other hand, offers full flexibility in choosing the scale factor as any real
#'  number (e.g., \code{autosclae = 5.0}). When \code{autosclae = TRUE}, the
#'  \code{2.5} is the default scaling factor. We strongly recommend to go
#'  through the well documented details on prior specifications used in
#'  \pkg{brms} and \pkg{rstanarm} packages.
#'  
#'  Like \pkg{brms} package, the \pkg{bsitar} package offers a range of tools to
#'  evaluate the model fit that include posterior predictive check (see
#'  [brms::pp_check()]) and the leave one out (\code{loo}) cross validation (see
#'  [brms::loo()]). Furthermore, while the excellent post-processing support
#'  offered by the \pkg{brms} package is directly available to the users, the
#'  \pkg{bsitar} package includes many customized functions that allow for
#'  estimation and visualization of population average and individual specific
#'  distance (increase in size) and velocity (change in rate of growth), as well
#'  as computation of population average and individual specific growth
#'  parameters such as age at peak growth velocity (APGV) and the peak growth
#'  velocity (PGV).
#'
#'  Finally, the \pkg{bsitar} package allows three different types of model
#'  specifications: \code{'univariate'}, \code{'univariate_by'} and
#'  \code{'multivariate'}. A \code{'univariate'} fitting involves a single model
#'  applied to an outcome whereas both \code{'univariate_by'} and
#'  \code{'multivariate'} specifications comprise two or more sub models. The
#'  \code{'univariate_by'} fits two or more sub models to an outcome variable
#'  defined by a factor variable (e.g, sex). The data are typically stacked and
#'  the factor variable is used to set-up the sub models via the \code{'subset'}
#'  option available in the [brms::brm()]. The \code{'multivariate'} model allows
#'  simultaneous modelling of two or more outcomes with joint a distribution of
#'  the random effects. For both \code{'univariate_by'} and \code{'multivariate'}
#'  models, the \pkg{bsitar} package allows full flexibility in specifying
#'  separate arguments such as predictor variables (\code{x}), degree of freedom
#'  (\code{df}) for design matrix as well as the priors and the initial values.
#'  Furthermore, to enhance the ease of specifying different options and to make
#'  it user-friendly, there is no need to enclose the character option(s) in
#'  single or double quotes. For example to specify the \code{'univariate_by'}
#'  for sex, the \code{univariate_by = sex} is same as \code{univariate_by =
#'  'sex'} or \code{univariate_by = "sex"}. The same applies for all character
#'  string options.
#'
#'@param x Predictor variable (typically age in years). For \code{univariate}
#'  model, the \code{x} is a single variable whereas for \code{univariate_by}
#'  and \code{multivariate} models, the \code{x} can be same for sub models, or
#'  different for each sub model. For example, when fitting a bivariate model,
#'  the \code{x = list(x1, x2)} specifies that \code{x1} is the predictor
#'  variable for the first sub model, and \code{x2} for the second sub model. To
#'  specify \code{x1} as a common predictor
#'  variable for both sub models, the argument \code{x} is defined as \code{x =
#'  list(x1)} or simply \code{x = x1}.
#'
#'@param y Response variable (e.g., repeated height measurements). For
#'  \code{univariate} and \code{univariate_by} models, \code{y} is specified as
#'  a single variable. For \code{univariate_by} model, the response vector for
#'  each sub model is created and named internally based on the factor levels of
#'  the variable that is used to set up the \code{univariate_by} model. As an
#'  example, the model specified as \code{univariate_by = sex} creates response
#'  vectors \code{Female} and  \code{Male} when \code{Female} is the first level
#'  and \code{Male} is the second level of the \code{sex} variable. For
#'  \code{multivariate} model, the response variables are specified as a list
#'  such as \code{y = list(y1, y2}) where \code{y1} is the response variable for
#'  the first sub model and \code{y2} for the second sub model. Note that for
#'  \code{multivariate} model, data are not stacked but rather response vectors
#'  are separate variables in the \code{data} and are of same length.
#'
#'@param id A factor variable uniquely identifying the groups (e.g.,
#'  individuals) in the data frame. For \code{univariate_by} and
#'  \code{multivariate} models, the \code{id} can be same (typically) for sub
#'  models or different for each sub model (see argument \code{x} for details on
#'  setting different arguments for sub models).
#'
#'@param data Data frame containing variables such as \code{x}, \code{y}, 
#' \code{id} etc.
#'
#'@param df Degrees of freedom for the natural cubic spline design matrix
#'  (default \code{4}). The \code{df} is internally used to construct the
#'  knots (quantiles of \code{x} distribution) that are then used in the
#'  construction of the spline design matrix. For \code{univariate_by}
#'  and \code{multivariate} models, the \code{df} can be same (e.g., \code{df  =
#'  4}) for sub models or different for each sub model such as
#'  \code{df=list(4, 5)} where \code{df} is 4 is for the first sub model, and 5
#'  for the second sub model.
#'
#'@param knots A numeric vector vector specifying the knots for the natural
#'  cubic spline design matrix (default \code{NULL}) Note that \code{df} and
#'  \code{knots} can not be specified together, and also both of them can not be
#'  \code{NULL}. In other words, either \code{df} or \code{knots} must be
#'  specified. Like \code{df}, the \code{knots} can be same for sub models or
#'  different for each sub model when fitting \code{univariate_by} and
#'  \code{multivariate} models (see \code{df} for details).
#'
#'@param fixed A character string specifying the fixed effects structure
#'  (default \code{'a+b+c'}). Note that different fixed effect structures can be
#'  specified when fitting \code{univariate_by} and \code{multivariate} models.
#'  As an example, \code{fixed = list('a+b+c', 'a+b')} implies that the fixed
#'  effect structure for the first sub model is \code{'a+b+c'}, and \code{'a+b'}
#'  for the second sub model.
#'
#'@param random A character string specifying the random effects structure
#'  (default \code{'a+b+c'}). The approach used in setting the \code{random} is
#'  same as described above for the fixed effects structure (see \code{fixed}).
#'  
#'@param xoffset An optional character string, or a numeric value to set up the
#'  origin of the predictor variable, \code{x} (i.e., centering of \code{x}).
#'  The options available are \code{'mean'} (mean of x, i.e., \code{mean(x)}),
#'  \code{'max'} (maximum value of x, i.e., \code{max(x)}), \code{'min'}
#'  (minimum value of x, i.e., \code{min(x)}), \code{'apv'} (age at peak
#'  velocity estimated from the velocity curve derived from the simple linear
#'  model fit to the data), or any real number such as \code{xoffset = 12}.
#'  The default is \code{xoffset = 'mean'}. For \code{univariate_by} 
#'  and \code{multivariate} models, the \code{xoffset} can be same for sub 
#'  models or different for each sub model (see argument \code{x} for details
#'  on setting different arguments for sub models).
#'
#'@param bstart An optional character string, or a numeric value to set up the
#'  origin of the fixed effect parameter \code{b}. The argument \code{bstart}
#'  can be used to set up the location parameter for the location-scale based
#'  priors (such as \code{normal()}) via \code{b_prior_beta} argument and/or the
#'  initial value via the \code{b_init_beta} argument. The options available to
#'  set up the \code{bstart} are same as described above for the \code{xoffset}
#'  i.e., \code{'mean'}, \code{'min'}, \code{'max'}, \code{'apv'} or a real
#'  number such as \code{12}. The default is same as \code{xoffset} i.e.,
#'  \code{bstart = 'xoffset'}. For \code{univariate_by} and  \code{multivariate}
#'  models, the \code{xoffset} can be same for sub models (typically), or
#'  different for each sub model (see argument \code{x} for details on setting
#'  different arguments for sub models).
#'
#'@param cstart An optional character string, or a numeric value to set up the
#'  origin of the fixed effect parameter \code{c}. The argument \code{cstart}
#'  can be used to set up the location parameter for the location-scale based
#'  priors (such as \code{normal()}) via \code{c_prior_beta} argument and/or the
#'  initial value via the \code{c_init_beta} argument. The options available to
#'  set up the \code{cstart} are \code{'pv'} (peak velocity estimated from the
#'  velocity curve derived from the simple linear model fit to the data), or a
#'  real number such as \code{1}. Note that since parameter \code{c} is
#'  estimated on the exponential scale, the argument \code{cstart} should be
#'  adjusted accordingly. The default \code{cstart} is '0' i.e., \code{cstart =
#'  '0'}. For \code{univariate_by} and \code{multivariate} models,
#'  the \code{xoffset} can be same for sub models (typically), or different for
#'  each sub model (see argument \code{x} for details on setting different
#'  arguments for sub models).
#'
#'@param xfun An optional character string to specify the transformation of the
#'  predictor variable, The default is \code{NULL} indicating that no
#'  transformation is applied i.e., model is fit to the data with original scale
#'  of the \code{x}. Available transformation options are \code{'log'}
#'  (logarithmic transformation) and \code{'sqrt'} (square root transformation).
#'  For \code{univariate_by} and \code{multivariate} models, the \code{xfun} can
#'  be same for sub models (typically), or different for each sub model (see
#'  argument \code{x} for details on setting different arguments for sub
#'  models).
#'
#'@param yfun An optional character string to specify the transformation of the
#'  response variable, The default is \code{NULL}, indicating that no
#'  transformation is applied i.e., model is fit to the data with original scale
#'  of the \code{y}. Available transformation options are \code{'log'}
#'  (logarithmic transformation) and \code{'sqrt'} (square root transformation).
#'  For \code{univariate_by} and \code{multivariate} models, the \code{xfun} can
#'  be same for sub models (typically), or different for each sub model (see
#'  argument \code{x} for details on setting different arguments for sub
#'  models).
#'
#'@param bound An optional real number to extend the span of the predictor
#'  variable \code{x} by a small value (default 0.04). See package
#'  [sitar::sitar()] for details. For \code{univariate_by} and
#'  \code{multivariate} models, the \code{bound} can be same for sub models
#'  (typically), or different for each sub model (see argument \code{x} for
#'  details on setting different arguments for sub models).
#'
#'@param terms_rhs An optional character string (default \code{NULL}) to specify
#'  terms on the right hand side of the response variable (separated by
#'  \code{|}) but before the formula tilde sign i.e., \code{~}. The
#'  \code{terms_rhs} is used when fitting a measurement error model. As an
#'  example, consider fitting a model with measurement error in the response
#'  variable which is specified in the [brms::brmsformula()] as
#'  \code{brmsformula(y | mi(sdy) ~ ..)}. In this example, the \code{mi(sdy)} is
#'  passed to [brms::brmsformula()] as \code{terms_rhs = mi(sdy)}. For
#'  \code{multivariate} model, each outcome can have its own measurement error
#'  variable that can be specified as follows: \cr \code{terms_rhs =
#'  list(mi(sdy1), mi(sdy2))}. Note that [brms::brmsformula()] does not allow
#'  combining \code{mi()} with the \code{subset()} formulation that is used for
#'  fitting \code{univariate_by} model.
#'
#'@param a_formula Formula for the fixed effect parameter, \code{a} (default
#'  \code{~ 1}). User can specify different formula when fitting
#'  \code{univariate_by} and \code{multivariate} models. As an example
#'  \code{a_formula = list(~1, ~1 + cov)} implies that the \code{a_formula} for
#'  the first sub model includes an intercept only whereas the second sub model
#'  includes an intercept and a covariate, \code{cov}. The covariate(s)  can be
#'  continuous variable(s) or factor variable(s). For factor covariates, dummy
#'  variables are created internally via the [stats::model.matrix()]). The
#'  formula can include any combination of continuous and factor variables as
#'  well as their interactions.
#'
#'@param b_formula Formula for the fixed effect parameter, \code{b} (default
#'  \code{~ 1}). See \code{a_formula} for details.
#'
#'@param c_formula Formula for the fixed effect parameter, \code{c} (default
#'  \code{~ 1}). See \code{a_formula} for details.
#'
#'@param d_formula Formula for the fixed effect parameter, \code{d} (default
#'  \code{~ 1}). See \code{a_formula} for details.
#'  
#'@param s_formula Formula for the fixed effect parameter, \code{s} (default
#'  \code{~ 1}). The \code{s_formula} sets up the the spline design matrix.
#'  Typically, covariate(s) are not included in the \code{s_formula} to limit
#'  the population curve to be single curve for the whole data. In fact, the
#'  [sitar::sitar()] does not provide any option to include covariates in the
#'  \code{s_formula}, However, \pkg{bsitar} package allows inclusion of
#'  covariates but the user need to justify the modelling of separate curves for
#'  each category when covariate is a factor variable.
#'
#'@param a_formula_gr Formula for the random effect parameter, \code{a} (default
#'  \code{~ 1}). Similar to \code{a_formula}, user can specify different formula
#'  when fitting \code{univariate_by} and \code{multivariate} models and formula
#'  can include continuous and/or factor variable(s) including their
#'  interactions as covariates (see \code{a_formula} for details). In addition
#'  to setting up the design matrix for the random effect parameter \code{a},
#'  user can set up the group identifier and the correlation structure for
#'  random effects via the vertical bar \code{||} approach. For example,
#'  consider only an intercept for the random effects \code{a}, \code{b}, and
#'  \code{c} specified as \code{a_formula_gr = ~1}, \code{b_formula_gr = ~1} 
#'  and \code{c_formula_gr = ~1}. To specify the group identifier 
#'  (e.g., \code{id}) and an unstructured correlation structure, the formula 
#'  argument as specified as follows: \cr
#'  \code{a_formula_gr = ~ (1|i|id)} \cr
#'  \code{b_formula_gr = ~ (1|i|id)} \cr
#'  \code{c_formula_gr = ~ (1|i|id)} \cr 
#'  where  \code{i} within the vertical bars \code{||} is just a placeholder. A
#'  common identifier (i.e., \code{i}) shared across random effect formulas are
#'  modeled as unstructured correlated. For more details on the the vertical bar
#'  approach, please see [brms::brm()]. As explained below, an alternative
#'  approach to set up the group identifier and the correlation structure is to
#'  use \code{group_by} argument. In other words, to achieve the same set up as
#'  defined above by using the vertical bar approach, user can just specify the
#'  design matrix part of the formula as
#'  \code{a_formula_gr = ~ 1} \cr
#'  \code{b_formula_gr = ~ 1} \cr
#'  \code{c_formula_gr = ~ 1} \cr  
#'  and use the \code{group_by} argument as \code{group_by = list(groupvar = id,
#'  cor = un)} where \code{id} specifies the group identifier and \code{un} sets
#'  up the unstructured correlation structure. See \code{group_by} argument for
#'  details.
#'
#'@param b_formula_gr Formula for the random effect parameter, \code{b} (default
#'  \code{~ 1}). See \code{a_formula_gr} for details.
#'
#'@param c_formula_gr Formula for the random effect parameter, \code{c} (default
#'  \code{~ 1}). See \code{a_formula_gr} for details.
#'
#'@param d_formula_gr Formula for the random effect parameter, \code{d} (default
#'  \code{~ 1}). See \code{a_formula_gr} for details.
#'  
#'@param a_formula_gr_str Formula for the random effect parameter, \code{a}
#'  (default \code{NULL}) when fitting a hierarchical model with three or more
#'  levels of hierarchy. An example is model applied to the data that comprise
#'  repeated measurements (level 1) on individuals (level 2) nested further
#'  within the growth studies (level 3). Note that When using
#'  \code{a_formula_gr_str} argument, only the vertical bar approach (see
#'  \code{a_formula_gr}) can be used to set up the group identifiers and the
#'  correlation structure. An example of setting up the formula for a three
#'  level model with random effect parameter \code{a}, \code{b} is as follows:
#'  \cr
#'  \code{a_formula_gr_str = ~ (1|i|id:study) + (1|i2|study)} \cr
#'  \code{b_formula_gr_str = ~ (1|i|id:study) + (1|i2|study)} \cr
#'  \code{c_formula_gr_str = ~ (1|i|id:study) + (1|i2|study)} \cr 
#'  where \code{|i|} and \code{|i2|} set up the unstructured correlation 
#'  structure for individual and study level random effects. Note that 
#'  \code{|i|} and \code{|i2|} need to be distinct because random effect 
#'  parameters are not allowed to be correlated across different levels of 
#'  hierarchy.
#'  It is worth mentioning that user can set up model with any number of 
#'  hierarchical levels and include covariate into the random effect formula.
#'  
#'@param b_formula_gr_str Formula for the random effect parameter, \code{b}
#'  (default \code{NULL}) when fitting a hierarchical model with three or more
#'  levels of hierarchy. See \code{a_formula_gr_str} for details.
#'
#'@param c_formula_gr_str Formula for the random effect parameter, \code{c}
#'  (default \code{NULL}) when fitting a hierarchical model with three or more
#'  levels of hierarchy. See \code{a_formula_gr_str} for details.
#'
#'@param d_formula_gr_str Formula for the random effect parameter, \code{d}
#'  (default \code{NULL}) when fitting a hierarchical model with three or more
#'  levels of hierarchy. See \code{a_formula_gr_str} for details.
#'  
#'@param d_adjusted A logical indicator to set up the scale of predictor
#'  variable \code{x} when fitting the model with random effect parameter
#'  \code{d}. The coefficient of parameter \code{d} is estimated as a linear
#'  function of \code{x} i.e., \code{d * x}. If \code{FALSE} (default), the
#'  original \code{x} is used. When \code{d_adjusted = TRUE}, the \code{x} is
#'  adjusted for the timing (\code{b}) and intensity (\code{c}) parameters as
#'  \code{x} - \code{b}) * \code{exp(c)} i.e., \code{d * ((x-b)*exp(c))}. The
#'  adjusted scale of \code{x} reflects individual developmental age rather than
#'  chronological age. This makes d more sensitive to the timing of puberty in
#'  individuals. See [sitar::sitar()] function for details.
#'
#'@param sigma_formula Formula for the fixed effect distributional parameter,
#'  \code{sigma}. The \code{sigma_formula} sets up the fixed effect design
#'  matrix that may include continuous and/or factor variables (and their
#'  interactions) as covariates(s) for  the distributional parameter. In other
#'  words, setting up the covariates for \code{sigma_formula} is same as for any
#'  other fixed parameter such as \code{a} (see \code{a_formula} for details).
#'  Note that \code{sigma_formula} estimates \code{sigma} parameter at
#'  \code{log} scale. By default, the \code{sigma_formula} is \code{NULL}
#'  because the [brms::brm()] itself models the \code{sigma} as a residual
#'  standard deviation (\code{RSD}) parameter at the link scale. The
#'  \code{sigma_formula} along with the arguments \code{sigma_formula_gr} and
#'  \code{sigma_formula_gr_str} allow estimating the scale parameters as random
#'  effects for \code{sigma}. The set up to specify the fixed and random effects
#'  for \code{sigma} is similar to setting fixed and random effect structures
#'  for other model parameters such as \code{a}, \code{b}, and \code{c}. It is
#'  important to note that an alternative way to set up the fixed effect design
#'  matrix for the distributional parameter \code{sigma} is to use the
#'  \code{dpar_formula} argument. An advantage of \code{dpar_formula} over
#'  \code{sigma_formula} is that user can specify the linear and nonlinear
#'  formulation as allowed by the [brms::lf()] and [brms::nlf()] syntax. The
#'  [brms::lf()] and [brms::nlf()] offer flexibility in centering the predictors
#'  and also allows enabling/disabling of cell mean centering when excluding
#'  \code{intercept} via \code{0 + } formulation. A disadvantage of
#'  \code{dpar_formula} approach is that it is not possible to include random
#'  effects for the \code{sigma}. Note that \code{sigma_formula} and
#'  \code{dpar_formula} can not be specified together. When either
#'  \code{sigma_formula} or \code{dpar_formula} is used, the default estimation
#'  of the \code{RSD} by [brms::brm()] is automatically turned off.
#'
#'@param sigma_formula_gr Formula for the random effect parameter, \code{sigma}
#'  (default \code{NULL}). See \code{a_formula_gr} for details.
#'
#'@param sigma_formula_gr_str Formula for the random effect parameter,
#'  \code{sigma} when fitting a hierarchical model with three or more
#'  levels of hierarchy. See \code{a_formula_gr_str} for details. 
#'
#'@param dpar_formula Formula for the distributional fixed effect parameter,
#'  \code{sigma} (default \code{NULL}). See \code{sigma_formula} for details.
#'  
#'@param autocor_formula Formula to set up the autocorrelation structure of
#'  residuals (default \code{NULL}). Allowed autocorrelation structures are: 
#'  \itemize{
#'  \item autoregressive moving average (\code{arma}) of order \code{p} and 
#'  \code{q} specified as \code{autocor_formula = ~arms(p=1, q=1)}.
#'  \item autoregressive (\code{ar}) of order \code{p} specified as 
#'  \code{autocor_formula = ~ar(p=1)}.
#'  \item moving average (\code{ma}) of order \code{q} specified as 
#'  \code{autocor_formula = ~ma(q=1)}. 
#'  \item unstructured (\code{unstr}) over time (and individuals), The 
#'  \code{unstr} structure is specified as 
#'  \code{autocor_formula = ~unstr(time, id))}. 
#'  }
#'  See [brms::brm()] for further details on modeling autocorrelation structure
#'  of residuals
#'  
#'@param family Family distribution (default \code{gaussian}) and the link
#'  function (default \code{identity}). See [brms::brm()] for details on
#'  available distributions and link functions, and how to specify them. For
#'  \code{univariate_by} and \code{multivariate} models, the \code{family} can
#'  be same (e.g., \code{family = gaussian()}) for sub models or different for
#'  each sub model such as \code{family = list(gaussian(), student())} which
#'  sets \code{gaussian} distribution for the first sub model and
#'  \code{student_t} distribution for the second sub model.
#'
#'@param group_arg Specify arguments for group-level random effects. The
#'  \code{group_arg} should be a named list that may include \code{groupvar},
#'  \code{dist}, \code{cor} and \code{by} as described below:
#'  \itemize{
#'  \item The \code{groupvar} specifies the subject identifier. In case
#'  \code{groupvar = NULL} (default), the \code{groupvar} is automatically
#'  assigned based on the \code{id} argument.
#'  \item The \code{dist} specifies the distribution from which the random
#'  effects are drawn (default \code{gaussian}). As per the [brms::brm()]
#'  documentation, the \code{gaussian} distribution is the only available
#'  distribution (as of now).
#'  \item The \code{by} argument can be used to estimate separate variance
#'  covariance structure (i.e., standard deviation and correlation parameters)
#'  for random effect parameters (default \code{NULL}). If specified, variable
#'  used to set up the \code{by} argument must be a factor variable. For
#'  example, \code{by = 'sex'} implies that separate variance covariance
#'  structure are estimated for males and females.
#'  \item The \code{cor} is used to set up the covariance (i.e., correlation)
#'  structure for random effect parameters. The default covariance is
#'  unstructured (i.e, \code{cor = un}) for all three model settings, i.e.,
#'  \code{univariate}, \code{univariate_by} and \code{multivariate}. The
#'  alternative correlation structure available for \code{univariate} and
#'  \code{univariate_by} models is \code{diagonal}. While the \code{cor = un}
#'  models the full unstructured variance covariance structure, the \code{cor
#'  = diagonal} estimates only the variance (i.e, standard deviation) parameters
#'  and the covariance (i.e., correlation) parameters are set to zero. For
#'  \emph{multivariate} model, options include \code{un}, \code{diagonal} and
#'  \code{un_s}. The \code{un} sets up the unstructured correlation implying
#'  that the group level random effects across response variables are drawn for
#'  a joint multivariate normal distribution with shared correlation parameters.
#'  The \code{cor = diagonal} specifies that only the variance parameter are
#'  estimates for each sub model whereas the correlation parameters set to zero.
#'  Option \code{cor = un_s} allows for estimating unstructured variance
#'  covariance parameters separately for each response variable.
#'  }
#'  Note that user need not to define all or any of these options (i.e.,
#'  \code{groupvar}, \code{dist}, \code{cor}, or \code{by}) because if
#'  unspecified, they are are automatically set to their default values. Also
#'  note that only \code{groupvar} from the \code{group_arg} argument is passed
#'  on to the \emph{univariate_by} and \emph{multivariate} models because these
#'  model have their own additional options specified via the
#'  \code{univariate_by} and \code{multivariate} arguments. Lastly, the
#'  \code{group_arg} is completely ignored when user specify random effects via
#'  the vertical bar \code{||} approach (see \code{a_formula_gr} for details) or
#'  when fitting a hierarchical model with three or more levels of hierarchy
#'  (see \code{a_formula_gr_str} for details).
#'  
#'@param sigma_group_arg Specify arguments for modelling distributional level
#'  random effects, \code{sigma}. The approach used in setting up the
#'  \code{sigma_group_arg} is exactly same as described above for the group
#'  level random effects (see \code{group_arg} for details).
#'
#'@param univariate_by Set up the univariate-by-subgroup model fitting (default
#'  \code{NULL}) via a named list as described below:
#'  \itemize{
#'  \item The \code{by} (an optional character string) is used to specify the
#'  variable (must be a factor variable) to define the sub models (default
#'  \code{NA}).
#'  \item The \code{cor} (an optional character string) specifies the
#'  correlation structure. The options available are \code{un} and
#'  \code{diagonal}. The \code{un = un} (default) models the full unstructured
#'  variance covariance structure, whereas the \code{cor = diagonal} estimates
#'  only the variance (i.e, standard deviation) parameters and the covariance
#'  (i.e., correlation) parameters are set to zero.
#'  \item The \code{terms} (an optional character string) specifies the method
#'  used in setting up the sub models. Options are \code{'subset'} (default) and
#'  \code{'weights'}. See \code{brms::`addition-terms`} for details.
#'  }
#'
#'@param multivariate Set up the multivariate model fitting (default
#'  \code{NULL}) arguments as a named list:
#'  \itemize{
#'  \item The \code{mvar} (logical, default \code{FALSE}) indicates whether to
#'  fit a multivariate model.
#'  \item The \code{cor} (an optional character string) sets up the correlation
#'  structure. The options available are \code{un}, \code{diagonal} and
#'  \code{un_s}. The \code{un} sets up the unstructured correlation implying
#'  that the group level random effects across response variables are drawn for
#'  a joint multivariate normal distribution with shared correlation parameters.
#'  The \code{cor = diagonal} specifies that only the variance parameter are
#'  estimates for each sub model whereas the correlation parameters set to zero.
#'  Option \code{cor = un_s} allows for estimating unstructured variance
#'  covariance parameters separately for each response variable.
#'  \item The \code{rescor} (logical, default \code{TRUE}) indicates whether to
#'  estimate the residual correlation between response variables.
#'  }
#'
#'@param a_prior_beta Specify priors for the fixed effect parameter, \code{a}.
#'  (default \code{student_t(3, ymean, ysd, autoscale = TRUE)}). The key points
#'  in prior specification that are applicable for all parameters are
#'  highlighted below. For full details on prior specification, please see
#'  [brms::prior()].
#'  \itemize{
#'  \item Allowed distributions are \code{normal}, \code{student_t},
#'  \code{cauchy}, \code{lognormal}, \code{uniform}, \code{exponential},
#'  \code{gamma} and \code{inv_gamma} (inverse gamma). \item For each
#'  distribution, upper and lower bounds can be set via options \code{lb} and
#'  \code{ub} (default \code{NA} for both \code{lb} and \code{ub}). \item For
#'  location-scale based distributions (such as \code{normal}, \code{student_t},
#'  \code{cauchy}, and \code{lognormal}), an option \code{autosclae} (default
#'  \code{FALSE}) can be used to multiply the scale parameter by a numeric
#'  value. Both \pkg{brms} and \pkg{rstanarm} packages allow similar auto
#'  scaling under the hood. While \pkg{rstanarm} earlier used to set
#'  \code{autosclae} as \code{TRUE} which internally multiplied scale parameter
#'  by a value 2.5 (recently authors changed this behavior to \code{FALSE}), the
#'  \pkg{brms} package sets scaling factor as 1.0 or 2.5 depending on the
#'  standard deviation of the response variable (See [brms::prior()]). The
#'  \pkg{bsitar} package offers full flexibility in choosing the scaling factor
#'  as any real number instead of 1.0 or 2.5 (e.g., \code{autosclae = 5.0}).
#'  When \code{autosclae = TRUE}, \code{2.5} is the default scaling factor.
#'  \item For strictly positive distributions such as \code{exponential},
#'  \code{gamma} and \code{inv_gamma}, the lower bound (\code{lb}) is
#'  automatically set to zero i.e., \code{lb = 0}. \item For uniform
#'  distribution, an option \code{addrange} is available to symmetrically widen
#'  the prior range. For example, prior \code{uniform(a, b, addrange = 5)}
#'  implies that the lower and upper limits will be evaluated as
#'  \code{uniform(a-5, b+5)}. \item For exponential distribution, the rate
#'  parameter is evaluated as inverse. In other words, prior set as
#'  \code{exponential(10)} is translated to 0.1 i.e.,
#'  \code{exponential(1.0/10.0)}. \item User need not to specify each option
#'  explicitly because the missing options are set to their default values
#'  automatically. For example, the prior specified as
#'  \code{a_prior_beta = normal(location = 5, scale = 1, lb = NA, ub = NA,
#'  addrange = NA, autosclae = FALSE)}) is same as
#'  \code{a_prior_beta = normal(5, 1)}).
#'  \item For \code{univariate_by} \code{multivariate} models, priors
#'  can be same for sub models (e.g., \code{a_prior_beta =
#'  normal(5, 1)}), or different for each sub such as \code{a_prior_beta =
#'  list(normal(5,1), normal(10, 5)}).
#'  }
#'  The location parameter for the location-scale based distributions can be
#'  specified as mean (by specifying \code{'ymean'}) or the median (by using
#'  \code{'ymedian'}) of the response variable. Similarly, the scale parameter
#'  can be set as the standard deviation (SD) or the median absolute deviation
#'  (MAD) of the response variable via \code{'ysd'} and \code{'ymad'} options.
#'  Another option available is to use the coefficients \code{'lm'} from the
#'  simple linear model applied to the data (e.g., \code{lm(y ~ age, data =
#'  data}). This is true even when model has covariates i.e.,
#'  \code{lm(y ~ age + cov, data = data}).  A few examples of specifying priors
#'  using these options are: \cr
#'  \code{a_prior_beta = normal(ymean, ysd)}, \cr 
#'  \code{a_prior_beta = normal(ymean, ysd)}, \cr 
#'  \code{a_prior_beta = normal(ymedian, ymad)}, \cr 
#'  \code{a_prior_beta = normal(lm, ysd)}, \cr 
#'  Note that options \code{'ymean'}, \code{'ymedian'}, \code{'ysd'},
#'  \code{'ymad'}, \code{'ymad'} and \code{'lm'} are available only for the
#'  fixed effect parameter, \code{a} and not for parameters \code{b}, \code{c}
#'  or \code{d}.
#'  
#'@param b_prior_beta Specify priors for the fixed effect parameter, \code{b}.
#'  (default \code{student_t(3, 0, 3.5, autoscale = FALSE)}). See
#'  \code{a_prior_beta} for details.
#'
#'@param c_prior_beta Specify priors for the fixed effect parameter, \code{c}.
#'  (default \code{student_t(3, 0, 1.5, autoscale = FALSE)}). See
#'  \code{a_prior_beta} for details.
#'
#'@param d_prior_beta Specify priors for the fixed effect parameter, \code{d}.
#'  (default \code{student_t(3, 0, 1.0, autoscale = FALSE)}). See
#'  \code{a_prior_beta} for details.
#'  
#'@param s_prior_beta  Specify priors for the fixed effect parameter, \code{s}
#'  (i.e., spline coefficients). (default \code{student_t(3, 0, 'lm', autoscale
#'  = TRUE)}). The general approach is same as described
#'  earlier for the fixed effect parameters (see \code{a_prior_beta} for
#'  details). A few key points are highlighted below:
#'  \itemize{
#'  \item When specifying location-scale based priors using 'lm' such as
#'  \code{s_prior_beta = normal(lm, 'lm')} , it sets spline coefficients
#'  obtained from the simple linear model fit as location parameter whereas
#'  scale parameter is based on the standard deviation of the spline design
#'  matrix. However, typically, the location parameter is set at '0' (default),
#'  and the autoscale option is set as \code{TRUE}.
#'  \item For location-scale based priors, an option \code{sethp} (logical,
#'  default \code{FALSE}) is available to set up the hierarchical priors. To set \code{sethp} as \code{TRUE}, the prior is
#'  specified as \code{s_prior_beta = normal(0, 'lm', autoscale = TRUE, sethp =
#'  TRUE)}). When \code{sethp = TRUE}, instead of setting prior as \code{s ~ normal(0,
#'  'lm')} the hierarchical priors are set as \code{s ~ normal(0, 'hp')} where
#'  \code{'hp'} is defined as \code{hp ~ normal(0, 'lm')}. Note that the scale 
#'  parameter for the  \code{hp ~ normal(0, 'lm')} is automatically taken from the 
#'  \code{s ~ normal(0, 'hp')}. Setting \code{sethp = TRUE} implies that the 
#'  scale for spline coefficients is estimated from the data itself. The 
#'  distribution of hierarchical priors is automatically matched with the prior 
#'  set for the \code{s} parameter, or else can be set by the same \code{sethp} 
#'  option. For example, \code{s_prior_beta = normal(0, 'lm', sethp = cauchy)} 
#'  will be translated to \code{s ~ normal(0, 'lm')}, \code{hp  ~ cauchy(0, 'lm')}.
#'  \item For \code{uniform} priors, the  option\code{addrange} can be used to
#'  symmetrically expand the prior range.
#'  }
#'  It is observed that location scale based prior distributions (e.g,
#'  \code{normal}, \code{student_t}, and \code{cauchy}) perform well for the
#'  spline coefficients.
#'  
#'@param a_cov_prior_beta Specify priors for the covariate(s) included in the
#'  fixed effect parameter, \code{a} (default \code{student_t(3, 0, 5.0,
#'  autoscale = FALSE)}). The approach is same as described earlier for the
#'  \code{a_prior_beta} except that options \code{'ymean'}, \code{'ymedian'},
#'  \code{'ysd'}, and \code{'ymad'} are not allowed. The Option \code{'lm'} for
#'  the location parameter sets covariate(s) coefficient obtained from the
#'  simple linear model fit to the data. Note that option \code{'lm'} is allowed
#'  only for the \code{a_cov_prior_beta} and not for the covariate(s) included
#'  in the other fixed or random effect parameters. Lastly, separate priors can
#'  be specified for sub models when fitting \code{univariate_by} and
#'  \code{a_prior_beta} models (see \code{a_prior_beta}).
#' 
#'@param b_cov_prior_beta Specify priors for the covariate(s) included in the
#'  fixed effect parameter, \code{b} (default \code{student_t(3, 0, 1.0,
#'  autoscale = FALSE)}). See \code{a_cov_prior_beta} for details.
#'
#'@param c_cov_prior_beta Specify priors for the covariate(s) included in the
#'  fixed effect parameter, \code{c} (default \code{student_t(3, 0, 0.1,
#'  autoscale = FALSE)}). See \code{a_cov_prior_beta} for details.
#'
#'@param d_cov_prior_beta Specify priors for the covariate(s) included in the
#'  fixed effect parameter, \code{d} (default \code{student_t(3, 0, 1.0,
#'  autoscale = FALSE)}). See \code{a_cov_prior_beta} for details.
#'  
#'@param s_cov_prior_beta Specify priors for the covariate(s) included in the
#'  fixed effect parameter, \code{s} (default \code{student_t(3, 0, 10.0,
#'  autoscale = FALSE)}). However, as described earlier, (see \code{s_formual}),
#'  the \emph{SITAR} model does not allows for inclusion of covariate(s) in the
#'  spline design matrix. If and when covariate(s) are specified (see
#'  \code{s_formual}), the approach of setting priors for the covariate(s)
#'  included in the parameter, \code{s} via \code{s_cov_prior_beta} is same as
#'  described earlier for the fixed effect parameter \code{a} (see
#'  \code{a_cov_prior_beta}). For the location-scale based priors, the option
#'  \code{'lm'} sets the location parameter same as the spline coefficients
#'  obtained from fitting a simple linear to the data.
#'
#'@param a_prior_sd Specify priors  for the random effect parameter, \code{a}.
#'  (default \code{student_t(3, 0, 'ysd', autoscale = FALSE)}). Note that prior
#'  is on the standard deviation (which is the square root of the variance) and
#'  not on the variance itself. The approach of setting the prior is same as
#'  described earlier for the fixed effect parameter, \code{a} (See
#'  \code{a_prior_beta}) with the exception that location parameter is always
#'  zero. The lower bound \code{0} is automatically set by the
#'  \code{brms::brm()}. For \code{univariate_by} and \code{multivariate} models,
#'  priors can be same for sub models or different for each sub model (See
#'  \code{a_prior_beta}).
#'
#'@param b_prior_sd  Specify priors  for the random effect parameter, \code{b}
#'  (default \code{student_t(3, 0, 2.0, autoscale = FALSE)}). See
#'  \code{a_prior_sd} for details.
#'
#'@param c_prior_sd Specify priors  for the random effect parameter, \code{c}
#'  (default \code{student_t(3, 0, 1.25, autoscale = FALSE)}). See
#'  \code{a_prior_sd} for details.
#'
#'@param d_prior_sd Specify priors  for the random effect parameter,
#'  \code{d} (default \code{student_t(3, 0, 1.0, autoscale = FALSE)}). See
#'  \code{a_prior_sd} for details.
#'
#'@param a_cov_prior_sd Specify priors for the covariate(s) included in the
#'  random effect parameter, \code{a} (default \code{student_t(3, 0, 5.0,
#'  autoscale = FALSE)}). The approach is same as described earlier for the
#'  \code{a_cov_prior_beta} except that no pre-defined option (e.g.,
#'  \code{'lm'}) is allowed.
#'
#'@param b_cov_prior_sd Specify priors for the covariate(s) included in the
#'  random effect parameter, \code{b} (default \code{student_t(3, 0, 1.0,
#'  autoscale = FALSE)}). See \code{a_cov_prior_sd} for details.
#'
#'@param c_cov_prior_sd Specify priors for the covariate(s) included in the
#'  random effect parameter, \code{c} (default \code{student_t(3, 0, 0.1,
#'  autoscale = FALSE)}). See \code{a_cov_prior_sd} for details.
#'
#'@param d_cov_prior_sd Specify priors for the covariate(s) included in the
#'  random effect parameter, \code{d} (default \code{student_t(3, 0, 1.0,
#'  autoscale = FALSE)}). See \code{a_cov_prior_sd} for details.
#'
#'@param a_prior_sd_str Specify priors for the random effect parameter, \code{a}
#'  when fitting a hierarchical model with three or more levels of hierarchy
#'  (default \code{NULL}). The approach is same as described earlier (see the
#'  \code{a_prior_sd}).
#'
#'@param b_prior_sd_str Specify priors for the random effect parameter, \code{b}
#'  when fitting a hierarchical model with three or more levels of hierarchy
#'  (default \code{NULL}). The approach is same as described earlier (see the
#'  \code{a_prior_sd_str}).
#'
#'@param c_prior_sd_str Specify priors for the random effect parameter, \code{c}
#'  when fitting a hierarchical model with three or more levels of hierarchy
#'  (default \code{NULL}). The approach is same as described earlier (see the
#'  \code{a_prior_sd_str}).
#'
#'@param d_prior_sd_str Specify priors for the random effect parameter, \code{d}
#'  when fitting a hierarchical model with three or more levels of hierarchy
#'  (default \code{NULL}). The approach is same as described earlier (see the
#'  \code{a_prior_sd_str}).
#'  
#'@param a_cov_prior_sd_str Specify priors for the covariate(s) included in the
#'  random effect parameter, \code{a} when fitting a hierarchical model with
#'  three or more levels of hierarchy (default \code{NULL}). The approach is
#'  same as described earlier (see the \code{a_cov_prior_sd}).
#'
#'@param b_cov_prior_sd_str Specify priors for the covariate(s) included in the
#'  random effect parameter, \code{b} when fitting a hierarchical model with
#'  three or more levels of hierarchy (default \code{NULL}). The approach is
#'  same as described earlier (see the \code{a_cov_prior_sd_str}).
#'
#'@param c_cov_prior_sd_str Specify priors for the covariate(s) included in the
#'  random effect parameter, \code{c} when fitting a hierarchical model with
#'  three or more levels of hierarchy (default \code{NULL}). The approach is
#'  same as described earlier (see the \code{a_cov_prior_sd_str}).
#'
#'@param d_cov_prior_sd_str Specify priors for the covariate(s) included in the
#'  random effect parameter, \code{d} when fitting a hierarchical model with
#'  three or more levels of hierarchy (default \code{NULL}). The approach is
#'  same as described earlier (see the \code{a_cov_prior_sd_str}).
#'  
#'@param sigma_prior_beta Specify priors for the fixed effect distributional
#'  parameter, \code{sigma} (default \code{student_t(3, 0, 1.0, autoscale =
#'  FALSE)}). The approach is same as described earlier for the fixed effect
#'  parameter, \code{a} (See \code{a_prior_beta} for details).
#'
#'@param sigma_cov_prior_beta Specify priors for the covariate(s) included in
#'  the fixed effect distributional parameter, \code{sigma} (default
#'  \code{student_t(3, 0, 0.5, autoscale = FALSE)}). The approach is same as
#'  described earlier for the covariate(s) included the fixed effect parameter,
#'  \code{a} (see \code{a_cov_prior_beta} for details).
#'
#'@param sigma_prior_sd Specify priors for the random effect distributional
#'  parameter, \code{sigma} (default \code{student_t(3, 0, 0.25, autoscale =
#'  FALSE)}). The approach is same as described earlier the random effect
#'  parameter \code{a} (see \code{a_prior_sd} for details).
#'
#'@param sigma_cov_prior_sd Specify priors for the covariate(s) included in the
#'  random effect distributional parameter, \code{sigma} (default
#'  \code{student_t(3, 0, 0.15, autoscale = FALSE)}). The approach is same as
#'  described earlier for the covariate(s) included in the random effect
#'  parameter \code{a} (see \code{a_cov_prior_sd} for details).
#'  
#'@param sigma_prior_sd_str Specify priors for the the random effect
#'  distributional parameter, \code{sigma} when fitting a hierarchical model
#'  with three or more levels of hierarchy (default \code{NULL}). The approach
#'  is same as described earlier for the random effect parameter, \code{a} (See
#'  \code{a_prior_sd_str} for details).
#'
#'@param sigma_cov_prior_sd_str Specify priors for the covariate(s) included in
#'  the random effect distributional parameter, \code{sigma} when fitting a
#'  hierarchical model with three or more levels of hierarchy (default
#'  \code{NULL}). The approach is same as described earlier for the covariate(s)
#'  included in the random effect parameter, \code{a} (See
#'  \code{a_cov_prior_sd_str} for details).
#'
#'@param rsd_prior_sigma Specify priors for the residual standard deviation
#'  parameter \code{sigma} (default \code{exponential('ysd', autoscale =
#'  TRUE)}). Note that this argument is evaluated only when both
#'  \code{dpar_formula} and \code{sigma_formula} are \code{NULL}. For location
#'  scale based distributions, user can use specify standard deviation
#'  (\code{ysd}) or the median absolute deviation (\code{ymad}) as scale
#'  parameter.
#'
#'@param dpar_prior_sigma Specify priors for the fixed effect distributional
#'  parameter \code{sigma} (default \code{student_t(3, 0, 'ysd', autoscale =
#'  TRUE)}). The argument is evaluated only when
#'  \code{sigma_formula} is \code{NULL}.
#'
#'@param dpar_cov_prior_sigma Specify priors for the covariate(s) included in
#'  the fixed effect distributional parameter \code{sigma} (default
#'  \code{student_t(3, 0, 1.0, autoscale = FALSE)}). The argument is evaluated
#'  only when \code{sigma_formula} is \code{NULL}.
#'
#'@param autocor_prior_acor Specify priors for the autocorrelation parameters
#'  when fitting a model with the \code{'arma'}, \code{'ar'} or the \code{'ma'}
#'  autocorrelation structures (see \code{autocor_formula} for details). The
#'  only allowed distribution is \code{uniform} distribution bounded between -1
#'  and +1 (default \code{uniform(-1, 1, autoscale = FALSE)}). For the
#'  unstructured residual correlation structure, a separate argument
#'  \code{autocor_prior_unstr_acor} is used to specify the priors (see below).
#'  
#' @param autocor_prior_unstr_acor Specify priors for the autocorrelation
#'   parameters when fitting a model with the unstructured (\code{'un'})
#'   autocorrelation structure (see \code{autocor_formula} for details). The
#'   only allowed distribution is the \code{lkj} (default \code{lkj(1)}). See
#'   \code{gr_prior_cor} below for details on setting up the \code{lkj} prior.
#'
#'@param gr_prior_cor Specify priors for the correlation parameter(s) of
#'  group-level random effects (default \code{lkj(1)}). The only allowed
#'  distribution is \code{lkj} that is specified via a single parameter
#'  \code{eta} (see [brms::prior()] for details).
#'  
#'@param gr_prior_cor_str Specify priors for the correlation parameter(s) of
#'  group-level random effects when fitting a hierarchical model with three or
#'  more levels of hierarchy (default \code{lkj(1)}). The approach is same as
#'  described above (See \code{gr_prior_cor}).
#'
#'@param sigma_prior_cor Specify priors for the correlation parameter(s) of
#'  distributional random effects \code{sigma} (default \code{lkj(1)}). The only
#'  allowed distribution is \code{lkj} (see \code{gr_prior_cor} for details).
#'  Note that currently \code{brms::brm()} does not allow for setting different
#'  \code{lkj} priors for the group level and distributional random effects that
#'  share the same group identifier (\code{id}). Therefore, either create a copy
#'  of group identifier and use that but then this will not allow correlation
#'  parameter across group random effects and sigma.
#'  
#'@param sigma_prior_cor_str Specify priors for the correlation parameter(s) of
#'  distributional random effects \code{sigma} when fitting a hierarchical model
#'  with three or more levels of hierarchy (default \code{lkj(1)}). The approach
#'  is same as described above (See \code{sigma_prior_cor}).
#'
#'@param mvr_prior_rescor Specify priors for the residual correlation parameter
#'  when fitting a multivariate model (default \code{lkj(1)}). The only allowed
#'  distribution is \code{lkj} (see \code{gr_prior_cor} for details).
#'
#'@param init Initial values for the sampler. If \code{init = '0'}, all
#'  parameters are initialized to zero. For \code{init = 'random'},
#'  \strong{Stan} will randomly generate initial values for each parameter
#'  within a range specified by the \code{init_r} (see below), or between -2 and
#'  2 in unconstrained space when \code{init_r = NULL}. Another available option
#'  is \code{init = 'prior'} which sets initial values based on the prior
#'  specified for each parameter. Lastly, when \code{init = NULL} (default),
#'  initial value for each parameter is specified by the corresponding init
#'  arguments defined see below.
#'
#'@param init_r A positive real value to set range for the random generation of 
#'  initial values (default \code{NULL}). This argument is evaluated only when
#'  \code{init = 'random'}.
#'
#'@param a_init_beta Initial values for the fixed effect parameter, \code{a}
#'  (default 'lm'). Options available are \code{'0'}, \code{'random'} and
#'  \code{'prior'}. In addition, user can specify \code{'ymean'} and
#'  \code{'ymedian'} to set initial as the mean or the median of the response
#'  variable. Also, option \code{'lm'} can be used to set coefficients obtained
#'  from the simple linear model fitted to the data as initial values for the
#'  fixed effect parameter, \code{a}. Note that this is similar to the location
#'  parameter for prior on the fixed effect parameter \code{a} (see
#'  \code{a_prior_beta} for details). These options (\code{'ymean'},
#'  \code{'ymedian'}, and \code{'lm'}) are available only for the fixed effect
#'  parameter \code{a} and not for other parameters described below. Lastly, For
#'  \code{univariate_by} and \code{multivariate} models, the initials can be
#'  same (e.g., \code{a_init_beta = 0}) for sub models or different for each sub
#'  model such as \cr \code{list(a_init_beta = '0', a_init_beta = 'lm')}.
#'
#'@param b_init_beta Initial values for the fixed effect parameter, \code{b}
#'  (default '0'). See \code{a_init_beta} for details.
#'
#'@param c_init_beta Initial values for the fixed effect parameter, \code{c}
#'  (default '0'). See \code{a_init_beta} for details.
#'
#'@param d_init_beta Initial values for the fixed effect parameter, \code{d}
#'  (default '0'). See \code{a_init_beta} for details.
#'  
#'@param s_init_beta  Initial values for the fixed effect parameter, \code{s}
#'  (default 'lm'). Options available are \code{'0'}, \code{'random'},
#'  \code{'prior'}, and \code{'lm'}.
#'
#'@param a_cov_init_beta Initial values for the covariate(s) included in the
#'  fixed effect parameter, \code{a} (default '0'). Options available are
#'  \code{'0'}, \code{'random'}, \code{'prior'} and \code{'lm'}. The option
#'  \code{'lm'} is available only for the \code{a_cov_init_beta} and not for the
#'  covariate(s) included in other fixed effect parameters \code{b}, \code{c},
#'  or \code{d}.
#'
#'@param b_cov_init_beta Initial values for covariate(s) included in the fixed
#'  effect parameter, \code{b} (default '0'). See \code{a_cov_init_beta} for
#'  details.
#'
#'@param c_cov_init_beta Initial values for covariate(s) included in the fixed
#'  effect parameter, \code{c} (default '0'). See \code{a_cov_init_beta} for
#'  details.
#'
#'@param d_cov_init_beta Initial values for covariate(s) included in the fixed
#'  effect parameter, \code{d} (default '0'). See \code{a_cov_init_beta} for
#'  details.
#'  
#'@param s_cov_init_beta Initial values for covariate(s) included in the fixed
#'  effect parameter, \code{s} (default 'lm'). See \code{a_cov_init_beta} for
#'  details. The option \code{'lm'} will set the spline coefficients obtained
#'  from the simple linear model fitted to the data. Note that
#'  \code{s_cov_init_beta} is only a placeholder and is not valuated because
#'  covariate(s) are not allowed for the \code{s} parameter. See
#'  \code{s_formula} for details.
#'
#'@param a_init_sd Initial value for the standard deviation of group level
#'  random effect parameter, \code{a} (default 'random'). Options available are
#'  \code{'0'}, \code{'random'} and \code{'prior'}. In addition, \code{'ysd'},
#'  \code{'ymad'}, \code{'lme_sd_a'}, and \code{'lm_sd_a'} can be used to
#'  specify initial values as described below:
#'  \itemize{
#'  \item The \code{'ysd'} sets standard deviation (\code{sd}) of the response
#'  variable as an initial value.
#'  \item The \code{'ymad'} sets median absolute deviation (\code{mad}) of the
#'  response variable as an initial value.
#'  \item The \code{'lme_sd_a'} sets initial value based on the standard 
#'  deviation of random Intercept obtained from the linear mixed model 
#'  (\code{nlme::lme()}) fitted to the data. Note that in case 
#'  \code{nlme::lme()} fails to converge, the option \code{'lm_sd_a'} 
#'  (see below) is set automatically.
#'  \item The \code{'lm_sd_a'} sets square root of the residual variance  
#'  obtained from the simple linear model applied to the data as an initial 
#'  value.
#'  }
#'  Note that these option described above (\code{'ysd'}, \code{'ymad'},
#'  \code{'lme_sd_a'}, and \code{'lm_sd_a'}) are available only for the random
#'  effect parameter \code{a} and not for other group level random effects.
#'  Lastly, when fitting \code{univariate_by} and \code{multivariate} models,
#'  user can set same initials for sub models, or different for each sub model.
#'
#'@param b_init_sd Initial value for the standard deviation of group level
#'  random effect parameter, \code{b} (default 'random'). See \code{a_init_sd}
#'  for details.
#'
#'@param c_init_sd Initial values for the group level random effect parameter,
#'  \code{c} (default 'random'). See \code{a_init_sd} for details.
#'
#'@param d_init_sd Initial value for the standard deviation of group level
#'  random effect parameter, \code{d} (default 'random'). See \code{a_init_sd}
#'  for details.
#'
#'@param a_cov_init_sd Initial values for the covariate(s) included in the
#'  random effect parameter, \code{a} (default 'random'). Options available are
#'  \code{'0'}, \code{'random'} and \code{'prior'}.
#'  
#'@param b_cov_init_sd Initial values for the covariate(s) included in the
#'  random effect parameter, \code{b} (default 'random'). See
#'  \code{a_cov_init_sd} for details.
#'
#'@param c_cov_init_sd Initial values for the covariate(s) included in the
#'  random effect parameter, \code{c} (default 'random'). See
#'  \code{a_cov_init_sd} for details.
#'
#'@param d_cov_init_sd Initial values for the covariate(s) included in the
#'  random effect parameter, \code{d} (default 'random'). See
#'  \code{a_cov_init_sd} for details.
#'
#'@param sigma_init_beta Initial values for the fixed effect distributional
#'  parameter, \code{sigma} (default 'random'). Options available are
#'  \code{'0'}, \code{'random'} and \code{'prior'}.
#'
#'@param sigma_cov_init_beta Initial values for the covariate(s)
#'  included in the fixed effect distributional parameter, \code{sigma} (See
#'  \code{sigma_init_beta} for details).
#'
#'@param sigma_init_sd Initial value for the standard deviation of
#'  distributional random effect parameter, \code{sigma} (default 'random'). The
#'  approach is same as described earlier for the group level random effect
#'  parameters such as \code{a} (See \code{a_init_sd} for details).
#'
#'@param sigma_cov_init_sd Initial values for the covariate(s) included in the
#'  distributional random effect parameter, \code{sigma} (default 'random').
#'  (See \code{a_cov_init_sd} for details).
#'
#'@param gr_init_cor Initial values for the correlation parameters of
#'  group-level random effects parameters (default 'random'). Allowed options
#'  are \code{'0'}, \code{'random'} and \code{'prior'}.
#'
#'@param sigma_init_cor Initial values for the correlation parameters of
#'  distributional random effects parameter \code{sigma} (default 'random').
#'  Allowed options are \code{'0'}, \code{'random'} and \code{'prior'}.
#'
#'@param rsd_init_sigma Initial values for the residual standard deviation
#'  parameter, \code{sigma} (default 'random'). Options available are
#'  \code{'0'}, \code{'random'} and \code{'prior'}. In addition, options
#'  \code{'lme_rsd'} and \code{'lm_rsd'} can be used as follows. The
#'  \code{lme_rsd} sets initial value based on the standard deviation of
#'  residuals obtained from the linear mixed model (\code{nlme::lme()}) fitted
#'  to the data. The initial value set by the \code{'lm_rsd'} is the square root
#'  of the residual variance from the simple linear model applied to the data.
#'  Note that in case \code{nlme::lme()} fails to converge, then option
#'  \code{'lm_sd_a'} is set automatically. The argument \code{rsd_init_sigma} is
#'  evaluated when \code{dpar_formula} and \code{sigma_formula} are set to
#'  \code{NULL}.
#'
#'@param dpar_init_sigma Initial values for the distributional parameter
#'  \code{sigma} (default 'random'). The approach and options available are same
#'  as described above for the \code{rsd_init_sigma}. This argument is evaluated
#'  only when \code{dpar_formula} is not \code{NULL}.
#'
#'@param dpar_cov_init_sigma Initial values for the covariate(s) included in the
#'  distributional parameter, \code{sigma} (default 'random'). Allowed options
#'  are \code{'0'}, \code{'random'}, and \code{'prior'}.
#'
#'@param autocor_init_acor Initial values for autocorrelation parameter (see
#'  \code{autocor_formula} for details). Allowed options are \code{'0'},
#'  \code{'random'}, and \code{'prior'} (default 'random').
#'
#'@param autocor_init_unstr_acor Initial values for unstructured residual
#'  autocorrelation parameters (default 'random'). Allowed options are
#'  \code{'0'}, \code{'random'}, and \code{'prior'}. Note that the approach to
#'  set initials for \code{autocor_init_unstr_acor} is identical to the
#'  \code{gr_init_cor}.
#'
#'@param mvr_init_rescor Initial values for the residual correlation parameter
#'  when fitting a \code{multivariate} model (default 'random'). Allowed options
#'  are \code{'0'}, \code{'random'}, and \code{'prior'}.
#'
#'@param r_init_z Initial values for the standardized group level random effect
#'  parameters (default 'random'). These parameters are part of the Non-Centered
#'  Parameterization (NCP) approach used in the [brms::brm()].
#'  
#'@param vcov_init_0 A logical (default \code{TRUE}) to set initials for
#'  variance (i.e, standard deviation) and covariance (i.e., correlation)
#'  parameters as zero. This allows for setting custom initials for the fixed
#'  effects parameters but zero for variance covariance parameters.
#'
#'@param jitter_init_beta A value as proportion (between 0 and 1) to perturb the
#'  initial values for fixed effect parameters. The default is \code{NULL}
#'  indicating that same initials are used across all chains. A sensible option
#'  can be \code{jitter_init_beta = 0.1} as it mildly perturb the initials. Note
#'  that jitter is not absolute but proportion of the specified initial value.
#'  For example, if initial value is \code{100}, then \code{jitter_init_beta =
#'  0.1} implies that the perturbed initial value will be within \code{90} and
#'  \code{110}. On the other hand, if initial values is \code{10}, then the
#'  perturbed initial value will be within \code{9} and \code{11}.
#'
#'@param jitter_init_sd A value as proportion (between 0 and 1) to perturb
#'  the initials for standard deviation of random effect parameters. The default
#'  is \code{NULL} indicating that same initials are used across all chains.
#'  An option of setting \code{jitter_init_beta = 0.01} looked good during early
#'  testing.
#'
#'@param jitter_init_cor A value as proportion (between 0 and 1) to perturb the
#'  initials for correlation parameters of random effects. The default is
#'  \code{NULL} indicating that same initials are used across all chains. An
#'  option of setting \code{jitter_init_beta = 0.001} looked good during early
#'  testing.
#'
#'@param prior_data An optional argument (a named list, default \code{NULL})
#'  that can be used to pass information to the prior arguments for each
#'  parameter (e.g., \code{a_prior_beta}). The \code{prior_data} is particularly
#'  helpful in passing a long vector or a matrix as priors. These vectors and
#'  matrices can be created in the R framework and then passed using the
#'  \code{prior_data}. For example, to pass a vector of location and scale
#'  parameters when setting priors for covariate coefficients (with 10 dummy
#'  variables) included in the fixed effects parameter \code{a}, the following
#'  steps can be used to set covariate priors that each has scale parameter
#'  (\code{sd}) as 5 but mean values are drawn from a normal distribution with
#'  \code{mean = 0} and \code{sd = 1}:
#'  \itemize{
#'  \item create the named objects \code{prior_a_cov_location} and
#'  \code{prior_a_cov_scale} in the R environment as follows: \cr
#'  \code{prior_a_cov_location <- rnorm(n = 10, mean = 0, sd = 1)} \cr
#'  \code{prior_a_cov_scale <- rep(5, 10)} \cr
#'  \item specify the above created objects \code{prior_a_cov_location} and 
#'  \code{prior_a_cov_scale} in the \code{prior_data} as follows: \cr
#'  \code{prior_data = list(prior_a_cov_location = prior_a_cov_location, 
#'  prior_a_cov_scale = prior_a_cov_scale)}.
#'  \item now use the \code{prior_data} objects to set up the priors as: \cr
#'  \code{a_cov_prior_beta = normal(prior_a_cov_location, prior_a_cov_scale)}.
#'  }
#'
#'@param init_data An optional argument (a named list, default \code{NULL}) that
#'  can be used to pass information to the initial arguments. The approach is
#'  the exact same as described above for the \code{prior_data}.
#'  
#'@param init_custom Specify a custom initials object (a named list). Note that
#'  the named list is directly passed to the \code{init} argument without
#'  checking for the dimensions and name matching.
#'
#'@param verbose An optional argument (logical, default \code{FALSE}) to
#'  indicate whether to print information collected during setting up the model
#'  formula priors, and initials. As an example, the user might be interested in
#'  knowing the response variables created for the sub model when fitting a
#'  univariate-by-subgroup model. This information can then be used in setting
#'  the desired order of options passed to each such model such as \code{df},
#'  \code{prior}, \code{initials} etc.
#'
#'@param expose_function An optional argument (logical, default \code{FALSE}) to
#'  indicate whether to expose Stan function used in model fitting.
#'  
#'@param get_stancode An optional argument (logical, default \code{FALSE}) to
#'  get the stancode (see [brms::stancode()] for details).
#'
#'@param get_standata An optional argument (logical, default \code{FALSE}) to
#'  get the standata (see [brms::standata()] for details).
#'
#'@param get_formula An optional argument (logical, default \code{FALSE}) to
#'  get the formula. (see [brms::brmsformula()] for details).
#'
#'@param get_stanvars An optional argument (logical, default \code{FALSE}) to
#'  get the stanvars (see [brms::stanvar()] for details).
#'
#'@param get_priors An optional argument (logical, default \code{FALSE}) to
#'  get the priors. (see [brms::get_prior()] for details).
#'
#'@param get_priors_eval An optional argument (logical, default \code{FALSE}) to
#'  get the priors specified by the user.
#'
#'@param get_init_eval An optional argument (logical, default \code{FALSE}) to
#'  get the initial values specified by the user.
#' 
#'@param validate_priors An optional argument (logical, default \code{FALSE}) to
#'  validate the specified priors. (see [brms::validate_prior()] for details).
#'
#'@param set_self_priors An optional argument (default \code{NULL}) to manually
#'  specify the priors. Note that \code{set_self_priors} is passed directly to
#'  the [brms::brm()] without performing any checks.
#'  
#'@param set_replace_priors An optional argument (default \code{NULL}) to
#'  replace part of prior object. This is for internal use only.
#' 
#'@param set_same_priors_hierarchy An optional argument (default \code{NULL}) to
#'  replace part of the prior object. This is for internal use only.
#'  
#'@param outliers An optional argument (default \code{NULL}) to remove outliers.
#'  The argument should be a named list which is passed directly to the
#'  [sitar::velout()] and [sitar::zapvelout()] functions. This is for internal
#'  use only.
#'
#'@param unused An optional formula that defines variables that are unused in
#'  the model but should still be stored in the model's data frame. This can be
#'  useful when variables are required during the post-processing.
#'
#'@param chains Number of Markov chains (default 4).
#'
#'@param iter Number of total iterations per chain, including warmup (default
#'  2000)
#'
#'@param warmup A positive integer specifying the number of warmup (aka burnin)
#'  iterations. This also specifies the number of iterations used for stepsize
#'  adaptation, so warmup draws should not be used for inference. The number of
#'  warmup should not be larger than \code{iter} and the default is
#'  \code{iter/2}.
#'
#'@param thin A positive integer. Set \code{thin > 1} to save memory and
#'  computation time if \code{iter} is large. The \code{thin > 1} is often used
#'  in cases with high autocorrelation of MCMC draws An indication of high
#'  autocorrelation is poor mixing of chain ( i.e., high \code{rhat} values)
#'  despite the fact that model recovers the parameters well. An easy diagnostic
#'  to check for autocorrelation of MCMC draws is to use the \code{mcmc_acf}
#'  function from the \pkg{bayesplot}.
#'
#'@param cores Number of cores to be used when executing the chains in parallel.
#'  See [brms::brm()] for details. Note that unlike [brms::brm()], which sets
#'  default \code{cores} argument as \code{cores=getOption("mc.cores", 1)}, the
#'  default \code{cores} in \pkg{bsitar} package is
#'  \code{cores=getOption("mc.cores", 'optimize')} which optimizes the
#'  utilization of system resources. The maximum number of cores that can be
#'  deployed is calculated as the maximum number of available cores minus 1.
#'  When the number of available cores is greater than the number of chains (see
#'  \code{chains}), then number of cores is set equal to the number of chains.
#'  Another option is to set \code{cores} as \code{getOption("mc.cores",
#'  'maximise')} which sets the number of cores as the maximum number of cores
#'  available from the system regardless of the number of chains specified. Note
#'  that the user can also set \code{cores} argument similar to the
#'  [brms::brm()] i.e., \code{getOption("mc.cores", 1)}. All these three options
#'  can be set globally as \code{options(mc.cores = x}) where x can be
#'  \code{'optimize'}, \code{'maximise'} or \code{1}. Lastly, the \code{cores}
#'  can set by directly by specifying an integer e.g., \code{cores = 4}.
#'
#'@param backend A character string naming the package to be used when executing
#'  the the Stan model. Options are \code{"rstan"} (the default) or
#'  \code{"cmdstanr"}. Can be set globally for the current \R session via the
#'  \code{"brms.backend"}. See [brms::brm()] for details.
#'
#'@param threads Number of threads to be used in within-chain parallelization.
#'  Note that unlike the [brms::brm()] which sets the \code{threads} argument as
#'  \code{getOption("brms.threads", NULL)} implying that no within-chain
#'  parallelization is used by default, the \pkg{bsitar} package, by default,
#'  sets \code{threads} as \code{getOption("brms.threads", 'optimize')} to
#'  utilize the available resources from the modern computing systems. The
#'  number of threads per chain is set as the maximum number of cores available
#'  minus 1. Another option is to set \code{threads} as
#'  \code{getOption("brms.threads", 'maximise')} which set the number threads
#'  per chains same as the  maximum number of cores available. User can also set
#'  the \code{threads} similar to the \code{brms} i.e.,
#'  \code{getOption("brms.threads", NULL)}. All these three options can be set
#'  globally as \code{options(brms.threads = x}) where x can be
#'  \code{'optimize'}, \code{'maximise'} or \code{NULL}.
#'  Alternatively, the number of threads can be set directly as \code{threads
#'  = threading(x)} where \code{X} is an integer. Other arguments that can be
#'  passed to the \code{threads} are \code{grainsize} and the \code{static}. See
#'  [brms::brm()] for further details on within-chain parallelization.
#'
#'@param opencl The platform and device IDs of the OpenCL device to use for
#'  fitting using GPU support. If you don't know the IDs of your OpenCL device,
#'  \code{c(0,0)} is most likely what you need. For more details, see
#'  \code{\link{opencl}}. Can be set globally for the current \R session via the
#'  \code{"brms.opencl"} option.
#'
#'@param normalize Indicates whether normalization constants should be included
#'  in the Stan code (default \code{TRUE}). Setting it to \code{FALSE} requires
#'  Stan version >= 2.25. If \code{FALSE}, sampling efficiency may be increased
#'  but some post processing functions such as [brms::bridge_sampler()] will not
#'  be available. This option can be controlled globally via the
#'  \code{brms.normalize} option.
#'
#'@param algorithm Character string naming the estimation approach to use.
#'  Options are \code{"sampling"} for MCMC (the default), \code{"meanfield"} for
#'  variational inference with independent normal distributions,
#'  \code{"fullrank"} for variational inference with a multivariate normal
#'  distribution, or \code{"fixed_param"} for sampling from fixed parameter
#'  values. Can be set globally for the current \R session via the
#'  \code{"brms.algorithm"} option (see \code{\link{options}}).
#'
#'@param control A named \code{list} to control the sampler's behavior. The
#'  default are same as [brms::brm()] with the exception that the
#'  \code{max_treedepth} has been increased form 10 to 12 to allow better
#'  exploration of typically challenging posterior geometry posed by the
#'  nonlinear model. However, another control parameter, the \code{adpat_delta}
#'  which is also  often need to be increased for nonlinear model, has be set to
#'  default setting as in [brms::brm()] i.e, 0.8. This is to avoid unnecessarily
#'  increasing the sampling time. See [brms::brm()] for full details on control
#'  parameters and their default values.
#'
#'@param sample_prior Indicates whether to draw sample from priors in addition
#'  to the posterior draws. Options are \code{"no"} (the default), \code{"yes"},
#'  and \code{"only"}. Among others, these draws can be used to calculate Bayes
#'  factors for point hypotheses via [brms::hypothesis()]. Please note that
#'  improper priors are not sampled, including the default improper priors used
#'  by \code{brm}. See [brms::set_prior()] on how to set (proper) priors. Please
#'  also note that prior draws for the overall intercept are not obtained by
#'  default for technical reasons. See [brms::brmsformula()] how to obtain prior
#'  draws for the intercept. If \code{sample_prior} is set to \code{"only"},
#'  draws are drawn solely from the priors ignoring the likelihood, which allows
#'  among others to generate draws from the prior predictive distribution. In
#'  this case, all parameters must have proper priors.
#'
#'@param save_pars An object generated by \code{\link{save_pars}} controlling
#'  which parameters should be saved in the model. The argument has no impact on
#'  the model fitting itself.
#'
#'@param drop_unused_levels Should unused factors levels in the data be dropped?
#'  Defaults to \code{TRUE}.
#'
#'@param stan_model_args A \code{list} of further arguments passed to
#'   \code{\link[rstan:stan_model]{rstan::stan_model}} for \code{backend =
#'   "rstan"} or \code{backend = "cmdstanr"}, which allows to change how
#'  models are compiled.
#'
#'@param refresh An integer to set the printing of every nth iteration. Default
#'  \code{NULL} indicates that refresh will be set automatically by the
#'  [brms::brm()]. Setting  \code{refresh} is useful especially when \code{thin}
#'  is greater than \code{1}. In that case, the \code{refresh} is recalculated
#'  as (\code{refresh} * \code{thin}) / \code{thin}.
#'
#'@param silent Verbosity level between \code{0} and \code{2}. If \code{1} (the
#'  default), most of the informational messages of compiler and sampler are
#'  suppressed. If \code{2}, even more messages are suppressed. The actual
#'  sampling progress is still printed. Set \code{refresh = 0} to turn this off
#'  as well. If using \code{backend = "rstan"} you can also set
#'  \code{open_progress = FALSE} to prevent opening additional progress bars.
#'
#'@param seed The seed for random number generation to make results
#'  reproducible. If \code{NA} (the default), \pkg{Stan} will set the seed
#'  randomly.
#'
#'@param save_model A character string or \code{NULL} (default). If not
#'  \code{NULL}, then the model's Stan code is saved via in a text file named
#'  after the string supplied in \code{save_model}.
#'
#'@param fit An instance of S3 class \code{brmsfit} derived from a previous fit;
#'  defaults to \code{NA}. If \code{fit} is of class \code{brmsfit}, the
#'  compiled model associated with the fitted result is re-used and all
#'  arguments modifying the model code or data are ignored. It is not
#'  recommended to use this argument directly, but to call the
#'  \code{\link[brms:update.brmsfit]{update}} method, instead.
#'
#'@param file Either \code{NULL} or a character string. In the latter case, the
#'  fitted model object is saved via \code{\link{saveRDS}} in a file named after
#'  the string supplied in \code{file}. The \code{.rds} extension is added
#'  automatically. If the file already exists, \code{brm} will load and return
#'  the saved model object instead of refitting the model. Unless you specify
#'  the \code{file_refit} argument as well, the existing files won't be
#'  overwritten, you have to manually remove the file in order to refit and save
#'  the model under an existing file name. The file name is stored in the
#'  \code{brmsfit} object for later usage.
#'
#'@param file_refit Modifies when the fit stored via the \code{file} argument is
#'  re-used. Can be set globally for the current \R session via the
#'  \code{"brms.file_refit"} option (see \code{\link{options}}). For
#'  \code{"never"} (default) the fit is always loaded if it exists and fitting
#'  is skipped. For \code{"always"} the model is always refitted. If set to
#'  \code{"on_change"}, brms will refit the model if model, data or algorithm as
#'  passed to Stan differ from what is stored in the file. This also covers
#'  changes in priors, \code{sample_prior}, \code{stanvars}, covariance
#'  structure, etc. If you believe there was a false positive, you can use
#'  \code{\link{brmsfit_needs_refit}} to see why refit is deemed necessary.
#'  Refit will not be triggered for changes in additional parameters of the fit
#'  (e.g., initial values, number of iterations, control arguments, ...). A
#'  known limitation is that a refit will be triggered if within-chain
#'  parallelization is switched on/off.
#'
#'@param future Logical; If \code{TRUE}, the \pkg{\link[future:future]{future}}
#'  package is used for parallel execution of the chains and argument
#'  \code{cores} will be ignored. Can be set globally for the current \R session
#'  via the \code{"future"} option. The execution type is controlled via
#'  \code{\link[future:plan]{plan}} (see the examples section below).
#'
#'@param parameterization A character string to specify Non-centered
#'  parameterization, NCP (\code{'ncp'}) or the Centered parameterization, CP
#'  (\code{'cp'}) to draw group level random effect. The NCP is generally
#'  recommended when likelihood is not strong (e.g., a few number of
#'  observations per individual). The NCP is the default (and only) approach
#'  implemented in the [brms::brm()]. The CP parameterization, on the other
#'  hand, is often considered more efficient than NCP when a relatively large
#'  number of observations are available across individual. The 'relatively
#'  large number' is not defined in the literature and we follow a general
#'  approach wherein CP parameterization is used when each individual provides
#'  at least 10 repeated measurements and NCP otherwise. Note this automatic
#'  behavior is set only when the argument \code{parameterization = NULL}. To
#'  set CP parameterization, use \code{parameterization = 'cp'}. The default is
#'  \code{parameterization = 'ncp'}. Note that since [brms::brm()] does not
#'  offer CP parameterization, the [brms::brm()] generated \code{stancode} is
#'  first edited internally and then the model is fit using the [rstan::rstan()]
#'  or \code{cmdstanr}, depending on the \code{backend} choice. Therefore, we
#'  caution that CP parameterization should be considered experimental and it
#'  may fail if structure of the [brms::brm()] generated \code{stancode} changes
#'  in future.
#'   
#'@param ... Further arguments passed to [brms::brm()]
#'
#'@return An object of class \code{brmsfit, bsiatr}, that contains the posterior
#'  draws and other useful information about the model.
#'
#'@export
#'
#'@importFrom methods formalArgs
#'
#'@importFrom stats as.formula coef df dist filter fitted gaussian lm mad median
#'  model.matrix predict quantile rbeta sd setNames smooth.spline rnorm runif
#'  rcauchy rexp rlnorm rgamma rlnorm loess na.omit residuals complete.cases
#'  deriv formula update
#' 
#'@importFrom rlang .data
#'
#'@importFrom utils combn head installed.packages packageVersion tail data
#'
#'@importFrom Rdpack reprompt
#'
#'@import brms
#'
#'@note The package is under continuous development and new models and 
#'  post-processing features will be added soon.
#'
#' @seealso [brms::brm()] [brms::brmsformula()] [brms::prior()]
#'
#' @references
#'  \insertAllCited{}
#'  
#' @inherit berkeley author
#' 
#' @examples
#' 
#' # Examples below fits the SITAR model to the Berkley height data for males. 
#' # See help file (?berkeley_mdata) for details on berkeley_mdata dataset.
#'   
#' # Fit maximum likelihood (frequentist) SITAR model with df = 3 by using 
#' # the sitar package 
#' 
#' model_ml <- sitar::sitar(x = age, y = height, id = id, 
#'                           df = 3, 
#'                           data = berkeley_mdata, 
#'                           xoffset = 'mean',
#'                           fixed = 'a+b+c', 
#'                           random = 'a+b+c',
#'                           a.formula = ~1, 
#'                           b.formula = ~1, 
#'                           c.formula = ~1
#'                           )
#' 
#' 
#' # Fit Bayesian SITAR model 
#' 
#' # To save time and memory, the model is fit using 2 chain and thin set as 15.
#' # To get sufficient draws after thinning, the number of iterations are 
#' # increased from 2000 per chain (default) to 6000 iteration per chain.
#' # Note that fitting model with these setting still taken a while. 
#' 
#' # To avoid mode estimation which takes time, a model fitted to the 
#' # 'berkeley_mdata' has already been saved as 'berkeley_mfit'. 
#' 
#' if(exists('berkeley_mfit')) {
#'   model <- berkeley_mfit
#' } else {
#'  # Fit model with default priors 
#'  # See documentation for prior on each parameter
#'   model <- bsitar(x = age, y = height, id = id, 
#'                   df = 3, 
#'                   data = berkeley_mdata,
#'                   xoffset = 'mean', 
#'                   fixed = 'a+b+c', 
#'                   random = 'a+b+c',
#'                   a_formula = ~1, 
#'                   b_formula = ~1, 
#'                   c_formula = ~1, 
#'                   threads = brms::threading(NULL),
#'                   chains = 2, cores = 2, iter = 6000, thin = 15)
#'                   
#' # Note that we can test for the sensitivity to the priors by re fitting the
#' # above model with flat (i.e., uniform) priors on the regression coefficients
#' # for parameters a, b and c.
#' model <- bsitar(x = age, y = height, id = id, 
#'                   df = 3, 
#'                   data = berkeley_mdata,
#'                   xoffset = 'mean', 
#'                   fixed = 'a+b+c', 
#'                   random = 'a+b+c',
#'                   a_formula = ~1, 
#'                   b_formula = ~1, 
#'                   c_formula = ~1, 
#'                   a_prior_beta = flat,
#'                   b_prior_beta = flat,
#'                   c_prior_beta = flat,
#'                   threads = brms::threading(NULL),
#'                   chains = 2, cores = 2, iter = 6000, thin = 15)
#' }
#' 
#' # Generate model summary
#' summary(model)
#' 
#' # Compare model summary with the maximum likelihood SITAR model
#' print(model_ml)
#' 
#' \donttest{
#' # Check model fit via posterior predictive checks. The plot_ppc is a based
#' # on the pp_check function from the brms package.  
#' 
#' plot_ppc(model, ndraws = 100)
#' 
#' # Plot distance and velocity curves using plot_conditional_effects() function.
#' # This function works exactly same as as conditional_effects() from the brms
#' # package with the exception that plot_conditional_effects allows for 
#' # plotting velocity curve also.
#' 
#' # Distance
#' plot_conditional_effects(model, deriv = 0)
#' 
#' # Velocity
#' plot_conditional_effects(model, deriv = 1)
#' 
#' # Plot distance and velocity curve along with the parameter estimates using 
#' # the plot_curves() function. This function works exactly the same way as 
#' # plot.sitar from the sitar package
#' 
#' plot_curves(model, apv = TRUE)
#' 
#' # Compare plot with the maximum likelihood SITAR model
#' 
#' plot(model_ml)
#' }
#' 
#'
bsitar <- function(x,
                   y,
                   id,
                   data,
                   df = 4,
                   knots = NA,
                   fixed = a + b + c,
                   random = a + b + c,
                   xoffset = mean,
                   bstart = xoffset,
                   cstart = 0,
                   xfun = NULL,
                   yfun = NULL,
                   bound = 0.04,
                   terms_rhs = NULL,
                   a_formula = ~ 1,
                   b_formula = ~ 1,
                   c_formula = ~ 1,
                   d_formula = ~ 1,
                   s_formula = ~ 1,
                   a_formula_gr = ~ 1,
                   b_formula_gr = ~ 1,
                   c_formula_gr = ~ 1,
                   d_formula_gr = ~ 1,
                   a_formula_gr_str = NULL,
                   b_formula_gr_str = NULL,
                   c_formula_gr_str = NULL,
                   d_formula_gr_str = NULL,
                   d_adjusted = FALSE,
                   sigma_formula = NULL,
                   sigma_formula_gr = NULL,
                   sigma_formula_gr_str = NULL,
                   dpar_formula = NULL,
                   autocor_formula = NULL,
                   family = gaussian(),
                   group_arg = list(
                     groupvar = NULL,
                     by = NULL,
                     cor = un,
                     cov = NULL,
                     dist = gaussian
                   ),
                   sigma_group_arg = list(
                     groupvar = NULL,
                     by = NULL,
                     cor = un,
                     cov = NULL,
                     dist = gaussian
                   ),
                   univariate_by = list(by = NA, cor = un, terms = subset),
                   multivariate = list(mvar = FALSE,
                                       cor = un,
                                       rescor = TRUE),
                   a_prior_beta = student_t(3, ymean, ysd, autoscale = TRUE),
                   b_prior_beta = student_t(3, 0, 3.5, autoscale = FALSE),
                   c_prior_beta = student_t(3, 0, 1.5, autoscale = FALSE),
                   d_prior_beta = student_t(3, 0, 1.0, autoscale = TRUE),
                   s_prior_beta = student_t(3, 0, lm, autoscale = TRUE),
                   a_cov_prior_beta = student_t(3, 0, 5.0, autoscale = FALSE),
                   b_cov_prior_beta = student_t(3, 0, 1.0, autoscale = FALSE),
                   c_cov_prior_beta = student_t(3, 0, 0.1, autoscale = FALSE),
                   d_cov_prior_beta = student_t(3, 0, 1.0, autoscale = FALSE),
                   s_cov_prior_beta = student_t(3, 0, 10.0, autoscale = FALSE),
                   a_prior_sd = student_t(3, 0, ysd, autoscale = TRUE),
                   b_prior_sd = student_t(3, 0, 2.0, autoscale = FALSE),
                   c_prior_sd = student_t(3, 0, 1.25, autoscale = FALSE),
                   d_prior_sd = student_t(3, 0, 1.0, autoscale = TRUE),
                   a_cov_prior_sd = student_t(3, 0, 5.0, autoscale = FALSE),
                   b_cov_prior_sd = student_t(3, 0, 1.0, autoscale = FALSE),
                   c_cov_prior_sd = student_t(3, 0, 0.1, autoscale = FALSE),
                   d_cov_prior_sd = student_t(3, 0, 1.0, autoscale = FALSE),
                   a_prior_sd_str = NULL,
                   b_prior_sd_str = NULL,
                   c_prior_sd_str = NULL,
                   d_prior_sd_str = NULL,
                   a_cov_prior_sd_str = NULL,
                   b_cov_prior_sd_str = NULL,
                   c_cov_prior_sd_str = NULL,
                   d_cov_prior_sd_str = NULL,
                   sigma_prior_beta = student_t(3, 0, 1, autoscale = FALSE),
                   sigma_cov_prior_beta = student_t(3, 0, 0.5, autoscale = FALSE),
                   sigma_prior_sd = student_t(3, 0, 0.25, autoscale = FALSE),
                   sigma_cov_prior_sd = student_t(3, 0, 0.15, autoscale = FALSE),
                   sigma_prior_sd_str = NULL,
                   sigma_cov_prior_sd_str = NULL,
                   rsd_prior_sigma = exponential(ysd, autoscale = TRUE),
                   dpar_prior_sigma = student_t(3, 0, ysd, autoscale = TRUE),
                   dpar_cov_prior_sigma = student_t(3, 0, 1, autoscale = FALSE),
                   autocor_prior_acor = uniform(-1, 1, autoscale = FALSE),
                   autocor_prior_unstr_acor = lkj(1),
                   gr_prior_cor = lkj(1),
                   gr_prior_cor_str = lkj(1),
                   sigma_prior_cor = lkj(1),
                   sigma_prior_cor_str = lkj(1),
                   mvr_prior_rescor = lkj(1),
                   init = NULL,
                   init_r = NULL,
                   a_init_beta = lm,
                   b_init_beta = 0,
                   c_init_beta = 0,
                   d_init_beta = 0,
                   s_init_beta = lm,
                   a_cov_init_beta = 0,
                   b_cov_init_beta = 0,
                   c_cov_init_beta = 0,
                   d_cov_init_beta = 0,
                   s_cov_init_beta = lm,
                   a_init_sd = random,
                   b_init_sd = random,
                   c_init_sd = random,
                   d_init_sd = random,
                   a_cov_init_sd = random,
                   b_cov_init_sd = random,
                   c_cov_init_sd = random,
                   d_cov_init_sd = random,
                   sigma_init_beta = random,
                   sigma_cov_init_beta = random,
                   sigma_init_sd = random,
                   sigma_cov_init_sd = random,
                   gr_init_cor = random,
                   sigma_init_cor = random,
                   rsd_init_sigma = random,
                   dpar_init_sigma = random,
                   dpar_cov_init_sigma = random,
                   autocor_init_acor = random,
                   autocor_init_unstr_acor = random,
                   mvr_init_rescor = random,
                   r_init_z = random,
                   vcov_init_0 = TRUE,
                   jitter_init_beta = NULL,
                   jitter_init_sd = NULL,
                   jitter_init_cor = NULL,
                   prior_data = NULL,
                   init_data = NULL,
                   init_custom = NULL,
                   verbose = FALSE,
                   expose_function = FALSE,
                   get_stancode = FALSE,
                   get_standata = FALSE,
                   get_formula = FALSE,
                   get_stanvars = FALSE,
                   get_priors = FALSE,
                   get_priors_eval = FALSE,
                   get_init_eval = FALSE,
                   validate_priors = FALSE,
                   set_self_priors = NULL,
                   set_replace_priors = NULL,
                   set_same_priors_hierarchy = FALSE,
                   outliers = NULL, 
                   unused = NULL,
                   chains = 4,
                   iter = 2000,
                   warmup = floor(iter / 2),
                   thin = 1,
                   cores = getOption("mc.cores", "optimize"),
                   backend = getOption("brms.backend", "rstan"),
                   threads = getOption("brms.threads", "optimize"),
                   opencl = getOption("brms.opencl", NULL),
                   normalize = getOption("brms.normalize", TRUE),
                   algorithm = getOption("brms.algorithm", "sampling"),
                   control = list(adapt_delta = 0.8, max_treedepth = 15),
                   sample_prior = "no",
                   save_pars = NULL,
                   drop_unused_levels = TRUE,
                   stan_model_args = list(),
                   refresh = NULL,
                   silent = 1,
                   seed = 123,
                   save_model = NULL,
                   fit = NA,
                   file = NULL,
                   file_refit = getOption("brms.file_refit", "never"),
                   future = getOption("future", FALSE),
                   parameterization = 'ncp',
                   ...) {
  
  mcall <- mcall_ <- match.call()
  
  # Check and set Alias argument for a b c ... formula
  dots_allias <- list(...)
  collect_dot_names <- c()
  for (ia in letters[1:26]) {
    set_name_dot <- paste0(ia, ".", 'formula')
    set_name_uns <- paste0(ia, "_", 'formula')
    collect_dot_names <- c(collect_dot_names, set_name_dot)
    if (set_name_dot %in% names(dots_allias)) {
      if (eval(bquote(missing(.(set_name_uns)))) ) { 
        mcall[[set_name_uns]] <- dots_allias[[set_name_dot]]
      } else if (!eval(bquote(missing(.(set_name_uns)))) ) { 
        err_msg <- paste0("both '", set_name_uns, "' and '" , 
                          set_name_dot, "' found, ignoring '",set_name_dot, "'")
        if(verbose) warning(err_msg)
      }
    }
  }
  
  for (collect_dot_namesi in collect_dot_names) {
    if(!is.null(mcall[[collect_dot_namesi]])) 
      mcall[[collect_dot_namesi]] <- NULL
  }
  
  # Check and set Alias argument for d_adjusted (SITAR)
  collect_dot_names <- c()
  for (ia in letters[4]) {
    set_name_dot <- paste0(ia, ".", 'adjusted')
    set_name_uns <- paste0(ia, "_", 'adjusted')
    collect_dot_names <- c(collect_dot_names, set_name_dot)
    if (set_name_dot %in% names(dots_allias)) {
      if (eval(bquote(missing(.(set_name_uns)))) ) { 
        mcall[[set_name_uns]] <- dots_allias[[set_name_dot]]
      } else if (!eval(bquote(missing(.(set_name_uns)))) ) { 
        err_msg <- paste0("both '", set_name_uns, "' and '" , 
                          set_name_dot, "' found, ignoring '",set_name_dot, "'")
        if(verbose) warning(err_msg)
      }
    }
  }
  
  for (collect_dot_namesi in collect_dot_names) {
    if(!is.null(mcall[[collect_dot_namesi]])) 
      mcall[[collect_dot_namesi]] <- NULL
  }
  
  # Clear alias argument for formula and adjusted
  rm(dots_allias)
  
  
  mcall <- mcall_ <- mcall
  
  no_default_args <- c("x", "y", "id", "data", "...")
  
  # Problem with rethinking occurs during the expose_model_function
  if("rethinking" %in% (.packages())){
    message("Package 'rethinking' detached and unloaded as it creates conflict",
            " \nwith the rstan version ", utils::packageVersion('rstan'))
    detach("package:rethinking", unload=TRUE) 
  }
 
  if(utils::packageVersion('rstan') < 2.26) {
    if(expose_function) stop("Argument 'expose_function' not allowed ",
                             "for this rstan version ",
                             utils::packageVersion('rstan'))
  }
  
  
 quote_random_as_init_arg <- function(temp_init_call_in, mcall,...) {
   if(is.null(temp_init_call_in)) temp_init_call_c <- temp_init_call_in
   if(is.symbol(temp_init_call_in) | is.numeric(temp_init_call_in)) {
     if(!is.character(temp_init_call_in)) {
       temp_init_call_c <- deparse(temp_init_call_in)
     } else {
       temp_init_call_c <- temp_init_call_in
     }
   } else if(is.character(temp_init_call_in)) {
     temp_init_call_c <- temp_init_call_in
   }
   
   if(is.language(temp_init_call_in)) {
     temp_init_call <- deparse(temp_init_call_in)
     temp_init_call <- gsub("[[:space:]]", "", temp_init_call)
     temp_init_call <- regmatches(temp_init_call, 
                                  gregexpr("(?<=\\().*?(?=\\))", 
                                           temp_init_call, perl=T))[[1]]
     if(length(temp_init_call) != 0) {
       temp_init_call <- strsplit(temp_init_call, ",")[[1]]
       temp_init_call_c <- c()
       for (temp_init_calli in temp_init_call) {
         if(!grepl("\"", temp_init_calli)) {
           temp_init_call2 <- deparse(temp_init_calli)
         } else {
           temp_init_call2 <- temp_init_calli
         }
         temp_init_call_c <- c(temp_init_call_c, temp_init_call2)
       }
       temp_init_call_c <- gsub("[[:space:]]", "", temp_init_call_c)
       temp_init_call_c <- paste0("list(", 
                                  paste(temp_init_call_c, collapse = ",") , ")")
       temp_init_call_c <- str2lang(temp_init_call_c)
     } else if(is.symbol(temp_init_call_in)) { 
       temp_init_call_c <- deparse(substitute(temp_init_call_c))
       temp_init_call_c <- gsub("\"" , "", temp_init_call_c)
     } else {
       temp_init_call_c <- mcall$init
     }
   } 
   temp_init_call_c
 } # quote_random_as_init_arg
  
 
 
  mcall$init <- quote_random_as_init_arg(mcall$init, mcall)
  
  for (inxc in letters[1:26]) {
    what_inxc <- paste0(inxc, "_", "init", "_", "beta", "")
    if(!is.null(mcall[[what_inxc]])) mcall[[what_inxc]] <- 
        quote_random_as_init_arg(mcall[[what_inxc]], mcall)
    what_inxc <- paste0(inxc, "_", "cov", "_", "init", "_", "beta", "")
    if(!is.null(mcall[[what_inxc]])) mcall[[what_inxc]] <- 
      quote_random_as_init_arg(mcall[[what_inxc]], mcall)
    what_inxc <- paste0(inxc, "_", "init", "_", "sd", "")
    if(!is.null(mcall[[what_inxc]])) mcall[[what_inxc]] <- 
      quote_random_as_init_arg(mcall[[what_inxc]], mcall)
    what_inxc <- paste0(inxc, "_", "cov", "_", "init", "_", "sd", "")
    if(!is.null(mcall[[what_inxc]])) mcall[[what_inxc]] <- 
      quote_random_as_init_arg(mcall[[what_inxc]], mcall)
  }
  
  what_inxc <- paste0('sigma', "_", "init", "_", "beta", "")
  if(!is.null(mcall[[what_inxc]])) mcall[[what_inxc]] <- 
    quote_random_as_init_arg(mcall[[what_inxc]], mcall)
  
  what_inxc <- paste0('sigma', "_", "cov", "_", "init", "_", "beta", "")
  if(!is.null(mcall[[what_inxc]])) mcall[[what_inxc]] <- 
    quote_random_as_init_arg(mcall[[what_inxc]], mcall)
 
  
  # Initiate non formalArgs()
  a <- b <- c <- d <- e <- f <- g <- h <- i <- NULL;
  sitar <- NULL;
  mean <- NULL;
  xoffset <- NULL;
  gaussian <- NULL;
  un <- NULL;
  normal <- NULL;
  student_t <- NULL;
  cauchy <- NULL;
  lognormal <- NULL;
  uniform <- NULL;
  exponential <- NULL;
  gamma <- NULL;
  inv_gamma <- NULL;
  lkj <- NULL;
  idsi <- NULL;
  dfsi <- NULL;
  knotssi <- NULL;
  d_formulasi <- NULL;
  ysi <- NULL;
  a_formula_gr_strsi <- NULL;
  b_formula_gr_strsi <- NULL;
  c_formula_gr_strsi <- NULL;
  d_formula_gr_strsi <- NULL;
  e_formula_gr_strsi <- NULL;
  f_formula_gr_strsi <- NULL;
  g_formula_gr_strsi <- NULL;
  h_formula_gr_strsi <- NULL;
  i_formula_gr_strsi <- NULL;
  s_formula_gr_strsi <- NULL;
  d_adjustedsi <- NULL;
  xsi <- NULL;
  xfunsi <- NULL;
  yfunsi <- NULL;
  boundsi <- NULL;
  xoffsetsi <- NULL;
  bstartsi <- NULL;
  cstartsi <- NULL;
  apvsi <- NULL;
  pvsi <- NULL;
  group_arg_groupvar <- NULL;
  multivariate_rescor <- NULL;
  univariate_by_by <- NULL;
  sigma_arg_groupvar <- NULL;
  a_init_betasi <- NULL;
  b_init_betasi <- NULL;
  c_init_betasi <- NULL;
  d_init_betasi <- NULL;
  e_init_betasi <- NULL;
  f_init_betasi <- NULL;
  g_init_betasi <- NULL;
  h_init_betasi <- NULL;
  i_init_betasi <- NULL;
  s_init_betasi <- NULL;
  a_cov_init_betasi <- NULL;
  b_cov_init_betasi <- NULL;
  c_cov_init_betasi <- NULL;
  d_cov_init_betasi <- NULL;
  e_cov_init_betasi <- NULL;
  f_cov_init_betasi <- NULL;
  g_cov_init_betasi <- NULL;
  h_cov_init_betasi <- NULL;
  i_cov_init_betasi <- NULL;
  s_cov_init_betasi <- NULL;
  a_init_sdsi <- NULL;
  b_init_sdsi <- NULL;
  c_init_sdsi <- NULL;
  d_init_sdsi <- NULL;
  e_init_sdsi <- NULL;
  f_init_sdsi <- NULL;
  g_init_sdsi <- NULL;
  h_init_sdsi <- NULL;
  i_init_sdsi <- NULL;
  s_init_sdsi <- NULL;
  a_cov_init_sdsi <- NULL;
  b_cov_init_sdsi <- NULL;
  c_cov_init_sdsi <- NULL;
  d_cov_init_sdsi <- NULL;
  e_cov_init_sdsi <- NULL;
  f_cov_init_sdsi <- NULL;
  g_cov_init_sdsi <- NULL;
  h_cov_init_sdsi <- NULL;
  i_cov_init_sdsi <- NULL;
  s_cov_init_sdsi <- NULL;
  sigma_init_betasi <- NULL;
  sigma_cov_init_betasi <- NULL;
  sigma_init_sdsi <- NULL;
  sigma_cov_init_sdsi <- NULL;
  rsd_init_sigmasi <- NULL;
  dpar_init_sigmasi <- NULL;
  dpar_cov_init_sigmasi <- NULL;
  autocor_init_acorsi <- NULL;
  autocor_init_unstr_acorsi <- NULL;
  gr_init_corsi <- NULL;
  sigma_init_corsi <- NULL;
  mvr_init_rescorsi <- NULL;
  r_init_zsi <- NULL;
  a_prior_betasi <- NULL;
  b_prior_betasi <- NULL;
  c_prior_betasi <- NULL;
  d_prior_betasi <- NULL;
  e_prior_betasi <- NULL;
  f_prior_betasi <- NULL;
  g_prior_betasi <- NULL;
  h_prior_betasi <- NULL;
  i_prior_betasi <- NULL;
  s_prior_betasi <- NULL;
  a_cov_prior_betasi <- NULL;
  b_cov_prior_betasi <- NULL;
  c_cov_prior_betasi <- NULL;
  d_cov_prior_betasi <- NULL;
  e_cov_prior_betasi <- NULL;
  f_cov_prior_betasi <- NULL;
  g_cov_prior_betasi <- NULL;
  h_cov_prior_betasi <- NULL;
  i_cov_prior_betasi <- NULL;
  s_cov_prior_betasi <- NULL;
  a_prior_sdsi <- NULL;
  b_prior_sdsi <- NULL;
  c_prior_sdsi <- NULL;
  d_prior_sdsi <- NULL;
  e_prior_sdsi <- NULL;
  f_prior_sdsi <- NULL;
  g_prior_sdsi <- NULL;
  h_prior_sdsi <- NULL;
  i_prior_sdsi <- NULL;
  s_prior_sdsi <- NULL;
  a_cov_prior_sdsi <- NULL;
  b_cov_prior_sdsi <- NULL;
  c_cov_prior_sdsi <- NULL;
  d_cov_prior_sdsi <- NULL;
  e_cov_prior_sdsi <- NULL;
  f_cov_prior_sdsi <- NULL;
  g_cov_prior_sdsi <- NULL;
  h_cov_prior_sdsi <- NULL;
  i_cov_prior_sdsi <- NULL;
  s_cov_prior_sdsi <- NULL;
  gr_prior_corsi <- NULL;
  sigma_prior_corsi <- NULL;
  sigma_prior_betasi <- NULL;
  sigma_cov_prior_betasi <- NULL;
  sigma_prior_sdsi <- NULL;
  sigma_cov_prior_sdsi <- NULL;
  rsd_prior_sigmasi <- NULL;
  dpar_prior_sigmasi <- NULL;
  dpar_cov_prior_sigmasi <- NULL;
  autocor_prior_acorsi <- NULL;
  autocor_prior_unstr_acorsi <- NULL;
  mvr_prior_rescorsi <- NULL;
  initsi <- NULL;
  hierarchical_gr_names <- NULL;
  sigma_hierarchical_gr_names <- NULL;
  lb <- NULL;
  ub <- NULL;
  init_rsi <- NULL;
  `:=` <- NULL;
  . <- NULL;
  s_formulasi <- NULL;
  ymean <- NULL;
  ysd <- NULL;
  lm <- NULL;
  checks. <- NULL;
  NoccPI <- NULL;
  NoccAI <- NULL;
  xs <- NULL;
  ids <- NULL;
  dfs <- NULL;
  XXi <- NULL;
  onepic <- NULL;
  temp1 <- NULL;
  temp2 <- NULL;
  
  
  
  enverr. <- parent.frame()
  for (i in names(mcall)[-1]) {
    no_default_args_plus_family <- c(no_default_args, "family")
    if (!i %in% no_default_args_plus_family) {
      assign('err.', FALSE, envir = enverr.)
      tryCatch(
        expr = {
          # suppressWarnings 14 01 2024
          if (is.function(suppressWarnings(eval(mcall[[i]])))) {
            checks. <- deparse_0(mcall[[i]])
          } else {
            # suppressWarnings 14 01 2024
            suppressWarnings(checks. <- eval(mcall[[i]]))
          }
        },
        error = function(e) {
          assign('err.', TRUE, envir = enverr.)
        }
      )
      err. <- get('err.', envir = enverr.)
      if(length(checks.) == 0) err. <- TRUE
      if (err.) {
        mcall[[i]] <- mcall[[i]]
      } else if (!err.) {
        if (is.list(checks.)) {
          if (is.list(checks.[[1]])) {
            mcall[[i]] <- checks.[[1]]
          } else if (!is.list(checks.[[1]])) {
            if (is.list(checks.)) {
              if (is.symbol(mcall[[i]]))
                mcall[[i]] <- deparse_0(mcall[[i]]) # for set_self_priors
              # suppressWarnings 14 01 2024
                suppressWarnings(mcall[[i]] <- eval(mcall[[i]]))
                temp       <- str2lang(deparse_0((mcall[[i]])))
                mcall[[i]] <- temp
            } else if (!is.list(checks.)) {
              mcall[[i]] <- checks.
            }
          }
        } else {
          mcall[[i]] <-  checks.
        }
      }
    } else if (i %in% no_default_args_plus_family) {
      mcall[[i]] <-  mcall[[i]]
    }
  }
  
  
  arguments <- as.list(mcall)[-1]
 
  match.call.defaults <- function(...) {
    call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
    formals <- evalq(formals(), parent.frame(1))
    for(i in setdiff(names(formals), names(call)))
      call[i] <- list( formals[[i]] )
    match.call(sys.function(sys.parent()), call)
  }
  
  call.full <- match.call.defaults()
  call.full <- call.full[-length(call.full)]
 
  for (call.fulli in names(call.full)) {
    if(call.fulli != "") {
      if(call.fulli == 'family' & 
         is.language(call.full[[call.fulli]])) {
        call.full[[call.fulli]] <- deparse(call.full[[call.fulli]])
      } else if(call.fulli == 'stan_model_args')  { 
        if(length(eval(call.full[[call.fulli]])) == 0) {
          call.full[[call.fulli]] <- NULL
        } else {
          call.full[[call.fulli]] <- call.full[[call.fulli]] 
        }
      } else {
        
      }
    } else {
      #
    }
  }
  

  f_funx_arg <- formals(bsitar)
  nf_funx_arg_names <-
    intersect(names(arguments), names(f_funx_arg))
  arguments <-
    c(arguments, f_funx_arg[names(f_funx_arg) %!in% nf_funx_arg_names])
  
  
  
  familyzzzx <- arguments$family
  if(grepl("^c\\(", deparse_0(familyzzzx), fixed = F)) {
    stop("Argument family should be a list() and not a vector 'c()'")
  } else if(grepl("^\\(", deparse_0(familyzzzx), fixed = F)) {
    familyzzzx <- paste0("list", "", deparse_0(familyzzzx) , "")
    familyzzzx <- str2lang(familyzzzx)
  } else if(!is.list(familyzzzx) & !grepl("list", familyzzzx)[[1]]) {
    familyzzzx <- paste0("list", "(", deparse_0(familyzzzx) , ")")
    familyzzzx <- str2lang(familyzzzx)
  } else {
    familyzzzx <- familyzzzx 
  }
  familyzzzx2 <- familyzzzx
  arguments$family <- familyzzzx
  

  checks_start_names <- c('bstart', 'cstart', 'apv', 'pv')
  for (checks_start_namesi in checks_start_names) {
    if(checks_start_namesi %in% names(mcall_)) {
      if(is.null(mcall_[[checks_start_namesi]])) {
        arguments[[checks_start_namesi]] <- 'NULL'
      }
    }
  }
  
  
  
  
  # Override when restricting to abcd
  override_select_model <- TRUE # FALSE
  
  if(override_select_model) arguments$select_model <- select_model <- 'sitar'
  
  # Override when restricting to rcs
  if(select_model != 'rcs') decomp <- NULL
  
  if(!is.null(decomp)) {
    if(select_model != 'rcs') 
      stop("Decomposition (decomp = 'QR') is allowed only for the RCS model")
  }
  
  
  if(is.character(arguments$select_model)) {
    select_model <- arguments$select_model
  } else if(is.symbol(arguments$select_model)) {
    select_model <- deparse(arguments$select_model)
  } else if(!is.character(arguments$select_model) |
            !is.symbol(arguments$select_model)
            ) {
    stop("The argument 'select_model' must be a symbol or 
         single character string")
  }
  
  # For editing scode (if required for later use TODO)
  select_model_edit <- select_model
  
  if(select_model == 'logistic1e') select_model <- 'logistic1'
  if(select_model == 'logistic2e') select_model <- 'logistic2'
  if(select_model == 'logistic3e') select_model <- 'logistic3'
  
  # For default prior setting (if required for later use TODO)
  select_model_arg <- select_model
  
  
  if(select_model == 'pb')       select_model <- 'pb1'
  if(select_model == 'logistic') select_model <- 'logistic1'
  if(select_model == 'sitar3')   select_model <- 'sitar'
  
  if(select_model == 'rcs' | 
     select_model == 'rcsf' | 
     select_model == 'rcsfr') {
    if(select_model == 'rcsf') {
      rcs_add_re_spline <- FALSE 
    } else {
      rcs_add_re_spline <- TRUE
    }
    select_model <- 'rcs'
  }
    
  match_sitar_d_form <- FALSE
  if(grepl('sitar4', select_model)) {
    if(select_model == 'sitar4fr') match_sitar_d_form <- FALSE
    if(select_model == 'sitar4f')  match_sitar_d_form <- FALSE
    if(select_model == 'sitar4r')  match_sitar_d_form <- TRUE
    if(select_model == 'sitar4')   match_sitar_d_form <- FALSE
    sitar_nparms <- 4
    select_model <- 'sitar'
  } else if(select_model == 'sitar') {
    sitar_nparms <- 3
    select_model <- 'sitar'
    match_sitar_d_form <- FALSE
  } else {
    match_sitar_d_form <- FALSE
  }
  
 
  sitar_models    <- c('sitar', 'sitar3', 'sitar4', 
                       'sitar4f', 'sitar4fr', 'sitar4r')
  pb_models       <- c('pb1', 'pb2', 'pb3')
  logistic_models <- c('logistic1', 'logistic2', 'logistic3')
  rcs_models      <- c('rcs', 'rcsf', 'rcsfr')
  
  allowed_model_names <- c(sitar_models, 
                           pb_models, 
                           logistic_models, 
                           rcs_models)
  
  sitar_models    <- paste0(sitar_models, collapse=", ")
  pb_models       <- paste0(pb_models, collapse=", ")
  logistic_models <- paste0(logistic_models, collapse=", ")
  rcs_models      <- paste0(rcs_models, collapse=", ")
  
  all_models <- paste0('NLME models: ', "\n  ", 
                       '  SITAR: ',  sitar_models, "\n  ", 
                       '  PB: ', pb_models, "\n  ", 
                       '  LOGISTIC: ', logistic_models, "\n  ",
                       'LME models: ', "\n  ", 
                       '  RCS: ', rcs_models)
  
 
  if(!select_model_arg %in% allowed_model_names) {
    stop("Currently supported models (via 'select_model' argument) are:",
         "\n ",
         " ", all_models
         )
  }
  
  
  for (ip in names(arguments)) {
    if (grepl("_init_", ip)) {
      d_mcall_ <- deparse_0(mcall_[[ip]])
      if(is.symbol(mcall_[[ip]])) {
        arguments[[ip]] <- d_mcall_
      } else if(grepl("c(", d_mcall_, fixed = T)) {
        arguments[[ip]] <- gsub("c(", "list(", d_mcall_, fixed = T)
      } else if(grepl("list(", d_mcall_, fixed = T)) {
        arguments[[ip]] <- mcall_[[ip]]
      }
    }
  }
  
  remove_spaces <- c('a_formula_gr_str', 'b_formula_gr_str', 
                     'c_formula_gr_str', 'd_formula_gr_str',
                     'e_formula_gr_str', 'f_formula_gr_str', 
                     'g_formula_gr_str', 'h_formula_gr_str', 
                     'i_formula_gr_str', 's_formula_gr_str', 
                     'sigma_formula_gr_str')
  
  for (ip in remove_spaces) {
    arguments[[ip]] <-  gsub_space(arguments[[ip]] )
  }
  
  brms_arguments_list <-
    c(
      'chains',
      'iter',
      'warmup',
      'thin',
      'cores',
      'backend',
      'threads',
      'opencl',
      'normalize',
      'algorithm',
      'control',
      'sample_prior',
      'save_pars',
      'drop_unused_levels',
      'stan_model_args',
      'refresh',
      'silent',
      'seed',
      'save_model',
      'fit',
      'file',
      'file_refit',
      'future'
    )
  


  if(is.numeric(arguments$cores)) {
   oldopts <- options(mc.cores = arguments$cores)
   on.exit(options(oldopts))
  }
  
  iter <-  arguments$iter
  warmup <-  arguments$warmup <- eval(arguments$warmup)
 
 
  brms_arguments <- list()
  for (brms_arguments_listi in brms_arguments_list) {
    brms_arguments[[brms_arguments_listi]] <-
      arguments[[brms_arguments_listi]]
    arguments[[brms_arguments_listi]] <- NULL
  }
  
 
  brms_arguments <- mget(brms_arguments_list)
  
  if (eval(brms_arguments$backend) != "rstan" &
      eval(brms_arguments$backend) != "mock" &
      eval(brms_arguments$backend) != "cmdstanr") {
    stop("The backend argument must be either 'rstan', 'mock', or 'cmdstanr'",
         "\n ",
         "\ Please check it which you have specified as: ", 
         eval(brms_arguments$backend))
  }
  
  
  displayit <- 'col'
  setcolh   <- 47 
  setcolb   <- 3
  
  # Quote unquoted character (e.g., sex to 'sex')
  list_to_quoted_if_not <- function(x) {
    splitmvar <- x
    splitmvar <- gsub("\\s", "", splitmvar)
    splitmvar <- paste(splitmvar, collapse = "")
    splitmvar_w <-
      gsub("[\\(\\)]", "", regmatches(splitmvar, gregexpr("\\(.*?\\)",
                                                          splitmvar))[[1]])
    splitmvar_w2 <- strsplit(splitmvar_w, ",")[[1]]
    splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
    gsubs_c_counter <- 0
    for (i in splitmvar_w3) {
      gsubs_c_counter <- gsubs_c_counter + 1
      if (gsubs_c_counter < max(length(splitmvar_w3))) {
        pattern <- paste0(i, "=", "\\s*(.*?)\\s*", ",")
      } else {
        pattern <- paste0(i, "=", "\\s*(.*?)\\s*", ")")
      }
      majors <- regmatches(splitmvar, regexec(pattern, splitmvar))
      majors2 <- majors[[1]][2]
      majors2 <- majors[[1]][2]
      if (grepl("^T$", majors2)) {
        majors2 <- gsub("^T$", "TRUE", majors2)
      }
      if (grepl("^F$", majors2)) {
        majors2 <- gsub("^F$", "FALSE", majors2)
      }
      majors2 <- gsub("\"", "", majors2)
      majors3 <- paste0("\"", majors2, "\"")
      if (gsubs_c_counter == 1) {
        splitmvar2 <- gsub(noquote(majors2), majors3, splitmvar, fixed = F)
      } else {
        splitmvar2 <- gsub(noquote(majors2), majors3, splitmvar2, fixed = F)
      }
    }
    for (i in 1:length(splitmvar_w3))
      splitmvar2 <- gsub("\"\"", "\"", splitmvar2)
    splitmvar3 <- eval(parse(text = splitmvar2))
    zzz <- splitmvar3
    
    enverr. <- parent.frame()
    for (z in names(splitmvar3)) {
      assign('err.', FALSE, envir = enverr.)
      tryCatch(
        expr = {
          eval(parse(text = zzz[[z]]), envir = parent.frame())
        },
        error = function(e) {
          assign('err.', TRUE, envir = enverr.)
        }
      )
      err. <- get('err.', envir = enverr.)
      if (!err.) {
        # if brms::brmsfamily(family), eval eliminates family 16 1 24
        if(z != "family") c_c_ <- eval(parse(text = zzz[[z]]))
        if(z == "family") c_c_ <- zzz[[z]] 
        # c_c_ <- eval(parse(text = zzz[[z]]))
        checkclass <- class(c_c_)
        if (checkclass == "NULL")
          checkclass_ <- NULL
        else
          checkclass_ <- NA
        if (is.logical(c_c_) | is.null(checkclass_))
          zzz[[z]] <- c_c_
      } else {
        zzz[[z]] <- zzz[[z]]
      }
    }
    return(zzz)
  }
  
  
  list_to_quoted_if_not_si <- function(xx) {
    xx.o <- xx
    prefix_ <- strsplit(xx, "\\(")[[1]][1]
    prefix_by <- "list"
    xx <- gsub(paste0("^", prefix_, ""), prefix_by, xx)
    if (sub("\\).*", "", sub(".*\\(", "", xx)) != "") {
      xxx <- list_to_quoted_if_not(xx)
      xxx <- gsub("\"" , "'", deparse_0(xxx))
      xxx <- gsub(paste0("^", prefix_by, ""), prefix_, xxx)
      xxx <- gsub("\\s", "", xxx)
    } else {
      xxx <- xx.o
    }
    xxx
  }
  
  
  list_to_quoted_if_not_si_lf <- function(xx) {
    xx.o <- xx
    prefix_ <- strsplit(xx, "\\(")[[1]][1]
    prefix_by <- "list"
    xx <- gsub(paste0("^", prefix_, ""), prefix_by, xx)
    if (sub("\\).*", "", sub(".*\\(", "", xx)) != "") {
      xxt <- sub("\\).*", "", sub(".*\\(", "", xx))
      xxtf <-
        strsplit(xxt, ",")[[1]][grepl("~", strsplit(xxt, ",")[[1]])]
      xxtnf <-
        strsplit(xxt, ",")[[1]][!grepl("~", strsplit(xxt, ",")[[1]])]
      xxtf <- gsub("\\s", "", xxtf)
      xxtnf <- gsub("\\s", "", xxtnf)
      xx <-
        paste0(prefix_by, "(", paste(xxtnf, collapse = ","), ")")
      xxx <- list_to_quoted_if_not(xx)
      xxx <- gsub("\"" , "'", deparse_0(xxx))
      xxx <- gsub(paste0("^", prefix_by, ""), prefix_, xxx)
      xxx <-
        gsub(paste0(prefix_, "\\("),
             paste0(prefix_, "(", xxtf, ","),
             xxx)
      xxx <- gsub("\\s", "", xxx)
    } else {
      xxx <- xx.o
    }
    xxx
  }
  
  
  # set multivariate arguments
  if (gsub("\\s", "",
           paste(deparse(substitute(multivariate)), collapse = "")) == "NULL" |
      gsub("\\s", "",
           paste(deparse(substitute(multivariate)), collapse = "")) == "NA" |
      gsub("\\s", "",
           paste(deparse(substitute(multivariate)), collapse = "")) == "FALSE" |
      gsub("\\s", "",
           paste(deparse(substitute(multivariate)), collapse = "")) == "F") {
    multivariate <- list()
    multivariate$mvar <- FALSE
  } else if (gsub("\\s", "",
                  paste(deparse(substitute(multivariate)),
                        collapse = "")) == "TRUE" |
             gsub("\\s", "",
                  paste(deparse(substitute(multivariate)),
                        collapse = "")) == "T") {
    multivariate <- list()
    multivariate$mvar <- TRUE
  } else if (!grepl("^list", gsub("\\s", "", paste(deparse(
    substitute(multivariate)
  ), collapse = ""))) &
  !is.null(gsub("\\s", "", paste(deparse(
    substitute(multivariate)
  ), collapse = "")))) {
    if (is.symbol(substitute(multivariate))) {
      multivariate <-
        gsub("\\s", "", paste(deparse(substitute(multivariate)), 
                              collapse = ""))
      if (multivariate == "T")
        multivariate <- eval(parse(text = multivariate))
      multivariate <- multivariate
      multivariate <- as.list(multivariate)
      names(multivariate) <- 'mvar'
    } else if (is.character(substitute(multivariate))) {
      multivariate <- multivariate
      multivariate <- as.list(multivariate)
      names(multivariate) <- 'mvar'
    }
  }
  if (grepl("^list", gsub("\\s", "", paste(deparse(
    substitute(multivariate)
  ), collapse = ""))) &
  length(strsplit(gsub("\\s", "", paste(
    deparse(substitute(multivariate)), collapse = ""
  )), ",")[[1]]) == 1) {
    if (!is.null(gsub("\\s", "", paste(deparse(
      substitute(multivariate)
    ), collapse = "")))) {
      if (is.language(substitute(multivariate))) {
        ttt <-
          gsub("\\s", "", paste(deparse(substitute(
            multivariate
          )), collapse = ""))
        temp <- sub("\\).*", "", sub(".*\\(", "", ttt))
        if (temp == "") {
          stop("empty list")
        }
        if (length(strsplit(temp, "=")[[1]]) == 1) {
          ttt <- gsub(strsplit(temp, "=")[[1]],
                      paste0("mvar=", strsplit(temp, "=")[[1]]),
                      ttt)
        }
        multivariate <- list_to_quoted_if_not(ttt)
        
      } else if (grepl("^list", multivariate)) {
        ttt <- deparse_0(as.name(substitute(multivariate)))
        temp <- sub("\\).*", "", sub(".*\\(", "", ttt))
        if (temp == "") {
          stop("empty list")
        }
        if (length(strsplit(temp, "=")[[1]]) == 1) {
          ttt <- gsub(strsplit(temp, "=")[[1]],
                      paste0("mvar=", strsplit(temp, "=")[[1]]),
                      ttt)
        }
        multivariate <- list_to_quoted_if_not(ttt)
        for (multivariatei in 1:length(multivariate)) {
          if (!is.null(multivariate[[multivariatei]])) {
            multivariate[[multivariatei]] <-
              gsub("'", "", multivariate[[multivariatei]])
          }
        }
      } else {
        if (!is.null(multivariate)) {
          if (!grepl("^list", multivariate)) {
            multivariate <- multivariate
            multivariate <- as.list(multivariate)
            names(multivariate) <- 'mvar'
          }
        } else if (is.null(multivariate)) {
          multivariate <- as.list(multivariate)
        }
      }
    }
  }
  if (length(strsplit(gsub("\\s", "", paste(
    deparse(substitute(multivariate)), collapse = ""
  )), ",")[[1]]) > 1) {
    ttt <-
      gsub("\\s", "", paste(deparse(substitute(multivariate)), collapse = ""))
    multivariate <- list_to_quoted_if_not(ttt)
  }
  
  
  # Set univariate_by arguments
  if (gsub("\\s", "",
           paste(deparse(substitute(univariate_by)), 
                 collapse = "")) == "NULL" |
      gsub("\\s", "",
           paste(deparse(substitute(univariate_by)), 
                 collapse = "")) == "NA" |
      gsub("\\s", "",
           paste(deparse(substitute(univariate_by)), 
                 collapse = "")) == "FALSE" |
      gsub("\\s", "",
           paste(deparse(substitute(univariate_by)), 
                 collapse = "")) == "F") {
    univariate_by <- list()
    univariate_by$by <- NA
  } else if (!grepl("^list", gsub("\\s", "", paste(deparse(
    substitute(univariate_by)
  ), collapse = ""))) &
  !is.null(gsub("\\s", "", paste(deparse(
    substitute(univariate_by)
  ), collapse = "")))) {
    if (is.symbol(substitute(univariate_by))) {
      univariate_by <-
        gsub("\\s", "", paste(deparse(substitute(
          univariate_by
        )), collapse = ""))
      univariate_by <- univariate_by
      univariate_by <- as.list(univariate_by)
      names(univariate_by) <- 'by'
    } else if (is.character(substitute(univariate_by))) {
      univariate_by <- univariate_by
      univariate_by <- as.list(univariate_by)
      names(univariate_by) <- 'by'
    }
  }
  if (grepl("^list", gsub("\\s", "", paste(deparse(
    substitute(univariate_by)
  ), collapse = ""))) &
  length(strsplit(gsub("\\s", "", paste(
    deparse(substitute(univariate_by)), collapse = ""
  )), ",")[[1]]) == 1) {
    if (!is.null(gsub("\\s", "", paste(deparse(
      substitute(univariate_by)
    ), collapse = "")))) {
      if (is.language(substitute(univariate_by))) {
        ttt <-
          gsub("\\s", "", paste(deparse(substitute(
            univariate_by
          )), collapse = ""))
        temp <- sub("\\).*", "", sub(".*\\(", "", ttt))
        if (temp == "") {
          stop("empty list")
        }
        if (length(strsplit(temp, "=")[[1]]) == 1) {
          ttt <- gsub(strsplit(temp, "=")[[1]],
                      paste0("by=", strsplit(temp, "=")[[1]]), ttt)
        }
        univariate_by <- list_to_quoted_if_not(ttt)
        
      } else if (grepl("^list", univariate_by)) {
        ttt <- deparse_0(as.name(substitute(univariate_by)))
        temp <- sub("\\).*", "", sub(".*\\(", "", ttt))
        if (temp == "") {
          stop("empty list")
        }
        if (length(strsplit(temp, "=")[[1]]) == 1) {
          ttt <- gsub(strsplit(temp, "=")[[1]],
                      paste0("by=", strsplit(temp, "=")[[1]]), ttt)
        }
        univariate_by <- list_to_quoted_if_not(ttt)
        for (univariate_byi in 1:length(univariate_by)) {
          if (!is.null(univariate_by[[univariate_byi]])) {
            univariate_by[[univariate_byi]] <-
              gsub("'", "", univariate_by[[univariate_byi]])
          }
        }
      } else {
        if (!is.null(univariate_by)) {
          if (!grepl("^list", univariate_by)) {
            univariate_by <- univariate_by
            univariate_by <- as.list(univariate_by)
            names(univariate_by) <- 'by'
          }
        } else if (is.null(univariate_by)) {
          univariate_by <- as.list(univariate_by)
        }
      }
    }
  }
  if (length(strsplit(gsub("\\s", "", paste(
    deparse(substitute(univariate_by)), collapse = ""
  )), ",")[[1]]) > 1) {
    ttt <-
      gsub("\\s", "", paste(deparse(substitute(univariate_by)), 
                            collapse = ""))
    univariate_by <- list_to_quoted_if_not(ttt)
  }
  
  # Set group_arg arguments 
  if (!paste(deparse(substitute(group_arg)), collapse = "") == "NULL"  &
      !any(grepl("^list", gsub("\\s", "", paste(
        deparse(substitute(group_arg)), collapse = ""
      )))) &
      any(gsub("\\s", "", paste(deparse(
        substitute(group_arg)
      ), collapse = "")) == "NULL")) {
    group_arg <- list()
    group_arg$groupvar <- NULL
  } else if (!any(grepl("^list", gsub("\\s", "", paste(
    deparse(substitute(group_arg)), collapse = ""
  )))) &
  any(gsub("\\s", "", paste(deparse(
    substitute(group_arg)
  ), collapse = "")) != "NULL")) {
    if (paste(deparse(substitute(group_arg)), collapse = "") == "T" |
        paste(deparse(substitute(group_arg)), collapse = "") == "TRUE" |
        paste(deparse(substitute(group_arg)), collapse = "") == "F" |
        paste(deparse(substitute(group_arg)), collapse = "") == "FALSE" |
        paste(deparse(substitute(group_arg)), collapse = "") == "NA") {
      stop("group_arg should be either NULL or a character",
           " denoting the group idetifier")
    }
    if (is.symbol(substitute(group_arg))) {
      group_arg <-
        gsub("\\s", "", paste(deparse(substitute(group_arg)), collapse = ""))
      group_arg <- group_arg
      group_arg <- as.list(group_arg)
      names(group_arg) <- 'groupvar'
    } else if (is.character(substitute(group_arg))) {
      group_arg <- group_arg
      group_arg <- as.list(group_arg)
      names(group_arg) <- 'groupvar'
    }
  }
  if (any(grepl("^list", gsub("\\s", "",
                              paste(
                                deparse(substitute(group_arg)),
                                collapse = ""
                              )))) &
      length(strsplit(gsub("\\s", "",
                           paste(
                             deparse(substitute(group_arg)),
                             collapse = ""
                           )), ",")[[1]]) == 1) {
    if (!is.null(gsub("\\s", "", paste(deparse(
      substitute(group_arg)
    ),
    collapse = "")))) {
      if (is.language(substitute(group_arg))) {
        ttt <- gsub("\\s", "", paste(deparse(substitute(group_arg)),
                                     collapse = ""))
        temp <- sub("\\).*", "", sub(".*\\(", "", ttt))
        if (temp == "T" |
            temp == "TRUE" |
            temp == "F" |
            temp == "FALSE") {
          stop(
            "group_arg should be either NULL or a character",
            " denoting the group idetifier"
          )
        }
        if (temp == "") {
          stop("empty list")
        }
        if (length(strsplit(temp, "=")[[1]]) == 1) {
          ttt <- gsub(strsplit(temp, "=")[[1]],
                      paste0("groupvar=", strsplit(temp, "=")[[1]]),
                      ttt)
        }
        group_arg <- list_to_quoted_if_not(ttt)
      } else if (grepl("^list", group_arg)) {
        ttt <- deparse_0(as.name(substitute(group_arg)))
        temp <- sub("\\).*", "", sub(".*\\(", "", ttt))
        if (temp == "") {
          stop("empty list")
        }
        if (length(strsplit(temp, "=")[[1]]) == 1) {
          ttt <- gsub(strsplit(temp, "=")[[1]],
                      paste0("groupvar=", strsplit(temp, "=")[[1]]),
                      ttt)
        }
        group_arg <- list_to_quoted_if_not(ttt)
        for (group_argi in 1:length(group_arg)) {
          if (!is.null(group_arg[[group_argi]])) {
            group_arg[[group_argi]] <- gsub("'", "", group_arg[[group_argi]])
          }
        }
      } else {
        if (!is.null(group_arg)) {
          if (!grepl("^list", gsub("\\s", "",
                                   paste(
                                     deparse(substitute(group_arg)),
                                     collapse = ""
                                   )))) {
            group_arg <- group_arg
            group_arg <- as.list(group_arg)
            names(group_arg) <- 'groupvar'
          }
        } else if (is.null(group_arg)) {
          group_arg <- as.list(group_arg)
        }
      }
    } else if (is.null(group_arg)) {
      group_arg <- list()
      group_arg$groupvar <- NULL
    }
  }
  if (any(grepl("^list", gsub("\\s", "",
                              paste(
                                deparse(substitute(group_arg)),
                                collapse = ""
                              )))) &
      length(strsplit(gsub("\\s", "",
                           paste(
                             deparse(substitute(group_arg)),
                             collapse = ""
                           )), ",")[[1]]) > 1) {
    ttt <-
      gsub("\\s", "", paste(deparse(substitute(group_arg)), collapse = ""))
    group_arg <- list_to_quoted_if_not(ttt)
  }
  if (length(group_arg) == 0) {
    group_arg <- list()
    group_arg$groupvar <- NULL
  }
  if (!is.null(group_arg$groupvar) &
      !is.character(group_arg$groupvar)) {
    stop("group_arg should be either NULL or a character",
         " denoting the group idetifier")
  }
  

  
  # Set up sigma_group_arg arguments 
  if (!paste(deparse(substitute(sigma_group_arg)), collapse = "") == "NULL"  &
      !any(grepl("^list", gsub("\\s", "", paste(
        deparse(substitute(sigma_group_arg)), collapse = ""
      )))) &
      any(gsub("\\s", "", paste(deparse(
        substitute(sigma_group_arg)
      ), collapse = "")) == "NULL")) {
    sigma_group_arg <- list()
    sigma_group_arg$groupvar <- NULL
  } else if (!any(grepl("^list", gsub("\\s", "", paste(
    deparse(substitute(sigma_group_arg)), collapse = ""
  )))) &
  any(gsub("\\s", "", paste(deparse(
    substitute(sigma_group_arg)
  ), collapse = "")) != "NULL")) {
    if (paste(deparse(substitute(sigma_group_arg)), collapse = "") == "T" |
        paste(deparse(substitute(sigma_group_arg)), collapse = "") == "TRUE" |
        paste(deparse(substitute(sigma_group_arg)), collapse = "") == "F" |
        paste(deparse(substitute(sigma_group_arg)), collapse = "") == "FALSE" |
        paste(deparse(substitute(sigma_group_arg)), collapse = "") == "NA") {
      stop("sigma_group_arg should be either NULL or a character",
           " denoting the group idetifier")
    }
    if (is.symbol(substitute(sigma_group_arg))) {
      sigma_group_arg <-
        gsub("\\s", "", 
             paste(deparse(substitute(sigma_group_arg)), collapse = ""))
      sigma_group_arg <- sigma_group_arg
      sigma_group_arg <- as.list(sigma_group_arg)
      names(sigma_group_arg) <- 'groupvar'
    } else if (is.character(substitute(sigma_group_arg))) {
      sigma_group_arg <- sigma_group_arg
      sigma_group_arg <- as.list(sigma_group_arg)
      names(sigma_group_arg) <- 'groupvar'
    }
  }
  if (any(grepl("^list", gsub("\\s", "",
                              paste(
                                deparse(substitute(sigma_group_arg)),
                                collapse = ""
                              )))) &
      length(strsplit(gsub("\\s", "",
                           paste(
                             deparse(substitute(sigma_group_arg)),
                             collapse = ""
                           )), ",")[[1]]) == 1) {
    if (!is.null(gsub("\\s", "", paste(deparse(
      substitute(sigma_group_arg)
    ),
    collapse = "")))) {
      if (is.language(substitute(sigma_group_arg))) {
        ttt <- gsub("\\s", "", paste(deparse(substitute(sigma_group_arg)),
                                     collapse = ""))
        temp <- sub("\\).*", "", sub(".*\\(", "", ttt))
        if (temp == "T" |
            temp == "TRUE" |
            temp == "F" |
            temp == "FALSE") {
          stop(
            "sigma_group_arg should be either NULL or a character",
            " denoting the group idetifier"
          )
        }
        if (temp == "") {
          stop("empty list")
        }
        if (length(strsplit(temp, "=")[[1]]) == 1) {
          ttt <- gsub(strsplit(temp, "=")[[1]],
                      paste0("groupvar=", strsplit(temp, "=")[[1]]),
                      ttt)
        }
        sigma_group_arg <- list_to_quoted_if_not(ttt)
      } else if (grepl("^list", sigma_group_arg)) {
        ttt <- deparse_0(as.name(substitute(sigma_group_arg)))
        temp <- sub("\\).*", "", sub(".*\\(", "", ttt))
        if (temp == "") {
          stop("empty list")
        }
        if (length(strsplit(temp, "=")[[1]]) == 1) {
          ttt <- gsub(strsplit(temp, "=")[[1]],
                      paste0("groupvar=", strsplit(temp, "=")[[1]]),
                      ttt)
        }
        sigma_group_arg <- list_to_quoted_if_not(ttt)
        for (sigma_group_argi in 1:length(sigma_group_arg)) {
          if (!is.null(sigma_group_arg[[sigma_group_argi]])) {
            sigma_group_arg[[sigma_group_argi]] <- 
              gsub("'", "", sigma_group_arg[[sigma_group_argi]])
          }
        }
      } else {
        if (!is.null(sigma_group_arg)) {
          if (!grepl("^list", gsub("\\s", "",
                                   paste(
                                     deparse(substitute(sigma_group_arg)),
                                     collapse = ""
                                   )))) {
            sigma_group_arg <- sigma_group_arg
            sigma_group_arg <- as.list(sigma_group_arg)
            names(sigma_group_arg) <- 'groupvar'
          }
        } else if (is.null(sigma_group_arg)) {
          sigma_group_arg <- as.list(sigma_group_arg)
        }
      }
    } else if (is.null(sigma_group_arg)) {
      sigma_group_arg <- list()
      sigma_group_arg$groupvar <- NULL
    }
  }
  if (any(grepl("^list", gsub("\\s", "",
                              paste(
                                deparse(substitute(sigma_group_arg)),
                                collapse = ""
                              )))) &
      length(strsplit(gsub("\\s", "",
                           paste(
                             deparse(substitute(sigma_group_arg)),
                             collapse = ""
                           )), ",")[[1]]) > 1) {
    ttt <-
      gsub("\\s", "", paste(deparse(substitute(sigma_group_arg)), 
                            collapse = ""))
    sigma_group_arg <- list_to_quoted_if_not(ttt)
  }
  if (length(sigma_group_arg) == 0) {
    sigma_group_arg <- list()
    sigma_group_arg$groupvar <- NULL
  }
  if (!is.null(sigma_group_arg$groupvar) &
      !is.character(sigma_group_arg$groupvar)) {
    stop("sigma_group_arg should be either NULL or a character",
         " denoting the group idetifier")
  }
  
  
  
  # Add defaults to univariate_by, multivariate, and group_arg arguments
  if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
    univariate_by$by <- gsub("\\s", "", univariate_by$by)
  }
  if (identical(univariate_by$by, character(0))) {
    univariate_by$by <- NA
  }
  
  if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
    if (univariate_by$by == "" |
        univariate_by$by == FALSE | is.null(univariate_by$by)) {
      univariate_by$by <- NA
    }
    if (univariate_by$by == TRUE) {
      stop(
        "For univeriate-by-subgroup model fitting (via univariate_by argument)",
        "\n ",
        "argument 'by' should be a variable name, '', NULL, or FALSE"
      )
    }
  }
  
  
  
  if (multivariate$mvar &
      !(is.na(univariate_by$by) | univariate_by$by == "NA")) {
    stop(
      "You have set multivariate as TRUE and also specified ",
      "\n ",
      " univeriate-by-subgroup model (see univariate_by argument)",
      "\n ",
      " Please specify either multivariate or univariate_by argument"
    )
  }
  
  
  if (is.symbol(arguments[["y"]]) |
      is.character(arguments[["y"]])) {
    nys <- length(arguments[["y"]])
  } else {
    nys <- length(arguments[["y"]]) - 1
  }
  
  
  if (multivariate$mvar & nys == 1) {
    stop(
      "You have set multivariate as TRUE but provided only one outcome ",
      "\n ",
      " Please set y as list or vector of multiple outcomes such as ",
      "\n ",
      " list(outcome1, outcome2) or y = c(outcome1, outcome2)"
    )
  }
  
  if (!multivariate$mvar & nys > 1) {
    stop(
      "You have set multivariate as FALSE but provided more than one outcome",
      "\n ",
      " Please set y as a symbol / list / vector of single outcome such as",
      "\n ",
      " y = outcome, y = list(outcome1) or y = c(outcome1)"
    )
  }
  
  if (!(is.na(univariate_by$by) |
        univariate_by$by == "NA") & nys > 1) {
    stop(
      "You have specified univariate_by model for ",
      univariate_by$by,
      "for which ",
      "\n ",
      " only one outcome varibale should be specified but have provided ",
      nys,
      " outcomes",
      "\n ",
      " Please set y as a symbol / list / vector of single outcome ",
      "\n ",
      " such as y = outcome, y = list(outcome1) or y = c(outcome1)"
    )
  }
  
  if (multivariate$mvar) {
    if (is.null(multivariate$cor))
      multivariate$cor <- "un"
    if (is.null(multivariate$rescor))
      multivariate$rescor <- TRUE
  }
  if (!multivariate$mvar) {
    if (is.null(multivariate$cor))
      multivariate$cor <- "un"
    if (is.null(multivariate$rescor))
      multivariate$rescor <- TRUE
  }
  
  
  
  if (is.na(univariate_by$by)) {
    if (is.null(univariate_by$cor))
      univariate_by$cor <- "un"
  }
  if (!is.na(univariate_by$by)) {
    if (is.null(univariate_by$cor))
      univariate_by$cor <- "un"
  }
  
  
  
  if (is.na(univariate_by$by)) {
    if (is.null(univariate_by$terms))
      univariate_by$terms <- "subset"
  }
  if (!is.na(univariate_by$by)) {
    if (is.null(univariate_by$terms))
      univariate_by$terms <- "subset"
  }
  
  
  
  
  
  if (is.null(group_arg$groupvar))
    group_arg$groupvar <- NULL
  if (is.null(group_arg$by))
    group_arg$by <- NULL
  if (is.null(group_arg$cor))
    group_arg$cor <- "un"
  if (is.null(group_arg$dist))
    group_arg$dist <- "gaussian"
  
  if (is.null(sigma_group_arg$groupvar))
    sigma_group_arg$groupvar <- NULL
  if (is.null(sigma_group_arg$by))
    sigma_group_arg$by <- NULL
  if (is.null(sigma_group_arg$cor))
    sigma_group_arg$cor <- "un"
  if (is.null(sigma_group_arg$dist))
    sigma_group_arg$dist <- "gaussian"
  
  
  
  multivariate$verbose <-
    univariate_by$verbose <- group_arg$verbose <- verbose
  
  sigma_group_arg$verbose <- verbose
  
  
  # Temporary placeholder for the number of response for univariate_by
  if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
    temp_ <- univariate_by$by
    if (!temp_ %in% colnames(data)) {
      stop(
        paste(
          "\nvariable",
          temp_,
          "used for setting univariate_by-univariate submodels is missing"
        )
      )
    }
    if (!is.factor(data[[temp_]])) {
      stop(temp_, "should be a factor variable")
    }
    nlevtemp_ <- nlevels(data[[temp_]])
    nys <- nlevtemp_
  }
  
  
  # Perform checks and set-up the 'to convert arguments' 
  to_list_if_not <- function(.x, nys, arguments, ...) {
    if (nys == 1) {
      if (!is.symbol(arguments[[.x]]) & !is.character(arguments[[.x]])) {
        arguments[[.x]] <- deparse_0(arguments[[.x]])
      } else {
        arguments[[.x]] <- arguments[[.x]]
      }
      if (is.symbol(arguments[[.x]]) &
          !is.character(arguments[[.x]])) {
        arguments[[.x]] <- deparse_0(arguments[[.x]])
      } else {
        arguments[[.x]] <- arguments[[.x]]
      }
      if (!is.character(.x)) {
        .xx <- eval(parse(text = .x))
      } else {
        .xx <- .x
      }
    }
    if (nys > 1) {
      .xx <- .x
    }
    if (!is.character(.xx) & !is.list(.xx)) {
      .xx <- deparse_0(.xx)
    } else {
      .xx <- .xx
    }
    . <- lapply(.xx, function(x)
      if (is.list(x))
        x <- x
      else
        x <- list(x)[[1]])
    assign(.x, ., envir = parent.frame())
  }
  
  eval_c_list_args <- function(.x, nys, arguments, ...) {
    if (is.language(arguments[[.x]]) &
        (strsplit(deparse_0(arguments[[.x]]), "\\(")[[1]][1] !=
         "c" &
         strsplit(deparse_0(arguments[[.x]]), "\\(")[[1]][1] != "list")) {
      arguments[[.x]] <- deparse(arguments[[.x]])
    } else {
      arguments[[.x]] <- (arguments[[.x]])
    }
    if (is.logical(arguments[[.x]])) {
      arguments[[.x]] <- deparse_0(arguments[[.x]])
    } else {
      arguments[[.x]] <- arguments[[.x]]
    }
    
    if (is.numeric(arguments[[.x]])) {
      arguments[[.x]] <- deparse_0(arguments[[.x]])
    } else {
      arguments[[.x]] <- (arguments[[.x]])
    }
    .xo <- .x
    .x <- arguments[[.x]]
    fun_ <- function(.x) {
      if (!is.character(.x))
        .x <- deparse_0(.x)
      else
        .x <- .x
      .x <- gsub("\\s", "", .x)
    }
    if (is.symbol(arguments[[.xo]]))
      .x <- deparse_0(.x)
    else
      .x <- .x
    if (is.symbol(arguments[[.xo]]))
      args_s <- mapply(fun_, .x)
    if (!is.symbol(arguments[[.xo]]))
      args_s <- mapply(fun_, .x)[-1]
    if (is.character(arguments[[.xo]]))
      args_s <- mapply(fun_, .x)
    if (length(args_s) > 1)
      args_s <- mapply(fun_, .x)[-1]
    attr(args_s, "names") <- NULL
    if (length(args_s) < nys)
      args_s <- rep(args_s, nys)
    if (length(args_s) > nys)
      args_s <- args_s[1:nys]
    assign(paste0(.xo, "s"), args_s, envir = parent.frame())
  }
  
  
  getArgNames <-
    function(value)
      formalArgs(deparse_0(substitute(value)[[1]]))
  
  convert_to_list <- getArgNames(bsitar())
  
  enverr. <- parent.frame()
  for (ip in convert_to_list) {
    if (grepl("_init_", ip)) {
      assign('err.', FALSE, envir = enverr.)
      tryCatch(
        expr = {
          out <- suppressWarnings(ept(ip))
        },
        error = function(e) {
          assign('err.', TRUE, envir = enverr.)
        }
      )
      err. <- get('err.', envir = enverr.)
      if (!err.) {
        if (length(out) > 1 & !is.list(out)) {
          stop(
            "Initials specified as vector [e.g, c(1, 2)] but must be a list, ",
            "\n ",
            " Note, initials can also be specified by using a single character",
            "\n ",
            " such as 0, random, or an object defined in the init_data",
            "\n ",
            " please check the following init arg: ",
            ip
          )
        }
      }
    }
  }
  
  
  # Convert arguments to the required format for setting sub-options 
  single_args <- c(
    "data",
    "group_arg",
    "sigma_group_arg",
    "univariate_by",
    "multivariate",
    "prior_data",
    "init_data",
    "init_custom",
    "jitter_init_beta",
    "jitter_init_sd",
    "jitter_init_cor",
    "expose_function",
    "verbose",
    "normalize",
    "seed",
    "brms_arguments",
    "get_stancode",
    "get_standata",
    "get_formula",
    "get_stanvars",
    "get_priors",
    "get_priors_eval",
    "validate_priors",
    "get_init_eval",
    "set_self_priors",
    "set_replace_priors",
    "set_same_priors_hierarchy",
    "outliers",
    "select_model",
    "decomp",
    "parameterization",
    "..."
  )
  
  
  
  for (i in convert_to_list) {
    if (!i %in% single_args) {
      to_list_if_not(i, nys, arguments)
    }
  }
  
  for (i in convert_to_list) {
    if (!i %in% single_args) {
      eval_c_list_args(i, nys, arguments)
    }
  }
  
  
  less_args <- extra_args <- c()
  outcomes_l <- paste0(" (", paste(ys, collapse = ", "), ")")
  for (i in convert_to_list) {
    .x <- i
    if (is.call(arguments[[.x]]))
      nl <- length(arguments[[.x]]) - 1
    if (!is.call(arguments[[.x]]))
      nl <- length(arguments[[.x]])
    if (nl > nys)
      extra_args <- c(extra_args, .x)
    if (nl < nys)
      less_args <- c(less_args, .x)
  }
  
  
  if (verbose) {
    setmsgtxt <- paste0("\n Preparing data")
    if (displayit == 'msg') {
      message(setmsgtxt)
    } else if (displayit == 'col') {
      col <- setcolh
      cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
    }
  }
  
  if(is.list(xfuns) & length(xfuns) == 0) {
    xfuns <- rep('NULL', length(ys))
  }
  if(is.list(yfuns) & length(yfuns) == 0) {
    yfuns <- rep('NULL', length(ys))
  }
  

  if(!is.null(outliers)) {
    if(is.null(outliers$remove))    outliers$remove <- TRUE
    if(is.null(outliers$icode))     outliers$icode <- c(4,5,6)
    if(is.null(outliers$limit))     outliers$limit <- 5
    if(is.null(outliers$velpower))  outliers$velpower <- 0.5
    if(is.null(outliers$lag))       outliers$lag <- 1
    if(is.null(outliers$linearise)) outliers$linearise <- FALSE
    if(is.null(outliers$verbose))   outliers$verbose <- FALSE
  }
  
  data.org.in <- data
  uvarby <- NULL
  data <- prepare_data(data = data,
                       x = xs, 
                       y = ys, 
                       id = ids,
                       uvarby = univariate_by$by, 
                       mvar = multivariate$mvar,
                       xfuns = xfuns, 
                       yfuns = yfuns,
                       outliers = outliers,
                       subset = FALSE)
  
  ys <- attr(data, "ys")
  subindicators <- attr(data, "subindicators")
  
  if (!is.na(univariate_by$by) & univariate_by$verbose) {
    resvcts_ <- levels(data[[univariate_by$by]])
    resvcts <- paste0(resvcts_, collapse = " ")
    setmsgtxt <- paste0(
      "\n For univariate-by-subgroup model fitting for variable '",
      uvarby,
      "'",
      " (specified via 'univariate_by' argument)",
      "\n ",
      resvcts,
      " response vectors created based on the factor levels",
      "\n\n ",
      "Please check corresponding arguments list.",
      " E.g, df = list(4, 5) denotes that\n df = 4 is for ",
      resvcts_[1],
      ", and  df = 5 is for ",
      resvcts_[2],
      " (and similalry knots, priors, initials etc)",
      "\n\n ",
      "If it does't correspond correctly, then either reverse the list ",
      "arguments\n such as df = list(5, 4),",
      " or else reverse sort the order of factor levels"
    )
    if (displayit == 'msg') {
      message(setmsgtxt)
    } else if (displayit == 'col') {
      col <- setcolb
      cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
    }
  }
  

  dataout <- priorlist <- NULL
  
  bflist <- list()
  bflist <- initialslist <- initialslist_s <- initsilist <- bflist
  blanketinitslist <- prior_stanvarlist <- auxillary_stanvarlist <- bflist
  
  funlist <- c()
  xoffsetvaluelist <- xoffsetnamelist <- knotsvaluelist <- funlist
  knotsnamelist <- spfun_collect <- xfunvaluelist <- xfunnamelist <- funlist
  yfunvaluelist <- yfunnamelist <- yyfunvaluelist <- yyfunnamelist <- funlist
  xxfunvaluelist <- xxfunnamelist <- fixedvaluelist <- fixednamelist <- funlist
  randomvaluelist <- randomnamelist <- groupvarvaluelist <- funlist
  yvarvaluelist <- ynamelist <- covvaluelist <- covnamelist <- funlist
  groupvarnamelist <- xvarvaluelist <- xnamelist <- funlist
  hierarchicalvarnamelist <- hierarchicalvarvaluelist <- funlist
  
  sigmacovnamelist <- sigmacovvaluelist <- funlist
  
  sigma_groupvarnamelist <- sigma_groupvarvaluelist <- funlist
  sigma_hierarchicalvarnamelist <- sigma_hierarchicalvarvaluelist <- funlist
  
  funlist_r <- funlist_rnamelist <- funlist_rvaluelist <- list()
  
  gq_funs <- list() 
  spfncname_c <- c()
  
  d_adjustedvaluelist <- d_adjustednamelist <- funlist
  
  # Start loop over response
  for (ii in 1:length(ys)) {
    if (nys > 1)
      resp <- ys[ii]
    else
      resp <- ""
    subindicatorsi <- subindicators[ii]
    
 
    for (i in convert_to_list) {
      if (!i %in% single_args) {
        assign(paste0(i, "s", "i"), eval(parse(text = paste0(i, "s")))[ii])
      }
    }
    
    
    if (is.null(group_arg$groupvar))
       group_arg$groupvar <- idsi
    
    
    if (is.null(sigma_group_arg$groupvar))
      sigma_group_arg$groupvar <- idsi
    
    
    if (!is.numeric(ept(dfsi)) & !is.numeric(ept(knotssi))) {
      stop("either df or knots must be specified")
    }
    if (is.numeric(ept(dfsi)) & is.numeric(ept(knotssi))) {
      stop("both df and knots specified. Specify one of them\n")
    }
    
    

    for (agsxi in letters[1:26]) {
      if(is.null(arguments[[paste0(agsxi, "_", "formula" , "")]])) {
        assign(paste0(agsxi, "_", "formula" , "si") , NULL)
        assign(paste0(agsxi, "_", "formula_gr" , "si") , NULL)
        assign(paste0(agsxi, "_", "formula_gr_str" , "si") , NULL)
        assign(paste0(agsxi, "_", "prior_beta" , "si") , NULL)
        assign(paste0(agsxi, "_", "cov_prior_beta" , "si") , NULL)
        assign(paste0(agsxi, "_", "prior_sd" , "si") , NULL)
        assign(paste0(agsxi, "_", "cov_prior_sd" , "si") , NULL)
        assign(paste0(agsxi, "_", "init_beta", "si" ) , NULL)
        assign(paste0(agsxi, "_", "cov_init_beta" , "si") , NULL)
        assign(paste0(agsxi, "_", "init_sd", "si" ) , NULL)
        assign(paste0(agsxi, "_", "cov_init_sd" , "si") , NULL)
      }
    }
    
    
    validate_fixed_random_parms <- function(fixedsi, 
                                            randomsi, 
                                            allowed_parm_letters, 
                                            select_model,
                                            match_sitar_d_form) {
      
      parm_letters_fixed <- strsplit(gsub("\\+", " ", fixedsi), " ")[[1]]
      parm_letters_fixed <- sort(parm_letters_fixed)
      parm_letters_fixed <- parm_letters_fixed[1:length(allowed_parm_letters)]
      parm_letters_fixed <- parm_letters_fixed[!is.na(parm_letters_fixed)]
      
      parm_letters_random <- strsplit(gsub("\\+", " ", randomsi), " ")[[1]]
      parm_letters_random <- sort(parm_letters_random)
      parm_letters_random <- parm_letters_random[1:length(allowed_parm_letters)]

      if(select_model == 'pb1' | 
         select_model == 'pb2' | 
         select_model == 'pb3' |
         select_model == 'logistic1' |
         select_model == 'logistic2' |
         select_model == 'logistic3' 
         ) {
        if(length(parm_letters_fixed) != length(allowed_parm_letters))
          stop("For model '", select_model, "'", ", 
               the number of parameters must be ",
               length(allowed_parm_letters),
               " \n ", 
               "(parameters ", 
               paste(paste0("'", allowed_parm_letters, "'"), collapse = " "),
               ")"
          )
      }
      
      if(select_model == 'sitar') {
        if(length(parm_letters_fixed) > length(allowed_parm_letters))
          stop("For model '", select_model, "'", ", 
               the maximum number of parameters is ",
               length(allowed_parm_letters),
               " \n ", 
               "(parameters ", 
               paste(paste0("'", allowed_parm_letters, "'"), collapse = " "),
               ")"
          )
      }
      
      get_parm_letters <- parm_letters_fixed
      sub_parm_letters_fixed <- intersect(allowed_parm_letters, 
                                          get_parm_letters)
      sub_parm_letters <- sub_parm_letters_fixed
      inv_parm_letters <- get_parm_letters[!get_parm_letters %in% 
                                             sub_parm_letters]
      inv_parm_letters <- sort(inv_parm_letters)
      if(length(inv_parm_letters) > 0) {
        see_what_formual <- paste0(" Please see and correctly ", "'", 
                                   'fixed', "'", " argument")
        not_allowed_parsm <- paste(paste0("'", inv_parm_letters, "'"), 
                                   collapse = " ")
        msg_1 <- paste0("Parameter ", not_allowed_parsm, 
                        " not allowed for ", "'", select_model, "'", " model")
        msg_2 <- paste0(" Allowed parameters are ", 
                        paste(paste0("'", 
                                     allowed_parm_letters, "'"), 
                              collapse = " "))
        stop(msg_1, "\n ", msg_2, " \n ", see_what_formual)
      }
      
      get_parm_letters <- parm_letters_random
      sub_parm_letters_random <- intersect(allowed_parm_letters, 
                                           get_parm_letters)
      sub_parm_letters <- sub_parm_letters_random
      inv_parm_letters <- get_parm_letters[!get_parm_letters %in% 
                                             sub_parm_letters]
      inv_parm_letters <- sort(inv_parm_letters)
      if(length(inv_parm_letters) > 0) {
        see_what_formual <- paste0(" Please see and correctly ", "'", 
                                   'random', "'", " argument")
        not_allowed_parsm <- paste(paste0("'", inv_parm_letters, "'"), 
                                   collapse = " ")
        msg_1 <- paste0("Parameter ", 
                        not_allowed_parsm, " not allowed for ", "'", 
                        select_model, "'", " model" )
        msg_2 <- paste0(" Allowed parameters are ", 
                        paste(paste0("'", 
                                     allowed_parm_letters, "'"), 
                              collapse = " "))
        stop(msg_1, "\n ", msg_2, " \n ", see_what_formual)
      }
      

      
      
      sub_parm_letters_fixed_random <- intersect(parm_letters_fixed, 
                                                 parm_letters_random)
      inv_parm_letters_fixed_random <- 
        parm_letters_random[!parm_letters_random %in% parm_letters_fixed]
      inv_parm_letters_fixed_random <- sort(inv_parm_letters_fixed_random)
      
      
      
      
      if(length(inv_parm_letters_fixed_random) > 0) {
        not_allowed_parsm <- paste(paste0("'", 
                                          inv_parm_letters_fixed_random, "'"), 
                                   collapse = " ")
        msg_mismatch_fixed_random_str <- 
          paste0(
            "Parameter ", not_allowed_parsm, " included in the random part of ",
            "\n ",
            " the model but missing from the fixed effects.",
            "\n ",
            " Please check and correctly specify the",
            "\n ",
            "'fixed'/'random' arguments for the ",
            toupper(select_model), " model."
          )
        
        if(select_model == 'sitar') {
          if(!match_sitar_d_form) stop(msg_mismatch_fixed_random_str)
        } else {
          stop(msg_mismatch_fixed_random_str)
        }
      } # if(length(inv_parm_letters_fixed_random) > 0) {
      
      
      sub_parm_letters_fixed <- sort(sub_parm_letters_fixed)
      sub_parm_letters_random <- sort(sub_parm_letters_random)
      
      out_fixed <- paste(sub_parm_letters_fixed, collapse = "+")
      out_random <- paste(sub_parm_letters_random, collapse = "+")
      list(fixed = out_fixed, random = out_random)
    } # validate_fixed_random_parms
    
    
    # Over ride when restricting to abcd
    if(override_select_model) {
      if(grepl("d", fixedsi) & grepl("d", randomsi)) {
        sitar_nparms <- 4
        match_sitar_d_form <- FALSE
      } else if(grepl("d", fixedsi) & !grepl("d", randomsi)) {
        sitar_nparms <- 4
        match_sitar_d_form <- FALSE
      } else if(!grepl("d", fixedsi) & grepl("d", randomsi)) {
        sitar_nparms <- 4
        match_sitar_d_form <- TRUE
      } else if(!grepl("d", fixedsi) & !grepl("d", randomsi)) {
        sitar_nparms <- 3
        match_sitar_d_form <- FALSE
      }
    }
    
    
    
    # Model specific number of fixed and random parameters
    
    allowed_parm_letters <- NULL
    if(select_model == 'sitar') allowed_parm_letters <- letters[1:sitar_nparms]
    if(select_model == 'pb1')   allowed_parm_letters <- letters[1:5]
    if(select_model == 'pb2')   allowed_parm_letters <- letters[1:6]
    if(select_model == 'pb3')   allowed_parm_letters <- letters[1:6]
    if(select_model == 'logistic1')   allowed_parm_letters <- letters[1:3]
    if(select_model == 'logistic2')   allowed_parm_letters <- letters[1:6]
    if(select_model == 'logistic3')   allowed_parm_letters <- letters[1:9]
    
    if(select_model == 'rcs')   allowed_parm_letters <- letters[1]
    
    
    fixedsi_randomsi <- 
      validate_fixed_random_parms(
        fixedsi = fixedsi, 
        randomsi = randomsi,
        allowed_parm_letters = allowed_parm_letters, 
        select_model = select_model,
        match_sitar_d_form = match_sitar_d_form)
    
    fixedsi <- fixedsi_randomsi[['fixed']]
    randomsi <- fixedsi_randomsi[['random']]
    
    
    # Covariate not allowed when matching to sitar 'd' form
    if(select_model == 'sitar') {
      if (!match_sitar_d_form) {
        if (!grepl("d", fixedsi, fixed = T) &
            grepl("d", randomsi, fixed = T)) {
          stop(
            "Parameter 'd' is missing in the fixed effects part of the model ",
            "\n ",
            " but specified in the random effects part of the model ",
            "\n ",
            " Either include 'd' in the fixed effects too or else ",
            "\n ",
            " remove it from the random effect part of the model"
          )
        }
      }
      
      
      if (match_sitar_d_form) {
        if ((grepl("d", fixedsi, fixed = T) |
             grepl("d", randomsi, fixed = T)) &
            (!grepl("^~1$", d_formulasi) |
             !grepl("^~1$", d_formula_grsi))) {
          stop(
            "Parameter 'd' is missing in the fixed effects part of the model ",
            "\n ",
            " but specified in the random effects part of the model ",
            "\n ",
            " (This is to match with the 'sitar' package's formulation)",
            "\n ",
            " For this formulation (i.e., 'd' is missing in the fixed effects)",
            "\n ",
            " covariate(s) are not allowed"
          )
        }
      }
    } # if(select_model == 'sitar') {
    
    

    if(select_model == 'sitar') {
      if(!any(grepl('s', fixedsi))) fixedsi <- paste0(fixedsi, "+", "s")
    }
    
    
    
    
    if(select_model == 'rcs') {
      if(!any(grepl('s', fixedsi))) fixedsi <- paste0(fixedsi, "+", "s")
      if(!any(grepl('s', randomsi)) & rcs_add_re_spline) {
          randomsi <- paste0(randomsi, "+", "s")
      }
      if(any(grepl('s', randomsi)) & !rcs_add_re_spline) {
        stop("you have specified select_model = 'rcsf' (i.e., no random",
             "\n ",
             "spline effects) but your random argument have parameter 's'.",
             "\n ",
             "Please check your 'select_model' and 'random' arguments")
      }
    }
    
    
    
    
    
    # Add missing parameters to the dpar_formula
    if (!is.null(dpar_formulasi)) {
      if (grepl("^1$", dpar_formulasi)) {
        dpar_formulasi <- paste0("lf(", "sigma", "~", dpar_formulasi, ")")
      } else if (grepl("^~1", dpar_formulasi)) { 
        dpar_formulasi <- paste0("lf(", "sigma", dpar_formulasi, ")")
      } else if (grepl("^sigma~1", dpar_formulasi)) { 
        dpar_formulasi <- paste0("lf(", "", dpar_formulasi, ")")
      } else {
        dpar_formulasi <- dpar_formulasi
      }
      if (grepl("lf\\(", dpar_formulasi) |
          grepl("nlf\\(", dpar_formulasi)) {
        if (grepl("^lf\\(", dpar_formulasi) &
            !grepl("nlf\\(", dpar_formulasi)) {
          lf_list <- c('flist',
                       'dpar',
                       'resp',
                       'center',
                       'cmc',
                       'sparse',
                       'decomp') #
        } else if (!grepl("^lf\\(", dpar_formulasi) &
                   grepl("^nlf\\(", dpar_formulasi)) {
          lf_list <- c('flist', 'dpar', 'resp', 'loop ') 
        }
        lf_list_c <- c()
        for (lf_listi in lf_list) {
          if (!grepl(lf_listi, dpar_formulasi)) {
            if (lf_listi == 'center') {
              o. <- paste0(lf_listi, "=", 'TRUE')
            } else if (lf_listi == 'cmc') {
              o. <- paste0(lf_listi, "=", 'TRUE')
            } else if (lf_listi == 'resp') {
              if (nys > 1) {
                o. <- paste0(lf_listi, "=", paste0("'", ysi, "'"))
                # o. <- paste0(lf_listi, "=", 'NULL')
              } else {
                o. <- paste0(lf_listi, "=", 'NULL')
              }
            } else {
              o. <- paste0(lf_listi, "=", 'NULL')
            }
            lf_list_c <- c(lf_list_c, o.)
          }
        }
        lf_list_c <- paste(lf_list_c, collapse = ",")
        if (lf_list_c != "")
          lf_list_c <- paste0(",", lf_list_c)
        dpar_formulasi <- gsub(")$", lf_list_c, dpar_formulasi)
        dpar_formulasi <- paste0(dpar_formulasi, ")")
      }
    }
    

    
    # Check for higher level model and update level 2 random formula
    f_checks_gr_gr_str <- function(a, b) {
      if(!is.null(a)) {
        gr_st_id <- sub(".*\\|", "", a) 
        a_ <- paste0("'", deparse_0(substitute(a)), "'")
        b_ <- paste0("'", deparse_0(substitute(b)), "'")
        b_out <- NULL
        if(is.null(b[[1]])) {
          if(grepl(":", gr_st_id, fixed = T) | 
             grepl("/", gr_st_id, fixed = T)) {
            stop("Models beyound two levels of hierarchy are not supported yet",
                 "\n ",
                 "An alternative to argument ", a_, " is to use ",
                 "\n ",
                 "argument ", b_, " to directly pass on the ", 
                 "\n ",
                 "random formula to the brms and then either accept",
                 "\n ",
                 "default priors placed by brms for those varinace covarinace", 
                 "\n ",
                 "or else use get_prios to place priors manually the pass ",
                 "\n ",
                 "by using argument 'set_self_priors'"
            )
          }
        } else if(!is.null(b[[1]])) {
          b_out <- b
        }
        out <- b_out
      } # if(!is.null(a)) {
      if(is.null(a)) {
        out <- NULL
      }
      out
    } # f_checks_gr_gr_str
    
    
   
    
    test_gr_sr_str_function <- function(x_grsi, x_gr_strsi) {
      if(!is.null(x_grsi)) {
        if(x_gr_strsi != 'NULL') {
          if(x_grsi == 'NULL') {
            x_grsi <- "~1"
          } else if(x_grsi != "~1") {
            if(verbose) {
              message("Argument '", 
                      substitute(x_grsi), "' changed from '", 
                      x_grsi , "' to  '~1'.")
              message("Instead of '", 
                      substitute(x_grsi), " = ", x_grsi, "', 
                    the covariates are now specified as '", 
                      substitute(x_gr_strsi), " = ", x_grsi, "'")
            }
            x_grsi <- "~1"
            
          } else {
            x_grsi <- x_grsi
          }
        } 
        out <- x_grsi
      } # if(!is.null(x_grsi)) {
      if(is.null(x_grsi)) {
        out <- NULL
      }
      out
    } 
    
    
    # Over ride when restricting to abcd
    if(!exists('s_formula_grsi')) s_formula_grsi <- NULL
    
    
    
    a_formula_grsi <- 
      test_gr_sr_str_function(a_formula_grsi, a_formula_gr_strsi)
    b_formula_grsi <- 
      test_gr_sr_str_function(b_formula_grsi, b_formula_gr_strsi)
    c_formula_grsi <- 
      test_gr_sr_str_function(c_formula_grsi, c_formula_gr_strsi)
    d_formula_grsi <- 
      test_gr_sr_str_function(d_formula_grsi, d_formula_gr_strsi)
    e_formula_grsi <- 
      test_gr_sr_str_function(e_formula_grsi, e_formula_gr_strsi)
    f_formula_grsi <- 
      test_gr_sr_str_function(f_formula_grsi, f_formula_gr_strsi)
    g_formula_grsi <- 
      test_gr_sr_str_function(g_formula_grsi, g_formula_gr_strsi)
    h_formula_grsi <- 
      test_gr_sr_str_function(h_formula_grsi, h_formula_gr_strsi)
    i_formula_grsi <- 
      test_gr_sr_str_function(i_formula_grsi, i_formula_gr_strsi)
    s_formula_grsi <- 
      test_gr_sr_str_function(s_formula_grsi, s_formula_gr_strsi)
    
    
    a_fcgs_out <- f_checks_gr_gr_str(a_formula_grsi, a_formula_gr_strsi)
    b_fcgs_out <- f_checks_gr_gr_str(b_formula_grsi, b_formula_gr_strsi)
    c_fcgs_out <- f_checks_gr_gr_str(c_formula_grsi, c_formula_gr_strsi)
    d_fcgs_out <- f_checks_gr_gr_str(d_formula_grsi, d_formula_gr_strsi)
    e_fcgs_out <- f_checks_gr_gr_str(e_formula_grsi, e_formula_gr_strsi)
    f_fcgs_out <- f_checks_gr_gr_str(f_formula_grsi, f_formula_gr_strsi)
    g_fcgs_out <- f_checks_gr_gr_str(g_formula_grsi, g_formula_gr_strsi)
    h_fcgs_out <- f_checks_gr_gr_str(h_formula_grsi, h_formula_gr_strsi)
    i_fcgs_out <- f_checks_gr_gr_str(i_formula_grsi, i_formula_gr_strsi)
    s_fcgs_out <- f_checks_gr_gr_str(s_formula_grsi, s_formula_gr_strsi)
    
    
    sigma_formula_grsi_NULL <- sigma_formula_gr_strsi_NULL <- FALSE
    if (is.null(sigma_formula_grsi[[1]][1]) |
        sigma_formula_grsi == "NULL") {
      sigma_formula_grsi_NULL <- TRUE
    }
    if (is.null(sigma_formula_gr_strsi[[1]][1]) |
        sigma_formula_gr_strsi == "NULL") {
      sigma_formula_gr_strsi_NULL <- TRUE
    }
    
    if(randomsi == "") {
      if (!sigma_formula_grsi_NULL |
          !sigma_formula_gr_strsi_NULL) {
        stop("Random effect for parameter 'sigma' are not allowed",
             " \n ",
             " if no group level random effect is specified i.e., random = ''",
             " \n ",
             " Therefore, 
             please set argument sigma_formula_gr/sigma_formula_gr_str to NULL")
      }
    }
    
  
    sigma_formula_grsi <- test_gr_sr_str_function(sigma_formula_grsi, 
                                                  sigma_formula_gr_strsi)
   
    
    if(sigma_formula_gr_strsi != 'NULL') {
      if(!grepl("^~", sigma_formula_gr_strsi)) {
        sigma_formula_gr_strsi <- paste0("~", sigma_formula_gr_strsi)
      }
    }
    if(is.null(sigma_formula_gr_strsi[[1]])) {
      sigma_formula_gr_strsi <- 'NULL'
    }
    
    if(sigma_formula_grsi != 'NULL') {
      if(!grepl("^~", sigma_formula_grsi)) {
        sigma_formula_grsi <- paste0("~", sigma_formula_grsi)
      }
    }
    if(is.null(sigma_formula_grsi[[1]])) {
      sigma_formula_grsi <- 'NULL'
    }
    

    sigma_fcgs_out <- f_checks_gr_gr_str(sigma_formula_grsi, 
                                         sigma_formula_gr_strsi)
    
    if(!is.null(a_fcgs_out)) {
      if(a_formula_grsi == "~1" & !is.null(a_formula_gr_strsi[[1]])) {
        a_formula_grsi <- strsplit(a_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    
    if(!is.null(b_fcgs_out)) {
      if(b_formula_grsi == "~1" & !is.null(b_formula_gr_strsi[[1]])) {
        b_formula_grsi <- strsplit(b_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    
    if(!is.null(c_fcgs_out)) {
      if(c_formula_grsi == "~1" & !is.null(c_formula_gr_strsi[[1]])) {
        c_formula_grsi <- strsplit(c_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    if(!is.null(d_fcgs_out)) {
      if(!is.null(d_formula_grsi[[1]]) & !is.null(d_formula_gr_strsi[[1]])) {
        if(d_formula_grsi == "~1" & !is.null(d_formula_gr_strsi[[1]])) {
          d_formula_grsi <- strsplit(d_formula_gr_strsi, 
                                     "+(", fixed = T)[[1]][1]
        }
      }
    }
    
    if(!is.null(e_fcgs_out)) {
      if(e_formula_grsi == "~1" & !is.null(e_formula_gr_strsi[[1]])) {
        e_formula_grsi <- strsplit(e_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    
    if(!is.null(f_fcgs_out)) {
      if(f_formula_grsi == "~1" & !is.null(f_formula_gr_strsi[[1]])) {
        f_formula_grsi <- strsplit(f_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    
    if(!is.null(g_fcgs_out)) {
      if(g_formula_grsi == "~1" & !is.null(g_formula_gr_strsi[[1]])) {
        g_formula_grsi <- strsplit(g_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    
    if(!is.null(h_fcgs_out)) {
      if(h_formula_grsi == "~1" & !is.null(h_formula_gr_strsi[[1]])) {
        h_formula_grsi <- strsplit(h_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    
    if(!is.null(i_fcgs_out)) {
      if(i_formula_grsi == "~1" & !is.null(i_formula_gr_strsi[[1]])) {
        i_formula_grsi <- strsplit(i_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    
    if(!is.null(s_fcgs_out)) {
      if(s_formula_grsi == "~1" & !is.null(s_formula_gr_strsi[[1]])) {
        s_formula_grsi <- strsplit(s_formula_gr_strsi, "+(", fixed = T)[[1]][1]
      }
    }
    
    
    
    if(!is.null(sigma_fcgs_out) & sigma_fcgs_out != 'NULL') {
      if(sigma_formula_grsi == "~1" & !is.null(sigma_formula_gr_strsi[[1]])) {
        sigma_formula_grsi <- strsplit(sigma_formula_gr_strsi, 
                                       "+(", fixed = T)[[1]][1]
      }
    }
    
    a_formula_grsi <- gsub("[()]", "", a_formula_grsi)
    b_formula_grsi <- gsub("[()]", "", b_formula_grsi)
    c_formula_grsi <- gsub("[()]", "", c_formula_grsi)
    if(!is.null(d_formula_grsi)) d_formula_grsi <- gsub("[()]", "", 
                                                        d_formula_grsi)
    e_formula_grsi <- gsub("[()]", "", e_formula_grsi)
    f_formula_grsi <- gsub("[()]", "", f_formula_grsi)
    g_formula_grsi <- gsub("[()]", "", g_formula_grsi)
    h_formula_grsi <- gsub("[()]", "", h_formula_grsi)
    i_formula_grsi <- gsub("[()]", "", i_formula_grsi)
    s_formula_grsi <- gsub("[()]", "", s_formula_grsi)
    
    
    sigma_formula_grsi <- gsub("[()]", "", sigma_formula_grsi)
 
    set_higher_levels <- TRUE
    if(is.null(a_fcgs_out) & 
       is.null(b_fcgs_out) & 
       is.null(c_fcgs_out) & 
       is.null(d_fcgs_out) &
       is.null(e_fcgs_out) &
       is.null(f_fcgs_out) &
       is.null(g_fcgs_out) &
       is.null(h_fcgs_out) &
       is.null(i_fcgs_out) &
       is.null(s_fcgs_out)
       ) {
      set_higher_levels <- FALSE
    }
    
    sigma_set_higher_levels <- TRUE
    if(is.null(sigma_fcgs_out) | sigma_fcgs_out == 'NULL') {
      sigma_set_higher_levels <- FALSE
    }
    
   
    
    check_formuals <-
      c(
        "a_formulasi",
        "b_formulasi",
        "c_formulasi",
        "d_formulasi",
        "e_formulasi",
        "f_formulasi",
        "g_formulasi",
        "h_formulasi",
        "i_formulasi",
        "s_formulasi",
        "a_formula_grsi",
        "b_formula_grsi",
        "c_formula_grsi",
        "d_formula_grsi",
        "e_formula_grsi",
        "f_formula_grsi",
        "g_formula_grsi",
        "h_formula_grsi",
        "i_formula_grsi",
        "s_formula_grsi",
        "sigma_formulasi",
        "sigma_formula_grsi"
      )
    
    for (check_formualsi in check_formuals) {
      if(!is.null(ept(check_formualsi)) & length(ept(check_formualsi)) !=0 ) {
        if (!grepl("~1", ept(check_formualsi)) &
            !grepl("~0", ept(check_formualsi))) {
          check_formualsi_with1 <-
            gsub("^~", "~1+", ept(check_formualsi), fixed = F)
          if(!grepl("^~", ept(check_formualsi))) {
            if(!grepl("^sigma", check_formualsi))
              check_formualsi_with1 <- paste0("~", check_formualsi_with1)
          }
          assign(check_formualsi, check_formualsi_with1)
        }
      } # if(!is.null(ept(check_formualsi))) {
      if(is.null(ept(check_formualsi)) | length(ept(check_formualsi)) ==0 ) {
        assign(check_formualsi, NULL)
      }
    } 
    
    
    if (is.null(sigma_formula_gr_strsi[[1]][1]) |
        sigma_formula_gr_strsi == "NULL") {
      sigma_formula_gr_strsi <- NULL
    }
    
    if (is.null(sigma_formula_grsi[[1]][1]) |
        sigma_formula_grsi == "NULL") {
      sigma_formula_grsi <- NULL
    }
    

    if (is.null(dpar_formulasi[[1]][1]) |
        dpar_formulasi == "NULL") {
      dpar_formulasi <- NULL
    }
    
    if (is.null(autocor_formulasi[[1]][1]) |
        autocor_formulasi == "NULL") {
      autocor_formi <- NULL
    } else {
      autocor_formulasi <- gsub("\"", "", autocor_formulasi)
      if(!grepl("^~", autocor_formulasi)) {
        stop('autocor_formula argument should be a formula. E.g.,',
             "\n ",
             " autocor_formula = ~arms(p=1,q=1)",
             "\n ", 
             " It seems you forgot to add '~' before the autocor structure")
      }
      autocor_formi <- autocor_formulasi
    } 
    

    if(!is.null(autocor_formi)) {
      tempunstx <- autocor_formi # '~unstr(time=visit, patient)'
      tempunstx <- gsub("[[:space:]]", "", tempunstx)
      if(grepl("unstr(", tempunstx, fixed = T)) {
        tempunstx_1 <- regmatches(tempunstx, gregexpr("(?<=\\().*?(?=\\))", 
                                                      tempunstx, perl=T))[[1]]
        tempunstx_2 <- strsplit(tempunstx_1, ",")[[1]][1]
        if(grepl("time=", tempunstx_2, fixed = T)) {
          tempunstx_3 <- sub(".*time=", "", tempunstx_2) 
        } else if(!grepl("time=", tempunstx_2, fixed = T)) {
          tempunstx_3 <- tempunstx_2
        }
        cortimeNlags_var <- tempunstx_3
      } # if(grepl("unstr(", tempunstx, fixed = T)) {
      
      if(!grepl("unstr(", tempunstx, fixed = T)) {
        cortimeNlags_var <- NULL
      }
    } 
      
    
    if(is.null(autocor_formi)) {
      cortimeNlags_var <- NULL
    }
    
   
    
    if (is.null(familysi[[1]][1]) |
        familysi == "NULL") {
      familysi <- NULL
    }
   
   
  # For backward compatibility if model fit using family = gaussian()
   if (!is.null(familysi)) {
     if(familysi == "gaussian()") {
       familysi <- "brms::brmsfamily(family = gaussian)"
     }
   }
    
   
    
    
    if (!is.null(familysi)) {
      familysi_check <- familysi
      if( grepl('brmsfamily', familysi_check) &
          !grepl('brms::', familysi_check)) {
        familysi_check <- paste0('brms::', familysi_check)
      } else if( grepl('brmsfamily', familysi_check) &
                 grepl('brms::', familysi_check)) {
        familysi_check <- familysi_check
      } else if(!grepl('brmsfamily', familysi_check)) {
        stop("Argument family should be specified as brmsfamily(family,...)")
      }
      familysi <- familysi_check
    }
  
    
    if (!is.null(familysi)) {
      familysi <- list_to_quoted_if_not_si(familysi)
    }
    
    
    if (!is.null(dpar_formulasi)) {
      if (grepl("^lf\\(", dpar_formulasi) |
          grepl("^nlf\\(", dpar_formulasi)) {
      } else {
        dpar_formulasi <- dpar_formulasi
      }
    }
    

    N_J_all <- length(unique(data[[idsi]]))
    
    if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
      sortbylayer <- NA
      data <- data %>%
        dplyr::mutate(sortbylayer =
                        forcats::fct_relevel(!!as.name(univariate_by$by),
                                             (levels(
                                               !!as.name(univariate_by$by)
                                             )))) %>%
        dplyr::arrange(sortbylayer) %>%
        dplyr::mutate(!!as.name(idsi) := factor(!!as.name(idsi),
                                                levels = 
                                                  unique(!!as.name(idsi)))) %>% 
        dplyr::select(-sortbylayer)
      
      
      datai <- data %>%
        dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>%
        droplevels()
      if (!subindicatorsi %in% colnames(datai)) {
        stop("variable ", subindicatorsi, " not in the dataframe")
      }
      if (!xsi %in% colnames(datai))
        stop("variable ", xsi, " not in the dataframe")
      if (!idsi %in% colnames(datai))
        stop("variable ", idsi, " not in the dataframe")
    }
    
    if ((is.na(univariate_by$by) | univariate_by$by == "NA")) {
      datai <- data %>%
        droplevels()
      if (!ysi %in% colnames(datai))
        stop("variable ", ysi, " not in the dataframe")
      if (!xsi %in% colnames(datai))
        stop("variable ", xsi, " not in the dataframe")
      if (!idsi %in% colnames(datai))
        stop("variable ", idsi, " not in the dataframe")
    }
    
    
    if(!is.null(cortimeNlags_var)) {
      if(!is.factor(datai[[cortimeNlags_var]])) {
        datai[[cortimeNlags_var]] <- as.factor(datai[[cortimeNlags_var]])
        datai[[cortimeNlags_var]] <- droplevels(datai[[cortimeNlags_var]])
      } else {
        datai[[cortimeNlags_var]] <-  datai[[cortimeNlags_var]]
      }
      cortimeNlags <- nlevels(unique(datai[[tempunstx_3]]))
    } else if(is.null(cortimeNlags_var)) {
      cortimeNlags <- NULL
    }
    

    if(is.null(parameterization)) {
      checkoccs <- datai %>% 
        dplyr::filter(!is.na(ysi)) %>% 
        droplevels() %>% 
        dplyr::mutate(nid = dplyr::n_distinct(idsi)) %>%
        dplyr::group_by_at(idsi) %>% 
        dplyr::mutate(NoccPI = dplyr::row_number()) %>% 
        dplyr::mutate(NoccAI = max(NoccPI)) %>% 
        dplyr::ungroup()
      
      if(min(checkoccs$NoccAI) >= 10) {
        parameterization = 'cp'
      } else {
        parameterization = 'ncp'
      }
    }
    

    
    fit_edited_scode <- FALSE
    
    if(select_model == 'logistic1e' |
       select_model == 'logistic2e' |
       select_model == 'logistic3e' |
       parameterization == 'cp'
    ) {
      
      fit_edited_scode <- TRUE
      
    }
    
    
    
    if (!is.null(xfunsi[[1]][1]) & xfunsi != "NULL") {
      if (xfunsi != "log" & xfunsi != "sqrt") {
        stop("only log and sqrt options allowed for xfun argument")
      }
    }
    
    
    if (!is.null(xfunsi[[1]][1]) & xfunsi != "NULL") {
      if (xfunsi == "log") {
        datai[[xsi]] <- log(datai[[xsi]])
      } else if (xfunsi == "sqrt") {
        datai[[xsi]] <- sqrt(datai[[xsi]])
      } else {
        stop("only log and sqrt options allowed for xfun argument")
      }
    }
    
    
    if (!is.null(xfunsi[[1]][1]) & xfunsi != "NULL") {
      xfunvalue <- xfunsi
    } else {
      xfunvalue <- NULL
    }
    
    if (!is.null(yfunsi[[1]][1]) & yfunsi != "NULL") {
      yfunvalue <- yfunsi
    } else {
      yfunvalue <- NULL
    }
    
    
    if (nys == 1) {
      xfun_name <- "xfun"
      yfun_name <- "yfun"
      xxfun_name <- "xvar_xfun"
      yyfun_name <- "yvar_yfun"
    } else if (nys > 1) {
      xfun_name <- paste0("xfun", "_", ysi)
      yfun_name <- paste0("yfun", "_", ysi)
      xxfun_name <- paste0("xvar_xfun", "_", ysi)
      yyfun_name <- paste0("yvar_yfun", "_", ysi)
    }
    
    xfunvaluelist[[ii]] <- xfunvalue
    xfunnamelist[[ii]] <- xfun_name
    
    yfunvaluelist[[ii]] <- yfunvalue
    yfunnamelist[[ii]] <- yfun_name
    
    if (!is.null(xfunsi[[1]][1]) & xfunsi != "NULL") {
      xxfunvaluelist[[ii]] <- paste0(xfunsi, "(", xsi, ")")
    } else {
      xxfunvaluelist[[ii]] <- NULL
    }
    
    if (!is.null(yfunsi[[1]][1]) & yfunsi != "NULL") {
      yyfunvaluelist[[ii]] <- paste0(yfunsi, "(", ysi, ")")
    } else {
      yyfunvaluelist[[ii]] <- NULL
    }
    
    xxfunnamelist[[ii]] <- xxfun_name
    yyfunnamelist[[ii]] <- xxfun_name
    
    gkn <- function(x, df, bounds) {
      c(min(x) - bounds * (max(x) - min(x)),
        quantile(x, (1:(df - 1)) / df),
        max(x) +
          bounds * (max(x) - min(x)))
    }
    
    if (is.numeric(ept(knotssi))) {
      knots <- ept(knotssi)
    }
    if (is.numeric(ept(dfsi))) {
      knots <- (unname(gkn(datai[[xsi]], ept(dfsi), ept(boundsi))))
    }
    
    
    if(select_model == "sitar") {
      if (match_sitar_d_form) {
        if (length(knots) > 2) {
          itemp <- strsplit(gsub("\\+", " ", fixedsi), " ")[[1]]
          itemp <- itemp[!grepl("d", itemp)]
          fixedsi <- paste(itemp, collapse = "+")
        }
      }
    }
    
  
    
    nabci <- length(strsplit(gsub("\\+", " ", fixedsi), " ")[[1]])
    nabcrei <-
      length(strsplit(gsub("\\+", " ", randomsi), " ")[[1]])
   
    make_spline_matrix <- function(x, knots) {
      X <- x
      N <- length(X)
      nk <- length(knots)
      basis_evals <- matrix(0, N, nk - 1)
      basis_evals[, 1] <- X
      basis_evals[, 1] <- X
      Xx <- matrix(0, N, nk)
      km1 <- nk - 1
      j <- 1
      knot1 <- knots[1]
      knotnk <- knots[nk]
      knotnk1 <- knots[nk - 1]
      kd <- (knotnk - knot1) ^ (2)
      for (ia in 1:N) {
        for (ja in 1:nk) {
          Xx[ia, ja] <- ifelse(X[ia] - knots[ja] > 0, X[ia] - knots[ja], 0)
        }
      }
      while (j <= nk - 2) {
        jp1 <- j + 1
        basis_evals[, jp1] <-
          (
            Xx[, j] ^ 3 - (Xx[, km1] ^ 3) * (knots[nk] - knots[j]) /
              (knots[nk] - knots[km1]) + (Xx[, nk] ^ 3) *
              (knots[km1] - knots[j]) / (knots[nk] - knots[km1])
          ) /
          (knots[nk] - knots[1]) ^ 2
        j <- j + 1
      }
      return(basis_evals)
    }
    
    
    
    eval_xoffset_bstart_args <-
      function(x, y, knots, data, eval_arg, xfunsi, arg = 'xoffset') {
        if (eval_arg == "mean") {
          eval_arg.o <- mean(data[[x]])
        } else if (eval_arg == "min") {
          eval_arg.o <- min(data[[x]])
        } else if (eval_arg == "max") {
          eval_arg.o <- max(data[[x]])
        } else if (eval_arg == "apv") {
          mat_s <- make_spline_matrix(data[[x]], knots)
          lmform <- as.formula(paste0(y, "~1+", "mat_s"))
          lmfit <- lm(lmform, data = data)
          eval_arg.o <- sitar::getPeak(data[[x]],
                                       predict(smooth.spline(data[[x]],
                                                             fitted(lmfit)),
                                               data[[x]], deriv = 1)$y)[1]
          if(is.na(eval_arg.o)) {
            stop(arg, " specified as '", eval_arg, "' returned NA.",
                 "\n ",
                 " Please change ", arg,
                 " argument to 'mean' or a numeric value.")
          }
        } else {
          eval_arg.o <- ept(eval_arg)
        }
        out <- as.numeric(eval_arg.o)
        out <- round(out, 3)
        return(out)
      }
    
    
    eval_xoffset_cstart_args <-
      function(x, y, knots, data, eval_arg, xfunsi) {
        if (eval_arg == "pv") {
          mat_s <- make_spline_matrix(data[[x]], knots)
          lmform <- as.formula(paste0(y, "~1+", "mat_s"))
          lmfit <- lm(lmform, data = data)
          eval_arg.o <- sitar::getPeak(data[[x]],
                                       predict(smooth.spline(data[[x]],
                                                             fitted(lmfit)),
                                               data[[x]], deriv = 1)$y)[2]
          if(is.na(eval_arg.o)) {
            stop("cstart specified as '", eval_arg, "' returned NA.",
                 "\n ",
                 " Please change cstart argument to 'mean' or a numeric value.")
          }
        } else {
          eval_arg.o <- ept(eval_arg)
        }
        out <- as.numeric(eval_arg.o)
        out <- round(out, 3)
        return(out)
      }
    
    
    
    if (xoffsetsi == 'NA' | xoffsetsi == '') {
      if(grepl('sitar', select_model) | grepl('rcs', select_model)) {
        xoffsetsi <- 'mean'
      } else {
        xoffsetsi <- 0
      }
    }
    
    if(bstartsi == 'xoffset') {
      bstartsi <- xoffsetsi
    }
    
    
    xoffset <-
      eval_xoffset_bstart_args(xsi, ysi, knots, datai, 
                               xoffsetsi, xfunsi, arg = 'xoffset')
    
    bstart <-
      eval_xoffset_bstart_args(xsi, ysi, knots, datai, 
                               bstartsi, xfunsi, arg = 'bstart')
    
    bstart <- bstart - xoffset
    
    cstart <-
      eval_xoffset_cstart_args(xsi, ysi, knots, datai, cstartsi, xfunsi)
    
    
    xoffset <- round(xoffset, 8)
    datai[[xsi]] <- datai[[xsi]] - xoffset
    knots <- knots - xoffset
    knots <- round(knots, 8)
    nknots <- length(knots)
    df <- length(knots) - 1
   
    mat_s <- make_spline_matrix(datai[[xsi]], knots)
    
    # Define names for Stan functions 
    SplineFun_name  <- paste0(toupper(select_model), "", 'Fun') # "DefFun" 
    getX_name       <- "getX"
    getKnots_name   <- "getKnots"
    
    
    if (nys > 1) {
      spfncname <- paste0(ysi, "_", SplineFun_name)
      getxname <- paste0(ysi, "_", getX_name)
      getknotsname <- paste0(ysi, "_", getKnots_name)
    } else if (nys == 1) {
      spfncname <- SplineFun_name
      getxname <- getX_name
      getknotsname <- getKnots_name
    }
    
    spfncname_c <- c(spfncname_c, spfncname)
    
    spfun_collect <-
      c(spfun_collect, c(spfncname, paste0(spfncname, "_", c("d1",
                                                             "d2"))))
    
    decomp_editcode <- FALSE
    if(select_model == 'rcs') {
      decomp_editcode <- FALSE
    }
    
    
    # For QR decomp to pass to prepare_function 
    # This to check s covs - re
    checkscovsi <-  getcovlist(s_formulasi)
    
    # This can be set to TRUE for RCS
    if(!is.null(checkscovsi)) {
      add_b_Qr_genquan_s_coef <- FALSE
      # if(select_model == 'rcs') add_b_Qr_genquan_s_coef <- TRUE
    } else {
      add_b_Qr_genquan_s_coef <- FALSE
    }
    
    # This control whether to add scode for genquant block for QR model
    # Relevant in both and prepare_function
    add_rcsfunmatqrinv_genquant <- FALSE # TRUE
    
    
    

    internal_function_args_names <-
      c(
        "fixedsi",
        "randomsi",
        "spfncname",
        "getxname",
        "getknotsname",
        'match_sitar_d_form',
        "d_adjustedsi",
        'xfunsi',
        'yfunsi',
        'xoffset',
        'brms_arguments',
        'select_model',
        'decomp', 
        'decomp_editcode',
        'nys',
        'checkscovsi',
        'add_b_Qr_genquan_s_coef',
        'add_rcsfunmatqrinv_genquant',
        "verbose"
      )
    
    internal_function_args <- list()
    internal_function_args <- mget(internal_function_args_names)
    
    if (verbose) {
      if (ii == 1) {
        setmsgtxt <- paste0("\n Preparing function")
        if (displayit == 'msg') {
          message(setmsgtxt)
        } else if (displayit == 'col') {
          col <- setcolh
          cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
        }
      }
    }
    
  
    
    
    get_s_r_funs <- 
      prepare_function(
      x = xsi,
      y = ysi,
      id = idsi,
      knots = knots,
      nknots = nknots,
      data = datai,
      internal_function_args = internal_function_args
    )
    
    funlist[ii] <- get_s_r_funs[['rcsfun']]
    funlist_r[[ii]] <- get_s_r_funs[['r_funs']]
    
    gq_funs[[ii]] <- get_s_r_funs[['gq_funs']]
    
    
    internal_formula_args_names <-
      c(
        "a_formulasi",
        "b_formulasi",
        "c_formulasi",
        "d_formulasi",
        "e_formulasi",
        "f_formulasi",
        "g_formulasi",
        "h_formulasi",
        "i_formulasi",
        "s_formulasi",
        "a_formula_grsi",
        "b_formula_grsi",
        "c_formula_grsi",
        "d_formula_grsi",
        "e_formula_grsi",
        "f_formula_grsi",
        "g_formula_grsi",
        "h_formula_grsi",
        "i_formula_grsi",
        "s_formula_grsi",
        "terms_rhssi",
        "sigma_formulasi",
        "sigma_formula_grsi",
        "dpar_formulasi",
        "autocor_formi",
        "subindicatorsi",
        "fixedsi",
        "randomsi",
        "univariate_by",
        "multivariate",
        "group_arg",
        "sigma_group_arg",
        "df",
        "mat_s",
        "spfncname",
        'nys',
        'ysi',
        'familysi',
        'xfunsi',
        'xoffset',
        'match_sitar_d_form',
        "a_formula_gr_strsi",
        "b_formula_gr_strsi",
        "c_formula_gr_strsi",
        "d_formula_gr_strsi",
        "e_formula_gr_strsi",
        "f_formula_gr_strsi",
        "g_formula_gr_strsi",
        "h_formula_gr_strsi",
        "i_formula_gr_strsi",
        "s_formula_gr_strsi",
        "sigma_formula_gr_strsi",
        "set_higher_levels",
        "sigma_set_higher_levels",
        "select_model",
        "verbose",
        "unusedsi"
      )
    
    
    internal_formula_args <- list()
    internal_formula_args <- mget(internal_formula_args_names)
    
    if (verbose) {
      if (ii == 1) {
        setmsgtxt <- paste0("\n Preparing formula")
        if (displayit == 'msg') {
          message(setmsgtxt)
        } else if (displayit == 'col') {
          col <- setcolh
          cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
        }
      }
    }
    
    formula_bf <-
      prepare_formula(
        x = xsi,
        y = ysi,
        id = idsi,
        knots = knots,
        nknots = nknots,
        data = datai,
        internal_formula_args = internal_formula_args
      )
    
    
    
    list_out <- attr(formula_bf, "list_out")
   
    attributes(formula_bf) <- NULL
    
    
    eout <- list2env(list_out)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
   
    group_arg$groupvar <- group_arg_groupvar
    multivariate$rescor <- multivariate_rescor
    univariate_by$by <- univariate_by_by
    covariates_ <- covariates_
    covariates_sigma_ <- covariates_sigma_
    set_higher_levels <- set_higher_levels
    
    sigma_set_higher_levels <- sigma_set_higher_levels
    
    
    sigma_group_arg$groupvar <- sigma_arg_groupvar
    
    
    lm_val_list <-
      names(eout)[grep(pattern = "^lm_|^lme_", names(eout))]
    lm_val_list <- sort(lm_val_list)
    
    lm_val_list_not <-
      names(eout)[!names(eout) %in%
                    names(eout)[grep(pattern = "^lm_|^lme_", names(eout))]]
    lm_val_list_not <- sort(lm_val_list_not)
    
    
    
    cov_list_names <- ls()[grepl(pattern = "_cov", ls())]
    cov_list_names <-
      cov_list_names[!grepl(pattern = "_init_", cov_list_names)]
    cov_list_names <-
      cov_list_names[!grepl(pattern = "_prior_", cov_list_names)]
    cov_list_names <-
      cov_list_names[!grepl(pattern = "^lm_", cov_list_names)]
    cov_list_names <- sort(cov_list_names)
    
    bflist[[ii]] <- formula_bf
    
    
    loess_fit <- paste0("loess(", ysi, "~", xsi, ",", 'datai', ")")
    loess_fitx <- eval(parse(text = loess_fit))
    
    
    ymean   <- mean(datai[[ysi]], na.rm = TRUE) %>% round(., 2)
    ymedian <- median(datai[[ysi]], na.rm = TRUE) %>% round(., 2)
    if(select_model == 'sitar' | select_model == 'rcs') {
      ymax  <- max(datai[[ysi]], na.rm = TRUE) %>% round(., 2)
      ymin  <- min(datai[[ysi]], na.rm = TRUE) %>% round(., 2)
      ymaxs <- NULL
    } else if(select_model != 'sitar' & select_model != 'rcs') {
      ymax <- round(max(predict(loess_fitx)), 2)
      ymin  <- min(datai[[ysi]], na.rm = TRUE) %>% round(., 2)
      ymaxs <- round(ymax * 0.95, 2)
    }
    
    ysd     <- sd(datai[[ysi]], na.rm = TRUE) %>% round(., 2)
    ymad    <- mad(datai[[ysi]], na.rm = TRUE) %>% round(., 2)
    xsd     <- sd(datai[[xsi]], na.rm = TRUE) %>% round(., 2)
    
    
    # This for logistic3 model
    ymeanxmin_ysdxmin <- 
      datai %>% dplyr::mutate(XXi := eval(parse(text = xsi))) %>% 
      dplyr::filter(XXi %in% 
                      (min(XXi):min(XXi)+0)) %>% 
      dplyr::mutate(onepic = 1) %>% dplyr::group_by(onepic) %>% 
      dplyr::mutate(temp1 = mean(eval(parse(text = ysi)))) %>% 
      dplyr::mutate(temp2 = sd(eval(parse(text = ysi)))) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(temp1, temp2) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      unlist() %>% as.numeric()
    
    ymeanxmin <- round(ymeanxmin_ysdxmin[1], 2)
    ysdxmin   <- round(ymeanxmin_ysdxmin[2], 2)
    
    ymeanxmax_ysdxmax <- 
      datai %>% dplyr::mutate(XXi := eval(parse(text = xsi))) %>% 
      dplyr::filter(XXi %in% 
                      (max(XXi):max(XXi)+0)) %>% 
      dplyr::mutate(onepic = 1) %>% dplyr::group_by(onepic) %>% 
      dplyr::mutate(temp1 = mean(eval(parse(text = ysi)))) %>% 
      dplyr::mutate(temp2 = sd(eval(parse(text = ysi)))) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(temp1, temp2) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      unlist() %>% as.numeric()
    
    ymeanxmax <- round(ymeanxmax_ysdxmax[1], 2)
    ysdxmax   <- round(ymeanxmax_ysdxmax[2], 2)
    
    ymeanxmid_ysdxmid <- 
      datai %>% dplyr::mutate(XXi := eval(parse(text = xsi))) %>% 
      dplyr::filter(XXi %in% 
                      (((max(XXi)-min(XXi))/2):
                         ((max(XXi)-min(XXi))/1.4))) %>% 
      dplyr::mutate(onepic = 1) %>% dplyr::group_by(onepic) %>% 
      dplyr::mutate(temp1 = mean(eval(parse(text = ysi)))) %>% 
      dplyr::mutate(temp2 = sd(eval(parse(text = ysi)))) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(temp1, temp2) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      unlist() %>% as.numeric()
    
    ymeanxmid <- round(ymeanxmid_ysdxmid[1], 2)
    ysdxmid   <- round(ymeanxmid_ysdxmid[2], 2)
    
    
    ymeanxmidxmaxdiff <- ymeanxmax - ymeanxmid
    ysdxmidxmaxdiff   <- (ysdxmax + ysdxmin) / 2
    
    ymeanxmidxmaxdiff <- round(ymeanxmidxmaxdiff, 2)
    ysdxmidxmaxdiff   <- round(ysdxmidxmaxdiff, 2)
    
    if(is.na(ymeanxmidxmaxdiff)) ymeanxmidxmaxdiff <- (ymeanxmax + ymeanxmin)/2
    
    ###
    # Add missing arguments when restricting to abcd
    if(is.null(pvsi))   pvsi  <- list(NULL)
    if(is.null(apvsi))  apvsi <- list(NULL)
    
    
    if (!is.null(pvsi[[1]][1]) & pvsi != "NULL") {
      setpv <- (eval(parse(text = pvsi)))
      # setpv <- log(eval(parse(text = pvsi)))
      if(grepl("sitar", select_model)) cstart <- setpv
      if(grepl("pb", select_model))    cstart <- setpv / 5.0
      if(grepl("pb", select_model))    dstart <- setpv
    } else if (!is.null(cstartsi[[1]][1]) & cstartsi != "NULL") {
      setpv <- (cstart)
      # setpv <- log(cstart)
      if(grepl("sitar", select_model)) cstart <- setpv
      if(grepl("pb", select_model))    cstart <- setpv / 5.0
      if(grepl("pb", select_model))    dstart <- setpv
    } else {
      if(grepl("sitar", select_model)) cstart <- 0.01
      if(grepl("pb", select_model))    cstart <- 0.01
      if(grepl("pb", select_model))    dstart <- 0.01 * 5.0
    }
    
    if (!is.null(apvsi[[1]][1]) & apvsi != "NULL") {
      setapv <- eval(parse(text = apvsi))
      if(grepl("sitar", select_model)) bstart <- setapv
      if(grepl("pb", select_model))    estart <- bstart
    } else if (!is.null(bstartsi[[1]][1]) & bstartsi != "NULL") {
      if(grepl("sitar", select_model)) bstart <- bstart
      if(grepl("pb", select_model))    estart <- bstart
    } else {
      if(grepl("sitar", select_model)) bstart <- 13
      if(grepl("pb", select_model))    estart <- 13
    }
    
    # Set missing start values to 0
    for (gwatxi in letters[1:26]) {
      gwatx__ <- paste0(gwatxi, 'start')
      if(!exists(gwatx__)) assign(gwatx__, 0)
    }
    
  
    
    # TODO
    # acov_sd etc setting numeric but later can be worked out to infer from
    # some meaningful way - until then, these will not be used in anywhere
    # and are included here just as placeholders
    lm_a_cov_sd <- 10
    lm_b_cov_sd <- 1
    lm_c_cov_sd <- 0.5
    
    
    vcov_init_0e <- eval(parse(text =  "vcov_init_0si" ))
    vcov_init_0e <- eval(parse(text =  vcov_init_0e ))
    
    # If vcov_init_0e = TRUE, override random options for below elements
    # Note that these are placeholders, actual setting to zero done later
    
    if(vcov_init_0e) {
      for (inxc in letters[1:26]) {
        assign(paste0(inxc, "_", "init", "_", "sd", "si"), '0')
        assign(paste0(inxc, "_", "cov", "_", "init", "_", "sd", "si"), '0')
      }
      assign('gr_init_corsi',         '0')
      assign('sigma_init_corsi',      '0')
      assign('rsd_init_sigmasi',      '0')
      assign('sigma_init_sdsi',       '0')
      assign('sigma_cov_init_sdsi',   '0')
      assign('dpar_init_sigmasi',     '0')
      assign('dpar_cov_init_sigmasi', '0')
      assign('mvr_init_rescorsi',     '0')
      assign('r_init_zsi',            '0')
    }
    
    
    
    prior_data_internal_names <-
      c(
        lm_val_list,
        "ymean",
        "ymedian",
        "ymax",
        "ymaxs",
        "ymin",
        "ysd",
        "ymad",
        "xsd",
        'ymeanxmin', 
        'ysdxmin', 
        'ymeanxmax', 
        'ysdxmax', 
        'ymeanxmid', 
        'ysdxmid',
        'ymeanxmidxmaxdiff',
        'ysdxmidxmaxdiff',
        "lm_a_cov_sd",
        "lm_b_cov_sd",
        "lm_c_cov_sd",
        "bstart",
        "cstart",
        "dstart",
        "estart"
      )
    
    
    prior_args_internal_names <-
      c(
        lm_val_list_not,
        cov_list_names,
        "a_formulasi",
        "b_formulasi",
        "c_formulasi",
        "d_formulasi",
        "e_formulasi",
        "f_formulasi",
        "g_formulasi",
        "h_formulasi",
        "i_formulasi",
        "s_formulasi",
        "a_formula_grsi",
        "fixedsi",
        "b_formula_grsi",
        "c_formula_grsi",
        "d_formula_grsi",
        "e_formula_grsi",
        "f_formula_grsi",
        "g_formula_grsi",
        "h_formula_grsi",
        "i_formula_grsi",
        "s_formula_grsi",
        "sigma_formulasi",
        "sigma_formula_grsi",
        "sigma_formula_gr_strsi",
        "autocor_formi",
        "randomsi",
        "nabci",
        "nabcrei",
        "univariate_by",
        "multivariate",
        "group_arg",
        "sigma_group_arg",
        "initsi",
        "df",
        "idsi",
        "ys",
        "resp",
        "ii",
        "nys",
        "N_J_all",
        "dpar_formulasi",
        "normalize",
        "seed",
        'match_sitar_d_form',
        'cortimeNlags_var',
        'cortimeNlags',
        "select_model",
        "verbose"
      )
    
    
    prior_data_internal <- list()
    prior_data_internal <- mget(prior_data_internal_names)
    
    
    prior_args_internal <- list()
    prior_args_internal <- mget(prior_args_internal_names)
    
    
    if (!is.null(prior_data[[1]])) {
      get_common_names_lists <-
        intersect(names(prior_data_internal), names(prior_data))
      ttt_n1 <- paste(names(prior_data_internal), collapse = ", ")
      ttt_nn2 <- paste(get_common_names_lists, collapse = ", ")
      if (!identical(get_common_names_lists, character(0))) {
        stop(
          "Names in prior_data list should not be following reserved names:",
          "\n ",
          ttt_n1,
          "\n ",
          " Please change the following name(s) ",
          "\n ",
          ttt_nn2
        )
      }
    }
    
    
    
    init_data_internal <- prior_data_internal
    init_args_internal <- prior_args_internal
    
    # Add if(!is.null(a_init_betasi)).. when restricting to abcd
    # check and set default initials (class = b)
    if(!is.null(a_init_betasi)) a_init_betasi <- 
      set_default_inits(select_model_arg, a_init_betasi)
    if(!is.null(b_init_betasi)) b_init_betasi <- 
      set_default_inits(select_model_arg, b_init_betasi)
    if(!is.null(c_init_betasi)) c_init_betasi <- 
      set_default_inits(select_model_arg, c_init_betasi)
    if(!is.null(d_init_betasi)) d_init_betasi <- 
      set_default_inits(select_model_arg, d_init_betasi)
    if(!is.null(e_init_betasi)) e_init_betasi <- 
      set_default_inits(select_model_arg, e_init_betasi)
    if(!is.null(f_init_betasi)) f_init_betasi <- 
      set_default_inits(select_model_arg, f_init_betasi)
    if(!is.null(g_init_betasi)) g_init_betasi <- 
      set_default_inits(select_model_arg, g_init_betasi)
    if(!is.null(h_init_betasi)) h_init_betasi <- 
      set_default_inits(select_model_arg, h_init_betasi)
    if(!is.null(i_init_betasi)) i_init_betasi <- 
      set_default_inits(select_model_arg, i_init_betasi)
    
    
    init_arguments <-
      list(
        a_init_beta = a_init_betasi,
        b_init_beta = b_init_betasi,
        c_init_beta = c_init_betasi,
        d_init_beta = d_init_betasi,
        e_init_beta = e_init_betasi,
        f_init_beta = f_init_betasi,
        g_init_beta = g_init_betasi,
        h_init_beta = h_init_betasi,
        i_init_beta = i_init_betasi,
        s_init_beta = s_init_betasi,
        a_cov_init_beta = a_cov_init_betasi,
        b_cov_init_beta = b_cov_init_betasi,
        c_cov_init_beta = c_cov_init_betasi,
        d_cov_init_beta = d_cov_init_betasi,
        e_cov_init_beta = e_cov_init_betasi,
        f_cov_init_beta = f_cov_init_betasi,
        g_cov_init_beta = g_cov_init_betasi,
        h_cov_init_beta = h_cov_init_betasi,
        i_cov_init_beta = i_cov_init_betasi,
        s_cov_init_beta = s_cov_init_betasi,
        a_init_sd = a_init_sdsi,
        b_init_sd = b_init_sdsi,
        c_init_sd = c_init_sdsi,
        d_init_sd = d_init_sdsi,
        e_init_sd = e_init_sdsi,
        f_init_sd = f_init_sdsi,
        g_init_sd = g_init_sdsi,
        h_init_sd = h_init_sdsi,
        i_init_sd = i_init_sdsi,
        s_init_sd = s_init_sdsi,
        a_cov_init_sd = a_cov_init_sdsi,
        b_cov_init_sd = b_cov_init_sdsi,
        c_cov_init_sd = c_cov_init_sdsi,
        d_cov_init_sd = d_cov_init_sdsi,
        e_cov_init_sd = e_cov_init_sdsi,
        f_cov_init_sd = f_cov_init_sdsi,
        g_cov_init_sd = g_cov_init_sdsi,
        h_cov_init_sd = h_cov_init_sdsi,
        i_cov_init_sd = i_cov_init_sdsi,
        s_cov_init_sd = s_cov_init_sdsi,
        sigma_init_beta = sigma_init_betasi,
        sigma_cov_init_beta = sigma_cov_init_betasi,
        sigma_init_sd = sigma_init_sdsi,
        sigma_cov_init_sd = sigma_cov_init_sdsi,
        rsd_init_sigma = rsd_init_sigmasi,
        dpar_init_sigma = dpar_init_sigmasi,
        dpar_cov_init_sigma = dpar_cov_init_sigmasi,
        autocor_init_acor = autocor_init_acorsi,
        autocor_init_unstr_acor = autocor_init_unstr_acorsi,
        gr_init_cor = gr_init_corsi,
        sigma_init_cor = sigma_init_corsi,
        mvr_init_rescor = mvr_init_rescorsi,
        r_init_z = r_init_zsi
      )
    
    
    if (verbose) {
      if (ii == 1) {
        setmsgtxt <- paste0("\n Preparing priors and initials")
        if (displayit == 'msg') {
          message(setmsgtxt)
        } else if (displayit == 'col') {
          col <- setcolh
          cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
        }
      }
    }
    
   
   
    # Add if(!is.null(a_prior_betasi)).. when restricting to abcd
    # check and set default priors (class = b)
    if(!is.null(a_prior_betasi)) a_prior_betasi <- 
      set_default_priors(select_model_arg, a_prior_betasi)
    if(!is.null(b_prior_betasi)) b_prior_betasi <- 
      set_default_priors(select_model_arg, b_prior_betasi)
    if(!is.null(c_prior_betasi)) c_prior_betasi <- 
      set_default_priors(select_model_arg, c_prior_betasi)
    if(!is.null(d_prior_betasi)) d_prior_betasi <- 
      set_default_priors(select_model_arg, d_prior_betasi)
    if(!is.null(e_prior_betasi)) e_prior_betasi <- 
      set_default_priors(select_model_arg, e_prior_betasi)
    if(!is.null(f_prior_betasi)) f_prior_betasi <- 
      set_default_priors(select_model_arg, f_prior_betasi)
    if(!is.null(g_prior_betasi)) g_prior_betasi <- 
      set_default_priors(select_model_arg, g_prior_betasi)
    if(!is.null(h_prior_betasi)) h_prior_betasi <- 
      set_default_priors(select_model_arg, h_prior_betasi)
    if(!is.null(i_prior_betasi)) i_prior_betasi <- 
      set_default_priors(select_model_arg, i_prior_betasi)
    
    
    # Add if(!is.null(a_prior_sdsi)).. when restricting to abcd
    # check and set default priors (class = sd)
    if(!is.null(a_prior_sdsi)) a_prior_sdsi <- 
      set_default_priors(select_model_arg, a_prior_sdsi)
    if(!is.null(b_prior_sdsi)) b_prior_sdsi <- 
      set_default_priors(select_model_arg, b_prior_sdsi)
    if(!is.null(c_prior_sdsi)) c_prior_sdsi <- 
      set_default_priors(select_model_arg, c_prior_sdsi)
    if(!is.null(d_prior_sdsi)) d_prior_sdsi <- 
      set_default_priors(select_model_arg, d_prior_sdsi)
    if(!is.null(e_prior_sdsi)) e_prior_sdsi <- 
      set_default_priors(select_model_arg, e_prior_sdsi)
    if(!is.null(f_prior_sdsi)) f_prior_sdsi <- 
      set_default_priors(select_model_arg, f_prior_sdsi)
    if(!is.null(g_prior_sdsi)) g_prior_sdsi <- 
      set_default_priors(select_model_arg, g_prior_sdsi)
    if(!is.null(h_prior_sdsi)) h_prior_sdsi <- 
      set_default_priors(select_model_arg, h_prior_sdsi)
    if(!is.null(i_prior_sdsi)) i_prior_sdsi <- 
      set_default_priors(select_model_arg, i_prior_sdsi)
    
    
    
    
    
    
    set_priors_initials_agrs <- list()

    set_priors_initials_agrs $ a_prior_beta <- a_prior_betasi
    set_priors_initials_agrs $ b_prior_beta <- b_prior_betasi
    set_priors_initials_agrs $ c_prior_beta <- c_prior_betasi
    set_priors_initials_agrs $ d_prior_beta <- d_prior_betasi
    set_priors_initials_agrs $ e_prior_beta <- e_prior_betasi
    set_priors_initials_agrs $ f_prior_beta <- f_prior_betasi
    set_priors_initials_agrs $ g_prior_beta <- g_prior_betasi
    set_priors_initials_agrs $ h_prior_beta <- h_prior_betasi
    set_priors_initials_agrs $ i_prior_beta <- i_prior_betasi
    set_priors_initials_agrs $ s_prior_beta <- s_prior_betasi
    
    set_priors_initials_agrs $ a_cov_prior_beta <- a_cov_prior_betasi
    set_priors_initials_agrs $ b_cov_prior_beta <- b_cov_prior_betasi
    set_priors_initials_agrs $ c_cov_prior_beta <- c_cov_prior_betasi
    set_priors_initials_agrs $ d_cov_prior_beta <- d_cov_prior_betasi
    set_priors_initials_agrs $ e_cov_prior_beta <- e_cov_prior_betasi
    set_priors_initials_agrs $ f_cov_prior_beta <- f_cov_prior_betasi
    set_priors_initials_agrs $ g_cov_prior_beta <- g_cov_prior_betasi
    set_priors_initials_agrs $ h_cov_prior_beta <- h_cov_prior_betasi
    set_priors_initials_agrs $ i_cov_prior_beta <- i_cov_prior_betasi
    set_priors_initials_agrs $ s_cov_prior_beta <- s_cov_prior_betasi
    
    set_priors_initials_agrs $ a_prior_sd <- a_prior_sdsi
    set_priors_initials_agrs $ b_prior_sd <- b_prior_sdsi
    set_priors_initials_agrs $ c_prior_sd <- c_prior_sdsi
    set_priors_initials_agrs $ d_prior_sd <- d_prior_sdsi
    set_priors_initials_agrs $ e_prior_sd <- e_prior_sdsi
    set_priors_initials_agrs $ f_prior_sd <- f_prior_sdsi
    set_priors_initials_agrs $ g_prior_sd <- g_prior_sdsi
    set_priors_initials_agrs $ h_prior_sd <- h_prior_sdsi
    set_priors_initials_agrs $ i_prior_sd <- i_prior_sdsi
    set_priors_initials_agrs $ s_prior_sd <- s_prior_sdsi
    
    set_priors_initials_agrs $ a_cov_prior_sd <- a_cov_prior_sdsi
    set_priors_initials_agrs $ b_cov_prior_sd <- b_cov_prior_sdsi
    set_priors_initials_agrs $ c_cov_prior_sd <- c_cov_prior_sdsi
    set_priors_initials_agrs $ d_cov_prior_sd <- d_cov_prior_sdsi
    set_priors_initials_agrs $ e_cov_prior_sd <- e_cov_prior_sdsi
    set_priors_initials_agrs $ f_cov_prior_sd <- f_cov_prior_sdsi
    set_priors_initials_agrs $ g_cov_prior_sd <- g_cov_prior_sdsi
    set_priors_initials_agrs $ h_cov_prior_sd <- h_cov_prior_sdsi
    set_priors_initials_agrs $ i_cov_prior_sd <- i_cov_prior_sdsi
    set_priors_initials_agrs $ s_cov_prior_sd <- s_cov_prior_sdsi
    
    set_priors_initials_agrs $ gr_prior_cor         <- gr_prior_corsi
    set_priors_initials_agrs $ sigma_prior_cor      <- sigma_prior_corsi
    set_priors_initials_agrs $ sigma_prior_beta     <- sigma_prior_betasi
    set_priors_initials_agrs $ sigma_cov_prior_beta <- sigma_cov_prior_betasi
    
    set_priors_initials_agrs $ sigma_prior_sd      <- sigma_prior_sdsi
    set_priors_initials_agrs $ sigma_cov_prior_sd  <- sigma_cov_prior_sdsi
    set_priors_initials_agrs $ rsd_prior_sigma     <- rsd_prior_sigmasi
    set_priors_initials_agrs $ dpar_prior_sigma    <- dpar_prior_sigmasi
    
  
    set_priors_initials_agrs $ dpar_cov_prior_sigma     <- 
      dpar_cov_prior_sigmasi
    set_priors_initials_agrs $ autocor_prior_acor       <- autocor_prior_acorsi
    set_priors_initials_agrs $ autocor_prior_unstr_acor <- 
      autocor_prior_unstr_acorsi
    set_priors_initials_agrs $ mvr_prior_rescor         <- mvr_prior_rescorsi
    set_priors_initials_agrs $ prior_data               <- prior_data
    set_priors_initials_agrs $ prior_data_internal      <- prior_data_internal
    set_priors_initials_agrs $ prior_args_internal      <- prior_args_internal
    set_priors_initials_agrs $ init_arguments           <- init_arguments
    set_priors_initials_agrs $ init_data                <- init_data
    set_priors_initials_agrs $ init_data_internal       <- init_data_internal
    set_priors_initials_agrs $ init_args_internal       <- init_args_internal
    set_priors_initials_agrs $ custom_order_prior_str   <- ""
    
    
    bpriors <- do.call(set_priors_initials, set_priors_initials_agrs)
    
    stanvar_priors <- attr(bpriors, "stanvars")
    
    initials <- attr(bpriors, "initials")
    
    # check and add hierarchical prior (for 3 level and more)
    # First, sd
    set_class_what <- 'sd'
    set_org_priors_initials_agrs_what <- set_priors_initials_agrs
    set_randomsi_higher_levsl <- strsplit(gsub("\\+", " ", randomsi), " ")[[1]]
    
    check_sigma_str <- 
      eval(parse(text = paste0('sigma', "covcoefnames_gr_str")))
    
    if(!is.null(check_sigma_str)) {
      set_randomsi_higher_levsl <- c(set_randomsi_higher_levsl, 'sigma')
    }
    
    evaluate_higher_level_sd_priors <- function(set_nlpar_, 
                                                set_class,
                                                set_prior,
                                                set_cov_prior,
                                                org_priors_initials_agrs,
                                                set_env,
                                                ...) {
      
      custom_order_prior_str <- c(paste0(set_nlpar_, "_prior_sd"),
                                  paste0(set_nlpar_, "_cov_prior_sd"))
     
      
      eval_what <- eval(parse(text = paste0(set_nlpar_, 
                                            "covcoefnames_gr_str")), 
                        envir = set_env_what)
      if(!is.null(eval_what)) {
        gr_str_id   <- eval(parse(text = paste0(set_nlpar_, 
                                                "covcoefnames_gr_str_id")), 
                            envir = set_env_what)
        gr_str_coef <- eval(parse(text = paste0(set_nlpar_, 
                                                "covcoefnames_gr_str")), 
                            envir = set_env_what)
        gr_str_form <- eval(parse(text = paste0(set_nlpar_, 
                                                "covcoefnames_gr_str_form")), 
                            envir = set_env_what)
        gr_str_ncov <- eval(parse(text = paste0(set_nlpar_, 
                                                "ncov_gr_str")), 
                            envir = set_env_what)
        temp_gr_str_priors <- list()
        temp_gr_str_stanvars <- c()
        temp_gr_str_inits <- c()
        set_priors_initials_agrs_str <- org_priors_initials_agrs 
        # this for adding _prior_cor 
        
       counter_start_from_one_for_prior <- 0
        for (istrx in 2:length(eval_what)) {
          counter_start_from_one_for_prior <- 
            counter_start_from_one_for_prior + 1
          if(set_nlpar_ == 'sigma') {
            assign('sigma_arg_groupvar', gr_str_id[[istrx]], 
                   envir = set_env_what)
          } else {
            assign('group_arg_groupvar', gr_str_id[[istrx]], 
                   envir = set_env_what)
          }
          
          assign( paste0(set_nlpar_, "_formula_grsi"), 
                  gr_str_form[[istrx]], envir = set_env_what)
          assign( paste0(set_nlpar_, "covcoefnames_gr"), 
                  gr_str_coef[[istrx]], envir = set_env_what)
          assign( paste0(set_nlpar_, "ncov_gr"), 
                  gr_str_coef[[istrx]], envir = set_env_what)
          
          prior_args_internal_str <- list()
          prior_args_internal_str <- mget(prior_args_internal_names, 
                                          envir = set_env_what)
          set_priors_initials_agrs_str $ prior_args_internal <- 
            prior_args_internal_str

          set_priors_initials_agrs_str $ custom_order_prior_str <- 
            custom_order_prior_str
       
          set_priors_initials_agrs_str [[paste0(set_nlpar_, 
                                                "_prior_sd")]]  <- 
            set_prior[counter_start_from_one_for_prior]
          set_priors_initials_agrs_str [[paste0(set_nlpar_, 
                                                "_cov_prior_sd")]] <- 
            set_cov_prior[counter_start_from_one_for_prior]

          bpriors_str <- do.call(set_priors_initials, 
                                 set_priors_initials_agrs_str, 
                                 envir = set_env_what)

          stanvars_str <- attr(bpriors_str, "stanvars")
          initials_str <- attr(bpriors_str, "initials")
          temp_gr_str_stanvars <- c(temp_gr_str_stanvars, stanvars_str)
          temp_gr_str_priors[[istrx]] <- bpriors_str
        }
        temp_gr_str_priors <- temp_gr_str_priors %>% do.call(rbind, .)
        out <- list(temp_gr_str_priors = temp_gr_str_priors,
                    temp_gr_str_stanvars = temp_gr_str_stanvars,
                    temp_gr_str_inits = temp_gr_str_inits)
      }
      out
    } 
    
    
    temp_gr_str_priors_sd <- list()
    temp_gr_str_stanvars_sd <-  temp_gr_str_inits_sd <- c()
    for (set_randomsi_higher_levsli in set_randomsi_higher_levsl) {
      set_nlpar_what <- set_randomsi_higher_levsli
      set_env_what   <- environment()
      n_higher_str   <- length(eval(parse(text = paste0(set_nlpar_what,
                                                        "covcoefnames_gr_str")),
                                    envir = set_env_what))
      n_higher_str   <- n_higher_str - 1
      
      if(n_higher_str > 0) {
        set_assign_prior_what <- '_prior'
        check_prior_ifp <- 
          extract_prior_str_lv(ept(paste0(set_nlpar_what, 
                                          paste0(set_assign_prior_what, 
                                                 "_", set_class_what, 
                                                 "_strsi"))))
        check_prior_ifp_true_false <- FALSE
        if(length(check_prior_ifp) == 1) {
          if(!is.null(ept(paste0(set_nlpar_what, 
                                 paste0(set_assign_prior_what, "_", 
                                        set_class_what, "_strsi") ))[[1]][1])) {
            check_prior_ifp_true_false <- TRUE 
          } else {
            check_prior_ifp_true_false <- FALSE
          }
          if(ept(paste0(set_nlpar_what, 
                        paste0(set_assign_prior_what, "_", 
                               set_class_what, "_strsi") )) != "NULL") {
            check_prior_ifp_true_false <- TRUE
          } else {
            check_prior_ifp_true_false <- FALSE
          }
        } else if(length(check_prior_ifp) > 1) {
          check_prior_ifp_true_false <- TRUE
        }
        if(check_prior_ifp_true_false) {
          assign(paste0(set_nlpar_what, paste0(set_assign_prior_what, "_",
                                               set_class_what, "si")),  
                 check_prior_ifp)
        }
        set_prior_what <- ept(paste0(set_nlpar_what, 
                                     paste0(set_assign_prior_what, "_", 
                                            set_class_what, "si") ))
        
        
        paste_message <- paste("Length of prior elements for random effect ",
             "'", set_nlpar_what, "'",
             " \n",
             "  specified by using the argument ", 
             "'", paste0(set_nlpar_what, 
                         paste0(set_assign_prior_what, "_", 
                                set_class_what, "_str")), "'",  " ",
             " \n",
             "  should be one or same as the levels of hierarchy minus one.",
             " \n",
             "  (minus one because the prior for the second level of hierarchy",
             " \n",
             "  is taken from the ", 
             "'", paste0(set_nlpar_what, paste0(set_assign_prior_what, 
                                                "_", set_class_what, "")), "'"
        )
        
        if(length(set_prior_what) > 1 & 
           length(set_prior_what) != n_higher_str) {
          stop(paste_message)
        } else if(length(set_prior_what) == 1) {
          set_prior_what <- rep(set_prior_what, n_higher_str)
        }
        paste_message <- NULL

        
        set_assign_prior_what <- '_cov_prior'
        check_prior_ifp <- 
          extract_prior_str_lv(ept(paste0(set_nlpar_what, 
                                          paste0(set_assign_prior_what, 
                                                 "_", set_class_what, 
                                                 "_strsi"))))
        check_prior_ifp_true_false <- FALSE
        if(length(check_prior_ifp) == 1) {
          if(!is.null(ept(paste0(set_nlpar_what, 
                                 paste0(set_assign_prior_what, "_", 
                                        set_class_what, "_strsi") ))[[1]][1])) {
            check_prior_ifp_true_false <- TRUE 
          } else {
            check_prior_ifp_true_false <- FALSE
          }
          if(ept(paste0(set_nlpar_what, 
                        paste0(set_assign_prior_what, "_", 
                               set_class_what, "_strsi") )) != "NULL") {
            check_prior_ifp_true_false <- TRUE
          } else {
            check_prior_ifp_true_false <- FALSE
          }
        } else if(length(check_prior_ifp) > 1) {
          check_prior_ifp_true_false <- TRUE
        }
        if(check_prior_ifp_true_false) {
          assign(paste0(set_nlpar_what, paste0(set_assign_prior_what, "_",
                                               set_class_what, "si")),  
                 check_prior_ifp)
        }
        set_cov_prior_what <- ept(paste0(set_nlpar_what, 
                                         paste0(set_assign_prior_what, "_", 
                                                set_class_what, "si") ))
        
        if(length(set_cov_prior_what) > 1 & 
           length(set_cov_prior_what) != n_higher_str) {
          stop("Length of prior elements for random effect parameter ",
               "'", set_nlpar_what, "'",
               " \n",
               "  specified by using the argument ", 
               "'", paste0(set_nlpar_what, 
                           paste0(set_assign_prior_what, "_", 
                                  set_class_what, "_str")), "'",  " ",
               " \n",
               "  should be one or same as the levels of hierarchy minus one.",
               " \n",
               "  (minus one because prior for the first level of hierarchy",
               " \n",
               "  is taken from the ", 
               "'", paste0(set_nlpar_what, 
                           paste0(set_assign_prior_what, "_", 
                                  set_class_what, "")), "'"
          )
        } else if(length(set_prior_what) == 1) {
          set_cov_prior_what <- rep(set_cov_prior_what, n_higher_str)
        }
       
       
        out2 <- evaluate_higher_level_sd_priors(set_nlpar_ = set_nlpar_what, 
                                        set_class  = set_class_what,
                                        set_prior = set_prior_what,
                                        set_cov_prior = set_cov_prior_what,
                                        set_env = set_env_what,
                                        org_priors_initials_agrs = 
                                          set_org_priors_initials_agrs_what)
      
      temp_gr_str_priors_sd[[set_randomsi_higher_levsli]] <- 
        out2 $ temp_gr_str_priors
      temp_gr_str_stanvars_sd <- 
        c(temp_gr_str_stanvars_sd, out2 $ temp_gr_str_stanvars)
      temp_gr_str_inits_sd <- 
        c(temp_gr_str_inits_sd,    out2 $ temp_gr_str_inits)
      } 
    } 
    
    higher_level_priors <- temp_gr_str_priors_sd %>% do.call(rbind, .)
    bpriors             <- rbind(bpriors, higher_level_priors)
    
    if(length(temp_gr_str_stanvars_sd) > 0) {
      stanvar_priors_c <- temp_gr_str_stanvars_sd_c <- c()
      for (i in 1:length(stanvar_priors)) {
        stanvar_priors_c <- c(stanvar_priors_c, stanvar_priors[i])
      }
      for (i in 1:length(temp_gr_str_stanvars_sd)) {
        temp_gr_str_stanvars_sd_c <- c(temp_gr_str_stanvars_sd_c, 
                                       temp_gr_str_stanvars_sd[i])
      }
      stanvar_priors <- c(stanvar_priors_c, temp_gr_str_stanvars_sd_c)
    } 
    
    if(length(temp_gr_str_inits_sd) > 0) {
      initials_c <- temp_gr_str_inits_sd_c <- c()
      for (i in 1:length(initials)) {
        initials_c <- c(initials_c, initials[i])
      }
      for (i in 1:length(temp_gr_str_inits_sd)) {
        temp_gr_str_inits_sd_c <- c(temp_gr_str_inits_sd_c, 
                                    temp_gr_str_inits_sd[i])
      }
      initials <- c(initials_c, temp_gr_str_inits_sd) 
    } 
    
    
    # Now, cor priors    
    # Adding cor priors is tricky because of complex |x| formulations
    
    set_class_what <- 'cor'
    set_org_priors_initials_agrs_what <- set_priors_initials_agrs
    set_randomsi_higher_levsl <- 'gr'
    
    check_sigma_str <- eval(parse(text = paste0('sigma', 
                                                "covcoefnames_gr_str")))
    
    if(!is.null(check_sigma_str)) {
      set_randomsi_higher_levsl <- c(set_randomsi_higher_levsl, 'sigma')
    }
    
    
    evaluate_higher_level_corr_priors <- function(set_nlpar_, 
                                                set_class,
                                                set_prior,
                                                id_higher_str = id_higher_str,
                                                corr_higher_str_tf = 
                                                  corr_higher_str_tf,
                                                org_priors_initials_agrs,
                                                set_env,
                                                ...) {
      
      custom_order_prior_str <- c(paste0(set_nlpar_, "_prior_cor"))
      
        temp_gr_str_priors <- list()
        temp_gr_str_stanvars <- c()
        temp_gr_str_inits <- c()

        set_priors_initials_agrs_str <- org_priors_initials_agrs 
        
        gr_str_id <- id_higher_str
        
        counter_start_from_one_for_prior <- 0
        for (istrx in 2:length(gr_str_id)) {
          counter_start_from_one_for_prior <- 
            counter_start_from_one_for_prior + 1
          get_corr_higher_str_tf <- corr_higher_str_tf[istrx]
                    if(get_corr_higher_str_tf) {
            if(set_nlpar_ == 'sigma') {
              assign('sigma_arg_groupvar', gr_str_id[istrx], 
                     envir = set_env_what)
              set_priors_initials_agrs_str $ sigma_prior_cor <- 
                set_prior[counter_start_from_one_for_prior]
            } else {
              assign('group_arg_groupvar', gr_str_id[istrx], 
                     envir = set_env_what)
              set_priors_initials_agrs_str $ gr_prior_cor <- 
                set_prior[counter_start_from_one_for_prior]
            }
            prior_args_internal_str <- list()
            prior_args_internal_str <- mget(prior_args_internal_names, 
                                            envir = set_env_what)
            set_priors_initials_agrs_str $ prior_args_internal <- 
              prior_args_internal_str
            
            set_priors_initials_agrs_str $ custom_order_prior_str <-
              custom_order_prior_str
            
            bpriors_str <- do.call(set_priors_initials, 
                                   set_priors_initials_agrs_str, 
                                   envir = set_env_what)
            stanvars_str <- attr(bpriors_str, "stanvars")
            initials_str <- attr(bpriors_str, "initials")
            temp_gr_str_stanvars <- c(temp_gr_str_stanvars, stanvars_str)
            temp_gr_str_inits    <- c(temp_gr_str_inits, initials_str)
            temp_gr_str_priors[[istrx]] <- bpriors_str
            
            # 26 12 2023
            bpriors_str_checks <- bpriors_str
            attr(bpriors_str_checks, "stanvars") <- NULL
            attr(bpriors_str_checks, "initials") <- NULL
            bpriors_str_checks <- bpriors_str_checks
            attributes(bpriors_str_checks) <- NULL
            if(!is.list(bpriors_str_checks)) {
              if(bpriors_str_checks == "") {
                temp_gr_str_priors[[istrx]] <- temp_gr_str_stanvars <- NULL
                temp_gr_str_inits <- NULL
              }
            }
            
          } # if(get_corr_higher_str_tf) {
          
          if(!get_corr_higher_str_tf) {
            temp_gr_str_priors[[istrx]] <- temp_gr_str_stanvars <- NULL
              temp_gr_str_inits <- NULL
          }
          
        } 
        
        temp_gr_str_priors <- temp_gr_str_priors %>% do.call(rbind, .)
       
        out <- list(temp_gr_str_priors = temp_gr_str_priors,
                    temp_gr_str_stanvars = temp_gr_str_stanvars,
                    temp_gr_str_inits = temp_gr_str_inits)
        
      out
    }
    

    temp_gr_str_priors_corr <- list()
    temp_gr_str_stanvars_corr <-  temp_gr_str_inits_corr <- c()

    for (set_randomsi_higher_levsli in set_randomsi_higher_levsl) {
      set_nlpar_what <- set_randomsi_higher_levsli
      set_env_what   <- environment()

      id_higher_str  <- eval(parse(text = paste0(set_nlpar_what, 
                                                 "_str_unique_id")), 
                             envir = set_env_what)
      
      n_higher_str   <- length(id_higher_str)
      n_higher_str   <- n_higher_str - 1
      corr_higher_str_tf <- eval(parse(text = paste0(set_nlpar_what, 
                                                     "_str_corr_tf")),
                                 envir = set_env_what)

      
      if(n_higher_str > 0) {
        set_assign_prior_what <- '_prior'
        check_prior_ifp <- 
          extract_prior_str_lv(ept(paste0(set_nlpar_what, 
                                          paste0(set_assign_prior_what, 
                                                 "_", set_class_what, 
                                                 "_strsi"))))
        check_prior_ifp_true_false <- FALSE
        if(length(check_prior_ifp) == 1) {
          if(!is.null(ept(paste0(set_nlpar_what, 
                                 paste0(set_assign_prior_what, "_", 
                                        set_class_what, "_strsi") ))[[1]][1])) {
            check_prior_ifp_true_false <- TRUE 
          } else {
            check_prior_ifp_true_false <- FALSE
          }
          if(ept(paste0(set_nlpar_what, 
                        paste0(set_assign_prior_what, "_", 
                               set_class_what, "_strsi") )) != "NULL") {
            check_prior_ifp_true_false <- TRUE
          } else {
            check_prior_ifp_true_false <- FALSE
          }
        } else if(length(check_prior_ifp) > 1) {
          check_prior_ifp_true_false <- TRUE
        }
        if(check_prior_ifp_true_false) {
          assign(paste0(set_nlpar_what, paste0(set_assign_prior_what, "_",
                                               set_class_what, "si")),  
                 check_prior_ifp)
        }
        
        set_prior_cor_what <- ept(paste0(set_nlpar_what, 
                                     paste0(set_assign_prior_what, "_", 
                                            set_class_what, "si") ))
        
        
        if(length(set_prior_cor_what) > 1 & 
           length(set_prior_cor_what) != n_higher_str) {
          stop("Length of prior elements for random effect parameter ",
               "'", set_prior_cor_what, "'",
               " \n",
               "  specified by using the argument ", 
               "'", paste0(set_prior_cor_what, paste0(set_assign_prior_what, 
                                                      "_", 
                                                      set_class_what, "_str")), 
               "'",  " ",
               " \n",
               "  should be one or same as the levels of hierarchy minus one.",
               " \n",
               "  (minus one because prior for the second level of hierarchy",
               " \n",
               "  is taken from the ", 
               "'", paste0(set_prior_cor_what, 
                           paste0(set_assign_prior_what, "_", 
                                  set_class_what, "")), "'"
          )
        } else if(length(set_prior_cor_what) == 1) {
          set_prior_cor_what <- rep(set_prior_cor_what, n_higher_str)
        }
       
        out2 <- evaluate_higher_level_corr_priors(
          set_nlpar_ = set_nlpar_what, 
          set_class  = set_class_what,
          set_prior = set_prior_cor_what,
          id_higher_str = id_higher_str,
          corr_higher_str_tf = corr_higher_str_tf,
          set_env = set_env_what,
          org_priors_initials_agrs = set_org_priors_initials_agrs_what
          )
       
        temp_gr_str_priors_corr[[set_randomsi_higher_levsli]] <- 
          out2 $ temp_gr_str_priors
        temp_gr_str_stanvars_corr <- 
          c(temp_gr_str_stanvars_corr, out2 $ temp_gr_str_stanvars)
        temp_gr_str_inits_corr <- 
          c(temp_gr_str_inits_corr,    out2 $ temp_gr_str_inits)
      } 
      
    } 
    
    
    
    higher_level_priors_corr <- temp_gr_str_priors_corr %>% do.call(rbind, .)
    bpriors                  <- rbind(bpriors, higher_level_priors_corr)
   
    
    if(length(temp_gr_str_stanvars_corr) > 0) {
      stanvar_priors_c <- temp_gr_str_stanvars_corr_c <- c()
      for (i in 1:length(stanvar_priors)) {
        stanvar_priors_c <- c(stanvar_priors_c, stanvar_priors[i])
      }
      for (i in 1:length(temp_gr_str_stanvars_corr)) {
        temp_gr_str_stanvars_corr_c <- c(temp_gr_str_stanvars_corr_c, 
                                         temp_gr_str_stanvars_corr[i])
      }
      stanvar_priors <- c(stanvar_priors_c, temp_gr_str_stanvars_corr_c)
    } 
    
    if(length(temp_gr_str_inits_corr) > 0) {
      initials_c <- temp_gr_str_inits_corr_c <- c()
      for (i in 1:length(initials)) {
        initials_c <- c(initials_c, initials[i])
      }
      for (i in 1:length(temp_gr_str_inits_corr)) {
        temp_gr_str_inits_corr_c <- c(temp_gr_str_inits_corr_c, 
                                      temp_gr_str_inits_corr[i])
      }
      initials <- c(initials_c, temp_gr_str_inits_corr) 
    } 
   
    
    priorlist <- rbind(priorlist, bpriors)
    stanvar_priors_names <- names(stanvar_priors)
    
    if(!"stanvars" %in% attr(stanvar_priors, 'class')) {
      attr(stanvar_priors, 'class') <- c("stanvars", 
                                         attr(stanvar_priors, 'class'))
    }
    
    prior_stanvarlist[[ii]] <- stanvar_priors 
    
    scode_auxillary <- attr(bpriors, "scode_auxillary")
    auxillary_stanvarlist[[ii]] <- scode_auxillary
    
    initialslist[[ii]] <- initials
    initialslist_s[[ii]] <- initsi
    
    if (initsi == "random") {
      blanketinits <- "random"
    } else if (initsi == "0") {
      blanketinits <- "0"
    } else {
      blanketinits <- "no"
    }
    
    blanketinitslist[[ii]] <- blanketinits
    
    
    if (nys == 1) {
      xoffset_name <- "xoffset"
      knots_name <- "knots"
      fixed_name <- "fixed"
      random_name <- "random"
      groupvar_name <- "groupvar"
      sigma_groupvar_name <- "sigma_groupvar"
      hierarchical_name <- "hierarchical"
      sigma_hierarchical_name <- "sigma_hierarchical"
      xvar_name <- "xvar"
      yvar_name <- "yvar"
      cov_name <- "cov"
      cov_name_sigma <- "cov_sigma"
      d_adjusted_name <- "d_adjusted"
    } else if (nys > 1) {
      xoffset_name <- paste0("xoffset", "_", ysi)
      knots_name <- paste0("knots", "_", ysi)
      fixed_name <- paste0("fixed", "_", ysi)
      random_name <- paste0("random", "_", ysi)
      groupvar_name <- paste0("groupvar", "_", ysi)
      sigma_groupvar_name <- paste0("sigma_groupvar", "_", ysi)
      hierarchical_name <- paste0("hierarchical", "_", ysi)
      sigma_hierarchical_name <- paste0("sigma_hierarchical", "_", ysi)
      xvar_name <- paste0("xvar", "_", ysi)
      yvar_name <- paste0("yvar", "_", ysi)
      cov_name <- paste0("cov", "_", ysi)
      cov_name_sigma <- paste0("cov_sigma", "_", ysi)
      d_adjusted_name <- paste0("d_adjusted", "_", ysi)
    }
    
    
    funlist_r_name <- 'funlist_r'
    funlist_rnamelist[[ii]] <- funlist_r_name
    funlist_rvaluelist[[ii]] <- funlist_r %>% unlist()
    
    xoffsetnamelist[[ii]] <- xoffset_name
    xoffsetvaluelist[[ii]] <- xoffset
    
    knotsnamelist[[ii]] <- knots_name
    knotsvaluelist[[ii]] <- knots
    
    fixednamelist[[ii]] <- fixed_name
    fixedvaluelist[[ii]] <-
      strsplit(gsub("\\+", " ", fixedsi), " ")[[1]]
    
    randomnamelist[[ii]] <- random_name
    randomvaluelist[[ii]] <-
      strsplit(gsub("\\+", " ", randomsi), " ")[[1]]
    
    
    groupvarnamelist[[ii]] <- groupvar_name
    groupvarvaluelist[[ii]] <- group_arg_groupvar
    
    
    sigma_groupvarnamelist[[ii]] <- sigma_groupvar_name
    sigma_groupvarvaluelist[[ii]] <- sigma_arg_groupvar
    
    hierarchicalvarnamelist[[ii]] <- hierarchical_name
    hierarchicalvarvaluelist[[ii]] <- hierarchical_gr_names
    
    sigma_hierarchicalvarnamelist[[ii]] <- sigma_hierarchical_name
    sigma_hierarchicalvarvaluelist[[ii]] <- sigma_hierarchical_gr_names
    
    xnamelist[[ii]] <- xvar_name
    xvarvaluelist[[ii]] <- xsi
    
    ynamelist[[ii]] <- yvar_name
    yvarvaluelist[[ii]] <- ysi
    
    covnamelist[[ii]] <- cov_name
    covvaluelist[[ii]] <- covariates_
    
    sigmacovnamelist[[ii]] <- cov_name_sigma
    sigmacovvaluelist[[ii]] <- covariates_sigma_
    
    d_adjustednamelist[[ii]] <- d_adjusted_name
    d_adjustedvaluelist[[ii]] <- ept(d_adjustedsi)
    
    
    if (!is.null(xfunsi[[1]][1]) & xfunsi != "NULL") {
      if (xfunsi == "log") {
        datai[[xsi]] <- exp(datai[[xsi]] + xoffset)
      } else if (xfunsi == "sqrt") {
        datai[[xsi]] <- (datai[[xsi]] + xoffset) ^ 2
      }
    } else if (is.null(xfunsi[[1]][1]) | xfunsi == "NULL") {
      datai[[xsi]] <- (datai[[xsi]] + xoffset)
    }
    
    if (!(is.na(univariate_by$by) | univariate_by$by == "NA"))
      dataout <- rbind(dataout, datai)
    else
      dataout <- datai

    
    if (!(is.na(univariate_by$by) | univariate_by$by == "NA"))
      uvarbyTF <- TRUE
    else
      uvarbyTF <- FALSE
  }  
  
  
  if (verbose) {
    if (multivariate$mvar) {
      setmsgtxt <-
        paste0(
          "\n Combining formula, function, priors and initials",
          "\n ",
          "for multivariate model fitting"
        )
    }
    if (!is.na(univariate_by$by)) {
      setmsgtxt <-
        paste0(
          "\n Combining formula, function, priors and initials",
          "\n ",
          "for univariate-by-subgroup model fitting"
        )
    }
    if (displayit == 'msg') {
      message(setmsgtxt)
    } else if (displayit == 'col') {
      col <- setcolh
      cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
    }
  }
  
  
  
  brmsdata <- dataout
  brmspriors <- priorlist
  
  # IMP - brms does not allow different lb for sd parsm (e.e, all to be NA)
  # Error: Conflicting boundary information for coefficients of class 'sd'.
  # Because prior function automatically sets lb 0 for positive priors 
  # such as exponential the following is need (again done at line 4753 )
  
  brmspriors <- brmspriors %>% 
    dplyr::mutate(lb = dplyr::if_else(class == 'sd', NA, lb))
  brmspriors <- brmspriors %>% 
    dplyr::mutate(ub = dplyr::if_else(class == 'sd', NA, ub))
  
  bflist_c_list <- list()
  bflist_c <- c()
  for (il in 1:length(bflist)) {
    bflist_c_list[[il]] <- ept(bflist[[il]])
    bflist_c <- c(bflist_c, paste0("bflist_c_list[[", il, "]]"))
  }
  
  bformula <- ept(paste(bflist_c, collapse = "+"))
  
  
  if (nys > 1) {
    if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
      bformula <- bformula + brms::set_rescor(FALSE)
    }
    if (multivariate$mvar && multivariate$rescor) {
      bformula <- bformula + brms::set_rescor(TRUE)
    }
    if (multivariate$mvar && !multivariate$rescor) {
      bformula <- bformula + brms::set_rescor(FALSE)
    }
  }
  
  
  fun_scode <- paste(funlist, collapse = "\n")
  fun_scode <- paste0("functions {", "\n", fun_scode, "\n", "}")

  bstanvars <-
    brms::stanvar(scode = paste(funlist, collapse = "\n"), block = "function")
  
  prior_stanvarlistlist <- c()
  for (i in 1:nys) {
    prior_stanvarlistlist[i] <- paste0("prior_stanvarlist[[", i, "]]")
  }
  
  bstanvars <-
    bstanvars + eval(parse(text = paste(prior_stanvarlistlist, 
                                        collapse = "+")))
  
  
  if (length(auxillary_stanvarlist) != 0) {
    auxillary_stanvarlistlist <- c()
    for (i in 1:nys) {
      auxillary_stanvarlistlist[i] <-
        paste0("auxillary_stanvarlist[[", i, "]]")
    }
    bstanvars <-
      bstanvars + eval(parse(text = paste(
        auxillary_stanvarlistlist, collapse = "+"
      )))
  }
  
  
  if (is.list(initialslist) & length(initialslist) == 0) {
    brmsinits <- NULL
  } else if (is.list(initialslist) & length(initialslist) > 0) {
    clistlist <- c()
    for (i in 1:length(initialslist)) {
      clistlist <- c(clistlist, ept(paste0("initialslist[[", i, "]]")))
    }
    brmsinits <- clistlist
  }
  

  
  if (!is.null(brmsinits)) {
    if (multivariate$mvar & multivariate$cor == "un") {
     
      c_it <- "sd_"
      brmsinits_names <- names(brmsinits)
      brmsinits_names <- brmsinits_names[!grepl('^_nu$|sd_nu', 
                                                brmsinits_names)]
      keys <- brmsinits_names[grepl(c_it, brmsinits_names)]
      temppp <- brmsinits[names(brmsinits) %in% keys]
      temppp <- unlist(unname(temppp))
      brmsinits <- brmsinits[!names(brmsinits) %in% keys]
      brmsinits[[keys[1]]] <- temppp %>%
        unname()
      
      c_it <- "L_"
      brmsinits_names <- names(brmsinits)
      keys <- brmsinits_names[grepl(c_it, brmsinits_names)]
      temppp <- brmsinits[names(brmsinits) %in% keys]
      brmsinits <- brmsinits[!names(brmsinits) %in% keys]
      t_names <- l_comb <- d_comb <- c()
      for (lnamei in names(temppp)) {
        t <- temppp[[lnamei]]
        l <- t[lower.tri(t)]
        names(l) <-
          apply(combn(colnames(t), 2), 2, paste, collapse = "_")
        d_comb <- c(d_comb, ncol(t))
        l_comb <- c(l_comb, l)
        t_names <- c(t_names, colnames(t))
      }
      
      create_cor_mat <- function(n, cor = NULL) {
        n_elements <- n
        m <- diag(n_elements)
        m_upper <- m_lower <- matrix(0, n_elements, n_elements)
        nc <- n_elements * (n_elements - 1) / 2
        if (is.null(cor)) {
          x <- rep(0, nc)
        } else {
          x <- cor
          if (length(x) != nc) {
            stop("length of correlation vector must be ",
                 nc,
                 "\n, ",
                 ", but found ",
                 length(x))
          }
        }
        m_lower[lower.tri(m_lower, diag = FALSE)] <- x
        m_upper <- t(m_lower)
        M <- m_lower + m + m_upper
        M
      }
      
      tt_names <- apply(combn(t_names, 2), 2, paste, collapse = "_")
      tt_dims <- sum(d_comb)
      tt_nc <- (tt_dims * (tt_dims - 1) / 2)
      tt_12 <- create_cor_mat(tt_dims, rep(0, tt_nc))
      colnames(tt_12) <- rownames(tt_12) <- t_names
      tt_ll <- tt_12[lower.tri(tt_12)]
      names(tt_ll) <-
        apply(combn(colnames(tt_12), 2), 2, paste, collapse = "_")
      tt_ll[names(l_comb)] <- l_comb
      tt_ll[!names(tt_ll) %in% names(l_comb)] <- 0
      brmsinits[[keys[1]]] <- create_cor_mat(tt_dims, tt_ll)
  
      c_it <- "z_"
      brmsinits_names <- names(brmsinits)
      keys <- brmsinits_names[grepl(c_it, brmsinits_names)]
      temppp <- brmsinits[names(brmsinits) %in% keys]
      brmsinits <- brmsinits[!names(brmsinits) %in% keys]
      brmsinits[[keys[1]]] <- do.call(rbind, temppp)
    } else if (multivariate$mvar &
               (multivariate$cor == "un" | multivariate$cor ==
                "un_s") &
               !any(grepl("^L_", names(brmsinits))))
    {
     
      c_it <- "sd_"
      brmsinits_names <- names(brmsinits)
      brmsinits_names <- brmsinits_names[!grepl('^_nu$|sd_nu', brmsinits_names)]
      keys <- brmsinits_names[grepl(c_it, brmsinits_names)]
      temppp <- brmsinits[names(brmsinits) %in% keys]
      temppp <- unlist(unname(temppp))
      brmsinits <- brmsinits[!names(brmsinits) %in% keys]
      for (sdi in 1:length(temppp)) {
        brmsinits[[paste0(c_it, sdi)]] <- temppp[sdi] %>%
          unname()
      }
    }  
    
    # keep only one Lrescor
    if (multivariate$mvar & multivariate$rescor) {
      c_it <- "Lrescor_"
      brmsinits_names <- names(brmsinits)
      keys <- brmsinits_names[grepl(c_it, brmsinits_names)]
      temppp <- brmsinits[names(brmsinits) %in% keys]
      brmsinits <- brmsinits[!names(brmsinits) %in% keys]
      brmsinits[["Lrescor"]] <- temppp[[1]]
    }
    
    if ((multivariate$mvar &
         multivariate$cor == "diagonal") |
        (!is.na(univariate_by$by) &
         univariate_by$cor == "diagonal") |
        group_arg$cor == "diagonal" |
        sigma_group_arg$cor == "diagonal") {
     
      c_it <- "sd_"
      brmsinits_names <- names(brmsinits)
      brmsinits_names <- brmsinits_names[!grepl('^_nu$|sd_nu', brmsinits_names)]
      keys <- brmsinits_names[grepl(c_it, brmsinits_names)]
      temppp <- brmsinits[names(brmsinits) %in% keys]
      temppp <- unlist(unname(temppp))
      brmsinits <- brmsinits[!names(brmsinits) %in% keys]
      xxx <- temppp
      ilc <- list()
      ilc_c <- 0
      for (nysi_ in 1:nys) {
        for (il in letters[1:26]) {
          ilc_c <- ilc_c + 1
          na <- paste0("^", il, nysi_)
          nb <- paste0("^", il, "cov", nysi_)
          nanb <- paste0(na, "|", nb)
          if (length(xxx[grepl(nanb, names(xxx))]) > 0) {
            ilc[[ilc_c]] <- xxx[grepl(nanb, names(xxx))]
          }
        }
      }
      
      if(!is_emptyx(ilc)) {
        ilc <- ilc[lengths(ilc) != 0]
        names(ilc) <- paste0("sd_", 1:length(ilc))
      }
      
      
      
      for (sdi in names(ilc)) {
        brmsinits[[sdi]] <- ilc[[sdi]]
      }
      
      
      c_it <- "z_"
      brmsinits_names <- names(brmsinits)
      keys <- brmsinits_names[grepl(c_it, brmsinits_names)]
      temppp <- brmsinits[names(brmsinits) %in% keys]
      
      if(!is_emptyx(ilc)) {
        for (zi in 1:length(ilc)) {
          brmsinits[[paste0(c_it, zi)]] <- temppp[[zi]]
        }
      }
      #
    }
  }  
  
  
  # For multivariate, it makes sense to keep initials for betas only otherwise
  # dimensional mismatch
  if (!is.null(brmsinits) & length(initialslist) != nys) {
    if (multivariate$mvar & multivariate$cor == "un") {
      c_it_names <- c("sd_", "L_", "z_", "Lrescor")
      for (c_it in c_it_names) {
        brmsinits_names <- names(brmsinits)
        brmsinits_names <- brmsinits_names[!grepl('^_nu$|sd_nu', 
                                                  brmsinits_names)]
        keys <- brmsinits_names[grepl(c_it, brmsinits_names)]
        brmsinits <- brmsinits[!names(brmsinits) %in% keys]
      }
    }
  }
  
  
  
  if (all(sapply("random", grepl, initialslist_s))) {
    brmsinits <- "random"
    brmsinits_r <- ept(init_rsi)
    brmsinits_ <- NULL
  } else if (all(sapply("0", grepl, initialslist_s))) {
    brmsinits <- "0"
    brmsinits_r <- ept(init_rsi)
    brmsinits_ <- NULL
  } else {
    brmsinits <- brmsinits
    brmsinits_r <- NULL
    brmsinits_ <- ""
  }
  
  
  for (inm in names(brmsinits)) {
    if (is.matrix(brmsinits[[inm]])) {
      colnames(brmsinits[[inm]]) <- rownames(brmsinits[[inm]]) <- NULL
      t__ <- brmsinits[[inm]]
      if (!is.null(attr(t__, "dimnames")))
        attr(t__, "dimnames") <- NULL
      brmsinits[[inm]] <- t__
    }
    if (is.vector(brmsinits[[inm]])) {
      t__ <- brmsinits[[inm]]
      if (!is.null(attr(t__, "names")))
        attr(t__, "names") <- NULL
      brmsinits[[inm]] <- t__
    }
  }
  
  
  
  if (!is.null(brmsinits_)) {
    eval_inits_fun <-
      function(inits,
               jitter_init_beta,
               jitter_init_sd,
               jitter_init_cor,
               digits) {
        if (is.character(jitter_init_beta))
          jitter_init_beta <- ept(jitter_init_beta)
        if (is.character(jitter_init_sd))
          jitter_init_sd <- ept(jitter_init_sd)
        if (is.character(jitter_init_cor))
          jitter_init_cor <- ept(jitter_init_cor)
        
        if (!is.null(jitter_init_beta) &
            !is.numeric(jitter_init_beta)) {
          stop("Argument jitter_init_beta should be NULL or a numeric value")
        }
        if (!is.null(jitter_init_sd) &
            !is.numeric(jitter_init_sd)) {
          stop("Argument jitter_init_sd should be NULL or a numeric value")
        }
        if (!is.null(jitter_init_cor) &
            !is.numeric(jitter_init_cor)) {
          stop("Argument jitter_init_cor should be NULL or a numeric value")
        }
        
        jitter_x <- function(x, a, digits) {
          x <- unname(x)
          col <- c()
          for (i in 1:length(x)) {
            amount <- abs(x[i]) * a
            col <- c(col, jitter(x[i], factor = 1, amount = amount))
          }
          col <- round(col, digits)
          col
        }
        
        jitter_mat <- function(x, a, digits) {
          mat_out <- x
          x <- x[lower.tri(x)]
          col <- c()
          for (i in 1:length(x)) {
            amount <- abs(x[i]) * a
            col <- c(col, jitter(x[i], factor = 1, amount = amount))
          }
          col <- round(col, digits)
          col <- ifelse(col > 1, 1, col)
          col <- ifelse(col < -1, 1, col)
          mat_out[lower.tri(mat_out)] <-
            mat_out[upper.tri(mat_out)] <- col
          return(mat_out)
        }
        
        eval_inits <- c()
        for (i_init in names(inits)) {
          if (grepl("^b_", i_init)) {
            if (!is.null(jitter_init_beta)) {
              values_i <-
                jitter_x(inits[[i_init]], jitter_init_beta, digits = digits)
            } else if (is.null(jitter_init_beta)) {
              values_i <- inits[[i_init]]
              values_i <- round(values_i, digits)
            }
            eval_inits[[i_init]] <- values_i
          } else if (grepl("^sd_", i_init)) {
            if (!is.null(jitter_init_sd)) {
              values_i <- jitter_x(inits[[i_init]], jitter_init_sd, digits)
              values_i <- abs(values_i)
              values_i <-
                ifelse(values_i <= 0, values_i + 0.01, values_i)
            } else if (is.null(jitter_init_sd)) {
              values_i <- inits[[i_init]]
              values_i <- abs(round(values_i, digits))
              values_i <-
                ifelse(values_i <= 0, values_i + 0.01, values_i)
            }
            eval_inits[[i_init]] <- values_i
          } else if (grepl("^L_", i_init)) {
            if (!is.null(jitter_init_cor)) {
              values_i <- jitter_mat(inits[[i_init]], jitter_init_cor, digits)
            } else if (is.null(jitter_init_cor)) {
              values_i <- inits[[i_init]]
              values_i <- round(values_i, digits)
            }
            eval_inits[[i_init]] <- values_i
          } else {
            eval_inits[[i_init]] <- inits[[i_init]]
          }
        }  # for(i_init in names(inits)) {
        eval_inits
        return(eval_inits)
      }
    
    
    temp_stancode2 <- brms::make_stancode(formula = bformula,
                                    stanvars = bstanvars,
                                    prior = brmspriors,
                                    data = brmsdata)
    temp_standata2 <- brms::make_standata(formula = bformula,
                                    stanvars = bstanvars,
                                    prior = brmspriors,
                                    data = brmsdata)
    
    

   
    move_from_model_to_qq_for_bqinv <- 
      function(temp_stancode2x, 
               section = 'model',
               replacemuby = NULL,
               spfncname_c = NULL,
               spfncname_c_vector = NULL,
               decomp_editcode = decomp_editcode) {
      regex_for_section <- 
        paste(".*(",section,"\\s*\\{.*?\\priors including constants).*", 
              sep = '')
      filtered_stan_code <- gsub(temp_stancode2x, pattern = regex_for_section, 
                                 replacement = "\\1")
      filtered_stan_code <- gsub('if (!prior_only) {', '', 
                                 filtered_stan_code, fixed = T)
      filtered_stan_code <- gsub('vector[N] mu;', '', 
                                 filtered_stan_code, fixed = T)
      filtered_stan_code <- gsub("target.*", '', filtered_stan_code, fixed = F)
      
      
      editedcode2 <- filtered_stan_code
      clines_p <- strsplit(filtered_stan_code, "\n")[[1]]
      for (il in clines_p) {
        editedcode2 <- gsub(pattern = "//", replacement = "//", 
                            x = editedcode2, fixed = T)
        editedcode2 <- gsub(pattern = "//[^\\\n]*", replacement = "", 
                            x = editedcode2)
        editedcode2 <- gsub(paste0(il, ""), "", editedcode2, fixed = T)
      }
      
      zz <- strsplit(editedcode2, "\n")[[1]]
      zz_c <- c()
      for (iz in 1:length(zz)) {
        if(!is_emptyx(gsub_space(zz[iz]))) {
          zz_in <- zz[iz]
          zz_in <- paste0("  ", zz_in)
          zz_c <- c(zz_c, zz_in)
        }
      }
      
      zz_c <- paste(zz_c, collapse = "\n")
      
      
      
      if(!decomp_editcode) {
        htx <- zz_c
        htx <- strsplit(htx, "\n")[[1]]
        nonmulines <- mulines <- c()
        for (htxi in htx) {
          tzx <- gsub("[[:space:]]", "", htxi)
          if(!grepl('mu', tzx)) {
            nonmulines <- c(nonmulines, htxi)
          } else if(grepl('mu', tzx)) {
            mulines <- c(mulines, htxi)
          }
        }
        nonmulines <- paste0(nonmulines, collapse = "\n")
        ##########
        
        lines_mu_subs <- c()
        for (htxi in mulines) {
            htxi_ <- gsub("[[:space:]]", "", htxi)
            if(grepl("=", htxi)) {
              htxi_ <- gsub("=(", "=", htxi_, fixed = T)
              htxi_ <- gsub("));", ");", htxi_, fixed = T)
              htxi_name <- strsplit(htxi_, "(", fixed = T)[[1]][1]
              htxi_2 <- regmatches(htxi_, gregexpr("(?<=\\().*?(?=\\))", 
                                                   htxi_, perl=T))[[1]]
              htxi_2 <- strsplit(htxi_2, ",", fixed = T)[[1]]
              htxi_3 <- c()
              nlp_s_number <- c()
              for (htxi_2i in htxi_2) {
                if(grepl('nlp_', htxi_2i) & grepl('_s', htxi_2i)) {
                  nlpsparms <- TRUE
                } else {
                  nlpsparms <- FALSE
                }
                if(!nlpsparms) htxi_3 <- c(htxi_3, htxi_2i)
                if(nlpsparms) nlp_s_number <- c(nlp_s_number, htxi_2i)
              }
              htxi_4 <- paste(htxi_3, collapse = ",")
              htxi_5 <- paste0(htxi_name, "(", htxi_4, ");")
              htxi_5 <- gsub("mu", " XR_inv", htxi_5)
              htxi_5 <- gsub("=", " = ", htxi_5)
              htxi_5 <- gsub(SplineFun_name, paste0(SplineFun_name, 
                                                    'QRsmatinv'), htxi_5)
              npsn   <- length(nlp_s_number)
              htxi_6 <- paste0("matrix[", npsn, ", ", npsn, "] ", htxi_5)
              lines_mu_subs <- c(lines_mu_subs, htxi_6)
            } # if(grepl("=", htxi)) {
        } # for (htxi in mulines) {
        lines_mu_subs <- paste0(lines_mu_subs, collapse = "\n")
        zz_c <- paste0(nonmulines, "\n", "  ", lines_mu_subs)
        zz_c2 <- zz_c
      }
        

      
      if(decomp_editcode) {
        zz_c_ <- strsplit(zz_c, "\n", fixed = T)[[1]]
        zz_c2 <- c()
        for (zz_ci in 1:length(zz_c_)) {
          if(!grepl('mu=', gsub_space(zz_c_[zz_ci])) ) {
            g <- zz_c_[zz_ci]
          }
          zz_c2 <- c(zz_c2, g)
        }
        zz_c2 <- paste(zz_c2, collapse = "\n")
      }
      
      return(zz_c2)
    }
    
   
    
    if(!is.null(decomp)) {
      if(add_rcsfunmatqrinv_genquant ) {
        temp_stancode_gqinv <- 
          brms::make_stancode(formula = bformula,
                              stanvars = bstanvars,
                              prior = brmspriors,
                              threads = brms::threading(NULL),
                              data = brmsdata)
        
        gq_funs_2 <- list()
        for (gq_funslen in 1:length(gq_funs)) {
          gq_funs_2[[gq_funslen]] <- gsub(gsub("\\;.*", "", 
                                               gq_funs[[gq_funslen]]),
                                          "", gq_funs[[gq_funslen]], 
                                          fixed = T)
        }
        
        qgcode <- 
          move_from_model_to_qq_for_bqinv(
            temp_stancode_gqinv, 
            replacemuby = NA, 
            spfncname_c = spfncname_c,
            spfncname_c_vector = spfncname_c_vector,
            decomp_editcode = decomp_editcode)
        
        
        qgcode <- gsub("\n  }\n  }", "\n  }", qgcode) 
        
        gq_funs_2 <- paste(unlist(gq_funs_2), collapse = "\n")
        gq_funs_2 <- paste0(qgcode, '\n', gq_funs_2)
        
        bstanvars <- bstanvars + 
          brms::stanvar(scode = gq_funs_2, block = "genquant", position = "end")
      } # if(add_rcsfunmatqrinv_genquant ) {
      
      
      
      if(decomp_editcode & add_rcsfunmatqrinv_genquant) {
        spfncname_c_mat <- c()
        spfncname_c_vector <- c()
        for (spfncname_ci in spfncname_c) {
          spfncname_ci2 <- gsub(spfncname_ci, 
                                paste0(spfncname_ci, 'QRsmat'), spfncname_ci)
          spfncname_invmat <- gsub(spfncname_ci, 
                                   paste0(spfncname_ci, 'QRsmatinv'), 
                                   spfncname_ci)
          waht_C <- 'C_1'
          waht_Cby <- paste0(waht_C, 'X')
          spfncname_c_vector <- c(spfncname_c_vector, waht_C)
          tdcode <- paste0('matrix[', 'N', ',', 4, ']', " ", waht_Cby, " = ",
                           spfncname_ci2, "(", waht_C, ",",
                           'rep_vector(0.0, num_elements(', waht_C, '))',
                           ");")
          waht_Cby_inv <- 'XR_inv'
          tdcode_inv <- paste0('matrix[', 4, ',', 4, ']', " ", 
                               waht_Cby_inv, " = ",
                               spfncname_invmat, "(", waht_C, ",",
                               'rep_vector(0.0, num_elements(', waht_C, '))',
                               ");")
          
          tdcode <- paste0(tdcode, "\n", tdcode_inv)
          spfncname_c_mat <- c(spfncname_c_mat, tdcode)
        }
        spfncname_c_mat2 <- paste(spfncname_c_mat, collapse = "\n")
        bstanvars <- bstanvars + brms::stanvar(scode = spfncname_c_mat2, 
                                               block = "tdata", 
                                               position = "end")
      } # if(decomp_editcode & add_rcsfunmatqrinv_genquant) {
    } # if(!is.null(decomp)) {
    
    
   
    
    
    
    if(vcov_init_0e) {
      initialsx2 <- brmsinits
      for (initialsi in names(initialsx2)) {
        if(grepl("sd_", initialsi)) {
          if(!grepl("sd_nu", initialsi, fixed = T)) {
            initialsx2[[initialsi]] <- NULL
            newinits <- set_init_gr_effects(temp_stancode2, 
                                            temp_standata2,
                                            parameterization = parameterization,
                                            what = 'sd')
            initialsx2 <- c(initialsx2, newinits)
          }
        }
        if(grepl("L_", initialsi)) {
          initialsx2[[initialsi]] <- NULL
          newinits <- set_init_gr_effects(temp_stancode2, 
                                          temp_standata2, 
                                          parameterization = parameterization,
                                          what = 'L')
          initialsx2 <- c(initialsx2, newinits)
        }
        if(grepl("z_", initialsi)) {
          initialsx2[[initialsi]] <- NULL
          newinits <- set_init_gr_effects(temp_stancode2, 
                                          temp_standata2, 
                                          parameterization = parameterization,
                                          what = 'z')
          initialsx2 <- c(initialsx2, newinits)
        }
      }
      uni_name <- unique(names(initialsx2))
      initialsx2 <- initialsx2[uni_name] 
      brmsinits <- initialsx2
    } 
    
    
    
    
    if(vcov_init_0e) {
      if(parameterization == 'cp') {
        initialsx2 <- brmsinits
        temp_stancode2cp <- edit_scode_ncp_to_cp(temp_stancode2, 
                                                 genq_only = FALSE, 
                                                 normalize = normalize)
        
        newinits <- set_init_gr_effects(temp_stancode2cp, 
                                        temp_standata2, 
                                        parameterization = parameterization,
                                        what = 'r')
        initialsx2 <- c(initialsx2, newinits)
        uni_name <- unique(names(initialsx2))
        initialsx2 <- initialsx2[uni_name] 
        brmsinits <- initialsx2
      }
    }
    
    
    
    brmsinits <- lapply(1:brms_arguments$chains, function(id) {
      eval_inits_fun(
        inits = brmsinits,
        jitter_init_beta = jitter_init_beta,
        jitter_init_sd = jitter_init_sd,
        jitter_init_cor = jitter_init_cor,
        digits = 4
      )
    })
  }
  
  
  
  

  # Add stanvars for logistic3e
  if(select_model_edit == 'logistic3e') {
    temp_stancode_logistic3e <- brms::make_stancode(formula = bformula,
                                          stanvars = bstanvars,
                                          prior = brmspriors,
                                          data = brmsdata)
    temp_standata_logistic3e <- brms::make_standata(formula = bformula,
                                                    stanvars = bstanvars,
                                                    prior = brmspriors,
                                                    data = brmsdata)
    
    
    check_p_dimes <- c()
    for (clines_tpi in names(temp_standata_logistic3e)) {
      for (igr in 1:9) {
        if(grepl(paste0("^K", "_"), clines_tpi) & 
           grepl(paste0("_", letters[igr]), clines_tpi)) {
           temd <- temp_standata_logistic3e[[clines_tpi]]
          check_p_dimes <- c(check_p_dimes, temd)
        }
      }
    }
    
    if(!all(check_p_dimes==check_p_dimes[1])) {
      stop('All parameters must have the same number of parameters')
    }
    
    
    check_p_attr1 <- c()
    for (clines_tpi in names(temp_standata_logistic3e)) {
      for (igr in 1:9) {
        if(grepl(paste0("^X", "_"), clines_tpi) & 
           grepl(paste0("_", letters[igr]), clines_tpi)) {
          temd <- temp_standata_logistic3e[[clines_tpi]]
          temd2 <- attr(temd, "assign")[1]
          check_p_attr1 <- c(check_p_attr1, temd2)
        }
      }
    }
    
    
    if(check_p_dimes[1] > 1) {
      if(any(check_p_attr1 == 0)) {
        stop('All parameters must have the covariate form as ~0+')
      }
    }
    
    
    outlogistic3e <- edit_scode_for_logistic3(temp_stancode_logistic3e, 
                                              normalize = normalize)
    
    bstanvars <- bstanvars + brms::stanvar(scode = outlogistic3e$pcode, 
                                           block = "parameters", 
                                           position = "start")
    
    bstanvars <- bstanvars + brms::stanvar(scode = outlogistic3e$fcode, 
                                           block = "functions")
    
    edit_ncov  <- as.integer(check_p_dimes[1])
    edit_npar  <- as.integer(3)
    edit_min_d <- array(rep(0, edit_ncov), dim = edit_ncov)
    edit_max_d <- array(rep(500,  edit_ncov), dim = edit_ncov)
    edit_min_v <- array(rep(0, edit_ncov), dim = edit_ncov)
    edit_max_v <- array(rep(2.5,    edit_ncov), dim = edit_ncov)
    edit_min_t <- array(rep(0,    edit_ncov), dim = edit_ncov)
    edit_max_t <- array(rep(18,   edit_ncov), dim = edit_ncov)
    bstanvars <- bstanvars + brms::stanvar(x = edit_ncov, name = 'Kedit',
                                           block = "data")
    bstanvars <- bstanvars + brms::stanvar(x = edit_npar, name = 'Cedit',
                                           block = "data")
    bstanvars <- bstanvars + brms::stanvar(x = edit_min_d, name = 'min_d',
                                           block = "data")
    bstanvars <- bstanvars + brms::stanvar(x = edit_max_d, name = 'max_d',
                                           block = "data")
    bstanvars <- bstanvars + brms::stanvar(x = edit_min_v, name = 'min_v',
                                           block = "data")
    bstanvars <- bstanvars + brms::stanvar(x = edit_max_v, name = 'max_v',
                                           block = "data")
    bstanvars <- bstanvars + brms::stanvar(x = edit_min_t, name = 'min_t',
                                           block = "data")
    bstanvars <- bstanvars + brms::stanvar(x = edit_max_t, name = 'max_t',
                                           block = "data")
    
  } # if(select_model_edit == 'logistic3e') {
  
  
  
  
  
  
  
  # Set brm arguments
  setup_brms_args <-
    function(formula,
             prior,
             stanvars,
             data,
             init_set,
             init_str,
             init_r,
             seed,
             verbose,
             setarguments,
             brmsdots) {
      
      exc_args <- c("formula", "prior", "stanvars", "init", "data")
      if (eval(setarguments$backend) == "rstan")
        exc_args <- c(exc_args, "stan_model_args")
      for (exc_argsi in exc_args) {
        if (exc_argsi %in% names(setarguments))
          setarguments[[exc_argsi]] <- NULL
      }
      setarguments$formula <- formula
      setarguments$prior <- prior
      setarguments$stanvars <- stanvars
      setarguments$data <- data
      
      if (eval(setarguments$backend) == "cmdstanr") {
        if (all(sapply("0", grepl, init_str))) {
          setarguments$init <- 0
          custom_init <- FALSE
        } else if (all(sapply("random", grepl, init_str))) {
          setarguments$init <- NULL
          custom_init <- FALSE
        } else {
          setarguments$init <- init_set
          custom_init <- TRUE
        }
        if (!custom_init & !is.null(init_r)) {
          setarguments$init <- init_r
        }
      }
      
      if (eval(setarguments$backend) == "rstan") {
        if (all(sapply("0", grepl, init_str))) {
          setarguments$init <- "0"
          custom_init <- FALSE
        } else if (all(sapply("random", grepl, init_str))) {
          setarguments$init <- "random"
          custom_init <- FALSE
        } else {
          setarguments$init <- init_set
          custom_init <- TRUE
        }
        if (!custom_init & !is.null(init_r)) {
          setarguments$init_r <- init_r
        }
      }
      
      if (eval(setarguments$backend) == "rstan" | 
          eval(setarguments$backend) == "cmdstanr") {
        if (is.null(eval(setarguments$control))) {
          setarguments$control <- list(adapt_delta = 0.8, max_treedepth = 15)
        }
        if (is.na(eval(setarguments$seed))) {
          setarguments$seed <- seed
        }
        
        cores_ <- eval(setarguments$cores)
        threads_ <- eval(setarguments$threads)
        
        if(cores_ == "maximise") {
          max.cores <- 
            as.numeric(future::availableCores(methods = "system", omit = 0))
          if(max.cores < 1) max.cores <- 1
        } else if(cores_ == "optimize") {
          max.cores <- 
            as.numeric(future::availableCores(methods = "system", omit = 1))
          if(max.cores < 1) max.cores <- 1
          if(max.cores > eval(setarguments$chains)) {
            max.cores <- eval(setarguments$chains)
          }
        } else if(!is.null(getOption('mc.cores')) &
                  cores_ != "maximise" &
                  cores_ != "optimize") {
          max.cores <- getOption('mc.cores')
        } else {
          max.cores <- eval(setarguments$cores)
        }
        setarguments$cores <-  max.cores
        
        
        
        if(!is.list(threads_)) {
          if( is.character(threads_) & threads_ == "maximise") {
            max.threads <- 
              as.numeric(future::availableCores(methods = "system", omit = 0))
            if(max.threads < 1) max.threads <- 1
          } else if( is.character(threads_) & threads_ == "optimize") {
            max.threads <- 
              as.numeric(future::availableCores(methods = "system", omit = 1))
            if(max.threads < 1) max.threads <- 1
            max.threads <- floor(max.threads /  eval(setarguments$chains))
          } else if(!is.null(getOption('brms.threads')) &
                    (is.character(threads_) & threads_ != "maximise") &
                    (is.character(threads_) & threads_ != "optimize")) {
            max.threads <- getOption('brms.threads')
          } else if(is.null(getOption('brms.threads')) &
                    (is.character(threads_) & threads_ != "maximise") &
                    (is.character(threads_) & threads_ != "optimize")) {
            max.threads <- getOption('brms.threads')
          } else {
            max.threads <- eval(setarguments$cores)
          }
          setarguments$threads <-  brms::threading(max.threads)
        }
      } 
      
      
      if (eval(setarguments$backend) == "cmdstanr") {
        if (is.list(eval(setarguments$stan_model_args)) &
            eval(length(setarguments$stan_model_args)) == 0) {
          setarguments$stan_model_args <- list(stanc_options = list("O1"))
        }
      }
      
      if (eval(setarguments$backend) == "rstan" & 
          packageVersion("rstan") < "2.26.1") {
        setarguments$threads <- setarguments$threads 
      }
      
      if (eval(setarguments$backend) == "mock") {
        max.threads <- getOption('brms.threads')
        setarguments$threads <- brms::threading(max.threads)
        max.cores <- getOption('mc.cores')
        setarguments$cores <-  max.cores
      }
      
      if (length(brmsdots) > 0) {
        setarguments <- c(setarguments, brmsdots)
      }
      return(setarguments)
    }
  
  
  
  if (verbose) {
    setmsgtxt <- paste0("\n Setting-up brms arguments")
    if (displayit == 'msg') {
      message(setmsgtxt)
    } else if (displayit == 'col') {
      col <- setcolh
      cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
    }
  }
  
  
  
  brmsdots_ <- list(...)
  
  for (collect_dot_namesi in collect_dot_names) {
    if(!is.null(brmsdots_[[collect_dot_namesi]])) 
      brmsdots_[[collect_dot_namesi]] <- NULL
  }
 
  
  brm_args <-
    setup_brms_args(
      formula = bformula,
      prior = brmspriors,
      stanvars = bstanvars,
      data = brmsdata,
      init = brmsinits,
      init_str = initialslist_s,
      init_r = brmsinits_r,
      seed,
      verbose,
      setarguments = brms_arguments,
      brmsdots = brmsdots_
    )
  
  
  if (verbose) {
    setmsgtxt <- paste0("\n Fitting model")
    if (displayit == 'msg') {
      message(setmsgtxt)
    } else if (displayit == 'col') {
      col <- setcolh
      cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
    }
  }
  
  cat("\n")
  
  
  insert_new_priors <- function(setdf_1, setdf_2) {
    cc <- zz <- list()
    for (i in 1:nrow(setdf_1)) {
      getx <- setdf_1[i,]
      zz[[i]] <- setdf_2 %>% 
        dplyr::mutate(prior = dplyr::if_else(.data$class == getx[['class']] &
                                               .data$coef == getx[['coef']] &
                                               .data$group == getx[['group']] &
                                               .data$resp == getx[['resp']] &
                                               .data$dpar == getx[['dpar']] &
                                               .data$nlpar == getx[['nlpar']],
                                             .data$getx$prior,
                                             .data$setdf_2$prior)) %>% 
        dplyr::filter(.data$class == getx[['class']] &
                        .data$coef == getx[['coef']] &
                        .data$group == getx[['group']] &
                        .data$resp == getx[['resp']] &
                        .data$dpar == getx[['dpar']] &
                        .data$nlpar == getx[['nlpar']])
      
      cc[[i]] <- setdf_2 %>% 
        dplyr::mutate(prior = dplyr::if_else(.data$class != getx[['class']] &
                                               .data$coef != getx[['coef']] &
                                               .data$group != getx[['group']] &
                                               .data$resp != getx[['resp']] &
                                               .data$dpar != getx[['dpar']] &
                                               .data$nlpar != getx[['nlpar']] ,
                                             .data$setdf_2$prior,
                                             .data$getx$prior)) %>% 
        
        dplyr::filter(.data$class != getx[['class']] & 
                        .data$coef != getx[['coef']] &
                        .data$group != getx[['group']] &
                        .data$resp != getx[['resp']] &
                        .data$dpar != getx[['dpar']] &
                        .data$nlpar != getx[['nlpar']])
      
    }
    p1 <- cc %>% do.call(rbind, .)
    p2 <- zz %>% do.call(rbind, .)
    p1p2 <- rbind(p1, p2)
    p1p2
  }
  
  
  if(set_higher_levels) {
    brmspriors_sdcor <- brmspriors %>% 
      dplyr::filter(.data$class == 'sd' | .data$class == 'cor')
    brmspriors_sdcor_gr <- brmspriors_sdcor$group
    
    brmsfit_sdcor <- do.call(brms::get_prior, brm_args) %>% 
      dplyr::filter(.data$class == 'sd' | .data$class == 'cor')
    
    brmsfit_sdcor_prior_gr <- brmsfit_sdcor %>% 
      dplyr::filter(!.data$group %in%  brmspriors_sdcor_gr)
    
    brmspriors_brmsfit_sdcor <- brmspriors %>% 
      dplyr::bind_rows(., brmsfit_sdcor_prior_gr)
    
    brmspriors <- brmspriors_brmsfit_sdcor
  }
  
  brm_args$prior <- brmspriors
  
  if(!is.null(set_self_priors) & !is.null(set_replace_priors)) {
    stop("Amongst 'set_self_priors' and 'set_replace_priors' arguments,",
         "\n ",
         " only one can be specified at a time")
  }
  
  if(get_priors & get_priors_eval & validate_priors & 
     get_stancode & get_standata & get_formula & get_stanvars) {
    stop("Amongst 'get_priors' 'get_priors_eval', 'validate_priors' ",
         "\n ",
         "'get_stancode', 'get_standata', 'get_formula', 'get_stanvars' ",
         "\n ",
         " arguments, only one can be set to TRUE at a time")
  }
  
  
  
  exe_model_fit <- TRUE
  if(get_stancode |
     get_standata |
     get_formula |
     get_stanvars |
     get_priors |
     get_priors_eval |
     validate_priors |
     get_init_eval) {
    exe_model_fit <- FALSE
  }
  
  
  
  
  lbbb_ <- ubbb_ <- NULL
  tempprior_hold <- brmspriors # brm_args$prior 
  setpriornamesorder <- colnames(tempprior_hold)
  tempprior_hold$lbbb_ <- tempprior_hold$lb
  tempprior_hold$ubbb_ <- tempprior_hold$ub
  tempprior_hold$lb <- tempprior_hold$ub <- NULL
  tempprior_hold <- tempprior_hold %>% 
    dplyr::mutate(lbbb_ = dplyr::if_else(class == 'sd', NA, lbbb_))
  tempprior_hold <- tempprior_hold %>% 
    dplyr::mutate(ubbb_ = dplyr::if_else(class == 'sd', NA, ubbb_))
  tempprior_hold$lb <- tempprior_hold$lbbb_
  tempprior_hold$ub <- tempprior_hold$ubbb_
  tempprior_hold$lbbb_ <- tempprior_hold$ubbb_ <- NULL
  tempprior_hold <- tempprior_hold %>% 
    dplyr::relocate(dplyr::all_of(setpriornamesorder))
  brmspriors <-   tempprior_hold
  
  
  
  if(!is.null(set_self_priors) & is.null(set_replace_priors)) {
    brmspriors <- set_self_priors
  }
  

  if(is.null(set_self_priors) & is.null(set_replace_priors)) {
    brmspriors <- brmspriors
  }
  
  
  brm_args$prior <- brmspriors
  
  decomp_escode2<- function(temp_stancode2x) {
    htx <- strsplit(temp_stancode2x, "\n")[[1]]
    lines_all <- c()
    for (htxi in 1:length(htx)) {
      htxi_ <- htx[htxi]
      if(grepl('^mu', gsub("[[:space:]]", "", htxi_))) {
        htxi_ <- gsub("[[:space:]]", "", htxi_)
        htxi_ <- gsub("=(", "=", htxi_, fixed = T)
        htxi_ <- gsub("));", ");", htxi_, fixed = T)
        htxi_name <- strsplit(htxi_, "(", fixed = T)[[1]][1]
        if(grepl('[', htxi_, fixed = T)) {
          htxi_c1 <- strsplit(htxi_, "[", fixed = T)[[1]][1]
        } else {
          htxi_c1 <- strsplit(htxi_, ",", fixed = T)[[1]][1]
        }
        htxi_others <- strsplit(htxi_, ",", fixed = T)[[1]][-1]
        htxi_others <- paste(htxi_others, collapse = ", ")
        htxi_c1 <- paste0(htxi_c1, 'X')
        htxi_c1 <- strsplit(htxi_c1, "(", fixed = T)[[1]][2]
        htxi_name <- paste0(htxi_name, 'X')
        if(grepl('[', htxi_, fixed = T)) {
          htxi_c1 <- paste0(htxi_c1, '[start:end,]')
        } else {
          htxi_c1 <- htxi_c1
        }
        htxi_final <- paste0(htxi_name, "(", htxi_c1, ", ", htxi_others)
      } else {
        htxi_final <- htxi_
      }
      lines_all <- c(lines_all, htxi_final)
      lines_all <- paste(lines_all, collapse = "\n")
    }
    dvciit <- paste0('data vector', " ", 'C_1')
    dvciby <- paste0('data matrix', " ", 'C_1', 'X')
    lines_all <- gsub(dvciit, dvciby, lines_all, fixed = T)
    dvciit <- ', C_1'
    dvciby <-  paste0(dvciit, 'X')
    lines_all <- gsub(dvciit, dvciby, lines_all, fixed = T)
    return(lines_all)
  }
  
  
  scode_final  <- do.call(brms::make_stancode, brm_args)
  sdata  <- do.call(brms::make_standata, brm_args)
  
  
  if(parameterization == 'cp') {
    scode_final <- edit_scode_ncp_to_cp(scode_final, 
                                        genq_only = FALSE, 
                                        normalize = normalize)
  } else if(parameterization == 'ncp') {
    scode_final <- scode_final
  }
  
  
  if(select_model_edit == 'logistic3e') {
    outedit_ <- edit_scode_for_logistic3(scode_final, 
                                         normalize = normalize)
    
    scode_final <- outedit_$editedcode
  }
  
  
  
  if(!is.null(decomp)) {
    if(decomp_editcode) scode_final <- decomp_escode2(scode_final)
  }
  

  get_priors_eval_numeric <- TRUE
  if(get_priors_eval & get_priors_eval_numeric) {
    get_priors_eval_out <- priors_to_textdata(spriors = brm_args$prior,
                                                  sdata = sdata)
  }
  
  if(get_priors_eval & !get_priors_eval_numeric) {
    get_priors_eval_out <- brm_args$prior
  }
  
    
  if(!exe_model_fit) {
    if(get_priors) {
      return(do.call(brms::get_prior, brm_args))
    } else if(get_standata) {
      return(do.call(brms::make_standata, brm_args))
    } else if(get_stancode) {
      return(scode_final)
    } else if(get_priors_eval) {
      return(get_priors_eval_out)
    } else if(validate_priors) {
      return(do.call(brms::validate_prior, brm_args))
    } else if(get_init_eval) {
      return(brm_args$init)
    } else if(get_formula) {
      return(brm_args$formula)
    } else if(get_stanvars) {
      return(brm_args$stanvars)
    }
  } 
  
  
  
  
  if(exe_model_fit) {
    if(brm_args$backend == "rstan") {
      if(length(brm_args$init) == 1) {
        if(brm_args$init == "0") {
          init_custom <- NULL
        } else if(brm_args$init == "random") {
          init_custom <- NULL
        } else {
          init_custom <- init_custom
        }
      } else {
        init_custom <- init_custom
      }
    }
    

    if(brm_args$backend == "cmdstanr") {
      if(is.null(brm_args$init)) {
        init_custom <- NULL
      } else if(!is.list(brm_args$init) & length(brm_args$init) == 1) {
        if(brm_args$init == "0") {
          init_custom <- NULL
        } else if(brm_args$init == "random") {
          init_custom <- NULL
        } else if(brm_args$init == 0) {
          init_custom <- NULL
        } else {
          init_custom <- init_custom
        }
      } else if(!is.null(brm_args$init)) {
        init_custom <- init_custom
      }
    } 
    
    
    
    if(!is.null(init_custom)) {
      init_fun <- function(chain_id = 1) init_custom
      if(!is.list(init_custom[[1]])) {
        init_custom <- 
          lapply(1:brm_args$chains, function(id) init_fun(chain_id = id))
      } else if(is.list(init_custom[[1]]) & length(init_custom) == 1) {
        init_custom <- rep(init_custom, brm_args$chains)
      } else {
        if(length(init_custom) != length(brm_args$init)) {
          stop("Custom initials specified via 'init_custom' argument must",
               "\n ", 
               " be a single named list (e.g., custom_init = list(x= 2,xx=5)) ",
               "\n ", 
               " or else a list of list matching the number of chains")
        }
      }
      new_init_append <- list()
      init_old <- brm_args$init
      init_append <- init_custom
      for (ilen in 1:length(init_old)) {
        new_init_append[[ilen]] <- c(init_old[[ilen]], init_append[[ilen]])
      }
      brm_args$init <- new_init_append
    } 
    
    
    
    # This when all lists of list NULL (e.g., when all init args random)
    if(length(brm_args$init[lengths(brm_args$init) != 0]) == 0) {
      if(brm_args$backend == 'cmdstanr') brm_args$init <- NULL
      if(brm_args$backend == 'rstan')    brm_args$init <- 'random'
    }
 
    
    # Set refresh based on thin argument
    if(!is.null(brm_args$refresh) & brm_args$thin > 1) {
      brm_args$refresh <- 
        ceiling((brm_args$refresh * brm_args$thin) / brm_args$thin)
    }
    
    brm_args$refresh <- NULL
    

    if(fit_edited_scode) {
      if(brm_args$backend == "cmdstanr") {
         stop("Please use 'rstan' as backend for CP parameterization")
        # brmsfit <- brms_via_cmdstanr(scode_final, sdata, brm_args)
      }
      if(brm_args$backend == "rstan") {
        brmsfit  <- brms_via_rstan(scode_final, sdata, brm_args)
      }
    } else if(!fit_edited_scode) {
      brmsfit <- do.call(brms::brm, brm_args)
    }
    
    
    if(brm_args$backend == "mock") {
      brmsfit <- do.call(brms::brm, brm_args)
    }
   
    
    # Add attr so that expose_model_functions() works on bgmfit
    attr(brmsfit, 'class') <- c(attr(brmsfit, 'class'), 'bgmfit')
    
    model_info <- list()
    
    model_info[['emodel']] <- scode_final
    
    model_info[['parameterization']] <- parameterization
    
    model_info[['d_adjusted']] <- d_adjusted

    for (i in 1:length(funlist_rnamelist)) {
      model_info[[funlist_rnamelist[[i]]]] <- funlist_rvaluelist[[i]]
    }
    
    for (i in 1:length(xoffsetnamelist)) {
      model_info[[xoffsetnamelist[[i]]]] <- xoffsetvaluelist[[i]]
    }
    
    for (i in 1:length(knotsnamelist)) {
      model_info[[knotsnamelist[[i]]]] <- knotsvaluelist[[i]]
    }
    
    for (i in 1:length(fixednamelist)) {
      model_info[[fixednamelist[[i]]]] <- fixedvaluelist[[i]]
    }
    
    for (i in 1:length(randomnamelist)) {
      model_info[[randomnamelist[[i]]]] <- randomvaluelist[[i]]
    }
    
    for (i in 1:length(xfunnamelist)) {
      model_info[[xfunnamelist[[i]]]] <- xfunvaluelist[[i]]
    }
    
    for (i in 1:length(yfunnamelist)) {
      model_info[[yfunnamelist[[i]]]] <- yfunvaluelist[[i]]
    }
    
    for (i in 1:length(xxfunnamelist)) {
      model_info[[xxfunnamelist[[i]]]] <- xxfunvaluelist[[i]]
    }
    
    for (i in 1:length(yyfunnamelist)) {
      model_info[[yyfunnamelist[[i]]]] <- yyfunvaluelist[[i]]
    }
    
    for (i in 1:length(groupvarnamelist)) {
      model_info[[groupvarnamelist[[i]]]] <- groupvarvaluelist[[i]]
    }
    
    for (i in 1:length(hierarchicalvarnamelist)) {
      model_info[[hierarchicalvarnamelist[[i]]]] <- 
        hierarchicalvarvaluelist[[i]]
    }
    
    for (i in 1:length(xnamelist)) {
      model_info[[xnamelist[[i]]]] <- xvarvaluelist[[i]]
    }
    
    for (i in 1:length(ynamelist)) {
      model_info[[ynamelist[[i]]]] <- yvarvaluelist[[i]]
    }
    
    for (i in 1:length(covnamelist)) {
      model_info[[covnamelist[[i]]]] <- covvaluelist[[i]]
    }
    
    for (i in 1:length(sigmacovnamelist)) {
      model_info[[sigmacovnamelist[[i]]]] <- sigmacovvaluelist[[i]]
    }
    
    if(!is.na(univariate_by$by)) {
      model_info[['subindicators']] <- subindicators
    } 
    
    for (i in 1:length(d_adjustednamelist)) {
      model_info[[d_adjustednamelist[[i]]]] <- d_adjustedvaluelist[[i]]
    }
    
    model_info[['StanFun_name']] <- SplineFun_name
    model_info[['multivariate']] <- multivariate$mvar
    model_info[['univariate_by']] <- univariate_by$by
    model_info[['nys']] <- nys
    model_info[['ys']] <- ys
    model_info[['xs']] <- xs
    model_info[['ids']] <- ids
    model_info[['dfs']] <- dfs
    model_info[['xfuns']] <- xfuns
    model_info[['yfuns']] <- yfuns
    model_info[['outliers']] <- outliers
    model_info[['bgmfit.data']] <- data.org.in
    model_info[['call.full.bgmfit']] <- call.full
    model_info[['call.bgmfit']] <- mcall_
    model_info[['brms_arguments_list']] <- brms_arguments_list
    model_info[['select_model']] <- select_model
    model_info[['decomp']] <- decomp
    model_info[['fun_scode']] <- fun_scode
    brmsfit$model_info <- model_info
    
    if (expose_function) {
      if (verbose) {
        setmsgtxt <-
          paste0("\n Exposing Stan functions for post-processing\n")
        if (displayit == 'msg') {
          message(setmsgtxt)
        } else if (displayit == 'col') {
          col <- setcolh
          cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
        }
      }
      if (!verbose) {
        setmsgtxt <-
          paste0("\n Exposing Stan functions for post-processing..\n")
        message(setmsgtxt)
      }
      
      brmsfit <- expose_model_functions(model = brmsfit, 
                                      scode = fun_scode,
                                      expose = TRUE, 
                                      select_model = NULL,
                                      returnobj = TRUE,
                                      envir = NULL)
      brmsfit$model_info[['expose_method']] <- 'S'
    } 
    
    if (!expose_function) {
      brmsfit <- expose_model_functions(model = brmsfit, 
                                      scode = fun_scode,
                                      expose = FALSE, 
                                      select_model = select_model,
                                      returnobj = TRUE,
                                      envir = NULL)
      brmsfit$model_info[['expose_method']] <- 'R'
    } 
    
    if (verbose) {
      setmsgtxt <- paste0("\nModel Fitting complete")
      if (displayit == 'msg') {
        message(setmsgtxt)
      } else if (displayit == 'col') {
        col <- setcolh
        cat(paste0("\033[0;", col, "m", setmsgtxt, "\033[0m", "\n"))
      }
    }
    if(exists("err.")) rm("err.")
    return(brmsfit)
  } # exe_model_fit
  
}

