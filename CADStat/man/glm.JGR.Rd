\name{glm.JGR}
\alias{glm.JGR }
\title{Dialog box for fitting a generalized linear regression model}
\description{
	Fits a generalized linear regression model returning model
	coefficient estimates, confidence intervals, and diagnostic plots. 
}

\details{
  \strong{User Interface}

  Select \emph{Analysis Tools -> Linear Regression} from the menus.  A dialog
  box will open.  Select the data set of interest from the pull-down
  menu, or browse for a tab-delimited text file.  The Data Subsetting
  tab can be used to select a subset of the data file by choosing a
  variable from the pull down menu and then selecting the levels of that
  variable to include.  Hold down the <ctrl> key to add several levels.
  
Select a response variable (the variable whose value you wish to
predict). An appropriate distribution for your response variable can be
selected by clicking on a radio button.  \bold{Normal} distributions are most
common.  A \bold{Poisson} distribution may be appropriate if your response
variable reflects a count of some attribute (e.g., number of distinct
taxa), and a \bold{Binomial} distribution may be appropriate if you are
modeling the probability of success and your response variable is a
number of successes for a fixed number of trials (e.g., the presence of
a species in a sample).

Select all explanatory variables you wish to include in the model (the
variables used to predict the dependent variable.  You can hold down the
<CTRL> key to add multiple 
variables.  Note: the dependent variable is in the list of possible
independent variables, but it \emph{should not} be selected as an independent
variable.

By default, an intercept is included in the model.,  The intercept can
be excluded by selecting \bold{Remove Intercept}, in which case at least one
independent variable must have been selected.

Confidence intervals for the regression coefficients are generated only
if \bold{Compute Confidence Intervals} is selected.  The confidence level can
be changed only after Compute Confidence Intervals is selected.

Several diagnostic plots are available for plotting.  None are produced
by default, but any subset of the five diagnostic plots can be selected
for display.

Should you wish to save the linear regression as an R object, select
\bold{Save R Results?} and type a variable name in R Result Name.
 
The output is the regression coefficients, along with standard errors,
t-statistics, and p-values.  Confidence intervals and diagnostic plots
are produced only if selected. 
}

\seealso{
	\code{\link{glm}}
}

\keyword{regression} 

