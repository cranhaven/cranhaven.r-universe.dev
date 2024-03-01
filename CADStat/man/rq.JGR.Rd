\name{rq.JGR}
\alias{rq.JGR}
\title{Dialog box for calculating a quantile regression}
\description{
  This function receives inputs from the Java GUI for quantile
  regression and sends these parameters to the quantile regression function.
}

\details{

  \strong{User Interface}

  Select \emph{Analysis Tools -> Quantile Regression} from the menus.  A dialog
  box will open.  Select the data set of interest from the pull-down
  menu, or browse for a tab-delimited text file.  The Data Subsetting
  tab can be used to select a subset of the data file by choosing a
  variable from the pull down menu and then selecting the levels of that
  variable to include.  You can hold down the <CTRL> key to select
  several levels.
  
Select a \bold{Dependent} variable (the variable you wish to model).
Select all explanatory (\bold{Independent}) variables you wish to include in
the model.  You can hold down the <CTRL> key to select several
variables. Note: the dependent variable is in the list of possible
independent variables, but it should not be selected as an independent
variable.

Type in the quantiles that you wish to estimate in the \bold{Quantiles} dialog.
You may type in up to 8 quantiles, all of which must be between 0 and 1.
These must be typed from top to bottom: skipped lines will result in
later values being ignored.  Be sure that you hit <TAB> or <ENTER> after
typing in your last quantile and before hitting Submit, or the last
quantile will not register.

Confidence intervals for the regression coefficients are generated only
if \bold{Compute Confidence Intervals} is selected.  The confidence level can
be changed only after Compute Confidence Intervals is selected. 

If \bold{Regression Scatterplot} is selected, then one scatterplot is plotted
for each independent variable, of the dependent versus independent
variable with overlaid quantile regression fits.  Confidence bounds on
the fit are added to the plot if Coefficient Confidence Bands is
chosen.

The output is the regression coefficients, with confidence bounds if
selected, and scatterplots if selected.
	
}

\seealso{
  \code{\link[quantreg]{rq}}
  }

\keyword{hplot} 

