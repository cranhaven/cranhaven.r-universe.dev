\name{glm.pred.JGR}
\alias{glm.pred.JGR }
\title{Dialog box to apply predictions from a regression model to assess
test sites}
\description{
  
  The regression prediction tool provides a way to model natural
  variations in an environmental parameter (e.g., stream temperature) in
  reference (i.e., least-disturbed) sites.  Then, one can use the
  reference site model to determine whether observations of that same
  parameter in test sites are within the range of variation defined by
  the reference sites.
  
The linear regression tool has more options available for diagnosing and
assessing the fit of a regression model on the quality data.  The
regression prediction tool assumes that such diagnostics have been
performed to define a regression model, and the goal is now to apply the
model to new data. 

}

\details{
  	
	\strong{User Interface}

	Select \emph{Analysis Tools -> Regression Prediction} from the menus.
	A dialog box will open.  Select the data set of interest from
	the pull-down menu, or browse for a tab-delimited text file.
	The Data Subsetting tab can be used to select a subset of the
	data file by choosing a variable from the pull down menu and
	then selecting the levels of that variable to include.  You can
	hold down the <CTRL> key to add several levels.
	
Select \bold{Dependent} variable and \bold{Independent} variables as in the linear
regression tool.

By default, an intercept is included in the model.  The intercept can be
excluded by selecting \bold{Remove Intercept} under the Analysis Options, in
which case at least one independent variable must have been selected.

\bold{Select a Reference Variable}: a variable that contains (character) values
which can be used to identify which rows correspond to reference data.
Once the Reference Variable is selected, select the levels of that
variable that correspond to reference data.  You can hold down the
<CTRL> key to select multiple levels that indicate reference.

Choose a \bold{Significance Level} for identification of anomalous data points
in the new data.  Note that the significance level corresponds to a
level for a single prediction, not a family-wise significance level; no
correction is performed for multiple comparisons when multiple
predictions are being made.  Keep in mind that some new data is expected
to be flagged as significant (on average, about the number of
predictions times the significance level should appear significant,
depending on correlation).

The \bold{ID Variable} allows for a label variable to be specified, so that
points that differ significantly from reference can be identified on the
plot.  If no ID Variable is selected, then the row number will be used
instead.

The output of the regression prediction tool is a table for the test
(non-reference) data, including the observed and predicted values, the
standard error associated with the prediction, a p-value for the
prediction, an indication of whether the sample differs significantly
from reference, an indication of whether the independent variables for
the new data are in range of the model fit, and finally an indication of
whether or not the normal approximation used to calculate the p-value is
appropriate (for poisson and binomial regression only).  Note: for
binomial data, the observed value is converted from a count to a
proportion (dependent variable divided by sample size).  A plot of
observed versus predicted values is also produced, to help assess the
model fit and identify anomalous data points. 
}

\keyword{regression} 

