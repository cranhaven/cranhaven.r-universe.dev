\name{conditionalprob.JGR }
\alias{conditionalprob.JGR}
\title{Dialog box for computing and plotting conditional probabilities}
\description{
Computes and plots conditional probabilities of an event (e.g., an undesirable
biological condition) given values of
a another variable (e.g., a stressor).
}
\details{
	A conditional probability plot provides a graphical representation of the
	impact one variable can have on the probability of another occuring. One
	way to assess whether the probability of an event of interest
	occuring changes for different values of an explanatory variable
	is to consider how susets of the response change as a function
	of the explanatory variable.

	The first variable is the response. It is this column that subsets will be
	taken from based on a user-specified conditioanl value. The
	condfitional value must be between the maximum and minimum
	values of the response variable. The second variable is the
	explanatory variable.

	It is often the case in biological applications, that thresholds exist.
	Conditional probability plots are useful for qualitatively identifying
	thresholds when they may exist.Conditional probability plots are
	often used in the development of tolerance values. Once the
	empirical curve for conditional probability is generated,
	possible threshold levels for eliciting impact can be identified
	for eventual use in the development of stressor criteria. This
	threshold of impact is determined by the identification of a
	changepoint that separates the empirical curve into two groups
	so that the probability of impact is different for samples above
	and below the changepoint. There are three potential methods
	for the identification of thresholds of impact for the
	conditional probability empirical curves.  These include: 1)
	change in curvature of fitted curve, 2) nonparametric deviance
	reduction, and 3)non-overlapping confidence intervals (CIS). For
	a detailed explanation of these methods see Paul and McDonald
	(Journal of the American Water Resources Association 41(5)
	p. 1211-1223 2005).  

	Once thresholds have been identified, it may be of interest to utilize
	additional analyses that will quantify the relationship between the
	subsetted data. Specifically, one may choose to compute the correlation
	between the variables in the conditional dataset or if a threshold exists
	it may be of interest to generate a regression line for the sets of
	probabilities that are both above and below a threshold.

	\strong{User Interface}

	Select \emph{Analysis Tools -> Conditional Probability} from the menus.
	A dialog box will open.  Select the data set of interest from
	the pull-down menu, or browse for a tab-delimited text file.
	The Data Subsetting tab can be used to select a subset of the
	data file by choosing a variable from the pull down menu and
	then selecting the levels of that variable to include.  You can
	hold down the <CTRL> key to select several levels.
	
	Select \bold{Stressor} and \bold{Response} variables from the pull-down
	menu.  If each sample in your data set represents a different
	proportion of your population (e.g., data collected using a
	stratified random sampling design), you can Specify a
	\bold{weighting variable} that provides the relative weight for each sample.
	
Assign a \bold{Response Cutoff Value}.  This value defines the threshold
for defining your response of interest.  For example, if your response
is an IBI, the Response Cutoff Value would be the criterion value for
your IBI below which the site is assess as biologically impaired.
Selection of the \bold{Cutoff Value Direction} depends on whether you expect
the value of your response to increase or decrease with stress.  For
IBI, which decreases with stress, \bold{Cutoff Value Direction} would be Less
Than.

\bold{Probability Direction} defines the direction in which you would
like to assess the stressor level.  For a stressor whose severity
increases with increasing values (e.g., percent fines), you should
select \bold{Greater Than or Equal}. Other stressors (e.g., pH) increase in
severity as their value decreases, in which case you would select
\bold{Less Than or Equal}.

Selecting \bold{Bootstrap confidence intervals} directs the tool to
repeatedly resample the data set to estimate confidence intervals on the
estimate relationship.

You may change the axes labels and plot title by typing them in the
\bold{Plot Labels} dialog.

The output is a graph of the fitted probability (that the dependent
variable is less than the conditional value) versus the independent
variable. 

}
\keyword{models}
