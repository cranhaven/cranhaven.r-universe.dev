\name{F.cjs.gof}
\alias{F.cjs.gof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ F.cjs.gof }
\description{
  Goodness of fit measures for a CJS open-population capture recapture model.
}
\usage{
F.cjs.gof( cjsobj, resid.type="pearson", rule.of.thumb = 2, HL.breaks = "deciles" )
}

\arguments{
   \item{cjsobj}{A CJS capture-recapture fitted object from a previous call to \code{F.cjs.estim}}
   \item{resid.type}{Type of residual to return.  \code{resid.type} = 'pearson' produces 
		Pearson residuals.  \code{resid.type} = 'deviance' produces deviance residuals.
		Anything other than 'deviance' gives you Pearson residuals.}
   \item{rule.of.thumb}{Rule of thumb to include a cell in one of the chi-square statistics. For example, 
		if \code{rule.of.thumb} = 2, the expected count in a cell has to be greater than 
		2 in order for the cell to be included in the overall Chi-square statistic for 
		that table. No pooling of cells is done. Cells with expected values less than 
		\code{rule.of.thumb} are dropped.}
   \item{HL.breaks}{vector of bin break points to use in the Hosmer-Lemeshow statistic.  This must 
		be a partition of the interval [0,1], with 0 as lowest break and 1 as max.
		E.g., if HL.breaks = c(.25,.75), the bins used are [0,.25),[.25,.75),[.75,1].
  		The default, "deciles", calculates breakpoints such that 10% of predicted 
		values are in each. I.e., approximately \code{0.1 * n} expected values
		are in each of 10 cells. }
}
\value{
	A CJS object equivalent to the input crobj, with additional components for 
	GOF testing. Additional components are a variety of goodness of fit statistics.  Goodness
	of tests included are: (1) "Overall" = Chi-square test of overall goodness of fit 
	based on all "live" cells in the capture histories, (2) "Osius and Rojek" = Osius and Rojeck
	correction to the overall chi-square test, (3) "Test 4" = Chi-square of observed and expected captures 
	by occasion, (4) "Test 5" = Chi-square of observed and expected captures by individual, summed 
	over animals, (5) "Hosmer-Lemeshow" = Hosmer-Lemeshow Chi-square GOF over all occasions 
	and animals, and (6) "ROC" = area under the curve overall classification accuracy of expected 
	values for capture histories. Tests (2), (5), and (6) are based on methods in chapter 5
	of Hosmer and Lemeshow (2000).
	

	Specifically, the output object has class c("cjsgof", "cjs", "cr"), contains 
	all the components of the original CJS object, plus the following components:
	\item{gof.chi}{Chi-square statistic for overall goodness of fit based on all "live" cells
		in the capture-recapture histories.}
	\item{gof.df}{Degrees of freedom for overall goodness of fit test.}
	\item{gof.pvalue}{P-value for overall goodness of fit.}
	\item{or.table}{Chi-square table for the Osius and Rojek correction 
		to the overall GOF test (See p. 153 of Hosmer and Lemeshow (2000)).}
	\item{or.chi}{Chi-square statistic for the Osius and Rojek test.}
	\item{or.df}{Degrees of freedom for the Osius and Rojek test.}
	\item{or.correction}{Correction to the Osius and Rojek test. This is computed as 
		number of unique expected values minus the sum of 1 over the individual 
		cell counts.} 
	\item{or.rss}{Root sum-of-squares for the Osius and Rojek test, obtained from 
		weighted regression.}
	\item{or.z}{Osius and Rojek Z statistic.  This is computed as 
		(or.chi - or.df) / sqrt( or.correction + or.rss )}
	\item{or.pvalue}{2-tailed Osius and Rojek p-value computed from standard normal 
		distribution and the Osius and Rojek Z statistic.}
	\item{t4.table}{Chi-square table for Test 4, which sums observed and expected 
		captures over individuals.  This table has one cell for each occasion.}
	\item{t4.chi}{Chi-square statistic for Test 4, computed from \code{t4.table} by 
		summing the chi-square contributions over cells that meet the \code{rule.of.thumb}. }
	\item{t4.df}{Degrees of freedom for Test 4. Equal to number of cells meeting \code{rule.of.thumb} 
		minus 1.}
	\item{t4.pvalue}{P-value for Test 4 computed from Chi-squared distribution.}
	\item{t5.table}{Chi-square table for Test 5, which sums observed and expected 
		captures over occasions.  This table has one cell for each individual.}
	\item{t5.chi}{Chi-square statistic for Test 5, compute from \code{t5.table} by 
		summing the chi-square contributions over cells that meet the \code{rule.of.thumb}.}
	\item{t5.df}{Degrees of freedom for Test 5. Equal to number of cells meeting \code{rule.of.thumb} 
		minus 1.}
	\item{t5.pvalue}{P-value for Test 5 computed from Chi-squared distribution.}
 	\item{HL.table}{Chi-square table for the Hosmer-Lemeshow test.}
	\item{HL.chi}{Chi-square statistic for the Hosmer-Lemeshow test.}
	\item{HL.df}{Degrees of freedom for the Hosmer-Lemeshow test.}
	\item{HL.pvalue}{P-value for the Hosmer-Lemeshow test.}
	\item{roc}{Area under the curve statistic for the ability of the "live" cell expected values 
		to classify captures. }
}
\details{
	The "overall" Chi-square test computes the sum of [(h(ij) - Psi(ij))*(h(ij) - Psi(ij))] / Psi(ij) over 
	all "live" cells in the capture-recapture problem.  "Live" cells are those following 
	initial captures, prior to and including the occasion when an animal was censoring 
	(died on capture and removed).  If an animal was not censored, the "live" cells for it
	extend from occasion following initial capture to the end of the study. In the above,
	h(ij) is the 0-1 capture indicator for animal i at occasion j.  Psi(ij) is the expected 
	value of h(ij), and is computed as the produce of survival estimates from initial capture 
	to occasion j, times probability of capture at occasion j.  Assuming animal i was initially
	captured at the a-th occasion, Psi(ij) is computed as 
	phi(ia) * phi(i(a+1)) * ... * phi(i(j-1)) * p(ij), where phi(ij) is the modeled estimate of 
	survival for animal i from occasion j to occasion j+1, and p(ij) is the probability of 
	capturing animal i during occasion j.  

	The other derived GOF tests computed here use h(ij) and its expected value Psi(ij).  Test 4 
	sums observed and expected over individuals.  Test 5 sums observed and expected over occasions. 
	The other 3 tests were borrowed from logistic regression by viewing h(ij) as a binary response, 
	and Psi(ij) as its expected value. 

}

\note{
  Future plans include adding the following: 
  (1) Osius-Rojek = Overall z statistic for GOF over all occasions and animals; and (2)
  Stukel = Overall z test for appropriateness of the logistic link.

  Future plans also include a plot method whereby all tests, especially the ROC, could be 
  assessed graphically. 

  Print the GOF results in a nice format using \code{print.cjs}.
  }

\references{ 
Hosmer, D. W. and S. Lemeshow. 2000. Applied Logistic Regression, 2nd edition. 
	New York: John Wiley and Sons. 

}
\author{ Trent McDonald, WEST Inc., tmcdonald@west-inc.com }

\seealso{ \code{\link{F.cjs.estim}}, \code{\link{print.cjs}} }

\examples{

data(dipper.histories)
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) } 
dipper.cjs <- F.cjs.estim( ~x2+x3+x4+x5+x6, ~x1+x2+x3+x4+x5, dipper.histories )
dipper.cjs.gof <- F.cjs.gof( dipper.cjs )
print(dipper.cjs.gof)

}

\keyword{ models }
