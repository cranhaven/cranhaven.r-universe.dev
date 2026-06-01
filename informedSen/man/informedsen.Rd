\name{informedsen}
\alias{informedsen}
\concept{Sensitivity analysis}
\concept{Observational study}
\concept{Placebo test}
\concept{Test for bias}
\concept{Causal inference}
\concept{Control outcome}
\concept{Unaffected outcome}
\concept{Known effect}

\title{
  Sensitivity Analysis Informed by a Test for Unmeasured Bias
}
\description{ The function does a a sensitivity analysis for one outcome informed or constrained by the results of a test for unmeasured bias based on another outcome known to be unaffected by the treatment. The package uses gurobi to solve a quadratically constrained quadratic program.  To use the package, the gurobi solver must be installed.  See the discussion about installing gurobi in the example below.
}
\usage{
informedsen(gamma, sc, z, mset, alpha = 0.05)
}

\arguments{
  \item{gamma}{
    The sensitivity parameter.  A number greater than or equal to 1.
}
  \item{sc}{
    A matrix with N rows and at least two columns.  The first column is the primary outcome, typically after scoring using senmscores.  The remaining columns are unaffected outcomes used to test for bias, typically after scoring using senmscores.
}
  \item{z}{
    A vector of length N whose N coordinates are 1 for treated, 0 for control.
}
  \item{mset}{
    A vector of length N indicating the matched set.  Each matched set contains one treated individual and the same fixed number of controls.
}
  \item{alpha}{
    A vector with length equal to the number of columns of sc.  The jth coordinate of alpha is the level of the test applied to the jth column of sc.  If alpha is a scalar, it is repeated for every column of sc.
}
}

\value{
\item{result }{Text indicating whether or not the test for bias rejects all biases of magnitude Gamma or less.  If yes, then the conclusion is that you must increase Gamma to continue.  If no, then the test on the promary outcome is conducted inside the confidence set defined by a test for bias. The text begins after gurobi prints its standard output for the underlying optimization problem.}
\item{optimization.problem }{Reiterates the result above, where the word yes means the optimization problem is infeasible, and the word no means it is feasible.  See the conclusion for a scientific interpretation of this aspect of the output.}
\item{conclusion }{Text indicating the result of the test for effect on the primary outcome.}
\item{deviates }{A vector of standardized deviates that might be compared with the standard Normal distribution.  There is one deviate for each column of sc.  If sc has column names, then the column names label the
deviates.  The deviates are computed at the treatment assignment probabilities, theta, that solve the constrained optimization problem.}
\item{alphas }{A vector of two-sided levels used for the deviates, together with their total.  The total is relevant if the Bonferroni inequality is used to ensure joint level of all the tests.  The absolute deviates might be compared with qnorm(1-alphas/2) for a two-sided test.}

}
\references{
Berger, R. L. and Boos, D. D. (1994). P-values maximized over a confidence set for the nuisance parameter. Journal of the American Statistical Association, 89, 1012-1016. <doi:10.1080/01621459.1994.10476836>

Rosenbaum, P. R. (1984). From association to causation in observational studies: The role of tests of strongly ignorable treatment assignment. Journal of the American Statistical Association 79, 41-48.
<doi:10.1080/01621459.1984.10477060>

Rosenbaum, P. R. (1989a). On permutation tests for hidden biases in observational studies. The Annals of Statistics 17, 643-653.
<doi:10.1214/aos/1176347131>

Rosenbaum, P. R. (1989b). The role of known effects in observational studies. Biometrics 45, 557-569. <doi:10.2307/2531497>

Rosenbaum, P. R. (1992). Detecting bias with confidence in observational studies. Biometrika, 79(2), 367-374. <doi:10.1093/biomet/79.2.367>

Rosenbaum, P. R. (2007) Sensitivity analysis for m-estimates, tests and confidence intervals in matched observational studies. Biometrics, 2007, 63, 456-464. <doi:10.1111/j.1541-0420.2006.00717.x>

Rosenbaum, P. R. (2021). Sensitivity analyses informed by tests for bias in observational studies.  Manuscript.
}
\author{
Paul R. Rosenbaum
}

\note{
When gurobi is called, it produces extensive output.  The output for informedsen appears at the end, after gurobi has produced its output.  Most users will wish to skip to the end, for the output from informedsen, returning to the gurobi output only if needed.

informedsen checks that your input has the required form, and it will stop if there is a problem with your input.  For instance, informedsen will stop if you supply a value of gamma that is less than one.
}



\examples{
\dontrun{
# To run these examples, you MUST have gurobi installed.
# gurobi is available for free to academic faculty
# Search for the gurobi web page, and click the menu for Academia
# Search for "gurobi and R" to find gurobi's local R package
#  connecting gurobi and R.  You must install both gurobi
#  and its local R package to run informedsen.
#
# The examples are from Rosenbaum (2021)
#
# gurobi generates output before the output from informedsen
#  appears.  In a first use, you might skip to the output
#  from informedsen, which begins with text labeled result.
#
data(HDL)
shdl<-senmscores(HDL$hdl,HDL$z,HDL$mset)
smmerc<-senmscores(HDL$mmercury,HDL$z,HDL$mset)
sc<-cbind(shdl,smmerc)

# A test within the confidence set
informedsen(3.5, sc, HDL$z, HDL$mset, alpha = 0.05)

# A test within the confidence set using
# the method of Berger and Boos (1994)
informedsen(3.4, sc, HDL$z, HDL$mset, alpha = c(0.04,0.01))

# An example in which the confidence set is empty
informedsen(1.25, sc, HDL$z, HDL$mset, alpha = .05)
}
}

\keyword{design}
\keyword{htest}
