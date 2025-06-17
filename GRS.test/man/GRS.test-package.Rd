\name{GRS.test-package}
\alias{GRS.test-package}
\docType{package}
\title{
\packageTitle{GRS.test}
}
\description{
\packageDescription{GRS.test}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{GRS.test}
\packageIndices{GRS.test}

The package accompanies the working paper:

Kim and Shamsuddin, 2017, Empirical Validity of Asset-pricing Models: Application of Optimal Significance Level and Equal Probability Test

The function GRS.test returns the GRS test statistics with model estimation results.

The function GRS.MLtest provides an alternative test statistic with theta and theta* estimation results. 

Additional functions for the power analysis and calculation of optimal level of significance are also included.

}
\author{
\packageAuthor{GRS.test}

Maintainer: \packageMaintainer{GRS.test}
}
\references{
Gibbons, Ross, Shanken, 1989. A test of the efficiency of a given portfolio, Econometrica, 57,1121-1152.
<DOI:10.2307/1913625>

Fama and French, 1993, Common risk factors in the returns on stocks and bonds, Journal of Financial Economics, 33, 3-56.
<DOI:10.1016/0304-405X(93)90023-5>

Fama and French, 2015, A five-factor asset-pricing model, Journal of Financial Economics, 1-22.
<DOI:http://dx.doi.org/10.1016/j.jfineco.2014.10.010>

}

\keyword{ package }
\seealso{
 The examples replicate the results reported in Fama and French (1993) and Kim and Shamsuddin (2016) 
}
\examples{
data(data)
factor.mat = data[1:342,2:4]            # Fama-French 3-factor model
ret.mat = data[1:342,8:ncol(data)]      # 25 size-BM portfolio returns
GRS.test(ret.mat,factor.mat)$GRS.stat   # Table 9C of Fama-French (1993)
}
