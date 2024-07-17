\name{bsTab}
\alias{bsTab}
\title{Generating Pretty Statistical Tables}
\description{Format statistics from regressions into pretty outputs}
\usage{bsTab(w,
  need = c("1T", "1E", "2T", "2E", "3T", "3E", "4T", "4E", "5"),
  wrap.TE = c("(", "", "["),
  add.sig = c("coef", "TE"),
  percent = c(0.01, 0.05, 0.10),
  symbol = c("***", "**", "*", ""),
  digits = c(3, 3, 3, 3), ... )
}
\arguments{
  \item{w }{ statistical results from regression models; an object of class \code{glm},  \code{lm}, and \code{systemfit} can be supplied directly, or a data frame with at least four columns with the sequence of estimates, errors, t-values, and p-values.}
  \item{need }{ a choice of output formats; default of \code{1T} is one column with t ratio and significance symbols; 1 to 5 is the number of columns; T is t ratios; E is standard errors. This argument must be a character string.}
  \item{wrap.TE }{ parentheses, none, or brackets can be used to enclose t ratios or standard errors; default value is parenthses for one-column format and none for other formats.}
  \item{add.sig }{ a chacter string to indicate where to add the significance symbol, either to the coefficents (\code{"coef"}) or the t-value and error (\code{"TE"}).}
  \item{percent }{ percentage values used to categorize p values.}
  \item{symbol }{ symbols used to represent p-value categories; the default values can be changed to symbols like a, b, c, or different combinations of *.}
  \item{digits }{ digits for outputs; the default values are 3, 3, 3, and 2 for estimate, error, t value, and p value, correspondingly. A single value like 4 can be supplied and it will be recycled for all of them.}
  \item{\dots }{ additional arguments to be passed.}
}

\details{Format statistics from regressions into tables that are often reported in economic journals. The column of 'Variable' in the outuput is the row names of the input data so the raw data should contain meaningful rownames. Besides the variable name column, the maximum number of output is five columns: estimate, error, t ratio, p value, and significance. \code{wrap.TE} and \code{add.sig} are only valid for column widths of 1 and 2.}
\value{A dataframe of statistical results.}
\author{Changyou Sun (\email{cs258@msstate.edu})}
\examples{

# a simulated data
tes <- data.frame(est = c(4, 56, 12), err = c(0.3, 0.56, 0.789), 
  t.rat = c(2.56, 7.9, 1.2), p.val = c(0.002, 0.23, 0.061))
tes
bsTab(tes)
bsTab(w = tes, need = "2E")

# real data
data(daIns)
ra <- glm(formula = Y ~ Injury + HuntYrs + Nonres + 
                    Lspman + Lnong + Gender + Age + 
                    Race + Marital + Edu + Inc + TownPop, 
          family = binomial(link="logit"),
          data = daIns, x = TRUE)
(ca <- data.frame(summary(ra)$coefficients))

# an object of class 'glm' as input
bsTab(w = ra, add.sig = "TE")
bsTab(w = ra, wrap.TE = "[")
bsTab(w = ra, need = "5")
bsTab(w = ra, need = "4T", wrap.TE = "[")
final <- bsTab(w = ra, need = "3T", 
  percent = c(0.01, 0.05, 0.10),
  symbol = c("a", "b", "c", ""), digits = 4)
final
print(final, right = FALSE) 

# any matrix with at least four columns can be supplied
cbind(bsTab(ca), bsTab(ra)) 
}

\keyword{manip}