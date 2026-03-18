\name{German_Credit}
\alias{German_Credit}
\docType{data}
\title{German Credit data set}
\description{
This data set classifies customers as "Good" or "Bad" as per their credit risks.This data set was contributed by Professor Dr. Hans Hofmann,and can be downloaded from the UCI Machine Learning Repository.
}
\usage{data("German_Credit")}
\format{
  A data frame with 1000 observations on the following 21 variables.
  \describe{
    \item{\code{Account_Balance}}{a factor with levels \code{A11} \code{A12} \code{A13} \code{A14}}
    \item{\code{Duration}}{a numeric vector}
    \item{\code{Credit_History}}{a factor with levels \code{A30} \code{A31} \code{A32} \code{A33} \code{A34}}
    \item{\code{Purpose}}{a factor with levels \code{A40} \code{A41} \code{A410} \code{A42} \code{A43} \code{A44} \code{A45} \code{A46} \code{A48} \code{A49}}
    \item{\code{Credit_Amount}}{a numeric vector}
    \item{\code{Saving_Accounts_Bonds}}{a factor with levels \code{A61} \code{A62} \code{A63} \code{A64} \code{A65}}
    \item{\code{Current_Employment_Length}}{a factor with levels \code{A71} \code{A72} \code{A73} \code{A74} \code{A75}}
    \item{\code{Installment_Rate}}{a numeric vector}
    \item{\code{MaritalStatusnGender}}{a factor with levels \code{A91} \code{A92} \code{A93} \code{A94}}
    \item{\code{Guarantors}}{a factor with levels \code{A101} \code{A102} \code{A103}}
    \item{\samp{Duration in Current Address}}{a numeric vector}
    \item{\code{Valuable_Asset}}{a factor with levels \code{A121} \code{A122} \code{A123} \code{A124}}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Other_Credit}}{a factor with levels \code{A141} \code{A142} \code{A143}}
    \item{\code{Housing}}{a factor with levels \code{A151} \code{A152} \code{A153}}
    \item{\code{Existing_Credits}}{a numeric vector}
    \item{\code{Job}}{a factor with levels \code{A171} \code{A172} \code{A173} \code{A174}}
    \item{\code{Dependents}}{a numeric vector}
    \item{\code{Telephone}}{a factor with levels \code{A191} \code{A192}}
    \item{\code{ForeignWorker}}{a factor with levels \code{A201} \code{A202}}
    \item{\code{Good_Bad}}{a numeric vector}
  }
}

\source{
https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
}

\examples{
data(German_Credit)
str(German_Credit)
}
\keyword{datasets}
