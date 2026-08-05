\name{longitudinal_mixed_type_data}
\alias{longitudinal_mixed_type_data}
\docType{data}
\title{
A Simulated Dataset of Longitudinal Mixed-Type Data
}
\description{
An artificially created dataset by \code{\link{generate_longitudinal_mixed_type_data}} 
to demonstrate the use of \code{\link{clustGLMM}}. 
For details about the parameter setting see the example at the bottom of \code{\link{generate_longitudinal_mixed_type_data}}.
}
\usage{data("longitudinal_mixed_type_data")}
\format{
  A data frame with 400 rows and 32 columns.
  \describe{
    \item{\code{i}}{an id variable}
    \item{\code{x}}{continuous regressor}
    \item{\code{j}}{order of observation for given id}
    \item{\code{bs1},\code{bs2},\code{bs3},\code{bs4},\code{bs5}}{B-spline basis for \code{x}}
    \item{\code{f}}{factor regressor}
    \item{\code{g}}{a true group label from 1, 2, 3}
    \item{\code{b_num},\code{b_poi},\code{b_bin},\code{b_ord},\code{b_cat}}{random intercept for the outcome of numerical type, analogously for other types}
    \item{\code{pred_num},\code{pred_poi},\code{pred_bin},\code{pred_ord},\code{pred_cat1},\code{pred_cat2},\code{pred_cat3},\code{pred_cat4}}{predictor for the outcome of numerical type, analogously for other types, categorical has multiple predictors for different categories}
    \item{\code{c_ord1},\code{c_ord2},\code{c_ord3},\code{c_ord4}}{ordered intercept for level 1, analogously for other levels}
    \item{\code{ynum},\code{ypoi},\code{ybin},\code{yord},\code{ycat}}{an outcome of numerical type, analogously for other types}
  }
}
\examples{
data(longitudinal_mixed_type_data)
summary(longitudinal_mixed_type_data)
}
\keyword{longitudinal_mixed_type_data}
