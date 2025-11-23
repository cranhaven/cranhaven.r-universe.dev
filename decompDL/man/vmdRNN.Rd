\name{vmdRNN}
\alias{vmdRNN}
\title{
Variational Mode Decomposition Based RNN Model
}
\description{
This function computes forecasted value with different forecasting evaluation criteria for Variational Mode Decomposition (VMD) Based RNN Model.
}
\usage{
vmdRNN (data, spl=0.8, n=4, alpha=2000, tau=0, D=FALSE, LU = 2, Epochs = 2)}
\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{spl}{
The forecast horizon.
}
  \item{n}{
The number of IMFs.
}
  \item{alpha}{
The balancing parameter.
}
  \item{tau}{
Time-step of the dual ascent.
}
  \item{D}{
a boolean.
}
\item{LU}{
Number of unit in RNN layer.
}
   \item{Epochs}{
Number of epochs.
}
}
\details{
The Variational Mode Decomposition method is a novel adaptive, non-recursive signal decomposition technology, which was introduced by Dragomiretskiy and Zosso (2014). VMD method helps to solve current decomposition methods limitation such as lacking mathematical theory, recursive sifting process which not allows for backward error correction, hard-band limits, the requirement to predetermine filter bank boundaries, and sensitivity to noise. It decomposes a series into sets of IMFs. RNN used to forecast decomposed components individually . Finally, the prediction results of all components are aggregated to formulate an ensemble output for the input time series.
}
\value{
  \item{TotalIMF }{Total number of IMFs.}
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF.}
  \item{FinalVMDRNN_forecast }{Final forecasted value of the VMD based RNN model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_VMDRNN }{Mean Absolute Error (MAE) for VMD based RNN model.}
  \item{MAPE_VMDRNN }{Mean Absolute Percentage Error (MAPE) for VMD based RNN model.}
  \item{rmse_VMDRNN }{Root Mean Square Error (RMSE) for VMD based RNN model.}
   \item{AllIMF_plots }{Decomposed IMFs and residual plot.}
   \item{plot_testset }{Test set forecasted vs actual value plot.}
 }
\references{
Choudhary, K., Jha, G.K., Kumar, R.R. and Mishra, D.C. (2019) Agricultural commodity price analysis using ensemble empirical mode decomposition: A case study of daily potato price series. Indian journal of agricultural sciences, 89(5), 882--886.

Wu, Z. and Huang, N.E. (2009) Ensemble empirical mode decomposition: a noise assisted data analysis method. Advances in adaptive data analysis, 1(1), 1--41.
}

\seealso{
emdRNN
}
\examples{
\donttest{
data("Data_Maize")
vmdRNN(Data_Maize)
}
}
\keyword{RNN}
