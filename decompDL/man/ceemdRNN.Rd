\name{ceemdRNN}
\alias{ceemdRNN}
\title{
Complementary Ensemble Empirical Mode Decomposition (CEEMD) Based Long Short Term  (RNN) Model
}
\description{
The eemdRNN function computes forecasted value with different forecasting evaluation criteria for EEMD based RNN model.
}
\usage{
ceemdRNN(data, spl=0.8, num.IMFs=emd_num_imfs(length(data)),
s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2,lg = 4,
LU = 2, Epochs = 2)}

\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{spl}{
Index of the split point and separates the data into the training and testing datasets.
}
   \item{num.IMFs}{
Number of Intrinsic Mode Function (IMF) for input series.
}
  \item{s.num}{
Integer. Use the S number stopping criterion for the EMD procedure with the given values of S. That is, iterate until the number of extrema and zero crossings in the signal differ at most by one, and stay the same for S consecutive iterations.
}
  \item{num.sift}{
Number of siftings to find out IMFs.
}
  \item{ensem.size}{
Number of copies of the input signal to use as the ensemble.
}
  \item{noise.st}{
Standard deviation of the Gaussian random numbers used as additional noise. This value is relative to the standard deviation of the input series.
}
  \item{lg}{
Lag of time series data.
}
\item{LU}{
Number of unit in RNN layer.
}
   \item{Epochs}{
Number of epochs.
}
}
\details{
A time series is decomposed by CEEMD into a set of intrinsic mode functions (IMFs) and a residual, which are modelled and predicted independently using RNN models. Finally, the ensemble output for the price series is produced by combining the forecasts of all IMFs and residuals.
}
\value{
  \item{TotalIMF }{Total number of IMFs.}
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF.}
  \item{FinalCEEMDRNN_forecast }{Final forecasted value of the CEEMD based RNN model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_CEEMDRNN }{Mean Absolute Error (MAE) for CEEMD based RNN model.}
  \item{MAPE_CEEMDRNN }{Mean Absolute Percentage Error (MAPE) for CEEMD based RNN model.}
  \item{rmse_CEEMDRNN }{Root Mean Square Error (RMSE) for CEEMD based RNN model.}
   \item{AllIMF_plots }{Decomposed IMFs and residual plot.}
   \item{plot_testset }{Test set forecasted vs actual value plot.}
 }
\references{
Choudhary, K., Jha, G.K., Kumar, R.R. and Mishra, D.C. (2019) Agricultural commodity price analysis using ensemble empirical mode decomposition: A case study of daily potato price series. Indian journal of agricultural sciences, 89(5), 882--886.

Wu, Z. and Huang, N.E. (2009) Ensemble empirical mode decomposition: a noise assisted data analysis method. Advances in adaptive data analysis, 1(1), 1--41.
}

\seealso{
eemdRNN
}
\examples{
\donttest{
data("Data_Maize")
ceemdRNN(Data_Maize)
}
}
\keyword{RNN}
