\name{emdLSTM}
\alias{emdLSTM}
\title{
Empirical Mode Decomposition (EEMD) Based Long Short Term (LSTM) Model
}
\description{
The emdLSTM function computes forecasted value with different forecasting evaluation criteria for EMD based LSTM model.
}
\usage{
emdLSTM(data, spl=0.8, num.IMFs=emd_num_imfs(length(data)),
s.num=4L, num.sift=50L,lag = 4, LU = 2, Epochs = 2)}

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
   \item{lag}{
Lag of time series data.
}
\item{LU}{
Number of unit in GRU layer.
}
   \item{Epochs}{
Number of epochs.
}
}
\details{
A time series is decomposed by EMD into a set of intrinsic mode functions (IMFs) and a residual, which are modelled and predicted independently using LSTM models. Finally, the ensemble output for the price series is produced by combining the forecasts of all IMFs and residuals.
}
\value{
  \item{TotalIMF }{Total number of IMFs.}
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF.}
  \item{FinalEEMDLSTM_forecast }{Final forecasted value of the EEMD based LSTM model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_EEMDLSTM }{Mean Absolute Error (MAE) for EEMD based LSTM model.}
  \item{MAPE_EEMDLSTM }{Mean Absolute Percentage Error (MAPE) for EEMD based LSTM model.}
  \item{rmse_EEMDLSTM }{Root Mean Square Error (RMSE) for EEMD based LSTM model.}
   \item{AllIMF_plots }{Decomposed IMFs and residual plot.}
   \item{plot_testset }{Test set forecasted vs actual value plot.}
 }
\references{
Choudhary, K., Jha, G.K., Kumar, R.R. and Mishra, D.C. (2019) Agricultural commodity price analysis using ensemble empirical mode decomposition: A case study of daily potato price series. Indian journal of agricultural sciences, 89(5), 882--886.

Huang, N.E., Shen, Z., Long, S.R., Wu, M.C., Shih, H.H., Zheng, Q. and Liu, H.H. (1998) The empirical mode decomposition and the Hilbert spectrum for nonlinear and non stationary time series analysis. In Proceedings of the Royal Society of London A: mathematical, physical and engineering sciences. 454, 903--995.

Jha, G.K. and Sinha, K. (2014) Time delay neural networks for time series prediction: An application to the monthly wholesale price of oilseeds in India. Neural Computing and Applications, 24, 563--571.
}

\seealso{
eemdLSTM
}
\examples{
\donttest{
data("Data_Maize")
emdLSTM(Data_Maize)
}
}
\keyword{LSTM}
