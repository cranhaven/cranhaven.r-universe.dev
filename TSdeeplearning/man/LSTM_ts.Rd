\name{LST_ts}
\alias{LSTM_ts}
\title{
Long- Short Term Memory Model
}
\description{
The LSTM function computes forecasted value with different forecasting evaluation criteria for long- short term memory  model.
}
\usage{
LSTM_ts(xt, xtlag = 4, uLSTM = 2, Drate = 0, nEpochs = 10,
Loss = "mse", AccMetrics = "mae",ActFn = "tanh",
Split = 0.8, Valid = 0.1)}

\arguments{
  \item{xt}{
Input univariate time series (ts) data.
}
  \item{xtlag}{
Lag of time series data.
}
  \item{uLSTM}{
Number of unit in LSTM layer.
}
  \item{Drate}{
Dropout rate.
}
  \item{nEpochs}{
Number of epochs.
}
  \item{Loss}{
Loss function.
}
 \item{AccMetrics}{
Metrics.
}
\item{ActFn}{
Activation function.
}
\item{Split}{
Index of the split point and separates the data into the training and testing datasets.
}
\item{Valid}{
Validation set.
}
}
\details{
Long short-term memory (LSTM) (Hochreiter and Schmidhuber, 1997) based RNN is designed to overcome the vanishing gradients problem while dealing with long term dependencies. In contrast to standard RNN, LSTM has this peculiar and unique inbuilt ability by maintaining a memory cell to determine which unimportant features should be forgotten and which important features should be remembered during the learning process (Jaiswal et al., 2022). An LSTM model analyses and captures both short-term and long-term temporal dependencies of a complex time series effectively due to its architecture of recurrent neural network and the memory function used in the hidden nodes.
}
\value{
  \item{TrainFittedValue }{Training Fitted value for given time series data.}
  \item{TestPredictedValue}{Final forecasted value of the LSTM model.}
  \item{fcast_criteria }{Different Forecasting evaluation criteria for LSTM model.}
 }
\references{
Cho, K., Van Merriënboer, B., Bahdanau, D. and Bengio, Y. (2014). On the properties of neural machine translation: Encoder-decoder approaches. arXiv preprint arXiv:1409.1259.

}
\seealso{
GRU, RNN
}
\examples{
\donttest{
data("Data_Maize")
LSTM_ts(Data_Maize)
}
}
\keyword{LSTM}
