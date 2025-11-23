\name{GRU_ts}
\alias{GRU_ts}
\title{
Gated Recurrent Unit Model
}
\description{
The GRU function computes forecasted value with different forecasting evaluation criteria for gated recurrent unit  model.
}
\usage{
GRU_ts(xt, xtlag = 4, uGRU = 2, Drate = 0, nEpochs = 10,
Loss = "mse", AccMetrics = "mae",ActFn = "tanh",
Split = 0.8, Valid = 0.1)}

\arguments{
  \item{xt}{
Input univariate time series (ts) data.
}
  \item{xtlag}{
Lag of time series data.
}
  \item{uGRU}{
Number of unit in GRU layer.
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
The gated recurrent unit (GRU) was introduced by Cho et al.(2014). A GRU is part of a specific model of recurrent neural network that intends to use connections through a sequence of nodes to perform machine learning tasks associated with memory and clustering. Its internal structure is simpler and, therefore, it is also easier to train, as less calculation is required to upgrade the internal states. The update port controls the extent to which the state information from the previous moment is retained in the current state, while the reset port determines whether the current state should be combined with the previous information. Gated recurrent units help to adjust neural network input weights to solve the vanishing gradient problem that is a common issue with recurrent neural networks.
}
\value{
  \item{TrainFittedValue }{Training Fitted value for given time series data.}
  \item{TestPredictedValue}{Final forecasted value of the GRU model.}
  \item{fcast_criteria }{Different Forecasting evaluation criteria for GRU model.}
 }
\references{
Cho, K., Van Merriënboer, B., Bahdanau, D. and Bengio, Y. (2014). On the properties of neural machine translation: Encoder-decoder approaches. arXiv preprint arXiv:1409.1259.

}
\seealso{
LSTM, RNN
}
\examples{
\donttest{
data("Data_Maize")
GRU_ts(Data_Maize)
}
}
\keyword{GRU}
