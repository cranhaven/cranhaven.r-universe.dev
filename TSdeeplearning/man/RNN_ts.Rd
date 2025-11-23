\name{RNN_ts}
\alias{RNN_ts}
\title{
Recurrent neural network Model
}
\description{
The RNN function computes forecasted value with different forecasting evaluation criteria for recurrent neural network model.
}
\usage{
RNN_ts(xt, xtlag = 4, uRNN = 2, Drate = 0, nEpochs = 10,
Loss = "mse", AccMetrics = "mae",ActFn = "tanh",
Split = 0.8, Valid = 0.1)}

\arguments{
  \item{xt}{
Input univariate time series (ts) data.
}
  \item{xtlag}{
Lag of time series data.
}
  \item{uRNN}{
Number of unit in RNN layer.
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
Recurrent neural networks (RNNs) (Rumelhart 1986) add the explicit handling of order between observations when learning a mapping function from inputs to outputs. RNNs actually process single elements of any input sequence at a particular time, and maintain a ‘state vector’ in their hidden units. Nevertheless, when the interval of data dependencies increases, the standard RNNs tend to suffer increasingly heavily from the problem of either vanishing gradient or exploding gradient (Bengio et al. 1994; Lin et al. 1996).
}
\value{
  \item{TrainFittedValue }{Training Fitted value for given time series data.}
  \item{TestPredictedValue}{Final forecasted value of the RNN model.}
  \item{fcast_criteria }{Different Forecasting evaluation criteria for RNN model.}
 }
\references{
Bengio et al. 1994; Lin Sagheer A, Kotb M (2019) Time series forecasting of petroleum production using deep LSTM recurrent networks. Neurocomputing 323: 203--213.

Rumelhart DE (1986) Learning internal representations by error propagation. In: Parallel distributed processing: Explorations in the microstructure of cognition. pp 318--362.

Jha, G. K. and Sinha, K. (2014). Time-delay neural networks for time series prediction: An application to the monthly wholesale price of oilseeds in India. Neural Computing and Applications, 24(3--4), 563--571.
Jaiswal, R., Jha, G. K., Kumar, R. R. and Choudhary, K. (2022). Deep long short-term memory based model for agricultural price forecasting. Neural Computing and Applications, 34(6), 4661--4676.


}
\seealso{
LSTM, GRU
}
\examples{
\donttest{
data("Data_Maize")
RNN_ts(Data_Maize)
}
}
\keyword{RNN}
