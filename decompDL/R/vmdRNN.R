#' @importFrom keras compile layer_simple_rnn layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict lag
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning RNN_ts
#' @importFrom VMDecomp vmd
#' @export
#'
vmdRNN <- function(data, spl=0.8, n=4, alpha=2000, tau=0, D=FALSE,LU = 2, Epochs = 2)
{
  data <- ts(data)
  data<- as.vector(data)
  v<- vmd(data , alpha=2000, tau=0, K=n, DC=D, init=1, tol = 1e-6)
  AllIMF<-v$u
  data_trn <- ts(data[c(1:(nrow(data) * spl)), ])
  data_test <- ts(data[-c(1:(nrow(data) * spl)), ])
  IMF_trn <- AllIMF
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    VMDRNNFit <- TSdeeplearning::RNN_ts(as.ts(IndIMF), xtlag = lag, uRNN = LU, nEpochs = Epochs, Split=spl)
    VMDRNN_fcast=VMDRNNFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(VMDRNN_fcast))
  }
  FinalVMDRNN_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_VMDRNN=mean(abs(data_test - FinalVMDRNN_fcast))
  MAPE_VMDRNN=mean(abs(data_test - FinalVMDRNN_fcast)/data_test)
  rmse_VMDRNN=sqrt(mean((data_test - FinalVMDRNN_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalVMDRNN_fcast), col = "red",
                                  ylim = c(min(FinalVMDRNN_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalVMDRNN_forecast=FinalVMDRNN_fcast, MAE_VMDRNN=MAE_VMDRNN,
              MAPE_VMDRNN=MAPE_VMDRNN, rmse_VMDRNN=rmse_VMDRNN,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
