#' @importFrom keras compile layer_lstm layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict lag
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning LSTM_ts
#' @importFrom VMDecomp vmd
#' @export
#'
vmdLSTM <- function(data, spl=0.8, n=4, alpha=2000, tau=0, D=FALSE,LU = 2, Epochs = 2)
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
    VMDLSTMFit <- TSdeeplearning::LSTM_ts(as.ts(IndIMF), xtlag = lag, uLSTM = LU, nEpochs = Epochs, Split=spl)
    VMDLSTM_fcast=VMDLSTMFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(VMDLSTM_fcast))
  }
  FinalVMDLSTM_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_VMDLSTM=mean(abs(data_test - FinalVMDLSTM_fcast))
  MAPE_VMDLSTM=mean(abs(data_test - FinalVMDLSTM_fcast)/data_test)
  rmse_VMDLSTM=sqrt(mean((data_test - FinalVMDLSTM_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalVMDLSTM_fcast), col = "red",
                                  ylim = c(min(FinalVMDLSTM_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalVMDLSTM_forecast=FinalVMDLSTM_fcast, MAE_VMDLSTM=MAE_VMDLSTM,
              MAPE_VMDLSTM=MAPE_VMDLSTM, rmse_VMDLSTM=rmse_VMDLSTM,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
