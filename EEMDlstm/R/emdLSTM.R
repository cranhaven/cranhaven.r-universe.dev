#' @importFrom keras compile layer_lstm layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning LSTM_ts
#' @importFrom Rlibeemd emd_num_imfs emd
#' @export
#'
emdLSTM <- function(data, spl=0.8, num.IMFs=emd_num_imfs(length(data)),
                    s.num=4L, num.sift=50L,lag = 4, LU = 2, Epochs = 2)
  {
  n.IMF <- num.IMFs
  AllIMF <- emd(data, num_imfs = n.IMF, S_number = s.num, num_siftings = num.sift)
  data_trn <- ts(data[c(1:(nrow(data) * spl)), ])
  data_test <- ts(data[-c(1:(nrow(data) * spl)), ])
  IMF_trn <- AllIMF
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EMDLSTMFit <- TSdeeplearning::LSTM_ts(as.ts(IndIMF), xtlag = lag, uLSTM = LU, nEpochs = Epochs, Split=spl)
    EMDLSTM_fcast=EMDLSTMFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EMDLSTM_fcast))
  }
  FinalEMDLSTM_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EMDLSTM=mean(abs(data_test - FinalEMDLSTM_fcast))
  MAPE_EMDLSTM=mean(abs(data_test - FinalEMDLSTM_fcast)/data_test)
  rmse_EMDLSTM=sqrt(mean((data_test - FinalEMDLSTM_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalEMDLSTM_fcast), col = "red",
                                  ylim = c(min(FinalEMDLSTM_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEMDLSTM_forecast=FinalEMDLSTM_fcast, MAE_EMDLSTM=MAE_EMDLSTM,
              MAPE_EMDLSTM=MAPE_EMDLSTM, rmse_EMDLSTM=rmse_EMDLSTM,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
