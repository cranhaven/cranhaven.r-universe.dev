#' @importFrom keras compile layer_lstm layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning LSTM_ts
#' @importFrom Rlibeemd emd_num_imfs eemd
#' @export
#'
eemdLSTM <- function(data, spl=0.8, num.IMFs=emd_num_imfs(length(data)),
                     s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2,
                     lag = 4, LU = 2, Epochs = 2)
  {
  n.IMF <- num.IMFs
  AllIMF <- eemd(ts(data), num_imfs = n.IMF, ensemble_size = ensem.size, noise_strength = noise.st,
                 S_number = s.num, num_siftings = num.sift, rng_seed = 0L, threads = 0L)
  data_trn <- ts(data[c(1:(nrow(data) * spl)), ])
  data_test <- ts(data[-c(1:(nrow(data) * spl)), ])
  IMF_trn <- AllIMF
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EEMDLSTMFit <- TSdeeplearning::LSTM_ts(as.ts(IndIMF), xtlag = lag, uLSTM = LU, nEpochs = Epochs, Split=spl)
    EEMDLSTM_fcast=EEMDLSTMFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EEMDLSTM_fcast))
  }
  FinalEEMDLSTM_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EEMDLSTM=mean(abs(data_test - FinalEEMDLSTM_fcast))
  MAPE_EEMDLSTM=mean(abs(data_test - FinalEEMDLSTM_fcast)/data_test)
  rmse_EEMDLSTM=sqrt(mean((data_test - FinalEEMDLSTM_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalEEMDLSTM_fcast), col = "red",
                                  ylim = c(min(FinalEEMDLSTM_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEEMDLSTM_forecast=FinalEEMDLSTM_fcast, MAE_EEMDLSTM=MAE_EEMDLSTM,
              MAPE_EEMDLSTM=MAPE_EEMDLSTM, rmse_EEMDLSTM=rmse_EEMDLSTM,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
