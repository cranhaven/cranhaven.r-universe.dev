#' @importFrom keras compile layer_simple_rnn layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict lag
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning RNN_ts
#' @importFrom Rlibeemd emd_num_imfs eemd
#' @export
#'
eemdRNN <- function(data, spl=0.8, num.IMFs=emd_num_imfs(length(data)),
                    s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2,
                    lg = 4, LU = 2, Epochs = 2)
{
  n.IMF <- num.IMFs
  AllIMF <- eemd(ts(data), num_imfs = num.IMFs, ensemble_size = ensem.size, noise_strength = noise.st,
                 S_number = s.num, num_siftings = num.sift, rng_seed = 0L, threads = 0L)
  data_trn <- ts(data[c(1:(nrow(data) * spl)), ])
  data_test <- ts(data[-c(1:(nrow(data) * spl)), ])
  IMF_trn <- AllIMF
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EEMDRNNFit <- TSdeeplearning::RNN_ts(as.ts(IndIMF), xtlag = lg, uRNN = LU, nEpochs = Epochs, Split=spl)
    EEMDRNN_fcast=EEMDRNNFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EEMDRNN_fcast))
  }
  FinalEEMDRNN_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EEMDRNN=mean(abs(data_test - FinalEEMDRNN_fcast))
  MAPE_EEMDRNN=mean(abs(data_test - FinalEEMDRNN_fcast)/data_test)
  rmse_EEMDRNN=sqrt(mean((data_test - FinalEEMDRNN_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalEEMDRNN_fcast), col = "red",
                                  ylim = c(min(FinalEEMDRNN_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEEMDRNN_forecast=FinalEEMDRNN_fcast, MAE_EEMDRNN=MAE_EEMDRNN,
              MAPE_EEMDRNN=MAPE_EEMDRNN, rmse_EEMDRNN=rmse_EEMDRNN,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
