#' @importFrom keras compile layer_gru layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict lag
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning GRU_ts
#' @importFrom Rlibeemd emd_num_imfs eemd
#' @export
#'
eemdGRU <- function(data, spl=0.8, num.IMFs=emd_num_imfs(length(data)),
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
    EEMDGRUFit <- TSdeeplearning::GRU_ts(as.ts(IndIMF), xtlag = lg, uGRU = LU, nEpochs = Epochs, Split=spl)
    EEMDGRU_fcast=EEMDGRUFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EEMDGRU_fcast))
  }
  FinalEEMDGRU_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EEMDGRU=mean(abs(data_test - FinalEEMDGRU_fcast))
  MAPE_EEMDGRU=mean(abs(data_test - FinalEEMDGRU_fcast)/data_test)
  rmse_EEMDGRU=sqrt(mean((data_test - FinalEEMDGRU_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalEEMDGRU_fcast), col = "red",
                                  ylim = c(min(FinalEEMDGRU_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEEMDGRU_forecast=FinalEEMDGRU_fcast, MAE_EEMDGRU=MAE_EEMDGRU,
              MAPE_EEMDGRU=MAPE_EEMDGRU, rmse_EEMDGRU=rmse_EEMDGRU,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
