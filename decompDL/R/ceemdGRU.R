#' @importFrom keras compile layer_gru layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict lag
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning GRU_ts
#' @importFrom Rlibeemd ceemdan
#' @importFrom Rlibeemd emd_num_imfs
#' @export
#'
ceemdGRU <- function(data, spl=0.8, num.IMFs=emd_num_imfs(length(data)),
                     s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2,
                     lg = 4, LU = 2, Epochs = 2)
{
  n.IMF <- num.IMFs
  AllIMF <- ceemdan(ts(data), num_imfs = num.IMFs, ensemble_size = ensem.size, noise_strength = noise.st,
                    S_number = s.num, num_siftings = num.sift, rng_seed = 0L, threads = 0L)
  data_trn <- ts(data[c(1:(nrow(data) * spl)), ])
  data_test <- ts(data[-c(1:(nrow(data) * spl)), ])
  IMF_trn <- AllIMF
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    CEEMDGRUFit <- TSdeeplearning::GRU_ts(as.ts(IndIMF), xtlag = lg, uGRU = LU, nEpochs = Epochs, Split=spl)
    CEEMDGRU_fcast=CEEMDGRUFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(CEEMDGRU_fcast))
  }
  FinalCEEMDGRU_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_CEEMDGRU=mean(abs(data_test - FinalCEEMDGRU_fcast))
  MAPE_CEEMDGRU=mean(abs(data_test - FinalCEEMDGRU_fcast)/data_test)
  rmse_CEEMDGRU=sqrt(mean((data_test - FinalCEEMDGRU_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalCEEMDGRU_fcast), col = "red",
                                  ylim = c(min(FinalCEEMDGRU_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalCEEMDGRU_forecast=FinalCEEMDGRU_fcast, MAE_CEEMDGRU=MAE_CEEMDGRU,
              MAPE_CEEMDGRU=MAPE_CEEMDGRU, rmse_CEEMDGRU=rmse_CEEMDGRU,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
