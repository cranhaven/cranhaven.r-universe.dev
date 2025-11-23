#' @importFrom keras compile layer_gru layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict lag
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning GRU_ts
#' @importFrom Rlibeemd emd_num_imfs emd
#' @export
#'
emdGRU <- function(data, spl=0.8, num.IMFs=emd_num_imfs(length(data)),
                   s.num=4L, num.sift=50L,lg = 4, LU = 2, Epochs = 2)
{
  n.IMF <- num.IMFs
  AllIMF <- emd(data, num_imfs = num.IMFs, S_number = s.num, num_siftings = num.sift)
  data_trn <- ts(data[c(1:(nrow(data) * spl)), ])
  data_test <- ts(data[-c(1:(nrow(data) * spl)), ])
  IMF_trn <- AllIMF
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EMDGRUFit <- TSdeeplearning::GRU_ts(as.ts(IndIMF), xtlag = lg, uGRU = LU, nEpochs = Epochs, Split=spl)
    EMDGRU_fcast=EMDGRUFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EMDGRU_fcast))
  }
  FinalEMDGRU_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EMDGRU=mean(abs(data_test - FinalEMDGRU_fcast))
  MAPE_EMDGRU=mean(abs(data_test - FinalEMDGRU_fcast)/data_test)
  rmse_EMDGRU=sqrt(mean((data_test - FinalEMDGRU_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalEMDGRU_fcast), col = "red",
                                  ylim = c(min(FinalEMDGRU_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEMDGRU_forecast=FinalEMDGRU_fcast, MAE_EMDGRU=MAE_EMDGRU,
              MAPE_EMDGRU=MAPE_EMDGRU, rmse_EMDGRU=rmse_EMDGRU,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
