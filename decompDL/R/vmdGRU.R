#' @importFrom keras compile layer_gru layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict lag
#' @importFrom magrittr %>%
#' @importFrom TSdeeplearning GRU_ts
#' @importFrom VMDecomp vmd
#' @export
#'
vmdGRU <- function(data, spl=0.8, n=4, alpha=2000, tau=0, D=FALSE,LU = 2, Epochs = 2)
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
    VMDGRUFit <- TSdeeplearning::GRU_ts(as.ts(IndIMF), xtlag = lag, uGRU = LU, nEpochs = Epochs, Split=spl)
    VMDGRU_fcast=VMDGRUFit$TestPredictedValue
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(VMDGRU_fcast))
  }
  FinalVMDGRU_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_VMDGRU=mean(abs(data_test - FinalVMDGRU_fcast))
  MAPE_VMDGRU=mean(abs(data_test - FinalVMDGRU_fcast)/data_test)
  rmse_VMDGRU=sqrt(mean((data_test - FinalVMDGRU_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  Actualvsforecast_testset = plot(ts(FinalVMDGRU_fcast), col = "red",
                                  ylim = c(min(FinalVMDGRU_fcast) - 20, max(data_test) + 20),
                                  ylab = "Value", main = "Test Set", type = "l",
                                  lwd = 3)
  lines(ts(data_test), col = "black", type = "l",
        lwd = 3)
  legend("bottomright", c("Forecast", "Actual"),
         lty = c(1, 1), inset = c(0, 1), xpd = TRUE, horiz = TRUE,
         bty = "n", col = c("red", "black"))
  return(list(TotalIMF = n, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalVMDGRU_forecast=FinalVMDGRU_fcast, MAE_VMDGRU=MAE_VMDGRU,
              MAPE_VMDGRU=MAPE_VMDGRU, rmse_VMDGRU=rmse_VMDGRU,
              AllIMF_plots=AllIMF_plots,plot_testset = Actualvsforecast_testset))
}
