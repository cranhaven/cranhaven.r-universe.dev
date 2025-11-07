goodness<-function (model,data){
  MSE <- modelr::mse(model = model, data = data)
  RMSE <- modelr::rmse(model = model, data = data)
  Rsquare <- modelr::rsquare(model = model, data = data)
  R2Adj<-function(r2,n,k){
    value=1-(n-1)/(n-k-1)*(1-r2)
    return(value)
  }
  adj.Rsquare<-R2Adj(r2=Rsquare,n=nrow(data),k=length(stats::coef(model)))
  MAE <- modelr::mae(model = model, data = data)
  MAPE <- modelr::mape(model = model, data = data)
  RASE <- modelr::rsae(model = model, data = data)
  AIC = AIC(object = model)
  BIC = AIC(object = model)
  return(data.frame(MSE = MSE, RMSE = RMSE, Rsquare = Rsquare,
                    adj.Rsquare = adj.Rsquare,
                    MAE = MAE, MAPE = MAPE, RASE = RASE, AIC = AIC, BIC = BIC))
}
