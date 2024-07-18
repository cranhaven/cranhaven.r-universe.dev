
#' @title OptiSembleForecasting
#' @description Optimization Based Ensemble Forecasting Using MCS Algorithm
#' @param TS Time series data with first column as date
#' @param Lag Number of lag for modelling
#' @param Optimization Optimization technique
#' @param Split_ratio Train-Test Split Ration
#' @import stats keras tsutils readxl tibble tensorflow forecast dplyr neuralnet Metrics MCS caretForecast kknn metaheuristicOpt FactoMineR factoextra utils
#' @return
#' \itemize{
#'   \item SelectedModel: Selected models with weights
#'   \item Accuracy: Accuracy matrix
#'   \item TestResults: Final predicted value
#' }
#' @examples
#' \donttest{
#' library(OptiSembleForecasting)
#' date<-seq.Date(from = as.Date('2019-09-17'), to = as.Date('2022-09-18'), by = 'days')
#' value<-rnorm(length(date),100, 50)
#' data<-cbind(date,value)
#' fit<-OptiSembleForcasting(TS=data,Lag = 20, Optimization = "ABC",Split_ratio = 0.9)
#' }
#' @export
#' @references
#' \itemize{
#'\item Wang, J., Wang, Y., Li, H., Yang, H. and Li, Z. (2022). Ensemble forecasting system based on decomposition-selection-optimization for point and interval carbon price prediction. Applied Mathematical Modelling, doi.org/10.1016/j.apm.2022.09.004.
#' \item Qu, Z., Li, Y., Jiang, X. and Niu, C. (2022). An innovative ensemble model based on multiple neural networks and a novel heuristic optimization algorithm for COVID-19 forecasting. Expert System Application, doi:10.1016/j.eswa.2022.118746
#' \item Kriz, K.A. (2019). Ensemble Forecasting. In: Williams, D., Calabrese, T. (eds) The Palgrave Handbook of Government Budget Forecasting. Palgrave Studies in Public Debt, Spending, and Revenue. Palgrave Macmillan, Cham. https://doi.org/10.1007/978-3-030-18195-6_21
#' }
OptiSembleForcasting<-function(TS,Lag,Optimization,Split_ratio){
  . <- NULL
  date<-TS[,1]
  ts<-as.ts(TS[,2])
  lag_exo<-0
  lag<-Lag
  splitratio<-Split_ratio
  LagLength<-1
  Sampling_Rate<-1
  BatchSize <- 1
  Stride<-1

  Dense1_Unit<-1
  Dense2_Unit<-1
  Dense3_Unit<-1
  Dense4_Unit<-1
  Dense5_Unit<-1
  Dense_Final_Unit<-1

  Activation<-'sigmoid'

  RNN1_Unit<-Lag*2

  GRU1_Unit<-Lag*2

  LSTM1_Unit<-Lag*2
  LSTM2_Unit<-Lag*2
  LSTM3_Unit<-Lag*2
  LSTM4_Unit<-Lag*2
  LSTM5_Unit<-Lag*2

  Optimizer<-optimizer_rmsprop()
  Loss <-"mse"
  Metrics <-"mae"
  ValidationSplit <- 0.2
  Epochs <- 50
  Validation_data <- NULL


  a<- 1/(max(ts)-min(ts))
  b<- min(ts)/(max(ts)-min(ts))


  normalize <- function(x) {
    return ((x*a)-b)
  }

  denormalize <- function(x) {
    return ((x + b) / a)
  }

  data_normalise <- apply( ts,2, normalize)
  lag_y<-lagmatrix(as.ts(data_normalise),lag = c(0:(lag)))[-c(1:lag),]


  ##################### Data Split #################################

  train_normalise <- lag_y[c(1:(nrow(lag_y)*splitratio)),]
  test_normalise<- lag_y[-c(1:(nrow(lag_y)*splitratio)),]

  train_y<-train_normalise[,1]
  train_x<-train_normalise[,c(2:(lag+1))]

  test_y<-test_normalise[,1]
  test_x<-test_normalise[,c(2:(lag+1))]

  dim(train_x)<-c(nrow(train_x),ncol(train_x),1)
  dim(test_x)<-c(nrow(test_x),ncol(test_x),1)
  n_train<-nrow(train_normalise)
  n_test<-nrow(test_normalise)

  ############# Simple RNN model ########################

  simple_rnn_model <- keras_model_sequential() %>%
    layer_dense(units = Dense1_Unit,input_shape = c(dim(train_x)[2],dim(train_x)[3])) %>%
    layer_simple_rnn(units = RNN1_Unit) %>%
    layer_dense(units = Dense_Final_Unit)

  simple_rnn_model %>% compile(optimizer = optimizer_rmsprop(), loss=Loss, metrics=Metrics)

  simple_rnn_history <- simple_rnn_model %>% fit(
    train_x,train_y,
    batch_size = BatchSize,
    epochs = Epochs,validation_split=ValidationSplit)

  rnn_fiited_norm <- simple_rnn_model %>%
    predict(train_x, batch_size=BatchSize)
  train_rnn_fiited<-denormalize(rnn_fiited_norm)
  dim(train_rnn_fiited)<-c(nrow(train_rnn_fiited))

  rnn_predicted_norm <- simple_rnn_model %>%
    predict(test_x,batch_size=BatchSize)%>%.[,1]
  test_rnn_predicted<-denormalize(rnn_predicted_norm)
  dim(test_rnn_predicted)<-c(nrow(test_rnn_predicted))


  ################################# LSTM model ###########################################

  lstm_model <- keras_model_sequential() %>%
    layer_lstm(units =LSTM1_Unit, input_shape = c(dim(train_x)[2],dim(train_x)[3]), return_sequences = FALSE) %>%
    layer_dense(units = Dense_Final_Unit)

  lstm_model %>% compile(optimizer = optimizer_rmsprop(), loss=Loss, metrics=Metrics)


  lstm_history <- lstm_model %>% fit(
    train_x,train_y,
    batch_size = BatchSize,
    epochs = Epochs,validation_split=ValidationSplit
  )
  lstm_fiited_norm <- lstm_model %>%
    predict(train_x,batch_size=BatchSize)
  train_lstm_fiited<-denormalize(lstm_fiited_norm)
  dim(train_lstm_fiited)<-c(nrow(train_lstm_fiited))

  lstm_predicted_norm <- lstm_model %>%
    predict(test_x,batch_size=BatchSize)
  test_lstm_predicted<-denormalize(lstm_predicted_norm)
  dim(test_lstm_predicted)<-c(nrow(test_lstm_predicted))

  ############ GRU model ###################


  GRU_model <- keras_model_sequential() %>%
    layer_gru(units =GRU1_Unit, input_shape = c(dim(train_x)[2],dim(train_x)[3]), return_sequences = FALSE) %>%
    layer_dense(units = Dense_Final_Unit)

  GRU_model %>% compile(optimizer = optimizer_rmsprop(), loss=Loss, metrics=Metrics)

  GRU_history <- GRU_model %>% fit(
    train_x, train_y,
    batch_size = BatchSize,
    epochs = Epochs,validation_split=ValidationSplit
  )

  GRU_fiited_norm <- GRU_model %>%
    predict(train_x, batch_size=BatchSize)
  train_GRU_fiited<-denormalize(GRU_fiited_norm)
  dim(train_GRU_fiited)<-c(nrow(train_GRU_fiited))


  # GRU  Prediction
  GRU_model %>% evaluate(test_x,test_y)
  GRU_predicted_norm <- GRU_model %>%
    predict(test_x,batch_size=BatchSize)
  test_GRU_predicted<-denormalize(GRU_predicted_norm)
  dim(test_GRU_predicted)<-c(nrow(test_GRU_predicted))

  ###################### bidirectional_lstm model #############

  bidirectional_lstm_model <- keras_model_sequential() %>%
    bidirectional(layer_lstm(units =LSTM1_Unit, return_sequences = FALSE), input_shape = c(dim(train_x)[2],dim(train_x)[3])) %>%
    layer_dense(units = Dense_Final_Unit)

  bidirectional_lstm_model %>% compile(optimizer = optimizer_rmsprop(), loss=Loss, metrics=Metrics)


  bidirectional_lstm_history <- bidirectional_lstm_model %>% fit(
    train_x, train_y,
    batch_size = BatchSize,
    epochs = Epochs,validation_split=ValidationSplit
  )


  bidirectional_lstm_fiited_norm <- bidirectional_lstm_model %>%
    predict(train_x,batch_size=BatchSize)
  train_bidirectional_lstm_fiited<-denormalize(bidirectional_lstm_fiited_norm)
  dim(train_bidirectional_lstm_fiited)<-c(nrow(train_bidirectional_lstm_fiited))

  bidirectional_lstm_predicted_norm <- bidirectional_lstm_model %>%
    predict(test_x,batch_size=BatchSize)
  test_bidirectional_lstm_predicted<-denormalize(bidirectional_lstm_predicted_norm)
  dim(test_bidirectional_lstm_predicted)<-c(nrow(test_bidirectional_lstm_predicted))

  ############## deep_lstm model #######################

  deep_lstm_model <- keras_model_sequential() %>%
    layer_lstm(units =LSTM1_Unit,input_shape = c(dim(train_x)[2],dim(train_x)[3]), return_sequences = TRUE) %>%
    layer_lstm(units =LSTM2_Unit,input_shape = c(dim(train_x)[2],dim(train_x)[3]), return_sequences = TRUE) %>%
    layer_lstm(units =LSTM3_Unit,input_shape = c(dim(train_x)[2],dim(train_x)[3]), return_sequences = TRUE) %>%
    layer_lstm(units =LSTM4_Unit,input_shape = c(dim(train_x)[2],dim(train_x)[3]), return_sequences = TRUE) %>%
    layer_lstm(units =LSTM5_Unit,input_shape = c(dim(train_x)[2],dim(train_x)[3]), return_sequences = FALSE) %>%
    layer_dense(units = Dense_Final_Unit)

  deep_lstm_model %>% compile(optimizer = optimizer_rmsprop(), loss=Loss, metrics=Metrics)

  summary(deep_lstm_model)

  deep_lstm_history <- deep_lstm_model %>% fit(
    train_x, train_y,
    batch_size = BatchSize,
    epochs = Epochs,validation_split=ValidationSplit
  )

  deep_lstm_fiited_norm <- deep_lstm_model %>%
    predict(train_x,batch_size=BatchSize)
  train_deep_lstm_fiited<-denormalize(deep_lstm_fiited_norm)
  dim(train_deep_lstm_fiited)<-c(nrow(train_deep_lstm_fiited))


  deep_lstm_predicted_norm <- deep_lstm_model %>%
    predict(test_x,batch_size=BatchSize)
  test_deep_lstm_predicted<-denormalize(deep_lstm_predicted_norm)
  dim(test_deep_lstm_predicted)<-c(nrow(test_deep_lstm_predicted))
  ########### ARIMA ################

  arima_train_y<- train_y
  arima_train_x<- train_x
  dim(arima_train_x)<-c(nrow(arima_train_x),ncol(arima_train_x))
  arima_test_y<-test_y
  arima_test_x<-test_x
  dim(arima_test_x)<-c(nrow(arima_test_x),ncol(arima_test_x))


  auto_arima<-auto.arima(arima_train_y)
  orderNS<-c(auto_arima$arma[1], auto_arima$arma[6],auto_arima$arma[2])
  orderS<-c(auto_arima$arma[3], auto_arima$arma[7],auto_arima$arma[4])
  arima_model=arima(arima_train_y, order=orderNS)
  p_value<-1-pt(abs(arima_model$coef)/sqrt(diag(arima_model$var.coef)),arima_model$nobs-nrow(as.matrix(arima_model$coef)))
  z<-abs(arima_model$coef)/sqrt(diag(arima_model$var.coef))
  table.arima<-((data.frame(arima_model$coef,p_value)))
  train_ARIMA_fitted<-as.vector(denormalize(arima_train_y-arima_model$residuals))
  predict_arima<-forecast::forecast(arima_model, h= nrow(test_x))
  test_ARIMA_predicted<-as.vector(denormalize(predict_arima$mean))

  ################ TS Aritificial Neural Network ##########################
  ann_y<-as.ts(normalize(ts))

  ann_train_y<-as.ts(ann_y[(1:(n_train+lag))])

  ann<- nnetar(
    ann_train_y,
    p=lag,
    size =(lag/2),
    repeats = 1,
    lambda = NULL,
    model = NULL,
    subset = NULL,
    scale.inputs = FALSE)

  train_ANN_fitted<-as.vector(denormalize(ann$fitted[-c(1:lag)]))
  predict_ann<-forecast::forecast(ann,h=nrow(test_x))
  test_ANN_predicted<-as.vector(denormalize(as.ts(predict_ann)))

  ############################# SVR ###################################

  AR_SVR<-ARml(ann_train_y,max_lag = lag, caret_method = "svmRadial")

  train_SVR_fitted <- denormalize(AR_SVR$fitted)[((lag+1):(n_train+lag))]
  predict_svr<-caretForecast::forecast(AR_SVR,h=nrow(test_x))
  test_SVR_predicted<-as.vector(denormalize(as.ts(predict_svr)))

  ############ Random forest ######################
  AR_RF<-ARml(ann_train_y,max_lag = lag, caret_method = "rf")

  train_RF_fitted <- denormalize(AR_RF$fitted)[((lag+1):(n_train+lag))]
  predict_rf<-caretForecast::forecast(AR_RF,h=nrow(test_x))
  test_RF_predicted<-as.vector(denormalize(as.ts(predict_rf)))

  ########## KNN Regression ############
  AR_KNN<-ARml(ann_train_y,max_lag = lag, caret_method = 'knn')

  train_KNN_fitted <- denormalize(AR_KNN$fitted)[((lag+1):(n_train+lag))]
  predict_knn<-caretForecast::forecast(AR_KNN,h=nrow(test_x))
  test_KNN_predicted<-as.vector(denormalize(as.ts(predict_knn)))

  ############ XGBTree #######################
  AR_XGB<-ARml(ann_train_y,max_lag = lag, caret_method = 'xgbTree')

  train_XGB_fitted <- denormalize(AR_XGB$fitted)[((lag+1):(n_train+lag))]
  predict_xgb<-caretForecast::forecast(AR_XGB,h=nrow(test_x))
  test_XGB_predicted<-as.vector(denormalize(as.ts(predict_xgb)))


  ############# TBATS model #######################################

  TBATSFit<-forecast::tbats(as.ts(arima_train_y))
  train_TBATS_fitted <- denormalize(as.vector(TBATSFit$fitted.values))
  test_TBATS_predicted<-denormalize(as.ts(predict(TBATSFit, h=length(arima_test_y)))[,1])

  ############# ETS model #######################################

  ETSFit<-forecast::ets(as.ts(arima_train_y))
  train_ETS_fitted <- denormalize(ETSFit$fitted)[((lag+1):(n_train+lag))]
  predict_ets<-forecast::forecast(ETSFit,h=nrow(test_x))
  test_ETS_predicted<-as.vector(denormalize(as.ts(predict_ets$mean)))

  ################### Actual value ########################################
  train_actual<-cbind(date, ts)[((lag+1):(n_train+lag)),]
  test_actual<-cbind(date, ts)[((n_train+lag+1):(nrow(ts))),]

  train_fitted<-cbind(train_actual,train_rnn_fiited,train_GRU_fiited,train_lstm_fiited,train_bidirectional_lstm_fiited,train_deep_lstm_fiited, train_ANN_fitted,train_SVR_fitted, train_RF_fitted, train_KNN_fitted, train_XGB_fitted, train_ARIMA_fitted,train_ETS_fitted, train_TBATS_fitted)
  test_predicted<-cbind(test_actual,test_rnn_predicted,test_GRU_predicted,test_lstm_predicted,test_bidirectional_lstm_predicted,test_deep_lstm_predicted,  test_ANN_predicted, test_SVR_predicted, test_RF_predicted, test_KNN_predicted, test_XGB_predicted, test_ARIMA_predicted,test_ETS_predicted, test_TBATS_predicted)
  colnames(train_fitted)<-c("Date","Actual","RNN", "GRU", "LSTM","Bidirectional LSTM","Deep LSTM", "ANN", "SVR", "RF","KNN", "XGB","ARIMA","ETS", "TBATS")
  colnames(test_predicted)<-c("Date","Actual","RNN", "GRU", "LSTM","Bidirectional LSTM","Deep LSTM", "ANN", "SVR", "RF","KNN", "XGB","ARIMA","ETS", "TBATS")


  ################ AccuracyTable ###############


  train_test<-rbind(train_fitted,NA,NA,NA, test_predicted)

  ########### MCS ########################
  Error_function <- function(actual,predicted) {
    loss_all<- NULL
    for (e in c(3:ncol(test_predicted))) {
      realized<-test_predicted[,2]
      evaluated<-test_predicted[,e]
      l1 = (evaluated - realized)^2
      l2 = (evaluated^2 - realized^2)^2
      l3 = log(evaluated^2) + realized^2 * evaluated^(-2)
      l4 = (log(realized^2 * evaluated^-2))^2
      l5 = abs(evaluated - realized)
      l6 = abs(evaluated^2 - realized^2)
      l_bind<-cbind(l1,l2,l3, l4, l5, l6)
      loss_all<-rbind(loss_all,l_bind)
    }

    loss<-NULL
    loss1 = (predicted - actual)^2
    loss2 = (predicted^2 - actual^2)^2
    loss3 = log(predicted^2) + actual^2 * predicted^(-2)
    loss4 = (log(actual^2 * predicted^-2))^2
    loss5 = abs(predicted - actual)
    loss6 = abs(predicted^2 - actual^2)
    loss<-cbind(loss1,loss2,loss3, loss4, loss5, loss6)
    norm_loss<-NULL
    for (l in c(1:ncol(loss_all))) {
      a<-min(loss_all[,l])
      b<-max(loss_all[,l])
      minMax <- function(x) {
        (x - a) / (b - a)
      }
      norm_loss_1<-minMax(loss[,l])
      norm_loss<- cbind(norm_loss,norm_loss_1)
    }
    colnames(norm_loss)<-colnames(loss)
    res_pca <- PCA(norm_loss,scale.unit= TRUE,graph = FALSE)
    eig_val <- get_eigenvalue(res_pca)
    no_pca<-sum(eig_val[,1]>= 1)
    var <- get_pca_var(res_pca)
    var_cont<-fviz_contrib(res_pca, choice = "var", axes = c(seq(1,no_pca,1)))
    Weight<-(var_cont$data[,2]/100)
    final_loss<-as.data.frame(apply(norm_loss*Weight,1,sum))
    names(final_loss)<-names(predicted)
    error<-final_loss
    return(error)
  }


  err<-NULL
  for (k in 3:ncol(test_predicted)) {

    err1<-Error_function(test_predicted[,2],test_predicted[,k])
    err1<-as.matrix(err1)
    err<-cbind(err,err1)
  }

  colnames(err)<-colnames(test_predicted[,c(3:ncol(test_predicted))])

  MCS<-MCSprocedure(err, alpha=0.01)
  var_sel<-names(MCS@show[,1])
  train_sel<-cbind(train_fitted[,2],train_fitted[,c(var_sel)])
  test_sel<-cbind(test_predicted[,2],test_predicted[,c(var_sel)])


  ######## optimization ####################
  numVar <- ncol(test_sel)-1

  rangeVar <- matrix(c(0,1), nrow=2)
  input_data <- test_sel

  if(ncol(test_sel)==3){
    message("Number of selected model is 1")
  } else if(ncol(test_sel)==3){
    sphere <- function(x){

      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3])^2)

      return(error_fn)
    }
  } else if(ncol(test_sel)==4){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4])^2)
      return(error_fn)
    }
  } else if(ncol(test_sel)==5){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4]-x[4]* input_data[,5])^2)
      return(error_fn)
    }
  } else if(ncol(test_sel)==6){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4]-x[4]* input_data[,5]-x[5]* input_data[,6])^2)
      return(error_fn)
    }
  } else if(ncol(test_sel)==7){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4]-x[4]* input_data[,5]-x[5]* input_data[,6]
                       -x[6]* input_data[,7])^2)
      return(error_fn)
    }
  } else if(ncol(test_sel)==8){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4]-x[4]* input_data[,5]-x[5]* input_data[,6]
                       -x[6]* input_data[,7]-x[7]* input_data[,8])^2)
      return(error_fn)
    }
  } else if(ncol(test_sel)==9){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4]-x[4]* input_data[,5]-x[5]* input_data[,6]
                       -x[6]* input_data[,7]-x[8]* input_data[,9])^2)
      return(error_fn)
    }
  } else if(ncol(test_sel)==10){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4]-x[4]* input_data[,5]-x[5]* input_data[,6]
                       -x[6]* input_data[,7]-x[8]* input_data[,9]-x[9]* input_data[,10])^2)
      return(error_fn)
    }
  } else if(ncol(test_sel)==11){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4]-x[4]* input_data[,5]-x[5]* input_data[,6]
                       -x[6]* input_data[,7]-x[8]* input_data[,9]-x[9]* input_data[,10]
                       -x[10]* input_data[,11])^2)
      return(error_fn)
    }
  }else if(ncol(test_sel)==12){
    sphere <- function(x){
      error_fn <- sum((input_data[,1] - x[1]*input_data[, 2]
                       -x[2]* input_data[,3]-x[3]* input_data[, 4]-x[4]* input_data[,5]-x[5]* input_data[,6]
                       -x[6]* input_data[,7]-x[8]* input_data[,9]-x[9]* input_data[,10]
                       -x[10]* input_data[,11]-x[11]* input_data[,12])^2)
      return(error_fn)
    }
  }else{
    message("Number of selected model is more than 11")
  }

  if(Optimization=="ABC"){
    opt <- ABC(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)

  } else if (Optimization=="ALO"){
    opt <- ALO(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="BA"){
    opt <- BA(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
              maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="BHO"){
    opt <- BHO(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="CLONALG"){
    opt <- CLONALG(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
                   maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="CS"){
    opt <- CS(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
              maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  }else if (Optimization=="CSO"){
    opt <- CSO(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="DA"){
    opt <- DA(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
              maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="DE"){
    opt <- DE(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
              maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="FFA"){
    opt <- FFA(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="GA"){
    opt <- GA(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
              maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="GBS"){
    opt <- GBS(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="GOA"){
    opt <- GOA(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="GWO"){
    opt <- GWO(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="HS"){
    opt <- HS(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
              maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="KH"){
    opt <- KH(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
              maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="MFO"){
    opt <- MFO(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="PSO"){
    opt <- PSO(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="SCA"){
    opt <- SCA(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="SFL"){
    opt <- SFL(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else if (Optimization=="WOA"){
    opt <- WOA(sphere, optimType="MIN", numVar, numPopulation=nrow(test_sel),
               maxIter=1000,rangeVar)
    optimum_value <- sphere(opt)
  } else {
    message("Please select a valid optimization technique")
  }

  final_predict<- NULL
  for (q in c(1:nrow(test_sel))) {
    row_pred<-sum(test_sel[q,-1]*opt)
    final_predict<-rbind(final_predict,row_pred)
  }
  colnames(final_predict)<-"Ensemble"

  compare_data<- cbind(test_predicted,final_predict)

  ########### accuracy ###############
  AccuracyTable<-matrix(nrow = (ncol(compare_data)-2),ncol = 9)
  for (c in 3:ncol(compare_data)) {
    test_rmse<-rmse(compare_data[,2],compare_data[,c])
    test_mape<-mape(compare_data[,2],compare_data[,c])


    test_mae<-mae(compare_data[,2],compare_data[,c])
    test_rrse<-rrse(compare_data[,2],compare_data[,c])
    test_mdae <-mdae(compare_data[,2],compare_data[,c])
    test_rmsle<-rmsle(compare_data[,2],compare_data[,c])
    test_rae<-rae(compare_data[,2],compare_data[,c])
    test_smape<-smape(compare_data[,2],compare_data[,c])

    test_r_square<-as.numeric(((cor(compare_data[,2],compare_data[,c]))^2))


    AccuracyTable[(c-2),1]<-round(test_rmse,digits = 4)
    AccuracyTable[(c-2),2]<-round(test_mape,digits = 4)
    AccuracyTable[(c-2),3]<-round(test_mae,digits = 4)
    AccuracyTable[(c-2),4]<-round(test_rrse,digits = 4)
    AccuracyTable[(c-2),5]<-round(test_mdae,digits = 4)
    AccuracyTable[(c-2),6]<-round(test_rmsle,digits = 4)
    AccuracyTable[(c-2),7]<-round(test_rae,digits = 4)
    AccuracyTable[(c-2),8]<-round(test_smape,digits = 4)
    AccuracyTable[(c-2),9]<-round(test_r_square,digits = 4)
  }

  colnames(AccuracyTable)<-c("RMSE","MAPE","MAE","APE","Bias","RMSLE","RAE","SMAPE", "R2")
  row.names(AccuracyTable)<-c("RNN", "GRU", "LSTM","Bidirectional LSTM","Deep LSTM", "ANN", "SVR", "RF","KNN","XGB","ARIMA","ETS", "TBATS", "Ensemble")
  model_weight<-cbind(as.vector(var_sel),opt)
  results<-list(SelectedModel=model_weight, Accuracy=AccuracyTable, TestResults=compare_data)
  return(results)
}

