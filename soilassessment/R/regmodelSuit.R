regmodelSuit=function(df, ...){
  total <- 20
  pb = txtProgressBar(min = 0, max = total, style = 3)

  dv=as.character(sapply(substitute(list(...))[-1], deparse))
  fml <- as.formula(paste(dv[1],paste((dv[2:length(dv)]),collapse="+"),sep="~"))
  names(fml)<-sub(".*\\$", "",names(fml))
  dat=data.frame(df)
  (na.cols=function(x){
    y <- sapply(x, function(xx)any(is.na(xx)))
    names(y[y])
  })
  if(any(is.na(dat))){stop(paste("Remove NA in columns: ", paste(na.cols(dat), collapse=", ")))}
  for(i in 1:5){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, i)}
  LM= train(fml,  data = dat,  method = "lm", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  RF= train(fml,  data = dat,  method = "rf", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  SVM= train(fml,  data = dat,  method = "svmLinear", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  BGLM= train(fml,  data = dat,  method = "bayesglm", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  CARTB= train(fml,  data = dat,  method = "treebag", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  CUB= train(fml,  data = dat,  method = "cubist",trControl=trainControl( method = "cv",number=5,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
  CART= train(fml,  data = dat,  method = "rpart", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  RANGE= train(fml,  data = dat,  method = "ranger", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  QRF= train(fml,  data = dat,  method = "qrf", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  QRNN= train(fml,  data = dat,  method = "qrnn", trControl=trainControl( method = "cv",number=5,savePredictions = TRUE, verboseIter = FALSE))
  for(i in 5:10){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, i)}

  { LM_MAE=mean(LM$results$MAE)
    RF_MAE=mean(RF$results$MAE)
    SVM_MAE=mean(SVM$results$MAE)
    BGLM_MAE=mean(BGLM$results$MAE)
    CARTB_MAE=mean(CARTB$results$MAE)
    CUB_MAE=mean(CUB$results$MAE)
    CART_MAE=mean(CART$results$MAE)
    RAN_MAE=mean(RANGE$results$MAE)
    QRF_MAE=mean(QRF$results$MAE)
    QRNN_MAE=mean(QRNN$results$MAE)

    LM_RMSE=mean(LM$results$RMSE)
    RF_RMSE=mean(RF$results$RMSE)
    SVM_RMSE=mean(SVM$results$RMSE)
    BGLM_RMSE=mean(BGLM$results$RMSE)
    CARTB_RMSE=mean(CARTB$results$RMSE)
    CUB_RMSE=mean(CUB$results$RMSE)
    CART_RMSE=mean(CART$results$RMSE)
    RAN_RMSE=mean(RANGE$results$RMSE)
    QRF_RMSE=mean(QRF$results$RMSE)
    QRNN_RMSE=mean(QRNN$results$RMSE)

    LM_Rsquared=mean(LM$results$Rsquared)
    RF_Rsquared=mean(RF$results$Rsquared)
    SVM_Rsquared=mean(SVM$results$Rsquared)
    BGLM_Rsquared=mean(BGLM$results$Rsquared)
    CARTB_Rsquared=mean(CARTB$results$Rsquared)
    CUB_Rsquared=mean(CUB$results$Rsquared)
    CART_Rsquared=mean(CART$results$Rsquared)
    RAN_Rsquared=mean(RANGE$results$Rsquared)
    QRF_Rsquared=mean(QRF$results$Rsquared)
    QRNN_Rsquared=mean(QRNN$results$Rsquared)}
  for(i in 10:12){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, i)}
  drt=data.frame(dat[1],fitted(LM),fitted(RF),predict(SVM),fitted(BGLM),fitted(CARTB),fitted(CUB),fitted(CART),fitted(RANGE),fitted(QRF),fitted(QRNN))#
  colnames(drt)=c("measured","LM","RF","SVM","BGLM","CARTB","CUB","CART","RANGE","QRF","QRNN")
  SSE_LM=1-sum((drt$LM-drt$measured)^2,na.rm=TRUE)/sum((drt$LM-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_RF=1-sum((drt$RF-drt$measured)^2,na.rm=TRUE)/sum((drt$RF-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_SVM=1-sum((drt$SVM-drt$measured)^2,na.rm=TRUE)/sum((drt$SVM-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_BGLM=1-sum((drt$BGLM-drt$measured)^2,na.rm=TRUE)/sum((drt$BGLM-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_CARTB=1-sum((drt$CARTB-drt$measured)^2,na.rm=TRUE)/sum((drt$CARTB-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_CUB=1-sum((drt$CUB-drt$measured)^2,na.rm=TRUE)/sum((drt$CUB-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_CART=1-sum((drt$CART-drt$measured)^2,na.rm=TRUE)/sum((drt$CART-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_RAN=1-sum((drt$RANGE-drt$measured)^2,na.rm=TRUE)/sum((drt$RANGE-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_QRF=1-sum((drt$QRF-drt$measured)^2,na.rm=TRUE)/sum((drt$QRF-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)
  SSE_QRNN=1-sum((drt$QRNN-drt$measured)^2,na.rm=TRUE)/sum((drt$QRNN-mean(drt$measured,na.rm=TRUE))^2,na.rm=TRUE)

  for(i in 12:15){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, i)}

  twy=matrix(c(LM_MAE,RF_MAE,SVM_MAE,BGLM_MAE,CARTB_MAE,CUB_MAE,CART_MAE,RAN_MAE,QRF_MAE,QRNN_MAE,
               LM_RMSE,RF_RMSE,SVM_RMSE,BGLM_RMSE,CARTB_RMSE,CUB_RMSE,CART_RMSE,RAN_RMSE,QRF_RMSE,QRNN_RMSE,
               LM_Rsquared,RF_Rsquared,SVM_Rsquared,BGLM_Rsquared,CARTB_Rsquared,CUB_Rsquared,CART_Rsquared,RAN_Rsquared,QRF_Rsquared,QRNN_Rsquared,
               SSE_LM,SSE_RF,SSE_SVM,SSE_BGLM,SSE_CARTB,SSE_CUB,SSE_CART,SSE_RAN,SSE_QRF,SSE_QRNN),nrow=10,ncol = 4,byrow = FALSE)
  colnames(twy)=c("ME","RMSE","R2","NSE")
  rownames(twy)=c("Linear","RandomForest","SVM","BayesianGLM","BaggedCART","Cubist","CART","Ranger","QuantRandForest","QuantNeuralNT")

  for(i in 15:total){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, i)}
  return(twy)

}
