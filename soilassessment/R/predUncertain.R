predUncertain=function(indata,fgrid, k, z, model="rf"){
  total <- 20
  pb = txtProgressBar(min = 0, max = total, style = 3)
  for(j in 1:5){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, j)}
  ssw=stack()
  z1=z/100
  z2=1-z1
  (na.cols=function(x){
    y <- sapply(x, function(xx)any(is.na(xx)))
    names(y[y])
  })
  if(any(is.na(data.frame(indata)))){stop(paste("Remove NA in columns: ", paste(na.cols(indata), collapse=", ")))}
  if(class(fgrid)[1]=="RasterStack"){{
  stack1=(fgrid)
  rasvali=extract(stack1,indata)
  rasvali=cbind(indata,rasvali)
  dfram=data.frame(rasvali)[,1:(ncol(rasvali))]
  for(j in 5:10){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, j)}
if(model=="qrf") {
  nn <- ncol(dfram)
  colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
  faw=c(colnames(dfram)[2:nn])
  fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
  for(i in 1:k) {
    bound = floor((nrow(dfram)/(1.2*i))*1)
    butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
    rf1=train(fml,  data = butdat,  method = "qrf",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
    i= raster::predict(stack1, rf1)
    ssw <- stack(ssw, i)
    pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
    pred_sd = raster::calc(ssw,fun = sd)
     }
pred_width = (pred_interval[[2]] - pred_interval[[1]])
pred_out=stack(pred_width,pred_sd)
}
 else if(model=="lm"){
   nn <- ncol(dfram)
   colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
   faw=c(colnames(dfram)[2:nn])
   fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
   for(i in 1:k) {
     bound = floor((nrow(dfram)/(1.2*i))*1)
     butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
     rf1=train(fml,  data = butdat,  method = "lm",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
     i= raster::predict(stack1, rf1)
     ssw <- stack(ssw, i)
     pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
     pred_sd = raster::calc(ssw,fun = sd)
   }
   pred_width = (pred_interval[[2]] - pred_interval[[1]])
   pred_out=stack(pred_width,pred_sd)
 }
  else if(model=="rf"){
    nn <- ncol(dfram)
    colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
    faw=c(colnames(dfram)[2:nn])
    fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
    for(i in 1:k) {
      bound = floor((nrow(dfram)/(1.2*i))*1)
      butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
      rf1=train(fml,  data = butdat,  method = "rf",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
      i= raster::predict(stack1, rf1)
      ssw <- stack(ssw, i)
      pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
      pred_sd = raster::calc(ssw,fun = sd)
    }
    pred_width = (pred_interval[[2]] - pred_interval[[1]])
    pred_out=stack(pred_width,pred_sd)
  }
  else if(model=="ranger"){
    nn <- ncol(dfram)
    colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
    faw=c(colnames(dfram)[2:nn])
    fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
    for(i in 1:k) {
      bound = floor((nrow(dfram)/(1.2*i))*1)
      butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
      rf1=train(fml,  data = butdat,  method = "ranger",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
      i= raster::predict(stack1, rf1)
      ssw <- stack(ssw, i)
      pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
      pred_sd = raster::calc(ssw,fun = sd)
    }
    pred_width = (pred_interval[[2]] - pred_interval[[1]])
    pred_out=stack(pred_width,pred_sd)
  }
  else if(model=="cubist"){
    nn <- ncol(dfram)
    colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
    faw=c(colnames(dfram)[2:nn])
    fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
    for(i in 1:k) {
      bound = floor((nrow(dfram)/(1.2*i))*1)
      butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
      rf1=train(fml,  data = butdat,  method = "cubist",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
      i= raster::predict(stack1, rf1)
      ssw <- stack(ssw, i)
      pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
      pred_sd = raster::calc(ssw,fun = sd)
    }
    pred_width = (pred_interval[[2]] - pred_interval[[1]])
    pred_out=stack(pred_width,pred_sd)
  }
  else if(model=="rpart"){
    nn <- ncol(dfram)
    colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
    faw=c(colnames(dfram)[2:nn])
    fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
    for(i in 1:k) {
      bound = floor((nrow(dfram)/(1.2*i))*1)
      butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
      rf1=train(fml,  data = butdat,  method = "rpart",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
      i= raster::predict(stack1, rf1)
      ssw <- stack(ssw, i)
      pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
      pred_sd = raster::calc(ssw,fun = sd)
    }
    pred_width = (pred_interval[[2]] - pred_interval[[1]])
    pred_out=stack(pred_width,pred_sd)
  }
  else if(model=="treebag"){
    nn <- ncol(dfram)
    colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
    faw=c(colnames(dfram)[2:nn])
    fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
    for(i in 1:k) {
      bound = floor((nrow(dfram)/(1.2*i))*1)
      butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
      rf1=train(fml,  data = butdat,  method = "treebag",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
      i= raster::predict(stack1, rf1)
      ssw <- stack(ssw, i)
      pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
      pred_sd = raster::calc(ssw,fun = sd)
    }
    pred_width = (pred_interval[[2]] - pred_interval[[1]])
    pred_out=stack(pred_width,pred_sd)
  }
  else if(model=="svmLinear"){
    nn <- ncol(dfram)
    colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
    faw=c(colnames(dfram)[2:nn])
    fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
    for(i in 1:k) {
      bound = floor((nrow(dfram)/(1.2*i))*1)
      butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
      rf1=train(fml,  data = butdat,  method = "svmLinear",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
      i= raster::predict(stack1, rf1)
      ssw <- stack(ssw, i)
      pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
      pred_sd = raster::calc(ssw,fun = sd)
    }
    pred_width = (pred_interval[[2]] - pred_interval[[1]])
    pred_out=stack(pred_width,pred_sd)
  }
  else if(model=="bayesglm"){
    nn <- ncol(dfram)
    colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
    faw=c(colnames(dfram)[2:nn])
    fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
    for(i in 1:k) {
      bound = floor((nrow(dfram)/(1.2*i))*1)
      butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
      rf1=train(fml,  data = butdat,  method = "bayesglm",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
      i= raster::predict(stack1, rf1)
      ssw <- stack(ssw, i)
      pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
      pred_sd = raster::calc(ssw,fun = sd)
    }
    pred_width = (pred_interval[[2]] - pred_interval[[1]])
    pred_out=stack(pred_width,pred_sd)
  }
  else if(model=="qrnn"){
    nn <- ncol(dfram)
    colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
    faw=c(colnames(dfram)[2:nn])
    fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
    for(i in 1:k) {
      bound = floor((nrow(dfram)/(1.2*i))*1)
      butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
      rf1=train(fml,  data = butdat,  method = "qrnn",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
      i= raster::predict(stack1, rf1)
      ssw <- stack(ssw, i)
      pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
      pred_sd = raster::calc(ssw,fun = sd)
    }
    for(j in 10:15){
      Sys.sleep(0.5)
      setTxtProgressBar(pb, j)}

    pred_width = (pred_interval[[2]] - pred_interval[[1]])
    pred_out=stack(pred_width,pred_sd)
  }
  for(j in 15:total){
    Sys.sleep(0.5)
    setTxtProgressBar(pb, j)}}
  }
  else {
    {
      stack1=raster::stack(fgrid)
      rasvali=extract(stack1,indata)
      rasvali=cbind(indata,rasvali)
      dfram=data.frame(rasvali)[,1:(ncol(rasvali))]
      for(j in 5:10){
        Sys.sleep(0.5)
        setTxtProgressBar(pb, j)}
      if(model=="qrf") {
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "qrf",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="lm"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "lm",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="rf"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "rf",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="ranger"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "ranger",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="cubist"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "cubist",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="rpart"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "rpart",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="treebag"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "treebag",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="svmLinear"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "svmLinear",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="bayesglm"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "bayesglm",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      else if(model=="qrnn"){
        nn <- ncol(dfram)
        colnames(dfram)<-sub(".*\\$", "",colnames(dfram))
        faw=c(colnames(dfram)[2:nn])
        fml=as.formula(paste(colnames(dfram[1]),paste(faw,collapse = "+"),sep = "~"))
        for(i in 1:k) {
          bound = floor((nrow(dfram)/(1.2*i))*1)
          butdat <- dfram[sample(nrow(dfram)), ][1:bound, ]
          rf1=train(fml,  data = butdat,  method = "qrnn",trControl=trainControl( method = "cv",number=k,returnResamp = "all",savePredictions = TRUE, search = "random",verboseIter = FALSE))
          i= raster::predict(stack1, rf1)
          ssw <- stack(ssw, i)
          pred_interval <- raster::calc(ssw, function(x) {quantile(x,probs = c(z2, z1),na.rm = TRUE)})
          pred_sd = raster::calc(ssw,fun = sd)
        }
        for(j in 10:15){
          Sys.sleep(0.5)
          setTxtProgressBar(pb, j)}

        pred_width = (pred_interval[[2]] - pred_interval[[1]])
        pred_out=stack(pred_width,pred_sd)
      }
      for(j in 15:total){
        Sys.sleep(0.5)
        setTxtProgressBar(pb, j)}}
  }

  names(pred_out)=c("pred_width","pred_sd")
  return(pred_out)
}
