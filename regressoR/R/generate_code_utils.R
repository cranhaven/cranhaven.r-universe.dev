codigo.modelo <- function(model.name = "knn", variable.pr = NULL, datos = "datos.aprendizaje"){
  return(paste0("modelo.",model.name," <- train.",model.name,"(",variable.pr,"~., data = ",datos,")\n"))
}

codigo.prediccion <- function(model.name = "knn", alg = NULL, datos = "datos.prueba"){
  if (is.null(alg)) {
    return(paste0("prediccion.",model.name," <- predict(modelo.",model.name,", ",datos,")\n"))
  }else{
    return(paste0("prediccion.",model.name,".",alg," <- predict(modelo.",model.name,".",alg,", ",datos,")\n"))
  }
}

#Codigo de la matriz de confucion de Bayes
codigo.IG <- function(model.name = "knn", alg = NULL, datos = "datos.prueba", variable.pr = NULL){
  if (is.null(alg)) {
    return(paste0("general_indices(",datos,"[,'",variable.pr,"'], prediccion.",model.name,")\n"))
    }
  else{
    return(paste0("general_indices(",datos,"[,'",variable.pr,"'], prediccion.",model.name,".",alg,")\n"))
  }  
}  

# SVM ----------------------------------------------------------------------------------------------------------------
codeSvm <- function(variable.predecir, scale, kernel, datos = "datos.aprendizaje"){
  return(paste0("modelo.svm <- train.svm(",variable.predecir, "~.,data =  ",datos,", scale = ",scale, ", kernel = '",kernel,"')\n"))
}

# KNN ----------------------------------------------------------------------------------------------------------------
codeKnn <- function(variable.predecir, scale, k, kernel, distance, datos = "datos.aprendizaje"){
  return(paste0("modelo.knn <- train.knn(",variable.predecir,"~.,data = ",datos,", scale = ",scale, ", k = ", k,
                ", kernel = '",kernel,"', distance = ", distance, ")\n"))
}

# BOOSTING ----------------------------------------------------------------------------------------------------------------
codeBoost <- function(variable.predecir, n.trees, distribution, shrinkage, datos = "datos.aprendizaje"){
  return(paste0("modelo.boosting <- boosting_model(data = ",datos,", '",variable.predecir,"', n.trees = ",n.trees, ", distribution = '", distribution, "', shrinkage = ",shrinkage, ")\n"))
}

# RD ----------------------------------------------------------------------------------------------------------------
codeRd <- function(variable.predecir, mode, scale, datos = "datos.aprendizaje"){
  return(paste0("modelo.rd <- rd_model(data = ",datos,", '",variable.predecir,"', mode = ",mode, ", scale = ", scale, ")\n"))
}

# RL ----------------------------------------------------------------------------------------------------------------
codeRl <- function(variable.predecir, datos = "datos.aprendizaje"){
  return(paste0("modelo.rl <- lm(",variable.predecir,"~.,data = ",datos,")\n"))
}

codeRlCoef <- function(nombreModelo = "modelo.rl"){
  return(paste0("information <- rl_coeff(",nombreModelo,")\n",
                "information$df.rl[,c(1,4)]\n"))
}

# NN ----------------------------------------------------------------------------------------------------------------
codeNn <- function(variable.predecir, hidden, threshold, stepmax, datos = "datos.aprendizaje"){
  return(paste0("modelo.nn <- train.neuralnet(",variable.predecir,"~., data = ",datos,", hidden = ",as_string_c(hidden,quote = FALSE), ", threshold = ", threshold, ", stepmax = ",stepmax, ")\n"))
}

# RLR ----------------------------------------------------------------------------------------------------------------
codeRlr <- function(variable.predecir, alpha, standardize, datos = "datos.aprendizaje"){
  return(paste0("modelo.rlr <- rlr_model(",datos,", '",variable.predecir,"', alpha = ",alpha, ", standardize = ",standardize,")\n"))
}

codeRlrCoeff <- function(variable.predecir, nombreModelo, log.lambda = NULL, datos = "datos.aprendizaje"){
  param.lambda <- ifelse(is.null(log.lambda),"",paste0(", log.lambda = ",log.lambda))
  return(paste0("coef_lambda(",datos,", '", variable.predecir,"', model = ",nombreModelo,
                param.lambda, ")\n"))
}

codeRlrPred <- function(nombreModelo, variable.predecir, log.lambda = NULL, datos = "datos.prueba"){
  param.lambda <- ifelse(is.null(log.lambda),"",paste0(", log.lambda = ",log.lambda))
  return(paste0("prediccion.rlr <- rlr_prediction(model = modelo.",nombreModelo, ", ",datos,", " , "'", variable.predecir,"'",param.lambda, ")\n"))
}

# RF ----------------------------------------------------------------------------------------------------------------
codeRf <- function(variable.predecir, ntree, mtry, datos = "datos.aprendizaje"){
  return(paste0("modelo.rf <- train.randomForest(",variable.predecir,"~., data = ",datos,", ntree = ",ntree, ", mtry = ", mtry, ")\n"))
}

# DT ----------------------------------------------------------------------------------------------------------------
codeDt <- function(variable.predecir, minsplit, maxdepth, datos = "datos.aprendizaje"){
  return(paste0("modelo.dt <- train.rpart(",variable.predecir,"~.,  ",datos,", minsplit = ",minsplit, ", maxdepth = ",maxdepth,")\n"))
}

codeDtPlot <- function(nombreModelo){
  return(paste0("dt_plot(modelo.dt)\n"))
}

# Códigos de Validación Cruzada ---------------------------------------------------------------------------------------------------

# KNN ---------------------------------------------------------------------------------------------------

cv_knn_code <- function(var_pred, validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.rectangular <- list()
MCs.triangular <- list()
MCs.epanechnikov <- list()
MCs.biweight <- list()
MCs.triweight <- list()
MCs.cos <- list()
MCs.inv <- list()
MCs.gaussian <- list()
MCs.optimal <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 5 grupos
  MC.rectangular   <- vector(mode = 'list', 4)
  MC.triangular    <- vector(mode = 'list', 4)
  MC.epanechnikov  <- vector(mode = 'list', 4)
  MC.biweight   <- vector(mode = 'list', 4)
  MC.triweight  <- vector(mode = 'list', 4)
  MC.cos  <- vector(mode = 'list', 4)
  MC.inv  <- vector(mode = 'list', 4)
  MC.gaussian  <- vector(mode = 'list', 4)
  MC.optimal   <- vector(mode = 'list', 4)
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 5
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    ttraining <- datos[-muestra, ]
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'rectangular')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.rectangular <- Map(c, MC.rectangular, MC)
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'triangular')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.triangular <-  Map(c, MC.triangular, MC)
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'epanechnikov')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.epanechnikov <-  Map(c, MC.epanechnikov, MC)
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'biweight')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.biweight <-  Map(c, MC.biweight, MC)
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'triweight')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.triweight <-  Map(c, MC.triweight, MC)
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'cos')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.cos <-  Map(c, MC.cos, MC)
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'inv')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.inv <-  Map(c, MC.inv, MC)
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'gaussian')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.gaussian <-  Map(c, MC.gaussian, MC)
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'optimal')
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.optimal <-  Map(c, MC.optimal, MC)
  }
  
  MCs.rectangular[[i]] <- sapply(MC.rectangular, mean)
  MCs.triangular[[i]]  <- sapply(MC.triangular, mean)
  MCs.epanechnikov[[i]] <- sapply(MC.epanechnikov, mean)
  MCs.biweight[[i]]  <- sapply(MC.biweight, mean)
  MCs.triweight[[i]] <- sapply(MC.triweight, mean)
  MCs.cos[[i]]  <- sapply(MC.cos, mean)
  MCs.inv[[i]]  <- sapply(MC.inv, mean)
  MCs.gaussian[[i]] <- sapply(MC.gaussian, mean)
  MCs.optimal[[i]]  <- sapply(MC.optimal, mean)
}")
}

# SVM ---------------------------------------------------------------------------------------------------

cv_svm_code <- function(var_pred,   validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.radial <- list()
MCs.linear <- list()
MCs.polynomial <- list()
MCs.sigmoid <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.radial  <- vector(mode = 'list', 4)
  MC.linear  <- vector(mode = 'list', 4)
  MC.polynomial  <- vector(mode = 'list', 4)
  MC.sigmoid  <- vector(mode = 'list', 4)
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.svm(",var_pred," ~ ., data = taprendizaje, kernel = 'radial', probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.radial <-  Map(c, MC.radial, MC)
    
    modelo     <- train.svm(",var_pred," ~ ., data = taprendizaje, kernel = 'linear', probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.linear <-  Map(c, MC.linear, MC)
    
    modelo     <- train.svm(",var_pred," ~ ., data = taprendizaje, kernel = 'polynomial', probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.polynomial <-  Map(c, MC.polynomial, MC)
    
    modelo <- train.svm(",var_pred," ~ ., data = taprendizaje, kernel = 'sigmoid', probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.sigmoid <-  Map(c, MC.sigmoid, MC)
  }

  MCs.radial[[i]] <- sapply(MC.radial, mean)
  MCs.linear[[i]] <- sapply(MC.linear, mean)
  MCs.polynomial[[i]] <- sapply(MC.polynomial, mean)
  MCs.sigmoid[[i]] <- sapply(MC.sigmoid, mean)
}"
  )
}


# DT ---------------------------------------------------------------------------------------------------

cv_dt_code <- function(var_pred,   validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.gini <- list()
MCs.information <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.gini  <- vector(mode = 'list', 4)
  MC.information  <- vector(mode = 'list', 4)
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo     <- train.rpart(",var_pred," ~ ., data = taprendizaje, parms = list(split = 'gini'), control = rpart.control(minsplit = 10, maxdepth = 100))
    prediccion <- predict(modelo, ttesting)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.gini    <-  Map(c, MC.gini, MC)
    
    modelo     <- train.rpart(",var_pred," ~ ., data = taprendizaje, , parms = list(split = 'information'), control = rpart.control(minsplit = 10, maxdepth = 100))
    prediccion <- predict(modelo, ttesting)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.information <-  Map(c, MC.information, MC)
    
  }

  MCs.gini[[i]] <- sapply(MC.gini, mean)
  MCs.information[[i]] <- sapply(MC.information, mean)
}"
  )
}


# RF ---------------------------------------------------------------------------------------------------

cv_rf_code <- function(var_pred,   validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.gini <- list()
MCs.information <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos   <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.gini  <- vector(mode = 'list', 4)
  MC.information  <- vector(mode = 'list', 4)
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra      <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting     <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo     <- train.randomForest(",var_pred," ~ ., data = taprendizaje, parms = list(split = 'gini'), , mtry =floor(sqrt(ncol(datos))), ntree = 100)
    prediccion <- predict(modelo, ttesting)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.gini    <-  Map(c, MC.gini, MC)
    
    modelo         <- train.randomForest(",var_pred," ~ ., data = taprendizaje, parms = list(split = 'information'), mtry =floor(sqrt(ncol(datos))), ntree = 100)
    prediccion     <- predict(modelo, ttesting)
    MC             <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.information <-  Map(c, MC.information, MC)
    
  }

  MCs.gini[[i]] <- sapply(MC.gini, mean)
  MCs.information[[i]] <- sapply(MC.information, mean)
}"
  )
}

# BOOST ---------------------------------------------------------------------------------------------------

cv_boost_code <- function(var_pred,   validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.gaussian <- list()
MCs.laplace  <- list()
MCs.tdist    <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos       <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.gaussian  <- vector(mode = 'list', 4)
  MC.laplace   <- vector(mode = 'list', 4)
  MC.tdist     <- vector(mode = 'list', 4)
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra  <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.gbm(",var_pred," ~ ., data = taprendizaje, distribution = 'gaussian', n.trees = 100,
                                          shrinkage  = 0.1)
    prediccion  <- predict(modelo, ttesting)
    MC          <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.gaussian <-  Map(c, MC.gaussian, MC)
    
    modelo <- train.gbm(",var_pred," ~ ., data = taprendizaje, distribution = 'laplace', n.trees = 100,
                                          shrinkage  = 0.1)
    prediccion <- predict(modelo, ttesting)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.laplace <-  Map(c, MC.laplace, MC)
    
    modelo     <- train.gbm(",var_pred," ~ ., data = taprendizaje, distribution = 'tdist', n.trees = 100,
                                          shrinkage = 0.1)
    prediccion <- predict(modelo, ttesting)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.tdist   <-  Map(c, MC.tdist, MC)
    
  }
  MCs.gaussian[[i]] <- sapply(MC.gaussian, mean)
  MCs.laplace[[i]]  <- sapply(MC.laplace, mean)
  MCs.tdist[[i]]    <- sapply(MC.tdist, mean)
}"
  )
}


# RL ---------------------------------------------------------------------------------------------------

cv_rl_code <- function(var_pred,   validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.rl <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.rl  <- vector(mode = 'list', 4)
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra  <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo     <- lm(",var_pred," ~ ., data = taprendizaje)
    prediccion <- predict(modelo, ttesting)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
    MC.rl      <-  Map(c, MC.rl, MC)
    
    
  }
  MCs.rl[[i]] <- sapply(MC.rectangular, mean)
}"
  )
}

# RLR ---------------------------------------------------------------------------------------------------

cv_rlr_code <- function(var_pred,   validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.ridge <- list()
MCs.lasso <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos    <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.ridge  <- vector(mode = 'list', 4)
  MC.lasso  <- vector(mode = 'list', 4)
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra  <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo     <- rlr_model(data = taprendizaje, variable.pred = var_pred, alpha = 0, standardize = TRUE)
    prediccion <- rlr_prediction(modelo, ttesting, var_pred)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion)
    MC.ridge   <-  Map(c, MC.ridge, MC)
    
    modelo     <- rlr_model(data = taprendizaje, variable.pred = var_pred, alpha = 1, standardize = TRUE)
    prediccion <- rlr_prediction(modelo, ttesting, var_pred)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion)
    MC.lasso   <-  Map(c, MC.lasso, MC)
    
  }

  MCs.ridge[[i]] <- sapply(MC.ridge, mean)
  MCs.lasso[[i]] <- sapply(MC.lasso, mean)
}"
  )
}


# RD ---------------------------------------------------------------------------------------------------

cv_rd_code <- function(var_pred,   validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.acp <- list()
MCs.mcp <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.acp  <- vector(mode = 'list', 4)
  MC.mcp  <- vector(mode = 'list', 4)
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra  <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo     <- rd_model(data = taprendizaje, variable.pred = var_pred, mode = 0, scale = TRUE)
    prediccion <- predict(modelo, ttesting, var_pred)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion)
    MC.acp     <-  Map(c, MC.acp, MC)
    
    modelo     <- rd_model(data = taprendizaje, variable.pred = var_pred, mode = 1, scale = TRUE)
    prediccion <- predict(modelo, ttesting, var_pred)
    MC         <- general_indices(ttesting[,",var_pred,"], prediccion)
    MC.mcp     <-  Map(c, MC.mcp, MC)
    
  }

  MCs.acp[[i]] <- sapply(MC.acp, mean)
  MCs.mcp[[i]] <- sapply(MC.mcp, mean)
}"
  )
}


# Todos ---------------------------------------------------------------------------------------------------

cv_cv_code <- function(var_pred,   validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.svm <- list()
MCs.knn <- list()
MCs.arbol <- list()
MCs.bosque <- list()
MCs.potenciacion <- list()
MCs.rl  <- list()
MCs.rlr <- list()
MCs.rd <- list()

# Validación cruzada 5 veces
for (i in 1:cantidad.validacion.cruzada) {
    grupos <- createFolds(1:numero.filas, cantidad.grupos) # Crea los 10 grupos
    
    MC.svm  <- vector(mode = 'list', 4)
    MC.knn  <- vector(mode = 'list', 4)
    MC.arbol  <- vector(mode = 'list', 4)
    MC.bosque  <- vector(mode = 'list', 4)
    MC.potenciacion  <- vector(mode = 'list', 4)
    MC.rl  <- vector(mode = 'list', 4) 
    MC.rlr  <- vector(mode = 'list', 4) 
    MC.rd  <- vector(mode = 'list', 4) 
    
    # Este ciclo es el que hace validación cruzada con 10 grupos
    for (k in 1:cantidad.grupos) {
        muestra <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
        ttesting <- datos[muestra, ]
        ttraining <- datos[-muestra, ]
        
        modelo <- train.svm(",var_pred," ~ ., data = ttraining, kernel = 'linear', probability = FALSE)
        prediccion <- predict(modelo, ttesting)
        MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
        MC.svm <-  Map(c, MC.svm, MC)
        
        modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37)
        prediccion <- predict(modelo, ttesting)
        MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
        MC.knn <-  Map(c, MC.knn, MC)
       
        modelo = train.rpart(",var_pred," ~ ., data = ttraining)
        prediccion <- predict(modelo, ttesting)
        MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
        MC.arbol <-  Map(c, MC.arbol, MC)
        
        modelo <- train.randomForest(",var_pred," ~ ., data = ttraining)
        prediccion <- predict(modelo, ttesting)
        MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
        MC.bosque <-  Map(c, MC.bosque, MC)
        
        
        modelo     <- lm(",var_pred," ~ ., data = ttraining)
        prediccion <- predict(modelo, ttesting)
        MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
        MC.rl <-  Map(c, MC.rl, MC)
        
        modelo     <- rlr_model(variable.pred =",var_pred," , data = ttraining)
        prediccion <- rlr_prediction(modelo, ttesting,",var_pred,")
        MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
        MC.rlr <-  Map(c, MC.rlr, MC)
        
        modelo     <- rlr_model(variable.pred =",var_pred," , data = ttraining)
        prediccion <- predict(modelo, ttesting)
        MC <- general_indices(ttesting[,",var_pred,"], prediccion$prediction)
        MC.rd <-  Map(c, MC.rd, MC)
    }
    
    
    MCs.svm[[i]] <- sapply(MC.svm, mean)
    MCs.knn[[i]] <- sapply(MC.knn, mean)
    MCs.arbol[[i]] <- sapply(MC.arbol, mean)
    MCs.bosque[[i]] <- sapply(MC.bosque, mean)
    MCs.potenciacion[[i]] <- sapply(MC.potenciacion, mean)
    MCs.rl[[i]]  <- sapply(MC.rl, mean)
    MCs.rlr[[i]] <- sapply(MC.rlr, mean)
    MCs.rd[[i]]  <- sapply(MC.rd, mean)
}"
  )
}



