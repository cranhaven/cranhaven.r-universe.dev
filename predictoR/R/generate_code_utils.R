# Módulo para generar códigos globales

#Genera el código para un modelo indicando el nombre utilizado por traineR
codigo.modelo <- function(model.name = "knn", variable.pr = NULL, datos = "datos.aprendizaje"){
  return(paste0("modelo.",model.name," <<- train.",model.name,"(",variable.pr,"~., data = ",datos,")\n"))
}

#Genera el código para la predicción indicando el nombre del modelo generado, verifica si se trata de un modelo con diferentes núcleos
codigo.prediccion <- function(model.name = "knn", alg = NULL, datos = "datos.prueba"){
  if (is.null(alg)) {
    return(paste0("prediccion.",model.name," <<- predict(modelo.",model.name,", ",datos,", type = 'class')\n"))
  }else{
    return(paste0("prediccion.",model.name,".",alg," <<- predict(modelo.",model.name,".",alg,", ",datos,", type='class')\n"))
  }
}

#Codigo de la matriz de confusión 
codigo.MC <- function(model.name = "knn", alg = NULL){
  if (is.null(alg)) {
    return(paste0("MC.",model.name," <<- confusion.matrix(datos.prueba, prediccion.",model.name,")","\n"))
  }else{
    return(paste0("MC.",model.name,".",alg," <<- confusion.matrix(datos.prueba, prediccion.",model.name,".",alg,")","\n"))
  }
}

# Códigos de BOOSTING --------------------------------------------------------------------------------------------------------

#Crea el modelo BOOSTING
boosting.modelo <- function(variable.pr = NULL, iter = 50, maxdepth = 1, minsplit = 1, coeflearn = "Breiman", datos = "datos.aprendizaje"){
  iter     <- ifelse(!is.numeric(iter), 50, iter)
  maxdepth <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo   <- paste0(
    "modelo.boosting <<- train.adabag(", variable.pr, "~., data = ", datos, ", mfinal = ", iter, 
    ", coeflearn = '", coeflearn, "', control = rpart.control(minsplit = ", minsplit, ", maxdepth = ", maxdepth, "))\n")
  return(codigo)
}

#Código del grafico de boosting
boosting.plot <- function(){
  return(paste0("error(modelo.boosting, datos.aprendizaje) -> evol.train\n",
                "e_evol_error(evol.train)\n"))
}

#Reglas de boosting
rules.boosting <- function(i){
  return(paste0("rules(modelo.boosting$trees[[",i,"]])\n"))
}

#Código del grafico de importancia de variables
boosting.plot.import <- function() {
  return(paste0(
    "aux <- data.frame(importancia = modelo.boosting$importance)\n",
    "aux$nombre <- row.names(aux)\n",
    "aux$importancia <- abs(aux$importancia)\n",
    "aux <- aux[order(aux$importancia, decreasing = T), ]\n\n",
    "aux |>  e_charts(nombre) |>  e_bar(importancia, name = var) |>  \n",
    "  e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() |>  \n",
    "  e_flip_coords() |> \n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}


# Códigos de DT --------------------------------------------------------------------------------------------------------

#Crea el modelo DT
dt.modelo  <- function(variable.pr = NULL, minsplit =  20, maxdepth = 15, split = "gini", datos = "datos.aprendizaje"){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit )
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo   <- paste0("modelo.dt.",split," <<- train.rpart(",variable.pr,"~., data = ",datos,",
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"),parms = list(split = '",split,"'))\n")
  return(codigo)
}

#Código del gráfico de dt
dt.plot <- function(tipo, num = 1){
  #∫num <- length(levels(datos[,var.pred]))
  return(paste0("prp(modelo.dt.",tipo,", type = 2, extra = 104, nn = T, varlen = 0, faclen = 0,
                fallen.leaves = TRUE, branch.lty = 6, shadow.col = 'gray82',
                box.col = gg_color_hue(",num,")[modelo.dt.",tipo,"$frame$yval])\n"))
}

# Códigos de KNN --------------------------------------------------------------------------------------------------------
#' @import traineR 

#Crea el modelo KNN
code.kkn.modelo <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal", datos = "datos.aprendizaje"){
  return(paste0("modelo.knn.",kernel," <<- traineR::train.knn(",variable.pr,"~., data = ",datos,", scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')\n"))
}

# Códigos de  RL --------------------------------------------------------------------------------------------------------------

#Crea el modelo RL
rl.modelo <- function(variable.predecir = NULL, datos = "datos.aprendizaje"){
  return(paste0("modelo.glm <<- train.glm(",variable.predecir,"~., data = ",datos,", family = binomial)\n"))
}

# Códigos de NN ---------------------------------------------------------------------------------------------------------

#Crea el modelo NN
nn.modelo   <- function(variable.pr = NULL, threshold = 0.01, stepmax = 1000, cant.cap = 2, datos = "datos.aprendizaje", ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax   <- ifelse(stepmax < 100, 100, stepmax)
  capas     <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)
  
  return(paste0("modelo.neuralnet <<- train.neuralnet(",variable.pr,"~., data = ",datos,", hidden = ",capas,",\n\t\t\tlinear.output = FALSE,",
                "threshold = ",threshold,", stepmax = ",stepmax,")\n"))
}

#Gráfico de la red neuronal
nn.plot <- function(){
  paste0("plot(modelo.neuralnet,,arrow.length = 0.1, rep = 'best', intercept = T,x.entry = 0.1, x.out = 0.9,\n\t",
         "information=F,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
         "dimension=15, radius = 0.2, fontsize = 10)\n")
}

# Códigos de RLR -------------------------------------------------------------------------------------------------------------

#Crea el modelo RLR
rlr.modelo <- function(variable.pr = NULL, type = "ridge", alpha = 0, escalar = TRUE, datos = "datos.aprendizaje"){
  return(paste0("modelo.glmnet.",type,"<<- train.glmnet(",variable.pr,"~., data = ",datos,", standardize = ",escalar,", alpha = ",alpha,", family = 'multinomial')\n"))
}

# Códigos de RF--------------------------------------------------------------------------------------------------

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, ntree = 500, mtry = 1, datos = "datos.aprendizaje"){
  ntree   <- ifelse(!is.numeric(ntree), 500, ntree)
  Codigo  <- paste0("modelo.rf <<- train.randomForest(",variable.pr,"~., data = ",datos,",importance = TRUE,",
                    " ntree =",ntree,",mtry =",mtry,")\n")
  return(Codigo)
}

#Código del gráfico de importancia de variables
rf.importance.plot <- function() {
  return(paste0(
    "aux <- data.frame(modelo.rf$importance)\n",
    "aux$MeanDecreaseAccuracy <- abs(aux$MeanDecreaseAccuracy)\n",
    "aux <- aux[order(aux$MeanDecreaseAccuracy, decreasing = T), ]\n",
    "aux$label <- row.names(aux)\n\n",
    "aux |>  e_charts(label) |>  e_bar(MeanDecreaseAccuracy, name = var) |>  \n",
    "  e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() |>  \n",
    "  e_flip_coords() |> \n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}

#' Código del gráfico de error del modelo
#' @noRd
plot.rf.error <- function(){
  return(paste0("e_rf_error(modelo.rf)\n"))
}

# Códigos de SVM -------------------------------------------------------------------------------------------------------------

#Crea el modelo SVM
svm.modelo <- function(variable.pr = NULL, scale = TRUE, kernel = "linear", datos = "datos.aprendizaje"){
  return(paste0("modelo.svm.",kernel," <- traineR::train.svm(",variable.pr,"~., data = ",datos,", scale =",scale,", kernel = '",kernel,"')\n"))
}

#Código del gráfico de svm
svm.plot <- function(var.pred,train,  variables, resto, kernel = "linear"){
  if(is.null(variables)){
    return("NULL")
  }
  
  l <- c()
  for(i in 1:length(resto)){
    l <- c(l , paste0(resto[i]," = ", i))
  }
  l <- paste0("list(",paste0(l,collapse = ","),")")
  s <- paste0("modelo.svm.temp <<- traineR::train.svm(",var.pred,"~",variables[1],"+",variables[2],", data = datos.aprendizaje, kernel = '",kernel,"') \n")
  color <- length(unique(train[,var.pred]))
  color <- as.string.c(gg_color_hue(color))
  return(paste0(s,"plot(modelo.svm.temp, datos, ",variables[1],"~",variables[2],", slice = ",l,", col = ",color,")\n"))
}
# Códigos de XGBOOSTING ---------------------------------------------------------------------------------------------------

#Crea el modelo
xgb.modelo <- function(variable.pr = NULL, booster = "gbtree",max.depth = 6, n.rounds = 60, datos = "datos.aprendizaje"){
  return(paste0("modelo.xgb.",booster," <<- traineR::train.xgboost(",variable.pr,"~., data = ",datos,", booster ='",booster,"', max_depth=",max.depth,", nrounds = ",n.rounds,")\n"))
}


#Código del grafico de importancia de variables
e_xgb_varImp <- function(booster = "gbtree"){
  paste0("nombres                   <- modelo.xgb.",booster,"$feature_names\n",   
         "variables.importantes     <- xgboost::xgb.importance(feature_names = nombres, model = modelo.xgb.",booster,")\n",
         "variables.importantes     <- variables.importantes[1:length(nombres),]","\n",
         "variables.importantes[,2] <- abs(variables.importantes[,2])","\n",
         "variables.importantes     <- na.omit(variables.importantes)\n",
         "datos.xgb <- data.frame(label = variables.importantes$Feature, values = variables.importantes[,2])\n",
         "datos.xgb |>  e_charts(label) |>  e_bar(values, name = var) |> \n",
         "e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()|> \n",
         "e_flip_coords()|> \n",
         "e_y_axis(inverse = TRUE)\n"
         
  )
}

# Códigos de Validación Cruzada ---------------------------------------------------------------------------------------------------

# KNN ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con KNN 
cv_knn_code <- function(var_pred, dim_v, validaciones, grupo){
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
  MC.rectangular <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.triangular  <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.epanechnikov  <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.biweight  <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.triweight <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.cos <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.inv <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.gaussian <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.optimal  <-  matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 5
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    ttraining <- datos[-muestra, ]
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'rectangular')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.rectangular <- MC.rectangular + MC
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'triangular')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.triangular <- MC.triangular + MC
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'epanechnikov')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.epanechnikov <- MC.epanechnikov + MC
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'biweight')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.biweight <- MC.biweight + MC
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'triweight')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.triweight <- MC.triweight + MC
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'cos')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.cos <- MC.cos + MC
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'inv')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.inv <- MC.inv + MC
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'gaussian')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.gaussian <- MC.gaussian + MC
    
    modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37, kernel = 'optimal')
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.optimal <- MC.optimal + MC
  }
  
  MCs.rectangular[[i]] <- MC.rectangular
  MCs.triangular[[i]]  <- MC.triangular
  MCs.epanechnikov[[i]] <- MC.epanechnikov
  MCs.biweight[[i]]  <- MC.biweight
  MCs.triweight[[i]] <- MC.triweight
  MCs.cos[[i]]  <- MC.cos
  MCs.inv[[i]]  <- MC.inv
  MCs.gaussian[[i]] <- MC.gaussian
  MCs.optimal[[i]]  <- MC.optimal
}")
}

# SVM ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con SVM 
cv_svm_code <- function(var_pred, dim_v, validaciones, grupo){
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
  MC.radial <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.linear <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.polynomial <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.sigmoid <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.svm(",var_pred," ~ ., data = taprendizaje, kernel = 'radial', probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.radial <- MC.radial + MC
    
    modelo <- train.svm(",var_pred," ~ ., data = taprendizaje, kernel = 'linear', probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.linear <- MC.linear + MC
    
    modelo <- train.svm(",var_pred," ~ ., data = taprendizaje, kernel = 'polynomial', probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.polynomial <- MC.polynomial + MC
    
    modelo <- train.svm(",var_pred," ~ ., data = taprendizaje, kernel = 'sigmoid', probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.sigmoid <- MC.sigmoid + MC
  }

  MCs.radial[[i]] <- MC.radial
  MCs.linear[[i]] <- MC.linear
  MCs.polynomial[[i]] <- MC.polynomial
  MCs.sigmoid[[i]] <- MC.sigmoid
}"
  )
}


# DT ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con DT 
cv_dt_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
"numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.gini <- list()
MCs.information <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.gini <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.information <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.rpart(",var_pred," ~ ., data = taprendizaje, parms = list(split = 'gini'), control = rpart.control(minsplit = 10, maxdepth = 100))
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.gini <- MC.gini + MC
    
    modelo <- train.rpart(",var_pred," ~ ., data = taprendizaje, , parms = list(split = 'information'), control = rpart.control(minsplit = 10, maxdepth = 100))
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.information <- MC.information + MC
    
  }

  MCs.gini[[i]] <- MC.gini
  MCs.information[[i]] <- MC.information
}"
  )
}


# RF ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con RF 
cv_rf_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
"numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.gini <- list()
MCs.information <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.gini <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.information <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.randomForest(",var_pred," ~ ., data = taprendizaje, parms = list(split = 'gini'), , mtry =floor(sqrt(ncol(datos))), ntree = 100)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.gini <- MC.gini + MC
    
    modelo <- train.randomForest(",var_pred," ~ ., data = taprendizaje, parms = list(split = 'information'), mtry =floor(sqrt(ncol(datos))), ntree = 100)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.information <- MC.information + MC
    
  }

  MCs.gini[[i]] <- MC.gini
  MCs.information[[i]] <- MC.information
}"
  )
}

# XGB ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con XGB 
cv_xgb_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
"numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.dart <- list()
MCs.gblinear <- list()
MCs.gbtree <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.dart <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.gblinear <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.gbtree <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.xgboost(",var_pred," ~ ., data = taprendizaje, booster = 'dart',
                                         max_depth = 100, nrounds = 50)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.dart <- MC.dart + MC
    
    modelo <- train.xgboost(",var_pred," ~ ., data = taprendizaje, booster = 'gblinear',
                                         max_depth = 100, nrounds = 50)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.gblinear <- MC.gblinear + MC
    
    modelo <- train.xgboost(",var_pred," ~ ., data = taprendizaje, booster = 'gbtree',
                                         max_depth = 100, nrounds = 50)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.gbtree <- MC.gbtree + MC
    
  }
  MCs.dart[[i]] <- MC.dart
  MCs.gblinear[[i]] <- MC.gblinear
  MCs.gbtree[[i]] <- MC.gbtree
}"
  )
}


# BAYES ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con BAYES 
cv_bayes_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.bayes <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.bayes <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.bayes(",var_pred," ~ ., data = taprendizaje)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.bayes <- MC.bayes + MC
    
    
  }
  MCs.bayes[[i]] <- MC.bayes
}"
  )
}

# BOOST ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con BOOSTING 
cv_boost_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.breiman <- list()
MCs.freund <- list()
MCs.zhu <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.breiman <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.freund <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.zhu <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.adabag(",var_pred," ~ ., data = taprendizaje, coeflearn = 'Freiman', mfinal = 100,
                                          control = rpart.control(maxdepth = 30))
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.breiman <- MC.breiman + MC
    
    modelo <- train.adabag(",var_pred," ~ ., data = taprendizaje, coeflearn = 'Freund', mfinal = 100,
                                          control = rpart.control(maxdepth = 30))
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.freund <- MC.freund + MC
    
    modelo <- train.adabag(",var_pred," ~ ., data = taprendizaje, coeflearn = 'Zhu', mfinal = 100,
                                          control = rpart.control(maxdepth = 30))
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.zhu <- MC.zhu + MC
    
  }
  MCs.breiman[[i]] <- MC.breiman
  MCs.freund[[i]] <- MC.freund
  MCs.zhu[[i]] <- MC.zhu
}"
  )
}


# RL ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con RL 
cv_rl_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.rl <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.rl <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.glm(",var_pred," ~ ., data = taprendizaje)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.rl <- MC.rl + MC
    
    
  }
  MCs.rl[[i]] <- MC.rl
}"
  )
}


# RLR ---------------------------------------------------------------------------------------------------


# Código para generar validación cruzada con RLR 
cv_rlr_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.ridge <- list()
MCs.lasso <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.ridge <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  MC.lasso <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.glmnet(",var_pred," ~ ., data = taprendizaje, standardize = TRUE, alpha = 0, family = 'multinomial' )
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.ridge <- MC.ridge + MC
    
    modelo <- train.glmnet(",var_pred," ~ ., data = taprendizaje, standardize = TRUE, alpha = 1, family = 'multinomial' )
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.lasso <- MC.lasso + MC
    
  }

  MCs.ridge[[i]] <- MC.ridge
  MCs.lasso[[i]] <- MC.lasso
}"
  )
}


# LDA ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con LDA 
cv_lda_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.lda <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.lda <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.lda(",var_pred," ~ ., data = taprendizaje)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.lda <- MC.lda + MC
    
    
  }
  MCs.lda[[i]] <- MC.lda
}"
  )
}

# LDA ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con LDA 
cv_qda_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.qda <- list()

for(i in 1:cantidad.validacion.cruzada){
  grupos  <- createFolds(1:numero.filas, cantidad.grupos)  # Crea los 10 grupos
  MC.qda <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:cantidad.grupos) {
    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    
    modelo <- train.qda(",var_pred," ~ ., data = taprendizaje)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    MC.qda <- MC.qda + MC
    
    
  }
  MCs.qda[[i]] <- MC.qda
}"
  )
}



# Todos ---------------------------------------------------------------------------------------------------

# Código para generar validación cruzada con todos los modelos 
cv_cv_code <- function(var_pred, dim_v, validaciones, grupo){
  paste0(
    "numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- ",validaciones,"
cantidad.grupos <- ",grupo,"

MCs.svm <- list()
MCs.knn <- list()
MCs.bayes <- list()
MCs.arbol <- list()
MCs.bosque <- list()
MCs.potenciacion <- list()
MCs.red <- list()
MCs.xgboost <- list()
MCs.red.neu <- list()
MCs.glm <- list()

# Validación cruzada 5 veces
for (i in 1:cantidad.validacion.cruzada) {
    grupos <- createFolds(1:numero.filas, cantidad.grupos) # Crea los 10 grupos
    
    MC.svm <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.knn <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.bayes <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.arbol <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.bosque <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.potenciacion <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.red <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.xgboost  <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.red.neu <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,")
    MC.glm <- matrix(rep(0, ",dim_v," * ",dim_v,"), nrow = ",dim_v,") 
    
    # Este ciclo es el que hace validación cruzada con 10 grupos
    for (k in 1:cantidad.grupos) {
        muestra <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
        ttesting <- datos[muestra, ]
        ttraining <- datos[-muestra, ]
        
        modelo <- train.svm(",var_pred," ~ ., data = ttraining, kernel = 'linear', probability = FALSE)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.svm <- MC.svm + MC
        
        modelo <- train.knn(",var_pred," ~ ., data = ttraining, kmax = 37)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.knn <- MC.knn + MC
        
        modelo <- train.bayes(",var_pred," ~ ., data = ttraining)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.bayes <- MC.bayes + MC
        
        modelo = train.rpart(",var_pred," ~ ., data = ttraining)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.arbol <- MC.arbol + MC
        
        modelo <- train.randomForest(",var_pred," ~ ., data = ttraining)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.bosque <- MC.bosque + MC
        
        modelo <- train.ada(",var_pred," ~ ., data = ttraining, iter = 20, nu = 1, type = 'discrete')
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.potenciacion <- MC.potenciacion + MC
        
        modelo <- train.nnet(",var_pred," ~ ., data = ttraining, size = 100, MaxNWts = 5000, rang = 0.01, 
                             decay = 5e-4, maxit = 45, trace = TRUE)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.red <- MC.red + MC
        
        modelo <- train.xgboost(",var_pred," ~ ., data = ttraining, nrounds = 79,
                                print_every_n = 10, maximize = F , eval_metric = 'error',verbose = 0)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.xgboost <- MC.xgboost + MC
        
        modelo <- train.neuralnet(",var_pred," ~., data = ttraining, hidden = c(8,6,4), 
                                  linear.output = FALSE, threshold = 0.5, stepmax = 1e+06)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.red.neu <- MC.red.neu + MC
        
        modelo <- train.glm(",var_pred," ~ ., data = ttraining)
        prediccion <- predict(modelo, ttesting)
        MC <- confusion.matrix(ttesting, prediccion)
        MC.glm <- MC.glm + MC
    }
    
    
    MCs.svm[[i]] <- MC.svm
    MCs.knn[[i]] <- MC.knn
    MCs.bayes[[i]] <- MC.bayes
    MCs.arbol[[i]] <- MC.arbol
    MCs.bosque[[i]] <- MC.bosque
    MCs.potenciacion[[i]] <- MC.potenciacion
    MCs.red[[i]] <- MC.red
    MCs.xgboost[[i]] <- MC.xgboost
    MCs.red.neu[[i]] <- MC.red.neu
    MCs.glm[[i]] <- MC.glm  
}"
  )
}

