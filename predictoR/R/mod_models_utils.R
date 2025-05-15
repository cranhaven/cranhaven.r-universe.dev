# Opciones KNN
opciones.knn <- function(ns, vc = F) {
  opciones <- tags$div(
    numericInput(ns("kmax"), labelInput("kmax"), min = 1, 
                 step = 1, value = 1),
    selectInput(
      inputId = ns("kernel"), label = labelInput("selkernel"), selected = "optimal",
      choices = c("optimal", "rectangular", "triangular", 
                  "epanechnikov", "biweight", "triweight",
                  "cos", "inv", "gaussian"), multiple = vc),
    radioSwitch(ns("scale"), "escal", c("si", "no"))
  )
  return(opciones)
}

# Opciones SVM
opciones.svm <- function(ns, vc = F) {
  opciones <- tags$div(
    radioSwitch(ns("scale"), "escal", c("si", "no")),
    selectInput(ns("kernel"), label = labelInput("selkernel"), 
                choices =  c("radial", "polynomial", "linear", "sigmoid"), 
                selected = "radial", multiple = vc)
  )
  return(opciones)
}

# Opciones Arboles
opciones.tree <- function(ns, vc = F) {
  opciones <- tags$div(
    numericInput(ns("minsplit"), labelInput("minsplit"), 2, 
                 width = "100%", min = 1),
    numericInput(ns("maxdepth"), labelInput("maxdepth"), 15,
                 width = "100%",min = 0, max = 30, step = 1),
    selectInput(ns("split"), label = labelInput("splitIndex"), selected = "gini",
                choices =  list("gini" = "gini", "Entropia" = "information"),
                multiple = vc)
  )
  return(opciones)
}

# Opciones Bosques
opciones.rndf <- function(ns) {
  opciones <- tags$div(
    numericInput(ns("ntree"), labelInput("numTree"), 5, width = "100%", min = 0),
    numericInput(ns("mtry"), labelInput("numVars"), 1, width = "100%", min = 1)
  )
  return(opciones)
}

# Opciones Potenciacion
opciones.boost <- function(ns, vc = F) {
  opciones <- tags$div(
    numericInput(ns("mfinal"), labelInput("numTree"), 
                 5, width = "100%", min = 1),
    numericInput(ns("maxdepth"), labelInput("maxdepth"), 
                 15, width = "100%", min = 1),
    numericInput(ns("minsplit"), labelInput("minsplit"), 
                 20, width = "100%", min = 1),
    selectInput(ns("coeflearn"), label = labelInput("selkernel"),
                selected = "Breiman", choices = c("Breiman", "Freund", "Zhu"),
                multiple = vc)
  )
  return(opciones)
}

# Opciones XGB
opciones.xgb <- function(ns, vc = F) {
  opciones <- tags$div(
    numericInput(ns("nrounds"), labelInput("selnrounds"),
                 min = 0, step = 1, value = 5),
    numericInput(ns("max_depth"), labelInput("maxdepth"),
                 min = 1, step = 1, value = 6),
    selectInput(ns("booster"), label = labelInput("selbooster"),
                selected = "gbtree", choices = c("gbtree", "gblinear", "dart"),
                multiple = vc)
  )
  return(opciones)
}

# Opciones Redes Neuronales
opciones.nnet <- function(ns) {
  opciones <- tags$div(
    numericInput(ns("threshold"), labelInput("threshold"),
                 min = 0, step = 0.01, value = 0.05),
    numericInput(ns("stepmax"), labelInput("stepmax"),
                 min = 100, step = 100, value = 5000),
    sliderInput(ns("n_hidden"), min = 1, max = 10,
                label = labelInput("selectCapas"), value = 2),
    fluidRow(
      id = ns("capasFila"), 
      lapply(1:10, function(i) {
        tags$span(
          col_4(
            numericInput(ns(paste0("hidden", i)), NULL,
                         min = 1, step = 1, value = 2), 
            class = "mini-numeric-select"
          )
        )
      })
    )
  )
  return(opciones)
}

# Opciones Regresion Penalizada
opciones.regp <- function(ns, vc = F) {
  opciones <- tags$div(
    selectInput(ns("alpha"), label = labelInput("selectAlg"),
                selected = 0, choices = list("Ridge" = 0, "Lasso" = 1), 
                multiple = vc),
    radioSwitch(ns("scales"), "escal", c("si", "no"))
  )
  return(opciones)
}

# Generar opciones
generar.opciones <- function(nombre, ns, vc = F) {
  opciones <- NULL
  if(nombre == "knn") {
    opciones <- opciones.knn(ns, vc)
  } else if (nombre == "svm") {
    opciones <- opciones.svm(ns, vc)
  } else if (nombre == "tree") {
    opciones <- opciones.tree(ns, vc)
  } else if (nombre == "rndf") {
    opciones <- opciones.rndf(ns)
  } else if (nombre == "boost") {
    opciones <- opciones.boost(ns, vc)
  } else if (nombre == "xgb") {
    opciones <- opciones.xgb(ns, vc)
  } else if (nombre == "bayes") {
    opciones <- NULL
  } else if (nombre == "nnet") {
    opciones <- opciones.nnet(ns)
  } else if (nombre == "reg") {
    opciones <- NULL
  } else if (nombre == "regp") {
    opciones <- opciones.regp(ns, vc)
  } else if (nombre == "lda") {
    opciones <- NULL
  } else if (nombre == "qda") {
    opciones <- NULL
  }
  
  return(opciones)
}

# Generar modelo
generar.modelo <- function(nombre, formula, datos, input, cat) {
  modelo    <- NULL
  algoritmo <- NULL
  tryCatch({
    if(nombre == "knn") {
      scale  <- isolate(input$scale)
      kernel <- isolate(input$kernel)
      kmax   <- isolate(input$kmax)
      modelo <- train.knn(formula, data = datos, scale = scale,
                          kernel = kernel, kmax = kmax)
      algoritmo <- paste0(nombre, "-", kernel)
    } else if (nombre == "svm") {
      scale  <- isolate(as.logical(input$scale))
      kernel <- isolate(input$kernel)
      modelo <- train.svm(formula, data = datos, 
                                   scale = scale, kernel = kernel)
      algoritmo <- paste0(nombre, "-", kernel)
    } else if (nombre == "tree") {
      minsplit <- isolate(input$minsplit)
      maxdepth <- isolate(input$maxdepth)
      split    <- isolate(input$split)
      modelo <- train.rpart(
        formula, data = datos, 
        control = rpart.control(minsplit = minsplit, maxdepth = maxdepth),
        parms = list(split = split))
      algoritmo <- paste0(nombre, "-", split)
    } else if (nombre == "rndf") {
      mtry  <- isolate(input$mtry)
      ntree <- isolate(input$ntree)
      modelo <-train.randomForest(
        formula, data = datos, mtry = mtry, ntree = ntree, importance = TRUE)
      algoritmo <- nombre
    } else if (nombre == "boost") {
      mfinal    <- isolate(input$mfinal)
      maxdepth  <- isolate(input$maxdepth)
      minsplit  <- isolate(input$minsplit) 
      coeflearn <- isolate(input$coeflearn)
      modelo <- train.adabag(
        formula, data = datos, mfinal = mfinal, coeflearn = coeflearn,
        control = rpart.control(minsplit = minsplit, maxdepth = maxdepth))
      algoritmo <- paste0(nombre, "-", coeflearn)
    } else if (nombre == "xgb") {
      nrounds  <- isolate(input$nrounds)
      max_depth <- isolate(input$max_depth)
      booster   <- isolate(input$booster)
      modelo <- train.xgboost(
        formula, data = datos, booster = booster,
        max_depth = max_depth, nrounds = nrounds)
      algoritmo <- paste0(nombre, "-", booster)
    } else if (nombre == "bayes") {
      modelo <- train.bayes(formula, data = datos)
      algoritmo <- nombre
    } else if (nombre == "nnet") {
      tryCatch({
        threshold <- isolate(input$threshold)
        stepmax   <- isolate(input$stepmax)
        hidden <- isolate(c(
          input$hidden1, input$hidden2, input$hidden3, input$hidden4,
          input$hidden5, input$hidden6, input$hidden7, input$hidden8,
          input$hidden9, input$hidden10))
        n_hidden <- isolate(input$n_hidden)
        hidden <- as.numeric(hidden[1:n_hidden])
        modelo <- train.neuralnet(
          formula = formula, data = datos, threshold = threshold,
          stepmax = stepmax, hidden = hidden, act.fct = "tanh")
        algoritmo <- nombre
      }, error = function(e) {
        stop(e)
        return(list(modelo = NULL, algoritmo = NULL))
      }, warning = function(w) {
        stop(w)
        return(list(modelo = NULL, algoritmo = NULL))
      })
    } else if (nombre == "reg") {
      suppressWarnings({
        modelo <- train.glm(formula, data = datos, control = list(maxit = 100))
        algoritmo <- nombre
      })
    } else if (nombre == "regp") {
      tryCatch({
        scales <- isolate(as.logical(input$scales))
        alpha  <- isolate(input$alpha)
        tipo   <- ifelse(alpha == 0, "ridge", "lasso")
        x      <- model.matrix(formula, datos)[, -1]
        y      <- datos[, cat]
        family <- ifelse(length(levels(y)) > 2, "multinomial", "multinomial")
        modelo <- cv.glmnet(x, y, standardize = scales, alpha = alpha, 
                            family = family)
        algoritmo <- paste0(nombre, "-", tipo)
      }, error = function(e) {
        return(list(modelo = NULL, algoritmo = NULL))
      }, warning = function(w) {
        return(list(modelo = NULL, algoritmo = NULL))
      })
    } else if (nombre == "lda") {
      suppressWarnings({
        modelo <- train.lda(formula, data = datos)
        algoritmo <- nombre
      })
    } else if (nombre == "qda") {
      modelo <- train.qda(formula, data = datos)
      algoritmo <- nombre
    }
    
    return(list(modelo = modelo, algoritmo = algoritmo))
  }, error = function(e) {
    print(e)
    return(list(modelo = NULL, algoritmo = NULL))
  })
}

# Generar modelos
generar.modelos <- function(nombre, formula, datos, input, cat) {
  modelos    <- list()
  algoritmos <- list()
  tryCatch({
    if(nombre == "knn") {
      scale  <- isolate(input$scale)
      kernel <- isolate(input$kernel)
      kmax   <- isolate(input$kmax)
      for (k in kernel) {
        algoritmo  <- paste0(nombre, "-", k)
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.knn(
          formula, data = datos, scale = scale, kernel = k, kmax = kmax)
      }
    } else if (nombre == "svm") {
      scale  <- isolate(as.logical(input$scale))
      kernel <- isolate(input$kernel)
      for (k in kernel) {
        algoritmo  <- paste0(nombre, "-", k)
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.svm(
          formula, data = datos, scale = scale, kernel = k)
      }
    } else if (nombre == "tree") {
      minsplit <- isolate(input$minsplit)
      maxdepth <- isolate(input$maxdepth)
      split    <- isolate(input$split)
      for (k in split) {
        algoritmo  <- paste0(nombre, "-", k)
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.rpart(
          formula, data = datos, 
          control = rpart.control(minsplit = minsplit, maxdepth = maxdepth),
          parms = list(split = k))
      }
    } else if (nombre == "rndf") {
      mtry  <- isolate(input$mtry)
      ntree <- isolate(input$ntree)
      algoritmo  <- nombre
      algoritmos <- append(algoritmos, algoritmo)
      modelos[[algoritmo]] <- train.randomForest(
        formula, data = datos, mtry = mtry, ntree = ntree, importance = TRUE)
    } else if (nombre == "boost") {
      mfinal    <- isolate(input$mfinal)
      maxdepth  <- isolate(input$maxdepth)
      minsplit  <- isolate(input$minsplit) 
      coeflearn <- isolate(input$coeflearn)
      for (k in coeflearn) {
        algoritmo  <- paste0(nombre, "-", k)
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.adabag(
          formula, data = datos, mfinal = mfinal, coeflearn = k,
          control = rpart.control(minsplit = minsplit, maxdepth = maxdepth))
      }
    } else if (nombre == "xgb") {
      nrounds  <- isolate(input$nrounds)
      max_depth <- isolate(input$max_depth)
      booster   <- isolate(input$booster)
      for (k in booster) {
        algoritmo <- paste0(nombre, "-", k)
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.xgboost(
          formula, data = datos, booster = k,
          max_depth = max_depth, nrounds = nrounds)
      }
    } else if (nombre == "bayes") {
      algoritmo <- nombre
      algoritmos <- append(algoritmos, algoritmo)
      modelos[[algoritmo]] <- train.bayes(formula, data = datos)
    } else if (nombre == "nnet") {
      threshold <- isolate(input$threshold)
      stepmax   <- isolate(input$stepmax)
      hidden <- isolate(c(
        input$hidden1, input$hidden2, input$hidden3, input$hidden4,
        input$hidden5, input$hidden6, input$hidden7, input$hidden8,
        input$hidden9, input$hidden10))
      n_hidden <- isolate(input$n_hidden)
      hidden <- as.numeric(hidden[1:n_hidden])
      algoritmo <- nombre
      algoritmos <- append(algoritmos, algoritmo)
      modelos[[algoritmo]] <- train.neuralnet(
        formula = formula, data = datos, threshold = threshold,
        stepmax = stepmax, hidden = hidden, act.fct = "tanh")
    } else if (nombre == "reg") {
      algoritmo <- nombre
      algoritmos <- append(algoritmos, algoritmo)
      modelos[[algoritmo]] <- train.glm(formula, data = datos)
    } else if (nombre == "regp") {
      scales <- isolate(as.logical(input$scales))
      alpha  <- isolate(input$alpha)
      for (k in alpha) {
        tipo <- ifelse(k == 0, "ridge", "lasso")
        algoritmo <- paste0(nombre, "-", tipo)
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.glmnet(
          formula, data = datos, standardize = scales,
          alpha = k, family = 'multinomial')
      }
    } else if (nombre == "lda") {
      suppressWarnings({
        algoritmo <- nombre
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.lda(formula, data = datos)
      })
    } else if (nombre == "qda") {
      algoritmo <- nombre
      algoritmos <- append(algoritmos, algoritmo)
      modelos[[algoritmo]] <- train.qda(formula, data = datos)
    }
    
    return(modelos)
  }, error = function(e) {
    print(e)
    return(list(modelos = NULL, algoritmos = NULL))
  })
}

# Indices con probabilidad de corte
indices.corte <- function(corte = 0.5, prob, categorias, positiva, prueba, print = TRUE){
  negativa <- categorias[categorias != positiva]
  pred     <- ifelse(prob >= corte, positiva, negativa)
  MC       <- table(prueba, Pred = factor(pred, levels = categorias))
  if(print){
    cat("\n========================================")
    cat("\nCorte usado para la Probabilidad = ")
    cat(Corte)
    cat("\n")
    print(general.indexes(mc = MC))
    cat("\n========================================")
  }
  return(list(MC = MC, pred = pred))
}

# Tabla con predicciones
tabla.pred <- function(real, pred, prob, etqs) {
  ids <- names(pred)
  if(is.numeric(pred)) {
    pred <- factor(pred, labels = levels(real))
  }
  real <- as.character(real)
  pred <- as.character(pred)
  #prob <- sapply(1:length(pred), function(i) prob[i, pred[i]])
  
  if(is.null(prob)) {
    df <- data.frame(real, pred)
  } else {
    prob <- round(prob, 3)
    df <- data.frame(real, pred, prob)
  }
  
  df$error <- ifelse(df$real == df$pred, 1, 0)
  e <- which(colnames(df) == "error")
  row.names(df) <- ids
  
  sketch <- htmltools::withTags(table(tableHeader(c("ID", etqs, "error"))))
  tabla <- DT::datatable(
    df, rownames = TRUE, selection = "none",
    editable = FALSE, escape = FALSE, container = sketch,
    options = list(
      dom = "frtip", pageLength = 10, 
      columnDefs = list(list(targets = e, visible = F))
    )
  ) |> formatStyle(
      'error', target = 'row',
      backgroundColor = styleEqual(c(0, 1), c('#f3c0bd', '#d5f0be')))
  return(tabla)
}

# Indices con probabilidad de corte por paso
indices.corte.paso <- function(paso = 0.5, prob, categorias, positiva, prueba) {
  negativa <- categorias[categorias != positiva]
  
  for (corte in seq(0, 1, by = paso)) {
    pred <- ifelse(prob >= corte, positiva, negativa)
    MC   <- table(prueba, Pred = factor(pred, levels = categorias))
    cat("\nCorte usado para la Probabilidad = ")
    cat(corte)
    cat("\n")
    print(general.indexes(mc = MC))
    cat("\n========================================")
  }
}

# Gráfico variación del error
e_var_error <- function(df, escalar = T, titulos = c("Iteracion", "Precision")) {
  r <- lapply(1:nrow(df), function(i) {
    pgs <- unname(unlist(df[i, , drop = F]))
    pgm <- mean(pgs)
    pgp <- lapply(1:length(pgs), function(j) {
      list(value = round(pgs[j], 2), xAxis = j - 1, yAxis = pgs[j])
    })
    if(length(pgs) == 1) {
      pgs <- list(pgs)
    }
    list(
      data = pgs,
      type = "line",
      name = row.names(df)[i],
      markLine = list(data = list(list(yAxis = pgm))),
      markPoint = list(data = pgp)
    )
  })
  
  if(ncol(df) == 1) {
    xdata <- as.character(1)
  } else {
    xdata <- 1:ncol(df)
  }
  
  res <- e_charts() |> e_list(list(
    xAxis = list(
      type = "category",
      data = xdata
    ),
    yAxis = list(
      type = "value"
    ),
    series = r
  )) |> e_show_loading() |> e_legend() |> 
    e_axis_labels(x = titulos[1], y = titulos[2]) |>
    e_x_axis(scale = T) |> e_datazoom(show = F)
  
  if(escalar) {
    res <- res |> e_y_axis(scale = T)
  } else {
    res <- res |> e_y_axis(min = 0, max = 100)
  }
  
  return(res)
}

# Gráfico Barras del error
e_eval_bar <- function(df, titulos = c("Modelo", "Precision")) {
  res <- df |> e_charts(Modelo) |>
    e_bar(PG, name = "Global")
  
  for (x in colnames(df)[4:ncol(df)]) {
    res <- res |> e_bar_(x, name = x) |>
      e_legend_unselect(x)
  }
  
  res <- res |> e_show_loading() |> e_legend(show = T) |>
    e_axis_labels(x = titulos[1], y = titulos[2]) |>
    e_datazoom(show = F) |> e_tooltip() |>
    e_x_axis(axisLabel = list(rotate = 45))
  
  return(res)
}

# Obtiene los puntos para graficar la curva ROC
roc.values <- function(score, clase, n = 20) {
  res <- lapply(seq(1, 0, length = n), function(umbral) {
    FN <- length(which(score[clase == levels(clase)[2]] < umbral))
    TP <- length(which(score[clase == levels(clase)[2]] >= umbral))
    FP <- length(which(score[clase == levels(clase)[1]] >= umbral))
    TN <- length(which(score[clase == levels(clase)[1]] < umbral))
    
    c(TP / ( FN + TP ), TN / ( FP + TN ), umbral)
  })
  res <- append(list(c(0, 1, 1)), res)
  res[!duplicated(res)]
}

# Obtiene todas las precisiones globales promedio train-test.
calc_cross_pg_tt <-  function(x, cat = "Global") {
  res <- data.frame()
  
  for (vc in 1:length(x)) {
    if(!is.null(names(x))) {
      if(names(x)[vc] %in% c("categoria", "corte")) {
        next
      }
    }
    k  <- x[[vc]]$nombre
    mc <- x[[vc]]$mc
    indices <- indices.generales(mc)
    if(cat == "Global") {
      res[k, vc] <- indices$precision.global
    } else {
      res[k, vc] <- indices$precision.clase[cat]
    }
  }
  
  return(res)
}
