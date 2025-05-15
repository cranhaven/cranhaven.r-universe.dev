#' general.indices
#'
#' @description calculates indices to measure accuracy of a model.
#'
#' @param real the real values in traning-testing.
#' @param prediccion the prediction values in traning-testing.
#'
#' @return a list with the Correlation, Relative Error, Mean Absolute Error and Root Mean Square Error.
#' @export
#'
#' @examples
#' real <- rnorm(45)
#' prediction <- rnorm(45)
#' model <- "KNN"
#' general.indices(real, prediction)
#' 
general.indices <- function(real, prediccion) {
  RMSE <- sqrt(sum((real - prediccion) ^ 2) / length(prediccion))
  MAE  <- sum(abs(real - prediccion)) / length(prediccion)
  RE   <- sum(abs(real - prediccion)) / sum(abs(real)) * 100
  desvStand <- sd(prediccion)
  COR  <- ifelse(near(desvStand,0), 0, as.numeric(cor(real, prediccion)))
  COR  <- ifelse(is.na(COR), 0 , COR)
  indices <- list(RMSE = RMSE, MAE = MAE, RE = RE, COR = COR)
  return(indices)
}

# Opciones KNN
opciones.knn <- function(ns, vc = F) {
  opciones <- tags$div(
    numericInput(ns("kmax"), labelInput("kmax"), min = 1, 
                 step = 1, value = 1),
    selectInput(
      inputId = ns("kernel"), label = labelInput("selkernel"),
      choices = c("optimal", "rectangular", "triangular", 
                  "epanechnikov", "biweight", "triweight",
                  "cos", "inv", "gaussian"), 
      selected = "optimal", multiple = vc),
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
    selectInput(ns("split"), label = labelInput("splitIndex"), 
                choices =  list("gini" = "gini", "Entropia" = "information"),
                selected = "gini", multiple = vc)
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
    numericInput(ns("n.trees"), labelInput("numTree"),
                 5, width = "100%", min = 1),
    numericInput(ns("shrinkage"), labelInput("shrinkage"), 0.1, 
                 width = "100%", min = 0.01, step = 0.01),
    selectInput(ns("distribution"), label = labelInput("selectAlg"), 
                choices = c("gaussian", "laplace", "tdist"),
                selected = "gaussian", multiple = vc)
  )
  return(opciones)
}

# Opciones XGB
opciones.xgb <- function(ns, vc = F) {
  opciones <- tags$div(
    numericInput(ns("nrounds"), labelInput("selnrounds"),
                 min = 0, step = 1, value = 20),
    numericInput(ns("max_depth"), labelInput("maxdepth"),
                 min = 1, step = 1, value = 6),
    selectInput(ns("booster"), label = labelInput("selbooster"),
                choices = c("gbtree", "gblinear", "dart"),
                selected = "gbtree", multiple = vc)
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

# Opciones reduccion de la dimension
opciones.rdim <- function(ns, vc = F) {
  opciones <- tags$div(
    selectInput(ns("algoritmo"), label = labelInput("selectAlg"),
                selected = "ACP", choices = list("ACP", "MCP"), multiple = vc),
    radioSwitch(ns("scale"), "escal", c("si", "no")),
    fluidRow(
      col_6(
        numericInput(ns("ncomp"), labelInput("ncomp"), 2, 1, width = "100%")
      ),
      col_6(
        radioSwitch(ns("ncompdef"), "", c("manual", "automatico"))
      )
    )
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
  } else if (nombre == "nnet") {
    opciones <- opciones.nnet(ns)
  } else if (nombre == "reg") {
    opciones <- NULL
  } else if (nombre == "regp") {
    opciones <- opciones.regp(ns, vc)
  } else if (nombre == "rdim") {
    opciones <- opciones.rdim(ns, vc)
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
      n.trees      <- isolate(input$n.trees)
      distribution <- isolate(input$distribution)
      shrinkage    <- isolate(input$shrinkage)
      modelo <- train.gbm(
        formula, data = datos, distribution = distribution, n.trees = n.trees,
        shrinkage = shrinkage)
      algoritmo <- paste0(nombre, "-", distribution)
    } else if (nombre == "xgb") {
      nrounds  <- isolate(input$nrounds)
      max_depth <- isolate(input$max_depth)
      booster   <- isolate(input$booster)
      modelo <- train.xgboost(
        formula, data = datos, booster = booster,
        max_depth = max_depth, nrounds = nrounds)
      algoritmo <- paste0(nombre, "-", booster)
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
        modelo <- train.glm(formula, data = datos, family = "gaussian")
        algoritmo <- nombre
      })
    } else if (nombre == "regp") {
      tryCatch({
        scales <- isolate(as.logical(input$scales))
        alpha  <- isolate(input$alpha)
        tipo   <- ifelse(alpha == 0, "ridge", "lasso")
        x      <- model.matrix(formula, datos)[, -1]
        y      <- datos[, cat]
        modelo <- cv.glmnet(x, y, standardize = scales, alpha = alpha)
        algoritmo <- paste0(nombre, "-", tipo)
      }, error = function(e) {
        return(list(modelo = NULL, algoritmo = NULL))
      }, warning = function(w) {
        return(list(modelo = NULL, algoritmo = NULL))
      })
    } else if (nombre == "rdim") {
      algoritmo <- isolate(input$algoritmo)
      scale     <- isolate(input$scale)
      ncomp     <- isolate(input$ncomp)
      ncompdef  <- isolate(input$ncompdef)
      if(ncompdef) {
        if(algoritmo == "ACP") {
          modelo <- pcr(formula, ncomp = ncomp, data = datos, scale = scale, validation = 'CV')
        } else {
          modelo <- plsr(formula, ncomp = ncomp, data = datos, scale = scale, validation = 'CV')
        }
      } else {
        if(algoritmo == "ACP") {
          modelo <- pcr(formula, data = datos, scale = scale, validation = 'CV')
        } else {
          modelo <- plsr(formula, data = datos, scale = scale, validation = 'CV')
        }
      }
      optimal.ncomp <- which.min(RMSEP(modelo)$val[1, 1, ]) - 1
      modelo$optimal.ncomp <- optimal.ncomp
      algoritmo <- paste0(nombre, "-", algoritmo)
    }
    
    return(list(modelo = modelo, algoritmo = algoritmo))
  }, error = function(e) {
    print(e)
    return(list(modelo = NULL, algoritmo = NULL))
  })
}

# Generar modelos
generar.modelos <- function(nombre, formula, datos, input) {
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
      n.trees      <- isolate(input$n.trees)
      distribution <- isolate(input$distribution)
      shrinkage    <- isolate(input$shrinkage)
      
      for (k in distribution) {
        algoritmo  <- paste0(nombre, "-", k)
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.gbm(
          formula, data = datos, distribution = k, n.trees = n.trees,
          shrinkage = shrinkage)
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
      modelos[[algoritmo]] <- train.glm(formula, data = datos,
                                        family = "gaussian")
    } else if (nombre == "regp") {
      scales <- isolate(as.logical(input$scales))
      alpha  <- isolate(input$alpha)
      for (k in alpha) {
        tipo <- ifelse(k == 0, "ridge", "lasso")
        algoritmo <- paste0(nombre, "-", tipo)
        algoritmos <- append(algoritmos, algoritmo)
        modelos[[algoritmo]] <- train.glmnet(
          formula, data = datos, standardize = scales,
          alpha = k, family = "gaussian")
      }
    } else if (nombre == "rdim") {
      algoritmo <- isolate(input$algoritmo)
      scale     <- isolate(input$scale)
      ncomp     <- isolate(input$ncomp)
      ncompdef  <- isolate(input$ncompdef)
      for (k in algoritmo) {
        algoritmo  <- paste0(nombre, "-", k)
        algoritmos <- append(algoritmos, algoritmo)
        if(ncompdef) {
          if(algoritmo == "ACP") {
            modelo <- pcr(
              formula, ncomp = ncomp, data = datos,
              scale = scale, validation = 'CV')
          } else {
            modelo <- plsr(
              formula, ncomp = ncomp, data = datos,
              scale = scale, validation = 'CV')
          }
        } else {
          if(algoritmo == "ACP") {
            modelo <- pcr(
              formula, data = datos,
              scale = scale, validation = 'CV')
          } else {
            modelo <- pcr(
              formula, data = datos,
              scale = scale, validation = 'CV')
          }
        }
        optimal.ncomp <- which.min(RMSEP(modelo)$val[1, 1, ]) - 1
        modelo$optimal.ncomp <- optimal.ncomp
        modelos[[algoritmo]] <- modelo
      }
    }
    
    return(modelos)
  }, error = function(e) {
    print(e)
    return(list(modelos = NULL, algoritmos = NULL))
  })
}

#' plot_real_prediction
#'
#' @description scatter plot between the actual value of the variable to be predicted and the prediction of the model.
#'
#' @param real the real values in traning-testing.
#' @param pred the prediction values in traning-testing.
#' @param titles Labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @export
#' 
plot_real_prediction <- function(real, pred, titles = c("Real", "Prediccion")) {
  
  tooltip <- paste0(
    "function(params) {",
    " mark = params.marker + '<br/>';",
    " id = '<b>ID: </b>' + params.name + '<br/>';",
    " real = '<b>", titles[1], ": </b>' + params.value[0].toFixed(4) + '<br/>';",
    " pred = '<b>", titles[2], ": </b>' + params.value[1].toFixed(4) + '<br/>';",
    " return(mark + id + real + pred)",
    "}"
  )
  
  df <- data.frame(real = real, pred = pred)
  df$ID <- row.names(df)
  df |> 
    e_charts(real) |> 
    e_scatter(pred, bind = ID, symbol_size = 10) |> 
    e_lm(pred ~ real) |>
    e_axis_labels(x = titles[1], y = titles[2]) |>
    e_axis(scale = TRUE) |>
    e_tooltip(formatter = e_JS(tooltip)) |>
    e_datazoom(show = FALSE) |>
    e_legend(show = FALSE)
}

# Tabla con predicciones
tabla.pred <- function(real, pred, etqs, decs) {
  ids  <- names(pred)
  real <- round(as.numeric(real), decs)
  pred <- round(as.numeric(pred), decs)
  df   <- data.frame(real, pred)
  df$error <- round(df$pred - df$real, decs)
  row.names(df) <- ids
  
  sketch <- htmltools::withTags(table(tableHeader(c("ID", etqs, "error"))))
  tabla <- DT::datatable(
    df, rownames = TRUE, selection = "none",
    editable = FALSE, escape = FALSE, container = sketch,
    options = list(dom = "frtip", pageLength = 10)
  )
  return(tabla)
}

# Gráfico variación del error
e_var_error <- function(df, escalar = T, titulos = c("Iteracion", "Error")) {
  r <- lapply(1:nrow(df), function(i) {
    pgs <- unname(unlist(df[i, , drop = F]))
    pgm <- mean(pgs)
    pgp <- lapply(1:length(pgs), function(j) {
      list(value = pgs[j], xAxis = j - 1, yAxis = pgs[j])
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
e_eval_bar <- function(df, titulos = c("Modelo", "Error")) {
  res <- df |> e_charts(Modelo) |>
    e_bar(RMSE, name = "RMSE")
  
  for (x in colnames(df)[3:ncol(df)]) {
    res <- res |> e_bar_(x, name = x) |>
      e_legend_unselect(x)
  }
  
  res <- res |> e_show_loading() |> e_legend(show = T) |>
    e_axis_labels(x = titulos[1], y = titulos[2]) |>
    e_datazoom(show = F) |> e_tooltip() |>
    e_x_axis(axisLabel = list(rotate = 45))
  
  return(res)
}