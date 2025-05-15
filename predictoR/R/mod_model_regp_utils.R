#' Possible lambda
#' 
#' @param cv.glm a cv.glmnet model.
#' @param labels a character vector of length 3 specifying the titles to use on legend.
#' 
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_posib_lambda
#' @import echarts4r
#' @importFrom glmnet cv.glmnet
#' @examples
#' x         <- model.matrix(Species~., iris)[, -1]
#' y         <- iris[,'Species']
#' cv.glm    <- glmnet::cv.glmnet(x, y, standardize = TRUE, alpha = 1, family = 'multinomial')
#' e_posib_lambda(cv.glm)
#'
e_posib_lambda <- function(cv.glm, labels = c("Valor Superior", "Valor Inferior", "lambda")) {
  error.tooltip <- e_JS(paste0(
    "function(params) {",
    "  var sup = Number.parseFloat(params.value[2]).toFixed(3);",
    "  var inf = Number.parseFloat(params.value[1]).toFixed(3);",
    "  var etqSup = '<b>", labels[1], ": </b>' + sup;",
    "  var etqInf = '<b>", labels[2], ": </b>' + inf;",
    "  return(etqSup + '<br>' + etqInf)",
    "}"
  ))
  
  lambda.min.tooltip <- e_JS(paste0(
    "function(params) {",
    "  var val = Number.parseFloat(params.value).toFixed(4);",
    "  return('<b>Log(lambda.min): </b>' + val)",
    "}"
  ))
  
  lambda.1se.tooltip <- e_JS(paste0(
    "function(params) {",
    "  var val = Number.parseFloat(params.value).toFixed(4);",
    "  return('<b>Log(lambda.1se): </b>' + val)",
    "}"
  ))
  
  datos <- data.frame(
    x = log(cv.glm$lambda), y = cv.glm$cvm,
    upper = cv.glm$cvup, lower = cv.glm$cvlo
  )
    
  res <- datos |> 
    e_charts(x) |> e_scatter(y, symbol_size = 7) |> 
    e_error_bar(lower, upper, tooltip = list(formatter = e_JS(error.tooltip))) |> 
    e_mark_line(data = list(xAxis = datos$x[cv.glm$index[[1]]],
                tooltip = list(formatter = lambda.min.tooltip))) |> 
    e_mark_line(data = list(xAxis = datos$x[cv.glm$index[[2]]],
                tooltip = list(formatter = lambda.1se.tooltip))) |> 
    e_axis_labels(x = labels[3], y = cv.glm$name[[1]]) |> 
    e_x_axis(formatter = e_axis_formatter(digits = 1)) |>  
    e_legend(FALSE) |>  
    e_tooltip() |> e_datazoom(show = F) |> e_show_loading()
  res$x$opts$xAxis[[1]]$type <- "value"
  return(res)
}

#' Coefficients and lambda
#' 
#' @description Plot the coefficients and selected lambda of a glmnet model.
#'
#' @param model a glmnet model.
#' @param cat a category of the variable to be predicted.
#' @param sel.lambda the selected lambda.
#' @param label a character specifying the title to use on selected lambda tooltip.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @import glmnet
#' @importFrom stats coef
#' 
#' @export e_coeff_lambda
#'
#' @examples
#' x <- model.matrix(Species ~ ., iris)[, -1]
#' y <- iris$Species
#' modelo <- glmnet::cv.glmnet(x, y, standardize = TRUE, alpha = 1, family = "multinomial")
#' e_coeff_lambda(modelo, 'setosa', log(modelo$lambda[1]))
#' 
e_coeff_lambda <- function(model, cat, sel.lambda = NULL, label = 'Log Lambda') {
  if(is.null(sel.lambda)) {
    sel.lambda <- log(model$lambda.1se)
  }
  
  datos <- data.frame()
  for (lambda in model$lambda) {
    coeficientes <- coef(model, s = lambda)
    coeficientes <- data.frame(t(coeficientes[[cat]][-1, 1]))
    coeficientes[["lambda"]] <- lambda
    coeficientes[["X"]] <- log(lambda)
    datos <- rbind(datos, coeficientes)
  }
  
  res <- datos |> e_charts(X)
  
  for (var in names(datos)[1:(ncol(datos)-2)]) {
    res <- res |> e_line_(var)
  }
  
  res <- res |>
    e_tooltip(trigger = "axis") |>
    e_mark_line(data = list(xAxis = sel.lambda)) |>
    e_axis_labels(x = label, y = paste0('Coefficients: Response ', cat)) |>  
    e_datazoom(show = F) |> e_show_loading()
  
  return(res)
}
