#' gg_color
#'
#' @keywords internal
#' @importFrom grDevices hcl
#'
gg_color <- function (n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' prediction.variable.balance
#'
#' @description Function that graphs the balance of the different categories of a column of a data frame.
#'
#' @param data A data frame.
#' @param predict.variable Character type. The name of the variable to predict. This name must be part of the columns of the data frame.
#' @param ylab A character string that describes the y-axis on the graph.
#' @param xlab A character string that describes the x-axis on the graph.
#' @param main Character type. The main title of the chart.
#' @param col A vector that specifies the colors of the categories represented by bars within the chart.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @return A ggplot object.
#'
#' @note With this function we can identify if the data is balanced or not, according to the variable to be predicted.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'
#' prediction.variable.balance(iris,"Species")
#'
prediction.variable.balance <- function(data, predict.variable, ylab = "Number of individuals",
                                         xlab = "", main = paste("Variable Distribution",predict.variable), col = NA) {
  if(missing(predict.variable) | !(predict.variable %in% colnames(data))){
    stop("predict.variable must be entered and be part of the table columns", call. = FALSE )
  }
  if(is.character(data[,predict.variable]) | is.factor(data[,predict.variable])){
    if(length(col) == 0 || is.na(col)){
      col <- gg_color(length(unique(data[,predict.variable])))
    }else{
      col <- rep(col,length(unique(data[,predict.variable])))
    }
    ggplot(data = data, mapping = aes_string(x = predict.variable, fill = predict.variable)) +
      geom_bar() +
      scale_fill_manual(values = col, name = predict.variable) +
      labs(x = xlab, y = ylab, title = main) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }else{
    stop("The variable to predict must be of type factor or character", call. = FALSE )
  }
}


#' numerical.predictive.power
#'
#' @description Function that graphs the density of individuals and shows their category according to a numerical variable.
#'
#' @param data A data frame.
#' @param predict.variable Character type. The name of the variable to predict. This name must be part of the columns of the data frame.
#' @param variable.to.compare Character type. The name of the numeric variable to compare. This name must be part of the columns of the data frame.
#' @param ylab A character string that describes the y-axis on the graph.
#' @param xlab A character string that describes the x-axis on the graph.
#' @param main Character type. The main title of the chart.
#' @param col A vector that specifies the colors of the categories of the variable to predict.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @return A ggplot object.
#'
#' @note With this function we can analyze the predictive power of a numerical variable.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'
#' numerical.predictive.power(iris,"Species","Sepal.Length")
#'
numerical.predictive.power <- function(data, predict.variable, variable.to.compare, ylab = "",
                                      xlab = "", main = paste("Variable Density", variable.to.compare, 'according to', predict.variable), col = NA){

  if(missing(predict.variable) | !(predict.variable %in% colnames(data))){
    stop("predict.variable must be entered and be part of the table columns", call. = FALSE )
  }
  if(missing(variable.to.compare) | !(variable.to.compare %in% colnames(data)) | !is.numeric(data[,variable.to.compare])){
    stop("variable.to.compare must be entered and be part of the numeric columns of the table", call. = FALSE )
  }

  if(is.character(data[,predict.variable]) | is.factor(data[,predict.variable])){
    if(length(col) == 0 || is.na(col)){
      col <- gg_color(length(unique(data[,predict.variable])))
    }else{
      col <- rep(col,length(unique(data[,predict.variable])))
    }

    ggplot(data = data, aes_string(variable.to.compare, fill = predict.variable)) +
      geom_density(alpha = .7, color = NA) +
      scale_fill_manual(values = col) +
      labs(title = main , y = ylab, x = xlab ,fill = predict.variable) +
      theme_minimal() +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            text = element_text(size = 15))

  }else{
    stop("The variable to predict must be of type factor or character", call. = FALSE )
  }
}


#' categorical.predictive.power
#'
#' @description Function that graphs the distribution of individuals and shows their category according to a categorical variable.
#'
#' @param data A data frame.
#' @param predict.variable Character type. The name of the variable to predict. This name must be part of the columns of the data frame.
#' @param variable.to.compare Character type. The name of the categorical variable to compare. This name must be part of the columns of the data frame.
#' @param ylab A character string that describes the y-axis on the graph.
#' @param xlab A character string that describes the x-axis on the graph.
#' @param main Character type. The main title of the chart.
#' @param col A vector that specifies the colors of the categories of the variable to predict.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @return A ggplot object.
#'
#' @note With this function we can analyze the predictive power of a categorical variable.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' cars <- datasets::mtcars
#' cars$cyl <- as.factor(cars$cyl)
#' cars$vs <- as.factor(cars$vs)
#' categorical.predictive.power(cars,"vs","cyl")
#'
categorical.predictive.power <- function(data, predict.variable, variable.to.compare, ylab = "",
                                        xlab = "", main = paste("Variable Distribution", variable.to.compare, 'according to', predict.variable), col = NA) {

  if(missing(predict.variable) | !(predict.variable %in% colnames(data))){
    stop("predict.variable must be entered and be part of the table columns", call. = FALSE )
  }
  if(missing(variable.to.compare) | !(variable.to.compare %in% colnames(data)) |
     !(is.factor(data[,variable.to.compare]) | is.character(data[,variable.to.compare])) ){
    stop("variable.to.compare must be entered and be part of the categorical columns of the table", call. = FALSE )
  }

  if(is.character(data[,predict.variable]) | is.factor(data[,predict.variable])){
    if(length(col) == 0 || is.na(col)){
      col <- gg_color(length(unique(data[,predict.variable])))
    }else{
      col <- rep(col,length(unique(data[,predict.variable])))
    }

    suppressWarnings(datos2 <- data |>
      dplyr::group_by_(variable.to.compare, predict.variable) |>
      dplyr::summarise(count = n()))

    if(variable.to.compare != predict.variable){
      suppressWarnings(datos2 <-   datos2 |> dplyr::group_by_(variable.to.compare))
    }
    datos2 <- datos2 |> dplyr::mutate(prop = round(count/sum(count),4))

    ggplot(data = datos2, mapping = aes_string(x = variable.to.compare, y = "prop", fill = predict.variable)) +
      geom_col(position = "fill") +
      geom_text(aes(label = paste0(round(prop * 100, 2), "% (", count, ")")), position = position_stack(vjust = .5), color = "white") +
      scale_y_continuous(labels = function(x) return(paste0(x * 100, "%"))) +
      labs(y =  xlab, x  = ylab, title = main) +
      scale_fill_manual(values = col, name = predict.variable) +
      theme(legend.position = "bottom")+
      coord_flip()

  }else{
    stop("The variable to predict must be of type factor or character", call. = FALSE )
  }
}

#' importance.plot
#'
#' @description Function that graphs the importance of the variables.
#'
#' @param model fitted model object.
#' @param col the color of the chart bars.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[traineR]{train.adabag}}, \code{\link[adabag]{boosting}}
#'
#' @return A ggplot object.
#'
#' @note With this function we can identify how important the variables are for the generation of a predictive model.
#'
#' @import ggplot2
#' @importFrom stats reorder
#' @importFrom gbm summary.gbm
#' @importFrom xgboost xgb.importance
#'
#' @export
#'
#' @examples
#'
#'data <- iris
#'n <- nrow(data)
#'
#'sam <- sample(1:n,n*0.75)
#'training <- data[sam,]
#'testing <- data[-sam,]
#'
#'model <- train.adabag(formula = Species~.,data = training,minsplit = 2,
#'  maxdepth = 30, mfinal = 10)
#'importance.plot(model)
#'
importance.plot <- function(model, col = "steelblue") {
  if("adabag.prmdt" %in% class(model)) {
    df <- data.frame(x = names(model$importance), y = model$importance)
  } else if("randomForest.prmdt" %in% class(model)) {
    aux <- data.frame(model$importance)
    df <- data.frame(x = row.names(aux), y = aux$MeanDecreaseGini)
  } else if("xgb.Booster.prmdt" %in% class(model)) {
    aux <- data.frame(xgb.importance(model = model))
    df <- data.frame(x = aux$Feature, y = aux$Frequency)
  } else if("gbm.prmdt" %in% class(model)) {
    aux <- summary.gbm(model, plotit = F)
    df <- data.frame(x = aux$var, y = aux$rel.inf)
  } else {
    stop("The model does not have this functionality.")
  }

  df$y <- round(df$y, digits = 2)
  ggplot(data = df, aes(x = reorder(x, .data$y), y = y)) +
    geom_bar(stat = "identity", fill = col, width = 0.6) +
    theme_minimal() +
    labs(title = "Variable Importance",
         y = "Percentage of Importance", x = "Variable Names") +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    coord_flip()
}


