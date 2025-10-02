#' Function to plot comparison of Prediction methods
#'
#' @param x as output object of 'prediction_errors()' function
#' @param ... arguments passed to or from other methods
#' @import ggplot2
#' @import reshape2
#' @import gridExtra
#' @return Returns error comparison plots for forecasting methods
#' @export
#' @examples
#' a <- prediction_errors(data = nottem)
#' b <- plot(a)

plot.prediction_errors <- function(x, ...)
{
  object <- x
  #options(warn=-1)
  args <- list(...)
  res <- list()
  for (i in 1:length(object@output))
  {
    if (grepl("error", tolower(names(object@output[i]))))
    {
      temp <- object@output[[i]]
      names(temp)[length(temp)] <- 'Execution time'
      name_df <- data.frame(names(temp))
      names(name_df) <- "Parameter"
      name_df <- data.frame(t(name_df))
      names(name_df) <- names(temp)
      df <- rbind(name_df, temp)

      df <- data.frame(t(df))
      plot_df <- melt(df, id.vars = c("Parameter"))
      plot_df$value <- round(as.numeric(plot_df$value),2)
      pbar <- ggplot(data = plot_df, aes(x = variable, y = value, fill = Parameter)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        ggtitle('Error values and execution time') +
        labs(x = "Method", y = 'Values') +
        theme_bw()+
        theme(text = element_text(size=15), legend.position = "bottom", legend.direction = "horizontal")
      res[[i]] <- pbar
    }
    else if(grepl("predicted", tolower(names(object@output[i]))))
    {
      plot_df <- melt(object@output[[i]], id.vars = names(object@output$Error_Parameters[,1]))
      names(plot_df) <- c('Series', 'Var2', 'value')
      pline <- ggplot(data = plot_df, aes(x = Var2)) +
        geom_line(aes(y = value, color=Series), size=1) +
        ggtitle('Forecasted values') +
        labs(x = "Time horizon", y = 'Values') +
        theme_bw()+
        theme(text = element_text(size=15), legend.position = "bottom", legend.direction = "horizontal")
      res[[i]] <- pline
    }

  }

  do.call(grid.arrange, res)
  names(res) <- names(object@output)
  return(res)
}

