#' Kullback-Leibler divergence plot
#' 
#' A plotting function for displaying Kullback-Liebler (KL) divergence across 
#' iterations of the optimization algorithm(s).
#'
#'@param monitoring \code{list} of monitoring estimates from \code{CytOpt()} output.
#'@param n_0 first iteration to plot. Default is 10.
#'@param n_stop last iteration to plot. Default is 1000.
#'@param title plot title. Default is \code{"Kullback-Liebler divergence trace"}.
#'
#'@return a \code{\link[ggplot2]{ggplot}} object
#'
#'@import ggplot2
#'@export
#'
#'@examples
#'
#'if(interactive()){
#'
#'gold_standard_manual_prop <- c(table(HIPC_Stanford_1369_1A_labels) / 
#'  length(HIPC_Stanford_1369_1A_labels))
#'res <- CytOpT(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A, 
#'              Lab_source = HIPC_Stanford_1228_1A_labels,
#'              theta_true = gold_standard_manual_prop,
#'              eps = 0.0001, lbd = 0.0001, n_iter = 10000, n_stoc=10,
#'              step_grad = 10, step = 5, power = 0.99, 
#'              method='both', monitoring = TRUE)
#'plot(res)
#'
#'}


KL_plot <- function (monitoring, n_0 = 10, n_stop=1000, title = "Kullback-Liebler divergence trace"){

  # sanity checks ----
  stopifnot(!(is.integer(n_0) & is.integer(n_stop)))

  #message
  message("Plotting KL divergence for iterations ", n_0, " to ", n_stop, " while there", 
          " were at least ", length(monitoring[[1]]), " iterations performed ",
          "for each method.")
  
  # constructing plot data ----
  index <- seq(n_0,n_stop)
  data2Opt <- data.frame("index" = index,
                        "values"=c(monitoring$Descent_ascent[index],
                                   monitoring$MinMax[index]),
                        "Method"=factor(rep(names(monitoring),each=length(index))))

  data2Opt$Method <- gsub("MinMax", "MinMax swapping",
                           gsub("Descent_ascent", "Descent-Ascent", data2Opt$Method))

  # constructing plot
  
  cols <- met.brewer("Egypt", type="discrete", n=3)[-1]
  names(cols) <- c("Descent-Ascent", "MinMax swapping")
  
  p <- ggplot(data2Opt, aes_string(x = "index", y = "values")) +
          geom_line(aes_string(color = "Method")) +
          scale_color_manual("Algorithm", 
                             values = cols,
                             breaks=unique(data2Opt$Method)
                             ) +
          ylab( expression(paste("KL(", hat(p), "|p)"))) +
          xlab("Iteration") +
          ggtitle(title) +
          theme_bw()

  return(p)

  }
