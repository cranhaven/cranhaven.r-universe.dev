histexpplot <- function(data, colX, colY, qmarg1, qmarg2, constrainedshape){
  data <- cbind(data[[colX]], data[[colY]])
  constrainedshape <- as.logical(constrainedshape)
  dataexp <- margtransf(data = data, qmarg = c(qmarg1, qmarg2), constrainedshape = constrainedshape)@dataexp
  df <- data.frame("Xexp" = dataexp[, 1], "Yexp" = dataexp[, 2])
  expX <- ggplot(data = df, aes(x = Xexp)) + geom_histogram(aes(y = after_stat(density)), col = "darkred", fill = "red", alpha = 0.3, 
                                                            na.rm = TRUE, show.legend = FALSE, bins = 30, boundary = 0) +
    theme_minimal() + labs(x = expression(X[exp]), y = "Frequency") + ggtitle("Marginal transformation of X")
  expY <- ggplot(data = df, aes(x = Yexp)) + geom_histogram(aes(y = after_stat(density)), col = "darkblue", fill = "blue", alpha = 0.3, 
                                                            na.rm = TRUE, show.legend = FALSE, bins = 30, boundary = 0) +
    theme_minimal() + labs(x = expression(Y[exp]), y = "Frequency") + ggtitle("Marginal transformation of Y")
  grid.arrange(expX, expY, nrow = 2)
}

qqplot <- function(data, colX, colY, qmarg1, qmarg2, constrainedshape, blocksize, nboot, alpha){
  data <- cbind(data[[colX]], data[[colY]])
  constrainedshape <- as.logical(constrainedshape)
  dataexp <- margtransf(data = data, qmarg = c(qmarg1, qmarg2), constrainedshape = constrainedshape)
  marggpddata <- marggpd(dataexp, blocksize = blocksize, nboot = nboot, alpha = alpha)
  plot(marggpddata)
}

jointexpplot <- function(data, colX, colY, qmarg1, qmarg2, constrainedshape){
  data <- cbind(data[[colX]], data[[colY]])
  constrainedshape <- as.logical(constrainedshape)
  dataexp <- margtransf(data = data, qmarg = c(qmarg1, qmarg2), constrainedshape = constrainedshape)@dataexp
  df <- data.frame("Xexp" = dataexp[, 1], "Yexp" = dataexp[, 2])
  df %>% ggplot(aes(x = Xexp, y = Yexp)) + geom_point(na.rm = TRUE) +
    theme_minimal() + labs(x = expression(X[exp]), y = expression(Y[exp])) +
    ggtitle("Standard exponential margins")
}