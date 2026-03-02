histplot <- function(data, ncolX, ncolY){
  # data <- data[, 2:3]
  df <- data.frame("X" = data[[ncolX]], "Y" = data[[ncolY]])
  origX <- df %>% ggplot(aes(x = X)) + geom_histogram(aes(y = after_stat(density)), col = "darkred", fill = "red", 
                                                      alpha = 0.3, na.rm = TRUE, show.legend = FALSE, bins = 30, boundary = 0) +
    theme_minimal() + labs(x = "X", y = "Frequency") + ggtitle("Original margin of X")
  origY <- df %>% ggplot(aes(x = Y)) + geom_histogram(aes(y = after_stat(density)), col = "darkred", fill = "red", 
                                                      alpha = 0.3, na.rm = TRUE, show.legend = FALSE, bins = 30, boundary = 0) +
    theme_minimal() + labs(x = "X", y = "Frequency") + ggtitle("Original margin of Y")
  grid.arrange(origX, origY, nrow = 2)
}

timeseriesplot <- function(data, ncolX, ncolY){
  # data <- data[, 2:3]
  df <- data.frame("X" = data[[ncolX]], "Y" = data[[ncolY]])
  origX <- df %>% ggplot(aes(x = 1:length(X), y = X)) + geom_line() +
    theme_minimal() + labs(x = "Index", y = "X") + ggtitle("Time series of X")
  origY <- df %>% ggplot(aes(x = 1:length(Y), y = Y)) + geom_line() +
    theme_minimal() + labs(x = "Index", y = "Y") + ggtitle("Time series of Y")
  grid.arrange(origX, origY, nrow = 2)
}

acfplot <- function(data, ncolX, ncolY){
  # data <- data[, 2:3]
  df <- data.frame("X" = data[[ncolX]], "Y" = data[[ncolY]])
  bacfX <- acf(df$X, plot = F)
  bacfY <- acf(df$Y, plot = F)
  bacfdfX <- with(bacfX, data.frame(lag, acf))
  bacfdfY <- with(bacfY, data.frame(lag, acf))
  origX <- bacfdfX %>% ggplot(aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    theme_minimal() + labs(x = "Lag", y = "ACF") + ggtitle("Auto correlation plot of X")
  origY <- bacfdfY %>% ggplot(aes(x = lag, y = acf)) + geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    theme_minimal() + labs(x = "Lag", y = "ACF") + ggtitle("Auto correlation plot of Y")
  grid.arrange(origX, origY, nrow = 2)
}

jointplot <- function(data, ncolX, ncolY){
  # data <- data[, 2:3]
  df <- data.frame("X" = data[[ncolX]], "Y" = data[[ncolY]])
  df %>% ggplot(aes(x = X, y = Y)) + geom_point(na.rm = T) +
    theme_minimal() + ggtitle("Original margins")
}