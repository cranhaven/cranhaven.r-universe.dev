
normalization <- function(data, class) {
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  class.name <- colnames(data[class]) 
  data <- data[ ,!colnames(data) == class.name]

  # Normalize each feature
  normalized.data <- as.data.frame(lapply(data, normalize))

  return(normalized.data)
}
