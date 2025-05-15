#' RSDA to MM
#'
#' @name RSDA_to_MM
#' @aliases RSDA_to_MM
#' @description To convert RSDA format interval dataframe to MM format.
#' @usage RSDA_to_MM(data, RSDA)
#' @param data The RSDA format with interval dataframe.
#' @param RSDA Whether to load the RSDA package.
#' @returns Return a dataframe with the MM format.
#' @examples
#' data(mushroom.int)
#' RSDA_to_MM(mushroom.int, RSDA = FALSE)
#' @export

RSDA_to_MM <- function(data, RSDA = TRUE){
  num_int <- 0
  num_chr <- 0
  chr <- c()
  int <- c()
  index <- c()
  for (i in 1:ncol(data)){
    if (sapply(data, mode)[i] == 'complex'){
      num_int <- num_int + 1
      int <- c(int, i)
    } else{
      num_chr <- num_chr + 1
      chr <- c(chr, i)
    }
    index <- append(chr, int)
  }
  num <- num_chr + 2 * num_int
  df <- as.data.frame(matrix(nrow = nrow(data), ncol = num))
  df1 <- as.data.frame(matrix(nrow = nrow(data), ncol = 1))
  gsubfun <- function(x){
    x <- gsub("[{.*}]", "", x)
  }
  if (RSDA == TRUE){
    if (length(chr) != 0){
      for (i in 1:length(chr)){
        if (index[i] == 1){
          df[index[i]] <- data.frame(data[index[i]])
          A <- lapply(format(df[index[i]])[[1]], gsubfun)
          for (k in 1:nrow(df1)){
            df1[k, 1] <- A[[k]]
          }
          df[index[i]] <- df1
          names(df)[index[i]] <- colnames(data)[index[i]]
        } else{
          j <- 2 * (index[i] - index[i - 1])
          df[j] <- data.frame(data[index[i]])
          A <- lapply(format(df[j])[[1]], gsubfun)
          for (k in 1:nrow(df1)){
            df1[k, 1] <- A[[k]]
          }
          df[j] <- df1
          names(df)[j] <- colnames(data)[index[i]]
        }
      }
      x <- 0
      for (i in 1:length(int)){
        df[index[length(chr) + i] + x] <- data.frame(data[[index[length(chr) + i]]])[1]
        df[index[length(chr) + i] + x + 1] <- data.frame(data[[index[length(chr) + i]]])[2]
        names(df)[index[length(chr) + i] + x] <- paste(names(data[index[length(chr) + i]]), '_min', sep = '')
        names(df)[index[length(chr) + i] + x + 1] <- paste(names(data[index[length(chr) + i]]), '_max', sep = '')
        x <- x + 1
      }
    } else{
      x <- 0
      for (i in 1:length(int)){
        df[index[i] + x] <- data.frame(data[[index[i]]])[1]
        df[index[i] + x + 1] <- data.frame(data[[index[i]]])[2]
        names(df)[index[i] + x] <- paste(names(data[index[i]]), '_min', sep = '')
        names(df)[index[i] + x + 1] <- paste(names(data[index[i]]), '_max', sep = '')
        x <- x + 1
      }
    }
  } else{
    for (i in 1:length(chr)){
      if (index[i] == 1){
        df[index[i]] <- data.frame(data[index[i]])
        names(df)[index[i]] <- colnames(data)[index[i]]
      } else{
        j <- 2 * (index[i] - index[i - 1])
        df[j] <- data.frame(data[index[i]])
        names(df)[j] <- colnames(data)[index[i]]
      }
    }
    x <- 0
    for (i in 1:length(int)){
      df[index[length(chr) + i] + x] <- lapply(data.frame(data[[index[length(chr) + i]]]), Re)
      df[index[length(chr) + i] + x + 1] <- lapply(data.frame(data[[index[length(chr) + i]]]), Im)
      names(df)[index[length(chr) + i] + x] <- paste(names(data[index[length(chr) + i]]), '_min', sep = '')
      names(df)[index[length(chr) + i] + x + 1] <- paste(names(data[index[length(chr) + i]]), '_max', sep = '')
      x <- x + 1
    }
    for (i in 1:length(df)){
      if (sapply(df, class)[i] != 'character'){
        attributes(df[[i]])$class <- 'numeric'
      }
    }
  }
  return(df)
}
