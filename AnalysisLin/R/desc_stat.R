#' @title Descriptive Statistics
#' @description desc_stat() function calculates various key descriptive statistics for each
#' variables in the provided data set. The function computes the count, number of unique values,
#' duplicate count, number of missing values, null rate, data type, minimum value, 25th percentile,
#' mean, median, 75th percentile, maximum value, standard deviation, kurtosis, skewness, and jarque_pvalue for each variable.
#' @param data input dataset
#' @param count An logical argument(default TRUE) that determines if count is included in the output
#' @param unique An logical argument(default TRUE) that determines if unique is included in the output
#' @param duplicate An logical argument(default TRUE) that determines if duplicate is included in the output
#' @param null An logical argument(default TRUE) that determines if null is included in the output
#' @param null_rate An logical argument(default TRUE) that determines if null_rate is included in the output
#' @param type An logical argument(default TRUE) that determines if type is included in the output
#' @param min An logical argument(default TRUE) that determines if min is included in the output
#' @param p25 An logical argument(default TRUE) that determines if p25 is included in the output
#' @param mean An logical argument(default TRUE) that determines if mean is included in the output
#' @param median An logical argument(default TRUE) that determines if median is included in the output
#' @param p75 An logical argument(default TRUE) that determines if p75 is included in the output
#' @param max An logical argument(default TRUE) that determines if max is included in the output
#' @param sd An logical argument(default TRUE) that determines if sd is included in the output
#' @param kurtosis An logical argument(default FALSE) that determines if kurtosis is included in the output
#' @param skewness An logical argument(default FALSE) that determines if skewness is included in the output
#' @param shapiro An logical argument(default FALSE) that determines if shapiro p-value is included in the output
#' @param kolmogorov An logical argument(default FALSE) that determines if kolmogorov p-value is included in the output
#' @param anderson An logical argument(default FALSE) that determines if anderson p-value is included in the output
#' @param lilliefors An logical argument(default FALSE) that determines if lilliefors p-value is included in the output
#' @param jarque An logical argument(default FALSE) that determines if jarque p-value is included in the output
#' @importFrom htmltools tagList
#' @importFrom DT datatable
#' @import stats
#' @return A data frame which summarizes the characteristics of a data set
#' @export
#'
#' @examples 
#' data(mtcars)
#' desc_stat(mtcars)
desc_stat <- function(data, count = TRUE, unique = TRUE, duplicate = TRUE, null = TRUE,
                      null_rate = TRUE, type = TRUE, min = TRUE, p25 = TRUE, mean = TRUE,
                      median = TRUE, p75 = TRUE, max = TRUE, sd = TRUE, skewness = FALSE,
                      kurtosis = FALSE, shapiro = FALSE, kolmogorov = FALSE, anderson = FALSE,
                      lilliefors = FALSE, jarque = FALSE) {
  
  if (length(data) == 0) stop("Input data is empty.") 
  Dname <- deparse(substitute(data))
  num_stats <- sum(count, unique, duplicate, null, null_rate, type, min, p25,
                   mean, median, p75, max, sd, skewness, kurtosis, shapiro, kolmogorov,
                   anderson, lilliefors, jarque)
  
  desc <- matrix(0, ncol =ncol(data), nrow = num_stats)
  colnames(desc) <- names(data)
  
  row_counter <- 1
  
  if (count) {
    desc[row_counter, ] <- sapply(data, function(x) sum(!is.na(x)))
    row_counter <- row_counter + 1
  }
  if (unique) {
    desc[row_counter, ] <- sapply(data, function(x) length(unique(x)))
    row_counter <- row_counter + 1
  }
  if (duplicate) {
    desc[row_counter, ] <- sum(duplicated(data))
    row_counter <- row_counter + 1
  }
  if (null) {
    desc[row_counter, ] <- sapply(data, function(x) sum(is.na(x)))
    row_counter <- row_counter + 1
  }
  if (null_rate) {
    desc[row_counter, ] <- sapply(data, function(x) round(sum(is.na(x)) / length(x),4))
    row_counter <- row_counter + 1
  }
  if (type) {
    desc[row_counter, ] <- sapply(data, function(x) class(x))
    row_counter <- row_counter + 1
  }
  if (min) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), min(x, na.rm = TRUE), NA))
    row_counter <- row_counter + 1
  }
  if (p25) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), quantile(x, 0.25, na.rm = TRUE), NA))
    row_counter <- row_counter + 1
  }
  if (mean) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), round(mean(x, na.rm = TRUE), 4), NA))
    row_counter <- row_counter + 1
  }
  if (median) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), median(x, na.rm = TRUE), NA))
    row_counter <- row_counter + 1
  }
  if (p75) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), quantile(x, 0.75, na.rm = TRUE), NA))
    row_counter <- row_counter + 1
  }
  if (max) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), max(x, na.rm = TRUE), NA))
    row_counter <- row_counter + 1
  }
  if (sd) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), round(sd(x,na.rm = TRUE), 4), NA))
    row_counter <- row_counter + 1
  }
  if (kurtosis) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), round(kurtosis(x), 4), NA))
    row_counter <- row_counter + 1
  }
  if (skewness) {
    desc[row_counter, ] <- sapply(data, function(x) ifelse(is.numeric(x), round(skewness(x), 4), NA))
    row_counter <- row_counter + 1
  }
  if (shapiro) {
    is_numeric <- sapply(data, is.numeric)
    desc[row_counter, ] <- ifelse(is_numeric, round(sapply(data[, is_numeric], function(x) shapiro.test(x)$p.value), 4), NA)
    row_counter <- row_counter + 1
  }
  
  if (kolmogorov) {
    is_numeric <- sapply(data, is.numeric)
    desc[row_counter, ] <- ifelse(is_numeric, round(sapply(data[, is_numeric], function(x) ks.test(x)$p.value), 4), NA)
    row_counter <- row_counter + 1
  }
  if (anderson) {
    is_numeric <- sapply(data, is.numeric)
    desc[row_counter, ] <- ifelse(is_numeric, round(sapply(data[, is_numeric], function(x) ad.test(x)), 4), NA)
    row_counter <- row_counter + 1
  }
  if (lilliefors) {
    is_numeric <- sapply(data, is.numeric)
    desc[row_counter, ] <- ifelse(is_numeric, round(sapply(data[, is_numeric], function(x) lillie.test(x)), 4), NA)
    row_counter <- row_counter + 1
  }

  if (jarque) {
    is_numeric <- sapply(data, is.numeric)
    desc[row_counter, ] <- ifelse(is_numeric, round(sapply(data[is_numeric], function(x) jarque.test(x)), 4), NA)
    row_counter <- row_counter + 1
  }
  
  rownames(desc) <- c(
    if (count) "Count",
    if (unique) "Unique",
    if (duplicate) "Duplicate",
    if (null) "Null",
    if (null_rate) "Null Rate",
    if (type) "Type",
    if (min) "Min",
    if (p25) "P25",
    if (mean) "Mean",
    if (median) "Median",
    if (p75) "P75",
    if (max) "Max",
    if (sd) "SD",
    if (kurtosis) "Kurtosis",
    if (skewness) "Skewness",
    if (shapiro) "Shapiro p-value",
    if (kolmogorov) "Kolmogorov p-value",
    if (anderson) "Anderson p-value",
    if (lilliefors) "Lilliefors p-value",
    if (jarque) "Jarque p-value"
  )
  desc_df <- as.data.frame(t(desc))
  
  table1 <- DT::datatable(
    desc_df,
    extensions = "Buttons",
    caption = paste(Dname, "Dataset Descriptive Statistics Summary With",
                    ncol(desc_df), "Features and",
                    nrow(desc_df), "Statistics"),
    options = list(
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf'),
      paging = TRUE,
      searching = FALSE,  
      ordering = TRUE,  
      scrollX = TRUE
      ),
    style = 'default',  
    class = 'table-striped table-bordered'
  )
  return (table1)
}

skewness <- function(x) {
  n <- length(x)
  mean_val <- mean(x,na.rm=T)
  std_dev <- sqrt(sum((x - mean_val)^2) / (n))
  Z <- (x-mean_val)/std_dev
  skewness <- sum(Z^3)/n
  return(skewness)
}


kurtosis <- function(x) {
  n <- length(x)
  mean_val <- mean(x,na.rm=T)
  std_dev <- sqrt(sum((x - mean_val)^2) / (n))
  Z <- (x-mean_val)/std_dev
  kurtosis <- sum(Z^4)/n
  return(kurtosis)
}

jarque.test <- function(x) {
  n <- length(x)
  skew <- skewness(x)
  kurt <- kurtosis(x)
  
  test_stat <- n/6 * (skew^2 + (kurt - 3)^2 / 4)
  p_value <- 1 - pchisq(test_stat, df = 2)
  
  return(p_value)
}

ad.test <- function(x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 5) warning("asymptotic distribution can be used for n greater than 5 for practical purposes")
  
  logp1 <- pnorm((x - mean(x)) / sd(x), log.p = TRUE)
  logp2 <- pnorm(-(x - mean(x)) / sd(x), log.p = TRUE)
  h <- (2 * seq_len(n) - 1) * (logp1 + rev(logp2))
  
  A <- -n - mean(h)
  AA <- (1 + 0.75/n + 2.25/n^2) * A
  
  p_value <- if (AA < 0.2) {
    1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
  } else if (AA < 0.34) {
    1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
  } else if (AA < 0.6) {
    exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
  } else if (AA < 10) {
    exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
  } else {
    3.7e-24
  }
  return(p_value)
}

lillie.test <- function(x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 5) warning("asymptotic distribution can be used for n greater than 5 for practical purposes")
  p <- pnorm((x - mean(x))/sd(x))
  Dplus <- max(seq(1:n)/n - p)
  Dminus <- max(p - (seq(1:n) - 1)/n)
  K <- max(Dplus, Dminus)
  if (n <= 100) {
    Kd <- K
    nd <- n
  } else {
    Kd <- K * ((n/100)^0.49)
    nd <- 100
  }
  p_value <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * 
                   Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) + 
                   1.67997/nd)
  if (p_value > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
    if (KK <= 0.302) {
      p_value <- 1
    } else if (KK <= 0.5) {
      p_value <- 2.76773 - 19.828315 * KK + 80.709644 * 
        KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
    } else if (KK <= 0.9) {
      p_value <- -4.901232 + 40.662806 * KK - 97.490286 * 
        KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
    } else if (KK <= 1.31) {
      p_value <- 6.198765 - 19.558097 * KK + 23.186922 * 
        KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
    } else {
      p_value <- 0
    }
  }
  return(p_value)
}




