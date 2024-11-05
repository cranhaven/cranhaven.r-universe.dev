#' Anova code a factor
#' 
#' Anova coding (also called deviation or simple coding) sets the grand mean as the intercept. 
#' Each contrast compares one level with the reference level (base).
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#' @param base the index of the level to use as baseline
#'
#' @return the factor with contrasts set
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog")), 
#'                  mu = c(10, 20), plot = FALSE)
#' df$pet <- contr_code_anova(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
#'                  mu = c(2, 4, 9), empirical = TRUE, plot = FALSE)
#'                  
#' df$pet <- contr_code_anova(df$pet, base = 1)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contr_code_anova(df$pet, base = 2)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contr_code_anova(df$pet, base = "ferret")
#' lm(y ~ pet, df) %>% broom::tidy()
contr_code_anova <- function(fct, levels = NULL, base = 1) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) levels <- levels(fct) %||% sort(unique(fct))
  fct <- factor(fct, levels)
  
  # create coding matrix
  n <- length(levels)
  if (!is.numeric(base)) base <- which(levels == base)
  tr_code <- stats::contr.treatment(n, base)
  my_code <- tr_code - 1/n
  
  # create column names
  colnames <- paste0(".", levels[-base], "-", levels[base])
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}


#' Sum code a factor
#' 
#' Sum coding sets the grand mean as the intercept. 
#' Each contrast compares one level with the grand mean.
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#' @param omit the level to omit (defaults to the last level)
#'
#' @return the factor with contrasts set
#'
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog", "bird", "ferret")), 
#'                  mu = c(2, 4, 9, 13), empirical = TRUE, plot = FALSE)
#' 
#' df$pet <- contr_code_sum(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contr_code_sum(df$pet, omit = "cat")
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contr_code_sum(df$pet, omit = 1)
#' lm(y ~ pet, df) %>% broom::tidy()
contr_code_sum <- function(fct, levels = NULL, omit = length(levels)) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) levels <- levels(fct) %||% sort(unique(fct))
  fct <- factor(fct, levels)
  
  # create coding matrix
  n <- length(levels)
  if (!is.numeric(omit)) omit <- which(levels == omit)
  my_code <- stats::contr.sum(n)
  if (n != omit) {
    reorder <- c(setdiff(1:n, omit), omit)
    reorder <- sapply(1:n, function(x) which(x == reorder))
    my_code <- my_code[reorder, , drop = FALSE]
  }
  
  # create column names
  colnames <- paste0(".", levels[-omit], "-intercept")
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}


#' Treatment code a factor
#' 
#' Treatment coding sets the mean of the reference level (base) as the intercept. 
#' Each contrast compares one level with the reference level.
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#' @param base the index of the level to use as baseline
#'
#' @return the factor with contrasts set
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog")), 
#'                  mu = c(10, 20), plot = FALSE)
#' df$pet <- contr_code_treatment(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
#'                  mu = c(2, 4, 9), empirical = TRUE, plot = FALSE)
#'                  
#' df$pet <- contr_code_treatment(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contr_code_treatment(df$pet, base = 2)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contr_code_treatment(df$pet, base = "ferret")
#' lm(y ~ pet, df) %>% broom::tidy()
contr_code_treatment <- function(fct, levels = NULL, base = 1) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) levels <- levels(fct) %||% sort(unique(fct))
  fct <- factor(fct, levels)
  
  # create coding matrix
  n <- length(levels)
  if (!is.numeric(base)) base <- which(levels == base)
  my_code <- stats::contr.treatment(n, base)
  
  # create column names
  colnames <- paste0(".", levels[-base], "-", levels[base])
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}



#' Helmert code a factor
#' 
#' Helmert coding sets the grand mean as the intercept. 
#' Each contrast compares one level with the mean of previous levels.
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#'
#' @return the factor with contrasts set
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog")), 
#'                  mu = c(10, 20), plot = FALSE)
#' df$pet <- contr_code_helmert(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
#'                  mu = c(2, 4, 9), empirical = TRUE, plot = FALSE)
#'                  
#' df$pet <- contr_code_helmert(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' # reorder the levels to change the comparisons
#' df$pet <- contr_code_helmert(df$pet, levels = c("dog", "cat", "ferret"))
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contr_code_helmert(df$pet, levels = c("ferret", "dog", "cat"))
#' lm(y ~ pet, df) %>% broom::tidy()
contr_code_helmert <- function(fct, levels = NULL) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) levels <- levels(fct) %||% sort(unique(fct))
  fct <- factor(fct, levels)
  
  # create coding matrix
  n <- length(levels)
  my_code <- stats::contr.helmert(n)
  for (i in 1:(n-1)) {
    my_code[, i] <- my_code[, i] / (i + 1)
  }
  
  # create column names
  comparison <- lapply(1:(n-1), function(x) {
    paste(levels[1:x], collapse = ".")
  })
  colnames <- paste0(".", levels[-1], "-", comparison)
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}


#' Polynomial code a factor
#' 
#' Polynomial coding sets the grand mean as the intercept. 
#' Each contrast tests a trend (linear, quadratic, cubic, etc.). This is only suitable for ordered factors.
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#'
#' @return the factor with contrasts set
#' @export
#'
#' @examples
#' df <- sim_design(within = list(time = 1:6), 
#'                  mu = 1:6 + (1:6-3.5)^2, 
#'                  long = TRUE, plot = FALSE)
#'                  
#' df$time <- contr_code_poly(df$time)
#' lm(y ~ time, df) %>% broom::tidy()
#' 
contr_code_poly <- function(fct, levels = NULL) {
  # make sure fct is an ordered factor with correct levels
  if (is.null(levels)) levels <- levels(fct) %||% sort(unique(fct))
  fct <- factor(fct, levels, ordered = TRUE)
  
  # create coding matrix
  n <- length(levels)
  my_code <- stats::contr.poly(n)
  
  # create column names
  colnames <- paste0("^", 1:(n-1))
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}


#' Difference code a factor
#' 
#' Difference coding sets the grand mean as the intercept. 
#' Each contrast compares one level with the previous level.
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#'
#' @return the factor with contrasts set
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
#'                  mu = c(2, 4, 9), empirical = TRUE, plot = FALSE)
#'                  
#' df$pet <- contr_code_difference(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
contr_code_difference <- function(fct, levels = NULL) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) levels <- levels(fct) %||% sort(unique(fct))
  fct <- factor(fct, levels)
  
  # create coding matrix
  n <- length(levels)
  dif_code <- .col(n:(n-1))
  upper.tri <- !lower.tri(dif_code)
  dif_code[upper.tri] <- dif_code[upper.tri] - n
  my_code <- dif_code / n
  
  # create column names
  colnames <- paste0(".", levels[2:n], "-", levels[1:(n-1)])
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}

#' Add a contrast to a data frame
#'
#' @param data the data frame
#' @param col the column to recode
#' @param contrast the contrast to recode to
#' @param levels the levels of the factor in order
#' @param ... arguments to pass to the contrast function (base or omit)
#' @param add_cols whether to just add the contrast to the existing column or also to create new explicit columns in the dataset (default)
#' @param colnames optional list of column names for the added contrasts
#'
#' @return the data frame with the recoded column and added columns (if add_cols == TRUE)
#' @export
#'
#' @examples
#' df <- sim_design(between = list(time = 1:6), plot = FALSE) %>%
#'    add_contrast("time", "poly")
#' 
#' # test all polynomial contrasts
#' lm(y ~ time, df) %>% broom::tidy()
#' 
#' # test only the linear and quadratic contrasts
#' lm(y ~ `time^1` + `time^2`, df) %>% broom::tidy()
add_contrast <- function(data, col, contrast = c("anova", "sum", "treatment", "helmert", "poly", "difference"), levels = NULL, ..., add_cols = TRUE, colnames = NULL) {
  fct <- data[[col]]
  if (is.null(fct)) stop("The column ", col, " doesn't exist", call. = FALSE)
  contrast <- match.arg(contrast)
  
  f <- match.fun(paste0("contr_code_", contrast))
  newfct <- f(fct, levels, ...)
  
  if (isTRUE(add_cols)) {
    contr <- stats::contrasts(newfct)
    colnames(contr) <- colnames %||% paste0(col, colnames(contr))
    contr <- dplyr::as_tibble(contr, rownames = col)
    suffix <- switch(contrast, 
                     anova = ".aov", 
                     sum = ".sum", 
                     treatment = ".tr", 
                     helmert = ".hmt", 
                     poly = ".poly", 
                     difference = ".dif")
    # match column types
    data[col] <- as.character(data[[col]])
    contr[col]<- as.character(contr[[col]])
    
    data <- dplyr::left_join(data, contr, by = col, suffix = c("", suffix))
  }
  
  # add col as factor after join, which removes factor from col
  data[col] <- newfct 
  
  data
}
