#' Frequency table of categorical variables
#' @description Calculates and sort the count and relative frequency of categories.
#' @param data \code{\link{data.frame}} with categorical variables.
#' @param variables name or position of categorical variables. If more than one variable is provided, contingency frequencies are calculated.
#' @param rnd the number of decimal places (round) or significant digits (signif) to be used.
#' @param decreasing \code{\link{logical}}. If \code{TRUE}, frequencies will be sorted in decreasing order, if \code{FALSE}, they will be sorted in increasing order.
#' @param use.na \code{\link{logical}}. If \code{FALSE} (default), missing values are omitted.
#' @references Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' \url{http://oswaldosantos.github.io/capm}
#' @seealso \link[base]{table} and \link[base]{sort}.
#' @return \code{\link{data.frame}}.
#' @export
#' @examples
#' data(cluster_sample)
#' FreqTab(cluster_sample$number_of_dogs)
#' 
#' data(dogs)
#' FreqTab(dogs, c("species", "sex"))
#' 
FreqTab <- function(data = NULL, variables = NULL, rnd = 3, decreasing = TRUE, use.na = FALSE) {
  # Workaround to the "no visible binding for global variable" note.
  Count <- c()
  
  if (is.null(dim(data)) & is.null(variables)) {
    # This "if" is for compatibility with versions older than v0.12.14.
    variables <- data
    tmp <- as.data.frame(sort(round(table(variables), 3),
                              decreasing = decreasing))
    if (use.na) {
      tmp <- as.data.frame(sort(round(table(variables, useNA = "always"), 3),
                                decreasing = decreasing))
    }
    names(tmp) <- c("Category", "Count")
    tmp$Proportion <- round(tmp$Count / sum(tmp$Count), rnd)
    return(tmp)
  }
  if (is.numeric(variables)) {
    variables = names(data)[variables]
  }
  data <- select_(data, .dots = variables)
  if (!use.na) {
    data <- na.omit(data)
  }
  data <- data %>%
    group_by_(.dots = variables) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    mutate(Proportion = round(Count / sum(Count), rnd))
  if (decreasing) {
    return(arrange(data, desc(Count)))
  } else {
    arrange(data, Count)
  }
}