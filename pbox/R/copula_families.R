#' Define Copula Families and Parameters
#'
#' Internal list of defined copula families and their corresponding parameters.
#'
#' @name copula_families
#' @export
#'
#'
.copula_families <- list(
  # Archimedean copula families # "amh",
  archmCopula = c("clayton", "frank", "gumbel", "joe"),
  # Extreme-Value copula families #"tawn" #"tev"
  evCopula = c("galambos", "gumbel", "huslerReiss"),
  # Elliptical copula families # "t"
  ellipCopula = c("normal")
)

