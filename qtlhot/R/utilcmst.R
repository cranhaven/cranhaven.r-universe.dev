DropMissing <- function(cross, pheno.names)
{
  ## drop subjects with missing phenotype or covariate values
  as.vector(attr(stats::na.omit(cross$pheno[, pheno.names]), "na.action"))
}
##############################################################################
normal.trans <- function (x) 
{
  x <- rank(x, na.last = "keep")
  stats::qnorm(x/(1 + sum(!is.na(x))))
}

