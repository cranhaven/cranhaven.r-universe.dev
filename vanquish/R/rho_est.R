#' @title Negative Log Likelihood
#' @description Calculates negative log likelihood for beta binomial distribution.
#'
#' @param x Depth of alternative allele
#' @param size Total depth
#' @param prob Theoretical probability for heterozygous is 0.5, for homozygous is 0.999
#' @param rho Rho parameter of Beta-Binomial distribution of alternative allele
#'
#' @importFrom VGAM dbetabinom
#'
negll <- function(x, size, prob, rho){
  -sum(dbetabinom(x, size, prob, rho, log = TRUE))
}

#' @title Estimate Rho for Alternative Allele Frequency
#' @description Estimates Rho parameter in beta binomial distribution for alternative allele frequency
#'
#' @param vl A list of vcf objects from read_vcf function.
#' @importFrom stats optim
#'
#' @return A list containing (1) het_rho: Rho parameter of heterozygous location; (2) hom_rho: Rho parameter homozygous location;
#' @export
#'
#' @examples
#' data("vcf_example")
#' vcf_list <- list()
#' vcf_list[[1]] <- vcf_example$VCF
#' res <- rho_est(vl = vcf_list)
#' res$het_rho[[1]]$par
#' res$hom_rho[[1]]$par
rho_est <- function(vl) {
  n <- length(vl)
  locinum <- lapply(X = vl, FUN = nrow)
  het_ac <- lapply(X = vl, FUN = function(x) {x$AC[x$ZG=='het']})
  hom_ac <- lapply(X = vl, FUN = function(x) {x$AC[x$ZG=='hom']})
  het_dp <- lapply(X = vl, FUN = function(x) {x$DP[x$ZG=='het']})
  hom_dp <- lapply(X = vl, FUN = function(x) {x$DP[x$ZG=='hom']})

  het_rho <- list()
  hom_rho <- list()

  for (i in 1:n) {
    het_objfn <- function(rho) {
      negll(x=het_ac[[i]], size=het_dp[[i]], prob=0.5, rho)
    }
    het_rho[[i]] <- optim(par = c(0.03),
                          fn = het_objfn,
                          method = "L-BFGS-B",
                          lower = 0,
                          upper = 0.999)
    hom_objfn <- function(rho) {
      negll(x=hom_ac[[i]], size=hom_dp[[i]], prob=0.999, rho)
    }
    hom_rho[[i]] <- optim(par = c(0.03),
                          fn = hom_objfn,
                          method = "L-BFGS-B",
                          lower = 0,
                          upper = 0.999)
  }
  res_list <- list("het_rho" = het_rho, "hom_rho" = hom_rho)
  return (res_list)
}
