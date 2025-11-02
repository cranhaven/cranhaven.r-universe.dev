#' Unbalanced Bayesian Simulation in Conjugate Linear Model Framework
#'
#' Approximates the Bayesian assurance of attaining \eqn{u'\beta > C}
#' for unbalanced study designs through Monte Carlo sampling.
#' See Argument descriptions for more detail.
#' @importFrom plot3D mesh
#' @importFrom plotly plot_ly
#' @importFrom stats qnorm rnorm
#' @importFrom pbapply pbmapply
#' @import dplyr
#' @import mathjaxr
#' @param n1 first sample size (vector or scalar).
#' @param n2 second sample size (vector or scalar).
#' @param repeats an positive integer specifying number of times to repeat 
#' `c(n1, n2)`. Applicable for studies that consider multiple measures 
#' within each group. Default setting is `repeats = 1` if not applicable.
#' @param u a scalar or vector to evaluate \deqn{u'\beta > C,}
#' where \eqn{\beta} is an unknown parameter that is to be estimated.
#' Default setting is `u = 1`.
#' @param C constant value to be compared to when evaluating \eqn{u'\beta > C}
#' @param Xn design matrix that characterizes where the data is to be 
#' generated from. This is specifically designed under the normal linear 
#' regression model \deqn{yn = Xn\beta + \epsilon,}
#' \deqn{\epsilon ~ N(0, \sigma^2 Vn).} When set to `NULL`, 
#' `Xn` is generated in-function using `bayesassurance::gen_Xn()`. 
#' Note that setting `Xn = NULL` also enables user to pass in a
#' vector of sample sizes to undergo evaluation.
#' @param Vn a correlation matrix for the marginal distribution of the 
#' sample data `yn`. Takes on an identity matrix when set to `NULL`.
#' @param mu_beta_d design stage mean
#' @param mu_beta_a analysis stage mean
#' @param Vbeta_d correlation matrix that helps describe the prior 
#' information on \eqn{\beta} in the design stage
#' @param Vbeta_a_inv inverse-correlation matrix that helps describe the prior 
#' information on \eqn{\beta} in the analysis stage
#' @param sigsq a known and fixed constant preceding all correlation matrices 
#' `Vn`, `Vbeta_d` and `Vbeta_a_inv`.
#' @param alt specifies alternative test case, where alt = "greater" tests if 
#' \eqn{u'\beta > C},
#' alt = "less" tests if \eqn{u'\beta < C}, and alt = "two.sided" 
#' performs a two-sided test. By default, alt = "greater".
#' @param alpha significance level
#' @param mc_iter number of MC samples evaluated under the analysis objective
#' @param surface_plot when set to `TRUE` and \eqn{n1} and \eqn{n2} are vectors, 
#' a contour map showcasing various assurance values corresponding to 
#' different combinations of \eqn{n1} and \eqn{n2} is produced.
#' @return a list of objects corresponding to the assurance approximations
#' \itemize{
#'      \item{assurance_table:} table of sample size and corresponding assurance
#'      values
#'      \item{contourplot:} contour map of assurance values
#'      \item{mc_samples:} number of Monte Carlo samples that were generated
#'      and evaluated
#' }
#' @examples 
#' ## Example 1
#' ## Sample size vectors are passed in for n1 and n2 to evaluate
#' ## assurance. 
#' n1 <- seq(20, 75, 5)
#' n2 <- seq(50, 160, 10)
#' 
#' assur_out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, u = c(1, -1),
#' C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),nrow = 2, ncol = 2),
#' Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
#' Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
#' mu_beta_a = c(0, 0), alt = "two.sided", alpha = 0.05, mc_iter = 1000,
#' surface_plot = FALSE)
#' 
#' assur_out$assurance_table
#' 
#' 
#' ## Example 2
#' ## We can produce a contour plot that evaluates unique combinations of n1
#' ## and n2 simply by setting `surfaceplot = TRUE`.
#' \donttest{
#' n1 <- seq(20, 75, 5)
#' n2 <- seq(50, 160, 10)
#' assur_out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, 
#' u = c(1, -1), C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),
#' nrow = 2, ncol = 2), Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
#' Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
#' mu_beta_a = c(0, 0), alt = "two.sided", alpha = 0.05, mc_iter = 1000,
#' surface_plot = TRUE)
#'
#' assur_out$assurance_table
#' assur_out$contourplot
#' }
#' @export
#'
bayes_sim_unbalanced <- function(n1, n2, repeats = 1, u, C, Xn = NULL, Vn = NULL,
                       Vbeta_d, Vbeta_a_inv, sigsq, mu_beta_d, mu_beta_a, alt, 
                       alpha, mc_iter, surface_plot = TRUE){

  if(length(n1) != length(n2) | dim(as.matrix(n1))[1] != dim(as.matrix(n2))[1]){
    stop("n1 and n2 must be equal lengths.")
  }
    
  if((length(n1) < 2 | length(n2) < 2) & surface_plot == TRUE){
    surface_plot <- FALSE
    warning("Can only set surface_plot = TRUE if n1 and n2 are vectors.")
  }

  # will rely on this embedded function to generate data and
  # assess satisfaction of analysis objective for n1 and n2
  MC_samp <- function(n1 = n1, n2 = n2){
    n <- rep(c(n1, n2), repeats)
    print(c(n1, n2))
    if(is.null(Xn)){
      Xn <- bayesassurance::gen_Xn(n)
      Xn_t <- t(Xn)
    }
    if(is.null(Xn) == FALSE){
      Xn_t <- t(Xn)
    }

    if(is.null(Vn)){
      Vn <- diag(x = 1, nrow = nrow(Xn), ncol = nrow(Xn))
    }

    Vn_inv <- chol2inv(chol(Vn))

    count <- 0 # counter for iterations satisfying the analysis objective

    # Analysis Stage Begins
    L_tran <- chol(Vbeta_a_inv + t(Xn) %*% Vn_inv %*% Xn) 
    v <- backsolve(L_tran, Vn)
    M <- v %*% t(v)

    # Components that make up the marginal distribution from which
    # y is to be generated from
    V_star <- sigsq * (Xn %*% Vbeta_d %*% Xn_t + Vn)
    L <- t(chol(V_star))

    posterior_vals <- c()
    Zi_vals <- c()
    #print(c(n1, n2))

    for(i in 1:mc_iter){
      # Design Stage Begins
      z <- matrix(stats::rnorm(nrow(Xn), 0, 1), nrow(Xn), 1)
      yi <- Xn %*% mu_beta_d + L %*% z
      # Design Stage Ends

      # Analysis Stage
      m <- Vbeta_a_inv %*% mu_beta_a + Xn_t %*% Vn_inv %*% yi
      Mm <- M %*% m

      # evaluates satisfaction of analysis objective depending
      # on the specification of "alt"
      if(alt == "greater"){
        Zi <- ifelse((C - t(u) %*% Mm) / (sqrt(sigsq) * sqrt(t(u) %*% M %*% u))
                     < stats::qnorm(alpha), 1, 0)
        count <- ifelse(Zi == 1, count <- count + 1, count <- count)
      }else if(alt == "less"){
        Zi <- ifelse((C - t(u) %*% Mm) / (sqrt(sigsq) * sqrt(t(u) %*% M %*% u))
                     > stats::qnorm(1-alpha), 1, 0)
        count <- ifelse(Zi == 1, count <- count + 1, count <- count)
      }else if(alt == "two.sided"){
        Zi <- ifelse((C - t(u) %*% Mm) / (sqrt(sigsq) * sqrt(t(u) %*% M %*% u))
                     > stats::qnorm(1-alpha/2) | (C - t(u) %*% Mm) / 
                       (sqrt(sigsq) * sqrt(t(u) %*% M %*% u))
                     < stats::qnorm(alpha/2), 1, 0)
        count <- ifelse(Zi == 1, count <- count + 1, count <- count)
      }

      # Analysis Stage Ends
    }
    assurance <- count / mc_iter
    return(assurance)
  }

  # If surface plot is TRUE, loop through all possible combinations 
  # of n1 and n2 to determine the assurance at each location 
  # (this is done using plot3D::mesh)
  if(surface_plot == TRUE){

    Mesh_grid <- plot3D::mesh(unique(n1), unique(n2))
    assurance <- pbapply::pbmapply(MC_samp, n1 = Mesh_grid$x, n2 = Mesh_grid$y)
    assurance <- matrix(data=assurance, nrow=dim(Mesh_grid$x)[1], 
                        ncol=dim(Mesh_grid$x)[1], byrow = FALSE)

    assur_contourplot <- plotly::plot_ly(x = Mesh_grid$x[,1], 
      y = Mesh_grid$y[1,], z = ~assurance, 
      colorscale = list(c(0, 0.25, 0.5, 0.75, 1), 
      c('white', 'lightcyan', 'lightskyblue', 'steelblue', 'blue'))) %>%
      plotly::add_contour() %>% plotly::layout(title = "Assurance Contour Plot",
      xaxis = list(title = "n1"), yaxis = list(title = "n2"))

    assur_tab <- as.data.frame(cbind(n1, n2, diag(assurance)))
    colnames(assur_tab) <- c("n1", "n2", "Assurance")

    return(list(assurance_table = assur_tab, contourplot = assur_contourplot,
                mc_samples = mc_iter))

  }
  if(surface_plot == FALSE){
    assurance <- pbapply::pbmapply(MC_samp, n1, n2)
    assur_tab <- as.data.frame(cbind(n1, n2, assurance))
    return(list(assurance_table = assur_tab, mc_samples = mc_iter))
  }

}
