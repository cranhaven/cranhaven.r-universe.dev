#' Plot the the inclusion of each variable for each MCMC iteration
#'
#' @param result Output object from mmvbvs function
#' @param title A string object for the title of the resulting plot
#' @return ggplot object
#' @example
#' set.seed(1001)
#' beta = c(rep(0.5, 3), rep(0,3))
#' n = 200; T = length(beta); nu = T+5
#' Sigma = matrix(0.8, T, T); diag(Sigma) = 1
#' X = as.numeric(scale(rnorm(n)))
#' error = MASS::mvrnorm(n, rep(0,T), Sigma)
#' gamma = c(rep(1,3), rep(0,3))
#' Y = X %*% t(beta) + error; Y = scale(Y)
#' Phi = matrix(0.5, T, T); diag(Phi) = 1
#' initial_chain = list(beta = rep(0,T),
#'                      gamma = rep(0,T),
#'                      Sigma = Phi,
#'                      sigmabeta = 1)
#' result = mmvbvs(X = X, Y = Y,initial_chain = initial_chain,Phi = Phi,
#'                 marcor = colMeans(X*Y, na.rm=TRUE), niter=10,verbose = FALSE)
#' plot_gamma(result, title = "gamma result")
#' @export
plot_gamma = function(result, title = "") {
    gamma = result$gamma
    nniter = nrow(gamma)
    colors <- c("lightblue", "darkblue")
    datgamma = as.data.frame(gamma[1:nniter, ])
    T = ncol(gamma)
    colnames(datgamma) = paste0("v", 1:T)
    rownames(datgamma) = paste0("iter", 1:nniter)
    datgamma$id = rownames(datgamma)
    datgamma$id = factor(datgamma$id, levels = datgamma$id)
    colnames(datgamma) = factor(colnames(datgamma), levels = colnames(datgamma))
    mdatgamma = reshape2::melt(datgamma, id = "id")
    colnames(mdatgamma)[3] = "gamma"
    mdatgamma$gamma = as.factor(mdatgamma$gamma)
    g = ggplot2::ggplot(mdatgamma, aes(x = .data$variable, y = .data$id, fill = .data$gamma)) + geom_tile(color = "white") +
        scale_fill_discrete() + ylab("iterations") + xlab("gamma") + ggtitle(title)
    # plot(g)
    return(g)
}

#' Plot the coefficients for each variable for each iteration of MCMC
#'
#' @param result Output object from mmvbvs function
#' @param title A string object for the title of the resulting plot
#' @return ggplot object
#' @example
#' set.seed(1001)
#' beta = c(rep(0.5, 3), rep(0,3))
#' n = 200; T = length(beta); nu = T+5
#' Sigma = matrix(0.8, T, T); diag(Sigma) = 1
#' X = as.numeric(scale(rnorm(n)))
#' error = MASS::mvrnorm(n, rep(0,T), Sigma)
#' gamma = c(rep(1,3), rep(0,3))
#' Y = X %*% t(beta) + error; Y = scale(Y)
#' Phi = matrix(0.5, T, T); diag(Phi) = 1
#' initial_chain = list(beta = rep(0,T),
#'                      gamma = rep(0,T),
#'                      Sigma = Phi,
#'                      sigmabeta = 1)
#' result = mmvbvs(X = X, Y = Y,initial_chain = initial_chain,Phi = Phi,
#'                 marcor = colMeans(X*Y, na.rm=TRUE), niter=10,verbose = FALSE)
#' plot_beta(result, title = "gamma result")
#' @export
plot_beta = function(result, title = "") {
    beta = result$beta
    T = ncol(beta)
    nniter = nrow(beta)
    datbeta = as.data.frame(beta[1:nniter, ])
    colnames(datbeta) = paste0("v", 1:T)
    rownames(datbeta) = paste0("iter", 1:nniter)
    datbeta$id = rownames(datbeta)
    datbeta$id = factor(datbeta$id, levels = datbeta$id)
    datbeta2 = reshape::melt(datbeta, id = "id")
    datbeta2$xx = rep(1:nniter, T)
    g = ggplot2::ggplot(datbeta2, aes(x = .data$xx, y = .data$value, col = .data$variable)) + ggplot2::geom_line(alpha = 0.7) +
        ggplot2::ylab("beta") + ggplot2::xlab("iteration") + ggplot2::ggtitle(title)
    plot(g)
    return(g)

}


#' Plot the posterior distribution of the coefficients
#'
#' @param result resulting object from mmvbvs function
#' @param title A string object for the title of the resulting plot
#' @return ggplot object
#' @example
#' set.seed(1001)
#' beta = c(rep(0.5, 3), rep(0,3))
#' n = 200; T = length(beta); nu = T+5
#' Sigma = matrix(0.8, T, T); diag(Sigma) = 1
#' X = as.numeric(scale(rnorm(n)))
#' error = MASS::mvrnorm(n, rep(0,T), Sigma)
#' gamma = c(rep(1,3), rep(0,3))
#' Y = X %*% t(beta) + error; Y = scale(Y)
#' Phi = matrix(0.5, T, T); diag(Phi) = 1
#' initial_chain = list(beta = rep(0,T),
#'                      gamma = rep(0,T),
#'                      Sigma = Phi,
#'                      sigmabeta = 1)
#' result = mmvbvs(X = X, Y = Y,initial_chain = initial_chain,Phi = Phi,
#'                 marcor = colMeans(X*Y, na.rm=TRUE), niter=10,verbose = FALSE)
#' beta_dist(result, title = "gamma result")
#' @export
beta_dist = function(result, title = "") {
    beta = result$beta[-1, ]
    T = ncol(beta)
    nniter = nrow(beta)
    datbeta = as.data.frame(beta[1:nniter, ])
    colnames(datbeta) = paste0("v", 1:T)
    rownames(datbeta) = paste0("iter", 1:nniter)
    datbeta$id = rownames(datbeta)
    datbeta$id = factor(datbeta$id, levels = datbeta$id)
    datbeta2 = reshape::melt(datbeta, id = "id")
    datbeta2$xx = rep(1:nniter, T)
    g = ggplot2::ggplot(datbeta2, aes(x = .data$variable, y = .data$value)) + ggplot2::geom_violin()
    # plot(g)
    return(g)
}

#' Plot the posterior mean of the covariance matrix
#'
#' @param result resulting object from mmvbvs function
#' @param title A string object for the title of the resulting plot
#' @return ggplot object#'
#' @example
#' set.seed(1001)
#' beta = c(rep(0.5, 3), rep(0,3))
#' n = 200; T = length(beta); nu = T+5
#' Sigma = matrix(0.8, T, T); diag(Sigma) = 1
#' X = as.numeric(scale(rnorm(n)))
#' error = MASS::mvrnorm(n, rep(0,T), Sigma)
#' gamma = c(rep(1,3), rep(0,3))
#' Y = X %*% t(beta) + error; Y = scale(Y)
#' Phi = matrix(0.5, T, T); diag(Phi) = 1
#' initial_chain = list(beta = rep(0,T),
#'                      gamma = rep(0,T),
#'                      Sigma = Phi,
#'                      sigmabeta = 1)
#' result = mmvbvs(X = X, Y = Y,initial_chain = initial_chain,Phi = Phi,
#'                 marcor = colMeans(X*Y, na.rm=TRUE), niter=10,verbose = FALSE)
#' plot_sigma(result, title = "gamma result")
#' @export
plot_sigma = function(result, title = "") {
    Sigma = apply(result$Sigma, c(1, 2), mean)
    T = nrow(Sigma)
    Sigma = as.data.frame(Sigma)
    colnames(Sigma) = paste0("v", 1:T)
    Sigma$id = paste0("v", 1:T)
    msig = reshape2::melt(Sigma, id = "id")
    msig$id = factor(msig$id, levels = paste0("v", 1:10))
    msig$variable = factor(msig$variable, levels = paste0("v", 1:10))
    g = ggplot2::ggplot(msig, aes(x = .data$id, y = .data$variable, fill = .data$value)) + ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(limits = c(0, 1.2)) + ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::geom_text(aes(label = round(.data$value, 2)))
    plot(g)
    return(g)
}
