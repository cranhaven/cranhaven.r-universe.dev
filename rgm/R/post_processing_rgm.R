#' Post-Processing for RGM (Random Graph Model)
#'
#' This function performs post-processing on simulated data and results from a Random Graph Model (RGM).
#' It calculates mean posterior estimates, compares true and estimated edge probabilities, generates
#' various diagnostic plots, and returns a list of these plots.
#'
#' @param simulated_data A list containing simulated data from an RGM.
#' @param results A list containing results from fitting an RGM to `simulated_data`.
#'
#' @return A list containing ggplot objects for different diagnostics:
#'   - `rgm_recovery`: A plot comparing true and estimated probit values.
#'   - `estimation_of_alpha`: A plot comparing true and estimated alpha values.
#'   - `posterior_distribution`: A density plot of the posterior distribution of the beta parameter.
#'   - `beta_convergence`: A trace plot of the beta parameter across MCMC iterations.
#'   - `roc_plot`: A ROC plot for graph recovery performance.
#'   - `edge_prob`: A heatmap of posterior edge probabilities for each environment.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline scale_color_manual
#' @importFrom ggplot2 labs theme_minimal theme element_blank element_text
#' @importFrom ggplot2 geom_density geom_vline geom_line theme_bw geom_hline
#' @importFrom ggplot2 geom_tile scale_fill_gradient2
#' @importFrom stats pnorm hclust dist order.dendrogram as.dendrogram
#' @importFrom pROC auc
#' @export
#'
post_processing_rgm <- function(simulated_data, results) {
  ## Data extraction from simulation object
  a = simulated_data
  res = results
  alpha.true <- a$alpha
  beta.true <- a$theta
  cloc.true <- a$loc
  G.true <- a$G
  data <- a$data
  X <- a$X

  ## Estimation
  iter <- ncol(res$sample.alpha)
  # Extracting samples after burnin
  burn <- floor(0.75 * iter)
  sample.graphs <- res$sample.graphs[, , -(1:burn)]
  sample.cloc <- res$sample.loc[, , -(1:burn)]
  sample.alpha <- res$sample.alpha[, -(1:burn)]
  sample.beta <- res$sample.theta[, -(1:burn)]
  post.pi <- res$sample.pi[, , -(1:burn)]
  probit.pi <- res$pi.probit[, , -(1:burn)]

  # Applying rotation of latent coordinates
  hlp <- array(apply(sample.cloc, 3, rot), dim = dim(sample.cloc))
  sample.cloc <- hlp

  # Mean posterior estimates of the parameters
  cloc.est <- apply(sample.cloc, c(1, 2), mean)
  alpha.est <- apply(sample.alpha, 1, mean)
  sample.beta <- t(as.matrix(sample.beta))
  beta.est <- apply(sample.beta, 1, mean)


  #Calculating the true edge probabilities (associated to the true graph and true parameters)
  Pi.true<-G.true
  dist.cond<-G.true
  B<-ncol(G.true)
  p<-ncol(data[[1]])
  m <- matrix(1:p, ncol = p, nrow = p)
  e1 <- t(m)[lower.tri(m)]
  e2 <- m[lower.tri(m)]
  for(b in 1:B)
  {
    dist.cond[,b]<-apply(G.true,1,function(g,cloc,b){crossprod(apply(cloc*g,2,sum)-cloc[b,]*g[b],cloc[b,])},cloc=cloc.true,b=b)
    for (i in 2:p){
      for (j in 1:(i-1)){
        ind<-e1==j & e2==i
        Pi.true[ind,b]<-pnorm(alpha.true[b]+dist.cond[ind,b]+X[ind,]%*%beta.true)
      }
    }}

  #Comparing the true edge probabilities with those estimated by the latent probit model, for each environment
  Pi.mean<-apply(probit.pi,c(1,2),mean)

  true_prob = est_prob = true_alpha = est_alpha = beta_value = value = label = iteration = specificity = sensitivity = Var1 = Var2 = NULL

  # Creating a data frame for ggplot
  data_to_plot <- data.frame(
    true_prob = c(Pi.true),
    est_prob = c(Pi.mean),
    environment = factor(rep(1:B, each = nrow(Pi.mean)))
  )

  # Define the colors for each environment
  col_vector <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#f032e6', '#fabebe', '#008080', '#000000', "#00FFFFFF", "#80FF00FF", "#FFFF00FF")
  col_vector <- col_vector[1:B]

  # Create the ggplot
  rgm_recovery = ggplot(data_to_plot, aes(x = true_prob, y = est_prob, color = environment)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, lwd = 3) +
    scale_color_manual(values = col_vector) +
    labs(x = "True Probit", y = "Estimated Probit", title = "Random Graph Model Recovery") +
    theme_minimal() +
    theme(legend.title = element_blank())


  #Comparing the true alphas with the estimated ones (i.e., checking whether the sparsity levels of the environment-specific graphs are correctly estimated)

  environments=1:B
  # Create a data frame for plotting
  data_to_plot <- data.frame(
    true_alpha = alpha.true,
    est_alpha = alpha.est,
    environment = factor(environments)  # Make sure this matches your data
  )

  # Create the ggplot
  estimation_of_alpha = ggplot(data_to_plot, aes(x = true_alpha, y = est_alpha, color = environment)) +
    geom_point(size = 4, shape = 15) +
    geom_abline(intercept = 0, slope = 1, lwd = 2) +
    scale_color_manual(values = col_vector) +
    labs(x = expression(paste("True ", alpha)),
         y = expression(paste("Posterior Mean ", alpha)),
         title = expression(paste("Estimation of ", alpha))) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.title = element_text(size = 14),
          axis.title = element_text(size = 12))

  beta_samples <- sample.beta[1, ]  # Extracting samples for the first beta parameter

  # Create a data frame for the density plot
  data_to_plot <- data.frame(beta_value = beta_samples)

  # Create a small data frame for the true and estimated lines
  lines_data <- data.frame(
    value = c(a$theta, beta.est[1]),
    label = c("True", "Estimated")
  )

  posterior_distribution = ggplot() +
    geom_density(data = data_to_plot, aes(x = beta_value), fill = "blue", alpha = 0.5) +
    geom_vline(data = lines_data, aes(xintercept = value, color = label), size = .5, linetype = "longdash") +
    scale_color_manual(values = c("True" = "darkgreen", "Estimation" = "red")) +
    labs(x = expression(beta),
         title = expression(paste(beta, " Posterior Distribution"))) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title = element_text(size = 12),
          legend.position = "top",
          legend.title = element_blank())

  df_p = data.frame(beta=res$sample.theta[1,], iteration=1:ncol(res$sample.theta))

  beta_convergence = ggplot(df_p) +
    geom_line(aes(x = iteration, y = beta)) +
    theme_bw() +
    geom_hline(yintercept = a$theta) +
    labs(title = "Trace plot of beta across MCMC Iterations")


  postpi.mean <- apply(post.pi, c(1, 2), mean)
  predictor <- postpi.mean
  response <- G.true

  roc_data <- lapply(1:B, function(j) {
    roc_obj <- pROC::roc(response = response[, j], predictor = predictor[, j], levels = c(0, 1), direction = "<")
    data.frame(
      specificity = 1 - roc_obj$specificities,
      sensitivity = roc_obj$sensitivities,
      environment = j
    )
  })

  roc_data <- do.call(rbind, roc_data)


  roc_plot = ggplot(roc_data, aes(x = specificity, y = sensitivity, color = as.factor(environment))) +
    geom_line(size = 1) +
    scale_color_manual(values = col_vector) +
    labs(title = "Graph Recovery", x = "Specificity", y = "Sensitivity", color = "Environment") +
    theme_minimal() +
    theme(legend.position = "right")

  #AUC values
  a<-NULL
  for(j in 1:B)
  {
    a[j]<-as.numeric(auc(response = response[,j], predictor = predictor[,j], levels = c(0, 1),direction="<"))
  }

  #Heatmap of posterior edge probabilities for each environment (blue 0, red 1)
  colnames(postpi.mean)<-as.character(1:B)
  dat.sm<-as.matrix(postpi.mean)

  # Perform hierarchical clustering on both rows (edges) and columns (environments)
  row_dend <- as.dendrogram(hclust(dist(dat.sm)))
  col_dend <- as.dendrogram(hclust(dist(t(dat.sm))))

  # Rearrange data according to the clustering
  ordered_dat.sm <- dat.sm[order.dendrogram(row_dend), order.dendrogram(col_dend)]

  # Reshape the matrix into a long format
  long_dat <- reshape2::melt(ordered_dat.sm)
  long_dat$Var1 <- as.factor(long_dat$Var1) # Environments
  long_dat$Var2 <- as.factor(long_dat$Var2) # Edges

  # Create the heatmap using ggplot2
  gg_heatmap <- ggplot(long_dat, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0.5, # Adjust based on your data range
                         limits = c(0, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),  # Remove x-axis text
          axis.text.y = element_text(angle = 45, hjust = 1)) +
    labs(x = "Edge", y = "Environment", fill = "Posterior Probability")


  # Return the list including the ggplot2 heatmap
  list(rgm_recovery = rgm_recovery,
       estimation_of_alpha = estimation_of_alpha,
       posterior_distribution = posterior_distribution,
       beta_convergence = beta_convergence,
       roc_plot = roc_plot,
       edge_prob = gg_heatmap)
}
