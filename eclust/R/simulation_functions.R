#' Fit Penalized Regression Models on Simulated Cluster Summaries
#'
#' @description This function creates summaries of the given clusters (e.g. 1st
#'   PC or average), and then fits a penalized regression model on those
#'   summaries. To be used with simulated data where the 'truth' is known i.e.,
#'   you know which features are associated with the response. This function was
#'   used to produce the simulation results in Bhatnagar et al. 2016. Can run
#'   lasso, elasticnet, SCAD or MCP models
#' @param x_train \code{ntrain x p} matrix of simulated training set where
#'   \code{ntrain} is the number of training observations  and \code{p} is total
#'   number of predictors. This matrix needs to have named columns representing
#'   the feature names or the gene names
#' @param x_test \code{ntest x p} matrix of simulated training set where
#'   \code{ntest} is the number of training observations  and \code{p} is total
#'   number of predictors. This matrix needs to have named columns representing
#'   the feature names or the gene names
#' @param y_train numeric vector of length \code{ntrain} representing the
#'   responses for the training subjects. If continuous then you must set
#'   \code{exp_family = "gaussion"}. For \code{exp_family="binomial"} should be
#'   either a factor with two levels, or a two-column matrix of counts or
#'   proportions (the second column is treated as the target class; for a
#'   factor, the last level in alphabetical order is the target class)
#' @param y_test numeric vector of length \code{ntest} representing the
#'   responses for the test subjects. If continuous then you must set
#'   \code{exp_family = "gaussion"}. For \code{exp_family="binomial"} should be
#'   either a factor with two levels, or a two-column matrix of counts or
#'   proportions (the second column is treated as the target class; for a
#'   factor, the last level in alphabetical order is the target class).
#' @param s0 chracter vector of the active feature names, i.e., the features in
#'   \code{x_train} that are truly associated with the response.
#' @param summary the summary of each cluster. Can be the principal component or
#'   average. Default is \code{summary = "pc"} which takes the first
#'   \code{number_pc} principal components. Currently a maximum of 2 principal
#'   components can be chosen.
#' @param model Regression model to be fit on cluster summaries. Default is
#'   \code{model="lasso"} which corresponds to glmnet mixing parameter
#'   \code{alpha=1}. \code{model="elasticnet"} corresponds to glmnet mixing
#'   parameter \code{alpha=0.5}, \code{model="mcp"} and \code{model="scad"} are
#'   the non-convex models from the \code{\link[ncvreg]{ncvreg}} package
#' @param exp_family Response type. See details for \code{y_train} argument
#'   above.
#' @param gene_groups data.frame that contains the group membership for each
#'   feature. The first column is called 'gene' and the second column should be
#'   called 'cluster'. The 'gene' column identifies the features and must be the
#'   same identifiers in the \code{x_train,x_test} matrices. The 'cluster'
#'   column is a numeric integer indicating the cluster group membership.  A
#'   cluster group membership of 0 implies the feature did not cluster into any
#'   group.
#' @param topgenes List of features to keep if \code{filter=TRUE}. Default is
#'   \code{topgenes = NULL} which means all features are kept for the analysis
#' @param stability Should stability measures be calculated. Default is
#'   \code{stability=FALSE}. See details
#' @param filter Should analysis be run on a subset of features. Default is
#'   \code{filter = FALSE}
#' @param include_E Should the environment variable be included in the
#'   regression analysis. Default is \code{include_E = TRUE}
#' @param include_interaction Should interaction effects between the features in
#'   \code{x_train} and the environment variable be fit. Default is
#'   \code{include_interaction=TRUE}
#' @param clust_type Method used to cluster the features. This is used for
#'   naming the output only and has no consequence for the results.
#'   \code{clust_type = "CLUST"} is the default which means that the environment
#'   varible was not used in the clustering step. \code{clust_type = "ECLUST"}
#'   means that the environment variable was used in the clustering aspect.
#' @param number_pc Number of principal components if \code{summary = "pc"}.
#'   Default is \code{number_pc = 1}. Can be either 1 or 2.
#' @details The stability of feature importance is defined as the variability of
#'   feature weights under perturbations of the training set, i.e., small
#'   modifications in the training set should not lead to considerable changes
#'   in the set of important covariates (Toloşi, L., & Lengauer, T. (2011)). A
#'   feature selection algorithm produces a weight, a ranking, and a subset of
#'   features. In the CLUST and ECLUST methods, we defined a predictor to be
#'   non-zero if its corresponding cluster representative weight was non-zero.
#'   Using 10-fold cross validation (CV), we evaluated the similarity between
#'   two features and their rankings using Pearson and Spearman correlation,
#'   respectively. For each CV fold we re-ran the models and took the average
#'   Pearson/Spearman correlation of the 10 choose 2 combinations of estimated
#'   coefficients vectors. To measure the similarity between two subsets of
#'   features we took the average of the Jaccard distance in each fold. A
#'   Jaccard distance of 1 indicates perfect agreement between two sets while no
#'   agreement will result in a distance of 0.
#' @references Toloşi, L., & Lengauer, T. (2011). \emph{Classification with
#'   correlated features: unreliability of feature ranking and solutions.
#'   Bioinformatics, 27(14), 1986-1994.}
#' @references Bhatnagar, SR., Yang, Y., Blanchette, M., Bouchard, L.,
#'   Khundrakpam, B., Evans, A., Greenwood, CMT. (2016+). \emph{An analytic
#'   approach for interpretable predictive models in high dimensional data, in
#'   the presence of interactions with exposures
#'   \href{http://sahirbhatnagar.com/slides/manuscript1_SB_v4.pdf}{Preprint}}
#' @references Langfelder, P., Zhang, B., & Horvath, S. (2008). \emph{Defining
#'   clusters from a hierarchical cluster tree: the Dynamic Tree Cut package for
#'   R. Bioinformatics, 24(5), 719-720.}
#' @references Friedman, J., Hastie, T. and Tibshirani, R. (2008)
#'   \emph{Regularization Paths for Generalized Linear Models via Coordinate
#'   Descent, \url{http://www.stanford.edu/~hastie/Papers/glmnet.pdf}}
#' @references Breheny, P. and Huang, J. (2011) \emph{Coordinate descent
#'   algorithms for nonconvex penalized regression, with applications to
#'   biological feature selection. Ann. Appl. Statist., 5: 232-253.}
#' @note \code{number_pc=2} will not work if there is only one feature in an
#'   estimated cluster
#' @importFrom pacman p_load
#' @import data.table
#' @import magrittr
#' @return This function has two different outputs depending on whether
#'   \code{stability = TRUE} or \code{stability = FALSE}
#'
#'   If \code{stability = TRUE} then this function returns a \code{p x 2}
#'   data.frame or data.table of regression coefficients without the intercept.
#'   The output of this is used for subsequent calculations of stability.
#'
#'   If \code{stability = FALSE} then returns a vector with the following
#'   elements (See Table 3: Measures of Performance in Bhatnagar et al (2016+)
#'   for definitions of each measure of performance): \item{mse or AUC}{Test set
#'   mean squared error if \code{exp_family = "gaussion"} or test set Area under
#'   the curve if \code{exp_family = "binomial"} calculated using the
#'   \code{\link[pROC]{roc}} function} \item{RMSE}{Square root of the mse. Only
#'   applicable if \code{exp_family = "gaussion"}} \item{Shat}{Number of
#'   non-zero estimated regression coefficients. The non-zero estimated
#'   regression coefficients are referred to as being selected by the model}
#'   \item{TPR}{true positive rate} \item{FPR}{false positive rate}
#'   \item{Correct Sparsity}{Correct true positives + correct true negative
#'   coefficients divided by the total number of features}
#'   \item{CorrectZeroMain}{Proportion of correct true negative main effects}
#'   \item{CorrectZeroInter}{Proportion of correct true negative interactions}
#'   \item{IncorrectZeroMain}{Proportion of incorrect true negative main
#'   effects} \item{IncorrectZeroInter}{Proportion of incorrect true negative
#'   interaction effects} \item{nclusters}{number of estimated clusters by the
#'   \code{\link[dynamicTreeCut]{cutreeDynamic}} function}
#'
#' @examples
#' library(magrittr)
#'
#' # simulation parameters
#' rho = 0.90; p = 500 ;SNR = 1 ; n = 200; n0 = n1 = 100 ; nActive = p*0.10 ; cluster_distance = "tom";
#' Ecluster_distance = "difftom"; rhoOther = 0.6; betaMean = 2;
#' alphaMean = 1; betaE = 3; distanceMethod = "euclidean"; clustMethod = "hclust";
#' cutMethod = "dynamic"; agglomerationMethod = "average"
#'
#' #in this simulation its blocks 3 and 4 that are important
#' #leaveOut:  optional specification of modules that should be left out
#' #of the simulation, that is their genes will be simulated as unrelated
#' #("grey"). This can be useful when simulating several sets, in some which a module
#' #is present while in others it is absent.
#' d0 <- s_modules(n = n0, p = p, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = n1, p = p, rho = rho, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' truemodule1 <- d1$setLabels
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'   magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'   magrittr::set_rownames(paste0("Subject",1:n))
#'
#' betaMainEffect <- vector("double", length = p)
#' betaMainInteractions <- vector("double", length = p)
#'
#' # the first nActive/2 in the 3rd block are active
#' betaMainEffect[which(truemodule1 %in% 3)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean - 0.1, betaMean + 0.1)
#'
#' # the first nActive/2 in the 4th block are active
#' betaMainEffect[which(truemodule1 %in% 4)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean+2 - 0.1, betaMean+2 + 0.1)
#' betaMainInteractions[which(betaMainEffect!=0)] <- runif(nActive, alphaMean - 0.1, alphaMean + 0.1)
#' beta <- c(betaMainEffect, betaE, betaMainInteractions)
#'
#' result <- s_generate_data(p = p, X = X,
#'                           beta = beta,
#'                           include_interaction = TRUE,
#'                           cluster_distance = cluster_distance,
#'                           n = n, n0 = n0,
#'                           eclust_distance = Ecluster_distance,
#'                           signal_to_noise_ratio = SNR,
#'                           distance_method = distanceMethod,
#'                           cluster_method = clustMethod,
#'                           cut_method = cutMethod,
#'                           agglomeration_method = agglomerationMethod,
#'                           nPC = 1)
#'
#' pen_res <- s_pen_clust(x_train = result[["X_train"]],
#'                        x_test = result[["X_test"]],
#'                        y_train = result[["Y_train"]],
#'                        y_test = result[["Y_test"]],
#'                        s0 = result[["S0"]],
#'                        gene_groups = result[["clustersAddon"]],
#'                        summary = "pc",
#'                        model = "lasso",
#'                        exp_family = "gaussian",
#'                        clust_type = "ECLUST",
#'                        include_interaction = TRUE)
#' unlist(pen_res)
#'
#'
#' @export
s_pen_clust <- function(x_train,
                        x_test,
                        y_train,
                        y_test,
                        s0,
                        gene_groups,
                        summary = c("pc","avg"),
                        model = c("lasso", "elasticnet", "scad", "mcp"),
                        exp_family = c("gaussian","binomial"),
                        filter = F,
                        topgenes = NULL,
                        stability = F,
                        include_E = T,
                        include_interaction = F,
                        clust_type = c("CLUST","ECLUST"),
                        number_pc = 1) {

  # result[["clustersAddon"]] %>% print(nrows=Inf)
  # result[["clustersAddon"]][, table(cluster, module)]
  # result %>% names
  # stability = F; gene_groups = result[["clustersAddon"]];
  # x_train = result[["X_train"]] ; x_test = result[["X_test"]];
  # y_train = result[["Y_train"]] ; y_test = result[["Y_test"]];
  # dim(x_train)
  # filter = F; filter_var = F; include_E = T; include_interaction = F;
  # s0 = result[["S0"]]; p = p ;true_beta = result[["beta_truth"]]
  # model = "lasso"; summary = "pc"; topgenes = NULL; clust_type="clust"; number_pc = 1
  # exp_family="binomial"

  coef.est = cluster = gene = NULL
  clust_type <- match.arg(clust_type)
  summary <- match.arg(summary)
  model <- match.arg(model)
  exp_family <- match.arg(exp_family)

  if (exp_family == "binomial") {
    pacman::p_load(char = "pROC")
  }

  message(sprintf("Summary measure: %s, Model: %s, Cluster Type: %s",
                  summary, model, clust_type))

  if (include_E == F & include_interaction == T) stop("include_E needs to be
                                                      TRUE if you want to include
                                                      interactions")

  if (is.null(topgenes) & filter == T) stop("Argument topgenes is missing but
                                            filter is TRUE. You need to provide
                                            a filtered list of genes if filter
                                            is TRUE")

  # train data which includes the relevant (filtered or not filtered genes
  # and E or not E)
  x_train_mod <- if (filter & !include_E) {
    x_train[, topgenes] %>% as.data.frame
  } else if (!filter & include_E) {
    x_train %>% as.data.frame
  } else if (!filter & !include_E) {
    x_train[,which(colnames(x_train) %ni% "E")] %>% as.data.frame
  } else if (filter & include_E) {
    x_train[, c(topgenes,"E")] %>% as.data.frame
  }

  # test data
  x_test_mod = if (filter & !include_E) {
    x_test[, topgenes] %>% as.data.frame
  } else if (!filter & include_E) {
    x_test %>% as.data.frame
  } else if (!filter & !include_E) {
    x_test[,which(colnames(x_test) %ni% "E")] %>% as.data.frame
  } else if (filter & include_E) {
    x_test[, c(topgenes,"E")] %>% as.data.frame
  }

  # these are only derived on the main effects genes.. E is only included in the model
  PC_and_avg <- u_extract_summary(x_train = x_train_mod[,gene_groups$gene],
                          colors = gene_groups$cluster,
                          x_test = x_test_mod[,gene_groups$gene],
                          nPC = number_pc)

  n.clusters <- PC_and_avg$nclusters

  # this contains either the averages or PCs for each module in a data.frame
  clust_data <- switch(summary,
                       avg = PC_and_avg$averageExpr,
                       pc = PC_and_avg$PC)

  ml.formula <- if (include_interaction & include_E) {
    stats::as.formula(paste0("y_train ~","(",paste0(colnames(clust_data), collapse = "+"),")*E"))
  } else if (!include_interaction & include_E) {
    stats::as.formula(paste0("y_train ~",paste0(colnames(clust_data), collapse = "+"),"+E"))
  } else if (!include_interaction & !include_E) {
    stats::as.formula(paste0("y_train ~",paste0(colnames(clust_data), collapse = "+")))
  }

  # this is the same as ml.formula, except without the response.. this is used for
  # functions that have the x = and y = input instead of a formula input
  model.formula <- if (include_interaction & include_E) {
    stats::as.formula(paste0("~ 0+(",paste0(colnames(clust_data), collapse = "+"),")*E"))
  } else if (!include_interaction & include_E) {
    stats::as.formula(paste0("~0+",paste0(colnames(clust_data), collapse = "+"),"+E"))
  } else if (!include_interaction & !include_E) {
    stats::as.formula(paste0("~0+",paste0(colnames(clust_data), collapse = "+")))
  }

  # this is the design matrix based on model.formula
  X.model.formula <- stats::model.matrix(model.formula, data = if (include_E) {
    cbind(clust_data,x_train_mod[,"E", drop = F])
  } else clust_data %>% as.data.frame)

  df <- X.model.formula %>% cbind(y_train) %>% as.data.frame()

  clust_train_model <- switch(model,
                              lasso = {if (n.clusters != 1) {
                                pacman::p_load(char = "glmnet")
                                glmnet::cv.glmnet(x = X.model.formula, y = y_train, alpha = 1, family = exp_family)
                              } else NA },
                              elasticnet = {if (n.clusters != 1) {
                                pacman::p_load(char = "glmnet")
                                glmnet::cv.glmnet(x = X.model.formula, y = y_train, alpha = 0.5, family = exp_family)
                              } else NA },
                              scad = {
                                pacman::p_load(char = "ncvreg")
                                ncvreg::cv.ncvreg(X = X.model.formula, y = y_train,
                                          family = "gaussian", penalty = "SCAD")
                              },
                              mcp = {
                                pacman::p_load(char = "ncvreg")
                                ncvreg::cv.ncvreg(X = X.model.formula, y = y_train,
                                          family = "gaussian", penalty = "MCP")
                              }
                              # ,
                              # shim = {
                              #   cv.shim(x = X.model.formula, y = y_train,
                              #           main.effect.names = c(colnames(clust_data), if (include_E) "E"),
                              #           interaction.names = setdiff(colnames(X.model.formula),c(colnames(clust_data),"E")),
                              #           max.iter = 120, initialization.type = "ridge",
                              #           verbose = FALSE, parallel = TRUE, nfolds = 10)
                              # }
                              )
  # plot(clust_train_model)

  # here we give the coefficient stability on the clusters and not the individual genes
  coefs <- switch(model,
                  lasso = {
                    # need to return all 0's if there is only 1 cluster since lasso
                    # wont run with only 1 predictor
                    dat <- data.table::data.table(Gene = colnames(X.model.formula),
                                                  coef.est = rep(0, ncol(X.model.formula)))
                    if (n.clusters != 1) {
                      stats::coef(clust_train_model, s = "lambda.min") %>%
                        as.matrix %>%
                        data.table::as.data.table(keep.rownames = TRUE) %>%
                        magrittr::set_colnames(c("Gene","coef.est"))
                    } else dat
                  },
                  elasticnet = {
                    # need to return all 0's if there is only 1 cluster since lasso
                    # wont run with only 1 predictor
                    dat <- data.table::data.table(Gene = colnames(X.model.formula),
                                                  coef.est = rep(0, ncol(X.model.formula)))
                    if (n.clusters != 1) {
                      stats::coef(clust_train_model, s = "lambda.min") %>%
                        as.matrix %>%
                        data.table::as.data.table(keep.rownames = TRUE) %>%
                        magrittr::set_colnames(c("Gene","coef.est"))
                    } else dat
                  },
                  scad = {
                    # need to return all 0's if there is only 1 cluster since lasso
                    # wont run with only 1 predictor
                    dat <- data.table::data.table(Gene = colnames(X.model.formula),
                                                  coef.est = rep(0, ncol(X.model.formula)))
                    if (n.clusters != 1) {

                      stats::coef(clust_train_model, lambda = clust_train_model$lambda.min) %>%
                        as.matrix %>%
                        data.table::as.data.table(keep.rownames = TRUE) %>%
                        magrittr::set_colnames(c("Gene","coef.est"))
                    } else dat
                  },
                  mcp = {
                    # need to return all 0's if there is only 1 cluster since lasso
                    # wont run with only 1 predictor
                    dat <- data.table::data.table(Gene = colnames(X.model.formula),
                                                  coef.est = rep(0, ncol(X.model.formula)))
                    if (n.clusters != 1) {

                      stats::coef(clust_train_model, lambda = clust_train_model$lambda.min) %>%
                        as.matrix %>%
                        data.table::as.data.table(keep.rownames = TRUE) %>%
                        magrittr::set_colnames(c("Gene","coef.est"))
                    } else dat
                  }
                  # ,
                  # shim = {
                  #   dat <- data.table::data.table(Gene = colnames(X.model.formula),
                  #                                 coef.est = rep(0, ncol(X.model.formula)))
                  #   if (n.clusters != 1) {
                  #
                  #     coef(clust_train_model, s = "lambda.min") %>%
                  #       as.matrix %>%
                  #       as.data.table(keep.rownames = TRUE) %>%
                  #       magrittr::set_colnames(c("Gene","coef.est"))
                  #   } else dat
                  # }
                  )

  if (stability) {
    # remove intercept for stability measures
    return(coefs %>% magrittr::extract(-1, , drop = F))
  } else {

    non_zero_clusters <- coefs[-1, , ][coef.est != 0] %>%
      magrittr::use_series("Gene")

    # need to determine which of non_zero_cluters are main effects and which
    # are interactions
    non_zero_clusters_interactions <- grep(":",non_zero_clusters, value = T)

    # this checks if the environment is non-zero
    non_zero_environment <- grep("^E", non_zero_clusters, value = T,
                                 ignore.case = TRUE)
    non_zero_clusters_main_effects <- setdiff(non_zero_clusters,
                                              c(non_zero_clusters_interactions,
                                                non_zero_environment))

    # this includes the environment if the environment is non-zero
    n.non_zero_clusters <- coefs[-1, , ][coef.est != 0] %>%
      magrittr::use_series("Gene") %>%
      length

    # need to get the genes corresponding to the non-zero clusters
    # NOTE: this also includes non-zero cluster:Environment interactions

    # genes corresponding to non-zero main effect clusters
    # this list might not be unique if clust_type="Addon" because the same gene
    # can be in different clusters
    clust.S.hat.main <- gene_groups[cluster %in%
                                      as.numeric(
                                        unlist(
                                          stringr::str_extract_all(
                                            non_zero_clusters_main_effects, "(\\d+)$")
                                        )
                                      ),gene]

    # identical(gene_groups[cluster %in% c(3,12),gene],
    #           clust.S.hat.main)
    # identical(unique(clust.S.hat.main), clust.S.hat.main)
    # table(clust.S.hat.main)

    # this is the same as gene_groups, but the gene names contain E
    # so that we can extract the interactions corresponding to the chose clusters
    gene_groups_E <- copy(gene_groups)
    gene_groups_E[,gene:=paste0(gene,":E")]

    clust.S.hat.interaction <- gene_groups_E[cluster %in%
                                               as.numeric(
                                                 unlist(
                                                   stringr::str_extract_all(
                                                     stringr::str_extract_all(non_zero_clusters_interactions,"^.*?(?=:)"),
                                                     "(\\d+)$")
                                                 )
                                               ),gene]

    # this represents all the genes corresponding to the non-zero PC or avg
    # this list might not be unique if clust_type="Addon"
    # identical(unique(clust.S.hat), clust.S.hat)
    # I will double count if a model takes a gene more than once. ie.
    # if the same gene gets selected twice, then this will contribute 2 to the
    # number of non-zero estimated coefficients
    clust.S.hat <- c(clust.S.hat.main, non_zero_environment,
                     clust.S.hat.interaction)


    clust_data_test <- switch(summary,
                              avg = PC_and_avg$averageExprTest,
                              pc = PC_and_avg$PCTest)

    # need intercept for prediction
    model.formula_test <- if (include_interaction & include_E) {
      stats::as.formula(paste0("~ 1+(",paste0(colnames(clust_data_test), collapse = "+"),")*E"))
    } else if (!include_interaction & include_E) {
      stats::as.formula(paste0("~1+",paste0(colnames(clust_data_test), collapse = "+"),"+E"))
    } else if (!include_interaction & !include_E) {
      stats::as.formula(paste0("~1+",paste0(colnames(clust_data_test), collapse = "+")))
    }


    # this includes the intercept!
    X.model.formula_test <- stats::model.matrix(model.formula_test,
                                         data = if (include_E) {
                                           cbind(clust_data_test,x_test_mod[,"E", drop = F])
                                         } else clust_data_test %>% as.data.frame)

    # True Positive Rate
    clust.TPR <- length(intersect(clust.S.hat, s0))/length(s0)

    # True negatives
    trueNegs <- setdiff(colnames(x_train_mod), s0)
    # identical(setdiff(colnames(x_train_mod), s0), setdiff(colnames(x_train), s0))

    # these are the terms which the model identified as zero
    modelIdentifyZero <- setdiff(colnames(x_train_mod),clust.S.hat)

    # how many of the terms identified by the model as zero, were actually zero
    # use to calculate correct sparsity as defined by Witten et al in the
    # Cluster Elastic Net paper Technometrics 2013
    C1 <- sum(modelIdentifyZero %in% trueNegs)
    C2 <- length(intersect(clust.S.hat, s0))
    clust.correct_sparsity <- (C1 + C2)/(ncol(x_train_mod))

    # this is from Interaction Screening for Ultrahigh Dimensional Data by ning hao and hao helen zhang
    true.interaction_names <- grep(":", s0, value = T)
    true.main_effect_names <- setdiff(s0, true.interaction_names)

    all.interaction_names <- grep(":", colnames(x_train_mod), value = T)
    all.main_effect_names <- setdiff(colnames(x_train_mod), all.interaction_names)

    true.negative_main_effects <- setdiff(all.main_effect_names, true.main_effect_names)
    true.negative_interaction_effects <- setdiff(all.interaction_names, true.interaction_names)

    (clust.correct_zeros_main_effects <- sum(setdiff(all.main_effect_names, c(clust.S.hat.main, non_zero_environment)) %in% true.negative_main_effects)/ length(true.negative_main_effects))
    (clust.correct_zeros_interaction_effects <- sum(setdiff(all.interaction_names, clust.S.hat.interaction) %in% true.negative_interaction_effects)/ length(true.negative_interaction_effects))

    (clust.incorrect_zeros_main_effects <- sum(setdiff(all.main_effect_names, c(clust.S.hat.main, non_zero_environment)) %in% true.main_effect_names)/ length(true.main_effect_names))
    (clust.incorrect_zeros_interaction_effects <- sum(setdiff(all.interaction_names, clust.S.hat.interaction) %in% true.interaction_names)/ length(true.interaction_names))

    # False Positive Rate = FP/(FP + TN) = FP / True number of 0 coefficients
    (clust.FPR <- sum(clust.S.hat %ni% s0)/(sum(clust.S.hat %ni% s0) + sum(modelIdentifyZero %in% trueNegs)))

    # Mean Squared Error
    (clust.mse <- crossprod(X.model.formula_test %*% coefs$coef.est - y_test)/length(y_test))


    if (exp_family == "binomial") {
      pred_response <- stats::predict(clust_train_model, newx = X.model.formula_test[,-which(colnames(X.model.formula_test)=="(Intercept)")],
                               type = "response", s = "lambda.min")

      clust.AUC <- pROC::roc(y_test,as.numeric(pred_response))$auc %>% as.numeric()

    }

    # Root Mean Squared Error
    (clust.RMSE <- sqrt(crossprod(X.model.formula_test %*% coefs$coef.est - y_test)/length(y_test)))

    # remove intercept for prediction error formula given by ||X\beta - X\hat{\beta}||_2
    # given in Witten 2013 Cluster ENET paper in Technometrics
    # (clust.test_set_pred_error <- sqrt(crossprod(as.matrix(x_test_mod) %*% as.numeric(true_beta) - X.model.formula_test[,-1] %*% coefs$coef.est[-1])))

    # mse.null
    (mse_null <- crossprod(mean(y_test) - y_test)/length(y_test))

    # the proportional decrease in model error or R^2 for each scenario (pg. 346 ESLv10)
    # clust.r2 <- (mse_null - clust.mse)/mse_null

    # clust.adj.r2 <- 1 - (1 - clust.r2)*(nrow(x_test) - 1)/(nrow(x_test) - n.non_zero_clusters - 1)


    ls <- if (exp_family=="binomial") {
      list(clust.mse = clust.mse,
           clust.RMSE,
           clust.AUC = clust.AUC,
           clust.S.hat = length(clust.S.hat),
           clust.TPR = clust.TPR,
           clust.FPR = clust.FPR,
           clust.correct_sparsity = clust.correct_sparsity,
           clust.correct_zeros_main_effects,
           clust.correct_zeros_interaction_effects,
           clust.incorrect_zeros_main_effects,
           clust.incorrect_zeros_interaction_effects,
           n.clusters) } else if (exp_family=="gaussian") {
             list(clust.mse = clust.mse,
                  clust.RMSE,
                  clust.S.hat = length(clust.S.hat),
                  clust.TPR = clust.TPR,
                  clust.FPR = clust.FPR,
                  clust.correct_sparsity = clust.correct_sparsity,
                  clust.correct_zeros_main_effects,
                  clust.correct_zeros_interaction_effects,
                  clust.incorrect_zeros_main_effects,
                  clust.incorrect_zeros_interaction_effects,
                  n.clusters)
           }

    if (exp_family=="binomial") {

      names(ls) <- c(paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_mse"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_RMSE"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_AUC"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_Shat"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_TPR"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_FPR"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectSparsity"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroMain"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroInter"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroMain"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroInter"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_nclusters")
      )

    } else if (exp_family=="gaussian") {
      names(ls) <- c(paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_mse"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_RMSE"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_Shat"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_TPR"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_FPR"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectSparsity"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroMain"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroInter"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroMain"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroInter"),
                     paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_nclusters")
      )
    }

    return(ls)

  }
}




#' Fit Penalized Regression Models on Simulated Data
#'
#' @description This function can run penalized regression models on the
#'   untransformed design matrix. To be used with simulated data where the
#'   'truth' is known i.e., you know which features are associated with the
#'   response. This function was used to produce the simulation results in
#'   Bhatnagar et al. 2016. Can run lasso, elasticnet, SCAD or MCP models
#'
#' @inheritParams s_pen_clust
#' @return This function has two different outputs depending on whether
#'   \code{stability = TRUE} or \code{stability = FALSE}
#'
#'   If \code{stability = TRUE} then this function returns a \code{p x 2}
#'   data.frame or data.table of regression coefficients without the intercept.
#'   The output of this is used for subsequent calculations of stability.
#'
#'   If \code{stability = FALSE} then returns a vector with the following
#'   elements (See Table 3: Measures of Performance in Bhatnagar et al (2016+)
#'   for definitions of each measure of performance): \item{mse or AUC}{Test set
#'   mean squared error if \code{exp_family = "gaussion"} or test set Area under
#'   the curve if \code{exp_family = "binomial"} calculated using the
#'   \code{\link[pROC]{roc}} function} \item{RMSE}{Square root of the mse. Only
#'   applicable if \code{exp_family = "gaussion"}} \item{Shat}{Number of
#'   non-zero estimated regression coefficients. The non-zero estimated
#'   regression coefficients are referred to as being selected by the model}
#'   \item{TPR}{true positive rate} \item{FPR}{false positive rate}
#'   \item{Correct Sparsity}{Correct true positives + correct true negative
#'   coefficients divided by the total number of features}
#'   \item{CorrectZeroMain}{Proportion of correct true negative main effects}
#'   \item{CorrectZeroInter}{Proportion of correct true negative interactions}
#'   \item{IncorrectZeroMain}{Proportion of incorrect true negative main
#'   effects} \item{IncorrectZeroInter}{Proportion of incorrect true negative
#'   interaction effects}
#' @details The stability of feature importance is defined as the variability of
#'   feature weights under perturbations of the training set, i.e., small
#'   modifications in the training set should not lead to considerable changes
#'   in the set of important covariates (Toloşi, L., & Lengauer, T. (2011)). A
#'   feature selection algorithm produces a weight, a ranking, and a subset of
#'   features. In the CLUST and ECLUST methods, we defined a predictor to be
#'   non-zero if its corresponding cluster representative weight was non-zero.
#'   Using 10-fold cross validation (CV), we evaluated the similarity between
#'   two features and their rankings using Pearson and Spearman correlation,
#'   respectively. For each CV fold we re-ran the models and took the average
#'   Pearson/Spearman correlation of the 10 choose 2 combinations of estimated
#'   coefficients vectors. To measure the similarity between two subsets of
#'   features we took the average of the Jaccard distance in each fold. A
#'   Jaccard distance of 1 indicates perfect agreement between two sets while no
#'   agreement will result in a distance of 0.
#' @references Toloşi, L., & Lengauer, T. (2011). \emph{Classification with
#'   correlated features: unreliability of feature ranking and solutions.
#'   Bioinformatics, 27(14), 1986-1994.}
#' @references Bhatnagar, SR., Yang, Y., Blanchette, M., Bouchard, L.,
#'   Khundrakpam, B., Evans, A., Greenwood, CMT. (2016+). \emph{An analytic
#'   approach for interpretable predictive models in high dimensional data, in
#'   the presence of interactions with exposures
#'   \href{http://sahirbhatnagar.com/slides/manuscript1_SB_v4.pdf}{Preprint}}
#' @references Langfelder, P., Zhang, B., & Horvath, S. (2008). \emph{Defining
#'   clusters from a hierarchical cluster tree: the Dynamic Tree Cut package for
#'   R. Bioinformatics, 24(5), 719-720.}
#' @references Friedman, J., Hastie, T. and Tibshirani, R. (2008)
#'   \emph{Regularization Paths for Generalized Linear Models via Coordinate
#'   Descent, \url{http://www.stanford.edu/~hastie/Papers/glmnet.pdf}}
#' @references Breheny, P. and Huang, J. (2011) \emph{Coordinate descent
#'   algorithms for nonconvex penalized regression, with applications to
#'   biological feature selection. Ann. Appl. Statist., 5: 232-253.}
#' @importFrom pacman p_load
#' @import data.table
#' @import magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # simulation parameters
#' rho = 0.90; p = 500 ;SNR = 1 ; n = 200; n0 = n1 = 100 ; nActive = p*0.10 ; cluster_distance = "tom";
#' Ecluster_distance = "difftom"; rhoOther = 0.6; betaMean = 2;
#' alphaMean = 1; betaE = 3; distanceMethod = "euclidean"; clustMethod = "hclust";
#' cutMethod = "dynamic"; agglomerationMethod = "average"
#'
#' #in this simulation its blocks 3 and 4 that are important
#' #leaveOut:  optional specification of modules that should be left out
#' #of the simulation, that is their genes will be simulated as unrelated
#' #("grey"). This can be useful when simulating several sets, in some which a module
#' #is present while in others it is absent.
#' d0 <- s_modules(n = n0, p = p, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = n1, p = p, rho = rho, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' truemodule1 <- d1$setLabels
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'   magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'   magrittr::set_rownames(paste0("Subject",1:n))
#'
#' betaMainEffect <- vector("double", length = p)
#' betaMainInteractions <- vector("double", length = p)
#'
#' # the first nActive/2 in the 3rd block are active
#' betaMainEffect[which(truemodule1 %in% 3)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean - 0.1, betaMean + 0.1)
#'
#' # the first nActive/2 in the 4th block are active
#' betaMainEffect[which(truemodule1 %in% 4)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean+2 - 0.1, betaMean+2 + 0.1)
#' betaMainInteractions[which(betaMainEffect!=0)] <- runif(nActive, alphaMean - 0.1, alphaMean + 0.1)
#' beta <- c(betaMainEffect, betaE, betaMainInteractions)
#'
#' result <- s_generate_data(p = p, X = X,
#'                           beta = beta,
#'                           include_interaction = TRUE,
#'                           cluster_distance = cluster_distance,
#'                           n = n, n0 = n0,
#'                           eclust_distance = Ecluster_distance,
#'                           signal_to_noise_ratio = SNR,
#'                           distance_method = distanceMethod,
#'                           cluster_method = clustMethod,
#'                           cut_method = cutMethod,
#'                           agglomeration_method = agglomerationMethod,
#'                           nPC = 1)
#'
#' pen_res <- s_pen_separate(x_train = result[["X_train"]],
#'                           x_test = result[["X_test"]],
#'                           y_train = result[["Y_train"]],
#'                           y_test = result[["Y_test"]],
#'                           s0 = result[["S0"]],
#'                           model = "lasso",
#'                           exp_family = "gaussian",
#'                           include_interaction = TRUE)
#' unlist(pen_res)
#' }
s_pen_separate <- function(x_train,
                           x_test,
                           y_train,
                           y_test,
                           s0,
                           exp_family = c("gaussian","binomial"),
                           model = c("lasso", "elasticnet", "scad", "mcp"),
                           topgenes = NULL,
                           stability = F,
                           filter = F,
                           include_E = T,
                           include_interaction = F){

  # stability = F; x_train = result[["X_train"]] ; x_test = result[["X_test"]] ;
  # y_train = result[["Y_train"]] ; y_test = result[["Y_test"]];
  # filter = F; filter_var = F; include_E = T; include_interaction = F;
  # s0 = result[["S0"]]; p = p ;
  # model = "lasso"; topgenes = NULL; true_beta = result[["beta_truth"]]

  # stability = F; x_train = result_interaction[["X_train"]] ; x_test = result_interaction[["X_test"]] ;
  # y_train = result_interaction[["Y_train"]] ; y_test = result_interaction[["Y_test"]];
  # filter = F; filter_var = F; include_E = T; include_interaction = T;
  # s0 = result_interaction[["S0"]]; p = 1000 ;
  # model = "scad"; topgenes = NULL; true_beta = result_interaction[["beta_truth"]]

  # result[["clustersAddon"]] %>% print(nrows=Inf)
  # result[["clustersAddon"]][, table(cluster, module)]
  # result %>% names
  # stability = F; gene_groups = result[["clustersAll"]];
  # x_train = result[["X_train"]] ; x_test = result[["X_test"]];
  # y_train = result[["Y_train"]] ; y_test = result[["Y_test"]];
  # filter = F; filter_var = F; include_E = T; include_interaction = T;
  # s0 = result[["S0"]]; p = p ;true_beta = result[["beta_truth"]]
  # model = "lasso"; summary = "pc"; topgenes = NULL; clust_type="clust"; nPC = 1

  # model: "scad", "mcp", "lasso", "elasticnet", "ridge"
  # filter: T or F based on univariate filter

  coef.est = NULL
  exp_family <- match.arg(exp_family)

  if (exp_family == "binomial") {
    pacman::p_load(char = "pROC")
  }


  print(paste(model,"filter = ",
              filter, "filter_var = ",
              include_E, "include_interaction = ",
              include_interaction, sep = " "))

  if (include_E == F & include_interaction == T) stop("include_E needs to be
                                                      TRUE if you want to include
                                                      interactions")
  #   if (filter == F & include_interaction == T) stop("Interaction can only be run
  #                                                      if filter is TRUE.
  #                                                      This is to avoid exceedingly
  #                                                      large models")
  if (is.null(topgenes) & filter == T) stop("Argument topgenes is missing but
                                            filter is TRUE. You need to provide
                                            a filtered list of genes if filter
                                            is TRUE")

  #gene.names <- colnames(x_train)[which(colnames(x_train) %ni% "E")]

  # penalization model
  pen_model <- switch(model,
                      {
                        pacman::p_load(char = "glmnet")
                        lasso = glmnet::cv.glmnet(x = if (!include_E) as.matrix(x_train[,-grep("E", colnames(x_train))]) else
                        as.matrix(x_train), y = y_train, alpha = 1, family = exp_family)
                        },
                      elasticnet = {
                        pacman::p_load(char = "glmnet")
                        glmnet::cv.glmnet(x = if (!include_E) as.matrix(x_train[,-grep("E", colnames(x_train))]) else
                        as.matrix(x_train), y = y_train, alpha = 0.5, family = exp_family)
                        },
                      ridge = {
                        pacman::p_load(char = "glmnet")
                        glmnet::cv.glmnet(x = if (!include_E) as.matrix(x_train[,-grep("E", colnames(x_train))]) else
                        as.matrix(x_train), y = y_train, alpha = 0, family = exp_family)
                        },
                      scad = {
                        pacman::p_load(char = "ncvreg")
                        ncvreg::cv.ncvreg(X = if (!include_E) as.matrix(x_train[,-grep("E", colnames(x_train))]) else
                        as.matrix(x_train), y = y_train,
                        family = "gaussian", penalty = "SCAD")},
                      mcp = {
                        pacman::p_load(char = "ncvreg")
                        ncvreg::cv.ncvreg(X = if (!include_E) as.matrix(x_train[,-grep("E", colnames(x_train))]) else
                        as.matrix(x_train), y = y_train,
                        family = "gaussian", penalty = "MCP")}
  )

  # here we give the coefficient stability on the individual genes
  coefs <- stats::coef(pen_model, s = "lambda.min") %>%
    as.matrix %>%
    data.table::as.data.table(keep.rownames = TRUE) %>%
    magrittr::set_colnames(c("Gene","coef.est")) %>%
    magrittr::extract(-1,)


  if (stability) {
    # remove intercept for stability measures
    return(coefs)
  } else {

    pen.S.hat <- coefs[coef.est != 0] %>% magrittr::use_series("Gene")
    pen.S.hat.interaction <- grep(":", pen.S.hat, value = T)
    pen.S.hat.main <- setdiff(pen.S.hat, pen.S.hat.interaction)

    pen.pred <- if (model %in% c("lasso","elasticnet","ridge")) {
      stats::predict(pen_model, newx =  if (!include_E) as.matrix(x_test[,-grep("E", colnames(x_test))]) else
        as.matrix(x_test), s = "lambda.min") } else if (model %in% c("scad","mcp")) {
          stats::predict(pen_model, X =  if (!include_E) as.matrix(x_test[,-grep("E", colnames(x_test))]) else
            as.matrix(x_test),
            lambda = pen_model$lambda.min)
        }

    # True Positive Rate
    pen.TPR <- length(intersect(pen.S.hat, s0))/length(s0)

    # True negatives
    trueNegs <- setdiff(colnames(x_train), s0)

    # these are the terms which the model identified as zero
    modelIdentifyZero <- setdiff(colnames(x_train),pen.S.hat)

    # how many of the terms identified by the model as zero, were actually zero
    # use to calculate correct sparsity as defined by Witten et al in the
    # Cluster Elastic Net paper Technometrics 2013
    C1 <- sum(modelIdentifyZero %in% trueNegs)
    C2 <- length(intersect(pen.S.hat, s0))
    correct_sparsity <- (C1 + C2)/(ncol(x_train))

    # this is from Interaction Screening for Ultrahigh Dimensional Data by ning hao and hao helen zhang
    true.interaction_names <- grep(":", s0, value = T)
    true.main_effect_names <- setdiff(s0, true.interaction_names)

    all.interaction_names <- grep(":", colnames(x_train), value = T)
    all.main_effect_names <- setdiff(colnames(x_train), all.interaction_names)

    true.negative_main_effects <- setdiff(all.main_effect_names, true.main_effect_names)
    true.negative_interaction_effects <- setdiff(all.interaction_names, true.interaction_names)

    (pen.correct_zeros_main_effects <- sum(setdiff(all.main_effect_names, pen.S.hat.main) %in% true.negative_main_effects)/ length(true.negative_main_effects))
    (pen.correct_zeros_interaction_effects <- sum(setdiff(all.interaction_names, pen.S.hat.interaction) %in% true.negative_interaction_effects)/ length(true.negative_interaction_effects))

    (pen.incorrect_zeros_main_effects <- sum(setdiff(all.main_effect_names, pen.S.hat) %in% true.main_effect_names)/ length(true.main_effect_names))
    (pen.incorrect_zeros_interaction_effects <- sum(setdiff(all.interaction_names, pen.S.hat.interaction) %in% true.interaction_names)/ length(true.interaction_names))

    # False Positive Rate = FP/(FP + TN) = FP / True number of 0 coefficients
    (pen.FPR <- sum(pen.S.hat %ni% s0)/(sum(pen.S.hat %ni% s0) + sum(modelIdentifyZero %in% trueNegs)))


    # Mean Squared Error
    (pen.mse <- crossprod(pen.pred - y_test)/length(y_test))

    # Root Mean Squared Error
    (pen.RMSE <- sqrt(crossprod(pen.pred - y_test)/length(y_test)))

    # mse.null
    mse_null <- crossprod(mean(y_test) - y_test)/length(y_test)

    if (exp_family == "binomial") {
      pred_response <- stats::predict(pen_model, newx =  if (!include_E) as.matrix(x_test[,-grep("E", colnames(x_test))]) else
        as.matrix(x_test), s = "lambda.min", type = "response")

      pen.AUC <- pROC::roc(y_test,as.numeric(pred_response))$auc %>% as.numeric()

    }

    ls <- if (exp_family == "binomial") {
      list(pen.mse = as.numeric(pen.mse),
           pen.RMSE = as.numeric(pen.RMSE),
           pen.AUC = pen.AUC,
           pen.S.hat = length(pen.S.hat),
           pen.TPR = pen.TPR,
           pen.FPR = pen.FPR,
           correct_sparsity,
           pen.correct_zeros_main_effects,
           pen.correct_zeros_interaction_effects,
           pen.incorrect_zeros_main_effects,
           pen.incorrect_zeros_interaction_effects
      ) } else if (exp_family=="gaussian") {

        list(pen.mse = as.numeric(pen.mse),
             pen.RMSE = as.numeric(pen.RMSE),
             pen.S.hat = length(pen.S.hat),
             pen.TPR = pen.TPR,
             pen.FPR = pen.FPR,
             correct_sparsity,
             pen.correct_zeros_main_effects,
             pen.correct_zeros_interaction_effects,
             pen.incorrect_zeros_main_effects,
             pen.incorrect_zeros_interaction_effects
        )
      }

    names(ls) <- if (exp_family == "binomial") {
      c(paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_mse"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_RMSE"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_AUC"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_Shat"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_TPR"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_FPR"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectSparsity"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroMain"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroInter"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroMain"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroInter"))
    } else if (exp_family=="gaussian") {
      c(paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_mse"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_RMSE"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_Shat"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_TPR"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_FPR"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectSparsity"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroMain"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroInter"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroMain"),
        paste0("pen_na_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroInter"))
    }

    return(ls)
  }

}






#' Fit Multivariate Adaptive Regression Splines on Simulated Data
#'
#' @description This function can run Friedman's MARS models on the
#'   untransformed design matrix. To be used with simulated data where the
#'   'truth' is known i.e., you know which features are associated with the
#'   response. This function was used to produce the simulation results in
#'   Bhatnagar et al. 2016. Uses caret functions to tune the degree and the
#'   nprune parameters
#'
#' @param model Type of non-linear model to be fit. Currently only Friedman's
#'   MARS is supported.
#' @param ... other parameters passed to \code{\link[caret]{trainControl}}
#'   function
#' @inheritParams s_pen_clust
#' @return This function has two different outputs depending on whether
#'   \code{stability = TRUE} or \code{stability = FALSE}
#'
#'   If \code{stability = TRUE} then this function returns a \code{p x 2}
#'   data.frame or data.table of regression coefficients without the intercept.
#'   The output of this is used for subsequent calculations of stability.
#'
#'   If \code{stability = FALSE} then returns a vector with the following
#'   elements (See Table 3: Measures of Performance in Bhatnagar et al (2016+)
#'   for definitions of each measure of performance): \item{mse or AUC}{Test set
#'   mean squared error if \code{exp_family = "gaussion"} or test set Area under
#'   the curve if \code{exp_family = "binomial"} calculated using the
#'   \code{\link[pROC]{roc}} function} \item{RMSE}{Square root of the mse. Only
#'   applicable if \code{exp_family = "gaussion"}} \item{Shat}{Number of
#'   non-zero estimated regression coefficients. The non-zero estimated
#'   regression coefficients are referred to as being selected by the model}
#'   \item{TPR}{true positive rate} \item{FPR}{false positive rate}
#'   \item{Correct Sparsity}{Correct true positives + correct true negative
#'   coefficients divided by the total number of features}
#'   \item{CorrectZeroMain}{Proportion of correct true negative main effects}
#'   \item{CorrectZeroInter}{Proportion of correct true negative interactions}
#'   \item{IncorrectZeroMain}{Proportion of incorrect true negative main
#'   effects} \item{IncorrectZeroInter}{Proportion of incorrect true negative
#'   interaction effects}
#' @details This function first does 10 fold cross-validation to tune the degree
#'   (either 1 or 2) using the \code{\link[caret]{train}} function with
#'   \code{method="earth"} and nprune is fixed at 1000. Then the
#'   \code{\link[earth]{earth}} function is used, with \code{nk = 1000} and
#'   \code{pmethod = "backward"} to fit the MARS model using the optimal degree
#'   from the 10-fold CV.
#' @export
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # simulation parameters
#' rho = 0.90; p = 500 ;SNR = 1 ; n = 200; n0 = n1 = 100 ; nActive = p*0.10 ; cluster_distance = "tom";
#' Ecluster_distance = "difftom"; rhoOther = 0.6; betaMean = 2;
#' alphaMean = 1; betaE = 3; distanceMethod = "euclidean"; clustMethod = "hclust";
#' cutMethod = "dynamic"; agglomerationMethod = "average"
#'
#' #in this simulation its blocks 3 and 4 that are important
#' #leaveOut:  optional specification of modules that should be left out
#' #of the simulation, that is their genes will be simulated as unrelated
#' #("grey"). This can be useful when simulating several sets, in some which a module
#' #is present while in others it is absent.
#' d0 <- s_modules(n = n0, p = p, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = n1, p = p, rho = rho, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' truemodule1 <- d1$setLabels
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'   magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'   magrittr::set_rownames(paste0("Subject",1:n))
#'
#' betaMainEffect <- vector("double", length = p)
#'
#' # the first nActive/2 in the 3rd block are active
#' betaMainEffect[which(truemodule1 %in% 3)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean - 0.1, betaMean + 0.1)
#'
#' # the first nActive/2 in the 4th block are active
#' betaMainEffect[which(truemodule1 %in% 4)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean+2 - 0.1, betaMean+2 + 0.1)
#' beta <- c(betaMainEffect, betaE)
#'
#' result <- s_generate_data_mars(p = p, X = X,
#'                                beta = beta,
#'                                binary_outcome = FALSE,
#'                                truemodule = truemodule1,
#'                                nActive = nActive,
#'                                include_interaction = FALSE,
#'                                cluster_distance = cluster_distance,
#'                                n = n, n0 = n0,
#'                                eclust_distance = Ecluster_distance,
#'                                signal_to_noise_ratio = SNR,
#'                                distance_method = distanceMethod,
#'                                cluster_method = clustMethod,
#'                                cut_method = cutMethod,
#'                                agglomeration_method = agglomerationMethod,
#'                                nPC = 1)
#'
#'
#' mars_res <- s_mars_separate(x_train = result[["X_train"]],
#'                             x_test = result[["X_test"]],
#'                             y_train = result[["Y_train"]],
#'                             y_test = result[["Y_test"]],
#'                             s0 = result[["S0"]],
#'                             exp_family = "gaussian")
#' unlist(mars_res)
#' }

s_mars_separate <- function(x_train,
                            x_test,
                            y_train,
                            y_test,
                            s0,
                            model = c("MARS"),
                            exp_family = c("gaussian", "binomial"),
                            topgenes = NULL,
                            stability = F,
                            filter = F,
                            include_E = T,
                            include_interaction = F, ...){

  # stability = F; x_train = result[["X_train"]] ; x_test = result[["X_test"]] ;
  # y_train = result[["Y_train"]] ; y_test = result[["Y_test"]];
  # filter = F; filter_var = F; include_E = T; include_interaction = T;
  # s0 = result[["S0"]]; p = p ;
  # model = "MARS"; topgenes = NULL; true_beta = result[["beta_truth"]]

  # stability = F; x_train = result_interaction[["X_train"]] ; x_test = result_interaction[["X_test"]] ;
  # y_train = result_interaction[["Y_train"]] ; y_test = result_interaction[["Y_test"]];
  # filter = F; filter_var = F; include_E = T; include_interaction = T;
  # s0 = result_interaction[["S0"]]; p = 1000 ;
  # model = "scad"; topgenes = NULL; true_beta = result_interaction[["beta_truth"]]

  # result[["clustersAddon"]] %>% print(nrows=Inf)
  # result[["clustersAddon"]][, table(cluster, module)]
  # result %>% names
  # stability = F; gene_groups = result[["clustersAll"]];
  # x_train = result[["X_train"]] ; x_test = result[["X_test"]];
  # y_train = result[["Y_train"]] ; y_test = result[["Y_test"]];
  # filter = F; filter_var = F; include_E = T; include_interaction = T;
  # s0 = result[["S0"]]; p = p ;true_beta = result[["beta_truth"]]
  # model = "lasso"; summary = "pc"; topgenes = NULL; clust_type="clust"; nPC = 1

  # model: "scad", "mcp", "lasso", "elasticnet", "ridge"
  # filter: T or F based on univariate filter

  pacman::p_load(char = "earth")

  exp_family <- match.arg(exp_family)

  if (exp_family == "binomial") {
    pacman::p_load(char = "pROC")
  }

  print(paste(model,"filter = ", filter, "include_E = ",
              include_E, "include_interaction = ", include_interaction, sep = " "))

  if (include_E == F & include_interaction == T) stop("include_E needs to be
                                                      TRUE if you want to include
                                                      interactions")
  #   if (filter == F & include_interaction == T) stop("Interaction can only be run
  #                                                      if filter is TRUE.
  #                                                      This is to avoid exceedingly
  #                                                      large models")
  if (is.null(topgenes) & filter == T) stop("Argument topgenes is missing but
                                            filter is TRUE. You need to provide
                                            a filtered list of genes if filter
                                            is TRUE")

  #gene.names <- colnames(x_train)[which(colnames(x_train) %ni% "E")]

  # mars model
  mars_model <- switch(model,
                       MARS = {
                         pacman::p_load(char = "caret")
                         pacman::p_load(char = "earth")

                         fitControl <-  caret::trainControl(method = "cv",
                                                            verboseIter = FALSE, ...)

                         marsGrid <- expand.grid(.degree = 1:2, .nprune = 1000)

                         switch(exp_family,
                                gaussian = {
                                  mars_tuned <- caret::train(as.matrix(x_train),
                                                             y_train,
                                                             method = "earth",
                                                             trace = 1, nk = 1000, keepxy = TRUE, pmethod = "backward",
                                                             tuneGrid = marsGrid,
                                                             trControl = fitControl)

                                  earth::earth(x = as.matrix(x_train),
                                               y = y_train,
                                               keepxy = TRUE,
                                               pmethod = "backward",
                                               nk = 1000,
                                               degree = mars_tuned$bestTune$degree,
                                               trace = 1, nfold = 10) },
                                binomial = {

                                  mars_tuned <- caret::train(as.matrix(x_train),
                                                             as.factor(y_train),
                                                             method = "earth",
                                                             trace = 1, nk = 1000, keepxy = TRUE, pmethod = "backward",
                                                             glm=list(family="binomial"),
                                                             tuneGrid = marsGrid,
                                                             trControl = fitControl)

                                  earth::earth(x = as.matrix(x_train),
                                               y = as.factor(y_train),
                                               keepxy = TRUE,
                                               pmethod = "backward",
                                               nk = 1000,
                                               glm=list(family="binomial"),
                                               degree = mars_tuned$bestTune$degree,
                                               trace = 1, nfold = 10)
                                })
                       }
  )


  # selected genes
  # coef(mars_model)
  # u_extract_selected_earth(mars_model)
  #
  # plot(mars_model, which=1, col.rsq=0) # which=1 for Model Selection plot only (optional)
  # plot.earth.models(mars_model$cv.list, which=1)
  # plot(mars_model)
  # plot(mars_model, which=1,
  #      col.mean.infold.rsq="blue", col.infold.rsq="lightblue",
  #      col.grsq=0, col.rsq=0, col.vline=0, col.oof.vline=0)


  # ONLY Jaccard index can be calculated for MARS
  # since u_extract_selected_earth returns only the non-zero coefficients,
  # we give a coefficient of 1 here so that the stability calculation works
  # because it takes non-zero coef estimates

  coefs <- data.frame(u_extract_selected_earth(mars_model), rep(1, length(u_extract_selected_earth(mars_model))), stringsAsFactors = FALSE) %>%
    data.table::as.data.table(keep.rownames = FALSE) %>%
    magrittr::set_colnames(c("Gene","coef.est"))

  if (stability) {
    # remove intercept for stability measures
    return(coefs)
  } else {

    mars.S.hat <- u_extract_selected_earth(mars_model)
    mars.S.hat.interaction <- grep(":", mars.S.hat, value = T)
    mars.S.hat.main <- setdiff(mars.S.hat, mars.S.hat.interaction)

    mars.pred <- stats::predict(mars_model, newdata = x_test, trace = 4)

    if (exp_family == "binomial") {
      pred_response <- stats::predict(mars_model, newdata = x_test, trace = 4,
                               type = "response")

      mars.AUC <- pROC::roc(y_test,as.numeric(pred_response))$auc %>% as.numeric()

    }

    # True Positive Rate
    mars.TPR <- length(intersect(mars.S.hat, s0))/length(s0)

    # True negatives
    trueNegs <- setdiff(colnames(x_train), s0)

    # these are the terms which the model identified as zero
    modelIdentifyZero <- setdiff(colnames(x_train),mars.S.hat)

    # how many of the terms identified by the model as zero, were actually zero
    # use to calculate correct sparsity as defined by Witten et al in the
    # Cluster Elastic Net paper Technometrics 2013
    C1 <- sum(modelIdentifyZero %in% trueNegs)
    C2 <- length(intersect(mars.S.hat, s0))
    correct_sparsity <- (C1 + C2)/(ncol(x_train))

    # this is from Interaction Screening for Ultrahigh Dimensional Data by ning hao and hao helen zhang
    true.interaction_names <- grep(":", s0, value = T)
    true.main_effect_names <- setdiff(s0, true.interaction_names)

    all.interaction_names <- grep(":", colnames(x_train), value = T)
    all.main_effect_names <- setdiff(colnames(x_train), all.interaction_names)

    true.negative_main_effects <- setdiff(all.main_effect_names, true.main_effect_names)
    true.negative_interaction_effects <- setdiff(all.interaction_names, true.interaction_names)

    (mars.correct_zeros_main_effects <- sum(setdiff(all.main_effect_names, mars.S.hat.main) %in% true.negative_main_effects)/ length(true.negative_main_effects))
    (mars.correct_zeros_interaction_effects <- sum(setdiff(all.interaction_names, mars.S.hat.interaction) %in% true.negative_interaction_effects)/ length(true.negative_interaction_effects))

    (mars.incorrect_zeros_main_effects <- sum(setdiff(all.main_effect_names, mars.S.hat) %in% true.main_effect_names)/ length(true.main_effect_names))
    (mars.incorrect_zeros_interaction_effects <- sum(setdiff(all.interaction_names, mars.S.hat.interaction) %in% true.interaction_names)/ length(true.interaction_names))

    # False Positive Rate = FP/(FP + TN) = FP / True number of 0 coefficients
    (mars.FPR <- sum(mars.S.hat %ni% s0)/(sum(mars.S.hat %ni% s0) + sum(modelIdentifyZero %in% trueNegs)))

    # # False Positive Rate
    # mars.FPR <- sum(mars.S.hat %ni% s0)/(p - length(s0))

    # Mean Squared Error
    (mars.mse <- crossprod(mars.pred - y_test)/length(y_test))

    # Root Mean Squared Error
    (mars.RMSE <- sqrt(crossprod(mars.pred - y_test)/length(y_test)))

    # mse.null
    mse_null <- crossprod(mean(y_test) - y_test)/length(y_test)

    ls <- switch(exp_family,
                 gaussian = {list(mars.mse = as.numeric(mars.mse),
                                  mars.RMSE = as.numeric(mars.RMSE),
                                  mars.S.hat = length(mars.S.hat),
                                  mars.TPR = mars.TPR,
                                  mars.FPR = mars.FPR,
                                  correct_sparsity,
                                  mars.correct_zeros_main_effects,
                                  mars.correct_zeros_interaction_effects,
                                  mars.incorrect_zeros_main_effects,
                                  mars.incorrect_zeros_interaction_effects)},
                 binomial = {list(mars.mse = as.numeric(mars.mse),
                                  mars.RMSE = as.numeric(mars.RMSE),
                                  mars.AUC = mars.AUC,
                                  mars.S.hat = length(mars.S.hat),
                                  mars.TPR = mars.TPR,
                                  mars.FPR = mars.FPR,
                                  correct_sparsity,
                                  mars.correct_zeros_main_effects,
                                  mars.correct_zeros_interaction_effects,
                                  mars.incorrect_zeros_main_effects,
                                  mars.incorrect_zeros_interaction_effects)

                 })

    names(ls) <- switch(exp_family,
                        gaussian = {
                          c(paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_mse"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_RMSE"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_Shat"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_TPR"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_FPR"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectSparsity"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroMain"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroInter"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroMain"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroInter"))},
                        binomial = {
                          c(paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_mse"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_RMSE"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_AUC"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_Shat"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_TPR"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_FPR"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectSparsity"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroMain"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroInter"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroMain"),
                            paste0("mars_na_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroInter"))
                        })
    return(ls)
  }
}




#' Fit MARS Models on Simulated Cluster Summaries
#'
#' @description This function creates summaries of the given clusters (e.g. 1st
#'   PC or average), and then runs Friedman's MARS on those
#'   summaries. To be used with simulated data where the 'truth' is known i.e.,
#'   you know which features are associated with the response. This function was
#'   used to produce the simulation results in Bhatnagar et al. 2016.
#'
#' @param model Type of non-linear model to be fit. Currently only Friedman's
#'   MARS is supported.
#' @param true_beta numeric vector of true beta coefficients
#' @param nPC Number of principal components if \code{summary = "pc"}.
#'   Default is \code{nPC = 1}. Can be either 1 or 2.
#' @inheritParams s_pen_clust
#' @details This function first does 10 fold cross-validation to tune the degree
#'   (either 1 or 2) using the \code{\link[caret]{train}} function with
#'   \code{method="earth"} and nprune is fixed at 1000. Then the
#'   \code{\link[earth]{earth}} function is used, with \code{nk = 1000} and
#'   \code{pmethod = "backward"} to fit the MARS model using the optimal degree
#'   from the 10-fold CV.
#' @return This function has two different outputs depending on whether
#'   \code{stability = TRUE} or \code{stability = FALSE}
#'
#'   If \code{stability = TRUE} then this function returns a \code{p x 2}
#'   data.frame or data.table of regression coefficients without the intercept.
#'   The output of this is used for subsequent calculations of stability.
#'
#'   If \code{stability = FALSE} then returns a vector with the following
#'   elements (See Table 3: Measures of Performance in Bhatnagar et al (2016+)
#'   for definitions of each measure of performance): \item{mse or AUC}{Test set
#'   mean squared error if \code{exp_family = "gaussion"} or test set Area under
#'   the curve if \code{exp_family = "binomial"} calculated using the
#'   \code{\link[pROC]{roc}} function} \item{RMSE}{Square root of the mse. Only
#'   applicable if \code{exp_family = "gaussion"}} \item{Shat}{Number of
#'   non-zero estimated regression coefficients. The non-zero estimated
#'   regression coefficients are referred to as being selected by the model}
#'   \item{TPR}{true positive rate} \item{FPR}{false positive rate}
#'   \item{Correct Sparsity}{Correct true positives + correct true negative
#'   coefficients divided by the total number of features}
#'   \item{CorrectZeroMain}{Proportion of correct true negative main effects}
#'   \item{CorrectZeroInter}{Proportion of correct true negative interactions}
#'   \item{IncorrectZeroMain}{Proportion of incorrect true negative main
#'   effects} \item{IncorrectZeroInter}{Proportion of incorrect true negative
#'   interaction effects}
#' @export
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # simulation parameters
#' rho = 0.90; p = 500 ;SNR = 1 ; n = 200; n0 = n1 = 100 ; nActive = p*0.10 ; cluster_distance = "tom";
#' Ecluster_distance = "difftom"; rhoOther = 0.6; betaMean = 2;
#' alphaMean = 1; betaE = 3; distanceMethod = "euclidean"; clustMethod = "hclust";
#' cutMethod = "dynamic"; agglomerationMethod = "average"
#'
#' #in this simulation its blocks 3 and 4 that are important
#' #leaveOut:  optional specification of modules that should be left out
#' #of the simulation, that is their genes will be simulated as unrelated
#' #("grey"). This can be useful when simulating several sets, in some which a module
#' #is present while in others it is absent.
#' d0 <- s_modules(n = n0, p = p, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = n1, p = p, rho = rho, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' truemodule1 <- d1$setLabels
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'   magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'   magrittr::set_rownames(paste0("Subject",1:n))
#'
#' betaMainEffect <- vector("double", length = p)
#'
#' # the first nActive/2 in the 3rd block are active
#' betaMainEffect[which(truemodule1 %in% 3)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean - 0.1, betaMean + 0.1)
#'
#' # the first nActive/2 in the 4th block are active
#' betaMainEffect[which(truemodule1 %in% 4)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean+2 - 0.1, betaMean+2 + 0.1)
#' beta <- c(betaMainEffect, betaE)
#'
#' result <- s_generate_data_mars(p = p, X = X,
#'                                beta = beta,
#'                                binary_outcome = FALSE,
#'                                truemodule = truemodule1,
#'                                nActive = nActive,
#'                                include_interaction = FALSE,
#'                                cluster_distance = cluster_distance,
#'                                n = n, n0 = n0,
#'                                eclust_distance = Ecluster_distance,
#'                                signal_to_noise_ratio = SNR,
#'                                distance_method = distanceMethod,
#'                                cluster_method = clustMethod,
#'                                cut_method = cutMethod,
#'                                agglomeration_method = agglomerationMethod,
#'                                nPC = 1)
#'
#'
#' mars_res <- s_mars_clust(x_train = result[["X_train"]],
#'                          x_test = result[["X_test"]],
#'                          y_train = result[["Y_train"]],
#'                          y_test = result[["Y_test"]],
#'                          s0 = result[["S0"]],
#'                          summary = "pc",
#'                          exp_family = "gaussian",
#'                          gene_groups = result[["clustersAddon"]],
#'                          clust_type = "ECLUST")
#' unlist(mars_res)
#' }
s_mars_clust <- function(x_train,
                         x_test,
                         y_train,
                         y_test,
                         s0,
                         summary = c("pc","avg"),
                         model = c("MARS"),
                         exp_family = c("gaussian","binomial"),
                         gene_groups,
                         true_beta = NULL,
                         topgenes = NULL,
                         stability = F,
                         filter = F,
                         include_E = T,
                         include_interaction = F,
                         clust_type = c("CLUST","ECLUST"),
                         nPC = 1) {

  # result[["clustersAddon"]] %>% print(nrows=Inf)
  # result[["clustersAddon"]][, table(cluster, module)]
  # result %>% names
  # stability = F; gene_groups = result[["clustersAddon"]];
  # x_train = result[["X_train"]] ; x_test = result[["X_test"]];
  # y_train = result[["Y_train"]] ; y_test = result[["Y_test"]];
  # dim(x_train)
  # filter = F; filter_var = F; include_E = T; include_interaction = F;
  # s0 = result[["S0"]]; p = p ;true_beta = result[["beta_truth"]]
  # model = "MARS"; summary = "pc"; topgenes = NULL; clust_type="Eclust"; nPC = 1
  # exp_family = "binomial"

  pacman::p_load(char = "earth")

  coef.est = cluster = gene = NULL
  clust_type <- match.arg(clust_type)
  summary <- match.arg(summary)
  model <- match.arg(model)
  exp_family <- match.arg(exp_family)

  if (exp_family == "binomial") {
    pacman::p_load(char = "pROC")
  }

  message(sprintf("Summary measure: %s, Model: %s, Cluster Type: %s",
                  summary, model, clust_type))

  if (include_E == F & include_interaction == T) stop("include_E needs to be
                                                      TRUE if you want to include
                                                      interactions")

  if (is.null(topgenes) & filter == T) stop("Argument topgenes is missing but
                                            filter is TRUE. You need to provide
                                            a filtered list of genes if filter
                                            is TRUE")

  # train data which includes the relevant (filtered or not filtered genes
  # and E or not E)
  x_train_mod <- if (filter & !include_E) {
    x_train[, topgenes] %>% as.data.frame
  } else if (!filter & include_E) {
    x_train %>% as.data.frame
  } else if (!filter & !include_E) {
    x_train[,which(colnames(x_train) %ni% "E")] %>% as.data.frame
  } else if (filter & include_E) {
    x_train[, c(topgenes,"E")] %>% as.data.frame
  }

  # test data
  x_test_mod = if (filter & !include_E) {
    x_test[, topgenes] %>% as.data.frame
  } else if (!filter & include_E) {
    x_test %>% as.data.frame
  } else if (!filter & !include_E) {
    x_test[,which(colnames(x_test) %ni% "E")] %>% as.data.frame
  } else if (filter & include_E) {
    x_test[, c(topgenes,"E")] %>% as.data.frame
  }

  # these are only derived on the main effects genes.. E is only included in the model
  PC_and_avg <- u_extract_summary(x_train = x_train_mod[,gene_groups$gene],
                          colors = gene_groups$cluster,
                          x_test = x_test_mod[,gene_groups$gene],
                          nPC = nPC)

  n.clusters <- PC_and_avg$nclusters

  # this contains either the averages or PCs for each module in a data.frame
  clust_data <- switch(summary,
                       avg = PC_and_avg$averageExpr,
                       pc = PC_and_avg$PC)

  ml.formula <- if (include_interaction & include_E) {
    stats::as.formula(paste0("y_train ~","(",paste0(colnames(clust_data), collapse = "+"),")*E"))
  } else if (!include_interaction & include_E) {
    stats::as.formula(paste0("y_train ~",paste0(colnames(clust_data), collapse = "+"),"+E"))
  } else if (!include_interaction & !include_E) {
    stats::as.formula(paste0("y_train ~",paste0(colnames(clust_data), collapse = "+")))
  }

  # this is the same as ml.formula, except without the response.. this is used for
  # functions that have the x = and y = input instead of a formula input
  model.formula <- if (include_interaction & include_E) {
    stats::as.formula(paste0("~ 0+(",paste0(colnames(clust_data), collapse = "+"),")*E"))
  } else if (!include_interaction & include_E) {
    stats::as.formula(paste0("~0+",paste0(colnames(clust_data), collapse = "+"),"+E"))
  } else if (!include_interaction & !include_E) {
    stats::as.formula(paste0("~0+",paste0(colnames(clust_data), collapse = "+")))
  }

  # this is the design matrix based on model.formula
  X.model.formula <- stats::model.matrix(model.formula, data = if (include_E) {
    cbind(clust_data,x_train_mod[,"E", drop = F])
  } else clust_data %>% as.data.frame)

  df <- X.model.formula %>% cbind(y_train) %>% as.data.frame()

  clust_train_model <- switch(model,
                              MARS = {

                                fitControl <-  caret::trainControl(method = "cv",
                                                            # number = 25,
                                                            # repeats = 3,
                                                            verboseIter = FALSE)

                                marsGrid <- expand.grid(.degree = 1:2, .nprune = 1000)

                                switch(exp_family,
                                       gaussian = {
                                         mars_tuned <- caret::train(X.model.formula,
                                                                    y_train,
                                                                    method = "earth",
                                                                    trace = 1, nk = 1000,
                                                                    keepxy = TRUE, pmethod = "backward",
                                                                    tuneGrid = marsGrid,
                                                                    trControl = fitControl)

                                         earth::earth(x = X.model.formula,
                                                      y = y_train,
                                                      keepxy = TRUE,
                                                      pmethod = "backward",
                                                      nk = 1000,
                                                      degree = mars_tuned$bestTune$degree,
                                                      trace = 4, nfold = 10) },
                                       binomial = {

                                         mars_tuned <- caret::train(X.model.formula,
                                                             as.factor(y_train),
                                                             method = "earth",
                                                             trace = 1, nk = 1000, keepxy = TRUE, pmethod = "backward",
                                                             glm=list(family="binomial"),
                                                             tuneGrid = marsGrid,
                                                             trControl = fitControl)

                                         earth::earth(x = X.model.formula,
                                                      y = as.factor(y_train),
                                                      keepxy = TRUE,
                                                      pmethod = "backward",
                                                      nk = 1000,
                                                      glm=list(family="binomial"),
                                                      degree = mars_tuned$bestTune$degree,
                                                      trace = 4, nfold = 10)
                                       })
                              }
  )


  # summary(clust_train_model)
  # ONLY Jaccard index can be calculated for MARS
  # since u_extract_selected_earth returns only the non-zero coefficients,
  # we give a coefficient of 1 here so that the stability calculation works
  # because it takes non-zero coef estimates

  coefs <- data.frame(u_extract_selected_earth(clust_train_model), rep(1, length(u_extract_selected_earth(clust_train_model))), stringsAsFactors = F) %>%
    as.data.table(keep.rownames = FALSE) %>%
    magrittr::set_colnames(c("Gene","coef.est"))

  if (stability) {
    # remove intercept for stability measures
    return(coefs)
  } else {

    non_zero_clusters <- coefs[coef.est != 0] %>%
      magrittr::use_series("Gene")

    # need to determine which of non_zero_cluters are main effects and which
    # are interactions
    non_zero_clusters_interactions <- grep(":",non_zero_clusters, value = T)

    # this checks if the environment is non-zero
    non_zero_environment <- grep("^E", non_zero_clusters, value = T,
                                 ignore.case = TRUE)
    non_zero_clusters_main_effects <- setdiff(non_zero_clusters,
                                              c(non_zero_clusters_interactions,
                                                non_zero_environment))

    # this includes the environment if the environment is non-zero
    n.non_zero_clusters <- coefs[coef.est != 0] %>%
      magrittr::use_series("Gene") %>%
      length

    # need to get the genes corresponding to the non-zero clusters
    # NOTE: this also includes non-zero cluster:Environment interactions

    # genes corresponding to non-zero main effect clusters
    # this list might not be unique if clust_type="Addon" because the same gene
    # can be in different clusters
    clust.S.hat.main <- gene_groups[cluster %in%
                                      as.numeric(
                                        unlist(
                                          stringr::str_extract_all(
                                            non_zero_clusters_main_effects, "(\\d+)$")
                                        )
                                      ),gene]

    # identical(gene_groups[cluster %in% c(3,12),gene],
    #           clust.S.hat.main)
    # identical(unique(clust.S.hat.main), clust.S.hat.main)
    # table(clust.S.hat.main)

    # this is the same as gene_groups, but the gene names contain E
    # so that we can extract the interactions corresponding to the chose clusters
    gene_groups_E <- copy(gene_groups)
    gene_groups_E[,gene:=paste0(gene,":E")]

    clust.S.hat.interaction <- gene_groups_E[cluster %in%
                                               as.numeric(
                                                 unlist(
                                                   stringr::str_extract_all(
                                                     stringr::str_extract_all(non_zero_clusters_interactions,"^.*?(?=:)"),
                                                     "(\\d+)$")
                                                 )
                                               ),gene]

    # this represents all the genes corresponding to the non-zero PC or avg
    # this list might not be unique if clust_type="Addon"
    # identical(unique(clust.S.hat), clust.S.hat)
    # I will double count if a model takes a gene more than once. ie.
    # if the same gene gets selected twice, then this will contribute 2 to the
    # number of non-zero estimated coefficients
    clust.S.hat <- c(clust.S.hat.main, non_zero_environment,
                     clust.S.hat.interaction)


    clust_data_test <- switch(summary,
                              avg = PC_and_avg$averageExprTest,
                              pc = PC_and_avg$PCTest)

    if (summary=="pc") { colnames(clust_data_test) <- colnames(clust_data) }

    # need intercept for prediction
    model.formula_test <- if (include_interaction & include_E) {
      stats::as.formula(paste0("~ 1+(",paste0(colnames(clust_data_test), collapse = "+"),")*E"))
    } else if (!include_interaction & include_E) {
      stats::as.formula(paste0("~1+",paste0(colnames(clust_data_test), collapse = "+"),"+E"))
    } else if (!include_interaction & !include_E) {
      stats::as.formula(paste0("~1+",paste0(colnames(clust_data_test), collapse = "+")))
    }


    # this includes the intercept!
    X.model.formula_test <- stats::model.matrix(model.formula_test,
                                         data = if (include_E) {
                                           cbind(clust_data_test,x_test_mod[,"E", drop = F])
                                         } else clust_data_test %>% as.data.frame)

    # True Positive Rate
    clust.TPR <- length(intersect(clust.S.hat, s0))/length(s0)

    # True negatives
    trueNegs <- setdiff(colnames(x_train_mod), s0)
    # identical(setdiff(colnames(x_train_mod), s0), setdiff(colnames(x_train), s0))

    # these are the terms which the model identified as zero
    modelIdentifyZero <- setdiff(colnames(x_train_mod),clust.S.hat)

    # how many of the terms identified by the model as zero, were actually zero
    # use to calculate correct sparsity as defined by Witten et al in the
    # Cluster Elastic Net paper Technometrics 2013
    C1 <- sum(modelIdentifyZero %in% trueNegs)
    C2 <- length(intersect(clust.S.hat, s0))
    clust.correct_sparsity <- (C1 + C2)/(ncol(x_train_mod))

    # this is from Interaction Screening for Ultrahigh Dimensional Data by ning hao and hao helen zhang
    true.interaction_names <- grep(":", s0, value = T)
    true.main_effect_names <- setdiff(s0, true.interaction_names)

    all.interaction_names <- grep(":", colnames(x_train_mod), value = T)
    all.main_effect_names <- setdiff(colnames(x_train_mod), all.interaction_names)

    true.negative_main_effects <- setdiff(all.main_effect_names, true.main_effect_names)
    true.negative_interaction_effects <- setdiff(all.interaction_names, true.interaction_names)

    (clust.correct_zeros_main_effects <- sum(setdiff(all.main_effect_names, c(clust.S.hat.main, non_zero_environment)) %in% true.negative_main_effects)/ length(true.negative_main_effects))
    (clust.correct_zeros_interaction_effects <- sum(setdiff(all.interaction_names, clust.S.hat.interaction) %in% true.negative_interaction_effects)/ length(true.negative_interaction_effects))

    (clust.incorrect_zeros_main_effects <- sum(setdiff(all.main_effect_names, c(clust.S.hat.main, non_zero_environment)) %in% true.main_effect_names)/ length(true.main_effect_names))
    (clust.incorrect_zeros_interaction_effects <- sum(setdiff(all.interaction_names, clust.S.hat.interaction) %in% true.interaction_names)/ length(true.interaction_names))

    # False Positive Rate = FP/(FP + TN) = FP / True number of 0 coefficients
    (clust.FPR <- sum(clust.S.hat %ni% s0)/(sum(clust.S.hat %ni% s0) + sum(modelIdentifyZero %in% trueNegs)))

    # Mean Squared Error

    # mars predict automatically adds the intercept if its not already in the dataset
    # the code below proves this
    # mars.pred1 <- predict(clust_train_model, newdata = X.model.formula_test[,-1], trace = 4)
    # mars.pred2 <- predict(clust_train_model, newdata = X.model.formula_test, trace = 4)
    # plot(mars.pred1, mars.pred2) ; identical(mars.pred1,mars.pred2)
    mars.pred <- stats::predict(clust_train_model, newdata = X.model.formula_test, trace = 4)

    # Mean Squared Error
    (clust.mse <- crossprod(mars.pred - y_test)/length(y_test))

    # Root Mean Squared Error
    (clust.RMSE <- sqrt(crossprod(mars.pred - y_test)/length(y_test)))

    # remove intercept for prediction error formula given by ||X\beta - X\hat{\beta}||_2
    # given in Witten 2013 Cluster ENET paper in Technometrics
    # (clust.test_set_pred_error <- sqrt(crossprod(as.matrix(x_test_mod) %*% as.numeric(true_beta) - X.model.formula_test[,-1] %*% coefs$coef.est[-1])))

    # mse.null
    (mse_null <- crossprod(mean(y_test) - y_test)/length(y_test))

    if (exp_family == "binomial") {
      pred_response <- stats::predict(clust_train_model, newdata = X.model.formula_test, trace = 4,
                               type = "response")

      clust.AUC <- pROC::roc(y_test,as.numeric(pred_response))$auc %>% as.numeric()

    }


    # the proportional decrease in model error or R^2 for each scenario (pg. 346 ESLv10)
    # clust.r2 <- (mse_null - clust.mse)/mse_null

    # clust.adj.r2 <- 1 - (1 - clust.r2)*(nrow(x_test) - 1)/(nrow(x_test) - n.non_zero_clusters - 1)


    ls <- switch(exp_family,
                 gaussian = {list(clust.mse = as.numeric(clust.mse),
                                  clust.RMSE = as.numeric(clust.RMSE),
                                  clust.S.hat = length(clust.S.hat),
                                  clust.TPR = clust.TPR,
                                  clust.FPR = clust.FPR,
                                  clust.correct_sparsity,
                                  clust.correct_zeros_main_effects,
                                  clust.correct_zeros_interaction_effects,
                                  clust.incorrect_zeros_main_effects,
                                  clust.incorrect_zeros_interaction_effects)},
                 binomial = {list(clust.mse = as.numeric(clust.mse),
                                  clust.RMSE = as.numeric(clust.RMSE),
                                  clust.AUC = clust.AUC,
                                  clust.S.hat = length(clust.S.hat),
                                  clust.TPR = clust.TPR,
                                  clust.FPR = clust.FPR,
                                  clust.correct_sparsity,
                                  clust.correct_zeros_main_effects,
                                  clust.correct_zeros_interaction_effects,
                                  clust.incorrect_zeros_main_effects,
                                  clust.incorrect_zeros_interaction_effects)

                 })

    names(ls) <- switch(exp_family,
                        gaussian = {
                          c(paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_mse"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_RMSE"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_Shat"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_TPR"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_FPR"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectSparsity"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroMain"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroInter"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroMain"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroInter"))},
                        binomial = {
                          c(paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_mse"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_RMSE"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_AUC"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_Shat"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_TPR"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_FPR"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectSparsity"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroMain"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_CorrectZeroInter"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroMain"),
                            paste0(clust_type,"_",summary,"_",model,ifelse(include_interaction,"_yes","_no"),"_IncorrectZeroInter"))
                        })
    return(ls)

  }
}



#' Simulate Covariates With Exposure Dependent Correlations
#'
#' @description This is a wrapper of the \code{\link[WGCNA]{simulateDatExpr}}
#'   function which simulates data in a modular structure (i.e. in blocks). This
#'   function simulates data in 5 blocks referred to as Turquoise, Blue, Red,
#'   Green and Yellow, separately for exposed (E=1) and unexposed (E=0)
#'   observations.
#'
#' @param n number of observations
#' @param p total number of predictors to simulate
#' @param exposed binary numeric vector of length \code{n} with 0 for unexposed
#'   and 1 for exposed
#' @param rho numeric value representing the expected correlation between green
#'   module and red module
#' @param ... arguments passed to the \code{\link[WGCNA]{simulateDatExpr}} function
#' @return \code{n x p} matrix of simulated data
#' @export
#' @examples
#' library(magrittr)
#' p <- 1000
#' n <- 200
#' d0 <- s_modules(n = 100, p = 1000, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = 100, p = 1000, rho = 0.90, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'  magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'  magrittr::set_rownames(paste0("Subject",1:n))
#' dim(X)

s_modules <- function(n, p, rho, exposed, ...) {

  if (exposed) {
    #Step 1: simulate the seed module eigengenes
    sMEturquoise <- stats::rnorm(n)

    #expected cor(sMEblue,sMEturquoise) = 0.60
    sMEblue <- 0.60 * sMEturquoise + sqrt(1 - 0.60 ^ 2) * stats::rnorm(n)

    sMEyellow <- stats::rnorm(n)

    sMEgreen <- stats::rnorm(n)

    #expected cor(e.continuous,seed.ME)=0.95
    temp0 <- rho[1] * sMEgreen + sqrt(1 - rho[1] ^ 2) * stats::rnorm(n)

    #expected cor(y.continuous,seed.ME) <- -0.95
    sMEred <- rho[1] * temp0 + sqrt(1 - rho[1] ^ 2) * stats::rnorm(n)

    datsME <- data.frame(sMEturquoise,sMEblue,sMEred,sMEgreen,sMEyellow)

    dat1 <- WGCNA::simulateDatExpr(eigengenes = datsME, nGenes = p, ...)
  } else {

    #Step 1: simulate the seed module eigengenes
    sMEturquoise <- stats::rnorm(n)

    #expected cor(sMEblue,sMEturquoise) = 0.60
    sMEblue <- 0.60 * sMEturquoise + sqrt(1 - 0.60 ^ 2) * stats::rnorm(n)

    sMEyellow <- stats::rnorm(n)

    sMEgreen <- stats::rnorm(n)

    #expected cor(e.continuous,seed.ME)=0.95
    temp0 <- rho[1] * sMEgreen + sqrt(1 - rho[1] ^ 2) * stats::rnorm(n)

    #expected cor(y.continuous,seed.ME) <- -0.95
    sMEred <- rho[1] * temp0 + sqrt(1 - rho[1] ^ 2) * stats::rnorm(n)

    datsME <- data.frame(sMEturquoise,sMEblue,sMEred,sMEgreen,sMEyellow)

    dat1 <- WGCNA::simulateDatExpr(eigengenes = datsME, nGenes = p, ...)

  }

  return(dat1)
}



#' Generate linear response data and test and training sets for simulation study
#'
#' @description create a function that takes as input, the number of genes, the
#'   true beta vector, the gene expression matrix created from the
#'   generate_blocks function and returns a list of data matrix, as well as
#'   correlation matrices, TOM matrices, cluster information, training and test
#'   data
#' @note this function calls the \code{s_response} to generate phenotype as a
#'   function of the gene expression data. This function also returns other
#'   information derived from the simulated data including the test and training
#'   sets, the correlation and TOM matrices and the clusters.
#' @note the PCs and averages need to be calculated in the fitting functions,
#'   because these will change based on the CV fold
#' @return list of (in the following order) \describe{ \item{beta_truth}{a 1
#'   column matrix containing the true beta coefficient vector}
#'   \item{similarity}{an object of class similarity which is the similarity
#'   matrix specified by the \code{cluster_distance}
#'   argument}\item{similarityEclust}{an object of class similarity which is the
#'   similarity matrix specified by the \code{eclust_distance} argument}
#'   \item{DT}{data.table of simulated data from the \code{s_response} function}
#'   \item{Y}{The simulated response} \item{X0}{the n0 x p design matrix for the
#'   unexposed subjects} \item{X1}{the n1 x p design matrix for the exposed
#'   subjects} \item{X_train}{the training design matrix for all subjects}
#'   \item{X_test}{the test set design matrix for all subjects}
#'   \item{Y_train}{the training set response} \item{Y_test}{the test set
#'   response} \item{DT_train}{the training response and training design matrix
#'   in a single data.frame object} \item{DT_test}{the test response and
#'   training design matrix in a single data.frame object} \item{S0}{a character
#'   vector of the active genes i.e. the ones that are associated with the
#'   response} \item{n_clusters_All}{the number of clusters identified by using
#'   the similarity matrix specified by the \code{cluster_distance} argument}
#'   \item{n_clusters_Eclust}{the number of clusters identified by using the
#'   similarity matrix specified by the \code{eclust_distance}
#'   argument}\item{n_clusters_Addon}{the sum of \code{n_clusters_All} and
#'   \code{n_clusters_Eclust}} \item{clustersAll}{the cluster membership of each
#'   gene based on the \code{cluster_distance} matrix} \item{clustersAddon}{the
#'   cluster membership of each gene based on both the \code{cluster_distance}
#'   matrix and the \code{eclust_distance} matrix. Note that each gene will
#'   appear twice here}\item{clustersEclust}{the cluster membership of each gene
#'   based on the \code{eclust_distance} matrix}
#'   \item{gene_groups_inter}{cluster membership of each gene with a penalty
#'   factor used for the group lasso} \item{gene_groups_inter_Addon}{cluster
#'   membership of each gene with a penalty factor used for the group lasso}
#'   \item{tom_train_all}{the TOM matrix based on all training subjects}
#'   \item{tom_train_diff}{the absolute difference of the exposed and unexposed
#'   TOM matrices: \eqn{|TOM_{E=1} - TOM_{E=0}|}} \item{tom_train_e1}{the TOM
#'   matrix based on training exposed subjects only} \item{tom_train_e0}{the TOM
#'   matrix based on training unexposed subjects only} \item{corr_train_all}{the
#'   Pearson correlation matrix based on all training subjects}
#'   \item{corr_train_diff}{the absolute difference of the exposed and unexposed
#'   Pearson correlation matrices: \eqn{|Cor_{E=1} - Cor_{E=0}|}}
#'   \item{corr_train_e1}{the Pearson correlation matrix based on training
#'   exposed subjects only} \item{corr_train_e0}{the Pearson correlation matrix
#'   based on training unexposed subjects only}\item{fisherScore}{The fisher
#'   scoring matrix. see \code{\link{u_fisherZ}} for details}\item{corScor}{The
#'   correlation scoring matrix: \eqn{|Cor_{E=1} + Cor_{E=0} - 2|}}
#'   \item{mse_null}{The MSE for the null model}\item{DT_train_folds}{The 10
#'   training folds used for the stability measures}\item{X_train_folds}{The 10
#'   X training folds (the same as in DT_train_folds)}\item{Y_train_folds}{The
#'   10 Y training folds (the same as in DT_train_folds)} }
#' @param agglomeration_method the agglomeration method to be used. This should
#'   be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single",
#'   "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC)
#'   or "centroid" (= UPGMC).
#' @param p number of genes in design matrix
#' @param X gene expression matrix of size n x p using the
#'   \code{generate_blocks} function
#' @param beta true beta coefficient vector
#' @param binary_outcome Logical. Should a binary outcome be generated. Default
#'   is \code{FALSE}. See details on how a binary outcome is generated
#' @param n total number of subjects
#' @param n0 total number of subjects with E=0
#' @param signal_to_noise_ratio signal to noise ratio, default is 1
#' @param cluster_distance character representing which matrix from the training
#'   set that you want to use to cluster the genes. Must be one of the following
#'   \itemize{ \item corr, corr0, corr1, tom, tom0, tom1, diffcorr, difftom,
#'   corScor, tomScor, fisherScore }
#' @param eclust_distance character representing which matrix from the training
#'   set that you want to use to cluster the genes based on the environment. See
#'   \code{cluster_distance} for avaialble options. Should be different from
#'   \code{cluster_distance}. For example, if \code{cluster_distance=corr} and
#'   \code{EclustDistance=fisherScore}. That is, one should be based on
#'   correlations ignoring the environment, and the other should be based on
#'   correlations accounting for the environment. This function will always
#'   return this add on
#' @param distance_method  one of "euclidean","maximum","manhattan", "canberra",
#'   "binary","minkowski" to be passed to \code{\link[stats]{dist}} function.
#' @param include_interaction Should an interaction with the environment be
#'   generated as part of the response. Default is FALSE.
#' @param cluster_method Cluster the data using hierarchical clustering or
#'   prototype clustering. Defaults \code{cluster_method="hclust"}. Other option
#'   is \code{\link[protoclust]{protoclust}}, however this package must be
#'   installed before proceeding with this option
#' @param cut_method what method to use to cut the dendrogram. \code{'dynamic'}
#'   refers to \code{dynamicTreeCut} library. \code{'gap'} is Tibshirani's gap
#'   statistic \code{\link[cluster]{clusGap}} using the \code{'Tibs2001SEmax'}
#'   rule. \code{'fixed'} is a fixed number specified by the \code{n_clusters}
#'   argument
#' @param n_clusters Number of clusters specified by the user. Only applicable
#'   when \code{cut_method="fixed"}
#' @param nPC number of principal components to extract from each cluster.
#'   Default is 1. Only 1 or 2 is allowed.
#' @details To generate a binary outcome we first generate a continuous outcome
#'   Y which is \eqn{X^T \beta}, defined \eqn{p = 1/(1 + exp(-Y ))} and used
#'   this to generate a two-class outcome z with \eqn{Pr(z = 1) = p} and
#'   \eqn{Pr(z = 0) = 1 - p}.
#' @inheritParams u_cluster_similarity
#' @examples
#' library(magrittr)
#'
#' # simulation parameters
#' rho = 0.90; p = 500 ;SNR = 1 ; n = 200; n0 = n1 = 100 ; nActive = p*0.10 ; cluster_distance = "tom";
#' Ecluster_distance = "difftom"; rhoOther = 0.6; betaMean = 2;
#' alphaMean = 1; betaE = 3; distanceMethod = "euclidean"; clustMethod = "hclust";
#' cutMethod = "dynamic"; agglomerationMethod = "average"
#'
#' #in this simulation its blocks 3 and 4 that are important
#' #leaveOut:  optional specification of modules that should be left out
#' #of the simulation, that is their genes will be simulated as unrelated
#' #("grey"). This can be useful when simulating several sets, in some which a module
#' #is present while in others it is absent.
#' d0 <- s_modules(n = n0, p = p, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = n1, p = p, rho = rho, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' truemodule1 <- d1$setLabels
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'   magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'   magrittr::set_rownames(paste0("Subject",1:n))
#'
#' betaMainEffect <- vector("double", length = p)
#' betaMainInteractions <- vector("double", length = p)
#'
#' # the first nActive/2 in the 3rd block are active
#' betaMainEffect[which(truemodule1 %in% 3)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean - 0.1, betaMean + 0.1)
#'
#' # the first nActive/2 in the 4th block are active
#' betaMainEffect[which(truemodule1 %in% 4)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean+2 - 0.1, betaMean+2 + 0.1)
#' betaMainInteractions[which(betaMainEffect!=0)] <- runif(nActive, alphaMean - 0.1, alphaMean + 0.1)
#' beta <- c(betaMainEffect, betaE, betaMainInteractions)
#' \dontrun{
#' result <- s_generate_data(p = p, X = X,
#'                           beta = beta,
#'                           include_interaction = TRUE,
#'                           cluster_distance = cluster_distance,
#'                           n = n, n0 = n0,
#'                           eclust_distance = Ecluster_distance,
#'                           signal_to_noise_ratio = SNR,
#'                           distance_method = distanceMethod,
#'                           cluster_method = clustMethod,
#'                           cut_method = cutMethod,
#'                           agglomeration_method = agglomerationMethod,
#'                           nPC = 1)
#' names(result)
#' }
#' @export

s_generate_data <- function(p, X, beta, binary_outcome = FALSE,
                            cluster_distance = c("corr", "corr0", "corr1", "tom",
                                                 "tom0", "tom1", "diffcorr",
                                                 "difftom","corScor", "tomScor",
                                                 "fisherScore"),
                            n, n0, include_interaction = F,
                            signal_to_noise_ratio = 1,
                            eclust_distance = c("fisherScore", "corScor", "diffcorr",
                                                "difftom"),
                            cluster_method = c("hclust", "protoclust"),
                            cut_method = c("dynamic","gap", "fixed"),
                            distance_method = c("euclidean","maximum", "manhattan",
                                                "canberra", "binary", "minkowski"),
                            n_clusters,
                            agglomeration_method = c("complete", "average", "ward.D2",
                                                     "single", "ward.D", "mcquitty",
                                                     "median", "centroid"),
                            nPC = 1,
                            K.max = 10, B = 10) {

  # p = p; X = X ; beta = beta
  # n = n; n0 = n0
  # cluster_distance = "corr"
  # include_interaction = F
  # signal_to_noise_ratio = 0.5
  # cluster_method = "hclust" ; cut_method = "dynamic";agglomeration_method="complete";
  # distance_method = "euclidean"
  # eclust_distance = "diffcorr"; nPC = 1

  Stom = nClusters = cluster = gene = pf = N = NULL
  agglomeration_method <- match.arg(agglomeration_method)
  cut_method <- match.arg(cut_method)
  cluster_method <- match.arg(cluster_method)
  distance_method <- match.arg(distance_method)
  cluster_distance <- match.arg(cluster_distance)
  eclust_distance <- match.arg(eclust_distance)


  names(beta) <- if (include_interaction) {
    c(paste0("Gene",1:p),"E", paste0("Gene",1:p,":E"))
  } else c(paste0("Gene",1:p),"E")

  # total true beta vector: this includes all the betas for the genes, then the
  # environment beta, then their interactions if interaction is true.
  # This is used to calculate the model error. This is the same as beta,
  # but in matrix form
  beta_truth <- as.matrix(beta)

  # Gene names belonging to the active set
  S0 <- names(beta)[which(beta != 0)]

  n1 <- n - n0

  message("Creating data and simulating response")

  DT <- as.data.frame(s_response(n = n, n0 = n0, p = p, genes = X,
                                 binary_outcome = binary_outcome,
                                 include_interaction = include_interaction,
                                 E = c(rep(0,n0), rep(1, n1)),
                                 beta = beta,
                                 signal_to_noise_ratio = signal_to_noise_ratio))
  dim(DT)

  Y <- as.matrix(DT[,"Y"])

  #remove response from X0 and X1
  X0 <- as.matrix(DT[which(DT$E == 0),-1])
  X1 <- as.matrix(DT[which(DT$E == 1),-1])

  # partition-data
  trainIndex <- caret::createDataPartition(DT$E, p = .5, list = FALSE, times = 1)
  DT_train <- DT[trainIndex,]
  DT_test <- DT[-trainIndex,]

  # X_train and X_test contain the environment variable
  X_train <- DT_train[,-1] %>% as.matrix
  Y_train <- DT_train[, 1]
  X_test <- DT_test[,-1] %>% as.matrix
  Y_test <- DT_test[, 1]

  mse_null <- crossprod(mean(Y_test) - Y_test)/length(Y_test)

  # gene expression data
  genes_e0 <- DT_train[which(DT_train$E == 0),paste0("Gene",1:p)] %>% as.matrix
  genes_e1 <- DT_train[which(DT_train$E == 1),paste0("Gene",1:p)] %>% as.matrix
  genes_all <- rbind(genes_e0,genes_e1)

  message("Calculating similarity matrices")

  # gene expression data
  genes_all_test <- DT_test[,paste0("Gene",1:p)] %>% as.matrix

  corr_train_e0 <- WGCNA::cor(genes_e0)
  corr_train_e1 <- WGCNA::cor(genes_e1)
  corr_train_diff <- abs(corr_train_e1 - corr_train_e0)
  corr_train_all <- WGCNA::cor(genes_all)

  tom_train_e0 <- WGCNA::TOMsimilarityFromExpr(genes_e0)
  dimnames(tom_train_e0)[[1]] <- dimnames(corr_train_all)[[1]]
  dimnames(tom_train_e0)[[2]] <- dimnames(corr_train_all)[[2]]

  tom_train_e1 <- WGCNA::TOMsimilarityFromExpr(genes_e1)
  dimnames(tom_train_e1)[[1]] <- dimnames(corr_train_all)[[1]]
  dimnames(tom_train_e1)[[2]] <- dimnames(corr_train_all)[[2]]

  tom_train_diff <- abs(tom_train_e1 - tom_train_e0)
  dimnames(tom_train_diff)[[1]] <- dimnames(corr_train_all)[[1]]
  dimnames(tom_train_diff)[[2]] <- dimnames(corr_train_all)[[2]]

  tom_train_all <- WGCNA::TOMsimilarityFromExpr(genes_all)
  dimnames(tom_train_all)[[1]] <- dimnames(corr_train_all)[[1]]
  dimnames(tom_train_all)[[2]] <- dimnames(corr_train_all)[[2]]


  # corScor and Fisher Score matrices
  alpha <- 2
  Scorr <- abs(corr_train_e0 + corr_train_e1 - alpha * corr_train_all)
  class(Scorr) <- c("similarity", class(Scorr))

  # Stom <- abs(tom_train_e1 + tom_train_e0 - alpha * tom_train_all)
  # class(Stom) <- c("similarity", class(Stom))

  fisherScore <- u_fisherZ(n0 = n0, cor0 = corr_train_e0,
                         n1 = n1, cor1 = corr_train_e1)

  class(tom_train_all) <- append(class(tom_train_all), "similarity")
  class(tom_train_diff) <- append(class(tom_train_diff), "similarity")
  class(tom_train_e1) <- append(class(tom_train_e1), "similarity")
  class(tom_train_e0) <- append(class(tom_train_e0), "similarity")
  class(corr_train_all) <- append(class(corr_train_all), "similarity")
  class(corr_train_diff) <- append(class(corr_train_diff), "similarity")
  class(corr_train_e1) <- append(class(corr_train_e1), "similarity")
  class(corr_train_e0) <- append(class(corr_train_e0), "similarity")

  message("Creating CV folds from training data")

  # Folds for Cross validation
  folds_train <- caret::createFolds(Y_train, k = 10, list = T)
  DT_train_folds <- lapply(folds_train, function(i) DT_train[-i,])
  X_train_folds <- lapply(DT_train_folds, function(i) i[,-grep("Y",colnames(i))])
  Y_train_folds <- lapply(DT_train_folds, function(i) i[,grep("Y",colnames(i))])

  message(sprintf("Calculating number of clusters based on %s using %s with %s
                  linkage and the %s to determine the number of clusters",
                  cluster_distance, cluster_method, agglomeration_method, cut_method))

  # clusters based on cluster_distance argument
  similarity <- switch(cluster_distance,
                       corr = corr_train_all,
                       corr0 = corr_train_e0,
                       corr1 = corr_train_e1,
                       diffcorr = corr_train_diff,
                       difftom = tom_train_diff,
                       tom0 = tom_train_e0,
                       tom1 = tom_train_e1,
                       tom = tom_train_all,
                       corScor = Scorr,
                       tomScor = Stom,
                       fisherScore = fisherScore)

  # results for clustering, PCs and averages for each block
  # the only difference here is the distance_method arg
  res <- if (cluster_distance %in% c("diffcorr","difftom",
                                     "corScor", "tomScor","fisherScore")) {
    u_cluster_similarity(x = similarity,
                       expr = genes_all,
                       exprTest = genes_all_test,
                       distanceMethod = distance_method,
                       clustMethod = cluster_method,
                       cutMethod = cut_method,
                       method = agglomeration_method,
                       K.max = K.max, B = B, nClusters = nClusters, nPC = nPC)
  } else {
    u_cluster_similarity(x = similarity,
                       expr = genes_all,
                       exprTest = genes_all_test,
                       clustMethod = cluster_method,
                       cutMethod = cut_method,
                       method = agglomeration_method,
                       K.max = K.max, B = B, nClusters = nClusters, nPC = nPC)
  }

  message(paste("Calculating number of environment clusters based on ",
                eclust_distance))

  # clusters based on eclust_distance
  similarityEclust <- switch(eclust_distance,
                             corr = corr_train_all,
                             corr0 = corr_train_e0,
                             corr1 = corr_train_e1,
                             diffcorr = corr_train_diff,
                             difftom = tom_train_diff,
                             tom0 = tom_train_e0,
                             tom1 = tom_train_e1,
                             tom = tom_train_all,
                             corScor = Scorr,
                             tomScor = Stom,
                             fisherScore = fisherScore)


  resEclust <- if (eclust_distance %in% c("diffcorr","difftom",
                                          "corScor", "tomScor","fisherScore")) {
    u_cluster_similarity(x = similarityEclust,
                       expr = genes_all,
                       exprTest = genes_all_test,
                       distanceMethod = distance_method,
                       clustMethod = cluster_method,
                       cutMethod = cut_method,
                       method = agglomeration_method,
                       K.max = K.max, B = B, nClusters = nClusters, nPC = nPC)
  } else {
    u_cluster_similarity(x = similarityEclust,
                       expr = genes_all,
                       exprTest = genes_all_test,
                       clustMethod = cluster_method,
                       cutMethod = cut_method,
                       method = agglomeration_method,
                       K.max = K.max, B = B, nClusters = nClusters, nPC = nPC)
  }


  # we need to combine the cluster information here
  # this is based on cluster_distance only
  clustersAll <- copy(res$clusters)
  n_clusters_All <- res$pcInfo$nclusters

  message(sprintf("There are %d clusters derived from the %s similarity matrix",
                  n_clusters_All, cluster_distance))

  # this is based on eclust_distance only
  n_clusters_Eclust <- resEclust$pcInfo$nclusters
  clustersEclust <- copy(resEclust$clusters)

  message(sprintf("There are %d clusters derived from the %s environment similarity matrix",
                  n_clusters_Eclust, eclust_distance))

  # this is based on both
  n_clusters_Addon <- n_clusters_All + n_clusters_Eclust

  message(sprintf("There are a total of %d clusters derived from the %s
                  similarity matrix and the %s environment similarity matrix",
                  n_clusters_Addon,cluster_distance,eclust_distance))

  # check if any of the cluster numbers in clustersEclust are 0
  # if there are, then add n_clusters+1 to each module number in
  # clustersEclust, else just add n_clusters. this is to control for the
  # situation where there are some clusters numbers of 0 which would cause
  # identical cluster numbers in the clusters and clustersEclust data
  if (clustersEclust[,any(cluster==0)]) {
    clustersEclust[,cluster := cluster + n_clusters_All + 1 ]
  } else {
    clustersEclust[,cluster := cluster + n_clusters_All ]
  }

  # this contains the clusters from the cluster_distance (e.g. corr matrix)
  # and the clusters from the eclust_distance (e.g. fisherScore)
  clustersAddon <- rbindlist(list(clustersAll, clustersEclust))

  # need to calculate penalty factors for group lasso
  # I put all main effects and interactions of a given module in the same group
  # and the size of the penalty factor is sqrt(size of module), where the
  # size of the module includes both main and interaction effects
  # environment should get penalized, in the original simulation 1
  # it was not being penalized which is maybe why it was performing well
  if (include_interaction) {

    gene_groups = copy(clustersAll)
    gene_groups[, gene := paste0(gene,":E")]
    gene_groups <- rbind(clustersAll,gene_groups) %>% setkey(cluster)

    pf_temp <- gene_groups[,.N, by = cluster][,pf := sqrt(N)] %>% setkey(cluster)

    gene_groups_inter <- rbind(pf_temp[gene_groups],
                               data.table(cluster = n_clusters_All, N = 1,
                                          pf = 1, gene = "E", module = "empty"))
    # gglasso needs groups number consecutively 1, 2,3 ...
    gene_groups_inter[, cluster:=cluster+1]
    setkey(gene_groups_inter, cluster)

    gene_groups_Addon = copy(clustersAddon)
    gene_groups_Addon[, gene := paste0(gene,":E")]
    gene_groups_Addon <- rbind(clustersAddon, gene_groups_Addon) %>% setkey(cluster)

    pf_temp_Addon <- gene_groups_Addon[,.N, by = cluster][,pf := sqrt(N)] %>% setkey(cluster)

    gene_groups_inter_Addon <- rbind(pf_temp_Addon[gene_groups_Addon],
                                     data.table(cluster = n_clusters_Addon, N = 1,
                                                pf = 1, gene = "E", module = "empty"))
    # gglasso needs groups number consecutively 1, 2,3 ...
    gene_groups_inter_Addon[, cluster:=cluster+1]
    setkey(gene_groups_inter_Addon, cluster)
  }

  DT <- DT %>% as.matrix
  class(DT) <- append(class(DT),"eset")

  result <- list(beta_truth = beta_truth,
                 similarity = similarity,
                 similarityEclust = similarityEclust,
                 DT = DT,
                 Y = Y, X0 = X0, X1 = X1, X_train = X_train, X_test = X_test,
                 Y_train = Y_train, Y_test = Y_test, DT_train = DT_train,
                 DT_test = DT_test, S0 = S0,
                 n_clusters_All = n_clusters_All,
                 n_clusters_Eclust = n_clusters_Eclust,
                 n_clusters_Addon = n_clusters_Addon,
                 clustersAll = clustersAll,
                 clustersAddon = clustersAddon,
                 clustersEclust = clustersEclust,
                 gene_groups_inter = if (include_interaction) gene_groups_inter else NULL,
                 gene_groups_inter_Addon = if (include_interaction) gene_groups_inter_Addon else NULL,
                 tom_train_all = tom_train_all, tom_train_diff = tom_train_diff,
                 tom_train_e1 = tom_train_e1,tom_train_e0 = tom_train_e0,
                 corr_train_all = corr_train_all,
                 corr_train_diff = corr_train_diff,
                 corr_train_e1 = corr_train_e1, corr_train_e0 = corr_train_e0,
                 fisherScore = fisherScore,
                 corScor = Scorr,
                 # corTom = Stom,
                 mse_null = mse_null, DT_train_folds = DT_train_folds,
                 X_train_folds = X_train_folds, Y_train_folds = Y_train_folds)
  return(result)
}



#' Generate non linear response and test and training sets for non-linear
#' simulation study
#'
#' @description create a function that takes as input, the number of genes, the
#'   true beta vector, the gene expression matrix created from the
#'   generate_blocks function and returns a list of data matrix, as well as
#'   correlation matrices, TOM matrices, cluster information, training and test
#'   data
#'
#' @param truemodule numeric vector of the true module membership used in the
#'   \code{s_response_mars} function. Modules 3 and 4 are active in the
#'   response. See \code{s_response_mars} function for details.
#' @param nActive number of active genes in the response used in the
#'   \code{s_response_mars}
#' @inheritParams u_cluster_similarity
#' @inheritParams s_generate_data
#' @return list of (in the following order) \describe{ \item{beta_truth}{a 1
#'   column matrix containing the true beta coefficient vector}
#'   \item{similarity}{an object of class similarity which is the similarity
#'   matrix specified by the \code{cluster_distance}
#'   argument}\item{similarityEclust}{an object of class similarity which is the
#'   similarity matrix specified by the \code{eclust_distance} argument}
#'   \item{DT}{data.table of simulated data from the \code{s_response} function}
#'   \item{Y}{The simulated response} \item{X0}{the n0 x p design matrix for the
#'   unexposed subjects} \item{X1}{the n1 x p design matrix for the exposed
#'   subjects} \item{X_train}{the training design matrix for all subjects}
#'   \item{X_test}{the test set design matrix for all subjects}
#'   \item{Y_train}{the training set response} \item{Y_test}{the test set
#'   response} \item{DT_train}{the training response and training design matrix
#'   in a single data.frame object} \item{DT_test}{the test response and
#'   training design matrix in a single data.frame object} \item{S0}{a character
#'   vector of the active genes i.e. the ones that are associated with the
#'   response} \item{n_clusters_All}{the number of clusters identified by using
#'   the similarity matrix specified by the \code{cluster_distance} argument}
#'   \item{n_clusters_Eclust}{the number of clusters identified by using the
#'   similarity matrix specified by the \code{eclust_distance}
#'   argument}\item{n_clusters_Addon}{the sum of \code{n_clusters_All} and
#'   \code{n_clusters_Eclust}} \item{clustersAll}{the cluster membership of each
#'   gene based on the \code{cluster_distance} matrix} \item{clustersAddon}{the
#'   cluster membership of each gene based on both the \code{cluster_distance}
#'   matrix and the \code{eclust_distance} matrix. Note that each gene will
#'   appear twice here}\item{clustersEclust}{the cluster membership of each gene
#'   based on the \code{eclust_distance} matrix}
#'   \item{gene_groups_inter}{cluster membership of each gene with a penalty
#'   factor used for the group lasso} \item{gene_groups_inter_Addon}{cluster
#'   membership of each gene with a penalty factor used for the group lasso}
#'   \item{tom_train_all}{the TOM matrix based on all training subjects}
#'   \item{tom_train_diff}{the absolute difference of the exposed and unexposed
#'   TOM matrices: \eqn{|TOM_{E=1} - TOM_{E=0}|}} \item{tom_train_e1}{the TOM
#'   matrix based on training exposed subjects only} \item{tom_train_e0}{the TOM
#'   matrix based on training unexposed subjects only} \item{corr_train_all}{the
#'   Pearson correlation matrix based on all training subjects}
#'   \item{corr_train_diff}{the absolute difference of the exposed and unexposed
#'   Pearson correlation matrices: \eqn{|Cor_{E=1} - Cor_{E=0}|}}
#'   \item{corr_train_e1}{the Pearson correlation matrix based on training
#'   exposed subjects only} \item{corr_train_e0}{the Pearson correlation matrix
#'   based on training unexposed subjects only}\item{fisherScore}{The fisher
#'   scoring matrix. see \code{\link{u_fisherZ}} for details}\item{corScor}{The
#'   correlation scoring matrix: \eqn{|Cor_{E=1} + Cor_{E=0} - 2|}}
#'   \item{mse_null}{The MSE for the null model}\item{DT_train_folds}{The 10
#'   training folds used for the stability measures}\item{X_train_folds}{The 10
#'   X training folds (the same as in DT_train_folds)}\item{Y_train_folds}{The
#'   10 Y training folds (the same as in DT_train_folds)} }
#' @export
#' @examples
#' library(magrittr)
#'
#' # simulation parameters
#' rho = 0.90; p = 500 ;SNR = 1 ; n = 200; n0 = n1 = 100 ; nActive = p*0.10 ; cluster_distance = "tom";
#' Ecluster_distance = "difftom"; rhoOther = 0.6; betaMean = 2;
#' alphaMean = 1; betaE = 3; distanceMethod = "euclidean"; clustMethod = "hclust";
#' cutMethod = "dynamic"; agglomerationMethod = "average"
#'
#' #in this simulation its blocks 3 and 4 that are important
#' #leaveOut:  optional specification of modules that should be left out
#' #of the simulation, that is their genes will be simulated as unrelated
#' #("grey"). This can be useful when simulating several sets, in some which a module
#' #is present while in others it is absent.
#' d0 <- s_modules(n = n0, p = p, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = n1, p = p, rho = rho, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' truemodule1 <- d1$setLabels
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'   magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'   magrittr::set_rownames(paste0("Subject",1:n))
#'
#' betaMainEffect <- vector("double", length = p)
#'
#' # the first nActive/2 in the 3rd block are active
#' betaMainEffect[which(truemodule1 %in% 3)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean - 0.1, betaMean + 0.1)
#'
#' # the first nActive/2 in the 4th block are active
#' betaMainEffect[which(truemodule1 %in% 4)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean+2 - 0.1, betaMean+2 + 0.1)
#' beta <- c(betaMainEffect, betaE)
#'
#' result <- s_generate_data_mars(p = p, X = X,
#'                                beta = beta,
#'                                binary_outcome = FALSE,
#'                                truemodule = truemodule1,
#'                                nActive = nActive,
#'                                include_interaction = FALSE,
#'                                cluster_distance = cluster_distance,
#'                                n = n, n0 = n0,
#'                                eclust_distance = Ecluster_distance,
#'                                signal_to_noise_ratio = SNR,
#'                                distance_method = distanceMethod,
#'                                cluster_method = clustMethod,
#'                                cut_method = cutMethod,
#'                                agglomeration_method = agglomerationMethod,
#'                                nPC = 1)
#' names(result)
s_generate_data_mars <- function(p, X, beta,  binary_outcome = FALSE,
                                 truemodule,
                                 nActive,
                                 cluster_distance = c("corr", "corr0", "corr1", "tom",
                                                      "tom0", "tom1", "diffcorr",
                                                      "difftom","corScor", "tomScor",
                                                      "fisherScore"),
                                 n, n0, include_interaction = F,
                                 signal_to_noise_ratio = 1,
                                 eclust_distance = c("fisherScore", "corScor", "diffcorr",
                                                     "difftom"),
                                 cluster_method = c("hclust", "protoclust"),
                                 cut_method = c("dynamic","gap", "fixed"),
                                 distance_method = c("euclidean","maximum", "manhattan",
                                                     "canberra", "binary", "minkowski"),
                                 n_clusters,
                                 agglomeration_method = c("complete", "average", "ward.D2",
                                                          "single", "ward.D", "mcquitty",
                                                          "median", "centroid"),
                                 nPC = 1,
                                 K.max = 10, B = 10) {

  # p = p; X = X ; beta = beta
  # n = n; n0 = n0
  # cluster_distance = "corr"
  # include_interaction = F
  # signal_to_noise_ratio = 0.5
  # cluster_method = "hclust" ; cut_method = "dynamic";agglomeration_method="complete";
  # distance_method = "euclidean"
  # eclust_distance = "diffcorr"; nPC = 1


  Stom = nClusters = cluster = gene = pf = N = NULL

  agglomeration_method <- match.arg(agglomeration_method)
  cut_method <- match.arg(cut_method)
  cluster_method <- match.arg(cluster_method)
  distance_method <- match.arg(distance_method)
  cluster_distance <- match.arg(cluster_distance)
  eclust_distance <- match.arg(eclust_distance)


  names(beta) <- if (include_interaction) {
    c(paste0("Gene",1:p),"E", paste0("Gene",1:p,":E"))
  } else c(paste0("Gene",1:p),"E")

  # total true beta vector: this includes all the betas for the genes, then the
  # environment beta, then their interactions if interaction is true.
  # This is used to calculate the model error. This is the same as beta,
  # but in matrix form
  beta_truth <- as.matrix(beta)

  # Gene names belonging to the active set
  S0 <- names(beta)[which(beta != 0)]

  n1 <- n - n0

  message("Creating data and simulating response for MARS model")

  DT <- as.data.frame(s_response_mars(n = n, n0 = n0, p = p, genes = X,
                                    binary_outcome = binary_outcome,
                                    beta = beta,
                                    truemodule = truemodule,
                                    nActive = nActive,
                                    E = c(rep(0,n0), rep(1, n1)),
                                    signal_to_noise_ratio = signal_to_noise_ratio))
  dim(DT)

  Y <- as.matrix(DT[,"Y"])

  #remove response from X0 and X1
  X0 <- as.matrix(DT[which(DT$E == 0),-1])
  X1 <- as.matrix(DT[which(DT$E == 1),-1])

  # partition-data
  trainIndex <- caret::createDataPartition(DT$E, p = .5, list = FALSE, times = 1)
  DT_train <- DT[trainIndex,]
  DT_test <- DT[-trainIndex,]

  # X_train and X_test contain the environment variable
  X_train <- DT_train[,-1] %>% as.matrix
  Y_train <- DT_train[, 1]
  X_test <- DT_test[,-1] %>% as.matrix
  Y_test <- DT_test[, 1]

  mse_null <- crossprod(mean(Y_test) - Y_test)/length(Y_test)

  # gene expression data
  genes_e0 <- DT_train[which(DT_train$E == 0),paste0("Gene",1:p)] %>% as.matrix
  genes_e1 <- DT_train[which(DT_train$E == 1),paste0("Gene",1:p)] %>% as.matrix
  genes_all <- rbind(genes_e0,genes_e1)

  message("Calculating similarity matrices")

  # gene expression data
  genes_all_test <- DT_test[,paste0("Gene",1:p)] %>% as.matrix

  corr_train_e0 <- WGCNA::cor(genes_e0)
  corr_train_e1 <- WGCNA::cor(genes_e1)
  corr_train_diff <- abs(corr_train_e1 - corr_train_e0)
  corr_train_all <- WGCNA::cor(genes_all)

  tom_train_e0 <- WGCNA::TOMsimilarityFromExpr(genes_e0)
  dimnames(tom_train_e0)[[1]] <- dimnames(corr_train_all)[[1]]
  dimnames(tom_train_e0)[[2]] <- dimnames(corr_train_all)[[2]]

  tom_train_e1 <- WGCNA::TOMsimilarityFromExpr(genes_e1)
  dimnames(tom_train_e1)[[1]] <- dimnames(corr_train_all)[[1]]
  dimnames(tom_train_e1)[[2]] <- dimnames(corr_train_all)[[2]]

  tom_train_diff <- abs(tom_train_e1 - tom_train_e0)
  dimnames(tom_train_diff)[[1]] <- dimnames(corr_train_all)[[1]]
  dimnames(tom_train_diff)[[2]] <- dimnames(corr_train_all)[[2]]

  tom_train_all <- WGCNA::TOMsimilarityFromExpr(genes_all)
  dimnames(tom_train_all)[[1]] <- dimnames(corr_train_all)[[1]]
  dimnames(tom_train_all)[[2]] <- dimnames(corr_train_all)[[2]]




  # corScor and Fisher Score matrices
  alpha <- 2
  Scorr <- abs(corr_train_e0 + corr_train_e1 - alpha * corr_train_all)
  class(Scorr) <- c("similarity", class(Scorr))

  # Stom <- abs(tom_train_e1 + tom_train_e0 - alpha * tom_train_all)
  # class(Stom) <- c("similarity", class(Stom))

  fisherScore <- u_fisherZ(n0 = n0, cor0 = corr_train_e0,
                         n1 = n1, cor1 = corr_train_e1)

  class(tom_train_all) <- append(class(tom_train_all), "similarity")
  class(tom_train_diff) <- append(class(tom_train_diff), "similarity")
  class(tom_train_e1) <- append(class(tom_train_e1), "similarity")
  class(tom_train_e0) <- append(class(tom_train_e0), "similarity")
  class(corr_train_all) <- append(class(corr_train_all), "similarity")
  class(corr_train_diff) <- append(class(corr_train_diff), "similarity")
  class(corr_train_e1) <- append(class(corr_train_e1), "similarity")
  class(corr_train_e0) <- append(class(corr_train_e0), "similarity")

  message("Creating CV folds from training data")

  # Folds for Cross validation
  folds_train <- caret::createFolds(Y_train, k = 10, list = T)
  DT_train_folds <- lapply(folds_train, function(i) DT_train[-i,])
  X_train_folds <- lapply(DT_train_folds, function(i) i[,-grep("Y",colnames(i))])
  Y_train_folds <- lapply(DT_train_folds, function(i) i[,grep("Y",colnames(i))])

  message(sprintf("Calculating number of clusters based on %s using %s with %s
                  linkage and the %s to determine the number of clusters",
                  cluster_distance, cluster_method, agglomeration_method, cut_method))

  # clusters based on cluster_distance argument
  similarity <- switch(cluster_distance,
                       corr = corr_train_all,
                       corr0 = corr_train_e0,
                       corr1 = corr_train_e1,
                       diffcorr = corr_train_diff,
                       difftom = tom_train_diff,
                       tom0 = tom_train_e0,
                       tom1 = tom_train_e1,
                       tom = tom_train_all,
                       corScor = Scorr,
                       tomScor = Stom,
                       fisherScore = fisherScore)

  # results for clustering, PCs and averages for each block
  # the only difference here is the distance_method arg
  res <- if (cluster_distance %in% c("diffcorr","difftom",
                                     "corScor", "tomScor","fisherScore")) {
    u_cluster_similarity(x = similarity,
                         expr = genes_all,
                         exprTest = genes_all_test,
                         distanceMethod = distance_method,
                         clustMethod = cluster_method,
                         cutMethod = cut_method,
                         method = agglomeration_method,
                         K.max = K.max, B = B, nClusters = nClusters, nPC = nPC)
  } else {
    u_cluster_similarity(x = similarity,
                         expr = genes_all,
                         exprTest = genes_all_test,
                         clustMethod = cluster_method,
                         cutMethod = cut_method,
                         method = agglomeration_method,
                         K.max = K.max, B = B, nClusters = nClusters, nPC = nPC)
  }

  message(paste("Calculating number of environment clusters based on ",
                eclust_distance))

  # clusters based on eclust_distance
  similarityEclust <- switch(eclust_distance,
                             corr = corr_train_all,
                             corr0 = corr_train_e0,
                             corr1 = corr_train_e1,
                             diffcorr = corr_train_diff,
                             difftom = tom_train_diff,
                             tom0 = tom_train_e0,
                             tom1 = tom_train_e1,
                             tom = tom_train_all,
                             corScor = Scorr,
                             tomScor = Stom,
                             fisherScore = fisherScore)


  resEclust <- if (eclust_distance %in% c("diffcorr","difftom",
                                          "corScor", "tomScor","fisherScore")) {
    u_cluster_similarity(x = similarityEclust,
                         expr = genes_all,
                         exprTest = genes_all_test,
                         distanceMethod = distance_method,
                         clustMethod = cluster_method,
                         cutMethod = cut_method,
                         method = agglomeration_method,
                         K.max = K.max, B = B, nClusters = nClusters, nPC = nPC)
  } else {
    u_cluster_similarity(x = similarityEclust,
                         expr = genes_all,
                         exprTest = genes_all_test,
                         clustMethod = cluster_method,
                         cutMethod = cut_method,
                         method = agglomeration_method,
                         K.max = K.max, B = B, nClusters = nClusters, nPC = nPC)
  }


  # we need to combine the cluster information here
  # this is based on cluster_distance only
  clustersAll <- copy(res$clusters)
  n_clusters_All <- res$pcInfo$nclusters

  message(sprintf("There are %d clusters derived from the %s similarity matrix",
                  n_clusters_All, cluster_distance))

  # this is based on eclust_distance only
  n_clusters_Eclust <- resEclust$pcInfo$nclusters
  clustersEclust <- copy(resEclust$clusters)

  message(sprintf("There are %d clusters derived from the %s environment similarity matrix",
                  n_clusters_Eclust, eclust_distance))

  # this is based on both
  n_clusters_Addon <- n_clusters_All + n_clusters_Eclust

  message(sprintf("There are a total of %d clusters derived from the %s
                  similarity matrix and the %s environment similarity matrix",
                  n_clusters_Addon,cluster_distance,eclust_distance))

  # check if any of the cluster numbers in clustersEclust are 0
  # if there are, then add n_clusters+1 to each module number in
  # clustersEclust, else just add n_clusters. this is to control for the
  # situation where there are some clusters numbers of 0 which would cause
  # identical cluster numbers in the clusters and clustersEclust data
  if (clustersEclust[,any(cluster==0)]) {
    clustersEclust[,cluster := cluster + n_clusters_All + 1 ]
  } else {
    clustersEclust[,cluster := cluster + n_clusters_All ]
  }

  # this contains the clusters from the cluster_distance (e.g. corr matrix)
  # and the clusters from the eclust_distance (e.g. fisherScore)
  clustersAddon <- rbindlist(list(clustersAll, clustersEclust))

  # need to calculate penalty factors for group lasso
  # I put all main effects and interactions of a given module in the same group
  # and the size of the penalty factor is sqrt(size of module), where the
  # size of the module includes both main and interaction effects
  # environment should get penalized, in the original simulation 1
  # it was not being penalized which is maybe why it was performing well
  if (include_interaction) {

    gene_groups = copy(clustersAll)
    gene_groups[, gene := paste0(gene,":E")]
    gene_groups <- rbind(clustersAll,gene_groups) %>% setkey(cluster)

    pf_temp <- gene_groups[,.N, by = cluster][,pf := sqrt(N)] %>% setkey(cluster)

    gene_groups_inter <- rbind(pf_temp[gene_groups],
                               data.table(cluster = n_clusters_All, N = 1,
                                          pf = 1, gene = "E", module = "empty"))
    # gglasso needs groups number consecutively 1, 2,3 ...
    gene_groups_inter[, cluster:=cluster+1]
    setkey(gene_groups_inter, cluster)

    gene_groups_Addon = copy(clustersAddon)
    gene_groups_Addon[, gene := paste0(gene,":E")]
    gene_groups_Addon <- rbind(clustersAddon, gene_groups_Addon) %>% setkey(cluster)

    pf_temp_Addon <- gene_groups_Addon[,.N, by = cluster][,pf := sqrt(N)] %>% setkey(cluster)

    gene_groups_inter_Addon <- rbind(pf_temp_Addon[gene_groups_Addon],
                                     data.table(cluster = n_clusters_Addon, N = 1,
                                                pf = 1, gene = "E", module = "empty"))
    # gglasso needs groups number consecutively 1, 2,3 ...
    gene_groups_inter_Addon[, cluster:=cluster+1]
    setkey(gene_groups_inter_Addon, cluster)
  }

  DT <- DT %>% as.matrix
  class(DT) <- append(class(DT),"eset")

  result <- list(beta_truth = beta_truth,
                 similarity = similarity,
                 similarityEclust = similarityEclust,
                 DT = DT,
                 Y = Y, X0 = X0, X1 = X1, X_train = X_train, X_test = X_test,
                 Y_train = Y_train, Y_test = Y_test, DT_train = DT_train,
                 DT_test = DT_test, S0 = S0,
                 n_clusters_All = n_clusters_All,
                 n_clusters_Eclust = n_clusters_Eclust,
                 n_clusters_Addon = n_clusters_Addon,
                 clustersAll = clustersAll,
                 clustersAddon = clustersAddon,
                 clustersEclust = clustersEclust,
                 gene_groups_inter = if (include_interaction) gene_groups_inter else NULL,
                 gene_groups_inter_Addon = if (include_interaction) gene_groups_inter_Addon else NULL,
                 tom_train_all = tom_train_all, tom_train_diff = tom_train_diff,
                 tom_train_e1 = tom_train_e1,tom_train_e0 = tom_train_e0,
                 corr_train_all = corr_train_all,
                 corr_train_diff = corr_train_diff,
                 corr_train_e1 = corr_train_e1, corr_train_e0 = corr_train_e0,
                 fisherScore = fisherScore,
                 corScor = Scorr,
                 # corTom = Stom,
                 mse_null = mse_null, DT_train_folds = DT_train_folds,
                 X_train_folds = X_train_folds, Y_train_folds = Y_train_folds)
  return(result)
}





#' Generate True Response vector for Linear Simulation
#'
#' Given the true beta vector, covariates and environment variable this function
#' generates the linear response with specified signal to noise ratio.
#'
#'
#' @param n Total number of subjects
#' @param n0 Total number of unexposed subjects
#' @param p Total number of genes (or covariates)
#' @param genes nxp matrix of the genes or covariates
#' @param E binary 0,1, vector of the exposure/environment variable
#' @param signal_to_noise_ratio a numeric variable for the signal to noise ratio
#' @param include_interaction Logical. Should the response include the
#'   interaction between E and the genes (for the non-zero \code{beta}
#'   coefficient vector)
#' @param beta true beta coefficient vector. Assumes this vector is in the same
#'   order as the \code{genes}.
#' @return a data.frame/data.table containing the response and the design
#'   matrix. Also an object of class \code{expression}
#' @inheritParams s_generate_data
#' @export
#' @examples
#' library(magrittr)
#'
#' # simulation parameters
#' rho = 0.90; p = 500 ;SNR = 1 ; n = 200; n0 = n1 = 100 ; nActive = p*0.10 ; cluster_distance = "tom";
#' Ecluster_distance = "difftom"; rhoOther = 0.6; betaMean = 2;
#' alphaMean = 1; betaE = 3; distanceMethod = "euclidean"; clustMethod = "hclust";
#' cutMethod = "dynamic"; agglomerationMethod = "average"
#'
#' #in this simulation its blocks 3 and 4 that are important
#' #leaveOut:  optional specification of modules that should be left out
#' #of the simulation, that is their genes will be simulated as unrelated
#' #("grey"). This can be useful when simulating several sets, in some which a module
#' #is present while in others it is absent.
#' d0 <- s_modules(n = n0, p = p, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = n1, p = p, rho = rho, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' truemodule1 <- d1$setLabels
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'   magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'   magrittr::set_rownames(paste0("Subject",1:n))
#'
#' betaMainEffect <- vector("double", length = p)
#'
#' # the first nActive/2 in the 3rd block are active
#' betaMainEffect[which(truemodule1 %in% 3)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean - 0.1, betaMean + 0.1)
#'
#' # the first nActive/2 in the 4th block are active
#' betaMainEffect[which(truemodule1 %in% 4)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean+2 - 0.1, betaMean+2 + 0.1)
#' beta <- c(betaMainEffect, betaE)
#'
#' result <- s_response(n = n, n0 = n0,
#'                      p = p, genes = X, binary_outcome = FALSE,
#'                      E = c(rep(0,n0), rep(1, n1)), signal_to_noise_ratio = 1,
#'                      include_interaction = FALSE,
#'                      beta = beta)
#' result[1:5,1:5]
#'
s_response <- function(n , n0 , p , genes,  binary_outcome = FALSE,
                       E, signal_to_noise_ratio = 1,
                       include_interaction = FALSE,
                       beta = NULL) {

  # number of subjects with E=1
  #n1 = n - n0

  # not used for now
  # The coefficients are constructed to have alternating signs and to be exponentially
  # decreasing. (Friedman et al 2010, Journal of Statistical Software)
  # beta <- (-1)^{1:p} * exp(-(2*1:p-1)/100)
  # beta <- (-1)^{1:p} * exp(-(2*1:p - 1 )/600)*sin(1:p)

  #   genes = X
  #   signal_to_noise_ratio = 4
  #   n0 = n1 = 100
  #   E = c(rep(0,n0),rep(1, n1))
  #   beta = c(rnorm(1000),0, rep(0,1000));include_interaction = T
  #   beta = c(rnorm(1000));include_interaction = F

  if (include_interaction) {
    DT <- cbind(genes,E) %>% as.data.table()
    alloc.col(DT,2*p + 1) %>% invisible()
    indx <- grep('Gene', colnames(DT))

    for (j in indx){
      set(DT, i = NULL, j = paste0("Gene",j,":E"), value = DT[[j]]*DT[['E']])
    }
  } else {
    DT <- cbind(genes,E) %>% as.data.table()
  }

  y.star <- {DT %>% as.matrix()} %*% beta
  error <- stats::rnorm(n)
  k <- sqrt(stats::var(y.star)/(signal_to_noise_ratio*stats::var(error)))

  y <- y.star + k*error

  if (binary_outcome) {
    prob <- 1/(1+exp(-y))
    z <- stats::rbinom(nrow(prob),1,prob)
    result <- if (include_interaction) as.matrix(cbind(z,DT)) else as.matrix(cbind(z,DT))
    colnames(result)[1] <- "Y"
    class(result) <- append(class(result), "expression")
    return(result)
  } else {
    result <- if (include_interaction) as.matrix(cbind(y,DT)) else as.matrix(cbind(y,DT))
    colnames(result)[1] <- "Y"
    class(result) <- append(class(result), "expression")
    return(result)
  }
}


#' Generate True Response vector for Non-Linear Simulation
#'
#' Given the covariates and environment variable this function
#' generates the nonlinear response with specified signal to noise ratio.
#' @inheritParams s_generate_data_mars
#' @inheritParams s_response
#' @note See Bhatnagar et al (2017+) for details on how the response is simulated.
#' @return a data.frame/data.table containing the response and the design
#'   matrix. Also an object of class \code{expression}
#'
#'
#' @examples
#' library(magrittr)
#'
#' # simulation parameters
#' rho = 0.90; p = 500 ;SNR = 1 ; n = 200; n0 = n1 = 100 ; nActive = p*0.10 ; cluster_distance = "tom";
#' Ecluster_distance = "difftom"; rhoOther = 0.6; betaMean = 2;
#' alphaMean = 1; betaE = 3; distanceMethod = "euclidean"; clustMethod = "hclust";
#' cutMethod = "dynamic"; agglomerationMethod = "average"
#'
#' #in this simulation its blocks 3 and 4 that are important
#' #leaveOut:  optional specification of modules that should be left out
#' #of the simulation, that is their genes will be simulated as unrelated
#' #("grey"). This can be useful when simulating several sets, in some which a module
#' #is present while in others it is absent.
#' d0 <- s_modules(n = n0, p = p, rho = 0, exposed = FALSE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.01,
#'                 maxCor = 1,
#'                 corPower = 1,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE,
#'                 leaveOut = 1:4)
#'
#' d1 <- s_modules(n = n1, p = p, rho = rho, exposed = TRUE,
#'                 modProportions = c(0.15,0.15,0.15,0.15,0.15,0.25),
#'                 minCor = 0.4,
#'                 maxCor = 1,
#'                 corPower = 0.3,
#'                 propNegativeCor = 0.3,
#'                 backgroundNoise = 0.5,
#'                 signed = FALSE)
#'
#' truemodule1 <- d1$setLabels
#'
#' X <- rbind(d0$datExpr, d1$datExpr) %>%
#'   magrittr::set_colnames(paste0("Gene", 1:p)) %>%
#'   magrittr::set_rownames(paste0("Subject",1:n))
#'
#' betaMainEffect <- vector("double", length = p)
#'
#' # the first nActive/2 in the 3rd block are active
#' betaMainEffect[which(truemodule1 %in% 3)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean - 0.1, betaMean + 0.1)
#'
#' # the first nActive/2 in the 4th block are active
#' betaMainEffect[which(truemodule1 %in% 4)[1:(nActive/2)]] <- runif(
#'   nActive/2, betaMean+2 - 0.1, betaMean+2 + 0.1)
#' beta <- c(betaMainEffect, betaE)
#'
#' result <- s_response_mars(n = n, n0 = n0,
#'                           p = p, genes = X, binary_outcome = TRUE,
#'                           E = c(rep(0,n0), rep(1, n1)), signal_to_noise_ratio = 1,
#'                           truemodule = truemodule1, nActive = nActive,
#'                           beta = beta)
#' result[1:5,1:5]
#' @export
s_response_mars <- function(n , n0 , p , genes, beta,  binary_outcome = FALSE,
                            E, signal_to_noise_ratio = 1,
                            truemodule, nActive) {

  # truemodule = truemodule1
  # genes = X
  # E = c(rep(0,n0),rep(1, n1))
  # signal_to_noise_ratio = SNR

  DT <- cbind(genes,E) %>% as.data.table()

  x1 <- genes[,which(truemodule %in% 3)[1:(nActive/2)]]
  x2 <- genes[,which(truemodule %in% 4)[1:(nActive/2)]]

  # p causaully associated genes
  xp <- cbind(x1,x2)
  # all(c(colnames(x1),colnames(x2)) %in% colnames(xp))

  # means of the causal genes for each subject
  xpbar <- rowMeans(xp)

  # these are equivalent
  # apply(xp, 2, function(i) (i - xpbar)^2)[1:3,1:3]
  # ((xp - xpbar)[1:3,1:3])^2

  Qip <- (xp - xpbar)^2
  Qi <- -1*apply(Qip, 1, max)

  x_inter <- (Qi - min(Qi))/(-1 * min(Qi))

  x_main <- svd(xp)$u[,1]

  # plot(x_main, x_inter)
  # x1 <- genes[,which(truemodule %in% 3)[1:(nActive/2)]]
  # u1 <- svd(x1)$u[,1]
  #
  # x2 <- genes[,which(truemodule %in% 4)[1:(nActive/2)]]
  # u2 <- svd(x2)$u[,1]
  # y.star <- 0.1*(u1 + u2 + E) + 4 * pmax(u1-0.01, 0) * pmax(u2-0.05, 0) * E

  # y.star <- x_main + E + E * x_inter

  y.star <- {as.matrix(DT)} %*% beta + E * x_inter

  error <- stats::rnorm(n)
  k <- sqrt(stats::var(y.star)/(signal_to_noise_ratio*stats::var(error)))

  y <- y.star + k*error

  if (binary_outcome) {
    prob <- 1/(1+exp(-y))
    z <- stats::rbinom(nrow(prob),1,prob)

    result <- as.matrix(cbind(z,DT))
    colnames(result)[1] <- "Y"
    class(result) <- append(class(result), "expression")
    return(result)
  } else {
    result <- as.matrix(cbind(y,DT))
    colnames(result)[1] <- "Y"
    class(result) <- append(class(result), "expression")
    return(result)
  }
}


