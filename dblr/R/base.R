#' @import methods
#' @import xgboost
#' @import data.table
#' @import CatEncoders
#' @import Metrics

#' @title Discrete Boosting Logistic Regression Training
#' @description dblr_train fits a dblr (discrete boosting logistic regression) model.
#' @param train_x A data.frame of training variables, which can include NA as well
#' @param train_y A vector of 0 and 1 to represent labels of training samples
#' @param category_cols A vector of column names to indicate which columns are categorical. Default: NULL means all columns are continuous
#' @param metric Which metric to use, can be either auc or logloss. Default: auc
#' @param subsample Subsample ratio from the trainnig samples in each iteration. Default: 1.0
#' @param eta Controls the rate of learning. eta should be between 0 and 1. Default: 0.1
#' @param colsample Subsample ratio from all available variables/columns. Default: 1.0
#' @param cv_nfold Number of folds used for cross-validation. Default: 5
#' @param cv_nrounds Number of iterations used for cross-validation. Default: 1000
#' @param cv_early_stops Cross-validation would be stopped if there is no improvement after cv_early_stops iterations. Default: 25
#' @param lambda Control L2 regularization term. Default: 1.0
#' @param alpha Control L1 regularization term. Default: 0.0
#' @param scale_pos_weight Useful when training metric is set to auc for imbalanced training data
#' @param verbose Default: FALSE. If TRUE, the cross-validation process would be showed
#' @param seed Random seed for the sampling. Default: 123456
#' @details As one of the generalized linear models, traditional logistic regression on continuous variables implies that there is a monotonic relation between each predictor and the predicted probability. Bining or discretizing the continuous variables would be helpful when non-monotonic relation exists. In general, it is challenging to find the optimal binning for continuous variables. Too many bins may cause over-fitting and too few bins may not reveal the non-monotinc relation as much as possible. Thus, we propose to use a boosting decision trees to construct a discrete logistic regressions aiming at an automated binning process with good performance. Our algorithm is to construct  a sequence of gradient boosting decision trees with at most 1 variable in each tree. Aggregating all decision trees with the same variable would result in the corresponding bins and the coefficients. And by aggregating all trees without variables we would get the intercept. \cr \cr The model is defined as: \deqn{Pr(y=1|\bm{x}_i)=  \frac 1{1+{\exp (- \sum_{j=1}^{m}{g(\bm{x}_{i,j})}- b)}},} where \eqn{g(\bm{x}_{i,j})} denotes the coefficient of the bin which \eqn{\bm{x}_{i,j}} falls into and \eqn{b} denotes the intercept. Both coefficients and intercept are consolidated from boosting trees. More specifically, \deqn{g(\bm{x}_{i,j})=\sum_{k=1}^{K} f_k(\bm{x}_{i,j})\cdot I(\textrm{tree } k \textrm{ splits on variable } j),} \deqn{b=\sum_{k=1}^{K} f_k\cdot I(\textrm{tree } k \textrm{ does not split on any variable}),} where \eqn{K} is the total number of trees and \eqn{f_k} is the output value for tree \eqn{k}. In this package, we use xgboost package to training the underlying gradient boosting trees.
#' @return Returns an object of S3 class dblr, which contains two attributes, i.e., continuous_bins and categorical_bins.
#' @export
#' @docType methods
#' @rdname dblr_train
#' @examples
#' # use iris data for example
#' dat <- iris
#' # create two categorical variables
#' dat$Petal.Width <- as.factor((iris$Petal.Width<=0.2)*1+(iris$Petal.Width>1.0)*2)
#' dat$Sepal.Length <- (iris$Sepal.Length<=3.0)*2+(iris$Sepal.Length>6.0)*1.25
#' # create the response variable
#' dat$Species <- as.numeric(dat$Species=='versicolor')
#' set.seed(123)
#' # random sampling
#' index <- sample(1:150,100,replace = FALSE)
#' # train the dblr model using the training data
#' dblr_fit <- dblr_train(train_x=dat[index,c(1:4)],
#' train_y=dat[index,5],category_cols = c('Petal.Width','Sepal.Length'),
#' metric = 'logloss',subsample = 0.5,eta = 0.05,colsample = 1.0,
#' lambda = 1.0,cv_early_stops = 10,verbose=FALSE)
#' # make predictions on testing data
#' pred_dblr <- predict(dblr_fit,newdata = dat[-index,],type = 'response')
#' dblr_auc <- Metrics::auc(actual = dat[-index,'Species'],predicted = pred_dblr)
#' dblr_logloss <- Metrics::logLoss(actual = dat[-index,'Species'],predicted = pred_dblr)
#' cat('test auc for dblr model:',dblr_auc,'\n')
#' cat('test logloss for dblr model:',dblr_logloss,'\n')
#' glm_fit <- glm(data=dat[index,],formula =Species~. ,family = binomial)
#' pred_glm <- predict(glm_fit,newdata = dat[-index,],type='response')
#' glm_auc <- Metrics::auc(actual = dat[-index,'Species'],predicted = pred_glm)
#' glm_logloss <- Metrics::logLoss(actual = dat[-index,'Species'],predicted = pred_glm)
#' cat('test auc for glm model:',glm_auc,'\n')
#' cat('test logloss for glm model:',glm_logloss,'\n')
dblr_train <-
  function(train_x,
           train_y,
           category_cols = NULL,
           metric = 'auc',
           subsample = 1.0,
           eta = 0.1,
           colsample = 1.0,
           cv_nfold = 5,
           cv_nrounds = 1000,
           cv_early_stops = 25,
           lambda = 1.0,
           alpha = 0.0,
           scale_pos_weight = 1.0,
           verbose = FALSE,
           seed = 123456L) {
    if (!is.data.frame(train_x))
      stop("train_x must be a data.frame")
    if (length(unique(colnames(train_x))) < ncol(train_x))
      stop("train_x has duplicate column names")

    if (!(metric %in% c('logloss', 'auc')))
      stop ('metric must be either logloss or auc')
    Feature <- NULL
    value <- NULL
    Split <- NULL
    variable <- NULL
    break_point<- NULL
    coef <- NULL
    Tree <- NULL
    Quality <- NULL
    Yes <- NULL
    Missing <- NULL
    No <- NULL
    N <- NULL
    Quality.l <- NULL
    Quality.m <- NULL
    Quality.r <- NULL
    m <- NULL
    l <- NULL
    r <- NULL
    vl <- NULL
    vr <- NULL
    condition <- NULL

    if (is.null(category_cols)) {
      train_x_num <- train_x
      train_x_cat <- NULL
      feature_names <- colnames(train_x)
    } else{
      train_x_cat <- train_x[, category_cols, drop = FALSE]
      if (length(setdiff(colnames(train_x), category_cols)) == 0) {
        train_x_num <- NULL
        feature_names <- colnames(train_x_cat)
      } else {
        train_x_num <-
          train_x[, setdiff(colnames(train_x), category_cols), drop = FALSE]
        feature_names <-
          c(colnames(train_x_num), colnames(train_x_cat))
      }
    }

    if (!is.null(train_x_cat)) {
      lencs <- sapply(train_x_cat, CatEncoders::LabelEncoder.fit)
      train_x_cat_num <-
        sapply(1:ncol(train_x_cat), function(e) {
          CatEncoders::transform(lencs[[e]], train_x_cat[, e])
        })
      colnames(train_x_cat_num) <- colnames(train_x_cat)
      train_x_prepared <- cbind(train_x_num, train_x_cat_num) + 0.0
    } else
      train_x_prepared <- train_x_num + 0.0
    set.seed(seed)
    dtrain <-
      xgboost::xgb.DMatrix(data.matrix(train_x_prepared),
                           label = train_y,
                           missing = NA)
    params <-
      list(
        booster = 'gbtree',
        tree_method = 'exact',
        max_depth = 1,
        eta = eta,
        objective = 'reg:logistic',
        lambda = lambda,
        alpha = alpha,
        subsample = subsample,
        colsample_bytree = colsample,
        scale_pos_weight = scale_pos_weight
      )
    cv_fit <-
      xgboost::xgb.cv(
        params = params,
        data = dtrain,
        nrounds = cv_nrounds,
        nfold = cv_nfold,
        metrics = metric,
        verbose = verbose,
        early_stopping_rounds = cv_early_stops,
        maximize = if (metric == 'logloss')
          FALSE
        else
          TRUE
      )
    best_iter <- cv_fit$best_iteration
    fit <-
      xgboost::xgb.train(
        params = params,
        data = dtrain,
        nrounds = best_iter,
        metrics = metric,
        verbose = verbose,
        maximize = if (metric == 'logloss')
          {FALSE} else {TRUE}
      )
    trees <- xgboost::xgb.model.dt.tree(feature_names, model = fit)
    selected_variables <- trees[Feature!='Leaf',unique(Feature)]
    selected_con_variables <- intersect(selected_variables,setdiff(colnames(train_x),category_cols))
    selected_cat_variables <- intersect(selected_variables,category_cols)

    if (!is.null(train_x_cat)) {
      lencs <- lencs[names(lencs) %in% selected_cat_variables]}
    trees[, Split := as.numeric(Split)]
    trees_id_with_leaves <- trees[, .N, by = 'Tree'][N > 1, Tree]
    intercept <- trees[!(Tree %in% trees_id_with_leaves), sum(Quality)]
    trees <- trees[Tree %in% trees_id_with_leaves, ]
    trees_nodes_0 <-
      trees[Feature != 'Leaf'][, `:=`(l = as.integer(sapply(strsplit(Yes, '-'), function(e)
        e[2])),
        m = as.integer(sapply(strsplit(Missing, '-'), function(e)
          e[2])),
        r = as.integer(sapply(strsplit(No, '-'), function(e)
          e[2])))]
    trees_nodes_0 <-
      merge(
        trees_nodes_0,
        trees[, c('Tree', 'Node', 'Quality')],
        by.x = c('Tree', 'l'),
        by.y = c('Tree', 'Node'),
        suffixes = c('', '.l'),
        all.x = TRUE
      )
    trees_nodes_0 <-
      merge(
        trees_nodes_0,
        trees[, c('Tree', 'Node', 'Quality')],
        by.x = c('Tree', 'm'),
        by.y = c('Tree', 'Node'),
        suffixes = c('', '.m'),
        all.x = TRUE
      )
    trees_nodes_0 <-
      merge(
        trees_nodes_0,
        trees[, c('Tree', 'Node', 'Quality')],
        by.x = c('Tree', 'r'),
        by.y = c('Tree', 'Node'),
        suffixes = c('', '.r'),
        all.x = TRUE
      )
    data.table::setorderv(trees_nodes_0, c('Feature', 'Split'))
    trees_nodes_0 <-
      trees_nodes_0[, list(
        'l' = sum(Quality.l),
        'm' = sum(Quality.m),
        'r' = sum(Quality.r)
      ), by = c('Feature', 'Split')]
    missing_values_num <-
      trees_nodes_0[!(Feature %in% category_cols), list(missing = sum(m)), by =
                      'Feature']

    missing_values_cat <-
      trees_nodes_0[Feature %in% category_cols, list(missing = sum(m)), by =
                      'Feature']
    trees_nodes_0[, `:=`(vl = cumsum(l[.N:1])[.N:1], vr = cumsum(r)), by =
                    'Feature']
    trees_nodes_0[, c('l', 'm', 'r') := NULL]
    trees_nodes_0_inf = trees_nodes_0[, .SD[1], by = 'Feature']
    trees_nodes_0_inf[, `:=`(Split = Inf, vl = 0.0, vr = 0.0)]
    trees_nodes_0 <- rbind(trees_nodes_0, trees_nodes_0_inf)
    data.table::setorderv(trees_nodes_0, c('Feature', 'Split'))
    trees_nodes_0[, value := vl + data.table::shift(vr, 1, fill = 0.0), by =
                    'Feature']
    trees_nodes_0[, c('vl', 'vr') := NULL]
    if (!is.null(category_cols) &&
        length(selected_cat_variables) > 0) {
      categorical_bins = trees_nodes_0[Feature %in% selected_cat_variables]
      categorical_bins_ <- lapply(1:length(lencs), function(e) {
        name <- names(lencs)[[e]]
        dt <-
          data.table::data.table(
            Feature = name,
            Split = lencs[[e]]@mapping$classes,
            value = categorical_bins[Feature == name, value][findInterval(lencs[[e]]@mapping$ind,
                                                                          c(-Inf, categorical_bins[Feature == name &
                                                                                                     !is.na(Split), Split]),
                                                                          left.open = FALSE)]
          )
        if (!(NA %in% dt[Feature == name, Split])) {
          rdt <- rbind(
            dt,
            data.table::data.table(
              Feature = name,
              Split = NA,
              value = missing_values_cat[Feature == name, missing]
            )
          )
          rdt[, condition := '=']
          colnames(rdt) <-
            c('variable', 'break_point', 'coef', 'condition')
          data.table::setorderv(rdt, c('variable', 'break_point'))
          rdt
        } else{
          dt[, condition := '=']
           colnames(dt) <-
             c('variable', 'break_point', 'coef', 'condition')
           data.table::setorderv(dt, c('variable', 'break_point'))
           dt
        }
      })
      names(categorical_bins_) <- names(lencs)
      categorical_bins <- categorical_bins_
    } else {
      categorical_bins = NULL
    }
    if (length(setdiff(colnames(train_x), category_cols)) > 0 &&
        nrow(trees_nodes_0[!(Feature %in% category_cols)]) > 0) {
      continuous_bins <-
        trees_nodes_0[Feature %in% selected_con_variables][, condition := '<']
      missing_rows <- continuous_bins[, .SD[1], by = 'Feature']
      missing_rows[, `:=`(Split = NA, condition = '=')]
      missing_rows <-
        merge(
          missing_rows,
          missing_values_num,
          by.x = 'Feature',
          by.y = 'Feature',
          suffixes = c('', '.y'),
          all.x = TRUE
        )
      missing_rows[, `:=`(value = missing, missing = NULL)]

      continuous_bins <- rbind(continuous_bins, missing_rows)
      colnames(continuous_bins) <-
        c('variable', 'break_point', 'coef', 'condition')

      data.table::setorderv(continuous_bins, c('variable', 'break_point'))
    } else{
      continuous_bins = NULL
    }
    model <-
      list(
        continuous_bins = continuous_bins,
        categorical_bins = categorical_bins,
        intercept = intercept
      )
    class(model) <- 'dblr'
    model
  }

#' @title Discrete Boosting Logistic Regression Prediction
#' @description predict.dblr makes predictions on new data set given the fitted dblr model object.
#' @param object A fitted dblr model object, which should be returned by calling dblr_train function
#' @param newdata A data.frame contains the samples to predict
#' @param type Control the output of prediction. Default: 'response' means probability; 'Link' would produce the linear part; 'mapped' would produce a data.frame filling with the coefficients of the model
#' @param ... further arguments passed to or from other methods
#' @return Returns a vector of prediction or a data.frame
#' @export
#' @docType methods
#' @rdname predict.dblr
predict.dblr <- function(object, newdata, type = 'response',...) {
  if (!is.data.frame(newdata))
    stop("newdata must be a data.frame")
  if (length(unique(colnames(newdata))) < ncol(newdata))
    stop("newdata has duplicate column names")

  if (!(type %in% c('response', 'link', 'mapped')))
    stop('type should be either link or response')

  variable <- NULL
  break_point<- NULL
  coef <- NULL

  if (!is.null(object$continuous_bins)) {
    variables <- object$continuous_bins[, unique(variable)]
    continuous_values <- sapply(variables, function(e) {
      to_value <-
        object$continuous_bins[variable == e &
                                 !is.na(break_point), coef][findInterval(newdata[[e]], c(-Inf, object$continuous_bins[variable ==
                                                                                                                        e & !is.na(break_point), break_point]), left.open = FALSE)]
      to_value[is.na(to_value)] <-
        object$continuous_bins[variable == e & is.na(break_point), coef]
      to_value
    })
    con_sum <- rowSums(continuous_values)
  } else {
    continuous_values <- NULL
    con_sum <- 0.0
  }
  if (!is.null(object$categorical_bins)) {
    variables <- names(object$categorical_bins)
    categorical_values <- sapply(variables, function(e) {
      mapping <-
        object$categorical_bins[[e]][, c('break_point', 'coef')]
      data.table::setkey(mapping, "break_point")
      J <- 1
      mapping[J(newdata[[e]]), coef]
    })
    cat_sum <- rowSums(categorical_values)
  } else {
    categorical_values <- NULL
    cat_sum <- 0.0
  }
  if (type == 'link')
    return(con_sum + cat_sum + object$intercept)
  else if (type == 'response') {
    return (1.0 / (1 + exp(
      -(con_sum + cat_sum + object$intercept)
    )))
  } else {
    return(cbind(continuous_values, categorical_values))
  }
}
