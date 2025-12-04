#' Train XGBoost Model
#'
#' @param dataset your data frame
#' @param nround number of iterations
#' @param verbose display errors ?
#'
#' @examples
#' \dontrun{
#' list <- oncrawlTrainModel(dataset)
#' plot(list$roc)
#' print(list$matrix)
#' }
#'
#' @return a list with your ML model, your training data
#' @author Vincent Terrasi
#' @export
#' @importFrom stats predict
#' @importFrom rlang .data
#' @importFrom stats median
oncrawlTrainModel <- function(dataset, nround=300, verbose=1) {

  if ( which("analytics_entrances_seo_google"==names(dataset))==0 ) {
    warning("You need analytics data, please connect analytics datasource")
    return()
  }

  #create training dataset : predit hit_crawls
  dataset <- dplyr::select(dataset,
                             -.data$url
                             ,-.data$title
                             ,-.data$h1
                             ,-.data$fetch_date
                             ,-dplyr::contains("gsc_")
                             ,-dplyr::contains("_hash")
                             ,-dplyr::contains("urlpath")
                             ,-dplyr::contains("hreflang_")
                             ,-dplyr::contains("meta_")
                             ,-dplyr::contains("is_")
                             ,-dplyr::contains("redirect_")
                             ,-dplyr::contains("twc_")
  )


  # logistic regression 0 or 1 : choose a thresold
  thresold <- median(dataset$analytics_entrances_seo_google)
  #thresold <- mean(dataset$analytics_entrances_seo_google)
  dataset$analytics_entrances_seo_google[which(is.na(dataset$analytics_entrances_seo_google))] <- 0
  dataset$analytics_entrances_seo_google[which(dataset$analytics_entrances_seo_google <= thresold )] <- 0
  dataset$analytics_entrances_seo_google[which(dataset$analytics_entrances_seo_google > thresold)] <- 1

  # remove all NA
  datasetMat <- dataset[,colSums(is.na(dataset))<nrow(dataset)]

  ## 75% of the sample size
  smp_size <- floor(0.75 * nrow(datasetMat))
  train_ind <- sample(seq_len(nrow(datasetMat)), size = smp_size)

  X <- datasetMat[train_ind, ]
  X_test <- datasetMat[-train_ind, ]
  y<- datasetMat[train_ind, "analytics_entrances_seo_google"]
  y_test<-datasetMat[-train_ind, "analytics_entrances_seo_google"]

  # wt = without target
  X_wt <- dplyr::select(X,
                        -.data$analytics_entrances_seo_google
                        ,-dplyr::contains("ati_")
                        ,-dplyr::contains("google_analytics_")
                        ,-dplyr::contains("adobe_analytics_")
                        ,-dplyr::contains("googlebot_")
                        ,-dplyr::contains("crawled_by_googlebot")
                        #-.data$crawl_hits_google,
                        #-.data$crawl_hits_google_smartphone,
                        #-.data$crawl_hits_google_web_search
  )

  # wt = without target
  X_test_wt <- dplyr::select(X_test,
                             -.data$analytics_entrances_seo_google
                             ,-dplyr::contains("ati_")
                             ,-dplyr::contains("google_analytics_")
                             ,-dplyr::contains("adobe_analytics_")
                             ,-dplyr::contains("googlebot_")
                             ,-dplyr::contains("crawled_by_googlebot")
                             #-.data$crawl_hits_google,
                             #-.data$crawl_hits_google_smartphone,
                             #-.data$crawl_hits_google_web_search
  )

  # create the model
  model <- xgboost::xgboost(data = data.matrix(X_wt),
                   label = data.matrix(y),
                   eta = 0.1,
                   max_depth = 10,
                   verbose= verbose,
                   nround = nround,
                   objective = "binary:logistic",
                   nthread = 8
  )

  y_pred <- predict(model, data.matrix(X_test_wt))

  # display confusion matrix
  matrix <- caret::confusionMatrix(as.factor(round(y_pred)), as.factor(y_test))

  # display roc curb
  roc <- pROC::roc(y_test, y_pred)

  return(list(model=model,
              x=data.matrix(X_wt),
              y=data.matrix(y),
              matrix=matrix,
              roc=roc))

}

#' Explain XGBoost Model by displaying each importance variables
#'
#' @param model your XgBoost model
#' @param x your training data
#' @param y your predicted data
#' @param max the number of importance variable you want to explain
#' @param path path of your conf file
#'
#' @examples
#' \dontrun{
#' list <- oncrawlTrainModel(dataset,200)
#' oncrawlExplainModel(list$model, list$x, list$y, 3)
#' }
#' @return graphs
#' @author Vincent Terrasi
#' @export
#' @importFrom graphics plot title
#' @importFrom rlang .data
#'
oncrawlExplainModel <- function(model, x, y, max=10, path=tempdir()) {

  if(!file.exists(path)) stop("Please, set a valid path")

  explainer_xgb <- DALEX::explain(model,
                           data = data.matrix(x),
                           y = data.matrix(y),
                           label = "xgboost")

  #print importance variables
  vd_xgb <- DALEX::variable_importance(explainer_xgb, type = "raw")
  vd_plot <- plot(vd_xgb)
  #ggplot2::ggsave(file.path(path,"variable_importance.jpg"),vd_plot)

  variables <- dplyr::arrange(vd_xgb, -vd_xgb$dropout_loss)
  variables <- as.character(variables$variable)

  list_factors <- list()
  # plot each importance variables
  # avoid the first row
  for(i in 2:(max+1)) {

    sv_xgb_satisfaction  <- DALEX::single_variable(explainer_xgb,
                                            variable = variables[i],
                                            type = "pdp")

    sv_xgb_satisfaction <- c(sv_xgb_satisfaction, name=variables[i])

    #p <- plot(sv_xgb_satisfaction)
    #ggplot2::ggsave(file.path(path,paste0("explain_top",(i-1),".jpg")),p)

    list_factors <- rlist::list.append(list_factors,sv_xgb_satisfaction)

  }

  return(list(plot=vd_plot,factors=list_factors))

}
