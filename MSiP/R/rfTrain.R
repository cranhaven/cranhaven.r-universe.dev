#' rfTrain
#' @param dtInput Data frame containing instances with class labels
#' @param impute Logical value, indicating whether to impute missing values
#' @param p The percentage of data that goes to training; defaults to 0.3
#' @param parameterTuning Logical value; indicating whether to
#' tune rf hyper parameters
#' @param mtry Number of variables to possibly split at in each node and it
#' is bound by the number of variables in your model
#' @param min_node_size Minimal node size
#' @param splitrule Splitrule rule for classification: 'gini', 'extratrees' or 'hellinger' with default 'gini'
#' @param metric A string that specifies what summary metric will be used to select the optimal model; default to
#' Accuracy
#' @param resampling.method The resampling method:'boot', 'boot632', 'optimism_boot',
#' 'boot_all', 'cv', 'repeatedcv', 'LOOCV', 'LGOCV'; defaults to repeatedcv
#' @param iter Number of resampling iterations; defaults to 5
#' @param repeats for repeated k-fold cross validation only; defaults to 5
#' @param pr.plot Logical value, indicating whether to plot precision-recall (PR) curve
#' @param roc.plot Logical value, indicating whether to plot ROC curve
#' @return Data frame containing a classification results for all instances in the data set,
#' where positive confidence score corresponds to  the level of support for the pair of proteins to be true positive,
#' whereas negative score corresponds to the level of support for the pair of proteins to be true negative.
#' @author Matineh Rahmatbakhsh, \email{matinerb.94@gmail.com}
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom caret createDataPartition
#' @importFrom caret trainControl
#' @importFrom caret train
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve
#' @importFrom stats predict
#' @importFrom stats na.omit
#' @importFrom pROC plot.roc
#' @importFrom mice mice
#' @description The labeled feature matrix can be used as input for
#' Random Forest (RF) classifier. The classifier then assigns each
#' bait-prey pair a confidence score, indicating the level of support for
#' that pair of proteins to interact. Hyperparameter optimization can also be
#' performed to select a set of parameters that maximizes the model's performance.
#' This function also computes the areas under the precision-recall (PR) and
#'  ROC curve to evaluate the performance of the classifier.
#' @exportPattern '^[[:alpha:]]+'
#' @export
#' @examples
#' data(testdfClassifier)
#' \donttest{
#' predidcted_RF <-
#' rfTrain(testdfClassifier,impute = FALSE, p = 0.3, parameterTuning = FALSE,
#' mtry  = seq(from = 1, to = 5, by = 1),
#' min_node_size = seq(from = 1, to = 5, by = 1),
#' splitrule =c("gini"),metric = "Accuracy",
#' resampling.method = "cv",iter = 2,repeats = 2,
#' pr.plot = TRUE, roc.plot = FALSE)
#' head(predidcted_RF)
#'}


rfTrain  <- function(dtInput,impute = TRUE,p =0.3,parameterTuning = TRUE,
    mtry  = seq(from = 1, to = 10, by = 2),
    min_node_size = seq(from = 1, to = 9, by = 2),
    splitrule =c("gini"),metric = "Accuracy",
    resampling.method = "repeatedcv",iter = 5,repeats = 5,
    pr.plot = TRUE, roc.plot = TRUE) {

    PPI <- NULL
    target <- NULL

    fulldat <-
        dtInput
    data <-
        dtInput
    rocP <- NULL
    prP <- NULL


    if(!is.character(data[,1])){
        stop("The values for the first column should be a character vector")
    }

    if(!is.data.frame(data)){
        stop("Input data should be data.frame")
    }

    if(all(colnames(data) != "target") == TRUE){
        stop("target is absent from the data.frame")
    }

    if(!is.character(data$target)){
        stop("The values for the target column should be a character vector")
    }
    #Data preparation
    m.df <-
        data %>%
        dplyr::select(-1)%>%
        filter(!is.na(target))
    m.df$target <-#chanage charactor to factor
        as.factor(as.character(m.df$target))
    levels(m.df$target) <-
        c("Negative", "Positive")
    m.df$target <-
        factor(m.df$target, levels=rev(levels(m.df$target)))

    #Removing columns with more than 20% missing values
    miss_prec <-
        colSums(is.na(m.df))/nrow(m.df) * 100
    print(miss_prec[miss_prec > 20])
    col_miss <-
        names(miss_prec[miss_prec>20])
    #Removing columns with more than 20% missing value
    m.df[,c(col_miss)] <- NULL


    #Impute missing valyse using mice package
    if(impute == TRUE){
        m.df <-
            mice(m.df, m = 5, method = "rf", maxit = 10)
        m.df <- complete(m.df, 1)

        #Split the data into training and test set
        data_index <-
            createDataPartition(m.df$target, p= p, list = F)
        x_train <-
            m.df[-data_index,]
        x_test <-
            m.df[data_index,]
    } else {

        #Split the data into training and test set
        data_index <-
            createDataPartition(m.df$target, p= 0.3, list = F)
        x_train <-
            m.df[-data_index,]
        x_test <-
            m.df[data_index,]
    }

    #create tunegrid for mtry for model tunning

    #Hyperparameter tunning & fitting RF to the training set

    if(parameterTuning == TRUE){
        message(paste("mtry range:"))
        print(mtry)
        message(paste("node_size range:"))
        print(min_node_size)
        message(paste("splitrule:"))
        print(splitrule)

        hyper_grid <- expand.grid(
            mtry = mtry,
            min.node.size =min_node_size,
            splitrule = splitrule
        )

        #fit the model
        fit.rf <-
            train(target~., data=x_train, #train on train set
                method = "ranger",
                tuneGrid = hyper_grid,
                metric = metric,
                trControl = trainControl(# train control
                    method = resampling.method,
                    number =iter,# number of resampling iterations
                    repeats = repeats, # sets of folds to for repeated cross-validation
                    verboseIter = F,
                    classProbs = T,
                    savePredictions = T))
        message("Best Hyperparameters are:")
        print(fit.rf[["bestTune"]])

    } else {
        fit.rf <-
            train(target~., data=x_train, #train on train set
                method = "ranger",
                metric = metric,
                trControl = trainControl(# train control
                    method = resampling.method,
                    number =iter,# number of resampling iterations
                    repeats = repeats, # sets of folds to for repeated cross-validation
                    verboseIter = F,
                    classProbs = T,
                    savePredictions = T))

    }

    #Predicting the Test set results
    pred.mod <-
        predict(fit.rf, x_test,type = "prob")


    #PR curve
    if(pr.plot){
        #PR curve
        prP <- plot(pr.curve(
            scores.class0 = pred.mod$Positive[x_test$target == "Positive"],
            scores.class1 = pred.mod$Positive[x_test$target == "Negative"],
            curve = T,
            max.compute = TRUE, min.compute = TRUE, rand.compute = TRUE))
    }#ROC curve
    if(roc.plot) {
        rocP <- invisible(plot.roc(x_test$target,pred.mod$Positive,
            percent = TRUE, main = "ROC curves",
            legacy.axes = TRUE,
            add =  FALSE, asp = NA, print.auc = TRUE))

    }


    #Predict on whole data (i.e., data with unknown class label)
    fdf <- # predict on the whole data
        fulldat %>%
        dplyr::select(-target,-1)


    predfdf  <-
        predict(fit.rf,fdf,type = "prob")


    output <-
        cbind(fulldat[,1],predfdf)

    return(output)
}


