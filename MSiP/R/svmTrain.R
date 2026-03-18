#' svmTrain
#' @param dtInput Data frame containing instances with class labels
#' @param impute Logical value, indicating whether to impute missing values
#' @param p The percentage of data that goes to training; defaults to 0.3
#' @param parameterTuning Logical value; indicating whether to
#' tune SVM hyper parameters
#' @param cost Cost of constraints violation
#' @param gamma Parameter needed for all kernels except linear
#' @param kernel Kernel type: 'linear', 'polynomial', 'sigmoid', or 'radial';
#' defaults to 'radial'
#' @param ncross  K-fold cross validation on the training data is performed to assess the quality of the model;
#' defaults to 10
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
#' @importFrom dplyr mutate_if
#' @importFrom caret createDataPartition
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve
#' @importFrom stats predict
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom pROC plot.roc
#' @importFrom mice mice
#' @importFrom mice complete
#' @importFrom e1071 tune
#' @importFrom e1071 svm
#' @importFrom e1071 tune.control
#' @description The labeled feature matrix can be used as input for
#' Support Vector Machines (SVM) classifier. The classifier then assigns each
#' bait-prey pair a confidence score, indicating the level of support for
#' that pair of proteins to interact. Hyperparameter optimization can also be
#' performed to select a set of parameters that maximizes the model's performance.
#' This function also computes the areas under the precision-recall (PR) and
#' ROC curve to evaluate the performance of the classifier.
#' @export
#' @examples
#' data(testdfClassifier)
#' predidcted_SVM <-
#' svmTrain(testdfClassifier,impute = FALSE,p = 0.3,parameterTuning = FALSE,
#' cost = seq(from = 2, to = 10, by = 2),
#' gamma = seq(from = 0.01, to = 0.10, by = 0.02),
#' kernel = "radial",ncross = 10,
#' pr.plot = FALSE, roc.plot = TRUE)
#' head(predidcted_SVM)

svmTrain  <-
    function(dtInput,impute = TRUE,p = 0.3,parameterTuning = TRUE,
        cost = seq(from = 2, to = 10, by = 2),
        gamma = seq(from = 0.01, to = 0.10, by = 0.02),
        kernel = "radial",ncross = 10,
        pr.plot = TRUE, roc.plot = TRUE
    ) {

        PPI <- NULL
        target <- NULL
        `x_test$target` <- NULL
        rocP <- NULL
        prP <- NULL


        fulldat <-
            dtInput
        data <-
            dtInput


        if(!is.character(data[,1])){
            stop("The values for the first column should be a character vector")
        }

        if(!is.data.frame(data)){
            stop("Input data should be data.frame")
        }

        if(all(colnames(data) != "target") == TRUE){
            stop("Target is absent from the data.frame")
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

        #remove column with more than 20% missing values
        miss_prec <-
            colSums(is.na(m.df))/nrow(m.df) * 100
        print(miss_prec[miss_prec > 20])
        col_miss <-
            names(miss_prec[miss_prec>20])
        # Removing the columns with more than 20% missing value
        m.df[,c(col_miss)] <- NULL

        #impute missing valyse using mice package
        if(impute == TRUE){
            m.df <-
                mice(m.df, m = 5, method = "rf", maxit = 10)
            m.df <- complete(m.df, 1)

            #split the data into training and test set
            data_index <-
                createDataPartition(m.df$target, p= p, list = F)
            x_train <-
                m.df[-data_index,]
            x_test <-
                m.df[data_index,]
        } else {

            #split the data into training and test set
            data_index <-
                createDataPartition(m.df$target, p= p, list = F)
            x_train <-
                m.df[-data_index,]
            x_test <-
                m.df[data_index,]
        }


        #Feature Scaling
        x_train_scaled <-
            x_train %>%
            mutate_if(is.numeric, scale)

        #scale the test data using training data
        trainMean <- apply(x_train[,-which(names(x_train) %in% "target")], 2, mean)
        trainSd <- apply(x_train[,-which(names(x_train) %in% "target")], 2, sd)
        # centred and scaled
        x_test_scaled <-
            sweep(sweep(x_test[,-which(names(x_test) %in% "target")], 2L, trainMean), 2, trainSd, "/")
        x_test_scaled <-
            cbind(x_test_scaled,x_test$target)
        x_test_scaled <-
            x_test_scaled %>%
            dplyr::rename(target=`x_test$target`)

        # Fitting SVM to the training set

        if(parameterTuning == TRUE){
            message(paste("cost range:"))
            print(cost)
            message(paste("gamma range:"))
            print(gamma)

            param.tune <-
                tune(svm,target~., data=x_train_scaled,
                    ranges = list(gamma = gamma, cost = cost),
                    probability=TRUE, kernel = kernel,
                    tunecontrol = tune.control(cross = ncross))

            fit.svm <- svm(target~., data=x_train_scaled,
                kernel = kernel,
                probability=TRUE,
                type = "C-classification",
                gamma = param.tune$best.parameters$gamma,
                cost = param.tune$best.parameters$cost,
                cross = ncross)

        } else {
            fit.svm <- svm(target~., data=x_train_scaled,
                kernel = kernel,
                type = "C-classification",
                probability=TRUE,
                cross = ncross)
        }

        # Predicting the Test set results
        pred.mod <-
            predict(fit.svm, x_test_scaled, probability=TRUE) # predict on test set


        #PR curve
        if(pr.plot){
            prP <- plot(pr.curve(
                scores.class0 =
                    attr(pred.mod, "probabilities")[,2][x_test$target == "Positive"],
                scores.class1 =
                    attr(pred.mod, "probabilities")[,2][x_test$target == "Negative"],
                curve = T))
        }#ROC curve
        if(roc.plot) {
            rocP <- invisible(plot.roc(x_test$target,attr(pred.mod, "probabilities")[,2],
                percent = TRUE, main = "ROC curves",
                legacy.axes = TRUE,
                add =  FALSE, asp = NA, print.auc = TRUE))
        }


        #predict on whole data (i.e., data with unknown class label)
        fdf <- # predict on the whole data
            fulldat %>%
            dplyr::select(-target,-1)

        # centred and scaled
        fdf <-
            sweep(sweep(fdf, 2L, trainMean), 2, trainSd, "/")


        predfdf  <-
            predict(fit.svm,fdf,probability=TRUE)


        output <-
            cbind(fulldat[,1],attr(predfdf, "probabilities")[,1:2])

        return(output)
    }







