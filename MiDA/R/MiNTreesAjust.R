#' Ajust number of trees parameter for fitting  generalized boosted regression models
#'
#' Test total number of trees parameter for microarray data binary classification using gradient boosting
#' over desicion trees.
#'
#'@param Matrix numeric matrix of expression data where each row corresponds to a probe (gene, transcript),
#'  and each column correspondes to a specimen (patient).
#'@param specimens factor vector with two levels specifying specimens in the columns of the \code{Matrix}
#'@param test.frac  integer specifying fraction of data to use for model testing
#'@param times integer specifying number of trials
#'@param ntrees vector of integer specifying the total number of decision trees (boosting iterations).The tested parameter.
#'@param shrinkage numeric specifying the learning rate. Scales the step size in the gradient descent procedure.
#'@param intdepth integer specifying the maximum depth of each tree.
#'@param n.terminal integer specifying the actual minimum number of observations in the terminal nodes of the trees.
#'@param bag.frac the fraction of the training set observations randomly selected to propose the next tree in the expansion.
#'
#'@details
#'\code{test.frac} defines fraction of specimens that will be used for model testing. For example, if
#'\code{test.frac}=5 then 4/5th of specimens will be used for model fitting (train data) and 1/5th of specimens
#' will be used for model testing (test data). Specimens for test and train data will be selected by random.
#'So with \code{times}>1, train and test data will differ each time.
#'\cr
#'While boosting basis functions are iteratively adding in a greedy fashion
#'so that each additional basis function further reduces the selected loss function.
#'Gaussian distribution (squared error) is used.
#'\code{ntrees}, \code{shrinkage}, \code{intdeep} are parameters for model tuning.
#'\code{bag.frac} introduces randomnesses into the model fit.
#'If \code{bag.frac} < 1 then running the same model twice will result in similar but different fits.
#'Number of specimens in train sample must be enough to provide the minimum number of observations in terminal nodes.I.e.
#'\cr(1-1/\code{test.frac})*\code{bag.frac} > \code{n.terminal}.
#'\cr
#'See \code{\link{gbm}} for details.
#'\cr
#'Use \code{\link{MiIntDepthAjust}} and \code{\link{MiShrinkAjust}} for ajusting other parameters.
#'\cr
#'Function is rather time-costing. If specimens are not equally distributed between two classified groups,
#' NA may be produced.
#'
#'@return list of 2
#'\cr
#'\code{train.accuracy} - a data frame of train data classification accuracy
#'for each \code{ntrees} value in each trial and their median.
#'\cr
#'\code{test.accuracy} - a data frame of test data classification accuracy
#'for each \code{ntrees} value in each trial and their median.
#'\cr
#'Also a plot for \code{ntrees} versus Accuracy is produced.
#'
#'@examples
#' #get gene expression and specimen data
#' data("IMexpression");data("IMspecimen")
#' #sample expression matrix and specimen data for binary classification,
#' #only "NORM" and "EBV" specimens are left
#' SampleMatrix<-MiDataSample(IMexpression, IMspecimen$diagnosis,"norm", "ebv")
#' SampleSpecimen<-MiSpecimenSample(IMspecimen$diagnosis, "norm", "ebv")
#' #Fitting, low tuning for faster running. Test ntrees
#' set.seed(1)
#' ClassRes<-MiNTreesAjust(SampleMatrix, SampleSpecimen, test.frac = 5, times = 3,
#'                        ntrees = c(10, 20), shrinkage = 1, intdepth = 2)
#' ClassRes[[1]] # train accuracy
#' ClassRes[[2]] # test accuracy
#'
#'
#'@author Elena N. Filatova
#'
#'@seealso \code{\link{gbm}}, \code{\link{MiIntDepthAjust}}, \code{\link{MiShrinkAjust}}
#'
#'@export


MiNTreesAjust <- function(Matrix, specimens, test.frac = 5, times = 5,
                        ntrees = c(100, 500, 1000, 5000, 10000), shrinkage = 0.1, intdepth=2,
                        n.terminal=10, bag.frac=0.5){
  Data <- as.data.frame(t(Matrix))
  accur.train.data <- data.frame(); accur.test.data <- data.frame()
  for(j in 1:times) {
    idx <- sample(1:length(specimens), size = length(specimens)/test.frac) # make random train and test specimens
    idx<-round(idx)
    train.data <- Data[-idx, ]; test.data <- Data[idx, ]
    train.group <- specimens[-idx]; test.group <- specimens[idx]
    accur.train <- c(); accur.test <- c();
    for (i in 1:length(ntrees)){
      Model <- gbm::gbm(train.group~., train.data, distribution = "gaussian",
                 n.trees = ntrees [i], shrinkage = shrinkage, interaction.depth = intdepth,
                 n.minobsinnode = n.terminal, bag.fraction = bag.frac)
      Pred <- stats::predict(Model, test.data, n.trees = ntrees [i])
      Pred <- round(Pred); Pred <- factor(Pred, levels = c(1,2), labels = levels(train.group))
      accur.test[i] <- 1-mean(test.group != Pred) # Accuracy
      Model.fit <- round(Model$fit)
      Model.fit <- factor(Model.fit, levels = c(1,2), labels = levels(train.group))
      accur.train[i] <- 1-mean(train.group != Model.fit)
    }
    accur.train.data <- rbind(accur.train.data, accur.train);
    accur.test.data <- rbind(accur.test.data, accur.test)
  }
  colnames(accur.train.data) <- ntrees; colnames(accur.test.data) <- ntrees
  accur.train.median <- apply(accur.train.data, 2, stats::median, na.rm = T) # count median for all trials
  accur.test.median <- apply(accur.test.data, 2, stats::median, na.rm = T)
  accur.train.data <- rbind(accur.train.data, accur.train.median);
  rownames(accur.train.data)<-c(paste("Trial", 1:times, sep=""), "Median")
  accur.test.data <- rbind(accur.test.data, accur.test.median)
  rownames(accur.test.data)<-c(paste("Trial", 1:times, sep=""), "Median")
  n.trees.res <- list("train.accuracy" = accur.train.data, "test.accuracy" = accur.test.data) # result data
  graphics::plot(x=ntrees, y=seq(from=0.2, to=1.2, length=length(ntrees)), type = "n", xlab = "N trees", ylab = "Accuracy") #plot
  graphics::lines(ntrees, accur.train.median, type = "o", col = 4, lwd = 1.8)
  graphics::lines(ntrees, accur.test.median, type = "o", col = 2, lwd = 1.8)
  graphics::legend(x = "topright", legend = c("Train", "Test"), col = c(4,2), pch=1)
  return(n.trees.res)
}
