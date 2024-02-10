#' Evaluate species distribution models
#'
#'@description Performs model evaluation. Measure of AUC (Area Under the Curve) value, confusion matrix, maxSSS threshold (Maximum Sensitivity plus Specificity), percentage of predicted preferential area based on the MaxSSS value and model stability (standard deviation of pixel values)
#'
#'@usage
#'SDMeval(model)
#'
#'@param model Model produced with \link{compute.maxent} or \link{compute.brt} functions
#'
#'@return
#'Dataframe with the following information
#'\itemize{
#'\item \emph{AUC.value}  Returns the AUC (Area Under the Curve) value of the model
#'\item \emph{maxSSS}     Maximum Sensitivity plus Sensibility threshold of the model
#'\item \emph{preferential.area} Pixel proportion for which the predicted value is superior to the MaxSSS threshold
#'\item \emph{omission.rate}  Proportion of data that fall out of the area predicted as preferential
#'\item \emph{nb.omission} Corresponding number of data that fall out of the predicted preferential area
#'\item \emph{SD.value} Mean standard deviation of the predicted grid }
#'
#'@details
#'Area Under the Curve is a parameter largely refered in the literature and used to test species distribution models performance (Fielding & Bell, 1997). It evaluates the area under the Receiver Operating Curve (ROC), which draws the relationship between 1-specificity (False Positive Rate) and specificity (True Positive Rate). AUC values bordering 1 present models with high True Positive Rate, 0.5 model with random prediction and 0 to models presenting a strong False Positive Rate.
#'
#'MaxSSS threshold value maximizes the sum of True Positive Rate and True Negative Rate. See Liu et al. (2013) for more information.
#'
#'Modelling performance can be evaluated with the measure of omission rate, the proportion of occurrences that falls out the area predicted as preferential by the MaxSSS threshold (False Positive Rate).
#'Models stability is evaluated with the mean standard deviation value of the pixel values of the grid predicted by the model.
#'
#'
#'@references
#'Fielding A, & J Bell (1997) A review of methods for the assessment of prediction errors in conservation presence absence models. \emph{Environmental Conservation}, 24(1): 38-49.
#'
#'Liu C, M White & G Newell (2013) Selecting thresholds for the prediction of species occurrence with presence only data. \emph{Journal of Biogeography}, 40(4): 778-789.
#'
#'@examples
#'#Generate a SDMtab and launch a model
#'data('ctenocidaris.nutrix')
#'occ <- ctenocidaris.nutrix
#'occ <- ctenocidaris.nutrix[,c('decimal.Longitude','decimal.Latitude')]
#'
#'data(predictors2005_2012)
#'envi <- predictors2005_2012
#'envi
#'
#'SDMtable_ctenocidaris <- SDMPlay:::SDMtab(xydata=occ,
#'                                          predictors=predictors2005_2012,
#'                                          unique.data=FALSE,
#'                                          same=TRUE)
#'model <- SDMPlay:::compute.brt(x=SDMtable_ctenocidaris, proj.predictors=envi,lr=0.005)
#'
#'# Evaluate modelling performance
#'SDMPlay:::SDMeval(model)



SDMeval <- function(model) {

    ########## AUC value ########## extracting presence values from SDMtab object
    presence.vals <- base::subset(model$data, model$data$id == 1)
    presence.vals <- presence.vals[, -c(1:3)]

    # extracting background values from SDMtab
    background.vals <- base::subset(model$data, model$data$id == 0)
    background.vals <- background.vals[, -c(1:3)]

if (model$algorithm=="brt"){
    testp <- raster::predict(model$response, data.frame(presence.vals), n.trees = model$response$gbm.call$best.trees, type = "response")
    testa <- raster::predict(model$response, data.frame(background.vals), n.trees = model$response$gbm.call$best.trees, type = "response")}

    if (model$algorithm=="maxent"){
      testp <- dismo::predict(model$response,data.frame(presence.vals))
      testa <- dismo::predict(model$response,data.frame(background.vals))}

    eval.data <- dismo::evaluate(p = testp, a = testa)
    AUCvalue <- eval.data@auc

    ######################## MaxSSS ############################
    tab <- base::cbind(eval.data@t, eval.data@TPR + eval.data@TNR)
    maxSSS <- (base::subset(tab, tab[, 2] == max(tab[, 2])))[1, 1]

    ######### Preferential area ################### Transform the raster in vector to count the number of
    ######### pixels that suits the condition
    prediction <- model$raster.prediction
    p.vect <- raster::reclassify(prediction, cbind(NA, 9999))
    p.vect <- raster::rasterToPoints(p.vect)

    # number of pixels for which the value is > to MaxSSS value
    nb.pix <- nrow(base::subset(p.vect, p.vect[, 3] > maxSSS & p.vect[, 3] <= 1))
    # Corresponding percentage of the total area
    preferential.area <- nb.pix * 100/nrow(subset(p.vect, p.vect[, 3] <= 1))

    ######################## Confusion matrix ########################
    confusion.matrix <- function(obs,pred,threshold=0.5){
      #input checks
      if (length(obs)!=length(pred)) stop(' Confusion matrix function : this requires the same number of observed & predicted values')
      if (!(length(threshold)==1 & threshold[1]<=1 & threshold[1]>=0)) stop(' Confusion matrix function :inappropriate threshold value... must be a single value between 0 & 1.')
      n = length(obs); if (length(which(obs %in% c(0,1,NA)))!=n) stop('Confusion matrix function :observed values must be 0 or 1') #ensure observed are values 0 or 1

      #deal with NAs
      if (length(which(is.na(c(obs,pred))))>0) {
        na = union(which(is.na(obs)),which(is.na(pred)))
        warning(length(na),' Confusion matrix function : data points removed due to missing data')
        obs = obs[-na]; pred = pred[-na]
      }
      #apply the threshold to the prediction
      if (threshold==0) {
        pred[which(pred>threshold)] = 1; pred[which(pred<=threshold)] = 0
      } else {
        pred[which(pred>=threshold)] = 1; pred[which(pred<threshold)] = 0
      }
      #return the confusion matrix
      mat = table(pred=factor(pred,levels=c(0,1)),obs=factor(obs,levels=c(0,1)))
      attr(mat,'class') = 'confusion.matrix'
      return(mat)
    }

    obs <- c(rep(1, length(testp)), rep(0, length(testa)))
    pred <- c(testp, testa)
    confusion.mat <- confusion.matrix(obs, pred, threshold = maxSSS)

    ######################### Omission rate #######################
    omission <- function(mat){
      #input checks
      if (attr(mat,'class')!='confusion.matrix') stop('mat must be of class confusion.matrix')
      #return the value
      return(mat[1,2]/sum(mat[,2]))
    }

    omission.rate <- omission(confusion.mat) * 100
    # corresponding number of data that fall out of the predicted area
    number.omission <- base::round(nrow(presence.vals) * omission.rate/100)

    ####################### Model stability #######################
    sd.map <- raster::cellStats(x = prediction, stat = stats::sd, na.rm = T)

    ######## Make a list of all of the results obtained #############
    result <- data.frame(cbind(AUCvalue, maxSSS, preferential.area, omission.rate, number.omission,
        sd.map))
    colnames(result) <- c("AUC.value", "MaxSSS", "preferential.area", "omission.rate", "nb.omission",
        "SD.value")
    return(result)

}
