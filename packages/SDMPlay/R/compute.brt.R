#' Compute BRT (Boosted Regression Trees) model
#'
#'@description Compute species distribution models with Boosted Regression Trees
#'
#'@usage
#'compute.brt(x, proj.predictors, tc = 2, lr = 0.001, bf = 0.75,
#'            n.trees = 50, step.size = n.trees, n.folds= 10, fold.vector = NULL)
#'
#'@param x \link{SDMtab} object or dataframe that contains id, longitude, latitude and values of environmental descriptors at corresponding locations
#'
#'@param proj.predictors RasterStack of environmental descriptors on which the model will be projected
#'
#'@param tc Integer. Tree complexity. Sets the complexity of individual trees
#'
#'@param lr Learning rate. Sets the weight applied to individual trees
#'
#'@param bf Bag fraction. Sets the proportion of observations used in selecting variables
#'
#'@param n.trees Number of initial trees to fit. Set at 50 by default
#'
#'@param step.size Number of trees to add at each cycle, set equal to n.trees by default
#'
#'@param n.folds Number of subsets into which the initial dataset (x) is divided for model evaluation procedures (cross-validation). Set to 10 by default.
#'
#'@param fold.vector Vector indicating the fold group to which each data belongs to.
#'
#'@return
#'\itemize{
#'A list of 5
#'\item \emph{model$algorithm} "brt" string character
#'\item \emph{model$data} x dataframe that was used to implement the model
#'\item \emph{model$response} Parameters returned by the model object: list of 41, see \link[dismo]{gbm.step} for more info
#'\item \emph{model$raster.prediction} Raster layer that predicts the potential species distribution
#'\item \emph{model$eval.stats} List of elements to evaluate the model: AUC, maxSSS, COR, pCOR, TSS, ntrees, residuals
#'}
#'
#'
#'@details
#'The function realises a BRT model according to the \link[dismo]{gbm.step} function provided by Elith et al.(2008). See the publication for further information about setting decisions. The map produced provides species presence probability on the projected area.
#'
#'@note
#'See Barbet Massin et al. (2012) for information about background selection to implement BRT models
#'
#'@seealso
#'\link[dismo]{gbm.step}

#'@references
#'Elith J, J Leathwick & T Hastie (2008) A working guide to boosted regression trees. \emph{Journal of Animal Ecology}, 77(4): 802-813.
#'
#'Barbet Massin M, F Jiguet, C Albert & W Thuiller (2012) Selecting pseudo absences for species distribution models: how, where and how many? \emph{Methods in Ecology and Evolution}, 3(2): 327-338.
#'

#'
#'
#'@examples
#'\dontrun{
#'#Download the presence data
#'data('ctenocidaris.nutrix')
#'occ <- ctenocidaris.nutrix
#'# select longitude and latitude coordinates among all the information
#'occ <- ctenocidaris.nutrix[,c('decimal.Longitude','decimal.Latitude')]
#'
#'#Download some environmental predictors
#'data(predictors2005_2012)
#'envi <- predictors2005_2012
#'envi
#'
#'#Create a SDMtab matrix
#'SDMtable_ctenocidaris <- SDMPlay:::SDMtab(xydata=occ,
#'                                          predictors=predictors2005_2012,
#'                                          unique.data=FALSE,
#'                                          same=TRUE)
#'
#'#Run the model
#'model <- SDMPlay:::compute.brt(x=SDMtable_ctenocidaris, proj.predictors=envi,lr=0.0005)
#'
#'#Plot the partial dependance plots
#'dismo::gbm.plot(model$response)
#'
#'#Get the contribution of each variable to the model
#'model$response$contributions
#'
#'#Get the interaction between variables
#'dismo::gbm.interactions(model$response)
#'
#'#Plot some interactions
#'int <- dismo::gbm.interactions(model$response)
#'dismo::gbm.perspec(model$response,int$rank.list[1,1],int$rank.list[1,3])
#'
#'#Plot the map prediction
#'library(grDevices) # add nice colors
#'palet.col <- colorRampPalette(c('deepskyblue','green','yellow', 'red'))( 80 )
#'raster::plot(model$raster.prediction, col=palet.col,
#'             main="Prediction map of Ctenocidaris nutrix distribution")
#'data('worldmap')
#'#add data
#'points(worldmap, type="l")
#'points(occ, col='black',pch=16)
#'
#'REMARK: see more examples in the vignette tutorials
#'}

compute.brt <- function(x, proj.predictors, tc = 2, lr = 0.001, bf = 0.75, n.trees = 50, step.size = n.trees, n.folds= 10, fold.vector = NULL) {

    if (!requireNamespace("dismo")) {
        stop("you need to install the dismo package to run this function")
    }

    if (!requireNamespace("raster", quietly = TRUE)) {
        stop("The function requires 'raster' package to work, please install it.", call. = FALSE)
    }

    # LAUNCH THE MODEL
    #------------------
    model.res <- dismo::gbm.step(data = x, gbm.x = 4:ncol(x), gbm.y = 1, tree.complexity = tc, learning.rate = lr, bag.fraction = bf, n.trees = n.trees, step.size = step.size, n.folds= n.folds, fold.vector = fold.vector)

    # map prediction
    #------------------
    prediction <- raster::predict(object = proj.predictors, model = model.res, n.trees = model.res$gbm.call$best.trees, type = "response", na.rm = TRUE)

    # eval stats
    #------------------
    eval.stats<-matrix(ncol=7,nrow=1,data=NA);colnames(eval.stats)<-c("AUC","maxSSS","COR","pCOR","TSS","ntrees", "residuals")
    eval.stats <- as.data.frame(eval.stats)

    eval.stats$ntrees <- model.res$n.trees

    presvals <- subset(x, x$id==1)[, -c(1:3)]
    pseudoabs <- subset(x, x$id==0)[, -c(1:3)]

        testp<-dismo::predict(model.res,data.frame(presvals),n.trees=model.res$gbm.call$best.trees,type="response")
    testa<-dismo::predict(model.res,data.frame(pseudoabs),n.trees=model.res$gbm.call$best.trees,type="response")

    eval3<-dismo::evaluate(p=testp,a=testa)
    #eval.stats$AUC_cv<-model.res$cv.statistics$discrimination.mean
    eval.stats$AUC<-eval3@auc
    eval.stats$COR<-eval3@cor
    eval.stats$pCOR<-eval3@pcor

    eval.stats$residuals<-list(model.res$residuals)

    # mesure TSS
    specificity <- (eval3@TPR/(eval3@TPR+eval3@FPR))
    sensitivity <- (eval3@TNR/(eval3@TNR+eval3@FNR))
    tss <- specificity + sensitivity -1
    eval.stats$TSS<- mean(tss, na.rm=T)

    # maxSSS
    tab<-cbind(eval3@t,eval3@TPR+eval3@TNR)
    eval.stats$maxSSS<-(subset(tab,tab[,2]==max(tab[,2])))[1,1]

    return(list(algorithm = "brt", data = x, response = model.res, raster.prediction = prediction, eval.stats=eval.stats))

}
