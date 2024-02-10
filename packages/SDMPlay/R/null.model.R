#' Compute null model
#'
#'@description
#'Compute null model. Null models are useful tools to highlight an a priori evaluation of the influence of presence records spatial structuration in model predictions (i.e. influence of aggregated sampling).
#'\cr Null model type #1 performs a model by randomly sampling locations from the ensemble of visited stations, therefore simulating the influence of sampling effort on model predictions.
#'
#'Null model type #2 samples data in the entire study area, and reflects what should be predicted if occurrences were randomly distributed in the area.
#'
#'Models should be replicated \emph{nb.rep} times in order to estimate statistical scores.
#'
#'@usage
#'null.model(predictors, xy = NULL, type = c(1, 2), algorithm = c("brt", "maxent"), nb,
#'          unique.data = T, same = T, background.nb = nb, nb.rep = 10, tc = 2,
#'          lr = 0.001, bf = 0.75, n.trees = 50, step.size = n.trees)
#'
#'@param predictors Rasterstack object that contains the predictors that will be used for species distribution models
#'@param type Null model type to perform. type=1 to perform a null model based on visited areas, type=2 to predict random model
#'@param algorithm Algorithm to compute the null model. 'brt' or 'maxent'
#'@param xy Dataframe that contains the longitude and latitude of the visited pixels. Information required to perform type 1 null model. Default= NULL
#'@param nb Number of points to randomly sample (among the matrix of visited pixels for 'type=1' model or in the entire geographic space for 'type=2')
#'@param same If TRUE (default), the number of background data sampled in the area will be 'nb'
#'@param unique.data If TRUE (default), pixel duplicates contained in 'xy' are removed
#'@param background.nb Number of background data to sample. If this argument is filled, 'same' is set FALSE.
#'@param nb.rep Null models number of replicates. See \link{compute.brt}

#'@param tc BRT parameter. Integer. Tree complexity. Sets the complexity of individual trees. See \link{compute.brt}
#'
#'@param lr BRT parameter.Learning rate. Sets the weight applied to individual trees. See \link{compute.brt}
#'
#'@param bf BRT parameter.Bag fraction. Sets the proportion of observations used in selecting variables. See \link{compute.brt}
#'
#'@param n.trees BRT parameter.Number of initial trees to fit. Set at 50 by default. See \link{compute.brt}
#'
#'@param step.size BRT parameter.Number of trees to add at each cycle. See \link{compute.brt}
#'

#'
#'@details
#'Data are sampled without replacement.
#'Each time the model is runned, new data (presence and background data) are sampled
#'
#'
#'@return
#'List of 6
#'\itemize{
#'\item \emph{$inputs}      Remembers the arguments used to implement null.model function
#'\item \emph{$eval}        Evaluation parameters of each model that compose the null model. See \link{SDMeval} for further information
#'\item \emph{$eval.null}   Evaluation of the mean null model. See \link{SDMeval} for further information
#'\item \emph{$pred.stack}  RasterStack of all the models produced to build the null model
#'\item \emph{$pred.mean}   Raster layer. Null model prediction. Mean of the $pred.stack RasterStack
#'\item \emph{$correlation} Spearman rank test value between the different maps produced }
#'
#'@note
#'Increasing the number of replications will enhance model null relevance (we advice nb.rep=100 for minimum). Please note that processing may take few minutes to hours.
#'
#'If you want to build a MaxEnt model, \link{compute.maxent} uses the functionalities of the \link[dismo]{maxent} function. This function uses MaxEnt species distribution software, which is a java program that could be downloaded at \url{https://github.com/charleneguillaumot/SDMPlay}. In order to run compute.maxent, put the 'maxent.jar' file downloaded at this adress in the 'java' folder of the dismo package (path obtained with the system.file('java', package='dismo') command). MaxEnt 3.3.3b version or higher is required.
#'
#'@seealso
#'\link[dismo]{nicheOverlap}: compare prediction maps
#'\link[rJava]{.jpackage}: initialize dismo for Java
#'
#'@examples
#'\dontrun{
#'# Load environmental predictors
#'data(predictors2005_2012)
#'envi <- predictors2005_2012
#'envi
#'
#' # Realise a null model type #2 with BRT
#' #--------------------------------------
#' modelN2 <- SDMPlay:::null.model(xy=NULL,predictors=envi,type=2,algorithm='brt',
#'                      nb=300,unique.data=TRUE, same=TRUE, nb.rep=2,lr=0.0005)
#'
#'# Look at the inputs used to implement the model
#'modelN2$input
#'
#'# Get the evaluation of the models produced
#'modelN2$eval
#'
#'# Get the evaluation of the mean of all these produced models (i.e. evaluation
#'# of the null model)
#'modelN2$eval.null
#'
#'# Get the values of Spearman correlations between the all the prediction maps produced
#'modelN2$correlation
#'
#'# Plot the mean null model map with nice colors
#'library(grDevices)
#'palet.col <- colorRampPalette(c('deepskyblue','green','yellow', 'red'))(80)
#'data('worldmap')
#'raster::plot(modelN2$pred.mean, col=palet.col)
#'points(worldmap, type="l")
#'}



null.model <- function(predictors, xy = NULL, type = c(1, 2), algorithm = c("brt", "maxent"), nb,unique.data = T, same = T, background.nb = nb, nb.rep = 10, tc = 2, lr = 0.001, bf = 0.75,
                       n.trees = 50, step.size = n.trees) {

    # remove duplicate data in the 'xy' frame
    if (unique.data == T) {
        xy <- unique(xy)
    }

    # initializing the data frame that will contain the evaluation data
    eval <- data.frame(AUC.value = NA, MaxSSS = NA, preferential.area = NA, omission.rate = NA, nb.omission = NA,
        SD.value = NA)

    # initializing the rasterStack
    stack.pred <- raster::subset(predictors, 1)
    raster::values(stack.pred) <- NA

    # initializing the dataframe that will contain the pixels values
    p.vect0 <- raster::reclassify(raster::subset(predictors, 1), base::cbind(NA, -99999))
    tableau.cor <- data.frame(raster::rasterToPoints(p.vect0)[, 3])
    tableau.cor <- base::replace(tableau.cor, values = NA)
    colnames(tableau.cor) <- "initial"


    ############################## MAXENT ################################################
    if (algorithm == "maxent")
        {
            # Type 1 model: sampling among visited areas
            if (type == 1)
                {

                  for (i in 1:nb.rep) {
                    samples <- xy[sample(nrow(xy), nb), ]

                    # building the SDMtab file ==========================
                    SDMtab.object <- SDMtab(xydata = samples, predictors = predictors, unique.data = unique.data, same = same,background.nb=background.nb)

                    # Running the model ==========================
                    model <- compute.maxent(x = SDMtab.object, proj.predictors = predictors)

                    # evaluate the model and add the data in a dataframe
                    eval <- base::rbind(eval, SDMeval(model))

                    # store the prediction map in a RasterStack
                    stack.pred <- raster::stack(stack.pred, model$raster.prediction)

                    # get the values of the pixels in a dataframe, each column = a run, each line= a pixel value
                    prediction.wNA <- raster::reclassify(model$raster.prediction, base::cbind(NA, -99999))
                    p.vect <- data.frame(raster::rasterToPoints(prediction.wNA)[, 3])
                    p.vect <- replace(p.vect, p.vect == -99999, NA)

                    tableau.cor <- base::cbind(tableau.cor, p.vect)

                  }  #close loop for nb.rep
                }  # close type #1

            # Type 2 model
            if (type == 2)
                {
                  for (boot in 1:nb.rep) {
                    # converting a a layer of the predictor stack into a data frame to have the coordinates
                    vect.r <- raster::reclassify(raster::subset(predictors, 1), base::cbind(NA, -99999))  # convert NA because rasterToPoints function does not deal with them
                    vect <- data.frame(raster::rasterToPoints(vect.r)[, c(1, 2)])
                    vect <- base::replace(vect, vect[, 1] == -99999, NA)
                    vect <- base::replace(vect, vect[, 2] == -99999, NA)

                    samples <- vect[sample(nrow(vect), nb), ]

                    # building the SDMtab file ==========================
                    SDMtab.object <- SDMtab(xydata = samples, predictors = predictors, unique.data = unique.data,
                      same = same, background.nb = background.nb)

                    # Running the model ==========================
                    model <- compute.maxent(x = SDMtab.object, proj.predictors = predictors)

                    # evaluate the model and add the data in a dataframe
                    eval <- base::rbind(eval, SDMeval(model))

                    # store the prediction map in a RasterStack
                    stack.pred <- raster::stack(stack.pred, model$raster.prediction)

                    # get the values of the pixels in a dataframe, each column = a run, each line= a pixel value
                    prediction.wNA <- raster::reclassify(model$raster.prediction, base::cbind(NA, -99999))
                    p.vect <- data.frame(raster::rasterToPoints(prediction.wNA)[, 3])
                    p.vect <- replace(p.vect, p.vect == -99999, NA)

                    tableau.cor <- base::cbind(tableau.cor, p.vect)

                  }  # close for loop nb.rep
                }  # close type#2
        }  # close algorithm maxent


    # ########################### BRT ################################
    if (algorithm == "brt")
        {
            if (type == 1)
                {
                  for (i in 1:nb.rep) {
                    samples <- xy[sample(nrow(xy), nb), ]

                    # building the SDMtab file ==========================
                    SDMtab.object <- SDMtab(xydata = samples, predictors = predictors, unique.data = unique.data,
                      same = same, background.nb = background.nb)

                    # Running the model ==========================
                    model <- compute.brt(x = SDMtab.object, proj.predictors = predictors,
                                         tc=tc,lr=lr,bf=bf,n.trees=n.trees,step.size=step.size)

                    # evaluate the model and add the data in a dataframe
                    eval <- base::rbind(eval, SDMeval(model))

                    # store the prediction map in a RasterStack
                    stack.pred <- raster::stack(stack.pred, model$raster.prediction)

                    # get the values of the pixels in a dataframe, each column = a run, each line= a pixel value
                    prediction.wNA <- raster::reclassify(model$raster.prediction, base::cbind(NA, -99999))
                    p.vect <- data.frame(raster::rasterToPoints(prediction.wNA)[, 3])
                    p.vect <- replace(p.vect, p.vect == -99999, NA)

                    tableau.cor <- base::cbind(tableau.cor, p.vect)

                  }  # close loop for
                }  # close type #1

            # Type 2 model
            if (type == 2)
                {
                  for (boot in 1:nb.rep) {
                    # converting a a layer of the predictor stack into a data frame to have the coordinates
                    vect.r <- raster::reclassify(raster::subset(predictors, 1), base::cbind(NA, -99999))  # convert NA because rasterToPoints function does not deal with them
                    vect <- data.frame(raster::rasterToPoints(vect.r)[, c(1, 2)])
                    vect <- base::replace(vect, vect[, 1] == -99999, NA)
                    vect <- base::replace(vect, vect[, 2] == -99999, NA)

                    samples <- vect[sample(nrow(vect), nb), ]

                    # building the SDMtab file ==========================
                    SDMtab.object <- SDMtab(xydata = samples, predictors = predictors, unique.data = unique.data,
                      same = same, background.nb = background.nb)

                    # Running the model ==========================
                    model <- compute.brt(x = SDMtab.object, proj.predictors = predictors,
                                         tc=tc,lr=lr,bf=bf,n.trees=n.trees,step.size=step.size)

                    # evaluate the model and add the data in a dataframe
                    eval <- base::rbind(eval, SDMeval(model))

                    # store the prediction map in a RasterStack
                    stack.pred <- raster::stack(stack.pred, model$raster.prediction)

                    # get the values of the pixels in a dataframe, each column = a run, each line= a pixel value
                    prediction.wNA <- raster::reclassify(model$raster.prediction, base::cbind(NA, -99999))
                    p.vect <- data.frame(raster::rasterToPoints(prediction.wNA)[, 3])
                    p.vect <- replace(p.vect, p.vect == -99999, NA)

                    tableau.cor <- base::cbind(tableau.cor, p.vect)

                  }  # close loop for
                }  # close type #2

        }  # close algorithm=brt

    ##### Make the list of returned options ##### First list argument: list containing all the parameters
    ##### entered in the function
    arguments <- list(xy = xy, predictors = predictors, type = type, algorithm = algorithm, nb = nb,
        same = same, background.nb = background.nb, nb.rep = nb.rep)
    # Second list argument : Dataframe containing the evaluation information of all the models
    # produced : delete the first row of the eval dataframe that is empty
    eval <- eval[-1, ]
    rownames(eval) <- seq(from = 1, to = nb.rep, by = 1)

    # Third list argument Dataframe containing the evaluation information of all the null model
    # (mean):
    eval.mean <- apply(eval, FUN = mean, MARGIN = 2, na.rm = T)

    # Fourth list argument : RasterStack of all the maps produced delete the first layer that is empty
    pred.stack <- raster::dropLayer(stack.pred, i = 1)

    # Fifth list argument: Mean prediction map
    pred.mean <- raster::calc(pred.stack, fun = mean, na.rm = T)

    # Sixth list argument : correlation values between all the maps produced table with the values of
    # each raster map delete the first column
    tableau.cor <- tableau.cor[, -1]
    colnames(tableau.cor) <- seq(from = 1, to = nb.rep, by = 1)

    # measure the correlation between each column : compute correlation matrix
    matrice.cor <- stats::cor(tableau.cor, metho = "spearman", use = "pairwise.complete.obs")
    # obtain the values of the matrix in a vector
    lt <- lower.tri(matrice.cor)
    data.cor <- matrice.cor[lt]

    return(list(input = arguments, eval = eval, eval.null = eval.mean, pred.stack = pred.stack, pred.mean = pred.mean,
        correlation = data.cor))

}  # close the function
