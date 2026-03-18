## -----------------------------------------------------------------------------
library(MSiP)

## -----------------------------------------------------------------------------
data("SampleDatInput")
head(SampleDatInput)

## -----------------------------------------------------------------------------
datScoring <- 
    cPASS(SampleDatInput)
head(datScoring)

## -----------------------------------------------------------------------------
datScoring <- 
    diceCoefficient(SampleDatInput)
head(datScoring)

## -----------------------------------------------------------------------------
#Jaccard coefficient
datScoring <- 
    jaccardCoefficient(SampleDatInput)
head(datScoring)

#Simpson coefficient
datScoring <- 
    simpsonCoefficient(SampleDatInput)
head(datScoring)

#Overlap score
datScoring <- 
    simpsonCoefficient(SampleDatInput)
head(datScoring)

## -----------------------------------------------------------------------------
datScoring <- 
Weighted.matrixModel(SampleDatInput)
head(datScoring)

## -----------------------------------------------------------------------------
data("testdfClassifier")
head(testdfClassifier)


## ----rfTrain output figure, echo=FALSE, fig.height=4, fig.width=5, message=FALSE, warning=FALSE, paged.print=FALSE----
#only generate the pr.curve
predidcted_RF <- 
    rfTrain(testdfClassifier,impute = FALSE, p = 0.3, parameterTuning = FALSE,
            mtry  = seq(from = 1, to = 5, by = 1),
            min_node_size = seq(from = 1, to = 5, by = 1),
            splitrule =c("gini"),metric = "Accuracy",
            resampling.method = "repeatedcv",iter = 5,repeats = 5,
            pr.plot = TRUE, roc.plot = FALSE
    )

## -----------------------------------------------------------------------------
#positive score corresponds to  the level of support for the pair of proteins to be true positive
#negative score corresponds to the level of support for the pair of proteins to be true negative
head(predidcted_RF)

## -----------------------------------------------------------------------------
#only generate the ROC curve
predidcted_SVM <- 
    svmTrain(testdfClassifier,impute = FALSE,p = 0.3,parameterTuning = FALSE,
             cost = seq(from = 2, to = 10, by = 2),
             gamma = seq(from = 0.01, to = 0.10, by = 0.02),
             kernel = "radial",ncross = 10,
             pr.plot = FALSE, roc.plot = TRUE
    ) 

## -----------------------------------------------------------------------------
#positive score corresponds to  the level of support for the pair of proteins to be true positive
#negative score corresponds to the level of support for the pair of proteins to be true negative
head(predidcted_SVM)

