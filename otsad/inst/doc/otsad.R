## ----install1, eval = FALSE----------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("alaineiturria/otsad")

## ----install2------------------------------------------------------------
library(otsad)

## ----document1, eval=FALSE-----------------------------------------------
#  ?CpSdEwma
#  help(CpSdEwma)

## ----generateData--------------------------------------------------------
## Generate data
set.seed(100)
n <- 500
x <- sample(1:100, n, replace = TRUE)
x[70:90] <- sample(110:115, 21, replace = TRUE) # distributional shift
x[25] <- 200 # abrupt transient anomaly
x[320] <- 170 # abrupt transient anomaly
df <- data.frame(timestamp = 1:n, value = x)

## ----plotData, eval = FALSE----------------------------------------------
#  plot(x = df$timestamp, y = df$value, type = "l",
#       main = "Time-Serie", col = "blue", xlab = "Time", ylab = "Value")

## ----acfAndPacf, eval = FALSE--------------------------------------------
#  forecast::Acf(ts(df$value), main = "ACF", lag = 20)
#  forecast::Pacf(ts(df$value), main = "PACF", lag = 20)

## ----stationarityTests, eval = FALSE-------------------------------------
#  library(tseries)
#  adf.test(df$value, alternative = 'stationary', k = 0)
#  kpss.test(df$value)

## ----detector------------------------------------------------------------
result <- CpSdEwma(data = df$value, n.train = 5, threshold = 0.01, l = 3)

## ----printResult---------------------------------------------------------
head(result, n = 15)

## ----plotResult, eval = FALSE--------------------------------------------
#  res <- cbind(df, result)
#  PlotDetections(res, title = "KNN-CAD ANOMALY DETECTOR")

## ----detector2, eval = FALSE---------------------------------------------
#  ## Initialize parameters for the loop
#  last.res <- NULL
#  res <- NULL
#  nread <- 250
#  numIter <- n%/%nread
#  iter <- seq(1, nread * numIter, nread)
#  
#  ## Calculate anomalies
#  for(i in iter) {
#    # read new data
#    newRow <- df[i:(i + nread - 1),]
#    # calculate if it's an anomaly
#    last.res <- IpSdEwma(
#      data = newRow$value,
#      n.train = 5,
#      threshold = 0.01,
#      l = 3,
#      last.res = last.res$last.res
#    )
#    # prepare the result
#    if(!is.null(last.res$result)){
#      res <- rbind(res, cbind(newRow, last.res$result))
#    }
#  }

## ----plotResult2, eval = FALSE-------------------------------------------
#  PlotDetections(res, title = "SD-EWMA ANOMALY DETECTOR")

## ----contextual.ad, eval = FALSE-----------------------------------------
#  cad.results <- ContextualAnomalyDetector(
#    df$value[1:250],
#    base.threshold = 0.75,
#    rest.period = 5,
#    max.value = 200,
#    min.value = 0
#  )
#  cad.results.online <- ContextualAnomalyDetector(
#    df$value[251:500],
#    python.object = cad.results$python.Object
#  )
#  
#  res <- cbind(df, rbind(cad.results$result, cad.results.online$result))
#  PlotDetections(res, title = "CONTEXTUAL ANOMALY DETECTOR")

## ----exapmle1, eval = FALSE----------------------------------------------
#  # Load the previously installed otsad package
#  library("otsad")
#  
#  # Load the dataset speed_7578, included in otsad
#  myData <- speed_7578
#  
#  # Initialize parameters
#  n <- nrow(myData)
#  n.train <- GetNumTrainingValues(n)
#  
#  # Calculate anomalies using KNN-CAD
#  result <- CpKnnCad(
#    data = myData$value,
#    n.train = n.train,
#    threshold = 1,
#    l = 19,
#    k = 27,
#    ncm.type = "ICAD",
#    reducefp = TRUE
#  )
#  
#  # Add results to dataset
#  myData <- cbind(myData, result)
#  

## ----exapmle1a, eval = FALSE---------------------------------------------
#  # Plot Results
#  PlotDetections(myData, title = "KNN-CAD ANOMALY DETECTOR")

## ----exapmle1b, eval = FALSE---------------------------------------------
#  # Get detector score
#  score <- GetDetectorScore(myData)
#  
#  # See this results
#  data.frame(score[-1])

## ----exapmle1c, eval = FALSE---------------------------------------------
#  # Get detector score
#  score <- GetDetectorScore(
#    myData,
#    print = TRUE,
#    title = "speed_7578 results using KNN-CAD detector"
#  )

## ----exapmle1d, eval = FALSE---------------------------------------------
#  # Normalize results
#  null.perfect <- GetNullAndPerfectScores(myData)
#  
#  standar.score <- NormalizeScore(
#    score$standard,
#    perfect.score = null.perfect[1, "perfect.score"],
#    null.score = null.perfect[1, "null.score"]
#  )
#  
#  low_FP_rate.score <- NormalizeScore(
#    score$low_FP_rate,
#    perfect.score = null.perfect[2, "perfect.score"],
#    null.score = null.perfect[2, "null.score"]
#  )
#  
#  low_FN_rate.score <- NormalizeScore(
#    score$low_FN_rate,
#    perfect.score = null.perfect[3, "perfect.score"],
#    null.score = null.perfect[3, "null.score"]
#  )
#  
#  # Show normalized scores
#  cbind(standar.score, low_FP_rate.score, low_FN_rate.score)
#  

