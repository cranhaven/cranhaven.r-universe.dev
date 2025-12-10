## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(specleanr)

## ----Getting iris and adding some outliers------------------------------------

irisdata1 <- iris

#introduce outlier data and NAs

rowsOutNA1 <- data.frame(x= c(344, NA,NA, NA),
                         x2 = c(34, 45, 544, NA), 
                         x3= c(584, 5, 554, NA),
                         x4 = c(575, 4554,474, NA), 
                         x5 =c('setosa', 'setosa', 'setosa', "setosa"))

colnames(rowsOutNA1) <- colnames(irisdata1)


dfinal <- rbind(irisdata1, rowsOutNA1)


## ----outlier detection using iris dataset-------------------------------------

setosadf <- dfinal[dfinal$Species%in%"setosa",c("Sepal.Width", 'Species')]

setosa_outlier_detection <- multidetect(data = setosadf, 
                                var = 'Sepal.Width', 
                                multiple = FALSE,
                                methods = c("adjbox", "iqr", "hampel","jknife",
                                            "seqfences", "mixediqr",
                                            "distboxplot", "semiqr",
                                            "zscore", "logboxplot", "medianrule"),
                       silence_true_errors = FALSE, 
                       missingness = 0.1,
                       sdm = FALSE,
                       na.inform = TRUE)

#extractMethods()


## ----ploting2, fig.width = 6, fig.height= 4, fig.align='center'---------------

ggoutliers(setosa_outlier_detection)



## ----clean data extraction and labelling--------------------------------------

setosa_qc_loess <- extract_clean_data(refdata = setosadf, 
                                      outliers = setosa_outlier_detection, loess = TRUE)

#clean dataset
nrow(setosa_qc_loess)

#reference data
nrow(setosadf)

setosa_qc_labeled <- classify_data(refdata = setosadf, outliers = setosa_outlier_detection)


## ----clean data extraction and labelling_multiple, fig.height=4.1, fig.width=4.5, fig.align='center'----


ggenvironmentalspace(setosa_qc_labeled, 
                     type = '1D',
                     ggxangle = 45, 
                     scalecolor = 'viridis',
                     xhjust = 1,
                     legend_position = 'blank',
                     ylab = "Number of records",
                     xlab = "Outlier labels")


## ----outlier detection using multiple-----------------------------------------

multspp_outlier_detection <- multidetect(data = dfinal, 
                                var = 'Sepal.Width', 
                                multiple = TRUE,
                                var_col = "Species",
                                methods = c("adjbox", "iqr", "hampel","jknife",
                                            "seqfences", "mixediqr",
                                            "distboxplot", "semiqr",
                                            "zscore", "logboxplot", "medianrule"),
                       silence_true_errors = FALSE, 
                       missingness = 0.1,
                       sdm = FALSE,
                       na.inform = TRUE)


## ----ploting_single, fig.width = 6, fig.height= 4, fig.align='center'---------

ggoutliers(multspp_outlier_detection)



## ----clean data extraction and labelling mult---------------------------------

multsp_qc_loess <- extract_clean_data(refdata = dfinal, 
                                      outliers = multspp_outlier_detection,
                                      var_col = 'Species',
                                      loess = TRUE)

#clean dataset
nrow(multsp_qc_loess)

#reference data
nrow(dfinal)

multi_qc_labeled <- classify_data(refdata = dfinal, 
                                      outliers = multspp_outlier_detection,
                                  var_col = 'Species')


## ----visualise data labelling, fig.height=4.5, fig.width=6.5, fig.align='center'----

ggenvironmentalspace(multi_qc_labeled, 
                     type = '1D',
                     ggxangle = 45, 
                     scalecolor = 'viridis',
                     xhjust = 1,
                     legend_position = 'blank',
                     ylab = "Number of records",
                     xlab = "Outlier labels")


## ----multiple variables of interest-------------------------------------------

multivariables <- multidetect(data = dfinal, multiple = TRUE,
                      var = c('Sepal.Length', 'Sepal.Width'), output = 'outlier',
                      var_col = 'Species',
                      methods = c('zscore', 'adjbox',
                                  'logboxplot', 'distboxplot',
                                  'iqr', 'semiqr','seqfences','hampel',
                                  'jknife'), 
                      warn = FALSE,
                      sdm = FALSE)


## ----ploting, fig.width = 6, fig.height= 4, fig.align='center'----------------

ggoutliers(multivariables)



## ----data extraction----------------------------------------------------------
#outliers will be returned to NA for each variable

lenwidth_clean <- extract_clean_data(dfinal, outliers = multivariables, 
                                     var_col = 'Species', outlier_to_NA = TRUE, threshold = 0.8)
nrow(lenwidth_clean)

lenwidth_long <- extract_clean_data(dfinal, outliers = multivariables, 
                                     var_col = 'Species', outlier_to_NA = FALSE, threshold = 0.8)
nrow(lenwidth_long)


