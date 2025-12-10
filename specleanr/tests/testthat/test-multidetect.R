
data("jdsdata")
data("efidata")

wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))

#match and clean

matchd <- match_datasets(datasets = list(jds= jdsdata, efi =efidata),
                         lats = 'lat', lons = 'lon',
                         country = 'JDS4_site_ID',
                         species = c('scientificName', 'speciesname'),
                         date=c('sampling_date','Date'))

db <- sf::read_sf(system.file('extdata/danube.shp.zip',
                              package = "specleanr"), quiet = TRUE)


#extract environmental data

refdata <- pred_extract(data = matchd, raster = wcd,
                        lat = 'decimalLatitude',
                        lon = 'decimalLongitude',
                        bbox = db,
                        colsp = 'species',
                        list = TRUE,
                        verbose = FALSE,
                        minpts = 6,
                        merge = FALSE)

#using a dataframe of species not a list# change list to FALSE

refdata_df <- pred_extract(data = matchd, raster = wcd,
                        lat = 'decimalLatitude',
                        lon = 'decimalLongitude',
                        bbox = db,
                        colsp = 'species',
                        list = FALSE,
                        verbose = FALSE,
                        minpts = 6,
                        merge = FALSE)

#for multiple species
outlist <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                       exclude = c('x','y'),
                       multiple = TRUE,
                       methods = c('mixediqr', "iqr", "seqfences",
                                   "mahal", 'glosh', 'onesvm','logboxplot','knn',
                                   'distboxplot','medianrule',
                                   'iforest','jknife','reference'))


test_that(desc = "Missing data returns an error",
          code = {
            expect_error(multidetect(var = 'bio6', output = 'outlier',
                                     exclude = c('x','y'),
                                     multiple = TRUE,
                                     methods = c('mixediqr', "iqr", "seqfences",
                                                 "mahal", 'glosh', 'onesvm','logboxplot','knn',
                                                 'distboxplot','medianrule',
                                                 'iforest','jknife','reference')))
          })

test_that(desc = 'List of species with outliers produced',
                    code = {

                      testthat::expect_equal(length(outlist@result), length(refdata))
                      testthat::expect_identical(outlist@dfname, "refdata")
                      testthat::expect_identical(outlist@varused, "bio6")
                      testthat::expect_identical(outlist@excluded, c('x','y'))
                      testthat::expect_identical(outlist@methodsused, c('mixediqr', "iqr", "seqfences",
                                                                        "mahal", 'glosh', 'onesvm','logboxplot','knn',
                                                                        'distboxplot','medianrule',
                                                                        'iforest','jknife','reference'))
                      testthat::expect_identical(outlist@out, "outlier")
                      testthat::expect_identical(outlist@mode, TRUE)
                      testthat::expect_type(refdata, 'list') #not a dataframe but list

                      #test that the datacleaner is printed out: multiple species
                      expect_output(show(outlist))

                      #multiple but index not provided.
                      expect_s3_class(extractoutliers(outlist), 'data.frame')
                    })

#test trycatch errors

test_that(desc = "Return errors and no errors",
          code = {
            #expect warning when the kmpar for kmeans the k is less than 2 and silence_true_errors = FALSE
            #that some methods particularly kmeans did not execute
            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                     exclude = c('x','y'),
                                     multiple = TRUE,
                                     methods = c('mixediqr', "iqr", "seqfences",
                                                 "mahal", 'glosh', 'onesvm','kmeans'),
                                     kmpar= list(k = 1, method = "silhouette", mode = "soft"),
                                     silence_true_errors = FALSE))

            #expect to hide the error/warning in the hood when silence_true_errors is FALSE and expect datacleaner

            outlierFALSE <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                        exclude = c('x','y'),
                                        multiple = TRUE,
                                        methods = c('mixediqr',"mahal", 'glosh', 'onesvm'),
                                        kmpar= list(k = 1, method = "silhouette", mode = "soft"),
                                        silence_true_errors = FALSE)
            expect_s4_class(outlierFALSE, 'datacleaner')

            #extract the outlier for species index = 2 to confirm that kmeans is not among the methods
            dfout <- extractoutliers(x= outlierFALSE, sp = 2)

            expect_false("kmeans"%in%unlist(dfout$method)) # kmeans did not exceute
            expect_true("mahal"%in%unlist(dfout$method)) # mahal executed

            #expect an error if the var (bio61) is not in the data.
            expect_error(multidetect(data = refdata, var = 'bio61', output = 'outlier',
                                     exclude = c('x','y'), multiple = TRUE,
                                     methods = c('mixediqr',"mahal", 'glosh', 'onesvm')))
            #if methods are not provided
            expect_error(multidetect(data = refdata, var = 'bio61', output = 'outlier',
                                     exclude = c('x','y'), multiple = TRUE))

            #if the var is included in the exclude vector
            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                     exclude = c('x','y', "bio6"), multiple = TRUE))

            #if the out the output is wrong (clean and outlier) accepted
            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlyingpoints',
                                     exclude = c('x','y', "bio6"), multiple = TRUE))

            #"All methods indicated in the allowed methods-seqfenc is not in

            expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                     exclude = c('x','y'), multiple = TRUE,
                                     methods = c('mixediqr', "iqr", "seqfenc")))
          })



#single species checks
spdata <- refdata[['Anguilla anguilla']]

testthat::test_that(desc = 'Not enough data provided and other methods may not work properly',
                    code = {
                      expect_warning(multidetect(data = spdata, var = 'bio6', output = 'outlier',
                                                 exclude = c('x','y'),
                                                 multiple = FALSE,
                                                 warn = TRUE,
                                                 methods = c('mixediqr', "iqr", "kmeans", "mahal")))

                      #provide a list of dataframes but when multiple is FALSE returns an error-expects a dataframe
                      expect_error(multidetect(data = refdata, var = 'bio6', output = 'outlier',
                                               exclude = c('x','y'),
                                               multiple = FALSE,
                                               methods = c('mixediqr', "iqr", "kmeans", "mahal")))
                      #use select function
                      expect_s4_class(multidetect(data = refdata, var = "bio6",
                                                  select = c('bio1', 'bio2', 'bio3', 'bio4', 'bio6', 'bio16', 'bio19'),
                                                  multiple = TRUE,
                                                  methods = c('logboxplot', 'iqr', 'semiqr', 'hampel', 'mixediqr'),
                                                  sdm = TRUE), 'datacleaner')

                    })

#=============
#outlier output to test for data extraction
#==========
outlierdf <- multidetect(data = refdata, var = 'bio6', output = 'outlier',
                         exclude = c('x','y'),
                         multiple = TRUE,
                         methods = c('mixediqr', "iqr", "kmeans", "mahal"))

#outliers generated using a dataframe not list: var_col parameter is provided.

outlierdf2 <- multidetect(data = refdata_df, var = 'bio6', output = 'outlier',
                          exclude = c('x','y'),
                          var_col = 'species',
                          multiple = TRUE,
                          methods = c('mixediqr', "iqr", "kmeans", "mahal"))


#test for removing NAs when only univariate methods are selected
irisdata <- iris
#add outlier data and NAs
rowsOutNA <- data.frame(x= c(344, NA,NA, NA), x2 = c(34, 45, 544, NA), x3= c(584, 5, 554, NA),
                        x4 = c(575, 4554,474, NA), x5 =c('Setosa', 'Setosa', 'Setosa', "Setosa"))
colnames(rowsOutNA) <- colnames(irisdata)

dfinal <- rbind(irisdata, rowsOutNA)

test_that(desc= "Errors ",
          code = {
            seldf <- dfinal[, 4:5]
            #Not enough columns to run SDMs
            expect_error(multidetect(data = seldf, var = "Petal.Width", multiple = FALSE,
                                     methods = c('logboxplot', 'iqr', 'semiqr', 'hampel', 'mixediqr'),
                                     sdm = TRUE))
            expect_s4_class(multidetect(data = seldf, var = "Petal.Width", multiple = FALSE,
                                        methods = c('logboxplot', 'iqr', 'semiqr', 'hampel', 'mixediqr'),
                                        sdm = FALSE), 'datacleaner')
          })

test_that(desc = "NAs if univariate methods only selected",
          code = {
            #silence_true_errors FALSE but the methods are well set: datacleaner produced and verbose = TRUE

            outl <- suppressMessages(multidetect(data = dfinal, var = 'Sepal.Width',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','kmeans', 'lof'), verbose =TRUE,
                        silence_true_errors = FALSE))
            expect_s4_class(outl, 'datacleaner')

            #test that the singl species multidetect is printed
            expect_output(show(outl))

            #return an error if the method parameter are wrongly set: using kmeans, warn=TRUE & silence_true_errors=TRUE
            #method wrongly set
            expect_warning(multidetect(data = dfinal, var = 'Sepal.Width',
                       multiple = FALSE,
                       methods = c('mixediqr', 'logboxplot','kmeans'), warn=TRUE,
                       silence_true_errors = TRUE,
                       kmpar = list(method="silhoest")))

            #expect error if non numeric variable are provided as variable checks
            expect_error(multidetect(data = dfinal, var = 'Species',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','iqr'), missingness = 0.01))

            #expect error if data is not provided
            expect_error(multidetect(var = 'Sepal.Width',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','iqr'), missingness = 0.01))

            #NA removed during computation only but the rows are left in the data output.
            testthat::expect_message(dout <- multidetect(data = dfinal, var = 'Sepal.Length',
                                                         multiple = FALSE,
                                                         methods = c('mixediqr', 'logboxplot','iqr'), verbose = TRUE))
            testthat::expect_s4_class(dout, 'datacleaner')

            #check that the cleaned data has the NAs not removed mostly for univariate data

            cleandata <- extract_clean_data(refdata = dfinal, outliers = dout, threshold = 0.6)

            testthat::expect_true(any(is.na(cleandata$Sepal.Length)))

            #if missingness leads to removal of the main variable-- var: missingness set to 0.001
            testthat::expect_error(multidetect(data = dfinal, var = 'Sepal.Length',
                                     multiple = FALSE,
                                     methods = c('mixediqr', 'logboxplot','iqr'), missingness = 0.001))

            #execute successfully if the missigness is greater the proportional of missing values in the dataset.
            testthat::expect_s4_class(multidetect(data = dfinal, var = 'Sepal.Length',
                                        multiple = FALSE,
                                        methods = c('mixediqr', 'logboxplot','iqr'), missingness = 0.1), 'datacleaner')


          })

multvarout <- multidetect(data = dfinal, multiple = TRUE,
                          var = c('Sepal.Length', 'Sepal.Width'), output = 'outlier',
                          var_col = 'Species',
                          methods = c('zscore', 'adjbox',
                                      'logboxplot', 'distboxplot',
                                      'iqr', 'semiqr','seqfences','hampel',
                                      'jknife'),
                          warn = FALSE,
                          sdm = FALSE)

test_that(desc='expect error if multiple is not FALSE for multvar',
          code = {
            expect_error(multidetect(data = dfinal,
                                     multiple = FALSE,
                                     var = c('Sepal.Length', 'Sepal.Width'),
                                     output = 'outlier',
                                     var_col = 'Species',
                                     methods = c('zscore', 'adjbox',
                                                 'logboxplot', 'distboxplot'),
                                     warn = FALSE,
                                     sdm = FALSE))
          })

test_that(desc='expect error if multiple is FALSE but var_col is not null',
          code = {
            expect_error(multidetect(data = dfinal, multiple = FALSE,
                                     var = c('Sepal.Length'),
                                     output = 'outlier',
                                     var_col = 'Species',
                                     methods = c('zscore', 'adjbox',
                                                 'logboxplot', 'distboxplot'),
                                     warn = FALSE,
                                     sdm = FALSE))
          })

test_that(desc = 'Implentation and error checks',
          code = {
            expect_s4_class(multvarout, 'datacleaner')

            seplen <- extract_clean_data(dfinal, outliers = multvarout,
                                         var_col = 'Species', outlier_to_NA = TRUE,
                                         threshold = 0.8)
            #same number of rows like original
            expect_equal(nrow(seplen), nrow(dfinal))

            seplen2 <- extract_clean_data(dfinal, outliers = multvarout,
                                          var_col = 'Species', outlier_to_NA = FALSE,
                                          threshold = 0.8)
            expect_gte(nrow(seplen2), nrow(dfinal))

            #expect error if threshold is NULL

            expect_error(extract_clean_data(dfinal, outliers = multvarout,
                                            var_col = 'Species', outlier_to_NA = TRUE))
          })

#outlier classification
#return errors with wrong specification
test_that(desc = "Errors for wrong specification or data",
          code = {
            #when clean data is output at the multidetect step
            cleanoutdf <- multidetect(data = dfinal, var = 'Sepal.Length', output = 'clean',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','iqr'))

            #error since x must datacleaner output but with outliers

            expect_error(ocindex(x = cleanoutdf, threshold = 0.6))

          })


test_that(desc = 'Test for success and errors during outlier detection and extraction',
                    code = {
                      #check out extract data

                      dx <- extractoutliers(x= outlierdf, sp=1)

                      expect_equal(nrow(dx), 4)#for each method

                      dxm <- extractoutliers(x=outlierdf)

                      #errors since no outliers in threshold of 0.8

                      expect_error(ocindex(x=outlierdf, sp=1,  threshold = 0.8, warn = FALSE))

                      #both threshold = NULL and autothreshold =FALSE
                      expect_error(ocindex(x=outlierdf, sp=1,  threshold = NULL, warn = FALSE, autothreshold = FALSE))

                      #both threshold = NOT NULL and autothreshold = TRUE
                      expect_error(ocindex(x=outlierdf, sp=1,  threshold = 0.2, warn = FALSE, autothreshold = TRUE))

                      #expect warning sinc the threshold is below 0.5
                      expect_warning(ocindex(x=outlierdf, sp=1, threshold = 0.2, warn = T))

                      #species index not provided
                      expect_error(ocindex(x=outlierdf, threshold = 0.2, warn = FALSE))

                      #threshold greater than 1
                      expect_error(ocindex(x = outlierdf, threshold = 1.2))

                      #threshold <less than 0 or = 0
                      expect_error(ocindex(x = outlierdf, threshold = -0.1))

                      #when the outliers x is not provided
                      expect_error(ocindex(threshold = -0.1))

                      #run autothreshold

                      #expect error: No absolute outliers from 0.51 to 1

                      expect_error(ocindex(x = outlist, autothreshold = TRUE, sp = 5))

                      #executed successfully with autothreshold

                      expect_type(ocindex(x = outlist, autothreshold = TRUE, sp = 1), 'character')

                    })

#preliminary for outlier extraction

test_that(desc = "Errors and success for extract outliers, extractoutliers, mult abs",
          code = {

            #expect error missing outlier output
            expect_error(extractoutliers())

            expect_error(multiabsolute())

            #expect error if datacleaner not provided
            expect_error(extractoutliers(x=wcd))

            expect_error(extractoutliers(x=outlierdf, sp ='snf')) #wrong species

            expect_error(multiabsolute(x= wcd, threshold = 0.2))

            #expect error if clean output is set at multidetect

            cleanout <- multidetect(data = dfinal, var = 'Sepal.Length', output = 'clean',
                        multiple = FALSE,
                        methods = c('mixediqr', 'logboxplot','iqr'))

            expect_error(extractoutliers(cleanout))

            #expect error for multiple item at multidetect but extractoutliers is used

            expect_error(extractoutliers(x=cleanout))

            #expect error for multiabsolute extract on clean data
            expect_error(multiabsolute(x= cleanout, threshold = 0.2))

            outabs <- multidetect(data = dfinal, var = 'Sepal.Length', output = 'outlier',
                                    multiple = FALSE,
                                    methods = c('mixediqr', 'logboxplot','iqr'))

            #expect error for multiabsolute extract on outlying data but threshold >1
            expect_error(multiabsolute(x= outabs, threshold = 1.3))

            #true multiple absolute outlier extract for multiple species-- the multiple for ociindex

            expect_s3_class(multiabsolute(x= outlist, threshold = 0.5, props = FALSE), 'data.frame')

            expect_s3_class(multiabsolute(outlist, threshold = 0.4, props = TRUE), 'data.frame')

            expect_s3_class(multiabsolute(x= outlist, autothreshold = TRUE, props = FALSE), 'data.frame')

            expect_s3_class(multiabsolute(outlist, autothreshold = TRUE, props = TRUE), 'data.frame')
          })





testthat::test_that(desc = 'Test for different method if they return a character/suitable method',
                    code = {
                      testthat::expect_type(jaccard(x=outlierdf, sp=1, warn = FALSE, threshold = 0.2), 'character')
                      testthat::expect_type(sorensen(x=outlierdf, sp=1,  warn = FALSE, threshold = 0.2), 'character')
                      testthat::expect_type(cosine(x=outlierdf, sp=1, warn = FALSE, threshold = 0.2), 'character')
                      testthat::expect_type(smc(x=outlierdf, sp=1,  warn = FALSE, threshold = 0.2), 'character')
                      testthat::expect_type(hamming(x=outlierdf, sp=1,  warn = FALSE, threshold = 0.2), 'character')
                      testthat::expect_type(overlap(x=outlierdf, sp=1,  warn = FALSE, threshold = 0.2), 'character')

                      #check for absolute = TRUE: expect a vector
                      testthat::expect_type(ocindex(x=outlierdf, threshold = 0.2, absolute = TRUE, warn = FALSE, sp = 1), 'double')

                      #check for absolute = TRUE and prop=TRUE: expect a dataframe
                      testthat::expect_s3_class(ocindex(x=outlierdf, threshold = 0.2, absolute = TRUE, warn = FALSE,
                                                        props = TRUE, sp = 1), 'data.frame')

                      #check for absolute = FALSE and prop=FALSE: expect a dataframe
                      testthat::expect_type(ocindex(x=outlierdf, threshold = 0.2, absolute = FALSE, props = FALSE,
                                                        warn = FALSE, sp=1 ), 'character')

                      #expect error: wrong data set for outliers. only accepts datacleaner
                      testthat::expect_error(ocindex(x=wcd, threshold = 0.2, absolute = TRUE, warn = FALSE, sp = 1))

                    })

#best methods checks

testthat::test_that(desc = "Success and errors expected and checked for best method selection.",
                    code = {
                      #one species
                      expect_type(bestmethod(x=outlierdf, sp=1, threshold = 0.2, warn = FALSE, verbose = FALSE),'character')

                      #expect error if the index is not provided yet the outlierdf is from multiple object.
                      expect_error(bestmethod(x=outlierdf, threshold = 0.2, warn = FALSE, verbose = FALSE))

                      #expect error if x is not provided.
                      expect_error(bestmethod(threshold = 0.2, warn = FALSE, verbose = FALSE, sp = 1))


                      #threshold greater than 1
                      expect_error(bestmethod(x = outlierdf, threshold = 1.2, sp = 1))

                      #threshold <less than 0 or = 0

                      #threshold greater than 1
                      expect_error(bestmethod(x = outlierdf, threshold = 0-1, sp = 1))

                      expect_success(expect_type(bestmethod(x = outlierdf, threshold = 0.2, sp = 1), 'character'))


                      #multiple species, expect dataframe
                      expect_s3_class(multibestmethod(x=outlierdf, threshold = 0.2, warn = FALSE, verbose = FALSE),
                                      'data.frame')

                      #use the one species for Anguilla anguilla (spdata): expect error because this can be extracted in
                      #bestmethod function not here with multiple species
                      out3 <-suppressWarnings(multidetect(data = spdata, output = 'outlier', var = 'bio6',
                                          methods = c('logboxplot', 'iqr', 'mixediqr'), multiple = FALSE))
                      expect_error(multibestmethod(x = out3, threshold = 0.2))
                    })


#tests for PCA and bootstrapping
data("ttdata", package = 'specleanr')

mergealltts <- match_datasets(datasets = list(efi= efidata, jds = jdsdata,
                                              onlinedata = ttdata),
                              country = c('JDS4_sampling_ID'),
                              lats = 'lat', lons = 'lon',
                              species = c('speciesname', 'scientificName'))

thymallusdata <- mergealltts[mergealltts[,'species'] %in%c("Thymallus thymallus"),]


ttrefdata <-  pred_extract(data= thymallusdata, raster= wcd,
                                         lat = 'decimalLatitude',
                                         lon = 'decimalLongitude',
                                         colsp = 'species',
                                         bbox  = db,
                                         list= TRUE,
                                         minpts = 10)

ttpca_boot <- multidetect(data = ttrefdata,
                          multiple = FALSE,
                          var = 'bio6',
                          exclude = c('x','y'),
                          methods = c('adjbox','zscore',
                                      'logboxplot', 'distboxplot',
                                      'iqr', 'semiqr',"medianrule",
                                      'hampel','kmeans', 'glosh', 'knn',
                                      'jknife', 'onesvm','mahal', 'lof',
                                      'iforest', "mixediqr","seqfences"),
                          bootSettings = list(run = TRUE,
                                              maxrecords = 120, nb = 10), #120 to ensure that BT works
                          pc = list(exec = TRUE, npc = 3, q = TRUE))

#==================================================
#test the graphs
#==================================================
test_that("output is a ggplot2 graph", {
  skip_if_not_installed("ggplot2")
  plt <- ggoutlieraccum(ttpca_boot, boots = 10)
  expect_s3_class(plt, "ggplot")
})

#check for environmental space
ccl <- classify_data(refdata = ttrefdata, outliers = ttpca_boot)

test_that('output is ggplot environmental space',
          code = {
            skip_on_cran()

            skip_if_not_installed("ggplot2")

            plt <- ggenvironmentalspace(qcdata  = ccl,
                                 xvar =  'bio1',
                                 yvar = 'bio6',
                                 xlab = 'Annual Mean Temperature',
                                 ylab = 'Min Temperature of Coldest Month',
                                 type = "2D",
                                 scalecolor  = 'viridis',
                                 pointsize = 2,
                                 fontsize = 12,
                                 themebackground = 'bw',
                                 legend_position  = 'inside',
                                 legend_inside =  c(0.2, 0.7)
            )
            expect_s3_class(plt, "ggplot")

            skip_if_not_installed("scatterplot3d")

            plt3d <- ggenvironmentalspace(qcdata  = ccl,
                                         xvar =  'bio1',
                                         yvar = 'bio6',
                                         zvar = "bio9",
                                         labelvar = "label",
                                         colorvalues = 'auto',
                                         type = "3D",
                                         lpos3d = "top",
                                         pch = "auto",
                                         cexsym = 1.1,
                                         xlab = 'Annual Mean Temperature',
                                         ylab = 'Min Temperature of Coldest Month',
                                         zlab = "Mean Temperature of Driest Quarter"
            )
            expect_type(plt3d, "list")
          })
#=================================================


testthat::test_that(desc = 'Check for PCA and boot out',
                    code = {
                      expect_true(ttpca_boot@pc)
                      expect_true(ttpca_boot@bootstrap)
                      expect_equal(length(ttpca_boot@methodsused), 18)
                      expect_gt(ttpca_boot@maxrecords, nrow(ttrefdata))
                    })

test_that(desc = "since PCA errors and warnings",
          code = {
            #since PCA runs on changed dataset, stop the code if optimal is set
            expect_error(multidetect(data = ttrefdata,
                                       multiple = FALSE,
                                       var = 'bio6',
                                       exclude = c('x','y'),
                                       methods = c('adjbox','zscore','optimal'),
                                       bootSettings = list(run = TRUE,
                                                           maxrecords = 120, nb = 10),
                                       pc = list(exec = TRUE, npc = 3, q = TRUE)))

            #expect warning from bootstrap about max records/bootstrap will change to FALSE
            expect_warning(multidetect(data = ttrefdata,
                                     multiple = FALSE,
                                     var = 'bio6',
                                     exclude = c('x','y'),
                                     methods = c('adjbox','zscore','iqr'),
                                     bootSettings = list(run = TRUE, maxrecords = 50, nb = 10),
                                     pc = list(exec = TRUE, npc = 3, q = TRUE)))
          })

test_that(desc = "Run bootstrapping only",
          code = {
            expect_gt(multidetect(data = ttrefdata,
                                     multiple = FALSE,
                                     var = 'bio6',
                                     exclude = c('x','y'),
                                     methods = c('adjbox','zscore','iqr'),
                                     bootSettings = list(run = TRUE,
                                                         maxrecords = 120, nb = 10),
                                     pc = list(exec = FALSE, npc = 3, q = TRUE))@maxrecords, nrow(ttrefdata))

            #bootstrap will not run if the max records are less than the rows in the reference data
            expect_warning(multidetect(data = ttrefdata,
                                  multiple = FALSE,
                                  var = 'bio6',
                                  exclude = c('x','y'),
                                  methods = c('adjbox','zscore','iqr'),
                                  bootSettings = list(run = TRUE, maxrecords = 50, nb = 10),
                                  pc = list(exec = FALSE, npc = 3, q = TRUE)))
          })

pcawarn <- data.frame(
  x1 = c(1, 2, 3, 4,7,9),
  x2 = c(5, 5, 5, 5,5,5),
  x3 = c(5, 5, 5, 5,5,5)
)

test_that(desc = "PCA not computed for zero variance",
          code = {
            expect_error(multidetect(data = c,
                                       multiple = FALSE,
                                       var = 'x1',
                                       methods = c('adjbox','zscore'),
                                       pc = list(exec = TRUE, npc = 2, q = TRUE)))
            #expect error for npc >nrow of pcd
            expect_error(multidetect(data = pcawarn,
                                     multiple = FALSE,
                                     var = 'x1',
                                     methods = c('adjbox','zscore'),
                                     pc = list(exec = TRUE, npc = 4, q = TRUE)))
          })

pcd <- data.frame(
  x1 = c(1, 2, 3, 4,7,9),
  x2 = c(5, 5, 5, 5,5,5)
)

test_that(desc = "Throw a warning if only two columns are provided for multivariate data ",
          code = {
            expect_warning(multidetect(data = pcd,
                                       multiple = FALSE,
                                       var = 'x1',
                                       methods = c('adjbox','zscore'),
                                       pc = list(exec = FALSE, npc = 2, q = TRUE)))
          })

#test ecological ranges
sqcep <- refdata["Squalius cephalus"]

optdata <- data.frame(species= c("Squalius cephalus", "Abramis brama"),
                      mintemp = c(6, 1.6),maxtemp = c(8.588, 21),
                      meantemp = c(8.5, 10.4), #ecoparam
                      direction = c('greater', 'greater'))

out_df <- multidetect(data = sqcep, multiple = TRUE,
                      var = 'bio1',
                      output = 'outlier',
                      exclude = c('x','y'),
                      methods = c('zscore', 'adjbox', 'optimal'),
                      optpar = list(optdf=optdata, optspcol = 'species',
                                    mincol = "mintemp", maxcol = "maxtemp"),
                      missingness = 0.2)

test_that(desc = "value with the ecological ranges of the species",
          code = {
            expect_length(out_df@result$`Squalius cephalus`$optimal$bio1, 2)
          })

expect_length(broad_classify(category = 'uni'), 12)













