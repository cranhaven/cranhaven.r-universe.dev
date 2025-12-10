data("jdsdata")
data("efidata")

wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))

mdf <- match_datasets(datasets = list(jds= jdsdata, efi =efidata),
                      lats = 'lat', lons = 'lon',
                      country = 'JDS4_site_ID',
                      species = c('scientificName', 'speciesname'),
                      date=c('sampling_date','Date'))

mdfclean <- check_names(mdf, colsp = 'species', verbose = FALSE, merge = TRUE)

db <- sf::read_sf(system.file('extdata/danube.shp.zip', package = "specleanr"), quiet = TRUE)


#extract data

refdata <- pred_extract(data = mdfclean, raster = wcd,
                        lat = 'decimalLatitude', lon = 'decimalLongitude',
                        bbox = db,
                        colsp = 'speciescheck',
                        list = TRUE,
                        verbose = FALSE,
                        minpts = 6,
                        merge = FALSE)

sp <- refdata[['Salmo trutta']]

#test for adjustedbxplots

test_that(desc = "Adjusted boxplots errors and success",
                    code = {
                      #var  not in data
                      expect_error(adjustboxplots(data = sp, var = 'bio', output = "outlier"))

                      #expect clean dataframe with outliers
                      expect_s3_class(adjustboxplots(data = sp, var = 'bio6', output='outlier'),
                                      'data.frame')
                      #expect clean dataframe without outliers
                      expect_s3_class(adjustboxplots(data = sp, var = 'bio6', output='clean'),
                                      'data.frame')
                      #data not numeric
                      expect_error(adjustboxplots(data = c("A", 'B', 'C')))

                      #data not a dataframe, list, atomic or vector
                      expect_error(adjustboxplots(data = wcd))

                    })

testthat::test_that(desc = "Semi interquartile range returns a dataframe of outliers.",
                    code = {
                      expect_s3_class(semiIQR(data = sp, var = 'bio6', output='outlier'), 'data.frame')
                      expect_s3_class(semiIQR(data = sp, var = 'bio6', output='clean'), 'data.frame')
                    })


testthat::test_that(desc = "Hampel returns a dataframe of outliers.",
                    code = {

                      expect_s3_class(hampel(data = sp, var = 'bio6', output='outlier'), 'data.frame')

                      expect_s3_class(hampel(data = sp, var = 'bio6', output='clean'), 'data.frame')

                      #data not numeric
                      expect_error(hampel(data = c("A", 'B', 'C')))

                      #data not a dataframe, list, atomic or vector
                      expect_error(hampel(data = wcd))

                      # #provide var when the data is atomic, vector or list: var not required
                      #
                      # expect_message(hampel(data = sp$bio6, var = "bio6"))
                      #
                      # expect_type(hampel(data = sp$bio6, output = 'clean'), 'double')

                    })

testthat::test_that(desc = "Reverse jack knifing returns a dataframe of outliers.",
                    code = {
                      #expect a dataframe: proper set up
                      expect_s3_class(jknife(data = sp, var = 'bio6', output='outlier'), 'data.frame')
                      #clean data not outliers
                      expect_s3_class(jknife(data = sp, var = 'bio6', output='clean'), 'data.frame')
                      #soft vs robust
                      expect_s3_class(jknife(data = sp, var = 'bio6', output='clean', mode = 'robust'), 'data.frame')
                      #expect error for missing data
                      expect_error(jknife(var = 'bio6'))
                    })


testthat::test_that(desc = "Z-score returns a dataframe of outliers.",
                    code = {
                      expect_s3_class(zscore(data = sp, var = 'bio6', output='outlier'), 'data.frame')
                      #test for clean data
                      expect_s3_class(zscore(data = sp, var = 'bio6', output='clean'), 'data.frame')

                      #test for robust
                      expect_s3_class(zscore(data = sp, var = 'bio6', mode = 'robust'), 'data.frame')

                      #expect error if mode is not robust or soft
                      expect_error(zscore(data = sp, var = 'bio6', mode = 'out'))
                    })


testthat::test_that(desc = "Median rule range returns a dataframe of outliers.",
                    code = {

                      expect_s3_class(medianrule(data = sp, var = 'bio6', output='outlier'), 'data.frame')

                      expect_s3_class(medianrule(data = sp, var = 'bio6', output='clean'), 'data.frame')
                    })

testthat::test_that(desc = "Distribution boxplot returns a dataframe of outliers.",
                    code = {
                      expect_s3_class(distboxplot(data = sp, var = 'bio6', output='outlier'), 'data.frame')

                      expect_s3_class(distboxplot(data = sp, var = 'bio6', output='clean'), 'data.frame')

                      # expect_type(distboxplot(data = sp$bio6, output = 'clean'), 'double')

                      #expect double
                      #expect_type(distboxplot(data = c(4,5,5,6,7,45,67,4,3,5,6,5,44), output = 'clean'), "double")

                      #data not a dataframe, list, atomic or vector
                      expect_error(distboxplot(data = wcd))

                      #check for nd 400, 300, 200, 100
                      #expect double 400
                      #expect_type(distboxplot(data = runif(412, 9, 43), output = 'clean'), 'double')

                      # #expect double 300
                      # expect_type(distboxplot(data = runif(300, 9.5, 43.7), output = 'clean'), 'double')#
                      #
                      # #expect double 200
                      # expect_type(distboxplot(data = runif(200, 0.01, 1.6), output = 'clean'), 'double')
                      # #expect double 100
                      # expect_type(distboxplot(data = runif(100, 9, 493), output = 'clean'), 'double')

                    })

testthat::test_that(desc = "Sequential fences returns a dataframe of outliers.",
                    code = {
                      expect_s3_class(seqfences(data = sp, var = 'bio6', output='outlier'), 'data.frame')

                      expect_s3_class(seqfences(data = sp, var = 'bio6', output='clean'), 'data.frame')

                      expect_s3_class(seqfences(data = sp, var = 'bio6', output='clean', mode = 'meo'), 'data.frame')
                      expect_s3_class(seqfences(data = sp, var = 'bio6', output='clean', mode = 'leo'), 'data.frame')
                      expect_s3_class(seqfences(data = sp, var = 'bio6', output='clean', mode = 'ao'), 'data.frame')
                      expect_s3_class(seqfences(data = sp, var = 'bio6', output='clean', mode = 'wo'), 'data.frame')
                      #gamma not in the standard values
                      expect_error(seqfences(data = sp, var = 'bio6', output='outlier', gamma = 0.3))

                      # expect_type(seqfences(data = sp$bio6, output = 'clean'), 'double')

                     # "Sequential fences returns an error if the records exceeds 100 records.",
                      sprbind <- rbind(sp, sp, sp, sp)

                      testthat::expect_error(seqfences(data = sprbind, var = 'bio6', output='outlier'))

                      #expect if vectors are more than 100

                      expect_error(seqfences(data = runif(110, min = 43, max = 403)))

                      #expect error if dataframe or vector/list/atomic not provided

                      expect_error(seqfences(data = wcd))

                    })


#Preferred temperature (Ref. 123201): 6.5 - 15.8, mean 10.1 °C #FishBase

testthat::test_that('use minimum and maximum temperature to flag suspicious outlier',
          code = {
            #output outliers
            expect_s3_class(ecological_ranges(data = sp, min = 6.5, max = 15.8, var = 'bio1',
                                              output ='outlier'),'data.frame')
            #output clean data

            expect_s3_class(ecological_ranges(data = sp, min = 6.5, max = 15.8, var = 'bio1',
                                              output ='clean'),'data.frame')

            #error if max and min are not provided
           expect_error(ecological_ranges(data = sp, var = 'bio1',output ='outlier'))
          })

testthat::test_that('use only one parameter such as max temperature-(ecoparam and direction)',
          code = {
            dx <- ecological_ranges(data = sp, ecoparam = 15.8, var = 'bio1', output ='outlier',
                                    direction = 'greater')
            testthat::expect_s3_class(dx, 'data.frame')
          })

#using parameter in a dataframe
#1. dataset with optimal parameters//collated in literature
optdata <- data.frame(species= c("Salmo trutta", "Abramis brama"),
                      mintemp = c(2, 10),maxtemp = c(24, 24),
                      meantemp = c(9, NA),
                      direction = c('less', 'greater'))

testthat::test_that(desc = "optimal ranges from literature for multiple species",
                    code = {
                      expect_s3_class(ecological_ranges(data = sp, species = 'Salmo trutta',
                                                        var = 'bio1', output = "outlier",
                                                        optimumSettings = list(optdf = optdata,
                                                                               maxcol = "maxtemp",
                                                                               mincol ="mintemp",
                                                                               optspcol = "species")),
                                      "data.frame")

                      #outputclean
                      expect_s3_class(ecological_ranges(data = sp, species = 'Salmo trutta',
                                                        var = 'bio1', output = "clean",
                                                        optimumSettings = list(optdf = optdata,
                                                                               maxcol = "maxtemp",
                                                                               mincol ="mintemp",
                                                                               optspcol = "species")),
                                      "data.frame")

                      #Error if data = NULL is not provided
                      expect_error(ecological_ranges(data = NULL, species = 'Salmo trutta',
                                                        var = 'bio1', output = "outlier",
                                                        optimumSettings = list(optdf = optdata,
                                                                               maxcol = "maxtemp",
                                                                               mincol ="mintemp",
                                                                               optspcol = "species")))
                      #error expected dataframe is expected for optimum data

                      expect_error(ecological_ranges(data = sp, species = 'Salmo trutta',
                                                        var = 'bio1', output = "outlier",
                                                        optimumSettings = list(optdf = c(1, 2,3,4),
                                                                               maxcol = "maxtemp",
                                                                               mincol ="mintemp",
                                                                               optspcol = "species")))

                      #Error expected if species column is not provided from the optimal dataset
                      expect_error(ecological_ranges(data = sp, species = 'Salmo trutta',
                                                     var = 'bio1', output = "outlier",
                                                     optimumSettings = list(optdf = optdata,
                                                                            maxcol = "maxtemp",
                                                                            mincol ="mintemp",
                                                                            optspcol = NULL)))

                    })

testthat::test_that(desc = "optimal ranges from literature for multiple species but only one",
          code = {
            dx <- ecological_ranges(data = sp, species = 'Salmo trutta',
                                              var = 'bio1', output = "outlier",
                                              optimumSettings = list(optdf = optdata,
                                                                     ecoparam = "meantemp",
                                                                     direction ="direction",
                                                                     optspcol = "species"))
            expect_s3_class(dx, "data.frame")

            #expect a dataframe with even a slightly misspelt species name Salmo trutta
            dx2 <- ecological_ranges(data = sp, species = 'Salmo truttar',
                                    var = 'bio1', output = "outlier",
                                    optimumSettings = list(optdf = optdata,
                                                           ecoparam = "meantemp",
                                                           direction ="direction",
                                                           optspcol = "species"),
                                    pct = 70)
            expect_s3_class(dx, "data.frame")

            #expect a original dataframe since stated species has optimal parameters Salmojg tttrruata
            dx2 <- ecological_ranges(data = sp, species = 'Salmo truttar',
                                     var = 'bio1', output = "outlier",
                                     optimumSettings = list(optdf = optdata,
                                                            ecoparam = "meantemp",
                                                            direction ="direction",
                                                            optspcol = "species"),
                                     pct = 70)
            expect_s3_class(dx, "data.frame")

          })

#If the taxa is fish and connected on internet, the user can access both the temperature and
#geospatial ranges (latitude and longitudinal ranges)

testthat::test_that(desc = "check for temperature or georanges",
                    code = {
                      testthat::skip_if_offline()
                      testthat::skip_if_offline()
                      #provide var or variable to check, annual temperature (bio1)
                      expect_s3_class(ecological_ranges(data = sp, species = 'Salmo trutta',
                                                        var = 'bio1', output = "outlier",
                                                        checkfishbase = TRUE, mode = 'temp'), "data.frame")

                      #species which is not in fishbase//first suppress checknames warnings
                      #No temperature ranges for Salmo trutta332re from FishBase and original data will be output from clean data

                      expect_message(suppressWarnings(ecological_ranges(data = sp, species = 'Salmo trutta332re',
                                        var = 'bio1', output = "clean",
                                        checkfishbase = TRUE, mode = 'temp')))

                      #provide decimal longitude and latitude for georanges (x and y extracted at pred_extract)
                      expect_s3_class(ecological_ranges(data = sp, species = 'Salmo trutta',
                                                        lat = 'y', lon = 'x', output = "outlier",
                                                        checkfishbase = TRUE, mode = 'geo'), "data.frame")

                      #error if the lat and lon is not provided yet mode is geo
                      expect_error(ecological_ranges(data = sp, species = 'Salmo trutta',
                                                     output = "outlier",checkfishbase = TRUE, mode = 'geo'))
                    })

testthat::test_that(desc = "Interquartile range returns a dataframe of outliers.",
                    code = {
                      iqrout <- interquartile(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = iqrout, 'data.frame')
                    })


testthat::test_that(desc = "Logarthmic boxplot returns a dataframe of outliers.",
                    code = {
                      logout <- logboxplot(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = logout, 'data.frame')
                    })
testthat::test_that(desc = "Mixed interquantile range returns a dataframe of outliers.",
                    code = {
                      mxdout <- mixediqr(data = sp, var = 'bio6', output='outlier')

                      testthat::expect_s3_class(object = mxdout, 'data.frame')
                    })

#multivariate tests
testthat::test_that(desc = "Checks for isolation forest",
                    code = {
                      expect_s3_class(isoforest(data = sp, size = 0.7,  output='outlier',
                                                          exclude = c("x", "y")), 'data.frame')
                      expect_s3_class(isoforest(data = sp, size = 0.7,  output='clean',
                                                exclude = c("x", "y")), 'data.frame')
                      #expect error cutoff greater 1
                      expect_error(isoforest(data = sp, size = 0.7, cutoff = 1.2,  output='clean',
                                             exclude = c("x", "y")))
                      #expect error size greater 1
                      expect_error(isoforest(data = sp, size = 1.2,  output='clean',exclude = c("x", "y")))

                      #exclude NULL.. x and y will be included in the computation
                      expect_s3_class(isoforest(data = sp, size = 0.6,  output='clean'), 'data.frame')
                    })


testthat::test_that(desc = "Checks for one class support vector machines",
                    code = {
                      expect_s3_class(onesvm(data = sp, exclude = c("x", "y"),  output='outlier'),
                                      'data.frame')
                      expect_s3_class(onesvm(data = sp, exclude = c("x", "y"),  output='clean'),
                                      'data.frame')

                      #exclude NULL.. x and y will be included in the computation
                      expect_s3_class(onesvm(data = sp,  output='clean'), 'data.frame')

                      #tune TRUE
                      expect_s3_class(onesvm(data = sp, exclude = c("x", "y"),  output='clean', tune = TRUE),
                                      'data.frame')
                    })

testthat::test_that(desc = "Checks for Local outlier factor whether return dataframe of outliers",
                    code = {
                      expect_s3_class(xlof(data = sp, exclude = c("x", "y"),output='outlier',
                                                              metric ='manhattan', minPts = 10,
                                           mode = "soft"), 'data.frame')
                      expect_s3_class(xlof(data = sp, exclude = c("x", "y"),output='clean',
                                           metric ='manhattan', minPts = 10,
                                           mode = "soft"), 'data.frame')

                      expect_s3_class(xlof(data = sp,output='clean',
                                           metric ='manhattan', minPts = 10,
                                           mode = "soft"), 'data.frame')
                    })

testthat::test_that(desc = "Checks for k-nearest neighbours whether return dataframe of outliers",
                    code = {
                      expect_s3_class(xknn(data = sp, exclude = c("x", "y"),
                                                     output='outlier', metric ='manhattan',
                                                     mode = "soft"), 'data.frame')
                      expect_s3_class(xknn(data = sp, exclude = c("x", "y"),
                                           output='clean', metric ='manhattan',
                                           mode = "soft"), 'data.frame')
                      #exclude
                      expect_s3_class(xknn(data = sp,
                                           output='clean', metric ='manhattan',
                                           mode = "soft"), 'data.frame')
                    })


testthat::test_that(desc = "Checks for Global-Local Outlier Score from Hierarchies whether return dataframe of outliers",
                    code = {
                      expect_s3_class(xglosh(data = sp, exclude = c("x", "y"),
                                                       output='outlier', metric ='manhattan', k = 3,
                                                       mode = "soft"), 'data.frame')

                      expect_s3_class(xglosh(data = sp, exclude = c("x", "y"),
                                             output='clean', metric ='manhattan', k = 3,
                                             mode = "soft"), 'data.frame')
                      #exclude
                      expect_s3_class(xglosh(data = sp,
                                             output='outlier', metric ='manhattan', k = 3,
                                             mode = "soft"), 'data.frame')
                    })

testthat::test_that(desc = "Checks Mahalanobis distance measures whether return dataframe of outliers",
                    code = {
                      expect_s3_class(mahal(data = sp, exclude = c('x','y'), output='outlier'), 'data.frame')
                      expect_s3_class(mahal(data = sp, exclude = c('x','y'), output='clean'), 'data.frame')

                                            #data missing
                      expect_error(mahal(exclude = c('x','y'), output='clean'))

                      #set the mode to robust for Phoxinus phoxinus

                      expect_s3_class(mahal(data = refdata[['Phoxinus phoxinus']], mode = 'robust',
                                            exclude = c('x','y')), "data.frame")

                      #set the mode to robust for Salmo trutta did not exceute
                      expect_error(suppressWarnings(mahal(data = sp, mode = 'robust')))

                      #return a warning if the data has y, therefore change to vifcor not vifstep not exclude x and y

                      expect_warning(mahal(data = refdata[['Phoxinus phoxinus']], tol = 1e-25))

                    })


testthat::test_that(desc = "Checks k-means whether return dataframe of outliers",
                    code = {
                      kmeanout <- xkmeans(data = sp, output='outlier', exclude = c('x', 'y'),
                                          mode = 'soft', k=3)

                      testthat::expect_s3_class(object = kmeanout, 'data.frame')

                      #test that k less than 2 will lead to errors.
                      expect_error(xkmeans(data = sp,output='outlier', exclude = c('x', 'y'),
                                           mode = 'soft', k=1))
                    })
