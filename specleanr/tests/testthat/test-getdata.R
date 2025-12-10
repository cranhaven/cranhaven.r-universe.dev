
test_that(desc = 'Return species data',
          code = {

            skip_on_cran()
            skip_if_offline()

            ondata <- getdata(data = 'Gymnocephalus baloni', gbiflim = 10, vertlim = 10,
                              inatlim = 10, verbose = FALSE)
            expect_s3_class(ondata, 'data.frame')

            expect_lt(nrow(ondata), 30)

            expect_contains(colnames(ondata), c('country', 'species', 'dates',
                                                'decimalLatitude','decimalLongitude'))

          })
test_that(desc = "Expect error when data is not provided",
          code = {
            skip_on_cran()
            skip_if_offline()
            expect_error(getdata(gbiflim = 10, vertlim = 10,inatlim = 10, verbose = FALSE))
          })

datasp <- data.frame(river= c("Danube"), species=c("Gymnocephalus baloni"))

test_that(desc = "Dataframe with species column provided. if not an error is returned",
          code = {
            skip_on_cran()
            expect_error(getdata(data = datasp, vertlim = 10,inatlim = 10, verbose = FALSE ))
          })
