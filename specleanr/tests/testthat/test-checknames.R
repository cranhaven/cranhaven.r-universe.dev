
#test check names whether they return dataframe, list or vector

#species data
data("jdsdata")
data("efidata")

test_that(desc = 'A dataframe is returned',
          code = {
            skip_on_cran()
            expect_s3_class(check_names(data = jdsdata, colsp = 'speciesname',verbose = FALSE,
                              pct = 90, merge = TRUE), 'data.frame')
          })

#check if a species cleaned name is returned

test_that(desc = 'Character of clean returned',
          code = {
            skip_on_cran()
            expect_type(check_names(data = 'Salmo Trutta FARIO',verbose = FALSE, pct = 90,
                                    merge = FALSE, sn=FALSE), 'character')
            expect_type(check_names(data = 'Salmo Trutta FARIO',verbose = FALSE, pct = 90,
                                    merge = FALSE, sn=TRUE), 'character')

            expect_message(check_names(data = 'Salmo Trutta FARIO',verbose = TRUE, pct = 90,
                        merge = FALSE, sn=TRUE), "The synoynm species Salmo trutta fario maintained in the list.")

            expect_message(check_names(data = 'Anthony Basooma',verbose = TRUE, pct = 90,
                                       merge = FALSE, sn=FALSE),"No close name in Fish Base for Anthony basooma.")
          })
test_that(desc = 'Species not found in FishBase',
          code = {
            skip_on_cran()
            expect_message(check_names(data = 'Salmerj ncnd', verbose = TRUE))
          })

test_that(desc = 'Expect number changes when merge is either FALSE or TRUE/contain speciescheck)',
          code = {

            skip_on_cran()

            df2col <- check_names(data = efidata, colsp = 'scientificName', verbose = FALSE)#merge false

            expect_equal(ncol(df2col), 2)

            #expect error if the species column is not in the data

            expect_error(check_names(data = efidata, colsp = 'species', merge = TRUE, verbose = FALSE))#species not in EFI data

            #error if the species column is not provided

            expect_error(check_names(data = efidata, merge = TRUE, verbose = FALSE))

            #merged is expected on dataframes only
            expect_error(check_names(data = 'Salmo Trutta FARIO',verbose = FALSE, pct = 90, merge = TRUE, sn=FALSE))

            dfmanycol <- check_names(data = efidata, colsp = 'scientificName', merge = TRUE, verbose = FALSE)

            expect_gt(ncol(dfmanycol), 2)

            expect_contains(colnames(df2col), 'speciescheck')

            expect_contains(colnames(dfmanycol), 'speciescheck')

            skip_if_not_installed("sf")

            efisf <- efidata %>% sf::st_as_sf(coords =c("decimalLongitude", "decimalLatitude" ), crs = sf::st_crs(4326))

            expect_equal(ncol(check_names(data = efisf, colsp = 'scientificName', verbose = FALSE)), 2) #expect two columns

            #expect two columns even if a list is used
            expect_equal(ncol(check_names(data = list(sp='salmo trutta', sp2 = 'abramis brama'),
                                          verbose = FALSE, pct = 90, merge = FALSE, sn=FALSE)), 2)

            #ecosystem types
            xvv <- check_names(data = efidata, colsp = "scientificName", verbose = FALSE, pct = 90,
                              merge = TRUE, sn=FALSE, ecosystem = TRUE, rm_duplicates = TRUE)
            expect_contains(colnames(xvv), 'Fresh')

          })

