
data("jdsdata")
data("efidata")


test_that(desc = 'Succefully match and bound the datasets',
          code = {
            matchdf <- match_datasets(datasets = list(jds = jdsdata, efi=efidata),
                           lats = 'lat',
                           lons = 'lon',
                           species = c('speciesname','scientificName'),
                           date = c('Date', 'sampling_date'),
                           country = c('JDS4_site_ID'))
            expect_equal(nrow(matchdf), sum(nrow(jdsdata), nrow(efidata)))
            expect_gt(ncol(matchdf), 5)
          })


test_that(desc = 'Standard column for country is left NULL',
          code = {
            #country left out yet important
            expect_error(match_datasets(datasets = list(jds = jdsdata, efi = efidata),
                                  lats = 'lat',
                                  lons = 'lon',
                                  species = c('speciesname','scientificName'),
                                  date = c('Date', 'sampling_date')))#country left
          })

test_that(desc = 'Datasets not provided in a list with names',
          code = {expect_error(match_datasets(datasets = c(jds = jdsdata, efi = efidata),#in vector not list
                                        lats = 'lat', lons = 'lon',
                                        species = c('speciesname','scientificName'),
                                        date = c('Date', 'sampling_date'),
                                        country = c('JDS4_site_ID'))) })

test_that(desc = 'The names provided are not in the merged data',
          code = {expect_error( match_datasets(datasets = list(jdsdata, efidata),
                             lats = 'lat1',#not found in any df
                             lons = 'lon1',
                             species = c('speciesname','scientificName'),
                             date = c('Date', 'sampling_date'),
                             country = c('JDS4_site_ID')))})

test_that(desc = 'Dataset lists not provided or empty list',
          code = {
            expect_error(match_datasets(lats = 'lat',lons = 'lon',
                                              species = c('speciesname','scientificName'),
                                              date = c('Date', 'sampling_date'),
                                              country = c('JDS4_site_ID')))
            expect_error(match_datasets(datasets = list(jds= NULL, efi = NULL),
                                        lats = 'lat',lons = 'lon',
                                        species = c('speciesname','scientificName'),
                                        date = c('Date', 'sampling_date'),
                                        country = c('JDS4_site_ID')))})

test_that(desc = "Latitudes,  Longitudes, country, dates, species are wrong for JDSdata",
          code={
            expect_error(match_datasets(datasets = list(jds = jdsdata, efi=efidata),
                                      lats = 'latitude',#instead of lat
                                      lons = 'lon',
                                      species = c('speciesname','scientificName'),
                                      date = c('Date', 'sampling_date'),
                                      country = c('JDS4_site_ID')))
            #Longitude
            expect_error(match_datasets(datasets = list(jds = jdsdata, efi=efidata),
                                        lats = 'lat',
                                        lons = 'longitude',#instead of lon
                                        species = c('speciesname','scientificName'),
                                        date = c('Date', 'sampling_date'),
                                        country = c('JDS4_site_ID')))
            #country
            expect_error(match_datasets(datasets = list(jds = jdsdata, efi=efidata),
                                        lats = 'lat',
                                        lons = 'lon',
                                        species = c('speciesname','scientificName'),
                                        date = c('Date', 'sampling_date'),
                                        country = c('JDS4_site')))#'JDS4_site_ID'
            #species
            expect_error(match_datasets(datasets = list(jds = jdsdata, efi=efidata),
                                        lats = 'lat',
                                        lons = 'lon',
                                        species = c('spec','scientificName'),#speciesname
                                        date = c('Date', 'sampling_date'),
                                        country = c('JDS4_site_ID')))

            #sampling date wrong
            expect_error(match_datasets(datasets = list(jds = jdsdata, efi=efidata),
                                        lats = 'lat',
                                        lons = 'lon',
                                        species = c('spec','scientificName'),
                                        date = c('Da', 'samplingate'),#sampling_date
                                        country = c('JDS4_site_ID')))
          })

