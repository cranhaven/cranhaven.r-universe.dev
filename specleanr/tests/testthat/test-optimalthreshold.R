
data(jdsdata)
data(efidata)

#Danube basin polygon

db <- sf::st_read(system.file('extdata/danube.shp.zip', package='specleanr'), quiet=TRUE)


matchdata <- match_datasets(datasets = list(jds = jdsdata, efi = efidata),
                            lats = 'lat',
                            lons = 'lon',
                            species = c('speciesname','scientificName'),
                            country= c('JDS4_site_ID'))

worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))

rdata <- pred_extract(data = matchdata,
                      raster= worldclim ,
                      lat = 'decimalLatitude',
                      lon= 'decimalLongitude',
                      colsp = 'species',
                      bbox = db,
                      minpts = 10,
                      list=TRUE,
                      merge=FALSE)



out_df <- multidetect(data = rdata, multiple = TRUE,
                      var = 'bio6',
                      output = 'outlier',
                      exclude = c('x','y'),
                      methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel'))

#extracting optimal threshold for each species

test_that(desc = "Data frame of minima and maxima for 9 species",
          code = {
            df <- optimal_threshold(refdata = rdata, outliers = out_df)

            expect_s3_class(df, "data.frame")
            expect_equal(nrow(df), 9)
          })
#test for one species

thymallus <- matchdata[matchdata$species=="Thymallus thymallus",]

rdata1 <- pred_extract(data = thymallus,
                      raster= worldclim ,
                      lat = 'decimalLatitude',
                      lon= 'decimalLongitude',
                      bbox = db,
                      colsp = 'species',
                      minpts = 10,
                      list=TRUE,
                      merge=FALSE)

#suppress warning of not enough data
toutliers1 <- suppressWarnings(multidetect(data = rdata1, multiple = FALSE,
                                           var = 'bio6',
                                           output = 'outlier',
                                           exclude = c('x','y'),
                                           methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel')))

test_that(desc = "One species optimal threshold checks.",
          code = {
            expect_null(search_threshold(data = rdata1, outliers = toutliers1, cutoff = 0.2))

          })



