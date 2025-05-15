# cruzDasSightRange for CruzPlot - step 2 of processing species data
#   cruzDasSightPosition returns das_sight with Lat/Lon columns adjusted for
#     world2 and ship/sighting position.
#     NOTE: Now done when removing records with NA positions in cruzDasSightProcess.R
#   cruzDasSightRange() returns list, which includes sightings within map range,
#     selected sighting type, species codes, and counts for each species


###############################################################################
cruzDasSightRange <- reactive({
  #----------------------------------------------------------------------------
  req(cruz.list$das.data)

  data.list <- cruzDasSightSpecies()

  das.sight    <- data.list$das.sight
  sight.type   <- data.list$sight.type
  sp.codes     <- data.list$sp.codes
  sp.selection <- data.list$sp.selection

  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range

  # Adjust longitudes if world2 map is being used
  if (cruz.map.range$world2)
    das.sight$Lon <- ifelse(das.sight$Lon < 0, das.sight$Lon + 360, das.sight$Lon)

  # NA values removed back in cruzDasSightSpeciesProcess()
  ll.na <- sum(is.na(das.sight$Lat) | is.na(das.sight$Lon))
  validate(
    need(ll.na == 0,
         "Error processing sighting positions - please report this as an issue")
  )


  #----------------------------------------------------------------------------
  # Filter to map range
  das.sight.filt <- das.sight %>%
    filter(between(.data$Lat, lat.range[1], lat.range[2]),
           between(.data$Lon, lon.range[1], lon.range[2]))
  validate(
    need(nrow(das.sight.filt) > 0,
         "No sightings are within the map boundaries for the selected sighting type/species")
  )


  #----------------------------------------------------------------------------
  list(
    das.sight = das.sight.filt, sight.type = sight.type,
    sp.codes = sp.codes, sp.selection = sp.selection
  )
})
