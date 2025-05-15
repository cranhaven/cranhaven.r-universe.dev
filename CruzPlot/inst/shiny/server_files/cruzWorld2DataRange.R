# Adjust longitudes of applicable data as necessary when moving to/from world2


### Non-DAS data
observe({
  req(cruz.list$ndas.df) # Use this for req since it goes back to NULL
  world2 <- cruz.map.range$world2

  # Probabaly doesn't actually need to be isolated?
  isolate({
    data.curr <- cruz.list$ndas.data

    if (world2) {
      # If world2 then convert lons to 0 to 360 range
      #   assumes there won't be any weird 0/360 overlap business

      cruz.list$ndas.data <- lapply(data.curr, function(list.curr) {
        lon <- list.curr$x
        list.curr$x <- ifelse(lon < 0, lon + 360, lon)
        list.curr
      })

    } else {
      # If not world2, convert to -180 to 180 range
      #   then see if there's any weird Pacific overlap business

      ndas.data.curr <- lapply(data.curr, function(list.curr) {
        lon <- list.curr$x
        list.curr$x <- ifelse(lon > 180, lon - 360, lon)
        list.curr
      })

      # Semi-arbitrary cutoffs to determine if transect lines are
      #   in the Pacific rather than Atlantic
      # lon.all <- lapply(ndas.data.curr, function(i) i$x)
      # if(!(all(sapply(lon.all, function(i) all(i < 0))) |
      #      all(sapply(lon.all, function(i) all(i > 0)))) &
      #    any(sapply(lon.all, function(i) any(i > 130))) &
      #    any(sapply(lon.all, function(i) any(i < -100)))
      # ) {
      #   ndas.data.curr <- lapply(data.curr, function(list.curr) {
      #     lon <- list.curr$x
      #     list.curr$x <- ifelse(lon > 0, lon - 360, lon)
      #     list.curr
      #   })
      # }

      ndas.data.curr <- lapply(ndas.data.curr, function(list.curr) {
        lon.curr <- list.curr$x

        if(!(all(sapply(lon.curr, function(i) all(i < 0)), na.rm = TRUE) |
             all(sapply(lon.curr, function(i) all(i > 0)), na.rm = TRUE)) &
           any(sapply(lon.curr, function(i) any(i > 130)), na.rm = TRUE) &
           any(sapply(lon.curr, function(i) any(i < -100)), na.rm = TRUE)
        ) {
          list.curr$x <- ifelse(lon.curr > 0, lon.curr - 360, lon.curr)
        }

        list.curr
      })

      cruz.list$ndas.data <- ndas.data.curr
    }
  })
})


### Planned transects
# Note that planned transects longs must be in range [-180 to 180] when loaded
# 'Weird Pacific overlap business' means that if map is not world2 because of map extent but
#   some longitudes are across the dateline, then transect lines will be very wrong
observe({
  req(cruz.list$planned.transects)
  world2 <- cruz.map.range$world2

  # Probabaly doesn't actually need to be isolated?
  isolate({
    data.curr <- cruz.list$planned.transects

    if (world2) {
      # If world2 then convert lons to 0 to 360 range
      #   assumes there won't be any weird 0/360 overlap business

      l1 <- data.curr$lon
      cruz.list$planned.transects$lon <- ifelse(l1 < 0, l1 + 360, l1)

    } else {
      # If not world2, convert to -180 to 180 range
      #   then see if there's any weird Pacific overlap business

      l1 <- data.curr$lon
      l1.fix <- ifelse(l1 > 180, l1 - 360, l1)

      # Semi-arbitrary cutoffs to determine if transect lines are in the Pacific rather than Atlantic
      lon.all <- c(l1.fix)
      if(!(all(lon.all < 0) | all(lon.all > 0)) &
         any(lon.all > 130) & any(lon.all < -100)
      ) {
        l1.fix <- ifelse(l1.fix > 0, l1.fix - 360, l1.fix)
      }

      cruz.list$planned.transects$lon <- l1.fix
    }
  })
})
