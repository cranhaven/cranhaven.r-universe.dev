#' Data collected from a cape gannet track (\emph{Morus capensis}, Lichtenstein 1823),
#' breeding on Bird Island (Algo Bay, South Africa) in december 2010.
#'
#' Tracking data recorded with a GPS device (i-GotU GT-600, Mobile Action
#' Technology Inc., Taipei, Taiwan). Regular step duration at 5s.
#' Behaviour data recorded with a video camera (Camsports nano, CamsportsTM,
#' Estrablin, France). Observations of taking off, landing and diving, from wich
#' three behavioural states were infered: flying, sitting on the water, diving.
#' Recording during the first 90 min (short autonomy of camera) of the trip.
#'
#' @format Tracking data
#'
#' Columns:
#'
#' x = longitude in decimal degrees
#'
#' y = latitude in decimal degrees
#'
#' t = time in POSIXct
#'
#' b = behaviour observed on video data (3:flying, 2:sitting on water,
#' 1:diving ,-1:no data)
#'
#' id = individual id 
#'
#' @source Andréa Thiebault
#' 
"track_CAGA_005"
