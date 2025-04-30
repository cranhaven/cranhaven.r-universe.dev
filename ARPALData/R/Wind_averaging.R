#' @keywords internal
#' @noRd

Wind_averaging <- function(ws,wd) {
  # Calculate the u and v wind components
  u_wind <- - ws * sin(2 * pi * wd/360)
  v_wind <- - ws * cos(2 * pi * wd/360)
  # Calculate the average wind vectors
  mean_u <- mean(u_wind, na.rm = T)
  mean_v <- mean(v_wind, na.rm = T)
  # Calculate the resultant vector average wind direction with atan2
  wd_average <- (atan2(mean_u, mean_v) * 360/2/pi)
  wd_average <- ifelse(wd_average > 180, wd_average - 180, wd_average + 180)
  # Calculate the vector average wind speed
  ws_average <- ((mean_u^2 + mean_v^2)^0.5)
  ### Output
  return(list(Wind_speed = ws_average, Wind_direction = wd_average))
}
