#' Reads tracking data from a specified file.
#'
#' Reads a text file containing tracking data, where each line represents 
#' coordinates. If a line contains "null", it adds NA values for that entry. Optionally, 
#' it can remove rows with NA values.
#'
#' @param file A character string specifying the path to the file containing the tracking data.
#' @param na.rm A logical value indicating whether to remove rows with NA values (default is FALSE).
#'
#' @return An object of class `trayectoria` containing the points (coordinates) of the object in each frame.
#'         The `trayectoria` object will include the `x` and `y` coordinates. If no object is detected in a frame,
#'         the corresponding coordinates will be set to `NA` in the `trayectoria` object.
#'
#' @examples
#' # Read tracking data from a file
#' path <- system.file('extdata/track.txt', package='BioTrajectory')
#' tracking_data <- readtrackData(path, na.rm = TRUE)
#' # Print the resulting data frame
#' print(tracking_data)
#'
#' @export

readtrackData <- function(file, na.rm = FALSE) {
  lines <- readLines(file)
  data <- data.frame(x = NA, y = NA)
  for (i in seq_along(lines)) {
    linea <- lines[i]
    if (linea == "null") {
      data <- rbind(data, c(NA, NA))
    } else {
      coords <- as.numeric(unlist(strsplit(linea, " ")))
      data <- rbind(data, coords)
    }
  }
  data <- data[-1, ]
  if (na.rm) {
    data <- na.omit(data)
    rownames(data) <- 1:nrow(data)
  }
  traj <- trajectory(x=data[,1],y=data[,2])
  return(traj)
}
