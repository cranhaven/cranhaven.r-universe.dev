#' Get Object Trajectory from Image Sequence
#'
#' This function calculates the trajectory of an object in a sequence of images. It compares each frame to a background 
#' image to detect movement. The function identifies the largest object in each frame and calculates its centroid coordinates 
#' across the sequence.
#'
#' @param listImages A vector of file paths to the images in the sequence.
#' @param Barnes A parameter used by the `removeBackground()` function to remove background noise.
#' @param iBackground An index indicating which image from the sequence is used as the background.
#' @param iBegin An index specifying the first image in the sequence to start tracking.
#' @param iEnd An index specifying the last image in the sequence to track.
#'
#' @return An object of class `trayectoria` containing the points (coordinates) of the object in each frame.
#'         The `trayectoria` object will include the `x` and `y` coordinates. If no object is detected in a frame,
#'         the corresponding coordinates will be set to `NA` in the `trayectoria` object.
#'
#' @details The function reads the images from `listImages` and compares each frame to the background image specified by `iBackground`.
#' Background subtraction is performed using the `removeBackground()` function, followed by thresholding to identify significant
#' changes between the background and the current frame. The largest connected component in the thresholded image is assumed
#' to be the object of interest, and its centroid is calculated.
#' 
#' Instead of directly returning the centroid coordinates, the function now creates an object of class `trayectoria` to store the 
#' calculated centroids. The coordinates of the object in each frame are added to the `trayectoria` object. If no object is detected 
#' in a frame, the corresponding coordinates are set to `NA`. The resulting `trayectoria` object is returned, which allows users to 
#' further manipulate and analyze the trajectory of the object across frames.
#' 
#' The returned `trayectoria` object contains methods for extracting and analyzing the coordinates, making it easier to track the 
#' object over time.

#'
#' @examples
#' \donttest{
#' # Not run:
#' path <- system.file('extdata/frames', package='BioTrajectory')
#' images <- list.files(path, full.names = TRUE)
#' B  <- list(c1 = structure(list(x = 342L, y = 263L), row.names = 1L, class = "data.frame"),
#'     boardRadius = 207, c2 = structure(list(x = c(157L, 172L, 202L, 245L,
#'     297L, 352L, 408L, 455L, 494L, 517L, 522L, 507L, 476L, 430L,
#'     375L, 318L, 262L, 215L, 180L, 160L), y = c(242L, 188L, 141L,
#'     105L, 85L, 80L, 93L, 124L, 166L, 219L, 277L, 334L, 383L,
#'     420L, 440L, 442L, 426L, 394L, 350L, 298L), class = "data.frame")), holeRadius = 13)
#' 
#' trajectory <- getTrajectory(images, B, 1, 1, 2)
#' print(trajectory) 
#' }
#'
#' @export

getTrajectory <- function(listImages, Barnes, iBackground, iBegin, iEnd) {
  background <- readImage(listImages[iBackground], 640)
  background <- resize(background, 640)
  background <- removeBackground(background, Barnes)
  coords <- c(NA, NA)

  for (i in (iBegin:iEnd)) {
    frame <- readImage(listImages[i], 640)
    frame <- removeBackground(frame, Barnes)
    difference <- background - frame
    diffT <- threshold(difference, 0.2)  
    labels <- label(diffT)
    areas <- sapply(1:max(labels), function(label) sum(labels == label))
    largest_object <- which.max(areas)
    coordsL <- which(labels == largest_object, arr.ind = TRUE)

    if (nrow(coordsL) > 0) {
      x_centroid <- sum(coordsL[, 1]) / nrow(coordsL)
      y_centroid <- sum(coordsL[, 2]) / nrow(coordsL)
      coords <- rbind(coords, c(x_centroid, y_centroid))
    } else {
      coords <- rbind(coords, c(NA, NA))
    }
  }

  coords <- coords[-1, ]
  coords <- data.frame(coords)
  coords<- na.omit(coords)
  traj <- trajectory(x=coords[,1],y=coords[,2])
  return(traj)
}

