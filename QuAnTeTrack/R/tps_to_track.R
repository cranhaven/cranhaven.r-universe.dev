#' Transform a *.tps file into a \code{track} R object
#'
#' \code{tps_to_track()} reads a *.tps file containing footprint coordinates of one or several tracks and transforms it into a \code{track} R object.
#'
#' @param file A *.tps file containing (x,y) coordinates of footprints in tracks.
#' @param scale A numeric value specifying the scale in meters per pixel.
#' @param missing A logical value indicating whether there are missing footprints in any track to be interpolated: \code{TRUE}, or \code{FALSE} (the default).
#' @param NAs A matrix with two columns indicating which missing footprints will be interpolated.
#'           The first column gives the number of the track containing missing footprints, and the second column gives the number of the footprint that is missing within this track.
#'           The number of rows is equal to the total number of missing footprints in the sample.
#' @param R.L.side A character vector specifying the side of the first footprint of each track.
#'           Only needed if \code{missing} is set to \code{TRUE}.
#'           The length of the vector must be equal to the total number of tracks in the sample.
#'    * \code{"L"}: first footprint corresponds to the left foot.
#'    * \code{"R"}: first footprint corresponds to the right foot.
#'
#' @details It is highly recommended that the *.tps file is built using the TPS software (Rohlf 2008, 2009).
#'          Tracks with a different number of footprints (i.e., landmarks) are allowed.
#'          This function transforms the coordinates of the footprints of each track into a set of trajectory coordinates.
#'          Each point of the trajectory is calculated as: \deqn{Point_i(x,y)= (Footprint_i(x,y) + Footprint_{i+1}(x,y)/2}
#'
#' @details The number of points of the resulting trajectory is \eqn{n_{footprints} - 1}.
#'
#' @details If \code{missing} is set to \code{TRUE}, missing footprints can be interpolated.
#'          This interpolation is based on adjacent footprints and the provided side information.
#'
#' @return A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#'
#' @section Logo:
#' \if{html}{\figure{Logo.png}{options: width=30\%}}
#'
#' @author Humberto G. Ferrón
#' @author humberto.ferron@uv.es
#' @author Macroevolution and Functional Morphology Research Group (www.macrofun.es)
#' @author Cavanilles Institute of Biodiversity and Evolutionary Biology
#' @author Calle Catedrático José Beltrán Martínez, nº 2
#' @author 46980 Paterna - Valencia - Spain
#' @author Phone: +34 (9635) 44477
#'
#' @references
#' Farlow, J. O., O’Brien, M., Kuban, G. J., Dattilo, B. F., Bates, K. T., Falkingham, P. L., & Piñuela, L. (2012). Dinosaur Tracksites of the Paluxy River Valley (Glen Rose Formation, Lower Cretaceous), Dinosaur Valley State Park, Somervell County, Texas. In Proceedings of the V International Symposium about Dinosaur Palaeontology and their Environment (pp. 41-69). Burgos: Salas de los Infantes.
#'
#' Ostrom, J. H. (1972). Were some dinosaurs gregarious?. Palaeogeography, Palaeoclimatology, Palaeoecology, 11(4), 287-301.
#'
#' Rohlf, F. J. 2008. TPSUTIL. Version 1.40. Department of Ecology and Evolution, State University of New York.
#'          Available at <https://sbmorphometrics.org/>.
#'
#' Rohlf, F. J. 2009. tpsDig. Version 2.14. Department of Ecology and Evolution, State University of New York.
#'          Available at <https://sbmorphometrics.org/>.
#'
#' @examples
#' # Example 1: Tracks without missing footprints. Based on the Paluxy River
#' # dinosaur chase sequence (Farlow et al., 2011).
#'
#' # Load the example TPS file provided in the QuAnTeTrack package.
#' # This TPS data includes footprint coordinates for different tracks,
#' # along with associated metadata.
#' tpsPaluxyRiver <- system.file("extdata", "PaluxyRiver.tps", package = "QuAnTeTrack")
#'
#' # Call the tps_to_track function to convert the TPS data in the file
#' # into a track object. The 'scale' argument sets the scaling factor
#' # for the coordinates, and 'missing=FALSE' indicates that no landmarks
#' # are missing in the dataset.
#' tps_to_track(tpsPaluxyRiver, scale = 0.004341493, missing = FALSE, NAs = NULL)
#'
#'
#' # Example 2: Tracks with missing footprints. Based on dinosaur tracks from
#' # the Mount Tom (Ostrom, 1972).
#'
#' # Load the example TPS file provided in the QuAnTeTrack package.
#' # This TPS data includes footprint coordinates for different tracks,
#' # along with associated metadata.
#' tpsMountTom <- system.file("extdata", "MountTom.tps", package = "QuAnTeTrack")
#'
#' # Define a matrix representing the footprints that are missing from the dataset.
#' # In this example, the matrix 'NAs' specifies that footprint 7 is missing in track 3.
#' NAs <- matrix(c(7, 3), nrow = 1, ncol = 2)
#'
#' # Call the tps_to_track function, which will convert the TPS data in the file
#' # to a track object. The 'scale' argument sets the scaling factor for the coordinates,
#' # 'missing' specifies whether missing footprints should be handled, 'NAs' provides
#' # the missing footprints matrix, and 'R.L.side' specifies which side (Right or Left)
#' # is the first footprint of each track.
#' tps_to_track(tpsMountTom,
#'   scale = 0.004411765, missing = TRUE, NAs = NAs,
#'   R.L.side = c(
#'     "R", "L", "L", "L", "R", "L", "R", "R", "L", "L", "L",
#'     "L", "L", "R", "R", "L", "R", "R", "L", "R", "R",
#'     "R", "R"
#'   )
#' )
#'
#' @importFrom berryFunctions insertRows
#' @importFrom dplyr bind_rows
#' @importFrom geomorph digit.curves
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggrepel geom_label_repel
#' @importFrom gridExtra grid.arrange
#' @importFrom graphics legend
#' @importFrom grDevices chull
#' @importFrom grDevices colors
#' @importFrom stats cor
#' @importFrom stringr str_pad
#' @importFrom trajr TrajFromCoords
#' @importFrom utils read.table
#'
#' @export


tps_to_track <- function(file, scale = NULL, missing = FALSE, NAs = NULL, R.L.side = NULL) {

  ## Errors and Warnings----

  # Check if the 'scale' argument is provided
  if (is.null(scale)) {
    stop("The 'scale' argument is missing. Please provide the scale in pixels per meter.")
  }

  # Validate 'scale': must be a positive numeric value
  if (!is.numeric(scale) || length(scale) != 1 || scale <= 0) {
    stop("The 'scale' argument must be a single positive numeric value.")
  }

  # Warn if 'NAs' is provided but 'missing' is set to FALSE
  if (!missing && !is.null(NAs)) {
    warning("The 'NAs' argument will be ignored because 'missing' is set to FALSE.")
  }

  # Validate 'NAs': must be provided if 'missing' is TRUE
  if (missing && is.null(NAs)) {
    stop("The 'NAs' argument must be provided if 'missing' is set to TRUE.")
  }

  # Validate 'R.L.side': must be provided if 'missing' is TRUE
  if (missing && is.null(R.L.side)) {
    stop("The 'R.L.side' argument must be provided if 'missing' is set to TRUE.")
  }

  # Validate 'missing': must be a single logical value
  if (!is.logical(missing) || length(missing) != 1) {
    stop("The 'missing' argument must be a single logical value: TRUE or FALSE.")
  }

  # Validate 'NAs': must be a matrix with two columns containing positive integers
  if (!is.null(NAs)) {
    if (!is.matrix(NAs) || ncol(NAs) != 2) {
      stop("The 'NAs' argument must be a matrix with two columns.")
    }
    if (any(NAs <= 0)) {
      stop("The 'NAs' matrix must contain positive integers.")
    }
  }

  # Validate 'R.L.side': must contain only 'R' or 'L' values
  if (!is.null(R.L.side)) {
    if (!all(R.L.side %in% c("R", "L"))) {
      stop("The 'R.L.side' vector must contain only 'R' or 'L' values.")
    }
  }

  ## Code----

  # Read the lines from the file
  a <- readLines(file)

  # Identify lines containing landmarks (LM) and IDs
  LM <- grep("LM", a)
  ID.ind <- grep("ID", a)

  # Extract image names from the lines preceding the ID lines
  images <- basename(gsub("(IMAGE=)(.*)", "\\2", a[ID.ind - 1]))

  # Extract the number of rows for each landmark set
  skip <- LM
  nrows <- as.numeric(gsub("(LM=)([0-9])", "\\2", grep("LM", a, value = T)))
  l <- length(LM)

  # Initialize a list to store landmark data frames
  landmarks <- vector("list", l)

  for (i in 1:l) {
    # Read the landmark data into a data frame
    landmarks[i] <- list(data.frame(
      read.table(
        file = file, header = F, skip = LM[i],
        nrows = nrows[i], col.names = c("X", "Y")
      ),
      IMAGE = images[i],
      ID = read.table(
        file = file, header = F, skip = ID.ind[i] - 1,
        nrows = 1, sep = "=", col.names = "ID"
      )[2, ]
    ))
  }

  # Create a data frame from the landmarks list
  data_frame <- landmarks

  if (missing == TRUE) {
    ## Inferring missing footprints

    ### Including NAs
    # Levels and corresponding numbers from the NAs matrix
    levels <- levels(as.factor(NAs[, 1]))
    levelsnum <- as.numeric(levels)

    # Include missing footprints in the data frame
    data_frame <- landmarks

    for (i in levelsnum) {
      data_frame[[i]] <- berryFunctions::insertRows(landmarks[[i]], c(NAs[which(NAs[, 1] == i), 2]), new = NA)
    }

    # Assign the correct IMAGE and ID to missing footprints
    for (i in levelsnum) {
      data_frame[[i]][c(NAs[which(NAs[, 1] == i), 2]), ]$IMAGE <- levels(as.factor(data_frame[[i]]$IMAGE))
    }

    for (i in levelsnum) {
      data_frame[[i]][c(NAs[which(NAs[, 1] == i), 2]), ]$ID <- levels(as.factor(data_frame[[i]]$ID))
    }

    ### Calculating track width
    vectorwidth <- c()
    meanwidth <- c()

    for (j in 1:length(data_frame)) {
      vectorwidth <- c()

      # Calculate the width of the track using the area of triangles
      for (i in 1:(length(data_frame[[j]][, 1]) - 2)) {
        df <- data_frame[[j]][i:(i + 2), 1:2]

        x1 <- df[1, 1]
        x2 <- df[2, 1]
        x3 <- df[3, 1]
        y1 <- df[1, 2]
        y2 <- df[2, 2]
        y3 <- df[3, 2]

        Area <- 0.5 * (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
        Base <- dist(df[c(1, 3), c(1, 2)], method = "euclidean")
        Height <- abs((Area * 2) / Base)

        vectorwidth[i] <- Height
      }

      # Compute the mean width of the track
      meanwidth[j] <- mean(vectorwidth[which(!is.na(vectorwidth))])
    }

    ### Extrapolating missing footprints
    for (i in 1:length(data_frame)) {
      vec <- c(1:length(data_frame[[i]][, 1]))
      vec2 <- c(1:length(data_frame[[i]][, 1]))

      # Assign "R" or "L" based on the R.L.side vector
      if (R.L.side[i] == "L") {
        vec2[which((vec %% 2) != 0)] <- rep("L", length(which((vec %% 2) != 0)))
        vec2[which((vec %% 2) == 0)] <- rep("R", length(which((vec %% 2) == 0)))
      }

      if (R.L.side[i] == "R") {
        vec2[which((vec %% 2) != 0)] <- rep("R", length(which((vec %% 2) != 0)))
        vec2[which((vec %% 2) == 0)] <- rep("L", length(which((vec %% 2) == 0)))
      }

      data_frame[[i]]$Side <- vec2
    }

    # Extrapolate missing footprints based on calculated track width
    for (j in 1:length(data_frame)) {
      for (i in 1:length(NAs[, 1])) {
        x1 <- data_frame[[NAs[i, 1]]][NAs[i, 2] - 1, 1]
        y1 <- data_frame[[NAs[i, 1]]][NAs[i, 2] - 1, 2]
        x2 <- data_frame[[NAs[i, 1]]][NAs[i, 2] + 1, 1]
        y2 <- data_frame[[NAs[i, 1]]][NAs[i, 2] + 1, 2]

        # Calculate distance based on side (R or L)
        if (data_frame[[NAs[i, 1]]][NAs[i, 2], 5] == "R") {
          dist <- -1 * meanwidth[j]
        }
        if (data_frame[[NAs[i, 1]]][NAs[i, 2], 5] == "L") {
          dist <- meanwidth[j]
        }

        x3 <- (x1 + x2) / 2
        y3 <- (y1 + y2) / 2

        b <- x2 - x1
        a <- y1 - y2

        norm <- sqrt(a * a + b * b)
        a <- a / norm
        b <- b / norm

        x4 <- x3 + a * dist
        y4 <- y3 + b * dist

        data_frame[[NAs[i, 1]]][NAs[i, 2], 1] <- x4
        data_frame[[NAs[i, 1]]][NAs[i, 2], 2] <- y4
      }
    }
  }

  ### Tracing medial tracks

  # Create medial tracks by averaging consecutive landmarks
  landmarks2 <- data_frame

  landmarks3 <- list()
  for (i in 1:length(landmarks2)) {
    landmarks3[[i]] <- as.data.frame(matrix(ncol = ncol(landmarks2[[i]]), nrow = (nrow(landmarks2[[i]]) - 1)))
    colnames(landmarks3[[i]]) <- colnames(landmarks2[[i]])

    landmarks3[[i]][, 3] <- rep(landmarks2[[i]][1, 3], length(landmarks3[[i]][, 3]))
    landmarks3[[i]][, 4] <- rep(landmarks2[[i]][1, 4], length(landmarks3[[i]][, 4]))
    landmarks3[[i]] <- landmarks3[[i]][, 1:4]

    # Compute the average position for each consecutive pair of landmarks
    for (j in 1:length(landmarks3[[i]][, 1])) {
      landmarks3[[i]][j, 1] <- (landmarks2[[i]][j, 1] + landmarks2[[i]][j + 1, 1]) / 2
    }

    for (j in 1:length(landmarks3[[i]][, 2])) {
      landmarks3[[i]][j, 2] <- (landmarks2[[i]][j, 2] + landmarks2[[i]][j + 1, 2]) / 2
    }
  }

  # Scale the coordinates and create trajectories from the coordinates
  for (i in 1:length(landmarks3)) {
    landmarks3[[i]][, 1:2] <- landmarks3[[i]][, 1:2] * scale
    landmarks3[[i]] <- TrajFromCoords(landmarks3[[i]])
  }
  names(landmarks3) <- paste0("Track_", str_pad(1:length(LM), nchar(length(LM)), pad = "0"), sep = "")

  # Mark missing footprints
  for (i in 1:length(data_frame)) {
    data_frame[[i]]$missing <- rep("Actual", length(data_frame[[i]][, 1]))
  }

  if (missing == TRUE) {
    levels <- levels(as.factor(NAs[, 1]))
    levelsnum <- as.numeric(levels)
    for (i in levelsnum) {
      data_frame[[i]][c(NAs[which(NAs[, 1] == i), 2]), ]$missing <- rep("Inferred", length(which(NAs[, 1] == i)))
    }
  }

  # Scale the X and Y coordinates
  for (i in 1:length(data_frame)) {
    data_frame[[i]]$X <- data_frame[[i]]$X * scale
    data_frame[[i]]$Y <- data_frame[[i]]$Y * scale
  }

  # Create a named list to return the results
  listdata <- list(
    Trajectories = landmarks3,
    Footprints = data_frame
  )

  return(listdata)
}
