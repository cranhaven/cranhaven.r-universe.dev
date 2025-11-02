#' Generating hypsometric tables
#'
#' This function accepts as input the DEM and sub-catchments boundaries and
#' pre-process this data  to extract the tables that are used to calculate the
#' hypsographic curves and integral of these sub-catchments.
#'
#' @param x An object of class SpatialPolygonsDataFrame or simple features (sf).
#' For instance, you can use RGDAL package's "readOGR()" function to create such an object from Shapefiles saved on disk.
#' Alternatively, you can use sf's st_read() function to get a simple features object from the shapefile.
#' NB: at least one column (attribute) named 'Name' should be present in the shapefile or input data frame.
#' @param y An object of class RasterLayer. You can use the raster package to read GeoTIFF and other raster formats from disk.
#' @return A list of length 2. The 1st element is a list of data frames, each representing the hypsometric tables for each sub-catchment.
#'     The 2nd element is a data frame with min & max elevation values for each sub-catchment.
#' @author Faustin Gashakamba \email{gashakamba@@gmail.com}
#' @details
#' The raster package's "crop()" and "mask()" functions are used to iteratively clip the DEM to each sub-catchment.
#' For each sub-cacthment, the elevation range is divided into 30 equidistant contours.
#' Then,  the "area()" function is used to calculate the areas between each contour.
#' @export
#' @seealso \code{\link{crop}}
#' @seealso \code{\link{mask}}
#' @seealso \code{\link{classify}}
#' @importFrom terra crop
#' @importFrom terra mask
#' @importFrom terra ext
#' @importFrom terra minmax
#' @importFrom terra vect
#' @importFrom terra classify
#'
generateHypsoTables <- function(x, y) {
  #convert vector input data to simple features (sf)
  x <- Vector_conversion(x)
  #convert raster input data to SpatRaster (terra)
  y <- raster_conversion(y)
  #test the arguments of the function
  check_arguments(x, y)
  # Load data and set the number of contours to use
  DEM <- NULL
  DEM <- y
  watersheds <- x
  nbr_classes <- 30
  #alternatively, you can request the user to supply this data
  #nbr_classes = readline()
  #nbr_classes = as.integer(nbr_classes)

  #initialize min-max data frame and the data list
  min_max <- data.frame(X1=character(), X2=double(), X3=double())
  colnames(min_max) <- c("code", "min", "max")
  data_list <- list()

  #loop and calculate the hypso of each feature
  for(i in 1:nrow(watersheds)){
    #extract the DEM of the sub-watershed
    DEM_i <- DEM %>% terra::crop(terra::ext(watersheds[i,])) %>% terra::mask(terra::vect(watersheds[i,]))
    #calculate range step
    max <- terra::minmax(DEM_i)[2]
    min <- terra::minmax(DEM_i)[1]
    range <- max - min
    step <- range/nbr_classes
    #update the min-max table
    min_max[i,] <- c(watersheds$Name[i], min, max)
    #create and update a temporary reclassification matrix
    reclass_df <- data.frame(X1=double(), X2=double(), X3=double())
    colnames(reclass_df) <- c("bottom_value", "top_value", "new_value")
    counter <- 1
    for(j in seq(from = min+step, to = max, by = step)){
      bottom_value <- j-step
      top_value <- j
      new_value <- j
      reclass_df[counter,] <- c(bottom_value, top_value, new_value)
      counter <- counter + 1
    }
    #reclassify DEM_i
    DEM_i_recl <- terra::classify(DEM_i, reclass_df, include.lowest=TRUE)
    #calculate areas per class, give it a clearer name, and add it to the data list
    table <- calc_areas(DEM_i_recl)
    data_list[[i]] <- table
    #reset the reclassification matrix
    rm(reclass_df)
  }
  return(list(min_max, data_list))
}

#' Draw Hypsometric curves and calculate hypsometric integrals
#'
#' This function takes as input the DEM and sub-catchments boundaries and calls the hypsoTables function to produce the hypsometric tables.
#' It then draws and prints out the hypsometric curves for each sub-catchment.
#' Then, it fits a function to the table of each sub-cacthment and uses it to calculate the hypsometric integral.
#' Finally, it summarizes the results in a well-formatted table and prints it out as CSV.
#' All these results are stored in a folder called "HYPSO_OUTPUT" created in the current working directory.
#' @param x An object of class SpatialPolygonsDataFrame or simple features (sf).
#' For instance, you can use RGDAL package's "readOGR()" function to create such an object from Shapefiles saved on disk.
#' Alternatively, you can use sf's st_read() function to get a simple features object from the shapefile.
#' NB: at least one column (attribute) named 'Name' should be present in the shapefile or input data frame.
#' @param y An object of class RasterLayer. You can use the raster package to read GeoTIFF and other raster formats from disk.
#' @param print_result A logical variable to decide whether the output folder will be created to contain the figures and summary table (CSV) or not.
#' @return A data frame containing the hypsometric integral for each sub-catchment along with other data such as maximum &  minimum elevation.
#' @author Faustin Gashakamba \email{gashakamba@@gmail.com}
#' @details
#' The elevation range of each sub-cacthment is divided into 30 contour intervals and the area covered by each contour interval is calculated.
#' The result is put into tables (one table for each sub-catchment).
#' This data is then used to construct the hypsometric curve through ggplot2.
#' A 3rd polynomial function is then fitted to the normalized table and PolynomF package is used to calculate the area under the hypsometric curve (its integral).
#' Finally,  the integral values for each sub-catchment are compiled into a data frame that is exported as CSV.
#' @export
#' @seealso \code{\link{lm}}
#' @importFrom stats lm
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom utils write.csv
#' @import dplyr
#' @import ggplot2
#' @import PolynomF
#' @import sjPlot
#' @examples
#' \donttest{drawHypsoCurves(watersheds, DEM)} #Draw hypsographic curves of the sub-catchments in 'watersheds'.
#'
drawHypsoCurves <- function(x, y, print_result = FALSE){
  #deal with the  R CMD check
  AREA_NORM <- ELEV_NORM <- CLASS <- AREA <- NULL
  #create the output folder in the working directory
  if(print_result == TRUE){
    dir.create("HYPSO_OUTPUT")
  }
  final_table <- list() #initialize the catchment details list

  # get minimum-maximum elevation data and hypsometric tables for each sub-catchments
  watersheds <- x
  DEM <- y
  temp <- generateHypsoTables(watersheds, DEM)
  min_max <- temp[[1]]
  data_list <- temp[[2]]
  for (i in 1:nrow(watersheds)){
    # Read in the data and preprocess it
    name <- min_max$code[i]
    minimum <- as.double(min_max$min[i])
    maximum <- as.double(min_max$max[i])
    data <- as.data.frame(data_list[[i]])
    data <- data %>% select(CLASS, AREA)
    data <- mutate(data, ELEV_NORM = (CLASS - minimum)/(maximum - minimum))
    data <- mutate(data, AREA_NORM = rev(cumsum(rev(AREA)))/sum(AREA))

    # Build the Hypsometric curve object
    g <- ggplot(data = data, aes(x = AREA_NORM * 100, y = CLASS))
    g <- g + geom_line(color = "blue", size = 2)
    g <- g + geom_point(color = "green", size = 3, shape = 18)
    g <- g + labs(title = paste(i, ". Hypsometric Curve  of Catchment ", name, sep = ""),
                  x = "% of Area above elevation h",  y = "Elevation in meters")
    g <- g + ylim(minimum, maximum)
    g <- g + xlim(0, 100)
    g <- g + theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                                   size = 2, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"),
                   panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))

    # Determine the Hypsometric Integral
    # First, let's define a polynomial equation that fits the curve
    fit = lm(ELEV_NORM ~ poly(AREA_NORM, 3), data = data)

    # Second, we display the coefficients of the fitted equation in a well-formatted table
    # If the p-values of any of them is too high or the R-squared value is too low, then a better model will need to be fitted
    if(print_result == TRUE){
      print(tab_model(fit,
                    title = paste("Coefficients for ", name, "</p>", sep = ""),
                    file = paste("HYPSO_OUTPUT/", i, "_Coefficients.html", sep = "")))
    }

    # Finally, we build the polynomial equation and calculate the integral
    # in order to have integrals that are less than 1, the intercept is ignored.
    x <- polynom()
    p = summary(fit)$coefficients[2,1] * x + summary(fit)$coefficients[3,1] * x^2 + summary(fit)$coefficients[4,1] * x^3
    HI <- integral(p, c(1,0))

    # Output the results
    g <- g + annotate("text", y=minimum + 15, x= 15, label = paste("HI = ", round(HI,3), sep = ""), color="blue", fontface="bold")
    print(g)
    if(print_result == TRUE){
      png(paste("HYPSO_OUTPUT/", i, ".png", sep = ""))
      print(g)
      dev.off()
    }

    #Update the main data list
    catchment_details <- list("name" = name, "minimum" = minimum, "maximum" = maximum, "data" = data, "equation" = p, "h_integral" = HI)
    final_table[[i]] <- catchment_details
  }

  # Generate the summary data
  summary_table <- data.frame(x1 = character(),
                              x2 = numeric(),
                              x3 = numeric(),
                              x4 = numeric(),
                              x5 = numeric(),
                              stringsAsFactors = FALSE)
  colnames(summary_table)<-c("SUB_CATCHMENT_CODE","MIN_ELEV", "MAX_ELEV", "TOTAL_AREA", "H_INTEGRAL")
  for (i in 1:nrow(watersheds)){
    SUB_CATCHMENT_CODE <- final_table[[i]]$name
    MIN_ELEV <- final_table[[i]]$minimum
    MAX_ELEV <- final_table[[i]]$maximum
    TOTAL_AREA <- round(sum(final_table[[i]]$data$AREA),2)
    H_INTEGRAL <- round(final_table[[i]]$h_integral,3)
    summary_table[i,] <- c(SUB_CATCHMENT_CODE, MIN_ELEV, MAX_ELEV, TOTAL_AREA, H_INTEGRAL)
  }
  # Produce a summary graph
  if(print_result == TRUE){
    g <- ggplot(summary_table, aes(x = as.numeric(H_INTEGRAL)))
    g <- g + xlim(0, 1)
    g <- g + geom_density(alpha=.2, fill="#FF6666")
    g <- g + geom_histogram(binwidth = .01, color="darkblue", fill="lightblue")
    g <- g + scale_y_continuous(expand = c(0,0), limits = c(0,8))
    g <- g + labs(title = "Distribution of hypsometric integrals of the sub-catchments", x = "Hypsometric integral", y = "Count of sub-catchments")
    g <- g + geom_vline(aes(xintercept=mean(as.numeric(H_INTEGRAL))),color="green", size = 2) #mean vertical line
    g <- g + geom_vline(aes(xintercept=0.4),color="blue", size = 0.2) #separate mature and equilibrium stages
    g <- g + geom_vline(aes(xintercept=0.6),color="blue", size = 0.2) #separate equilibrium and young stages
    g <- g + theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1", size = 2, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))
    #saving_plot
    png("HYPSO_OUTPUT/Summary_plot.png")
    print(g)
    dev.off()
    #Print the summary table and export it to CSV
    write.csv(summary_table, file = "HYPSO_OUTPUT/summary_table.csv")
  }
  return(summary_table)
}
