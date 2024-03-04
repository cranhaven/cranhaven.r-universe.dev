#' Create a spherical representation
#' 
#' From the segmentation features simulate data and create a spherical representation
#' 
#' @param segmentationCSV The file path to the csv file created from the segmentation process containing all features.
#' @param plotRows The rows from the csv file to be used to simulate plotting data.
#' @param levels breakpoints for plotting density.
#' @param singlePlot (logical) Indicates wether all rows should be added to one plot.
#' @param col A vector of colours.
#' @param alpha The range of alpha values to be used for plotting colours
#' @param arrow (logical) Indicates whether an arrow for directionality should be added to the plot.
#' @param nsims The number of simulated values to plot. (Default = 1000 however, if your computer has little RAM reduce this)
#' @details This function takes the features from the segmentation procedure and uses them to simulate data for
#' elevation and rotation. This data is then rotated to give the spherical representation which is ploted on the sphere.
#' Required columns are:\enumerate{
#'     \item UpDown.median
#'     \item UpDown.mad
#'     \item Degrees.median
#'     \item Degrees.mad
#' }
#' @return There is no return to the console. As a side effect an rgl graphic is created.
#' 
#' @export 
#' 
#' @examples 
#' \dontrun{
#' segmentationCSV = system.file("data", "SegData.csv", package = "GENEAsphere")
#' plotRows = c(1:1)
#' plotSegmentSphere(segmentationCSV, plotRows, levels = c(0.9, 0.75, 0.5, 0.25, 0.1),
#'                   singlePlot = TRUE, col = heat.colors(5),
#'                   alpha = c(0.03, 0.05, 0.1, 0.2, 0.3), arrow = FALSE, nsims = 1000)}

plotSegmentSphere <- function(segmentationCSV, plotRows, levels = c(0.9, 0.75, 0.5, 0.25, 0.1), singlePlot = TRUE, col = heat.colors(5), 
		alpha = c(0.03, 0.05, 0.1, 0.2, 0.3), arrow = FALSE, nsims = 1000){
	
	segmentationData <- read.csv(segmentationCSV)
	
	simData <- lapply(plotRows, function(row, data){
				upDownTheta <- c(segmentationData[row, "UpDown.median"], segmentationData[row, "UpDown.mad"])*pi/180
				degreeTheta <- c(segmentationData[row, "Degrees.median"], segmentationData[row, "Degrees.mad"])*pi/180
				
				upDownSim <- rnorm(nsims, upDownTheta[1], degreeTheta[2])
				degreeSim <- rnorm(nsims, degreeTheta[1], degreeTheta[2])
				
				xyzMatrix <- matrix(0, nrow = nsims, ncol =3)
				for(i in seq_len(nsims)){
					xyzMatrix[i, ] <- c(cos(upDownSim[i])*cos(degreeSim[i]), sin(upDownSim[i]), -sin(degreeSim[i]) )
				}
				xyzMatrix
			}, data = segmentationData)
	
	for(dataSet in seq_along(simData)){
		
		tempData <- simData[[dataSet]]

		d <- kde3d(tempData[,1], tempData[,2], tempData[,3],n = 50)
		dtmp = sort(d$d)
		
		levels = 1-levels
		lev <- sapply(levels, function(l, dtmp){
					which.max((cumsum(dtmp)/ sum(dtmp)) > l)
				}, dtmp = dtmp)
		lev = dtmp[lev]
		
		if(!singlePlot){
			open3d()
		}
		
		contour3d(d$d, lev,  d$x, d$y, d$z, color = col , alpha = alpha, scale = FALSE, add = singlePlot)
		
		if(dataSet == 1 | singlePlot == FALSE){
			aspect3d("iso")
			spheres3d(0,0,0,1, alpha = 0.5, front="line", back = "line")
			grid3d(side = c("x","y","z"),at = c(0,0,0))
			
			if(arrow){
				triangles3d(c(-1, 0, 1)/2, c(0,1,0)/2, c(0,0,0)/2, coords = c(1,3,2), back="cull", front = "lines", col=1, alpha = 1)
				triangles3d(c(-1, 0, 1)/2, c(0,1,0)/2, c(0,0,0)/2, front="cull", back = "lines", col="blue", alpha=1)
				quads3d( c(-0.5, 0.5, 0.5, -0.5)/2, c(0,0,-1,-1)/2, c(0,0,0,0), back = "cull", front = "lines", col= 1, alpha = 1)
				quads3d( c(-0.5, 0.5, 0.5, -0.5)/2, c(0,0,-1,-1)/2, c(0,0,0,0), front = "cull",back = "lines", col= "blue", alpha = 1)
			}
			
		}
			
	}
	
	invisible(NULL)
}

#' Plot a flat representation
#' 
#' Create a flat representation of the spherical data.
#' 
#' @param segmentationCSV The file path to the csv file created from the segmentation process containing all features.
#' @param plotRows The rows from the csv file to be used to simulate plotting data.
#' @param col A vector of colours. 
#' @param singlePlot (logical) Indicates wether all rows should be added to one plot.
#' @param nsims The number of simulated values to plot. (Default = 1000 however, if your computer has little RAM reduce this)
#'  @details This function takes the features from the segmentation procedure and uses them to simulate data for
#' elevation and rotation. This data is then plot on a flat representation of the sphere.
#' Required columns are:\enumerate{
#'     \item UpDown.median
#'     \item UpDown.mad
#'     \item Degrees.median
#'     \item Degrees.mad
#' }
#' @return There is no return to the console. As a side effect a graphic is created.
#' @importFrom MASS kde2d
#' @export 
#' 
#' @examples 
#' \dontrun{
#' segmentationCSV = system.file("data", "SegData.csv", package = "GENEAsphere")
#' plotRows = c(1:5)
#' plotSegmentFlat(segmentationCSV, plotRows,
#'                 col = c("white", heat.colors(5, alpha = c(0.3, 0.2, 0.1, 0.05, 0.03))),
#'                 singlePlot = TRUE, nsims= 1000)}


plotSegmentFlat <- function(segmentationCSV, plotRows, 
		col = c("white",heat.colors(5, alpha = c(0.3, 0.2, 0.1, 0.05, 0.03))), singlePlot = TRUE, nsims= 1000){
	
	segmentationData <- read.csv(segmentationCSV)
	
	simData <- lapply(plotRows, function(row, data){
				upDownTheta <- c(segmentationData[row, "UpDown.median"], segmentationData[row, "UpDown.mad"])
				degreeTheta <- c(segmentationData[row, "Degrees.median"], segmentationData[row, "Degrees.mad"])
				
				# Ensure that NAs go to 0 instead
				
				upDownTheta[is.na(upDownTheta)] <- 0.0001
				degreeTheta[is.na(degreeTheta)] <- 0.0001
				# Check that this works. 
				
				
				upDownSim <- rnorm(nsims, upDownTheta[1], degreeTheta[2])
				degreeSim <- rnorm(nsims, degreeTheta[1], degreeTheta[2])
				xy <- cbind(upDownSim, degreeSim)
				
			}, data = segmentationData)
	
	for(dataSet in seq_along(simData)){
		# Input a routine that stops NAs here
	  
		tempData <- simData[[dataSet]]

		# Current issue is with this line - NA problem has gone away
		# Basically need to assign a correct value for h. 
		# Issue comes from the line inside kde2d that states: 
		# h<- c(bandwidth.nrd(x),bandwidth.nrd(y))
		# this means that bandwidth.nrd(x) or bandwidth.nrd(y) create negative values. 
		# Find under what conditions this happens so I can then prevent it. 
		# Does it actually matter? - Yes I need to set the values before it gets taken by kde2d.
		# Here the values of tempData[,2] and tempData[,1] can not be 0. 
		
		# Need to change from 0 to 0.0001
	#	print(tempData[,1]) - potentially change this to a random no generator. 
		tempData[tempData[,1] == 0] <- 0.0001*runif(1, 0.1, 1)
		tempData[tempData[,2] == 0] <- 0.0001*runif(1, 0.1, 1)
		# The issue here is the same no recurring in a vector! how? 
		
		# Correct the bandwidth if needed. 
		h = c(bandwidth.nrd(tempData[,2]), bandwidth.nrd(tempData[,1]))

		for (i in 1:length(h)){
		  if (h[i] <= 0){
		    h[i]=0.01*runif(1, 0.1, 1) # Makes h slightly bigger than 0 and therefore postive
		  }
		}
		
		d <- MASS::kde2d(tempData[, 2], tempData[,1], n = 50, h = h)
		
		if(singlePlot & dataSet > 1){
			add = TRUE
		} else{
			add = FALSE
			dev.new()
		}
		
		image(d, col = col, xlim = c(0, 360), ylim = c(-90,90), add = add,
				xlab = "Longitude", ylab = "Latitude", frame = FALSE, axes = FALSE)
		
		box(bty = "o")
		axis(side = 1, at = seq(0, 360, by = 45))
		axis(side = 2, at = seq(-90, 90, by = 45))
		grid(24,12)
	}
}

#' Plot a projection representation
#' 
#' Create a projection representation of the spherical data
#' 
#' @param segmentationCSV The file path to the csv file created from the segmentation process containing all features.
#' @param plotRows The rows from the csv file to be used to simulate plotting data.
#' @param projection The type of projection to be used. Can be any of those used by \code{mapproject} in the package \code{mapproj}.
#' @param col A character string to indicate the colour of the heat mapping.
#' @param singlePlot (logical) Indicates wether all rows should be added to one plot.
#' @param nsims The number of simulated values to plot. (Default = 1000 however, if your computer has little RAM reduce this)
#'  @details This function takes the features from the segmentation procedure and uses them to simulate data for
#' elevation and rotation. This data is then plotted on a projected representation of the sphere.
#' Required columns are:\enumerate{
#'     \item UpDown.median
#'     \item UpDown.mad
#'     \item Degrees.median
#'     \item Degrees.mad
#' }
#' @return There is no return to the console. As a side effect a graphic is created.
#' @import ggplot2 
#' @importFrom misc3d kde3d contour3d
#' @export 
#' 
#' @examples 
#' \dontrun{
#' segmentationCSV = system.file("data", "SegData.csv", package = "GENEAsphere")
#' plotRows = c(1:1)
#' plotSegmentProjection(segmentationCSV, plotRows, projection = "aitoff", 
#'                     col = "red", singlePlot = TRUE, nsims = 1000)}

plotSegmentProjection <- function(segmentationCSV, plotRows, projection = "aitoff", 
		col = "red", singlePlot = TRUE, nsims = 1000){
	
	segmentationData <- read.csv(segmentationCSV)
	
	simData <- lapply(plotRows, function(row, data){
				upDownTheta <- c(segmentationData[row, "UpDown.median"], segmentationData[row, "UpDown.mad"])
				degreeTheta <- c(segmentationData[row, "Degrees.median"], segmentationData[row, "Degrees.mad"])
				
				upDownSim <- rnorm(nsims, upDownTheta[1], degreeTheta[2])
				degreeSim <- rnorm(nsims, degreeTheta[1], degreeTheta[2])
				
				xy <- cbind(upDownSim, degreeSim)
				
			}, data = segmentationData)
	
	theme_set(theme_bw())
	projectionPlot <- ggplot()
	
	for(dataSet in seq_along(simData)){
		
		tempData <- simData[[dataSet]]
		
		d <- kde2d(tempData[, 2], tempData[,1], n = 50)
		
		tempDataLong <- cbind(rep(d$x, each = 50), 
				reshape(as.data.frame(cbind(rev(d$y), d$z)), 
						idvar = "V1", varying = list(2:51), direction = "long"))
		names(tempDataLong) <- c("Long", "Lat", "time", "value")
		
		
		if(singlePlot & dataSet > 1){
			projectionPlot <- projectionPlot + geom_tile(data = tempDataLong, aes_string( x = "Long", y = "Lat", fill = "value"))
			if(dataSet == max(seq_along(simData))){
				print(projectionPlot)
			}
		} else if(singlePlot & dataSet == 1){
			projectionPlot <- projectionPlot + geom_tile(data = tempDataLong, aes_string( x = "Long", y = "Lat", fill = "value")) + 
					coord_map(projection, xlim = c(0, 360), ylim = c(-90,90)) + 
					scale_fill_gradient(low="white", high = col) +
					theme(legend.position  = "none", 
							axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
					scale_y_continuous(breaks=(-6:6)*15,limits=c(-90,90)) + 
					scale_x_continuous(breaks=(0:8)*45,limits=c(0,360))
			if(length(simData) == 1){
				print(projectionPlot)	
			}
		} else{
			projectionPlot <- ggplot() + geom_tile(data = tempDataLong, aes_string( x = "Long", y = "Lat", fill = "value")) + 
					coord_map(projection, xlim = c(0, 360), ylim = c(-90,90)) + 
					scale_fill_gradient(low="white", high = col) +
					theme(legend.position  = "none", 
							axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
					scale_y_continuous(breaks=(-6:6)*15,limits=c(-90,90)) + 
					scale_x_continuous(breaks=(0:8)*45,limits=c(0,360))
			dev.new()
			print(projectionPlot)
		}
		
	}
}

#' Plot an ellipse representation of a segment 'confidence interval'
#' 
#' Create an ellipse representing a 'confidence interval' of a given segment and plot a projected representation.
#' 
#' @param segmentationCSV The file path to the csv file created from the segmentation process containing all features.
#' @param plotRows The rows from the csv file to be used to simulate plotting data.
#' @param projection The type of projection to be used. Can be any of those used by \code{mapproject} in the package \code{mapproj}.
#' @param col A vector of character strings to indicate the colour of each ellipse. 
#' If the number of segments is greater than the length of the colours vector then colurs will be repeated.
#' @param singlePlot (logical) Indicates wether all rows should be added to one plot.
#' @param confidenceLevel The alpha value for the confidence interval. A value of 0.05 corresponds to a 95\% confidence interval.
#' @param alpha The alpha level to use for plotting colours. 
#' @param wrap (logical) Indicating whether segments should be wrapped around the sphere or cropped. By default ellipses are cropped.
#' @param greyGrid (logical) Should the plot be created with a white background and grey grid or a grey background with white grid (default).
#' 
#' @details This function uses the mean and standard deviation estimates for elevation and rotation of a segment to determine
#' a confidence interval for each direction. This is then used to generate an ellipse representing the two dimensional confidence region. 
#' This ellipse is plotted onto a projected representation of the sphere.
#' Required columns are:\enumerate{
#'     \item UpDown.median
#'     \item UpDown.mad
#'     \item Degrees.median
#'     \item Degrees.mad
#' }
#' @return There is no return to the console. As a side effect a graphic is created.
#' @import ggplot2 
#' @importFrom utils read.csv
#' @export 
#' 
#' @examples 
#' \dontrun{
#' segmentationCSV = system.file("data", "SegData.csv", package = "GENEAsphere")
#' plotRows = c(1:1)
#' plotSegmentEllipse(segmentationCSV, plotRows, projection = "aitoff", 
#'   col = "red", singlePlot = TRUE, confidenceLevel = 0.05, 
#'   alpha = thresholds, wrap = FALSE, greyGrid = FALSE)
#'  }
  
plotSegmentEllipse <- function(segmentationCSV, plotRows, projection = "aitoff", 
		col = "red", singlePlot = TRUE, confidenceLevel = 0.05, alpha = thresholds, wrap = FALSE, greyGrid = FALSE){
	
  thresholds <- alpha
  
	if(length(col)!= length(plotRows)){
		col = rep(col, length.out = length(plotRows))
	}
	ifelse(greyGrid, theme_set(theme_bw()), theme_set(theme_grey()) )	
	
	segmentationData <- read.csv(segmentationCSV)
	
	projectionPlot <- ggplot()
	
	for(data in seq_along(plotRows)){
		
		##Extract the means and standard deviations
		segMeans <- do.call("c", segmentationData[plotRows[data], c("Degrees.median", "UpDown.median")])
		segSD <- do.call("c", segmentationData[plotRows[data], c("Degrees.mad", "UpDown.mad")])
		
    ## find duration and multiplied sd to find corresponding alpha values
    segSDMult <- segSD["Degrees.mad"]*segSD["UpDown.mad"]
    segDuration <- segmentationData[plotRows[data], c("segment.duration")]
    alphaCol <- min(which(segSDMult < as.numeric(colnames(thresholds))))
    alphaRow <- min(which(segDuration < as.numeric(rownames(thresholds))))

    alpha <- thresholds[alphaRow, alphaCol]
    
		##Calculate the ellipse	
		ellipse <- calculateEllipse(means = segMeans, sd = segSD, alpha = confidenceLevel)
		
		ellipseWrap <- NULL
		
		##If wrap rather than crop create the second ellipse
		if(wrap){
			if(any(ellipse$x < 0)){
				ellipseWrap <- ellipse
				ellipseWrap$x <- ellipse$x + 360
				ellipseWrap$x[ellipseWrap$x > 360] <- 360
			}
			if(any(ellipse$x > 360)){
				ellipseWrap <- ellipse
				ellipseWrap$x <- ellipse$x - 360
				ellipseWrap$x[ellipseWrap$x < 0] <- 0
			}
		}
		
		##Crop the main ellipse
		ellipse$x[ellipse$x > 360] <- 360
		ellipse$x[ellipse$x < 0] <- 0
		
		ellipse$y[ellipse$y > 90] <- 90
		ellipse$y[ellipse$y < -90] <- -90
		if(!is.null(ellipseWrap)){
			ellipseWrap$y[ellipseWrap$y > 90] <- 90
			ellipseWrap$y[ellipseWrap$y < -90] <- -90
		}
		
		##Convert to a data frame for plotting
		ellipse <- do.call("data.frame", ellipse)
		if(!is.null(ellipseWrap)){
			ellipseWrap <- do.call("data.frame", ellipseWrap)
		}
		
		##Plot the ellipse
		if(singlePlot){
			projectionPlot <- projectionPlot + geom_polygon(aes_string(x = "x",y = "y"), data = ellipse, alpha = alpha, fill = col[data])
			if(!is.null(ellipseWrap)){
				projectionPlot <- projectionPlot + geom_polygon(aes_string(x = "x",y = "y"), data = ellipseWrap, alpha = alpha, fill = col[data])
			}
		} else{
			projectionPlot <- ggplot() + geom_polygon(aes_string(x = "x",y = "y"), data = ellipse, alpha = alpha, fill = col[data]) + 
					coord_map(projection, xlim = c(0, 360), ylim = c(-90,90)) + 
					theme(legend.position  = "none", 
							axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
					scale_y_continuous(breaks=(-6:6)*15,limits=c(-90,90)) + 
					scale_x_continuous(breaks=(0:8)*45,limits=c(0,360))
			if(!is.null(ellipseWrap)){
				projectionPlot <- projectionPlot + geom_polygon(aes_string(x = "x",y = "y"), data = ellipseWrap, alpha = alpha, fill = col[data])
			}
			dev.new()
			print(projectionPlot)
		}
		
	}
	
	##If plotting all segments on one plot change the coordinates, scale etc.
	if(singlePlot){
		projectionPlot <- projectionPlot + coord_map(projection, xlim = c(0, 360), ylim = c(-90,90)) + 
				theme(legend.position  = "none", 
						axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
				scale_y_continuous(breaks=(-6:6)*15,limits=c(-90,90)) + 
				scale_x_continuous(breaks=(0:8)*45,limits=c(0,360))
		print(projectionPlot)
	}
	
}






