
#' @title Plot an 3D sequence with sedentary sphere
#' 
#' @description From the output of the read.bin function in GENEAread to simulate data and create a spherical representation.
#' 
#' @param x The AccData input to be plotted
#' @param start start time to enter in the format 0 to 1 or "dd hh:mm:ss"
#' @param end end time to enter in the format 0 to 1 or "dd hh:mm:ss"
#' @param length Length of interval. 
#' @param time.format Data extraction via get.intervals
#' @param density Whether to plot a  3d density plot.
#' @param col Colours to use for lines or density plot
#' @param alpha Vector of transparencies to user for density plot
#' @param arrow To display a place holder arrow to establish directionality
#' @param levels Breakpoints for plotting of isospheres for density. Follows the formulation of 0.9 == respective isosphere contains the highest probability 10 of the population, according to kernel density estimate.
#' @param add If draw, superimpose on to existing plot. Else add to a new plot.
#' @param ... Arguements that will be passed to the 
#' 
#' @details Takes the raw data output of the GENEActiv as AccData to plot points on the sedentary sphere.
#' 
#' @return There is no return to the console. As a side effect an rgl graphic is created.
#' @importFrom GENEAread get.intervals
#' @import rgl misc3d 
#' @export 
#' 
#' @examples 
#' \dontrun{
#' x = readRDS(system.file("extdata", "AccData.rds", package = "GENEAsphere"))
#' plotSphere(x)}


plotSphere <- function(x, start=0, end= 1, length = NULL, time.format = "auto", 
                       density = FALSE, col, alpha, arrow = TRUE, levels, add = FALSE,...){
  
              if (add){
                arrow = FALSE
              }
              
              tmp3 = get.intervals(x, start, end, length, time.format, incl.date = T)[,-1]
              sampling.freq = 100
              #time.format = match.arg(time.format)
              
              if (inherits(x, "list")){
                sampling.freq = x$freq
              }
              if (!density){ 
                
                plot3d( tmp3, add = add)
                
                if (missing(col)){
                   col = heat.colors(nrow(tmp3))
                   lines3d(tmp3, col = col)
                }
          
              } else {
                #do a density estimate
                d <- kde3d(tmp3[,1], tmp3[,2], tmp3[,3],n = 50, ...)
                dtmp = sort(d$d)
                
                if (missing(levels)) levels = 1- c(0.1, 0.25, 0.5, 0.75, 0.9)
                levels = 1-levels
                lev = NULL
                for (il in levels){
                  
                  lev = c(lev, which.max((cumsum(dtmp)/ sum(dtmp)) > il))
                }
                lev = dtmp[lev]
                
                if (missing(col)) col = heat.colors(5)
                if (missing(alpha)) alpha = c(0.03, 0.05, 0.1, 0.2, 0.3)
                contour3d(d$d, lev,  d$x, d$y, d$z, color = col , alpha = alpha, scale = F, add=add)
              
              }
              if (!add){
                aspect3d("iso")
                spheres3d(0,0,0,1, alpha = 0.5, front="line", back = "line")
                grid3d(side = c("x","y","z"),at = c(0,0,0))#; plot(1:nrow(tmps)/nrow(tmps), tmps[,3], type="l");  abline(v = offset+c(0, leng), col=2) 
              }
              if (arrow){
                #add a placeholder arrow
                
                if (!density){
                  triangles3d(c(-1, 0, 1)/2, c(0,1,0)/2, c(0,0,0)/2, coords = c(1,3,2), back="cull", col="blue", alpha = 0.5)
                  triangles3d(c(-1, 0, 1)/2, c(0,1,0)/2, c(0,0,0)/2, front="cull", col="blue", alpha=0.5)
                  quads3d( c(-0.5, 0.5, 0.5, -0.5)/2, c(0,0,-1,-1)/2, c(0,0,0,0), back = "cull", col= "blue", alpha = 0.5)
                  quads3d( c(-0.5, 0.5, 0.5, -0.5)/2, c(0,0,-1,-1)/2, c(0,0,0,0), front = "cull", col= "blue", alpha = 0.5)
                } else {
                  triangles3d(c(-1, 0, 1)/2, c(0,1,0)/2, c(0,0,0)/2, coords = c(1,3,2), back="cull", front = "lines", col="blue", alpha = 1)
                  triangles3d(c(-1, 0, 1)/2, c(0,1,0)/2, c(0,0,0)/2, front="cull", back = "lines", col="blue", alpha=1)
                  quads3d( c(-0.5, 0.5, 0.5, -0.5)/2, c(0,0,-1,-1)/2, c(0,0,0,0), back = "cull", front = "lines", col= "blue", alpha = 1)
                  quads3d( c(-0.5, 0.5, 0.5, -0.5)/2, c(0,0,-1,-1)/2, c(0,0,0,0), front = "cull",back = "lines", col= "blue", alpha = 1)
                }
                
              }
              cat("Plotted ",nrow(tmp3) / sampling.freq , " seconds of data.\n")
              invisible(tmp3)
}

