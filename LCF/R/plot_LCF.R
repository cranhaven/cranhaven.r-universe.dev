#' Plot sample data, linear combination fit and residual spectrum
#'
#' This function allows plotting (png or tiff image files) of the corrected sample spectrum, the linear combination fit and the residual. 
#' @param all.samples List of all samples
#' @param all.standards List of all standards
#' @param LCF.res Results from function fit_float()
#' @param LC.vals The fitting range values for the linear combination fitting
#' @param corr.norm Vector of the base-line correction and edge-step normalization values (vector of length 4)
#' @param float Logical, default to FALSE
#' @param exclude Logical, default to FALSE
#' @param use.tiff Logical, default to FALSE
#' @param E.zero Set E0, defaults to NULL
#' @param set.plot.ymax Set maximum of plot y axis, defaults to NULL
#' @param file.output Logical, default to FALSE
#' @keywords plot, LCF
#' @importFrom grDevices dev.off png tiff
#' @importFrom graphics abline lines par plot  
#' @importFrom utils write.csv2 
#' @export
#' @examples
#' data(stdmix)
#' corr.spec.standards  <- initial_load(specdat[1:4], 
#'   corr.norm = c(-36, -15, 37, 58))
#' corr.spec.samples    <- initial_load(specdat[5:8], 
#'   corr.norm = c(-36, -15, 37, 58))
#' param.float <- expand.grid(pre.adj.1 = seq(-45,-30,5), 
#'   pre.adj.2 = seq(-19,-9,5), post.adj.1 = seq(34,40,2), 
#'   post.adj.2 = seq(50,65,5))
#' float.fit <- fit_float(all.samples = corr.spec.samples[1], 
#'   all.standards = corr.spec.standards, LC.vals = c(-14, 46), 
#'   float = param.float, ex.smaller = 0.05)
#' par(pty="s")
#' plot_LCF(all.samples = corr.spec.samples[1], 
#'   all.standards = corr.spec.standards, 
#'   LCF.res = float.fit[1,], LC.vals = c(-14,46), 
#'   corr.norm = c(-36, -15, 37, 58))


plot_LCF <- function (all.samples, all.standards, LCF.res, LC.vals, 
                      corr.norm, float = NULL, exclude = NULL, 
                      use.tiff = NULL, E.zero = NULL, 
                      set.plot.ymax = NULL, file.output = NULL) {
  
  ## check and assign FALSE if optional parameters are not set
  if(is.null(float)) {float <- FALSE}
  if(is.null(exclude)) {exclude <- FALSE}
  if(is.null(use.tiff)) {use.tiff <- FALSE}
  if(is.null(file.output)) {file.output <- FALSE}
  
  for (i in 1:length(all.samples)) {
    
    sample.name <- all.samples[[i]]$name
    
    sample.float.res <- LCF.res[(grep(sample.name, row.names(LCF.res))),]
    
    
    all.std.names <- NULL
    for (j in 1:length(all.standards)) {
      all.std.names <- c(all.std.names, all.standards[[j]]$name)
    }
    
    raw.coeff <- as.numeric(sample.float.res[,all.std.names])
    
    LC.pre <- LC.vals[1]
    LC.post <- LC.vals[2]
    if (is.null(E.zero)) {
      E.zero <- all.samples[[i]]$data$E0
    }
    
    
    
    corr.spec <- bkg_corr(raw.spec = all.samples[[i]], corr.norm = as.numeric(sample.float.res[,c("pre.adj.1", "pre.adj.2", "post.adj.1", "post.adj.2")]))
    
    ## create list of new sample for fitting function
    new.spec <- list("name" = all.samples[[i]]$name, "data" = list("E0" = all.samples[[i]]$data$E0, "corr.spec" = corr.spec))
    
    standards <- std_df(sample = new.spec, all.standards = all.standards)
    
    standards.proportion <- data.frame(mapply(`*`,standards,raw.coeff))
    
    fit.spec <- rowSums(standards.proportion)
    
    res.spec <- fit.spec - corr.spec$cor.absorption
    
    if (file.output == TRUE) {
      
      output.spec <- cbind(energy = corr.spec$energy, norm.absorption = corr.spec$cor.absorption, fitted.spectrum = fit.spec, residual.spectrum = res.spec, standards = standards.proportion)
      write.csv2(output.spec, paste("output.LCF", sample.name, "csv", sep = "."), row.names = FALSE)
    }
    
    LC.spec <- cbind(energy = corr.spec$energy, data = corr.spec$cor.absorption, fit = fit.spec, residual = res.spec)
    
    
    plot.x1 <- E.zero+LC.vals[1]-5
    plot.x2 <- E.zero+LC.vals[2]+5
    
    ## create image file name with high resolution (png or tiff)
    if (file.output == TRUE) {
      if (float == TRUE) {
        if (exclude == TRUE) {
          if (use.tiff == TRUE) {
            tiff(filename = paste("LC.fit", sample.name, "adjusted.EXCLU.tiff", sep = "."),
                 width = 3200, height = 3200, units = "px", res = 800,
                 compression = c("none"))
          } else {
            png(filename = paste("LC.fit", sample.name, "adjusted.EXCLU.png", sep = "."),
                width = 3200, height = 3200, units = "px", res = 800)
            
          }
          
          ## create header name
          head.name <- paste(sample.name, "adjusted and excluded", sep = " ")
          
        } else {
          if (use.tiff == TRUE) {
            tiff(filename = paste("LC.fit", sample.name, "adjusted.tiff", sep = "."),
                 width = 3200, height = 3200, units = "px", res = 800,
                 compression = c("none"))
          } else {
            png(filename = paste("LC.fit", sample.name, "adjusted.png", sep = "."),
                width = 3200, height = 3200, units = "px", res = 800)
            
          }
          
          ## create header name
          head.name <- paste(sample.name, "adjusted, unexcluded", sep = " ")
          
        }
        
      } else {
        if (exclude == TRUE) {
          if (use.tiff == TRUE) {
            tiff(filename = paste("LC.fit", sample.name, "initial.EXCLU.tiff", sep = "."),
                 width = 3200, height = 3200, units = "px", res = 800,
                 compression = c("none"))
          } else {
            png(filename = paste("LC.fit", sample.name, "initial.EXCLU.png", sep = "."),
                width = 3200, height = 3200, units = "px", res = 800)
          }
          
          ## create header name
          head.name <- paste(sample.name, "initial and excluded", sep = " ")
          
        } else {
          if (use.tiff == TRUE) {
            tiff(filename = paste("LC.fit", sample.name, "initial.tiff", sep = "."),
                 width = 3200, height = 3200, units = "px", res = 800,
                 compression = c("none"))
          } else {
            png(filename = paste("LC.fit", sample.name, "initial.png", sep = "."),
                width = 3200, height = 3200, units = "px", res = 800)
          }
          
          ## create header name
          head.name <- paste(sample.name, "initial, unexcluded", sep = " ")
          
        }
      }
    } else {
      
      ## simply use sample name as header name
      head.name <- paste(sample.name)
    }
    
    ## set squared plots (pty), margins (mar), and size of plot (cex)
    if (file.output == TRUE) {
      par(pty="s",
          mar=c(5,3,2,0)+0.1, # c(bottom, left, top, right)
          cex = 0.5
      )
    }
    
    if (is.null(set.plot.ymax)) {
      plot.ymax <- max(corr.spec$cor.absorption)+0.5
    } else {
      plot.ymax <- set.plot.ymax
    }
    
    ## plot the coordinate system
    plot(data~energy, 
         data = LC.spec,
         type = "n", 
         las=1,
         xlim=c(plot.x1,plot.x2), ylim = c(0, plot.ymax),
         xlab = "Energy (eV)", ylab = "Normalized absorption",
         main = head.name
    )
    
    
    ## create line y = 0
    abline(h = 0)
    
    ## add points of the energy
    lines(data~energy, data = LC.spec, type = "p", pch = 20, col="blue", cex=0.3)
    
    ## add line of the fit
    lines(fit~energy, data = LC.spec, lty = 1, col="red", lwd=1)
    
    ## add line of the residual
    lines(residual~energy, data = LC.spec, lty = 6, col="darkgreen", lwd = 1)
    
    ## close image file
    if (file.output == TRUE) {
      dev.off()
    }
  }
  
  ## close function 
}