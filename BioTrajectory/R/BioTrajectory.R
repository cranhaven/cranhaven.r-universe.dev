showBarnes <- function(im, boardRadius, holeRadius, c1, c2) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar)) 
  par(oma=c(0,0,1,0))
  plot(im)
  mtext("Barnes", outer = TRUE)
  with(c1, circles(x,y,boardRadius,fg="yellow", lwd=3))
  with(c2, circles(x,y,holeRadius,fg="yellow", lwd=3)) 
}

detectCircles <- function(px, radius, N, sigma = 25) {
  hc <- hough_circle(px, radius)
  nms <- function(im, sigma) {
    im[dilate_square(im, sigma) != im] <- 0
    im
  }
  hc.clean <- isoblur(hc, 4) %>% nms(sigma)
  hc2 <- as.data.frame(hc.clean)
  df <- dplyr::arrange(hc2,dplyr::desc(hc2$value)) %>%
    head(N)
  return(df)
}

findM <- function(k) {
  m1 <- ceiling(sqrt(k))
  m2 <- 1
  while (m2 * (m2 - 1) < k) {
    m2 <- m2 + 1
  }
  if (abs(m1 * m1 - k) < abs(m2 * (m2 - 1) - k)) {
    return(c(m1, m1))
  } else {
    return(c(m2, m2 - 1))
  }
}

legendPlot <- function(x, pal) {
  z <- seq(min(x), max(x), length.out = 100)
  legendMatrix <- matrix(z, ncol = 1)
  image(t(legendMatrix), col = pal, axes = FALSE, xlab = "", ylab = "")
  axis(4, at = c(0, 1), labels = round(range(x), 2))
}

removeBackground<- function(im, Barnes) {
  X <- row(as.matrix(im))
  Y <- col(as.matrix(im))
  I <- sqrt((X - Barnes$c1$x)^2 + (Y - Barnes$c1$y)^2) <= Barnes$boardRadius  
  im[!I] <- 0  
  return(im)
}

readImage <- function(path, resizeRows) {
  extension <- tolower(tools::file_ext(path))  # Get the file extension and convert it to lowercase
  image <- switch(extension,
                   "jpg" = as.cimg(readJPEG(path)),    # Read JPG files
                   "jpeg" = as.cimg(readJPEG(path)),  # Read JPEG files
                   "png" = as.cimg(readPNG(path)),    # Read PNG files
                   "tiff" = as.cimg(readTIFF(path)),  # Read TIFF files
                   "tif" = as.cimg(readTIFF(path)),   # Read TIF files
                   stop("Unsupported image format. Use PNG, JPG, TIFF.")  # Error message for unsupported formats
  )
  
  if (length(dim(image)) == 4) 
    image <- image[,,,1]
  
  if (length(dim(image)) == 3) 
    image <- image[,,1]
  
  image <- as.cimg(t(image))
  
  if (nrow(image) > resizeRows) {
    image <- resize(image, resizeRows, (ncol(image) * (resizeRows / nrow(image))))
  }
  
  return(image)
}