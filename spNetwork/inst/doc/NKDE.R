## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
backup_option <- options()
base_wd <- getwd()

## ----include=FALSE------------------------------------------------------------
load(system.file("extdata", "results_vignette_nkde.rda",
                           package = "spNetwork", mustWork = TRUE))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ggplot2)
library(reshape2)
library(kableExtra)
library(spNetwork)
library(RColorBrewer)

x <- seq(-15.01,15.01,by = 0.01)
df <- data.frame(
  x = x,
  gaussian = gaussian_kernel(x,15),
  epanechnikov = epanechnikov_kernel(x,15),
  quartic = quartic_kernel(x,15),
  triangle = triangle_kernel(x,15),
  tricube = tricube_kernel(x,15),
  triweight = triweight_kernel(x,15),
  cosine = cosine_kernel(x,15),
  uniform = uniform_kernel(x,15))

df2 <- melt(df, id.vars = "x")
names(df2) <- c("x","kernel","y")

ggplot(df2) + 
  geom_line(aes(x=x,y=y,color=kernel),size=1)


## ----message=FALSE, warning=FALSE---------------------------------------------
Funs <- c(gaussian_kernel, gaussian_kernel_scaled,epanechnikov_kernel,
          quartic_kernel,triangle_kernel, uniform_kernel,
          tricube_kernel,triweight_kernel,
          cosine_kernel)
Names <- c("gaussian", "scaled gaussian", "epanechnikov", "quartic",
           "triangle","uniform", "tricube", "triweight", "cosine")
integrals <- sapply(Funs,function(f){
  return(round(integrate(f,upper=15,lower=-15,bw=15)$value,3))
         })
df <- data.frame("kernel"=Names,
                 "integrals" = integrals)

kable(df)


## ----message=FALSE, warning=FALSE---------------------------------------------
x <- seq(-15.01,15.01,by = 0.01)
df <- data.frame(
  x = x,
  gaussian = gaussian_kernel(x,15),
  gaussian_scaled = gaussian_kernel_scaled(x,15),
  epanechnikov = epanechnikov_kernel(x,15),
  quartic = quartic_kernel(x,15)
  )

df2 <- melt(df, id.vars = "x")
names(df2) <- c("x","kernel","y")

ggplot(df2) + 
  geom_line(aes(x=x,y=y,color=kernel),size=1)

## ----message=FALSE, warning=FALSE---------------------------------------------
# first load data and packages
library(sf)
library(spNetwork)
library(tmap)

data(mtl_network) 
data(bike_accidents)

# then plotting the data
tm_shape(mtl_network) + 
  tm_lines("black") + 
  tm_shape(bike_accidents) + 
  tm_dots("red", size = 0.2)

# then calculating some lixels to use as sampling points
lixels <- lixelize_lines(mtl_network,200,mindist = 50)
samples <- lines_center(lixels)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  # then applying the NKDE
#  densities <- nkde(mtl_network,
#                    events = bike_accidents,
#                    w = rep(1,nrow(bike_accidents)),
#                    samples = samples,
#                    kernel_name = "quartic",
#                    bw = 300, div= "bw",
#                    method = "discontinuous", digits = 1, tol = 1,
#                    grid_shape = c(1,1), max_depth = 8,
#                    agg = 5, #we aggregate events within a 5m radius (faster calculation)
#                    sparse = TRUE,
#                    verbose = FALSE)
#  

## ----message=FALSE, warning=FALSE---------------------------------------------
samples$density <- densities

## ----message=FALSE, warning=FALSE---------------------------------------------

# rescaling to help the mapping
samples$density <- samples$density*1000

samples2 <- samples[order(samples$density),]

colorRamp <- brewer.pal(n = 7, name = "Spectral")
colorRamp <- rev(colorRamp)

title <- paste0("bike accident density by kilometres in 2016,",
                "\nwithin a radius of 300 metres")

tm_shape(mtl_network) + 
  tm_lines("black") + 
  tm_shape(samples2) + 
  tm_dots("density", style = "kmeans", palette = colorRamp, n = 7, size = 0.1) + 
  tm_layout(legend.outside = TRUE, 
            main.title = title , main.title.size = 1)


## ----message=FALSE, warning=FALSE, eval = FALSE-------------------------------
#  # setting the multisession plan
#  future::plan(future::multisession(workers=2))
#  
#  # then applying the NKDE
#  densities_mc <- nkde.mc(mtl_network,
#                    events = bike_accidents,
#                    w = rep(1,nrow(bike_accidents)),
#                    samples = samples,
#                    kernel_name = "quartic",
#                    bw = 300, div= "bw",
#                    method = "discontinuous", digits = 1, tol = 1,
#                    grid_shape = c(2,2), # splitting the study area in 4 rectangles
#                    max_depth = 8,
#                    agg = 5, #we aggregate events within a 5m radius
#                    sparse = TRUE,
#                    verbose = FALSE)
#  
#  # let's set back the classical sequential plan
#  if (!inherits(future::plan(), "sequential")) future::plan(future::sequential)

## ----message=FALSE, warning=FALSE---------------------------------------------
# we can compare the previous result and the new one
diff <- sum(abs(densities - densities_mc))
print(paste("overall difference between the regular and paralellized method : ",round(diff,12),sep=""))


## ----message=FALSE, warning=FALSE, eval = FALSE-------------------------------
#  adapt_densities <- nkde(mtl_network,
#                    events = bike_accidents,
#                    w = rep(1,nrow(bike_accidents)),
#                    samples = samples,
#                    kernel_name = "quartic",
#                    bw = 300, div= "bw",
#                    adaptive = TRUE, # we use here an adaptive bandwidth
#                    trim_bw = 600, # the maximum local values of bandwidth will be 600m
#                    method = "discontinuous", digits = 1, tol = 1,
#                    grid_shape = c(1,1), max_depth = 16,
#                    agg = 5, #we aggregate events within a 5m radius (faster calculation)
#                    sparse = TRUE,
#                    verbose=FALSE)
#  
#  samples$density <- adapt_densities$k

## ----message=FALSE, warning=FALSE---------------------------------------------
samples$density <- adapt_densities$k

## ----message=FALSE, warning=FALSE---------------------------------------------
circles <- st_buffer(adapt_densities$events,
                     dist = adapt_densities$events$bw)

ids <- c(1,52,20,86,14,75,126,200,177)

tm_shape(mtl_network) + 
  tm_lines("black") + 
  tm_shape(bike_accidents) + 
  tm_dots("red", size = 0.2) + 
  tm_shape(circles[ids,]) + 
  tm_borders("blue", lwd = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
# rescaling to help the mapping
samples$density <- samples$density*1000

colorRamp <- brewer.pal(n = 7, name = "Spectral")
colorRamp <- rev(colorRamp)

samples2 <- samples[order(samples$density),]
title <- paste0("bike accident density by kilometres in 2016,",
                "\nwithin a radius of 300 metres (adaptive bandiwdth)")

tm_shape(mtl_network) + 
  tm_lines("black") + 
  tm_shape(samples2) + 
  tm_dots("density", style = "kmeans", palette = colorRamp, n = 7, size = 0.1) + 
  tm_layout(legend.outside = TRUE, 
            main.title = title, main.title.size = 1)


## ----message=FALSE, warning=FALSE---------------------------------------------
# selecting the events in a subset of the data
center_event <- bike_accidents[125,]
study_area <- st_buffer(center_event, dist = 800)
events_sel <- as.vector(st_intersects(bike_accidents, study_area, sparse = FALSE))
events <- subset(bike_accidents,events_sel)

# generating the sampling points
lines_sel <- as.vector(st_intersects(mtl_network, study_area, sparse = FALSE))
lines <- subset(mtl_network,lines_sel)

lixels <- lixelize_lines(lines,200,mindist = 50)
samples <- lines_center(lixels)

tm_shape(study_area) + 
  tm_borders("black", lwd = 2) + 
  tm_shape(lines) + 
  tm_lines("black") +
  tm_shape(events) + 
  tm_dots("red", size = 0.2)

## ----message=FALSE, warning=FALSE, eval = FALSE-------------------------------
#  # calculating the NKDE values, adjusted
#  adjusted_densities <- nkde(lines = mtl_network,
#                              events = events,
#                              w = rep(1,nrow(events)),
#                              samples = samples,
#                              kernel_name = "quartic",
#                              bw = 150,
#                              adaptive = FALSE,
#                              method = "discontinuous",
#                              div = "bw",
#                              diggle_correction = TRUE, study_area = study_area,
#                              max_depth = 15,
#                              digits = 2,tol = 0.1,agg = 5,sparse = TRUE,
#                              grid_shape = c(1,1),verbose = FALSE)
#  
#  samples$density <- adjusted_densities

## ----message=FALSE, warning=FALSE---------------------------------------------
samples$density <- adjusted_densities * 1000

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
colorRamp <- brewer.pal(n = 7, name = "Spectral")
colorRamp <- rev(colorRamp)

title <- paste0("bike accident density by kilometres in 2016", 
                "\nwithin a radius of 150 metres")

tm_shape(lines) + 
  tm_lines("black") + 
  tm_shape(samples) + 
  tm_dots("density", palette = colorRamp, style = "kmeans", n = 7, size = 0.1) + 
  tm_layout(legend.outside = TRUE, 
            main.title = title, main.title.size = 1)
  

## ----eval = FALSE-------------------------------------------------------------
#  bws_selection_cv <- bw_cv_likelihood_calc(
#    bws = seq(50,700,50),
#    lines = mtl_network, events = bike_accidents,
#    w = rep(1,nrow(bike_accidents)),
#    kernel_name = "quartic", method = "discontinuous",
#    diggle_correction = FALSE, study_area = NULL,
#    max_depth = 8,
#    digits=2, tol=0.1, agg=5,
#    sparse=TRUE, grid_shape=c(1,1),
#    verbose=FALSE, check=TRUE)
#  
#  bws_selection_cvl <- bw_cvl_calc(
#    bws = seq(50,700,50),
#    lines = mtl_network, events = bike_accidents,
#    w = rep(1,nrow(bike_accidents)),
#    kernel_name = "quartic", method = "discontinuous",
#    diggle_correction = FALSE, study_area = NULL,
#    max_depth = 8,
#    digits=2, tol=0.1, agg=5,
#    sparse=TRUE, grid_shape=c(1,1),
#    verbose=FALSE, check=TRUE)
#  

## ----eval = FALSE-------------------------------------------------------------
#  bws_selection_cv_adpt_dis <- bw_cv_likelihood_calc(
#    bws = seq(50,700,50),
#    trim_bws = seq(50,700,50)*2,
#    lines = mtl_network,
#    events = bike_accidents,
#    w = rep(1,nrow(bike_accidents)),
#    adaptive = TRUE,
#    kernel_name = "quartic",
#    method = "discontinuous",
#    diggle_correction = FALSE,
#    study_area = NULL,
#    max_depth = 8,
#    digits=2,
#    tol=0.1,
#    agg=5,
#    sparse=TRUE,
#    grid_shape=c(1,1),
#    verbose=TRUE,
#    check=TRUE)
#  
#  bws_selection_cv_adpt_cont <- bw_cv_likelihood_calc(
#    bws = seq(50,700,50),
#    trim_bws = seq(50,700,50)*2,
#    lines = mtl_network,
#    events = bike_accidents,
#    w = rep(1,nrow(bike_accidents)),
#    adaptive = TRUE,
#    kernel_name = "quartic",
#    method = "continuous",
#    diggle_correction = FALSE,
#    study_area = NULL,
#    max_depth = 8,
#    digits=2,
#    tol=0.1,
#    agg=5,
#    sparse=TRUE,
#    grid_shape=c(1,1),
#    verbose=TRUE,
#    check=TRUE)
#  
#  cv_values <- data.frame(
#    "bw" = bws_selection_cv$bw,
#    "cv_likelihood" = bws_selection_cv$cv_scores,
#    "cvl_crit" = bws_selection_cvl$cvl_scores,
#    "cv_likelihood_adpt_dis" = bws_selection_cv_adpt_dis$cv_scores,
#    "cv_likelihood_adpt_cont" = bws_selection_cv_adpt_cont$cv_scores
#  )

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(cv_values, digits = 2,row.names = FALSE)

## ----include = FALSE----------------------------------------------------------
# reset all the user parameters
options(backup_option)
setwd(base_wd)
oldpar <- par(mfrow = c(1,2))
par(oldpar)

