## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.align = "center",
    fig.width = 10,
    fig.height = 8
)

## -----------------------------------------------------------------------------
library(EZFragility)
library(Epoch)
library(ggplot2)
library(ggtext)
library(gsignal)

## -----------------------------------------------------------------------------
dl<-EpochDownloader(progress = FALSE)
names(dl)

## -----------------------------------------------------------------------------
pt01sz1 <- dl$FragilityData_subpt01_1
pt01sz1

pt26sz1 <- dl$FragilityData_subjh103_1
pt26sz1

## -----------------------------------------------------------------------------
butterworthFilter <- function(epoch, lowpass=0.5, highpass=150) {
  order <- 4

  sampling_freq <- metaData(epoch)$samplingRate
  nyquist_freq <- sampling_freq / 2

  normalized_freqs <- c(lowpass, highpass) / nyquist_freq
  
  filter_type <- "pass"
  butter_filter <- gsignal::butter(
    n = order,
    w = normalized_freqs,
    type = filter_type)

  # Apply filter to epoch data
  mat <- tblData(epoch)

  # Apply zero-phase filtering (filtfilt) to each row
  filtered_data <- gsignal::filtfilt(
    filt = butter_filter,
    x = t(mat))

  filtered_data <- t(filtered_data)
  tblData(epoch) <- filtered_data

  epoch
}

## -----------------------------------------------------------------------------
pt01sz1Cropped <- pt01sz1 |>
    crop(start=-10, end=10) |>
    butterworthFilter()
pt01sz1Cropped

pt26sz1Cropped <- pt26sz1 |>
    crop(start=-10, end=10) |>
    butterworthFilter()
pt26sz1Cropped

## -----------------------------------------------------------------------------
# A helper function to visualize SOZ electrodes in red and non-SOZ in black
visualSOZ <- function(epoch, sozNames) {
  p <- plot(epoch, gap = 4, timeResolution = 512)

  elecColor <- rep("black", nrow(epoch))
  elecColor[rownames(epoch) %in% sozNames] <- 'red'
  elecColor <- rev(elecColor) # match the electrode order in the plot

  p +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1)+
    theme(axis.text.y = element_markdown(colour = elecColor))
}

## -----------------------------------------------------------------------------
pt01Subset <- c(
    "ATT1", "ATT2", "AD1", "AD2", "AD3", 
    "AD4", "PD1", "PD2", "PD3", "PD4", 
    "MLT1", "MLT2", "MLT3", "MLT4")
pt01sozName <- c(
    "ATT1", "ATT2", "AD1", "AD2", "AD3", 
    "AD4", "PD1", "PD2", "PD3", "PD4"
)
pt01sz1Reordered <- pt01sz1Cropped[pt01Subset, ]
visualSOZ(pt01sz1Reordered, pt01sozName)

## -----------------------------------------------------------------------------
pt26Subset <- c(
    "ABT1", "ABT2", "RAD1", "RAD2", "RAD3", "RAD4", "RAD5", "RAD6", 
    "RAD7", "RHD1", "RHD2", "RHD3", "RHD4", "RHD5", "RHD6", "RHD7",
    "RHD8", "RHD9", "RTG29", "RTG30", "RTG31", "RTG32", "RTG40",
    "RTG48")
pt26sozName <- c(
    "RAD1", "RAD2", "RAD3", "RAD4", "RAD5", "RAD6", "RAD7", "RHD1", 
    "RHD2", "RHD3", "RHD4", "RHD5", "RHD6", "RHD7", "RHD8", "RHD9",
    "RTG40", "RTG48")
pt26sz1Reordered <- pt26sz1Cropped[pt26Subset, ]
visualSOZ(pt26sz1Reordered, pt26sozName)

## -----------------------------------------------------------------------------
library(doSNOW)
# compute fragility

cl <- makeCluster(parallel::detectCores(), type = "SOCK")
registerDoSNOW(cl)

windowNum <- 250
step <- 125
pt01sz1Frag <- calcAdjFrag(epoch = pt01sz1Cropped, window = windowNum, step = step, parallel = TRUE, nSearch=100L, progress = FALSE)

pt26sz1Frag <- calcAdjFrag(epoch = pt26sz1Cropped, window = windowNum, step = step, parallel = TRUE, nSearch=100L, progress = FALSE)

# Stop the parallel backend
stopCluster(cl)

## -----------------------------------------------------------------------------
# A helper function to visualize fragility heatmap with SOZ electrodes in red and non-SOZ in black
fragHeatmap <- function(frag, sozNames, ranked=FALSE) {
  startTimes <- frag$startTimes

  indexsz <- which(abs(startTimes)<=0.01)
  elecColor <- rep("black", length(frag$electrodes))
  elecColor[frag$electrodes%in% sozNames] <- 'red'
  elecColor <- rev(elecColor)

  plot(frag, ranked=ranked) +
    geom_vline(xintercept = indexsz, color = "black", linetype = "dashed", linewidth = 1) +
    theme(
      axis.text.y = element_markdown(colour = elecColor)
    )
}

## -----------------------------------------------------------------------------
pt01sz1FragReordered <- pt01sz1Frag[pt01Subset]
fragHeatmap(pt01sz1FragReordered, pt01sozName)

## -----------------------------------------------------------------------------
pt26sz1FragReordered <- pt26sz1Frag[pt26Subset]
fragHeatmap(pt26sz1FragReordered, pt26sozName)

## -----------------------------------------------------------------------------
# A helper function to plot fragility distribution for SOZ and non-SOZ groups
fragDist <- function(frag, sozNames) {
    timeWindows <- frag$startTimes
    timeIdx <- which(timeWindows >= -5 & timeWindows <= 10)
    frag <- frag[, timeIdx]
    plotFragDistribution(frag = frag, groupIndex = sozNames, bandType="SEM", rollingWindow = 1) +
        geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1)
}

## -----------------------------------------------------------------------------
fragDist(pt01sz1Frag[pt01Subset], pt01sozName)

## -----------------------------------------------------------------------------
fragDist(pt26sz1Frag[pt26Subset], pt26sozName)

## -----------------------------------------------------------------------------
plotFragQuantile(pt01sz1Frag[pt01Subset])

## -----------------------------------------------------------------------------
plotFragQuantile(pt26sz1Frag[pt26Subset])

## -----------------------------------------------------------------------------
library(doSNOW)
cl <- makeCluster(parallel::detectCores(), type = "SOCK")
registerDoSNOW(cl)

windowNum <- 250
step <- 125

pt01sz1Frag_lambda1 <- calcAdjFrag(epoch = pt01sz1Cropped, window = windowNum, step = step, parallel = TRUE, nSearch=100L, progress = FALSE, lambda=1e-4)


pt01sz1Frag_lambda2 <- calcAdjFrag(epoch = pt01sz1Cropped, window = windowNum, step = step, parallel = TRUE, nSearch=100L, progress = FALSE, lambda=1e-3)

pt01sz1Frag_lambda3 <- calcAdjFrag(epoch = pt01sz1Cropped, window = windowNum, step = step, parallel = TRUE, nSearch=100L, progress = FALSE, lambda=1e-2)


# Stop the parallel backend
stopCluster(cl)

## -----------------------------------------------------------------------------
fragHeatmap(pt01sz1FragReordered, pt01sozName, ranked=FALSE) + ggtitle("λ = Auto-selected")
fragHeatmap(pt01sz1Frag_lambda1[pt01Subset], pt01sozName, ranked=FALSE) + ggtitle("λ = 1e-4")
fragHeatmap(pt01sz1Frag_lambda2[pt01Subset], pt01sozName, ranked=FALSE) + ggtitle("λ = 1e-3")
fragHeatmap(pt01sz1Frag_lambda3[pt01Subset], pt01sozName, ranked=FALSE) + ggtitle("λ = 1e-2")

