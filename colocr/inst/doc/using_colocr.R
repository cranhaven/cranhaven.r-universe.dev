## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = 'center',
  message = FALSE
)

## ----getting_started, fig.width=7, fig.height=7-------------------------------
# load libraries
library(colocr)

# load images
fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
img <- image_load(fl)

# select ROI and show the results
par(mfrow = c(2,2), mar = rep(1, 4))

img %>%
  roi_select(threshold = 90) %>%
  roi_show()

## ----colocr_app, eval=FALSE---------------------------------------------------
#  colocr_app()

## ----call_roi_test------------------------------------------------------------
# calculate co-localization statistics
img %>%
  roi_select(threshold = 90) %>%
  roi_test(type = 'both')

## ----show_images, fig.width=7, fig.height=2.5---------------------------------
# load required libraries
library(colocr)

# load images
img <- image_load(system.file('extdata', 'Image0001_.jpg', package = 'colocr'))       # merge
img1 <- imager::channel(img, 1)  # red
img2 <- imager::channel(img, 2)  # green

# show images
par(mfrow = c(1,3), mar = rep(1,4))
plot(img, axes = FALSE, main = 'Merged')
plot(img1, axes = FALSE, main = 'RKIP')
plot(img2, axes = FALSE, main = 'LC3')

## ----workflow, echo=FALSE, out.width='300px'----------------------------------
workflow_fig <- system.file('colocr_app', 'workflow.png', package = 'colocr')
knitr::include_graphics(workflow_fig)

## ----call_roi_select----------------------------------------------------------
# select regions of interest
img_rois <- img %>%
  roi_select(threshold = 90)

## ----output_str---------------------------------------------------------------
# class of the returned object
class(img); class(img_rois)

# name of added attribut
names(attributes(img)); names(attributes(img_rois))

# str of labels
label <- attr(img_rois, 'label')
str(label)

# unique labels 
unique(label)

## ----call_roi_show, fig.width=7, fig.height=7---------------------------------
# select ROI and show the results
par(mfrow = c(2,2), mar = rep(1, 4))

img_rois %>%
  roi_show()

## ----call_coloc_show, fig.width=7, fig.height=3-------------------------------
# show the scatter and density of the pixel values
par(mfrow=c(1,2), mar = c(4,4,1,1))

img_rois %>%
  roi_check()

## ----colocr_app2, eval=FALSE--------------------------------------------------
#  # run the shiny app
#  colocr_app()

## ----app_screenshot, echo=FALSE, out.width="600px"----------------------------
# show the screen shot of the shiny app
app_ss <- system.file('colocr_app/tests/one_image-expected', '001.png', package = 'colocr')
knitr::include_graphics(app_ss)

## ----call_roi_test2-----------------------------------------------------------
# Calculate the co-localization statistics
tst <- img_rois %>%
  roi_test(type = 'both')
tst

## ----output_str2--------------------------------------------------------------
# str of the roi_test output
str(tst)

## ----roi_subset, fig.width=7, fig.height=7------------------------------------
# load image
img2 <- image_load(system.file('extdata', 'Image0003_.jpg', package = 'colocr'))       # merge

# select ROI and show the results
par(mfrow = c(2,2), mar = rep(1, 4))

img2 %>%
  roi_select(threshold = 85,
             shrink = 10,
             clean = 10,
             n = 3) %>%
  roi_show()

## ----analyze_collection-------------------------------------------------------
# make a list of images
fls <- c(system.file('extdata', 'Image0001_.jpg', package = 'colocr'),
         system.file('extdata', 'Image0003_.jpg', package = 'colocr'))
image_list <- image_load(fls)

# call roi_select on multiple images
image_list %>%
  roi_select(threshold = 90) %>%
  roi_test()

## ----analyze_collection2------------------------------------------------------
# make threshold input list
thresholds <- c(90, 95)

# call roi_select on multiple images and specific thresholds for each
image_list %>%
  roi_select(threshold = thresholds) %>%
  roi_test()

## ----read_output--------------------------------------------------------------
# show the output
stats <- read.csv(system.file('colocr_app', 'stats_19.05.23_05.55.21.csv', package = 'colocr'))

stats

## ----read_inputs--------------------------------------------------------------
# show the inputs
inputs <- read.csv(system.file('colocr_app', 'inputs_19.05.23_05.55.22.csv', package = 'colocr'), stringsAsFactors = FALSE)

inputs

## ----apply_inputs-------------------------------------------------------------
# read images
fls <- lapply(inputs$image, function(x) {
  system.file('extdata', x, package = 'colocr')
  })
imgs <- image_load(fls)

# use the app input to the roi_select function
rep_stats <- imgs %>%
  roi_select(threshold = inputs$threshold,
             shrink = inputs$shrink,
             grow = inputs$grow,
             fill = inputs$fill,
             clean = inputs$clean,
             tolerance = inputs$tolerance,
             n = inputs$roi_num) %>%
  roi_test(type = 'both')

rep_stats

## ----check_equal--------------------------------------------------------------
# check the app and the package output is equal
all.equal(round(stats$pcc, 2), round(c(rep_stats[[1]]$pcc, rep_stats[[2]]$pcc), 2))
all.equal(round(stats$moc, 2), round(c(rep_stats[[1]]$moc, rep_stats[[2]]$moc), 2))

## ----example1_roi, fig.height=4, fig.width=4----------------------------------
# load image
fl1 <- system.file('extdata', 'example1.png', package = 'colocr')
ex1 <- image_load(fl1)

# select and show regions of interest (based on the app)
par(mfrow=c(2,2), mar = rep(1, 4))

ex1 %>%
  roi_select(threshold = 90, shrink = 8, n = 50) %>%
  roi_show()

## ----example1_test------------------------------------------------------------
# calculate co-localization statistics
# (expected pcc = 0.68, moc = 0.83)
ex1 %>%
  roi_select(threshold = 90, shrink = 8, n = 50) %>%
  roi_test(type = 'both') %>%
  colMeans()

## ----example3_roi, fig.height=4, fig.width=4----------------------------------
# load image
fl3 <- system.file('extdata', 'example3.png', package = 'colocr')
ex3 <- image_load(fl3)

# select and show regioins of interest (based on the app)
par(mfrow=c(2,2), mar = rep(1, 4))

ex3 %>%
  roi_select(threshold = 90, shrink = 9, grow = 1, fill = 10, clean = 10, n = 20) %>%
  roi_show()

## ----example3_test------------------------------------------------------------
# calculate co-localization statistics
# (expected pcc = 0.61, moc = 0.71)
ex3 %>%
  roi_select(threshold = 90, shrink = 9, grow = 1, fill = 10, clean = 10, n = 20) %>%
  roi_test() %>%
  colMeans()

