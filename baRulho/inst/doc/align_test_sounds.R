params <-
list(EVAL = FALSE)

## ----setup, eval = TRUE, echo = FALSE, message=FALSE----------------------------------------------

library(knitr)

# create custom printing method
.print_df <- function(x, highlight = NULL, ...) {
  kbl <- kableExtra::kable(
    head(as.data.frame(x)),
    align = "c",
    row.names = F,
    format = "html",
    escape = F
  )
  
  if (!is.null(highlight))
  kbl <- column_spec(kbl, column = which(names(x) %in% highlight), background = "#ccebff", bold = TRUE)
  
  kbl <-
    kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

  
  kbl <-
    kableExtra::scroll_box(kbl, width = "100%", height = "300px")
  
  asis_output(kbl)
}

# register data frame printing method
registerS3method("knit_print", "data.frame", .print_df)

# global option chunks
knitr::opts_chunk$set(
  fig.width = 5, 
  fig.height = 3.5,
  dpi = 70,
  comment = "",
  out.width = "80%",
  fig.align = "center",
  message = TRUE,
  warning = TRUE
)

options(width = 100, max.print = 100)


## ----eval = TRUE----------------------------------------------------------------------------------

# load packages
library(viridis)
library(baRulho)
library(Rraven)

# synthesize
synth_est <- baRulho::synth_sounds(
  mar = 0.1,
  frequencies = c(0.5, 1:5),
  durations = 0.1,
  fm = FALSE,
  am = FALSE,
  sampling.rate = 12
)

# convert into a single wave object
synth_wav <- Rraven::exp_est(X = synth_est,
                             single.file = TRUE,
                             wave.object = TRUE)


# plot spectro
seewave::spectro(
  wave = synth_wav,
  scale = FALSE,
  palette = viridis,
  grid = FALSE,
  collevels = seq(-20, 0, 1),
  osc = TRUE,
  colwave = "#31688EB3",
  heights = c(2, 1),
  wl = 100
)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------

# synthesize
synth_est2 <- baRulho::synth_sounds(
  mar = 0.01,
  sig2 = 0.05,
  frequencies = 2:4,
  durations = c(0.1, 0.2),
  fm = TRUE,
  am = TRUE,
  shuffle = TRUE,
  seed = 123,
  sampling.rate = 12, 
  freq.range = 1
)

# convert into a single wave object
synth_wav2 <- Rraven::exp_est(
  X = synth_est2,
  single.file = TRUE,
  path = tempdir(),
  wave.object = TRUE
)

# plot spectro
seewave::spectro(
  synth_wav2,
  tlim = c(0, 2),
  scale = FALSE,
  palette = viridis,
  grid = FALSE,
  collevels = seq(-20, 0, 1),
  osc = TRUE,
  colwave = "#31688EB3",
  heights = c(2, 1),
  wl = 140
)


## ----eval = TRUE----------------------------------------------------------------------------------
# check first 6 rows
head(as.data.frame(synth_est2))

## ----eval = TRUE----------------------------------------------------------------------------------

# check name of wave objects
names(attributes(synth_est2)$wave.objects)


## ----master sound file, eval = TRUE, echo = TRUE, fig.show='hide'---------------------------------

# create master sound file
synth_master_annotations <- baRulho::master_sound_file(
  X = synth_est,
  file.name = "synthetic_master",
  dest.path = tempdir(),
  gap.duration = 0.15
)


## ----spectro master 1, eval = TRUE----------------------------------------------------------------

# read wave file
wave <-
  tuneR::readWave(file.path(tempdir(), "synthetic_master.wav"))

# plot spectrogram
seewave::spectro(
  wave,
  scale = FALSE,
  palette = viridis,
  wl = 150,
  grid = FALSE,
  flim = c(0, 4.7)
)


## ----eval = TRUE, echo = TRUE, fig.show='hide'----------------------------------------------------

# load example data from warbleR
data(list = c(
  "Phae.long1",
  "Phae.long2",
  "Phae.long3",
  "Phae.long4",
  "lbh_selec_table"
))

# save sound files to temporary folder
tuneR::writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
tuneR::writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
tuneR::writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
tuneR::writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# make an extended selection table
est <- warbleR::selection_table(
  X = lbh_selec_table,
  extended = TRUE,
  path = tempdir(),
  pb = FALSE
)

# add sound_id column
est$sound.id <- paste0(abbreviate(est$sound.files), est$selec)

# create master sound file
master_annotations <- baRulho::master_sound_file(
  X = est,
  file.name = "example_master",
  dest.path = tempdir(),
  gap.duration = 0.3
)

## ----spectro master 2, eval = TRUE----------------------------------------------------------------

# read wave file
wave <-
  tuneR::readWave(file.path(tempdir(), "example_master.wav"))

# plot spectrogram
seewave::spectro(
  wave,
  scale = FALSE,
  palette = viridis,
  collevels = seq(-120, 0, 5),
  wl = 500,
  grid = FALSE,
  flim = c(0, 10)
)


## ----eval = TRUE----------------------------------------------------------------------------------

Rraven::exp_raven(master_annotations, path = tempdir(),
                  file.name = "example_master_selection_table")

## -------------------------------------------------------------------------------------------------
knitr::include_graphics("example_master_table.jpg")

## ----eval = TRUE----------------------------------------------------------------------------------

data("master_est")

data("test_sounds_est")


## ----eval = TRUE----------------------------------------------------------------------------------

unique(master_est$sound.files)
unique(test_sounds_est$sound.files)


## -------------------------------------------------------------------------------------------------


# first remove any other wave file in the temporary working directory (dont do it with your data!)
unlink(list.files(
  path = tempdir(),
  full.names = TRUE,
  pattern = ".wav"
))

# save master sound file
tuneR::writeWave(object = attr(master_est, "wave.objects")[[1]],
          file.path(tempdir(), "master.wav"))

# save test sound files
for (i in unique(test_sounds_est$sound.files)) {
  tuneR::writeWave(object = attr(test_sounds_est, "wave.objects")[[i]], file.path(tempdir(), i))
}

# make annotations a data frame
master_annotations <- as.data.frame(master_est)

## -------------------------------------------------------------------------------------------------
master_annotations

## ----eval = TRUE----------------------------------------------------------------------------------

markers_position <-
  baRulho::find_markers(X = master_annotations, path = tempdir())

markers_position

## ----eval = TRUE----------------------------------------------------------------------------------

# lower window length
markers_position <-
  baRulho::find_markers(X = master_annotations,
                        hop.size = 4,
                        path = tempdir())

markers_position


## ----eval = TRUE----------------------------------------------------------------------------------

aligned_tests <-
  baRulho::align_test_files(
    X = master_annotations,
    Y = markers_position,
    by.song = TRUE,
    remove.markers = FALSE,
    path = tempdir()
  )

## ----eval = TRUE----------------------------------------------------------------------------------

is.data.frame(aligned_tests)

aligned_tests

## ----eval = TRUE----------------------------------------------------------------------------------

aligned_imgs <- baRulho::plot_aligned_sounds(
  X = aligned_tests,
  path = tempdir(),
  dest.path = tempdir(),
  duration = 2.4,
  ovlp = 0
)

aligned_imgs


## ----echo = FALSE---------------------------------------------------------------------------------

# try to copy files to man/figures
# fc <- file.copy(from = aligned_imgs[1:2], 
#           to = file.path("../man/figures", basename(aligned_imgs[c(1, 4)])))

fc <- file.copy(from = aligned_imgs[c(1, 4)], 
          to = file.path("../vignettes", basename(aligned_imgs[c(1, 4)])),overwrite = TRUE)


## ----echo=FALSE-----------------------------------------------------------------------------------

knitr::include_graphics(basename(aligned_imgs[1]))

knitr::include_graphics(basename(aligned_imgs[4]))


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# baRulho::manual_realign(
#   X = aligned_tests,
#   Y = master_annotations,
#   duration = 2.4,
#   path = tempdir()
# )
# 

## ----eval = TRUE, echo=FALSE----------------------------------------------------------------------

knitr::include_graphics("manual_realign.gif")


## ----eval = TRUE----------------------------------------------------------------------------------

  data("test_sounds_est")
  data("master_est")
  
  # create "unaligned_test_sounds_est" by
  # adding error to "test_sounds_est" start and end
  unaligned_test_sounds_est <- test_sounds_est
  set.seed(123)
  noise_time <- sample(c(0.009, -0.01, 0.03, -0.03, 0, 0.07, -0.007),
  nrow(unaligned_test_sounds_est),
  replace = TRUE)
  
  attr(unaligned_test_sounds_est, "check.res")$start <- 
  unaligned_test_sounds_est$start <- 
  unaligned_test_sounds_est$start + noise_time
  attr(unaligned_test_sounds_est, "check.res")$end <- 
  unaligned_test_sounds_est$end  <- 
  unaligned_test_sounds_est$end + noise_time
  

## ----eval = TRUE----------------------------------------------------------------------------------

#re align
rts <- baRulho::auto_realign(X = unaligned_test_sounds_est, Y = master_est)


## ----eval = FALSE, echo = FALSE-------------------------------------------------------------------
# 
# # thsi code creates the images use to create the gif shown below
# # rename sound files so aligned and unaligned sounds are intercalated
# unalg <-
#   warbleR::rename_waves_est(
#     playback_est_unaligned,
#     playback_est_unaligned$sound.files,
#     new.selec = seq(1, 200, by = 2)[1:nrow(playback_est_unaligned)]
#   )
# alg <-
#   warbleR::rename_waves_est(playback_est_aligned,
#                    playback_est_aligned$sound.files,
#                    new.selec = seq(2, 200, by = 2)[1:nrow(playback_est_aligned)])
# 
# # add label
# unalg$type <- "Before aligning"
# alg$type <- "After aligning"
# 
# # put together in a single ext sel tab
# unalg.alg <- rbind(unalg, alg)
# 
# # create spectrograms
# warbleR::spectrograms(
#   unalg.alg[unalg.alg$sound.id != "ambient",],
#   dest.path = tempdir(),
#   res = 100,
#   wl = 300,
#   title.labels = "type",
#   sel.labels = NULL
# )

## ----session info, echo=F-------------------------------------------------------------------------

sessionInfo()


