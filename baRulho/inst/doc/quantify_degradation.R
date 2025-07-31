params <-
list(EVAL = FALSE)

## ----eval= TRUE, echo=FALSE-----------------------------------------------------------------------

library(knitr)

# create custom printing method
.print_df <- function(x, highlight = NULL, height = "300px", row.names = FALSE, ...) {
  kbl <- kableExtra::kable(
    as.data.frame(x)
    ,
    align = "c",
    row.names = row.names,
    format = "html",
    escape = F
  )
  
  if (!is.null(highlight))
    kbl <-
      kableExtra::column_spec(
        kbl,
        column = which(names(x) %in% highlight),
        background = "#ccebff",
        bold = TRUE
      )
  
  kbl <-
    kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)
  
  
  kbl <-
    kableExtra::scroll_box(kbl, width = "100%", height = height)
  
  asis_output(kbl)
}

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

## ----eval=FALSE-----------------------------------------------------------------------------------
# 
# # load packages
# library(baRulho)
# library(viridis)
# library(ggplot2)
# 
# # load example data
# data("test_sounds_est")
# 
# test_sounds_est

## ----eval = TRUE, echo = FALSE, message=FALSE-----------------------------------------------------

# load packages
library(baRulho)
library(viridis)
library(ggplot2)

# load example data
data("test_sounds_est")


.print_df(test_sounds_est, highlight = c("sound.id", "transect", "distance"))


## ----eval=TRUE------------------------------------------------------------------------------------

# count selection per recordings
unique(test_sounds_est$sound.files)


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# table(test_sounds_est$sound.id, test_sounds_est$distance)
# 

## ----eval = TRUE, echo=FALSE----------------------------------------------------------------------

tb <- as.data.frame.matrix(table(test_sounds_est$sound.id, test_sounds_est$distance))

.print_df(tb, height = NULL, row.names = TRUE)


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# # add reference column
# test_sounds_est <- set_reference_sounds(test_sounds_est, method = 1)
# 
# # print
# test_sounds_est

## ----echo = FALSE---------------------------------------------------------------------------------

# add reference column
test_sounds_est <- set_reference_sounds(test_sounds_est, method = 1)

.print_df(test_sounds_est, highlight = c("reference"))


## ----eval = TRUE----------------------------------------------------------------------------------
# sort to order panels
test_sounds_est <-
  test_sounds_est[order(test_sounds_est$sound.id,
                        test_sounds_est$transect,
                        decreasing = FALSE),]

# create plots
degrad_imgs <- plot_degradation(test_sounds_est, dest.path = tempdir())

## ----echo = FALSE---------------------------------------------------------------------------------

# try to copy files to vignettes
# fc <- file.copy(from = aligned_imgs[1:2], 
#           to = file.path("../man/figures", basename(aligned_imgs[c(1, 4)])))

fc <- file.copy(from = degrad_imgs[1], 
          to = file.path("../vignettes", basename(degrad_imgs[1])), overwrite = TRUE)


## ----eval = TRUE----------------------------------------------------------------------------------

degrad_imgs


## ----echo=FALSE-----------------------------------------------------------------------------------

knitr::include_graphics(basename(degrad_imgs[1]))


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# # run blur ratio
# br <- blur_ratio(X = test_sounds_est)
# 

## ----eval = TRUE, echo = FALSE--------------------------------------------------------------------

# run blur ratio
br <- blur_ratio(test_sounds_est)


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# # see output
# br

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(br, highlight = c("blur.ratio"))


## ----eval = TRUE----------------------------------------------------------------------------------
# plot blur ratio
blur_imgs <- plot_blur_ratio(X = test_sounds_est, dest.path = tempdir())

## ----eval = TRUE----------------------------------------------------------------------------------

head(blur_imgs)


## ----echo = FALSE---------------------------------------------------------------------------------

# try to copy files to vignettes
# fc <- file.copy(from = aligned_imgs[1:2], 
#           to = file.path("../man/figures", basename(aligned_imgs[c(1, 4)])))

fc <- file.copy(from = blur_imgs[1],
          to = file.path("../vignettes", basename(blur_imgs[1])), overwrite = TRUE)


## ----echo=FALSE-----------------------------------------------------------------------------------

# knitr::include_graphics("../vignettes/temp.img.jpeg")
knitr::include_graphics(basename(blur_imgs[1]))


## ----echo=FALSE-----------------------------------------------------------------------------------

# unlink("../vignettes/temp.img.jpeg")


## -------------------------------------------------------------------------------------------------

# get envelopes
br <- blur_ratio(X = test_sounds_est, envelopes = TRUE)
envs <- attributes(br)$envelopes

# make distance a factor for plotting
envs$distance <- as.factor(envs$distance)

# plot
ggplot(envs, aes(x = time, y = amp, col = distance)) +
  geom_line() + facet_wrap( ~ sound.id) +
  scale_color_viridis_d(alpha = 0.7) +
  labs(x = "Time (s)", y = "Amplitude (PMF)") +
  theme_classic()


## -------------------------------------------------------------------------------------------------

# get envelopes
br <- blur_ratio(X = test_sounds_est, envelopes = TRUE, env.smooth = 800)
envs <- attributes(br)$envelopes

envs$distance <- as.factor(envs$distance)

ggplot(envs, aes(x = time, y = amp, col = distance)) +
  geom_line() +
  facet_wrap( ~ sound.id) +
  scale_color_viridis_d(alpha = 0.7) +
  labs(x = "Time (s)", y = "Amplitude (PMF)") +
  theme_classic()


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# # run Spectrum blur ratio
# sbr <- spectrum_blur_ratio(test_sounds_est)
# 

## ----eval = TRUE, echo = FALSE--------------------------------------------------------------------

# run Spectrum blur ratio
sbr <- spectrum_blur_ratio(test_sounds_est)


## ----eval = FALSE, echo = FALSE-------------------------------------------------------------------
# 
# sbr <- spectrum_blur_ratio(test_sounds_est)
# 
# # make the gif here
# # https://ezgif.com

## ----eval = FALSE---------------------------------------------------------------------------------
# 
# # see output
# sbr

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(sbr, highlight = c("spectrum.blur.ratio"))

## -------------------------------------------------------------------------------------------------
sbr <- spectrum_blur_ratio(X = test_sounds_est, spectra = TRUE)

spctr <- attributes(sbr)$spectra

spctr$distance <- as.factor(spctr$distance)

ggplot(spctr[spctr$freq > 0.3,], aes(y = amp, x = freq, col = distance)) +
  geom_line() +
  facet_wrap( ~ sound.id) +
  scale_color_viridis_d(alpha = 0.7) +
  labs(x = "Frequency (kHz)", y = "Amplitude (PMF)") +
  coord_flip() +
  theme_classic()


## ----eval = TRUE----------------------------------------------------------------------------------

# run  envelope correlation
ea <- excess_attenuation(test_sounds_est)


## ----eval = FALSE---------------------------------------------------------------------------------
# # print output
# ea

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(ea, highlight = c("excess.attenuation"))


## ----eval = TRUE----------------------------------------------------------------------------------

# run  envelope correlation
ec <- envelope_correlation(test_sounds_est)


## ----eval = FALSE---------------------------------------------------------------------------------
# # print output
# ec

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(ea, highlight = c("envelope.correlation"))


## ----eval = TRUE----------------------------------------------------------------------------------

# run spectrum correlation
sc <- spectrum_correlation(test_sounds_est)


## ----eval = FALSE---------------------------------------------------------------------------------
# # print output
# sc

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(sc, highlight = c("spectrum.correlation"))


## ----eval = TRUE----------------------------------------------------------------------------------

# run signal to noise ratio
snr <-
  signal_to_noise_ratio(test_sounds_est,
                        pb = FALSE,
                        noise.ref = "custom",
                        mar = 0.1)


## ----eval = FALSE---------------------------------------------------------------------------------
# # print output
# snr

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(snr, highlight = c("signal.to.noise.ratio"))


## ----eval = TRUE----------------------------------------------------------------------------------

# run tail to signal ratio
tsr <- tail_to_signal_ratio(test_sounds_est, tsr.formula = 1, mar = 0.05)


## ----eval = FALSE---------------------------------------------------------------------------------
# # print output
# 
# tsr

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(tsr, highlight = c("tail.to.signal.ratio"))


## ----eval = TRUE----------------------------------------------------------------------------------

# run spcc
sc <- spcc(X = test_sounds_est, wl = 512)


## ----eval = FALSE---------------------------------------------------------------------------------
# # print output
# sc

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(sc, highlight = c("cross.correlation"))


## ----eval = TRUE----------------------------------------------------------------------------------

# run noise profile
np <-
  noise_profile(X = test_sounds_est[test_sounds_est$distance > 5,], mar = 0.05)

## ----eval = FALSE---------------------------------------------------------------------------------
# # print output
# head(np, 20)

## ----echo = FALSE---------------------------------------------------------------------------------

.print_df(np[1:20, ], highlight = c("freq", "amp"))


## ----eval = TRUE----------------------------------------------------------------------------------

ggplot(np, aes(y = amp, x = freq, col = sound.files)) +
  geom_line(linewidth = 1.4) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, alpha = 0.5) +
  labs(x = "Frequency (kHz)", y = "Amplitude (dBA)") +
  coord_flip() +
  theme_classic()

## ----eval = TRUE, warning = FALSE-----------------------------------------------------------------

np <-
  noise_profile(X = test_sounds_est[test_sounds_est$distance > 5, ],
                mar = 0.1, averaged = FALSE)

# make a column containing sound file and selection
np$sf.sl <- paste(np$sound.files, np$selec)

ggplot(np, aes(
  y = amp,
  x = freq,
  col = sound.files,
  group = sf.sl
)) +
  geom_line(linewidth = 1.4) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, alpha = 0.5) +
  labs(x = "Frequency (kHz)", y = "Amplitude (dBA)") +
  coord_flip() +
  theme_classic()


## ----eval = TRUE----------------------------------------------------------------------------------

np <- noise_profile(
  X = test_sounds_est[test_sounds_est$distance > 5,],
  mar = 0.05,
  bp = c(0, 10),
  averaged = FALSE,
  hop.size = 3
)

# make a column containing sound file and selection
np$sf.sl <- paste(np$sound.files, np$selec)

ggplot(np, aes(
  y = amp,
  x = freq,
  col = sound.files,
  group = sf.sl
)) +
  geom_line(linewidth = 1.4) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, alpha = 0.5) +
  labs(x = "Frequency (kHz)", y = "Amplitude (dBA)") +
  coord_flip() +
  theme_classic()


## ----session info, echo=F-------------------------------------------------------------------------

sessionInfo()

