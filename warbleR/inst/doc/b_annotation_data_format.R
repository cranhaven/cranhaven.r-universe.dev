params <-
list(EVAL = FALSE)

## ----annotations_2, echo = FALSE, message = FALSE-------------------------------------------------


rm(list = ls())

# unload all non-based packages
out <-
  sapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), function(x)
    try(detach(x, unload = FALSE, character.only = TRUE), silent = T)
  )

#load packages
library(warbleR)
library(knitr)
library(kableExtra)


# cf <- read.csv("./data/cuadro de funciones warbleR.csv", stringsAsFactors = FALSE)

# warbleR_options(wav.path = "./examples")

options(knitr.table.format = "html")

knitr::opts_chunk$set(
  comment = "",
  fig.width = 5, 
  fig.height = 3.5,
  dpi = 40,
  out.width = "80%"
)

options(width = 100, max.print = 100)

# avoid weird printing of selection tables
Sys.setenv(NO_COLOR = 1)

print_st_no_color <- function(x, ...) if (is(x, "selection_table")) warbleR:::print.selection_table(x, no.color = TRUE, ...) else
  warbleR:::print.extended_selection_table(x, no.color = TRUE, ...)


warbleR_options(pb = FALSE)

## ----echo = FALSE---------------------------------------------------------------------------------

options(digits = 3)

set.seed(123)

start <- runif(n = 4, min = 0.2, max = 10)

cd.anot <-
  data.frame(
    sound.files = rep(c("sound_file_1.wav", "sound_file_2.wav"), each = 2),
    selec = rep(1:2, 2),
    start,
    end =  start + abs(rnorm(n = 4, mean = 1))
  )


kbl <-
  knitr::kable(cd.anot,
               align = "c",
               row.names = F,
               format = "html")

kbl <-
  kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl


## ----echo = FALSE---------------------------------------------------------------------------------

cd.anot$bottom.freq <- rnorm(n = 4, mean = 5)
cd.anot$top.freq <- rnorm(n = 4, mean = 9)

cd.anot$channel <- rep(1, 4)

# cd.anot <- cd.anot[, c(1, 7, 2:6)]

kbl <-
  knitr::kable(cd.anot,
               align = "c",
               row.names = F,
               format = "html")


kbl <-
  kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl


## ----annotations_4.1, eval=FALSE------------------------------------------------------------------
# 
# library(warbleR)
# 
# data("lbh_selec_table")
# 
# 
# knitr::kable(lbh_selec_table)

## ----annotations_4.2, echo=FALSE------------------------------------------------------------------

kbl <-
  knitr::kable(
    lbh_selec_table,
    align = "c",
    row.names = F,
    format = "html"
  )

kbl <-
  kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## ----annotations_4.32, eval = FALSE---------------------------------------------------------------
# 
# # write example sound files in temporary directory
# writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
# writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
# writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
# writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
# 
# st <-
#   selection_table(X = lbh_selec_table, path = tempdir())
# 
# knitr::kable(st)

## ----eval = TRUE, echo = FALSE--------------------------------------------------------------------


writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# global parameters
st <-
  selection_table(X = lbh_selec_table, pb = FALSE, path = tempdir())

kbl <- knitr::kable(st)

kbl <-
  kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl


## -------------------------------------------------------------------------------------------------

class(st)


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# st
# 

## ----eval = TRUE, echo = FALSE, collapse = TRUE---------------------------------------------------

print_st_no_color(st)


## ----annotations_4.3, eval = FALSE----------------------------------------------------------------
# 
# est <- selection_table(
#   X = lbh_selec_table,
#   pb = FALSE,
#   extended = TRUE,
#   path = tempdir()
# )
# 

## ----annotations_4.33, eval = TRUE, echo = FALSE--------------------------------------------------

est <- selection_table(
  X = lbh_selec_table,
  pb = FALSE,
  extended = TRUE,
  path = tempdir()
)


## -------------------------------------------------------------------------------------------------

class(est)


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# est
# 

## ----eval = TRUE, echo = FALSE, collapse = TRUE---------------------------------------------------

print_st_no_color(est)


## ----annotations_5--------------------------------------------------------------------------------

is_extended_selection_table(est)

## ----annotations_6--------------------------------------------------------------------------------

est2 <- est[1:2, ]

is_extended_selection_table(est2)

## ----annotations_7, eval = FALSE------------------------------------------------------------------
# 
# ## print (equivalent to `print(est)`)
# est
# 

## ----annotations_7.05, eval = TRUE, echo = FALSE, collapse = TRUE---------------------------------

print_st_no_color(est)


## ----annotations_8, eval = FALSE------------------------------------------------------------------
# 
# est3 <- est[1:5, ]
# 
# est4 <- est[6:11, ]
# 
# est5 <- rbind(est3, est4)
# 
# # print
# est5

## ----annotations_8.1, echo=FALSE, collapse = TRUE-------------------------------------------------

est3 <- est[1:5, ]

est4 <- est[6:11, ]

est5 <- rbind(est3, est4)

# print
print_st_no_color(est5)

## ----annotations_8.2------------------------------------------------------------------------------

# same annotations
all.equal(est, est5, check.attributes = FALSE)

# same acoustic data
all.equal(attr(est, "wave.objects"), attr(est5, "wave.objects"))


## ----annotations_8.21-----------------------------------------------------------------------------

wv1 <- read_sound_file(X = est, index = 3, from = 0, to = 0.37)

## ----annotations_8.22, out.width= 750-------------------------------------------------------------

class(wv1)

wv1

# print spectrogram
seewave::spectro(
  wv1,
  wl = 150,
  grid = FALSE,
  scale = FALSE,
  ovlp = 90,
  palette = viridis::viridis,
  collevels = seq(-100, 0 , 5)
)

## ----annotations_8.23, out.width= 750-------------------------------------------------------------
par(mfrow = c(3, 2), mar = rep(0, 4))

for (i in 1:6) {
  wv <- read_sound_file(
    X = est,
    index = i,
    from = 0.05,
    to = 0.32
  )
  
  seewave::spectro(
    wv,
    wl = 150,
    grid = FALSE,
    scale = FALSE,
    axisX = FALSE,
    axisY = FALSE,
    ovlp = 90,
    palette = viridis::viridis,
    collevels = seq(-100, 0 , 5)
  )
}

## ----annotations_8.24-----------------------------------------------------------------------------

# create new data frame
Y <-
  data.frame(
    sound.files = est$sound.files,
    site = "La Selva",
    lek = c(rep("SUR", 5), rep("CCL", 6))
  )

# combine
mrg_est <- merge(est, Y, by = "sound.files")

# check class
is_extended_selection_table(mrg_est)

## ----annotations_8.25-----------------------------------------------------------------------------

# fix est
mrg_est <- fix_extended_selection_table(X = mrg_est, Y = est)

# check class
is_extended_selection_table(mrg_est)

## ----annotations_12.1, eval=FALSE-----------------------------------------------------------------
# 
# #  parametros espectrales
# sp <- spectro_analysis(est)
# 
# # check first 10 columns
# sp[, 1:10]

## ----eval = TRUE, echo = FALSE, message=FALSE-----------------------------------------------------

sp <- spectro_analysis(est, pb = FALSE)

kbl <- knitr::kable(sp[, 1:10])

kbl <-
  kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl

## -------------------------------------------------------------------------------------------------

length(attr(est, "wave.objects")) == length(unique(paste(est$sound.files)))


## ----songs 3, echo = FALSE, fig.align= "left", out.width="100%"-----------------------------------

knitr::include_graphics("Phaethornis-eurynome-15607-labeled.jpeg")


## ----warning=FALSE, message=FALSE-----------------------------------------------------------------

# load data
data("sth_annotations")

# download sound file from Xeno-Canto using catalog id
out <-
  query_xc(qword = "nr:15607",
           download = TRUE,
           path = tempdir())

# check file is found in temporary directory
list.files(path = tempdir(), "mp3")


## -------------------------------------------------------------------------------------------------

# load  Scale-throated Hermit example annotations
data("sth_annotations")


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# # print into the console
# head(sth_annotations)
# 

## ----echo=FALSE-----------------------------------------------------------------------------------

kbl <- knitr::kable(head(sth_annotations))

kbl <-  kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl


## ----eval = FALSE---------------------------------------------------------------------------------
# 
# # create by song extended selection table
# bs_est <-
#   selection_table(X = sth_annotations,
#                   extended = TRUE,
#                   by.song = "song",
#                   path = tempdir())
# 

## ----eval = FALSE---------------------------------------------------------------------------------
# 
# length(attr(bs_est, "wave.objects")) == length(unique(paste(bs_est$sound.files, bs_est$song)))
# 

## ----echo = FALSE---------------------------------------------------------------------------------

print(TRUE)


## ----eval = FALSE---------------------------------------------------------------------------------
# # extract wave object
# wave_song1 <-
#   read_sound_file(
#     X = bs_est,
#     index = 1,
#     from = 0,
#     to = Inf
#   )
# 
# # plot spectro
# seewave::spectro(
#   wave_song1,
#   wl = 150,
#   grid = FALSE,
#   scale = FALSE,
#   ovlp = 90,
#   palette = viridis::viridis,
#   collevels = seq(-100, 0 , 5),
#   flim = c(1, 12)
# )
# 

## ----by song est spectro, echo = FALSE, fig.align= "left", out.width="100%"-----------------------

knitr::include_graphics("by_song.png")


## ----annotations_13, eval = FALSE-----------------------------------------------------------------
# 
# # create long selection table
# lng.selec.table <- do.call(rbind, replicate(10, lbh_selec_table,
#                                             simplify = FALSE))
# 
# # relabels selec
# lng.selec.table$selec <- 1:nrow(lng.selec.table)
# 
# # create extended selection table
# lng_est <- selection_table(X = lng.selec.table,
#                            pb = FALSE,
#                            extended = TRUE)
# 
# 
# # load packages
# library(microbenchmark)
# library(ggplot2)
# 
# # check performance
# mbmrk.snr <- microbenchmark(
#   extended = sig2noise(lng_est,
#                        mar = 0.05),
#   regular = sig2noise(lng.selec.table,
#                       mar = 0.05),
#   times = 50
# )
# 
# autoplot(mbmrk.snr) + ggtitle("sig2noise")

## ----downloading rds, eval = FALSE----------------------------------------------------------------
# 
# URL <- "https://figshare.com/ndownloader/files/21167052"
# 
# options(timeout = max(300, getOption("timeout")))
# 
# download.file(
#   url = URL,
#   destfile = file.path(tempdir(), "est_inquiry.RDS"),
#   method = "auto"
# )
# 
# est <- readRDS(file.path(tempdir(), "est_inquiry.RDS"))
# 
# nrow(est)
# 

## ----eval = TRUE, echo = FALSE--------------------------------------------------------------------

print(336)


## ----eval = FALSE, out.width = 750----------------------------------------------------------------
# 
# par(mfrow = c(3, 2), mar = rep(0, 4))
# 
# for (i in 1:6) {
#   wv <- read_sound_file(
#     X = est,
#     index = i,
#     from = 0.05,
#     to = 0.17
#   )
# 
#   spectro(
#     wv,
#     grid = FALSE,
#     scale = FALSE,
#     axisX = FALSE,
#     axisY = FALSE,
#     ovlp = 90,
#     flim = c(10, 50),
#     palette = viridis::viridis,
#     collevels = seq(-100, 0 , 5)
#     )
# }
# 

## ----eval = FALSE---------------------------------------------------------------------------------
# 
# xcorr_inquiry <- cross_correlation(est[1:4, ])
# 
# xcorr_inquiry

## ----echo=FALSE-----------------------------------------------------------------------------------

xcorr_inquiry <- matrix(c(1.0000000, 0.5222115, 0.5350263, 0.5939756,
  0.5222115, 1.0000000, 0.8692543, 0.6599669,
  0.5350263, 0.8692543, 1.0000000, 0.8334820,
  0.5939756, 0.6599669, 0.8334820, 1.0000000), nrow = 4, dimnames = list(c("T2018-01-04_11-37-50_0000010.wav_1-1",  "T2018-01-04_11-37-50_0000010.wav_10-1",  "T2018-01-04_11-37-50_0000010.wav_11-1", "T2018-01-04_11-37-50_0000010.wav_12-1"), c("T2018-01-04_11-37-50_0000010.wav_1-1",  "T2018-01-04_11-37-50_0000010.wav_10-1",  "T2018-01-04_11-37-50_0000010.wav_11-1", "T2018-01-04_11-37-50_0000010.wav_12-1")))

kbl <- knitr::kable(xcorr_inquiry, row.names = TRUE)

kbl <-  kableExtra::kable_styling(kbl, bootstrap_options = "striped", font_size = 14)

kbl


