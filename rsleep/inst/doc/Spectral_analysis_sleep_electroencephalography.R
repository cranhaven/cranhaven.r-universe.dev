## ----env, include = FALSE-----------------------------------------------------

options(scipen=999)

options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")


## ----edf_read, echo=FALSE-----------------------------------------------------
library(edfReader)

fname <- "15012016HD.edf"
if(!file.exists(fname)){
  download.file(
  url = "https://rsleep.org/data/15012016HD.edf",
  destfile = fname,
  method = "curl")}

h <- readEdfHeader(fname)

s <- readEdfSignals(h, signals = "C3-M2")

## ----spectrogram, echo=FALSE, fig.width=7-------------------------------------
library(rsleep)

spectrogram(
  signal = s$signal,
  sRate = s$sRate,
  startTime = s$startTime)

## ----hypnogram, echo=FALSE, fig.width = 7-------------------------------------
if(!file.exists("15012016HD.csv")){
  download.file(
  url = "https://rsleep.org/data/15012016HD.csv",
  destfile = "15012016HD.csv",
  method="curl")}

events <- read_events_noxturnal("15012016HD.csv")

# Remove last epoch as signal stops before.
events <- head(events, -1)

# Remove other events
events <- events[events$event %in% c("AWA", "REM", "N1", "N2", "N3"),]

plot_hypnogram(events)

## ----epoching-----------------------------------------------------------------
epochs <- epochs(
  signals = s$signal,
  sRates = s$sRate,
  epoch = events,
  startTime = as.numeric(as.POSIXct(h$startTime)))

## ----pwelch, fig.width=7, message=FALSE, error=FALSE--------------------------
p <- pwelch(epochs[[120]], sRate = s$sRate)

summary(p)

## ----avg_pdg_compute----------------------------------------------------------
periodograms <- mapply(
  x = epochs, 
  y = events$event,
  FUN = function(x,y){
    p <- pwelch(x, sRate = s$sRate, show = FALSE)
    p <- as.data.frame(p[p$hz <= 30,])
    p$stage <- y
    p
}, SIMPLIFY = F)

## ----pdg_rbind----------------------------------------------------------------
periodograms_df <- do.call("rbind", periodograms)

## ----pdg_aggregate------------------------------------------------------------
avg_periodograms <- aggregate(psd ~ hz+stage, periodograms_df, mean)

## ----periodogram_plot, fig.width=7, message=FALSE, error=FALSE----------------
library(ggplot2)

palette <- c("#5BBCD6","#00A08A","#F2AD00","#F98400","#FF0000")

ggplot(avg_periodograms, aes(x=hz,y=psd,color=stage)) +
  geom_line() + theme_bw() +
  theme(legend.title = element_blank()) + 
  scale_colour_manual(name = "stage",
                      values = palette) +
  xlab("Frequency (Hertz)") + ylab("PSD")

## ----bands_compute------------------------------------------------------------
bands <- lapply(epochs,function(x){
    bands_psd(
      bands = list(c(0.5,3.5), # Delta
                   c(3.5,7.5), # Theta
                   c(7.5,13), # Alpha
                   c(13,30)), # Beta
      signal = x, sRate = s$sRate)
})

## ----bands_reshape------------------------------------------------------------
bands_df <- data.frame(matrix(unlist(bands), nrow=length(bands), byrow=TRUE))

colnames(bands_df) <- c("Delta","Theta","Alpha","Beta")

## ----bands_stages-------------------------------------------------------------
bands_df$stage <- rsleep::hypnogram(events)$event

## ----bands_plot, fig.width=7, fig.height=7, message=FALSE, error=FALSE--------
bands_df_long <- reshape2::melt(bands_df, "stage")

palette <-c("#F98400", "#F2AD00", "#00A08A", "#FF0000", "#5BBCD6")

ggplot(bands_df_long,
       aes(x=stage,y=value,color=stage)) +
  geom_boxplot() +
  facet_grid(rows = vars(variable),scales = "free") +
  scale_colour_manual(name = "stage",
                      values = palette) +
  theme_bw() + xlab("") + ylab("PSD") + 
  theme(legend.position = "none")

