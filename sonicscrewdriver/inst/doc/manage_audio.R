## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(sonicscrewdriver)

## -----------------------------------------------------------------------------
filename <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
w <- readAudio(filename)

## -----------------------------------------------------------------------------
drawWindow <- function( wave, start, window.length) {
  rect(start, -1, start+window.length, 1, col= rgb(0,0,1.0,alpha=0.5))
}


## ----results='hide'-----------------------------------------------------------
# Create a 5 second sine wave of 1Hz
w <- tuneR::sine(1, duration=5*44100)

plot(w@left, type="l", xlab="Time (samples)", ylab="Amplitude")

windowing(w, window.length=44100, window.overlap = 0, FUN=drawWindow)

## ----results='hide'-----------------------------------------------------------
plot(w@left, type="l", xlab="Time (samples)", ylab="Amplitude")

windowing(w, window.length=44100, window.overlap = 44100/2, FUN=drawWindow)

## ----results='hide'-----------------------------------------------------------
plot(w@left, type="l", xlab="Time (samples)", ylab="Amplitude")

windowing(w, window.length=44100, window.overlap = -44100, FUN=drawWindow)

## ----results='hide'-----------------------------------------------------------
w <- tuneR::sine(1, duration=5*44100)

addNoise <- function(w, start, window.length) {
  nw <- tuneR::noise("white", duration=length(w@left), samp.rate=w@samp.rate, pcm=w@pcm, bit=w@bit)
  rw <- w + nw/max(nw@left) # Scale noise to the amplitude of the sine wave
  return(rw)
}

o <- windowing(w, window.length=44100, window.overlap = -44100, FUN=addNoise, bind.wave=TRUE)
plot(o@left, type="l", xlab="Time (samples)", ylab="Amplitude")


