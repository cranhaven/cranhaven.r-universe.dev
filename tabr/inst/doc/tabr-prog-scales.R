## ----setup, include = FALSE---------------------------------------------------
options(crayon.enabled = TRUE)
sgr_wrap <- function(x, options){
  paste0("<pre class=\"r-output\"><code>", fansi::sgr_to_html(x = htmltools::htmlEscape(x)), "</code></pre>")
}
knitr::knit_hooks$set(output = sgr_wrap)
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE, error = FALSE, tidy = FALSE, out.width = "100%"
)
library(tabr)
library(dplyr)
mainIntervals <- tbl_df(mainIntervals)

## ----key----------------------------------------------------------------------
keys()

key_is_flat("f")
key_n_flats("f")

## ----scales-------------------------------------------------------------------
scale_hungarian_minor(key = "am", collapse = TRUE)

## ----scales2------------------------------------------------------------------
scale_major("f", TRUE, ignore_octave = TRUE)
scale_major("f", TRUE, ignore_octave = FALSE)

## ----modes--------------------------------------------------------------------
modes()
mode_aeolian("c")

## ----scale_chords-------------------------------------------------------------
scale_chords("b_", "major", "seventh", collapse = TRUE)
scale_chords("f#", "minor", "triad", collapse = TRUE)

## ----scale_degrees------------------------------------------------------------
x <- "c e gb'd'"
scale_degree(x)
scale_degree(x, key = "a")
scale_degree(x, key = "am")
scale_degree(x, scale = "chromatic")

scale_note(1:7, "d")
scale_note(c(1:8), "dm", "harmonic minor")

note_in_scale("a_ g#", "a_", strict_accidentals = FALSE)

x <- "r d dfa df#a f#ac#"
chord_degree(x, "d")
is_in_scale(x, "d")
is_diatonic(x, "d")

## ----intervals----------------------------------------------------------------
mainIntervals

interval_semitones(c("m3", "M7"))

## ----intervals2---------------------------------------------------------------
pitch_interval("a2", "c")
pitch_interval("c d e", "c c c")
pitch_interval("r c ceg c e g s", "a c d d f# a e")
pitch_interval("r c ceg c e g s", "a c d d f# a e", use_root = FALSE)

## ----intervals3---------------------------------------------------------------
scale_interval("c c c c", "c, e g b")
scale_interval("a2", "c", format = "mmp")

## ----intervals4---------------------------------------------------------------
pitch_diff("c d e f g a b")
pitch_diff("c d e f g a b", trim = TRUE)
scale_diff("c d e f g a b")
scale_diff("c d e f g a b", n = 2)

## ----intervals5---------------------------------------------------------------
x <- "a, c r r r r g"
pitch_diff(x)
scale_diff(x)
pitch_diff(x, n = 2)
scale_diff(x, n = 2, trim = TRUE)

