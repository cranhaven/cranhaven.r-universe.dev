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

## ----notes1a------------------------------------------------------------------
x <- "c e_ g b_"
note_has_accidental(x)
note_is_accidental(x)

## ----notes1b------------------------------------------------------------------
x <- "c e_ g b_ cd#g"
is_diatonic(x, key = "c")
is_diatonic(x, key = "b_")

## ----notes2a------------------------------------------------------------------
x <- "e_2 a_, c#f#a#"
n_steps(x)
n_notes(x)
n_chords(x)
chord_size(x)
octave_type(x)
time_format(x)

## ----notes2b------------------------------------------------------------------
x <- "e_2 a_, b_, c#f#a# c#'f#'a#''"
tally_notes(x)
tally_pitches(x)
distinct_notes(x)
distinct_pitches(x)
pitch_range(x)
semitone_range(x)

## ----notes3-------------------------------------------------------------------
flatten_sharp(x)
sharpen_flat(x)

## ----notes4-------------------------------------------------------------------
naturalize(x)

## ----notes5-------------------------------------------------------------------
note_set_key(x, "c") # no change possible
note_set_key(x, "f") # key of F has a flat
note_set_key(x, "g") # key of G has a sharp

## ----notes5b------------------------------------------------------------------
x <- "c, c c' c2 c c4"
as_integer_octaves(x)
as_tick_octaves(x)

## ----notes5c------------------------------------------------------------------
(x <- as_space_time(c("c", "e", "g", "ceg")))
(y <- as_vector_time("c e g ceg"))

as.character(x)
as.character(y)

## ----notes6-------------------------------------------------------------------
x <- "b_2 ce_g"
y <- "b_ cd#g"
note_is_equal(x, y)
note_is_identical(x, y)

pitch_is_equal(x, y)
pitch_is_identical(x, y)

## ----notes7-------------------------------------------------------------------
x <- "b_2 ce_g b_"
y <- "b_2 ce_gb_"
note_is_equal(x, y)

## ----notes8-------------------------------------------------------------------
x <- "b_2 ce_g b_"
y <- "b_2 ce_ gb_"
note_is_equal(x, y)

## ----notes9-------------------------------------------------------------------
x <- "a1 b_2 a1b2c3 a1b4 g1a1b1"
y <- "a_2 g#2 d1e1f2g3 a1b2b4 d1e1"
octave_is_equal(x, y)
octave_is_identical(x, y)
octave_is_identical(x, y, single_octave = TRUE)

x <- "a,, b_, a,,b,c a,,b' g,,a,,b,,"
y <- "a_, g#, d,,e,,f,g a,,b,b' d,,e,,"
octave_is_equal(x, y)
octave_is_identical(x, y)
octave_is_identical(x, y, single_octave = TRUE)

## ----notes10------------------------------------------------------------------
x <- "a b ceg"
note_slice(x, 2:3)
note_slice(x, c(FALSE, TRUE, TRUE))

## ----notes11------------------------------------------------------------------
x <- c("a", "b", "ceg")
note_slice(x, 2:3)
note_slice(x, c(FALSE, TRUE, TRUE))

## ----notes12------------------------------------------------------------------
x <- "bd'f#' a c'e'g' b ba c'g' gd'g'd''"
note_sort(x)
note_sort(x, decreasing = TRUE)

## ----notes13------------------------------------------------------------------
note_rotate(x, 1)
note_rotate(x, -1)

## ----notes14------------------------------------------------------------------
note_shift("c e g", 1)
note_shift("c e g", -4)

## ----notes15------------------------------------------------------------------
note_arpeggiate("c e_ g_ a", 3)
note_arpeggiate("c e_ g_ a", 3, -3)

## ----tp1----------------------------------------------------------------------
notes1 <- "c b, c d e e d c b, c c c'"
notes2 <- "c' b c' d' e' e' d' c' b c' c' c''"
transpose(notes1, 12, octaves = "integer") == as_noteworthy(notes2)

## ----tp2----------------------------------------------------------------------
transpose("a_ b_' c'", 0)
tp("a_ b_' c'", -1)
tp("a_ b_' c'", 1)
tp("a# b' c#'", 11)
tp("a# b' c#'", -12)
tp("a# b' c#'", 13)

## ----tp3----------------------------------------------------------------------
tp("a3 b4 c5", 2, key = "f")
tp("a3 b4 c5", 2, octaves = "tick", key = "g")
tp("a b' c''", 2, accidentals = "flat")
tp("a, b c'e'g'", 2, octaves = "integer", accidentals = "sharp")

