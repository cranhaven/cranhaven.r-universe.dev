# Tipitaka (Pali Canon)
#
# This file documents the multiple versions of the complete
# Pali Canon of Therevadin Buddhism, also know (in Pali)
# as the Tipitaka, that are included in this package.
#
# It is provided in both "long" and "wide" formats, as each are
# useful for different types of analysis, and conversion
# between the two can be slow and error-prone.


#' tipitaka: A package for exploring the Pali Canon in R.
#'
#' The package tipitaka provides access to the complete Pali
#' Canon, or Tipitaka, from R. The Tipitaka is the canonical
#' scripture for Therevadin Buddhists worldwide. This version
#' is largely taken from the Chattha Sangāyana Tipitaka
#' version 4.0 com;iled by the Vispassana Research Institute,
#' although edits have been made to conform to the numbering
#' used by the Pali Text Society. This package provides both
#' data and tools to facilitate the analysis of these ancient
#' Pali texts.
#'
#'
#' @section Data:
#' Several data sets are included:
#' \itemize{
#'   \item tipitaka_raw: the complete text of the Tipitaka
#'   \item tipitaka_long: the complete Tipitaka in "long" form
#'   \item tipitaka_wide: the complete Tipitaka in "wide" form
#'   \item tipitaka_names: the names of each book of the Tipitaka
#'   \item sutta_pitaka: the names of each volume of the Sutta Pitaka
#'   \item vinaya_pitaka: the names of each volume of the Vinaya Pitaka
#'   \item abhidhamma_pitaka: the names of each volume of the Abhidhamma Pitak
#'   \item sati_sutta_raw: the Mahāsatipatthāna Sutta text
#'   \item sati_sutta_long: the Mahāsatipatthāna Sutta in "long" form
#'   \item pali_alphabet: the complete pali alphabet in traditional order
#'   \item pali_stop_words: a set of "stop words" for Pali
#'   }
#'
#' @section Tools:
#' A few useful functions are provided for working with Pali text:
#' \itemize{
#'   \item pali_lt: less-than function for Pali strings
#'   \item pali-gt: greater-than function for Pali strings
#'   \item pali-eq: equals function for Pali strings
#'   \item pali-sort: sorting function for vectors of pali strings
#' }
#'
#' @docType package
#' @name tipitaka
#' @useDynLib tipitaka
NULL


#' Tipitaka text in raw form
#'
#' The unprocessed text of the Tipitaka, with one row per volume.
#'
#' @format A tibble with the variables:
#' \describe{
#' \item{text}{Text of each Tipitaka volume}
#' \item{book}{Abbreviated book name of each volume}
#' }
#'
#' @source Vipassana Research Institute, CST4, April 2020
"tipitaka_raw"


#' Tipitaka in "long" form
#'
#' Every word of every volume of the Tipitaka, with one word per
#' volume per line.
#'
#' @format A tibble with the variables:
#' \describe{
#' \item{word}{Pali word}
#' \item{n}{Number of time this word appears in this book}
#' \item{total}{Ttal number of words in this book}
#' \item{freq}{Frequency with which this word appears in this book}
#' \item{book}{Abbreviated book name}
#' }
#'
#' @source Vipassana Research Institute, CST4, April 2020
"tipitaka_long"


#' Tipitaka in "wide" form
#'
#' Every word of every volume of the Tipitaka, with one word per
#' column and one book per line. Each cell is the frequency at
#' which that word appears in that book.
#'
#'
#' @source Vipassana Research Institute, CST4, April 2020
"tipitaka_wide"


#' Names of each book of the Tipitaka, both abbreviated and
#' in full. These are easier to read if you call \code{pali_string_fix() first}.
#'
#' @format A tibble with the variables:
#' \describe{
#'   \item{book}{Abbreviated title}
#'   \item{name}{Full title}
#' }
#'
#' @examples
#' # Clean up the Unicode characters to make things more readble:
#' tipitaka_names$name <-
#'   stringi::stri_unescape_unicode(tipitaka_names$name)
#'
"tipitaka_names"


#' All the books of the Sutta Pitaka
#'
#' A subset of tipitaka_names consisting of only the books of
#' the Sutta Pitaka. These are easier to read if you call
#' \code{stringi::stri_unescape_unicode} first.
#'
#' @format A tibble with the variables:
#' \describe{
#'   \item{book}{Abbreviated title}
#'   \item{name}{Full title}
#' }
#'
#' @examples
#' # Clean up the Unicode characters to make things more readble:
#' sutta_pitaka$name <-
#'   stringi::stri_unescape_unicode(sutta_pitaka$name)
#' # Count all the words in the Suttas:
#' sum(
#'   unique(
#'     tipitaka_long[tipitaka_long$book %in% sutta_pitaka$book, "total"]))
#'
#' # Count another way:
#' sum(tipitaka_long[tipitaka_long$book %in% sutta_pitaka$book, "n"])
#'
#' # Create a tibble of just the Suttas
#' sutta_wide <-
#'   tipitaka_wide[row.names(tipitaka_wide) %in% sutta_pitaka$book,]
#'
"sutta_pitaka"


#' All the books of the Vinaya Pitaka
#'
#' A subset of tipitaka_names consisting of only the books of
#' the Vinaya Pitaka. These are easier to read if you call
#' \code{stringi::stri_unescape_unicode} first.
#'
#' @format A tibble with the variables:
#' \describe{
#'   \item{book}{Abbreviated title}
#'   \item{name}{Full title}
#'}
#'
#' @examples
#' # Clean up the Unicode characters to make things more readble:
#' vinaya_pitaka$name <-
#'   stringi::stri_unescape_unicode(vinaya_pitaka$name)
#'
#' # Count all the words in the Vinaya Pitaka:
#' sum(tipitaka_long[tipitaka_long$book %in% vinaya_pitaka$book, "n"])
#'
"vinaya_pitaka"


#' All the books of the Abhidhamma Pitaka
#'
#' A subset of tipitaka_names consisting of only the books of
#' the Abhidhamma Pitaka. These are easier to read if you call
#' \code{pali_string_fix() first}.
#'
#' @format A tibble with the variables:
#' \describe{
#'
#'   \item{book}{Abbreviated title}
#'   \item{name}{Full title}
#'}\
#'
#' @examples
#' # Clean up the Unicode characters to make things more readble:
#' abhidhamma_pitaka$name <-
#'   stringi::stri_unescape_unicode(abhidhamma_pitaka$name)
#'
#' # Count all the words in the Abhidhamma Pitaka:
#' sum(tipitaka_long[tipitaka_long$book %in% abhidhamma_pitaka$book, "n"])
#'
"abhidhamma_pitaka"



#' Mahāsatipatthāna Sutta in "long" form
#'
#' The Mahāsatipatthāna Sutta or Discourse on the Establishing
#' of Mindfulness in "long" form.
#'
#' @source Vipassana Research Institute, CST4, April 2020
"sati_sutta_long"

#' Mahāsatipatthāna Sutta text in raw form
#'
#' The unprocessed text of the Mahāsatipatthāna Sutta
#'
#' @format A tibble with the variable:
#' \describe{
#' \item{text}{Complete text}
#' }
#'
#' @source Vipassana Research Institute, CST4, April 2020
"sati_sutta_raw"


#' Tentative set of "stop words" for Pali
#'
#' A list of all declinables and particles from the PTS
#' Pali-English Dictionary.
#'
#' @examples
#' # Find most common words in the Mahāsatipatthāna Sutta excluding stop words
#' library(dplyr)
#' sati_sutta_long %>%
#'   anti_join(pali_stop_words, by = "word") %>%
#'   arrange(desc(freq))
#'
#' @source \url{https://dsalsrv04.uchicago.edu/dictionaries/pali/}
"pali_stop_words"


#' Pali alphabet in order
#'
#' @format The Pali alphabet in traditional order.
#'
#' @examples
#' # Returns TRUE because a comes before b in Pali:
#' match("a", pali_alphabet) < match("b", pali_alphabet)
#' # Returns FALSE beceause c comes before b in Pali
#' match("b", pali_alphabet) < match("c", pali_alphabet)
#'
"pali_alphabet"

