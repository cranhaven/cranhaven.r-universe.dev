# Pali Helper Functions
#
# Here are some helper functions for working with Pali.

library(stringr)
library(stringi)

# CRAN packages can't have Unicode characters, so the strings
# below are escaped using stringi::stri_escape_unicode(). Here
# are the originals for reference:
# pali_vowels <- c("a", "ā", "i", "ī", "u", "ū", "e", "o")
# pali_consonants <- c("k", "kh", "g", "gh", "ṅ",
#                      "c", "ch", "j", "jh", "ñ",
#                      "ṭ", "ṭh", "ḍ", "ḍh", "ṇ",
#                      "t", "th", "d", "dh", "n",
#                      "p", "ph", "b", "bh", "m",
#                      "y", "r", "l", "v", "s", "h", "ḷ", "ṃ")

pali_alphabet <-
  c("a", "\u0101", "i", "\u012b", "u", "\u016b", "e", "o",
    "k", "kh", "g", "gh", "\u1e45",
    "c", "ch", "j", "jh", "\u00f1",
    "\u1e6d", "\u1e6dh", "\u1e0d", "\u1e0dh", "\u1e47",
    "t", "th", "d", "dh", "n",
    "p", "ph", "b", "bh", "m",
    "y", "r", "l", "v", "s", "h", "\u1e37", "\u1e43")

# This is just an internal helper function to break Pali words
# into a vector of letters, respecting the fact that 'dh',
# 'kh', etc. are considered single letters in Pali
explode <- function(pali_word) {
  if (stringr::str_length(pali_word) < 2) {
    return(stringr::str_sub(pali_word, 1, 1))
  }
  else if ((stringr::str_sub(pali_word, 2, 2) == "h") &
           (stringr::str_sub(pali_word, 1, 2) %in% pali_alphabet)) {
    return(c(stringr::str_sub(pali_word, 1, 2),
             explode(stringr::str_sub(pali_word, 3))))
  }
  else {
    return(c(stringr::str_sub(pali_word, 1, 1),
             explode(stringr::str_sub(pali_word, 2))))
  }
}

#' Less-than (<) comparison function for Pali words
#'
#' Note that all Pali string comparisons are case-insensitive.
#' Also non-Pali characters are placed at the end of the
#' alphabet and are considered equivalent to each other.
#' This has been implemented in C++ for speed.
#'
#' @param word1 A first Pali word as a string
#' @param word2 A second Pali words as a string
#' @return TRUE if word1 comes before word2 alphabetically
#' @export
pali_lt <- function(word1, word2) {
  return(c_pali_lt(word1, word2))
}

# This is the original R version of pali_lt for reference:
#pali_lt <- function(word1, word2) {
#  temp1 <- explode(tolower(word1))
#  temp2 <- explode(tolower(word2))
#  nomatch <- length(pali_alphabet) + 1 # value for non-Pali letters
#  for (i in c(1:length(temp1))) {
#    if (i > length(temp2)) {
#      return(FALSE)
#    }
#    else if (match(temp1[i], pali_alphabet, nomatch = nomatch) <
#             match(temp2[i], pali_alphabet, nomatch = nomatch)) {
#      return(TRUE)
#    } else if (match(temp1[i], pali_alphabet, nomatch = nomatch) >
#               match(temp2[i], pali_alphabet, nomatch = nomatch)) {
#          return(FALSE)
#    }
#  }
#  if (stringr::str_length(word1) < stringr::str_length(word2))
#    return(TRUE)
#  else
#    return(FALSE)
#}

#' Equal (==) comparison function for Pali words
#'
#' Note that all Pali string comparisons are case-insensitive.
#'
#' @param word1 A first Pali word as a string
#' @param word2 A second Pali word as a string
#' @return TRUE if word1 and word2 are the same
#' @export
pali_eq <- function(word1, word2) {
  return(tolower(word1) == tolower(word2))
}


#' Greater-than (>) comparison function for Pali words
#'
#' Note that all Pali string comparisons are case-insensitive.
#' #' Also non-Pali characters are placed at the end of the
#' alphabet and are considered equivalent to each other.
#'
#' @param word1 A first Pali word as a string
#' @param word2 A second Pali word as a string
#' @return TRUE if word1 comes after word2 alphabetically
#' @export
pali_gt <- function(word1, word2) {
  # Defined in terms of pali_lt so the logic can live in one place
  return(!pali_eq(word1, word2) & !pali_lt(word1, word2))
}


#' Sorting function for vectors of Pali words.
#'
#' Note that all Pali string comparisons are case-insensitive.
#' This algorithm is based on Quicksort, but creates lots of
#' intermediate data structures instead of doing swaps in place.
#' This has been implemented in C++ as the original R version
#' was about 500x slower.
#'
#' @param word_list A vector of Pali words
#' @return A new vector of Pali words in Pali alphabetical order
#' @export
#'
#' @examples
#' # Every unique word of of the Mahāsatipatthāna Sutta in
#' # Pali alphabetical order:
#' pali_sort(sati_sutta_long$word)
#'
#' # A sorted list of 100 random words from the Tiptaka:
#' library(dplyr)
#' pali_sort(sample(tipitaka_long$word, 100))
pali_sort <- function(word_list) {
  c_pali_sort(word_list)
}

# This is the original R version of pali_sort for reference:
#pali_sort <- function(word_list) {
#  if (length(word_list) <= 1)
#    return(word_list)
#  pivot <- word_list[1]
#  rest <- word_list[-1]
#  pivot_less <- c()
#  pivot_greater <- c()
#  for (next_word in rest) {
#    if (pali_lt(next_word, pivot)) {
#      pivot_less <- c(pivot_less, next_word)
#    }
#    else {
#      pivot_greater <- c(pivot_greater, next_word)
#    }
#  }
#  return(c(pali_sort(pivot_less),
#           pivot,
#           pali_sort(pivot_greater)))
#}
