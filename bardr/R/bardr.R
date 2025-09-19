#' bardr: providing the complete works of the Bard in tidy format.
#'
#' The bardr package provides R data structures for all of William
#' Shakespeare's works available in the Project Gutenberg ebook. The provided
#' data are designed to seamlessly work in R without the hassle of data
#' wrangling and cleaning, which has already been performed.
#'
#' Inspired by the janeaustenr package by Julia Silge: see
#' https://github.com/juliasilge/janeaustenr .
#'
#' @section Complete collections:
#' The complete works are available all at one time in two separate formats.
#'
#' One is a named list, where each entry is a named character vector. The name
#' of the vector is the name of the work, and the contents of the vector are
#' lines of the associated text file (all lines are <= 70 characters).
#'
#' The other is a data frame with a column for the name of the work (repeated
#' as many times as there are lines of content) and a column for the content
#' of the work, where each cell in the content column is one line of text.
#'
#' @docType package
#' @name bardr
NULL
