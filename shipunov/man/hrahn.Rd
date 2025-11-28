\name{hrahn}
\alias{hrahn}

\title{Angiosperm families: morphological characters}

\description{

This data originated from the Hansen and Rahn (1969) "punched cards"
publication, and subsequent additions and corrections (Hansen and Rahn,
1972; Hansen and Rahn, 1979). Idea was to use paper cards with holes to
assist identification of flowering plants (angiosperm) families. These
cards were digitized (Duncan and Meacham, 1986) and then used in several
multi-entry identification systems (for example, Duncan and Meacham,
1986; Ray, 1995; Families..., 2008).

But what was a sizeable task in 1980--1990s, now is only few hours of R
programming. It is therefore quite easy to make such system with R,
please see the example. The core function is only a few lines of code,
everything else is the interface "bells and whistles". This example
system is also applicable to any data with similar structure.

The 'hrahn' data can also be used for the purposes other than
identification, for example, to assist in the morphological analysis of
angiosperm families.

Comparing with original printed sources, the version used here misses
supporting illustrations and some comments to characters. Comparing with
digital sources, it was slightly modified, mostly to correct the
imperfect digitization, and add some comments from the printed version
(they are in lowercase).

One of comments is large but important so it is placed below as "Note I".

===

Note I. [concerning naming of perianth]

A. Perianth segments in 1 cycle or 2 cycles uniform in colour, size and
shape.

B. coloured and petal-like ... all _petals_

BB. green (colourless if the plant is without chlorophyll) or dry and
hyaline, glumaceous or scarious ... all _sepals_

AA. Perianth segments in 2 cycles different in colour, size or shape.

C. outer cycle ... _sepals_

CC. inner cycle ... _petals_

AAA. Perianth segments spirally arranged with a gradual transition in
colour, size and shape from inner to outer segments: in these cases we
have guarded against misinterpretations by stating _all_ segments as
_sepals_ _and_ as _petals_. If there is a tendency to differentiation
into sepals and petals, then the numbers judged by us to be interpretable
as sepals are stated as such and in the same way for the petals.

===

The data is based on the family concepts and characters used in Melchior
(1964), Hutchinson (1967) and Cronquist (1981). Therefore, family
concepts might be different from those which are in use now. In the data,
families in are given in accordance with classifications above so
outputted list of families is not sorted alphabetically.

}

\usage{

hrahn

}

\format{

This is a list which contains two components:

\describe{

\item{\code{data}}{Binary matrix, row names are families, columns with
'chars'}

\item{\code{chars}}{Character vector with descriptions of characters,
posititons correspond with columns of 'data'}

}}

\source{

Cronquist A. 1981. An integrated system of classification of flowering
plants. Columbia University Press, New York.

Duncan T., Meacham C.A. 1986. Multiple-entry keys for the identification
of angiosperm families using a microcomputer. Taxon. 35: 492--494.

Families of Angiosperms: Punched Cards by Hansen and Rahn. 2008. eFloras.
URL: http://www.efloras.org/flora_page.aspx?flora_id=900. Missouri
Botanical Garden, St. Louis, MO and Harvard University Herbaria, Cambridge,
MA.

Hansen B., Rahn K. 1969. Determination of angiosperm families by means of
a punched-card system. Dansk Botanisk Arkiv. 26: 1--46 + 172 punched
cards.

Hansen B., Rahn K. 1972. Determination of angiosperm families by means of
a punched-card system. Additions and corrections. I. Botanisk tidsskrift.
67: 152--163.

Hansen B., Rahn K. 1979. Determination of angiosperm families by means of
a punched-card system. Additions and corrections. II. Botanisk
tidsskrift. 74: 177--178.

Hutchinson J. 1967. Key to the families of flowering plants of the world.
Clarendon Press, Oxford.

Melchior H. 1964. A. Engler's Syllabus der Pflanzenfamilien 12. II Band.
Angiospermen. Gerbrueder Borntbaeger, Berlin, Nikolassee.

Ray Ph. 1995. Flowering plant family identification. URL:
http://www.colby.edu/info.tech/BI211/info.html

}

\examples{

data <- hrahn$data
chars <- hrahn$chars

showcharlist <- function(selchar) {
 tmp <- tempfile()
 selected <- ifelse(seq_along(chars) \%in\% selchar, "[X]", "[ ]")
 useful <- makeuseful(selchar)
 selected[useful] <- "[O]"
 write.table(data.frame(selected, seq_along(chars), chars),
  file=tmp, quote=FALSE, col.names=FALSE, row.names=FALSE)
 file.show(tmp)
}

makeuseful <- function(selchar) { # numbers of potentially useful characters
 selrows <- rowSums(data[, selchar, drop=FALSE]) == length(selchar)
 sums <- colSums(data[selrows, , drop=FALSE])
 seq_len(ncol(data))[sums > 0 & sums < sum(selrows)]
}

makefam <- function(selchar) { # the core function
 selrows <- rowSums(data[, selchar, drop=FALSE]) == length(selchar)
 row.names(data)[selrows]
}

displayfam <- function(selfam, howmany=12) { # display first "howmany" families
 if (is.null(selfam) || length(selfam) == 0) return("None")
 lfam <- length(selfam)
 if (lfam > howmany) {
 dfam <- selfam[seq_len(howmany)]
 res <- paste(c(dfam, paste0("and ", lfam-12, " more")), collapse=", ")
 } else {
 res <- paste(selfam, collapse=", ")
 }
 res
}

updatechar <- function(old, new) { # add or remove characters
 positive <- new[new > 0 & new <= length(chars)]
 old <- union(na.omit(old), positive)
 negative <- abs(new[new < 0])
 setdiff(old, negative)
}

displaydn <- function(num, sym="-") { # display numbers with dashes
 if (!is.numeric(num)) stop("Argument must be numeric")
 if (length(num) == 1) return(as.character(num))
 num <- sort(unique(num))
 if (length(num) == 2) return(paste(num, collapse=", "))
 num[abs(num - c(num[length(num)], num[-length(num)])) == 1 &
  abs(num - c(num[-1], num[1])) == 1] <- "-"
 gsub(", (-, )+", sym, paste(num, collapse=", "))
## slightly longer (but concatenates with +1 number):
## cc <- paste0(num, c(ifelse(diff(num) == 1, "-", ""), ""), collapse=", ")
## gsub("-, ", "-", gsub("-, (-*[0-9]+-, )+", "-", cc))
}

displaychar <- function(selchar) {
 if (is.null(selchar) || length(selchar) == 0) return("None")
 displaydn(selchar)
}

run <- function(howmany=12, selfam=NULL, selchar=NULL) { # interface, recursive function
if (!interactive()) return(cat("Please run in interactive mode\n"))
cat("Results:", displayfam(selfam, howmany=howmany), "\n")
cat("Selected characters:", displaychar(selchar), "\n")
cat("Potentially useful characters:", displaychar(makeuseful(selchar)), "\n")
cat("===\n")
cat("Type (character) numbers, separate with comma, negative numbers remove from selection\n")
cat("Type 'c' to see the list of characters, [X] selected, [O] potentially useful\n")
cat("Type any other single letter to exit\n")
cat("===\n")
x <- readline(prompt="Your choice: ")
while (TRUE) {
 if (x == "c") showcharlist(selchar)
 if (x \%in\% c(letters[-3], LETTERS)) break
 new <- suppressWarnings(as.integer(strsplit(x, split=",")[[1]]))
 selchar <- updatechar(selchar, new)
 selfam <- makefam(selchar)
 run(howmany=howmany, selfam=selfam, selchar=selchar)
 break
 }
}

run()

}

\keyword{datasets}
