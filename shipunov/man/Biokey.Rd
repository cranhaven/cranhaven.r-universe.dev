\name{Biokey}
\alias{Biokey}
\alias{Numranks}

\title{Convert diagnostic keys and classification lists}
\description{Convert the oldest biological data structures: diagnostic keys ("keys") and classification lists ("classifs")}

\usage{
Biokey(data, from="", to="", recalculate=TRUE, internal=FALSE, force=FALSE)
Numranks(nums=NULL, ranks=NULL, add=NULL, empty="Species")
}

\arguments{
\item{data}{Diagnostic keys ("keys"), classification lists ("classifs") and tables, or Newick phylogeny trees}
\item{from}{Data type to convert from}
\item{to}{Data type to convert to}
\item{recalculate}{Recalculate the numeric ids?}
\item{internal}{(For debugging) Output internal 4-column 'key' objects instead?}
\item{force}{(For debugging) Ignore list of allowable conversion pairs?}
\item{nums}{Numbers to convert into ranks}
\item{ranks}{Ranks to convert into numbers}
\item{add}{Rank-number conversion rule to add (overrides embedded rules)}
\item{empty}{What rank to use for empty number?}
}

\details{

Biokey() is a way to convert classification lists ("classifs") or
diagnostic keys into each other. In addition, it handles species
classification tables ("table") and Newick trees ("newick").

To know which conversions are allowed, simply type Biokey() without
arguments (this will also induce the harmless error message).

Numranks() converts biological rank names into numbers and numbers into
rank names (Shipunov, 2017). To see the embedded conversion table, type
Numranks() without arguments.

To know more about keys and classifs, read help for "classifs" and "keys".

Bracket keys (see help for "keys") could have more than two conditions,
other keys not, so problems might arise during conversion (see examples).

Backreferenced keys (see help for "keys") is just a variety of bracket keys
so the only possible way to make them is from bracket keys.

Branched key (see help for "keys") is an indented key with omitted
"indent" column, therefore it does not require the separate conversion
way. See examples about how to convert indent column into actual indents.

Classification "table" is the data frame where each column represent some
particular rank (see examples to understand better). Similarly to "classif", 
"table" should use numerical ranks. In this case, numerical ranks should be
column names (see examples).

When Biokey() converts "classif" to "newick", it keeps higher group names
as node labels. It does not do that in all other cases.

It is an open question if phylogeny tree (Newick) should be converted
into "classif" (see help for "classifs") with all intermediate ranks
propagated (thus frequently become monotypic, i.e. with just one
subgroup), or with only main ranks (whole numbers) propagated, or
terminals (by default, they always have  "species" rank = 1) could follow
much bigger ranks (i.e., "species" = 1 might follow "family" = 3, not
"genus" = 2). At the moment, the last variant is implemented.

Comparably, "newick" to "classif" conversion does _not_ remove names of
monotypic intermediate taxa, this might result in "crowding" of node
labels (see the example). Also, this conversion automatically propagates
intermediate ranks to make all ranks concerted, this might result in
empty labels.

}

\value{
Typically, the data frame or just a character string (in case of Newick output).
Output may contain column names but this is only to facilitate understanding of
the format and could be stripped without consequences. If 'internal=TRUE',
outputs a standardized 4-column data frame in a form of branched key
(columns 'id', 'description', 'terminal'), plus 'goto' column which might be just NAs.
}

\references{
Shipunov A. 2017. "Numerical ranks" to improve biological nomenclature of
higher groups. See "https://arxiv.org/abs/1708.07260".
}

\author{Alexey Shipunov}

\seealso{\code{\link{classifs}}, \code{\link{keys}}}

\examples{
## Biokey() # makes (harmless) error message but also shows which conversions are available
Numranks() # shows the conversion table

## ===

Numranks(nums=1:7)
Numranks(ranks="kingdom") # "kingdom", "order", "family" and "tribe" translate into Latin

## ===

## three branched keys
i1 <- c("1 A ", "2 B Name1", "2 BB Name2", "1 AA ", "3 C Name3", "3 CC Name4")
i2 <- c("1 A Name1", "2 B Name2", "2 BB ", "3 C Name3", "3 CC Name4")
i3 <- c("1 A Name1", "2 B Name2", "2 BB ", "3 C Name3",
 "3 CC Name4", "2 BBB ", "4 D Name5", "4 DD Name6", "4 DDD Name7")
k1 <- read.table(textConnection(i1), sep=" ", as.is=TRUE)
k2 <- read.table(textConnection(i2), sep=" ", as.is=TRUE)
k3 <- read.table(textConnection(i3), sep=" ", as.is=TRUE)

## convert them into phylogeny trees and plot
t1 <- Biokey(k1, from="branched", to="newick")
t2 <- Biokey(k2, from="branched", to="newick")
t3 <- Biokey(k3, from="branched", to="newick")
library(ape) # load 'ape' to plot Newick trees below
plot(read.tree(text=t1))
plot(read.tree(text=t2))
plot(read.tree(text=t3))

## ===

## Bracket keys
bracket1 <- keys[[1]]
bracket1
Biokey(bracket1, from="bracket", to="backreferenced")
(ii <- Biokey(bracket1, from="bracket", to="indented"))
## Remove third condition to avoid warnings:
Biokey(bracket1[bracket1[, 3] != "Horse", ], from="bracket", to="serial")
(nn <- Biokey(bracket1, from="bracket", to="newick"))
plot.phylo(read.tree(text=nn)) # plot newick as phylogeny trees

## Now convert indent column into actual indents:
for (i in 1:length(ii[, 1])) ii[i, 1] <- paste(rep(" ", ii[i, 1]), collapse="")
## and make also dot leaders
ifelse(!is.na(ii[, 3]), "...", "")
ii

## Branched keys
branched1 <- keys[[3]]
head(branched1)
Biokey(branched1, from="branched", to="bracket")[1:7, ]
Biokey(branched1, from="branched", to="indented")[1:7, ]
Biokey(branched1, from="branched", to="serial")[1:7, ]
(nn <- Biokey(branched1, from="branched", to="newick"))
plot.phylo(read.tree(text=nn))

## Indented keys (same as branched but with indent as first column)
indented0 <- c("0 1 Blue ", "1 2 Gas Sky", "1 2 Liquid ",
 "0 1 Yellow ", "2 3 Star Sun", "2 3 Buttecup Flower")
(indented1 <- read.table(textConnection(indented0), sep=" ", as.is=TRUE))
Biokey(indented1, from="indented", to="bracket")
Biokey(indented1, from="indented", to="serial")
(nn <- Biokey(indented1, from="indented", to="newick"))
plot.phylo(read.tree(text=nn))

## Serial keys
serial1 <- keys[[4]]
head(serial1)
Biokey(serial1, from="serial", to="bracket")[1:7, ]
Biokey(serial1, from="serial", to="indented")[1:7, ]
(nn <- Biokey(serial1, from="serial", to="newick"))
plot.phylo(read.tree(text=nn))

## Classifs
classif2 <- classifs[[2]]
classif2[, 1] <- Numranks(ranks=classif2[, 1], add=c(Series=1.1))
head(classif2)
Biokey(classif2, from="classif", to="table")[1:7, ]
(nn <- Biokey(classif2, from="classif", to="newick"))
tt <- read.tree(text=nn)
plot.phylo(tt, node.depth=2)
nodelabels(tt$node.label, frame="none", bg="transparent", adj=-0.05)

## Classification tables
table0 <- c("FAMILY SUBFAMILY TRIBE GENUS", "Hominidae Homininae Hominini Homo",
 "Hominidae Homininae Hominini Pan", "Hominidae Homininae Gorillini Gorilla",
 "Hominidae Ponginae Ponginini Pongo")
(table1 <- read.table(textConnection(table0), sep=" ", as.is=TRUE, h=TRUE))
names(table1) <- Numranks(ranks=names(table1))
table1
Biokey(table1, from="table", to="classif")

## Newick phylogeny trees
newick1 <- "((Coronopus,Plantago),(Bougueria,(Psyllium_s.str.,Albicans)),Littorella);"
plot.phylo(read.tree(text=newick1))
Biokey(newick1, from="newick", to="classif")

}

\keyword{classif}
