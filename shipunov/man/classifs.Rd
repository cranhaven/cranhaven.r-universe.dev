\name{classifs}
\alias{classifs}

\title{Classification lists}

\usage{
classifs
}

\format{

The list with two data frames representing 'classifs', classification
lists. First is the classif with textual ranks, second with numerical
ranks. Both based on some classifications of Plantago (ribworts,
plantains), first (Shipunov, 2000) include species only from European
Russia, the other is from the oldest Plantago monograph (Barneoud, 1845).

}

\description{

Classification lists ('classifs') are probably one of the most ancient
attempts to represent biological diversity, the ordered heterogeneity of
living things. In biological systematics, they dated from 1753 when
Linnaeus published his "Species Plantarum":

\if{latex}{\figure{linnaeus1753classif.png}{options: width=10cm}}
\if{html}{\figure{linnaeus1753classif.png}{options: width="50\%" alt="Figure: linnaeus1753classif.png"}}
\if{text}{Figure: linnaeus1753classif.png (to see it, check pdf or html)}

(here on the first page of this book four ranks and five names are
represented: class ("Monandria"), order ("Monogynia"), genus ("Canna")
and species ("Canna indica" and "Canna angustilolia"))

In essence, classifs require only two columns: rank and name (in that
order) so they are easy to standardize as two-column data frames.
However, we need to know how to order the ranks. One way is to convert
ranks into numbers (Shipunov, 2017). Numranks() implements this
functionality.

It is possible to extend classifs with more columns: synonyms, name
comments and taxonomic comments. Synonyms (the third column) are
especially useful; each synonym will be then one row where second
position is a valid name and third position is (one of) synonyms.

Please note that while 'classifs' as data frames are human-readable, they
are not typographic. To make them better suited for publication, one
might convert them into LaTeX where many packages could be used to
typeset classifications (for example, my 'classif2' package).

Note also that in classif, species names must be given in full (in
biology, species name consists of two words, (a) genus name and (b)
species epithet). One of examples below shows how to replace
abbreviations with full genus names. }

\source{

Linnaeus C. 1753. Species Plantarum. Holmieae.

Barneoud F.M. 1845. Monographie generale de la familie des
Plantaginaceae. Paris.

Shipunov A. 2019. classif2 -- Biological classification tables. Version
2.2. See "https://ctan.org/pkg/classif2".

Shipunov A. 2000. The genera Plantago L. and Psyllium Mill.
(Plantaginaceae Juss.) in the flora of East Europe. T. Novosti
Systematiki Vysshikh Rastenij. 32: 139--152. [In Russian]

Shipunov A. 2017. "Numerical ranks" to improve biological nomenclature of
higher groups. 2017. See "https://arxiv.org/abs/1708.07260".

}

\seealso{\code{\link{Biokey}}, \code{\link{Numranks}}}

\examples{

## European Russian species classif
plevru <- classifs$plevru
## convert rank names into numbers
plevru[, 1] <- Numranks(ranks=plevru[, 1], add=c(Series=1.1))

## now convert into Newick tree and plot it
plevru.n <- Biokey(plevru, from="classif", to="newick")
library(ape) # to plot, load the 'ape' package
plot(read.tree(text=plevru.n))

## convert classif to taxonomic table
plevru.t <- Biokey(plevru, from="classif", to="table")
colnames(plevru.t) <- Numranks(nums=as.numeric(colnames(plevru.t)))
plevru.t

## two Newick trees
aa <- "(A,(B,C),(D,E));"
bb <- "((A,(B,C)),(D,E));"
## convert them to classif
aa.c <- Biokey(aa, from="newick", to="classif")
bb.c <- Biokey(bb, from="newick", to="classif")
## ... and back to Newick
aa.n <- Biokey(aa.c, from="classif", to="newick")
bb.n <- Biokey(bb.c, from="classif", to="newick")

## how to convert abbreviated species names
spp <- c ("Plantago afra", "P. arborescens", "P. arenaria")
stt <- do.call(rbind, strsplit(spp, " "))
stt[, 1] <- Fill(stt[, 1], "P.")
(res <- apply(stt, 1, paste, collapse=" "))
}

\keyword{datasets}
