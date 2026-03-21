#' @import methods
#' @include Interval.R IntervalTree.R

library(rIntervalTree)
rm(list=ls())

print("testing Interval class")
i1 <- new("Interval", start=1.1,end=1.2, key="dummy")
isOverlap(i1, c(1.0, 1.5))
#isOverlap(i1, c(1.0, 1.5, 2))
isOverlap(i1, 1.0)
isOverlap(i1, c(1.1))
isOverlap(i1, c(1.13, 1.15))
#isOverlap(i1, c(1.8, 1.5))
isOverlap(i1, c(1.3, 1.5))
isOverlap(i1, 5)


print("testing IntervalTree class")

RNA_mass <- read.table("C:/GREENBLATT/RNA_MS/List_of_RNA_modifications.txt", sep="\t",header=T,quote="",comment.char = "", stringsAsFactors = F)

head(RNA_mass)
error <- 10 #ppm
name <- RNA_mass$Name
low <- RNA_mass$Monoisotopic.mass*(1-error/1000000)
high <- RNA_mass$Monoisotopic.mass*(1+error/1000000)

mass_ranges <- data.frame(name, low, high, stringsAsFactors = FALSE)
compounds <- mass_ranges[1:23,]
#write.table(compounds, "./data/compounds.tab", sep="\t", row.names=F)

data(compounds)
I <- new("IntervalTree", data=compounds, root=list())
II <- IntervalTree(data=compounds, root=list())
III <- IntervalTree()
res <- overlapQuery(I, 258.085)
res
res <- overlapQuery(II, 258.085)
res
res <- overlapQuery(III, 258.085)
res
III@data <- compounds
III <- buildTree(III)

overlapQuery(III, 258.085)
res <- overlapQuery(I, c(238, 250.047))
res
res <- overlapQuery(III, c(238, 250.047))
res

results <- as.data.frame(matrix(unlist(res), ncol=3, byrow=T), stringsAsFactors=FALSE)
results
names <- unlist(lapply(res, `[[`, 1))
names

print("testing insert interval")
III <- insertInterval(III, c("test", 280, 282))
overlapQuery(III, 281.11)
III <- insertInterval(III, c("test2", 260, 2820))
overlapQuery(III, 281.11)
III <- insertInterval(III, c("test3", -280, 282))
overlapQuery(III, 281.11)
overlapQuery(III, c(258, 260.047))
overlapQuery(III, c(-2880, 2900.047))
overlapQuery(III, 0)

devtools:::cran_comments()
