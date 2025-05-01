\name{TangPoemR}
\alias{TangPoemR}
\title{Write Chinese Tang Poems}
\description{
The package helps generate Chinese Tang poems automatically.
}
\details{
We created our poems dataset from Three Hundred Tang Poems:

#import the file "Three Hundred Tang Poems"

fileName <-"Three Hundred Tang Poems.txt"

SC <- readChar(fileName, file.info(fileName)$size)


#the process of text segmentation

library(jiebaR)

cc = worker()

analysis <- as.data.frame(table(cc[SC]))

names(analysis) <- c("word","freq")

analysis$word <- as.character(analysis$word)


#set the example poems sp5, sp7


tagger <- worker("tag")

sp5_2 <- tagger <= sp5

sp7_2 <- tagger <= sp7

example <- subset(analysis, freq >1 & nchar(word) <3 & freq < 300)


#extract the examples' parts of speech

cixing5 <- attributes(sp5_2)$names

cixing7 <- attributes(sp7_2)$names


#get the dataset

example_2 <- tagger <= example$word


Now, with our dataset, this package can automatically generate Chinese Tang poems of our specified length(5 or 7).}
\value{
no return
}
\references{
nothing
}
\author{
Yanni Li, Yongyuan Liang
}
\note{
The package helps people experience Chinese ancient poetry culture with R.
}
\seealso{
nothing
}
\examples{
write_tangpoem(5)
}
\keyword{Tang Poems, Language Processing}
