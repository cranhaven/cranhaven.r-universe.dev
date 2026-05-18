\name{VBTree-package}
\alias{VBTree-package}
\alias{VBTree}
\docType{package}
\title{
\packageTitle{VBTree}
}
\description{
\packageDescription{VBTree}
}
\details{
This package provide an efficient approach to manage data by structurizing the column names. A column name is generally seen as a character object, while if
it has a very organized pattern, such as "*-*-*-*" for example (each * mark presents a different condition), it must has a certain mapping relationship to a
specific tensor. This package uses two data structure: double list and vector binary tree, to implement the conversion between the character vector and tensor.
It affords various inquiry methods, which was mainly drived by vector binary tree, to extract the highly customizable subset from original data.
}
\author{
\packageAuthor{VBTree}

Maintainer: \packageMaintainer{VBTree}
}
\references{
Sedgewick, Robert & Wayne, Kevin (2011). Algorithms, 4th Edition.. Addison-Wesley

Prakash, P. K. S. & Rao, Achyutuni Sri Krishna (2016). R Data Structures and Algorithms. Packt Publishing
}
\keyword{ package }
\seealso{
\code{\link[tensorA:to.tensor]{to.tensor}}, \code{\link[tensorA:pos.tensor]{pos.tensor}}.
}
\examples{
\donttest{
#View the data to be visited:
summary(datatest)
colnames(datatest)

#Structurize colnames of data into vector binary tree:
dl <- chrvec2dl(colnames(datatest))
vbt <- dl2vbt(dl)
vbt

#Setting subset in different forms, for example the pattern
#"Strain-(900~1100)-(0.01, 1)-0.6" is desired:
subunregdl <- list(c(1), c(1:5), c(2,4), c(1)) # undifined double list
subregdl <- advbtinq(vbt, subunregdl) # regularized double list
subvbt <- dl2vbt(subregdl) # sub vector binary tree
subts <- vbt2ts(subvbt) # tensor
subarr <- vbt2arr(subvbt) # array
subchrvec <- as.vector(subarr) # character vector

#Visit the data through different methods:
datavisit(datatest, subunregdl) # by handmade double list
datavisit(datatest, subregdl) # by defined double list
datavisit(datatest, subvbt) # by vector binary tree
datavisit(datatest, subts) # by tensor
datavisit(datatest, subarr) # by array
datavisit(datatest, subchrvec) # by character vector
}
}
