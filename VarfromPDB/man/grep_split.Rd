\name{grep_split}
\alias{grep_split}
\title{
Extention for grep function
}
\description{
grep a string whether in another string or vector, the string are split by space.
}
\usage{
grep_split(keyword, x)
}
\arguments{
  \item{keyword}{
a character string, separator "|" is permitted. 
}
  \item{x}{
a character vector where matches are sought.
}
}
\details{
Extention for grep functin.
}
\value{
The function return the numbers vector which contain the keyword.
}
\author{
Zongfu Cao (caozongfu@nrifp.org.cn)
}
\seealso{
\code{\link{grep}}
}

\examples{
x <- c("you and he and I", "you", "Tom", "I", "you and I", "he and I")
grep_split("you and I | Tom", x)
#[1] 1 5 3
}
\keyword{grep}
