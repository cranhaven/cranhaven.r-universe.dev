\name{xparse}
\alias{xparse}
\title{Call script Perl for extracting data from corpus}
\description{
Call script Perl for extracting data from corpus. Before you run, you must configure a configuration file ini.json in the folder config as: dictionaries, graphs of grammar (Use tools Unitex for creating)...
}
\usage{
xparse(json_path = "",verbose=FALSE)
}
\arguments{
  \item{json_path}{path of configuration file (*.json)}
  \item{verbose}{logical. Should R report extra information on progress? Set to TRUE by the command-line option --verbose.}
}
\details{
  Input: dictionaries, grammars (build with software Unitex).
  Output: a result file of every entity and relation
}
\value{
Result file includes:
  \item{comp1}{data of every entity such as: file1:entity1:$:data1:data2:}
  \item{comp2}{data of every relation of every entity for example: file1:entity1:entity2:$$:data1:data2:1}
}
\examples{
  xparse()
}
\seealso{
  \code{\link{xshow}} display results
}