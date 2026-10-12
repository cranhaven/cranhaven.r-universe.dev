\name{locate.mismatch}
\alias{locate.mismatch}

\title{Identify Sectors not in All Regions}
\description{\code{locate.mismatch} finds which sectors are not found in all regions. If a sector is not in all regions a report is generated to indicate which regions have that sector, which regions don't have that sector, and where this sector is in the repository.}

\usage{locate.mismatch(io)}
\arguments{
  \item{io}{An object of class \code{InputOutput} created from \code{\link{as.inputoutput}}.}
}

\details{
\code{locate.mismatch} begins by identifying all sectors. Then if a sector is not in every region, the function identifies which regions have the sector, which regions don't have the sector, and where this sector is located. If it is important to have all regions having the same sectors, the location output can be used in \code{\link{agg.sector}}. For a full list of sectors, use \code{\link{easy.select}}.
}

\value{Produces a list of sectors. Each sector has a list of \code{location}, \code{regionswith}, and \code{regionswithout}. For example to find the regions that have a mismatched sector, use 

\code{(mismatch.object)$sector$regionswith}}


\author{John J. P. Wade}

\seealso{
\code{\link{as.inputoutput}}, \code{\link{agg.sector}}, \code{\link{easy.select}}
}
\examples{
data(toy.IO)
class(toy.IO)
# No mismatches
MM1 <- locate.mismatch(toy.IO)

# Making toy.IO have mismatches
toy.IO$RS_label <- rbind(toy.IO$RS_label,
                         c("Valhalla", "Wii"),
                         c("Valhalla", "Pizza"),
                         c("Valhalla", "Pizza"),
                         c("Valhalla", "Minions"))
MM2 <- locate.mismatch(toy.IO)
MM2$Lightsabers
}

