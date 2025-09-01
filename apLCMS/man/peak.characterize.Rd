\name{peak.characterize}
\alias{peak.characterize}
\title{ Internal function: Updates the information of a feature for the known feature table. }
\description{
The function takes the information about the feature in the known feature table (if available), and updates it using the information found in the current dataset. }
\usage{
peak.characterize(existing.row = NA, ftrs.row, chr.row)
}
\arguments{
  \item{existing.row}{The existing row in the known feature table (detailed in the semi.sup() document).}
  \item{ftrs.row}{The row of the matched feature in the new aligned feature table. }
  \item{chr.row}{The row of the matched feature in the new retention time table of aligned features. }
}
\details{The function calculates and updates the mean intensity, variation of intensity, mean retention time etc. }
\value{A vector, the updated row for the known feature table. }
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }