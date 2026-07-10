\name{protein}
\alias{protein}
\docType{data}
\title{Protein Secondary Structure Data}
\description{
This dataset contains protein sequences and their corresponding secondary structures, including beta-sheets (E), helices (H), and coils (_).
}
\usage{
protein
}
\format{
A data frame with multiple rows and columns representing protein sequences and their secondary structures.
\itemize{
\item \code{Sequence}: Amino acid sequence (using 3-letter codes).
\item \code{Structure}: Secondary structure of the protein (E for beta-sheet, H for helix, _ for coil).
\item \code{Parameters}: Additional parameters for neural networks (to be ignored).
\item \code{Biophysical_Constants}: Biophysical constants (to be ignored).
}
}
\examples{
# Load the dataset
data(protein)

# Print the first few rows of the dataset
print(head(protein))
}
\keyword{datasets}
