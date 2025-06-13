\name{PredCRG_Enc}
\alias{PredCRG_Enc}
\title{
Encoding of protein sequence data in to numeric feature vector based on PredCRG features.
}
\description{
Before using the protein sequences for prediction using the proposed model, the sequences must be transformed into numeric feature vectors. The function \code{PredCRG_Enc} will transform each protein sequnces to a numeric vector of 62 observations, based on the compositional, physico-chemical and transitional features used in the \code{PredCRG} model.
}
\usage{
PredCRG_Enc(prot_seq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prot_seq}{
Sequence dataset to be supplied as input, must be an object of class \code{\link{AAStringSet}}
}
}
\details{
The dataset must contains the protein sequences having standard amino acid residues only. The clas \code{\link{AAStringSet}} can be obtained by reading the FASTA file using \code{\link{readAAStringSet}} available in bioconductor package \bold{\code{Biostrings}}.
}
\value{
A matrix of dimension n*62, for n number of sequences.
}

\author{
Prabina Kumar Meher, ICAR-Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}


\seealso{
\link{PredCRG}, \link{PredCRG_training}, \link{model1}, \link{model2},\link{model3},\link{model4}
}
\examples{
data(test)
enc <- PredCRG_Enc(test)#encoding of test sequence data
enc[1:5,1:5]
  }

\keyword{ FASGAI features }
\keyword{ Crucini properties }
\keyword{Amino acid composition}
