\name{PredCRG}
\alias{PredCRG}
\title{
Prediction of circadian proteins using the proposed PredCRG model.
}
\description{
The user can predict the protein sequences as CRG (circadian protein) or non-CRG (non-circadian protein) with certain probability by supplying the test sequences.
}
\usage{
PredCRG(seq_data)
}

\arguments{
  \item{seq_data}{
Sequence dataset in FASTA format consisting of protein sequences with standard amino acid residues only. It must be an object of class \code{\link{AAStringSet}} which can be obtained by reading sequences with \code{\link{readAAStringSet}} available in \bold{\code{Biostrings}} package.
}
}
\details{
The user has to supply only the \code{seq_data} for which the prediction is to be made. 
}
\value{
A dataframe with three columns consisting of sequence name, predicted labels of sequences (CRG or non-CRG) and probabilities of prediction.
}

\author{
Prabina Kumar Meher, ICAR-Indian Agricultural Statsitics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\link{PredCRG_Enc}, \link{PredCRG_training},\link{model1}, \link{model2},\link{model3},\link{model4}
}


\examples{
data(test)
tst <- test[1:10]
PredCRG(seq_data=tst)
}

\keyword{AAStringSet}
\keyword{Biostrings}
\keyword{protr}
\keyword{Peptides}