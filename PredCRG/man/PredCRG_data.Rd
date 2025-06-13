\name{PredCRG_data}
\alias{PredCRG_data}
\docType{data}
\title{
Training dataset of the PredCRG model.
}
\description{
The dataset that has been used to train the PredCRG model contains four sub-datasets (Q1, Q2, Q3 and Q4) which are prepared based on the homogeneity of sequence length. The positive sets of the sub-datasets are denoted as pos_Q1, pos_Q2, pos_Q3 and pos_Q4 respectively, whereas the negative sets as neg_Q1, neg_Q2, neg_Q3 and neq_Q4 respectively. Further, same number of sequences are there in both positive and negative sets in each sub-dataset. More clearly, 1588, 1596, 1593 and 1365 sequences are present for both positive and negative sets for Q1, Q2, Q3 and Q4 sub-datasets respectively. Further, the range of the length of the sequences for pos_Q1, pos_Q2, pos_Q3 and pos_Q4 are 39-221, 221-363, 363-538, 538-1000 amino acids respectively, and the range of the length of the sequences for neg_Q1, neg_Q2, neg_Q3 and neg_Q4 are 43-407, 407-485, 485-607 and 607-1000 amino acids respectively. In this dataset, only the Q1 sub-dataset is available due to constraint of space in CRAN. However, one can get all the four sub-datasets from GitHub repository (\url{https://github.com/meher861982/PredCRG_dataset} ).
}
\usage{data("PredCRG_data")}
\format{
The datasets are in \code{\link{AAStringSet}} format, which can be obtained by reading the FASTA file using \code{\link{readAAStringSet}} function availbale in \bold{\code{Biostrings}} package.
}
\details{
The protein sequences encoded by the circadian genes contitutes the positive datasets, whereas a randomly selected dataset from the \bold{Uniprot} for the clad \emph{Viridi plantae} constitutes the negative dataset.
}
\source{
The circadian gene sequecnces are collected from the circadian gene database accessible at \url{http://cgdb.biocuckoo.org/} .
}

\seealso{
\link{PredCRG}, \link{PredCRG_Enc}, \link{PredCRG_training},\link{model1}, \link{model2},\link{model3},\link{model4}
}

\examples{

data(PredCRG_data)
\donttest{
pos_Q1 <- PredCRG_data$pos_Q1 #positive set of Q1 dataset
neg_Q1 <- PredCRG_data$neg_Q1 #negative set of Q1 dataset
}
}
\keyword{Circadian Gene}
\keyword{CRGB database}