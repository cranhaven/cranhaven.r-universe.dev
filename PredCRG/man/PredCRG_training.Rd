\name{PredCRG_training}
\alias{PredCRG_training}

\title{
Training of the PredCRG model using the user supplied sequence dataset.
}
\description{
User can build their own PredCRG model by using their own training dataset. User has to supply the protein sequence dataset of both positive and negative classes having standard amino acid residues only.
}
\usage{
PredCRG_training(pos_seq, neg_seq, kern)
}

\arguments{
  \item{pos_seq}{
circadian protein sequence dataset (also called positive dataset), must be an object of class \code{\link{AAStringSet}}.
}
  \item{neg_seq}{
non-circadian protein sequence dataset (also called negative dataset), must be an object of class \code{\link{AAStringSet}}.
}

\item{kern}{
Type of kernel to be used. It may be \code{laplace}, \code{linear}, \code{polynomial} or \code{RBF}.
}
}
\details{
The sequences must of \code{\link{AAStringSet}} type can be obtained by reading the FASTA file of the sequences using function \code{\link{readAAStringSet}} available in \bold{\code{Biostrings}} package.
}
\value{
Support Vector Machine object of class \code{\link{ksvm}}
}

\author{
Prabina Kumar Meher, ICAR-Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\link{PredCRG}, \link{PredCRG_Enc}, \link{model1}, \link{model2},\link{model3},\link{model4}
}


\examples{
library(kernlab)
pos_Q1 <- PredCRG_data$pos_Q1
neg_Q1 <- PredCRG_data$neg_Q1

#training of the model using laplace kernel.
user_model <- PredCRG_training(pos_seq=pos_Q1[1:100], neg_seq=neg_Q1[1:100], kern="laplace")

data(test)
tst_enc <- PredCRG_Enc(test[1:10])#encoding of the test set
predict(user_model, tst_enc, type="response") #predicting the label of the test instances
predict(user_model, tst_enc, type="probabilities")#predicting the probability of the test instances


library(e1071)
#training of the model using RBF kernel.
user_model <- PredCRG_training(pos_seq=pos_Q1[1:100], neg_seq=neg_Q1[1:100], kern="RBF")
predict(user_model, tst_enc, probability=TRUE) #Predicting probability
predict(user_model, tst_enc) #Predicting labels
}



\keyword{ SVM }
\keyword{Computationa prediction }
