\name{ensembler.regression}
\alias{ensembler.regression}

\title{
Ensemble Regressor Training & Prediction, Model Result Evaluation
}
\description{
This function uses the base learners, and the final top layer learner to produce an ensemble prediction. The user can input the number of base learners and specify the final learner along with the train-validation-test data partition split ratio. The predictions on the unseen new data is the resultant of the ensemble meta-learning of the heterogeneous learners aimed to reduce the generalization error in the predictive models. Functions from \pkg{caret} are used for training and prediction of the base learners and the final learner.  
}

\usage{
ensembler.regression(data
  , outcomeVARIABLEINDEX
  , IndividualModels
  , TopLayerModel
  , dstr
  , dsv
  , dst
  , unseen_new_data
)
}

\arguments{
  \item{data}{Data to be used for training, validation, and test.}
  \item{outcomeVARIABLEINDEX}{Index of the response/outcome variable.}
  \item{IndividualModels}{Training of base learners.}
  \item{TopLayerModel}{Top layer final learner.}
  \item{dstr}{Training data split ratio.}
  \item{dsv}{Validation data split ratio.}
  \item{dst}{Testing data split ratio.}
  \item{unseen_new_data}{Prediction on unseen new data.}
}

\value{
  \item{data}{Complete data or the indexed data.}
  \item{outcomeVARIABLEINDEX}{Integer value of the response/outcome variable index.}
  \item{IndividualModels}{A vector of base learners or standalone individual learner.}
  \item{TopLayerModel}{Name of the Top layer learner.}
  \item{dstr}{fraction integer. Not greater than 1. Total sum of dstr, dsv, dst should be equal to 1.}
  \item{dsv}{fraction integer. Not greater than 1. Total sum of dstr, dsv, dst should be equal to 1.}
  \item{dst}{fraction integer. Not greater than 1. Total sum of dstr, dsv, dst should be equal to 1.}
  \item{unseen_new_data}{Unseen new data provided either as a csv file or imported dataframe.}
}

\note{
The function \code{ensembler.regression}), returns a list with the following elements:
  {ensembler_return[1]}{Test data predictions.}
  {ensembler_return[2]}{Prediction values.}
  {ensembler_return[3]}{Model Result.}
  {ensembler_return[4]}{Unseen data predictions.}

}

\author{
Ajay Arunachalam
}

\examples{
\donttest{
library("metaEnsembleR")
data("rock")
unseen_rock_data <- rock[30:48,]
ensembler_return <- ensembler.regression(rock[1:30,]
  , 4
     ,c('lm')
           , 'rf'
                , 0.40
                     , 0.30
                          , 0.30
                               , unseen_rock_data)
testpreddata <- data.frame(ensembler_return[1])


####Performance comparison#####
modelresult <- ensembler_return[3]
modelresult
#write.csv(modelresult[[1]], "performance_chart.csv")

####unseen data###
unseenpreddata <- data.frame(ensembler_return[4])
}
}


