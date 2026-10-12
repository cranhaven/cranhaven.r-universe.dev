\name{ensembler.classifier}
\alias{ensembler.classifier}

\title{
Ensemble Classifiers Training & Prediction, Model Result Evaluation
}
\description{
This function uses the base learners, and the final top layer learner to produce an ensemble prediction. The user can input the number of base learners and specify the final learner along with the train-validation-test data partition split ratio. The predictions on the unseen new data is the resultant of the ensemble meta-learning of the heterogeneous learners aimed to reduce the generalization error in the predictive models. Functions from \pkg{caret} are used for training and prediction of the base learners and the final learner. 
}

\usage{
ensembler.classifier(data
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
The function \code{ensembler.classifier}), returns a list with the following elements:
  {ensembler_return[1]}{Test data predictions.}
  {ensembler_return[2]}{Prediction labels.}
  {ensembler_return[3]}{Model Result.}
  {ensembler_return[4]}{Unseen data predictions.}

}

\author{
Ajay Arunachalam
}

\examples{
\donttest{
library("metaEnsembleR")
attach(iris)
data("iris")
unseen_new_data_testing <- iris[130:150,]
#write.csv(unseen_new_data_testing
#       , 'unseen_check.csv'
#              , fileEncoding = 'UTF-8'
#                     , row.names = FALSE)
ensembler_return <- ensembler.classifier(iris[1:130,]
  , 5
     , c('rpart') #c('treebag','rpart')
        , 'rf'             # 'gbm'
            , 0.60
                , 0.20
                   , 0.20
                        , unseen_new_data_testing)  
# or
#ensembler_return <- ensembler.classifier(iris[1:130,]
#  , 5
#     , c('treebag','rpart')
#          , 'gbm'
#              , 0.60
#                , 0.20
#                  , 0.20
 #                      , read.csv('./unseen_check.csv'))  

testpreddata <- data.frame(ensembler_return[1])
table(testpreddata$actual_label)
table(ensembler_return[2])

#### Performance comparison #####
modelresult <- ensembler_return[3]
modelresult
act_mybar <- qplot(testpreddata$actual_label, geom="bar")
act_mybar
pred_mybar <- qplot(testpreddata$predictions, geom='bar')
pred_mybar
act_tbl <- tableGrob(t(summary(testpreddata$actual_label)))
pred_tbl <- tableGrob(t(summary(testpreddata$predictions)))
#ggsave("testdata_actual_vs_predicted_chart.pdf",grid.arrange(act_tbl, pred_tbl))
#ggsave("testdata_actual_vs_predicted_plot.pdf",grid.arrange(act_mybar, pred_mybar))

#### unseen data ###
unseenpreddata <- data.frame(ensembler_return[4])
table(unseenpreddata$unseenpreddata)
}
}