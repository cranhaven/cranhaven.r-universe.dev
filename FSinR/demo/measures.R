library("ucidata")
measures <- list(
  list(measure = binaryConsistency(),
       name = 'Binary Consistency'),
  list(measure = IEConsistency(),
       name = 'IE Consistency'),
  list(measure = IEPConsistency(),
       name = 'IEP Consistency'),
  list(measure = roughsetConsistency(),
       name = 'Rough Set Consistency'),
  list(measure = giniIndex(),
       name = 'Gini Index'),
  list(measure = mutualInformation(),
       name = 'Mutual Information'),
  list(measure = gainRatio(),
       name = 'Gain Ratio'),
  list(measure = symmetricalUncertain(),
       name = 'Symmetrical Uncertain'),
  list(measure = determinationCoefficient(),
       name = 'Determination Coefficient'),
  list(measure = chiSquared(),
       name = 'Chi Squared'),
  list(measure = cramer(),
       name = 'Cramer'),
  list(measure = relief(),
       name = 'Relief'),
  list(measure = MDLC(),
       name = 'MDLC'),
  list(measure = ReliefFeatureSetMeasure(),
       name = 'Relief Feature Set Measure')
  )
datasets <- list(
  list(dataset = wine,
       name = 'wine',
       class = 'color',
       features = c('fixed_acidity','density','quality')),
  list(dataset = adult,
       name = 'adult',
       class = 'income',
       features = c('race','sex','education'))
)
for (dataset in datasets) {
  print(paste("Dataset ",dataset$name))
  print(dataset$features)
  for (measure in measures) {
    value <- measure$measure(dataset$dataset,dataset$class,dataset$features)
    print(paste(measure$name , ': ' , value))
  }
}
# #Wine
# binaryConsistency(wine,'color',)
# IEConsistency(wine,'color',c('fixed_acidity','density','quality'))
# IEPConsistency(wine,'color',c('fixed_acidity','density','quality'))
# roughsetConsistency(wine,'color',c('fixed_acidity','density','quality'))
# giniIndex(wine, 'color',c('fixed_acidity','density','quality'))
# mutualInformation(wine,'color',c('fixed_acidity','density','quality'))
# gainRatio(wine,'color',c('fixed_acidity','density','quality'))
# symmetricalUncertain(wine,'color',c('fixed_acidity','density','quality'))
# determinationCoefficient(wine,'color',c('fixed_acidity','density','quality'))
# chiSquared(wine,'color',c('fixed_acidity','density','quality'))
# cramer(wine,'color',c('fixed_acidity','density','quality'))
# relief(wine,'color',c('fixed_acidity','density','quality'))
# 
# #Adult
# binaryConsistency(adult,'income',c('race','sex','education'))
# IEConsistency(adult,'income',c('race','sex','education'))
# IEPConsistency(adult,'income',c('race','sex','education'))
# roughsetConsistency(adult,'income',c('race','sex','education'))
# giniIndex(adult, 'income',c('race','sex','education'))
# mutualInformation(adult,'income',c('race','sex','education'))
# gainRatio(adult,'income',c('race','sex','education'))
# symmetricalUncertain(adult,'income',c('race','sex','education'))
# determinationCoefficient(adult,'income',)
# chiSquared(adult,'income',c('race','sex','education'))
# cramer(adult,'income',c('race','sex','education'))
# relief(adult,'income',c('race','sex','education'))