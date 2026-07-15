#if !defined(OPTIONS_H)
#define OPTIONS_H

#include <cstdio>

#include "general.h"
#include "contain.h"
#include "mstring.h"

enum splitSelectionType {FROM_FILES=0, CROSS_VALIDATION=1, STRATIFIED_CV=2,
                         LOOCV=3, ALL_TRAINING=4, RANDOM_SPLIT=5 } ;

class Options {
public:
   // command line options
   mstring optionFile ;
   mstring action ;

   // data options
   mstring domainName ;
   mstring dataDirectory ;
   mstring resultsDirectory ;
   mstring NAstring ;
   int splitIdx ;
   int numberOfSplits ;
   splitSelectionType splitSelection ;
   double trainProportion ;
   int rndSeedSplit ;


   // building options
   double minInstanceWeight ; // minimal probability of example to take it into further consideration
   double minReliefEstimate ; // minimal ReliefF's estimation to consider attribute worthy
   int selectionEstimator, constructionEstimator ;
   int selectionEstimatorReg, constructionEstimatorReg ;


   // attribute evaluation
   int attrEvaluationInstances ;  // maximal examples for estimation
   booleanT binaryEvaluation ;
   booleanT binaryEvaluateNumericAttributes ; // are numeric attributes' splits considered binary (or greedily discretized) in applicable measures
   int multiclassEvaluation;
   marray<booleanT> estOnReg;
   marray<booleanT> estOn;

   // ReliefF
   int ReliefIterations ; // number of ReliefF's main loops for estimation
   int kNearestEqual, kNearestExpRank  ;
   double quotientExpRankDistance ;
   double numAttrProportionEqual, numAttrProportionDifferent ;

   // ordEval
   int ordEvalNoRandomNormalizers ;
   booleanT ordEvalBootstrapNormalize ;
   double ordEvalNormalizingPercentile;
   marray<double> attrWeights ;

   // stopping options
   double minNodeWeightTree ; // minimum number of examples in a node to split further; used  for decision and regression trees
   double minNodeWeightRF ; // minimum number of examples in a node to split further; used in random forest
   double minNodeWeightEst ; // minimum number of examples in a split to consider it valid; used in all attribute estimation tasks: trees, RF, binarization, discretization
   double relMinNodeWeight ; // minimal proportion of examples in a leaf to spit further; used in decision trees, regression trees, and random forests
   double majorClassProportion ;
   double rootStdDevProportion ;
   double minNonMajorityWeight ;

   //  models in trees
   int modelType, modelTypeReg ;  // type of models in leaves
   int kInNN ;
   double nnKernelWidth ;

   // constructive induction
   int constructionMode ; // what constructs to consider
   int constructionDepth ;
   int beamSize, maxConstructSize ;
   int noCachedInNode ;

   // discretization
   int discretizationLookahead ; // number of times current discretization can be worse than the best
   int discretizationSample ;
   int bayesDiscretization ;
   int discretizationIntervals ;
   int maxValues4Exhaustive ; // maximal values of discrete attribute values to try exhaustive binarization
   int maxValues4Greedy ; // maximal values of discrete attribute values to try greedy binarization;
                          // if an attribute has more values than this, random binarization is tried

   // pruning
   int selectedPruner ;
   int selectedPrunerReg ;
   double mEstPruning ; // parameter for m-estimate in pruning
   double mdlModelPrecision ;
   double mdlErrorPrecision ;
   double alphaErrorComplexity ;

   // probability smoothing
   int smoothingType ;
   double smoothingValue ;

   // random forest options
   int rfNoTrees  ;
   int rfNoSelAttr ;
   booleanT rfMultipleEst ;
   int rfkNearestEqual ;
   double rfPropWeightedTrees ;
   booleanT rfPredictClass ;
   booleanT rfRandomBinarization ;
   booleanT rfAttrEvaluate ;
   double rfSampleProp ;
   int rfNoTerminals ;
   int rfRegType ;
   double rfRegLambda ;
   int rfRndSeed ;

   // miscellaneous
   int maxThreads ;
   booleanT printTreeInDot ;
   booleanT outProbDistr ;
   mstring defaultEditor ;

   // constructors
   Options(void) { setDefault() ; }
   Options(Options &cp) { copy(cp) ; }

   // methods
   void copy(const Options &cp) ;
   void setDefault(void) ;
   void processOptions(void) ;
   int readConfigFromString(char* optionsString) ;
   void parseOption(char *optString, char *keyword, char *key) ;
   void assignOption(char *optString) ;
   void assignOption(char *keyword, char *key)  ;
   int optionsFromStrings(int noOptions, marray<char* > &optionsName, marray<char* > &optionsVal) ;

   int readConfig(char* ConfigName) ;
   void outConfig(FILE *to) const ;
   int writeConfig(char* ConfigName) const ;


} ;

#endif
