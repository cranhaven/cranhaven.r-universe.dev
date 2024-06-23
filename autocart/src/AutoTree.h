#ifndef AUTOCART_AUTOTREE_H
#define AUTOCART_AUTOTREE_H

#include <RcppArmadillo.h>
using namespace Rcpp;

/*
 * The node structure contains the column that we make
 * a decision on, along with a value in that column.
 * If the data point is "true", then go right, and if "false",
 * then go left.
 */
struct node {
  double key;
  int factor;
  //int column;
  String column;
  int obsInNode;
  double prediction;
  bool isTerminalNode;
  bool isCategoricalSplit;

  // Pointers to the data in this node
  NumericVector response;
  DataFrame data;
  NumericMatrix locations;
  IntegerVector weightsIndices;

  // Evaluation measures at each node
  double RSS;
  double mi;
  double miSD;
  double gc;

  node* left;
  node* right;
};

/* This enumeration is used for the different spatial weighting options */
namespace SpatialWeights {
enum Type {
  Regular,
  Gaussian,
  Custom
};
}

/* Various helper methods */
double findMax(NumericVector x);
NumericMatrix matrixSubsetCells(NumericMatrix x, IntegerVector rIndex, IntegerVector cIndex);

/*
 * The AutoTree class contains the organization of all the decision rules
 * and nodes.
 */
class AutoTree {
public:
  AutoTree(double alpha_, double beta_, int minsplit_, int minbucket_, int maxdepth_, int distpower_, int maxobsMtxCalc_, bool islonglat_, bool useGearyC_, bool saddlepointApproximation_, bool useParallelCalculations_, bool asForest_, int asForestMTry_, SpatialWeights::Type spatialWeightsType_, double spatialBandwidth_, NumericMatrix globalSpatialWeightsMatrix_, NumericMatrix globalDistanceMatrix_);
  ~AutoTree();

  void destroyTree();
  void createTree(NumericVector response, DataFrame data, NumericMatrix locations);

  DataFrame createSplitDataFrame();
  double predictObservation(NumericVector predictors);
  NumericVector predictDataFrame(DataFrame data);

  // Getters / Setters
  int getNumTerminalNodes();
  int getMinSplit();
  int getMinBucket();
  int getMaxDepth();
  int getDistPower();
  int getMaxObsMtxCalc();
  bool getIsLongLat();
  bool isGearyC();
  bool getSaddlepointApproximation();
  bool getUseParallelCalculations();
  double getAlpha();
  double getBeta();
  double getSpatialBandwidth();
  SpatialWeights::Type getSpatialWeightsType();

private:
  node* root;
  int obsToCreate = 0; // The number of observations in DataFrame used to create tree
  int nodesInTree = 0;
  int numTerminalNodes = 0;

  // autocartControl parameters
  int minsplit;
  int minbucket;
  int maxdepth;
  int distpower;
  int maxobsMtxCalc;
  int asForestMTry;
  bool islonglat;
  bool useGearyC;
  bool saddlepointApproximation;
  bool useParallelCalculations;
  bool asForest;
  double alpha;
  double beta;
  double spatialBandwidth;
  SpatialWeights::Type spatialWeightsType;

  // We keep a copy of the spatial weights matrix and distance matrix for use throughout the splitting
  NumericMatrix globalSpatialWeightsMatrix;
  NumericMatrix globalDistanceMatrix;

  void destroyTree(node* leaf);
  node* createNode(NumericVector response, DataFrame data, NumericMatrix locations, IntegerVector weightsIndices, int level, int numObs);
  NumericVector split(NumericVector response, NumericVector x, NumericMatrix locations, NumericMatrix spatialWeightsMatrix, NumericMatrix distanceMatrix);
  NumericVector splitCategorical(NumericVector response, IntegerVector x, NumericMatrix locations, NumericMatrix spatialWeightsMatrix, NumericMatrix distanceMatrix);

  // Output
  void inorderPrint();
  void inorderPrint(node* leaf, int level);
  void preorderPrint();
  void preorderPrint(node* leaf, int level);
  void printNode(node* x);
};

#endif
