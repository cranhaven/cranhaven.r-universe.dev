/*
 * This file contains the code that will create a regression tree based upon
 * a passed in DataFrame with predictor values and an associated response
 * variable. By default, the DataFrame is assumed to only contain predictor variables
 * and no missing values. The response vector should be numeric and have the
 * same number of elements as the DataFrame has rows.
 *
 * The purpose of this modified regression tree code is to enable the usage of
 * geographical location while splitting. The knowledge of geographical
 * location allows the splitting function to optimize for chunks that retain a
 * high measure of spatial autocorrelation.
 *
 * The intended usage of this class is to group observations in a DataFrame
 * such that the individual buckets in the resulting regression tree also
 * represent areas where a spatial effect can be assumed. This code allows
 * one to break a global spatial process into smaller subprocesses, as in many
 * ecological applications one can not assume the same process at every size of
 * field.
 *
 * @author Ethan Ancell
 */
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <math.h>
#include <stack>
// #include <bits/stdc++.h>
#include "AutoTree.h"
#include "SpatialMethods.h"
#include "SplittingMethods.h"

using namespace Rcpp;

// Set all the parameters for the tree creation
AutoTree::AutoTree(double alpha_, double beta_, int minsplit_, int minbucket_, int maxdepth_, int distpower_, int maxobsMtxCalc_, bool islonglat_, bool useGearyC_, bool saddlepointApproximation_, bool useParallelCalculations_, bool asForest_, int asForestMTry_, SpatialWeights::Type spatialWeightsType_, double spatialBandwidth_, NumericMatrix globalSpatialWeightsMatrix_, NumericMatrix globalDistanceMatrix_) {
  root = NULL;

  // Error check the parameters
  if (alpha_ < 0 || alpha_ > 1) {
    stop("Creation of autotree failed. Alpha value not between 0 and 1.");
  }
  if (beta_ < 0 || beta_ > 1) {
    stop("Creation of autotree failed. Beta value not between 0 and 1.");
  }
  if (alpha_ + beta_ > 1) {
    stop("Creation of autotree failed. Alpha and beta can not sum to anything above 1.");
  }

  // Set the autocart control parameters
  alpha = alpha_;
  beta = beta_;
  minsplit = minsplit_;
  minbucket = minbucket_;
  maxdepth = maxdepth_;
  distpower = distpower_;
  maxobsMtxCalc = maxobsMtxCalc_;
  islonglat = islonglat_;
  useGearyC = useGearyC_;
  spatialWeightsType = spatialWeightsType_;
  spatialBandwidth = spatialBandwidth_;
  globalDistanceMatrix = globalDistanceMatrix_;
  globalSpatialWeightsMatrix = globalSpatialWeightsMatrix_;
  useParallelCalculations = useParallelCalculations_;
  asForest = asForest_;
  asForestMTry = asForestMTry_;
}

// Kick off the splitting
void AutoTree::createTree(NumericVector response, DataFrame data, NumericMatrix locations) {

  if (root == NULL) {
    // Error check
    if (response.size() != data.nrows()) {
      stop("Creation of autotree failed. Response vector not same length as the number of rows in the data matrix.");
    }
    if (response.size() != locations.rows()) {
      stop("Creation of autotree failed. Response vector not same length as number of rows in the locations matrix.");
    }
    if (locations.cols() != 2) {
      stop("Creation of autotree failed. Locations matrix should only have two columns.");
    }

    // Check to see if any NA, NaN, or Inf exists
    // ------------------------------------------
    // Check response
    for (int i=0; i<response.size(); i++) {
      if (NumericVector::is_na(response[i])) {
        stop("NA found in response vector. Consider imputation or removing rows with NA values.");
      }
    }

    // Check DataFrame. If any factor variables exist, then we need to document
    // those as they require special splitting rules compared to continuous predictors.
    for (int j=0; j<data.length(); j++) {

      // Error check to make sure it's all numeric vectors
      if (TYPEOF(data[j]) != REALSXP && TYPEOF(data[j]) != INTSXP) {
        //Rcout << "Column " << j << " supposed to be " << REALSXP << ", but was actually " << TYPEOF(data[j]) << std::endl;
        stop("All dataframe columns must be numeric vectors or factors.");
      }

      NumericVector temp = data[j];
      for (int i=0; i<temp.size(); i++) {
        if (NumericVector::is_na(temp[i])) {
          stop("NA found in dataframe. Consider imputation or removing rows with NA values.");
        }
      }
    }

    // Keep track of the # of DataFrame rows that were used to create the tree
    // (This is used for some types of stopping criteria)
    obsToCreate = response.size();
    Function subset("subset");

    DataFrame nodeData;
    DataFrame leftDataFrame;
    DataFrame rightDataFrame;

    NumericVector nodeResponse;
    NumericVector leftResponse;
    NumericVector rightResponse;

    NumericMatrix nodeLocations;
    NumericMatrix leftLocations;
    NumericMatrix rightLocations;

    IntegerVector weightsIndices = Rcpp::Range(0, response.size()-1);
    IntegerVector leftWeightsIndices;
    IntegerVector rightWeightsIndices;

    NumericVector splitColumnVector;
    LogicalVector isLeft;
    bool isCategoricalSplit;

    // Create tree non-recursively using a stack
    std::stack<node*> treeCreationStack;
    root = createNode(response, data, locations, weightsIndices, 0, response.size());

    treeCreationStack.push(root);
    nodesInTree++;
    while (!treeCreationStack.empty()) {
      // Take the node off the stack and try to assign its children
      node* nextNode = treeCreationStack.top();
      treeCreationStack.pop();

      // DEBUG 729
      // printNode(nextNode);

      // Error check
      if (nextNode->isTerminalNode) {
        // Rcout << "Node: " << nodesInTree << std::endl;
        stop("Error in autocart: no first split could be created. Most likely, there is a problem with the spatial weights matrix.");
      }

      // Split the data according to what's contained in "nextNode"
      nodeData = nextNode->data;
      nodeResponse = nextNode->response;
      nodeLocations = nextNode->locations;
      weightsIndices = nextNode->weightsIndices;
      isCategoricalSplit = nextNode->isCategoricalSplit;

      splitColumnVector = nodeData[nextNode->column];

      // Make split according to if it's a continuous split or a categorical split
      if (!isCategoricalSplit) {
        isLeft = splitColumnVector <= nextNode->key;
      }
      else {
        isLeft = splitColumnVector != nextNode->factor;
      }

      leftResponse = nodeResponse[isLeft];
      rightResponse = nodeResponse[!isLeft];
      leftDataFrame = subset(nodeData, isLeft);
      rightDataFrame = subset(nodeData, !isLeft);
      leftLocations = subset(nodeLocations, isLeft);
      rightLocations = subset(nodeLocations, !isLeft);
      leftWeightsIndices = weightsIndices[isLeft];
      rightWeightsIndices = weightsIndices[!isLeft];

      // I have partitioned my data above. In any case, I want to create the nodes for
      // both children. If a node happens to have numObs > minsplit, then I can push that
      // node onto the stack in order to keep splitting it down. I will know that a node has
      // numObs > minsplit if the returned node from "createNode" function is NOT a terminal node.
      node* leftnode = createNode(leftResponse, leftDataFrame, leftLocations, leftWeightsIndices, 0, leftResponse.size());
      node* rightnode = createNode(rightResponse, rightDataFrame, rightLocations, rightWeightsIndices, 0, rightResponse.size());
      nodesInTree+=2;

      nextNode->left = leftnode;
      nextNode->right = rightnode;

      if (!leftnode->isTerminalNode) {
        treeCreationStack.push(leftnode);
      }
      if (!rightnode->isTerminalNode) {
        treeCreationStack.push(rightnode);
      }
    }

    // Uncomment if you want to view the exact structure of the tree via a
    // pre-order print to the console.
    // preorderPrint();
  }
  else {
    stop("A tree has already been created with this C++ object!");
  }
}

// Recursive splitting function
node* AutoTree::createNode(NumericVector response, DataFrame data, NumericMatrix locations, IntegerVector weightsIndices, int level, int numObs) {

  // FIND INFORMATION ABOUT THIS NODE
  // ----------------------------------------------------
  // Find the average response value in this particular group
  double averageResponse = 0;
  for (int i=0; i<response.size(); i++) {
    averageResponse += response[i];
  }
  averageResponse = averageResponse / response.size();
  // Find the residual sum of squares for this group
  double RSS = 0;
  for (int i=0; i<response.size(); i++) {
    RSS += pow(response[i] - averageResponse, 2);
  }
  // Get the morans I for this group
  double groupMoranI = 0;
  NumericMatrix nodeWeights = getWeightsMatrix(locations, distpower, islonglat, spatialBandwidth, spatialWeightsType, getUseParallelCalculations());
  //groupMoranI = moranI(response, nodeWeights);
  if (getUseParallelCalculations()) {
    groupMoranI = moranIParallel(response, nodeWeights);
  }
  else {
    groupMoranI = moranI(response, nodeWeights);
  }
  // SD of Moran's I
  double groupMoranISD = 0;
  groupMoranISD = moranIVariance(response, nodeWeights);
  groupMoranISD = sqrt(groupMoranISD);
  // Get Geary's C for this group
  double groupGearyC = gearyC(response, nodeWeights);

  // SHOULD WE ASSIGN THIS NODE AS A TERMINAL NODE AND BE DONE?
  // -------------------------------------------------------------
  // If we meet the stopping conditions on this node, then we can stop and call this a terminal node.
  if (level > maxdepth || numObs < minsplit) {
    node* newnode = new node{-1, -1, "N/A", numObs, averageResponse, true, false, response, data, locations, weightsIndices, RSS, groupMoranI, groupMoranISD, groupGearyC, NULL, NULL};
    return newnode;
  }

  // If this is splitting as a forest, then we need to change the predictor pool that we are sampling from.
  std::vector<int> predictorPool;
  int poolSize = data.length();
  for (int i=0; i<data.length(); i++) {
    predictorPool.push_back(i);
  }
  if (asForest) {
    // Use R to randomly sample a vector.
    Function rSample("sample");
    NumericVector initialPool(data.length());
    for (int i=0; i<data.length(); i++) {
      initialPool[i] = i;
    }
    initialPool = rSample(initialPool, asForestMTry);
    std::vector<int> newPool;
    for (int i=0; i<asForestMTry; i++) {
      newPool.push_back(initialPool[i]);
    }
    predictorPool = newPool;
    poolSize = asForestMTry;

    // Print the predictor pool for debugging
    /*Rcout << "predictor pool: ";
    for (int i=0; i<asForestMTry; i++) {
      Rcout << predictorPool[i] << ", ";
    }*/
    //Rcout << std::endl;
  }

  // THIS IS NOT A TERMINAL NODE. PROCEED TO FIND THE BEST SPLIT THAT WE CAN HERE.
  // -----------------------------------------------------------------------------
  // Loop through all the columns, finding the best split
  String bestColumn = 0;
  int bestSplit = 0;
  double maxGoodness = 0;
  bool betterSplitFound = false;
  bool bestSplitIsCategorical = false;

  // Construct the weights/distance matrix for this node based off the indices
  NumericMatrix spatialWeightsMatrix = matrixSubsetCells(globalSpatialWeightsMatrix, weightsIndices, weightsIndices);
  NumericMatrix distanceMatrix = matrixSubsetCells(globalDistanceMatrix, weightsIndices, weightsIndices);

  CharacterVector dataframeNames = data.names();
  for (int columnIndex=0; columnIndex<poolSize; columnIndex++) {

    String column = dataframeNames[predictorPool[columnIndex]];

    /* We find the "goodness" vector returned by the splitting function.
     * if there is a goodness value that is better than the best one we have,
     * then we make a note of the column we are splitting on, the location of the split,
     * and also the goodness value of that split.
     */
    NumericVector goodnessVector;
    bool splitByCat;
    // The data might be categorical date, in which case we need a different splitting function.
    if (Rf_isFactor(data[column])) {
      goodnessVector = splitCategorical(response, data[column], locations, spatialWeightsMatrix, distanceMatrix);
      splitByCat = true;
    }
    else {
      goodnessVector = split(response, data[column], locations, spatialWeightsMatrix, distanceMatrix);
      splitByCat = false;
    }

    // Replace all NaNs with 0
    for (int tt=0; tt<goodnessVector.size(); tt++) {
      if (NumericVector::is_na(goodnessVector[tt])) {
        goodnessVector[tt] = 0;
      }
    }

    // DEBUG 729
    // Rcout << "column: " << columnIndex << std::endl;
    // Rcout << "Goodness: " << goodnessVector << std::endl;

    double tempGoodness = findMax(goodnessVector);
    if (tempGoodness > maxGoodness) {
      // We found a better split than the one we have currently
      bestColumn = column;
      bestSplit = which_max(goodnessVector);
      maxGoodness = tempGoodness;
      betterSplitFound = true;

      // Partitions occur differently with categorical data, so we need to
      // keep track if we have a best split that occurs on a categorical column
      if (splitByCat) {
        bestSplitIsCategorical = true;
      }
      else {
        bestSplitIsCategorical = false;
      }
    }
  }

  // Rcout << "maxGoodness: " << maxGoodness << std::endl;

  // If no better split is ever found, then we can simply call this a terminal node, just
  // like we did with the stopping condition.
  if (!betterSplitFound) {
    // Objective function vector is likely a vector of zeros. What does the response look like?
    /*
    Rcout << "Response: " << response << std::endl;
    Rcout << "Data: " << std::endl;
    for (int columnIndex=0; columnIndex<poolSize; columnIndex++) {
      std::string column = Rcpp::as<std::string>(dataframeNames[predictorPool[columnIndex]]);
      // std::string cname = Rcpp::as<std::string>(column);
      Rcout << column << ": " << data[String(column)] << std::endl;
    }
    */
    node* newnode = new node{-1, -1, "N/A", numObs, averageResponse, true, false, response, data, locations, weightsIndices, RSS, groupMoranI, groupMoranISD, groupGearyC, NULL, NULL};
    return newnode;
  }

  // Split according to categorical data or continuous data
  double splitValue;
  int factor;

  if (bestSplitIsCategorical) {
    IntegerVector x = data[bestColumn];
    factor = x[bestSplit];

    // Dummy value that doesn't get used.
    splitValue = -1.0;
  }
  else {
    Function f("order");
    NumericVector x = data[bestColumn];
    // DEBUG 729
    // Rcout << "X (unordered): " << x << std::endl;
    NumericVector order_x = f(x);
    order_x = order_x - 1;
    x = x[order_x];
    // Rcout << "X (ordered): " << x << std::endl;
    splitValue = x[bestSplit];
    // Set to dummy value. Shouldn't ever be used, but node struct can't be modified
    factor = -1;
  }

  node* newnode = new node{splitValue, factor, bestColumn, numObs, averageResponse, false, bestSplitIsCategorical, response, data, locations, weightsIndices, RSS, groupMoranI, groupMoranISD, groupGearyC, NULL, NULL};

  return newnode;
}

/* Given an x_vector (predictor), return a vector of length size(x_vector) - 1
 * with goodness values. The goodness value at location "i" evaluates the split
 * from 1:i vs i+1:n, where n is the length of the response/x_vector.
 */
NumericVector AutoTree::split(NumericVector response, NumericVector x_vector, NumericMatrix locations, NumericMatrix spatialWeightsMatrix, NumericMatrix distanceMatrix) {

  int n = response.size();
  NumericVector wt(n, 1.0);
  NumericVector y = clone(response);
  NumericVector x = clone(x_vector);

  NumericMatrix orderedLocations(n, 2);
  NumericMatrix orderedSpatialWeightsMatrix = Rcpp::no_init(n, n);
  NumericMatrix orderedDistanceMatrix = Rcpp::no_init(n, n);

  // The three terms used in the splitting
  // t1: reduction in variance
  // t2: spatial autocorrelation
  // t3: pairwise distances
  NumericVector t1(n-1, 0.0);
  NumericVector t2(n-1, 0.0);
  NumericVector t3(n-1, 0.0);

  // Order everything by x_vector
  Function f("order");
  IntegerVector x_order = f(x_vector);
  x_order = x_order - 1;

  y = y[x_order];
  x = x[x_order];
  for (int i=0; i<n; i++) {
    int slotLocation = x_order[i];
    orderedLocations(slotLocation, _) = locations(i, _);
  }

  // DEBUG 729
  // Rcout << "(SPLIT) orderedX: " << x << std::endl;

  // Order the weights/distance matrices in the same order as x_order
  for (int i=0; i<n; i++) {
    for (int j=0; j<n; j++) {
      orderedSpatialWeightsMatrix(i, j) = spatialWeightsMatrix(x_order[i], x_order[j]);
      orderedDistanceMatrix(i, j) = distanceMatrix(x_order[i], x_order[j]);
    }
  }

  // Only compute non-zero coefficients
  if ((alpha+beta) < 1) {
    t1 = continuousGoodnessByVariance(y, x, wt, minbucket, getUseParallelCalculations());
  }
  if (alpha > 0) {
    if (n <= maxobsMtxCalc) {
      t2 = continuousGoodnessByAutocorrelation(y, x, orderedLocations, orderedSpatialWeightsMatrix, wt, minbucket, distpower, islonglat, useGearyC, saddlepointApproximation, spatialBandwidth, spatialWeightsType, getUseParallelCalculations());
    }
  }
  if (beta > 0) {
    if (n <= maxobsMtxCalc) {
      t3 = continuousGoodnessBySize(x, orderedLocations, orderedDistanceMatrix, wt, minbucket, islonglat, getUseParallelCalculations());
      //t3 = continuousGoodnessBySizeConvexHull(orderedLocations, minbucket);

      //t3 = continuousGoodnessBySize(orderedLocations, getMinBucket());
      //t3 = continuousGoodnessBySizeOld(x, orderedLocations, orderedDistanceMatrix, wt, minbucket, islonglat);
      //Rcout << t3 << std::endl;
      //t3 = continuousGoodnessBySeparation(Rcpp::as<arma::mat>(orderedLocations), n, minbucket);
      //stop("bruh");
      //t3 = continuousGoodnessBySeparationOld(orderedLocations, orderedDistanceMatrix, minbucket, islonglat);
      //t3 = continuousGoodnessBySeparation(as<arma::mat>(orderedLocations), n, minbucket);
    }
  }

  // DEBUG: what do the objective function vectors look like?
  // Rcout << "t1: " << t1 << std::endl;
  // Rcout << "t2: " << t2 << std::endl;
  // stop("");

  // Return the linear combination of the goodness values
  t1 = (1-alpha-beta) * t1;
  t2 = alpha * t2;
  t3 = beta * t3;

  NumericVector goodness = t1 + t2 + t3;

  // BUG FIX FROM 7/29/2020
  // Sometimes when you have an ordered x_vector {x, x, x, x, 6, 6, 6, 6, 6, 6} goodness would be {g, g, g, g, g, g, g, g, g}
  // g = {g, g, g, g, g, 0, 0, 0, 0}
  // with minbucket=5, 6 gets chosen as best split which creates a "right" child
  // vector of length 0. If we perform a quick check such that if ordered x @ location of "n-minbucket-1" is
  // the same as x[n-1] then set goodness values starting at g[n-minbucket] until to when x stops being the same as x[n-1]
  // which stops those splits from being chosen.
  if (x[n-minbucket-1] == x[n-1]) {
    double endingX = x[n-1];
    int i = n-minbucket-1;
    while (x[i] == endingX && i >= 0) {
      goodness[i] = 0.0;
      i--;
    }
  }

  // Optional output for weird crashes
  // Rcout << goodness << std::endl;

  return goodness;
}

/* Given an x_vector (predictor), return a vector of length length(levels(x_vector)) (the number of factors)
 * with goodness values. The goodness value at location "i" evaluates the group containing factor i vs
 * the group not containing factor i.
 */
NumericVector AutoTree::splitCategorical(NumericVector response, IntegerVector x_vector, NumericMatrix locations, NumericMatrix spatialWeightsMatrix, NumericMatrix distanceMatrix) {

  int n = response.size();

  // Make a weights vector. This should probably be modified later, but for now
  // it will be a vector of ones.
  NumericVector wt(response.size(), 1.0);
  CharacterVector lvls = x_vector.attr("levels");
  int numLevels = lvls.size();

  // The three terms used in the splitting
  // t1: reduction in variance
  // t2: spatial autocorrelation
  // t3: pairwise distances (size)
  NumericVector t1(numLevels, 0.0);
  NumericVector t2(numLevels, 0.0);
  NumericVector t3(numLevels, 0.0);

  if ((alpha+beta) < 1) {
    t1 = categoricalGoodnessByVariance(response, x_vector, wt, minbucket, getUseParallelCalculations());
  }
  if (alpha > 0) {
    if (n <= maxobsMtxCalc) {
      t2 = categoricalGoodnessByAutocorrelation(response, x_vector, locations, spatialWeightsMatrix, wt, minbucket, distpower, islonglat, useGearyC, saddlepointApproximation, spatialBandwidth, spatialWeightsType, getUseParallelCalculations());
    }
  }
  if (beta > 0) {
    if (n <= maxobsMtxCalc) {
      t3 = categoricalGoodnessBySize(x_vector, locations, distanceMatrix, wt, minbucket, islonglat, getUseParallelCalculations());
    }
  }

  // Return the linear combination of goodness values
  t1 = (1-alpha-beta) * t1;
  t2 = alpha * t2;
  t3 = beta * t3;
  NumericVector objective = t1 + t2 + t2;

  // Optional output for weird crashes
  // Rcout << objective << std::endl;

  return objective;
}

/*
 * Return a prediction for a given observation. The input requires a
 * numeric vector of all the predictor variables. This might be changed to a
 * single-rowed DataFrame later, but for the time being the "ith" observation
 * of the NumericVector corresponds to the "ith" column of the DataFrame that
 * was used to create the tree.
 */
double AutoTree::predictObservation(NumericVector predictors) {
  node* iterNode;
  iterNode = root;
  while (!iterNode->isTerminalNode) {
    // travel down the children according to the split
    std::string splitColumn = iterNode->column;

    // "containsElementNamed()" requires a C style string for some reason...
    char* splitColumnCstr = new char[splitColumn.length()+1];
    std::strcpy(splitColumnCstr, splitColumn.c_str());

    // Make sure that it exists in the predictors
    if (!predictors.containsElementNamed(splitColumnCstr)) {
      CharacterVector pNames = predictors.names();
      Rcout << "The variable named " << splitColumn << " does not exist in predictors." << std::endl;
      Rcout << "Predictors: " << pNames << std::endl;
      stop("Can not proceed with predictions.");
    }

    delete[] splitColumnCstr;
    int columnIndex = predictors.offset(splitColumn);

    if (iterNode->isCategoricalSplit) {
      int splitFactor = iterNode->factor;

      if (predictors[columnIndex] == splitFactor) {
        iterNode = iterNode->right;
      }
      else {
        iterNode = iterNode->left;
      }
    }
    else {
      double splitValue = iterNode->key;

      if (predictors[columnIndex] <= splitValue) {
        iterNode = iterNode->left;
      }
      else {
        iterNode = iterNode->right;
      }
    }
  }

  // When we have landed on the terminal node, we can return the prediction
  // contained in that terminal node.
  return iterNode->prediction;
}

/*
 * Return a numeric vector with the predicted response values for each of the
 * rows contained in the DataFrame that's passed in
 */
NumericVector AutoTree::predictDataFrame(DataFrame data) {
  int nRows = data.nrows();
  int nCols = data.size();
  NumericVector predictions(nRows);
  CharacterVector dataNames = data.names();

  for (int i=0; i<nRows; i++) {

    NumericVector x;
    for (int j=0; j<nCols; j++) {
      String thisColumnName = dataNames[j];
      NumericVector columnData = data[j];
      x.push_back(columnData[i], thisColumnName);
    }
    double result = predictObservation(x);
    predictions[i] = result;
  }

  return predictions;
}

/* After the tree has been created, we often wish to create new predictions
 * from observations that were not used in the creation of the tree.
 * In order for this to be accessed in the R environment, we require an S3
 * object. This function creates the dataframe that contains the splitting
 * information so that new predictions can be obtained.
 *
 * column: The index of the column that is being split on
 * splitvalue: If <= splitvalue, go left in tree. If > splitvalue, go right.
 * leftloc: The row in the dataframe to jump to if <= splitvalue
 * rightloc: The row in the dataframe to jump to if > splitvalue
 * isterminal: A boolean for if this is a leaf node or not
 * prediction: The prediction for an observation that lands in this node.
 */
DataFrame AutoTree::createSplitDataFrame() {

  // Error check
  if (root == NULL) {
    stop("No tree exists. Impossible to create the splitting dataframe.");
  }

  // These vectors will make up the columns in the splitting dataframe
  CharacterVector column(nodesInTree);
  NumericVector splitvalue(nodesInTree);
  IntegerVector category(nodesInTree);
  IntegerVector numobs(nodesInTree);
  LogicalVector isterminal(nodesInTree);
  LogicalVector iscategorical(nodesInTree);
  NumericVector prediction(nodesInTree);
  IntegerVector leftloc(nodesInTree, -1);
  IntegerVector rightloc(nodesInTree, -1);

  // Node evaluation (spatial autocorrelation and residual sum of squares)
  NumericVector rss(nodesInTree);
  NumericVector mi(nodesInTree);
  NumericVector miSD(nodesInTree);
  NumericVector gc(nodesInTree);
  NumericVector expectedMi(nodesInTree);
  NumericVector expectedGc(nodesInTree);

  // Create the splitting dataframe using a stack
  std::stack<node*> dfCreationStack;
  std::stack<int> rowLocations;
  int row = 0;
  dfCreationStack.push(root);
  rowLocations.push(row);

  while (!dfCreationStack.empty()) {
    // Take the node off the stack and add an element to the vectors above
    node* nextNode = dfCreationStack.top();
    dfCreationStack.pop();

    // Tells you what row in the dataframe this node will be sent to
    int thisRow = rowLocations.top();
    rowLocations.pop();

    // Send information in the node to the dataframe
    column[thisRow] = nextNode->column;
    splitvalue[thisRow] = nextNode->key;
    category[thisRow] = nextNode->factor;
    numobs[thisRow] = nextNode->obsInNode;
    isterminal[thisRow] = nextNode->isTerminalNode;
    prediction[thisRow] = nextNode->prediction;
    iscategorical[thisRow] = nextNode->isCategoricalSplit;
    rss[thisRow] = nextNode->RSS;
    mi[thisRow] = nextNode->mi;
    miSD[thisRow] = nextNode->miSD;
    gc[thisRow] = nextNode->gc;

    // Expected value of Moran's I is calculated as -1 / (N-1)
    expectedMi[thisRow] = -1.0 / (nextNode->obsInNode - 1);

    // The expected value of Geary's C is always going to be 1. The only reason to include it is so that
    // I stay consistent with including the expected value of Moran's I in the datatable and I don't want to expect
    // that users of the package know that E(C) = 1
    expectedGc[thisRow] = 1.0;

    // Push the children if they exist and add to the left/right locations
    if (nextNode->left != NULL && nextNode->right != NULL) {
      // For both left and right sides, we use row+1 instead of row because
      // R is 1-indexed rather than 0-indexed
      // ------------------------------------
      // Left
      row++;
      dfCreationStack.push(nextNode->left);
      rowLocations.push(row);
      leftloc[thisRow] = row+1;

      // Right
      row++;
      dfCreationStack.push(nextNode->right);
      rowLocations.push(row);
      rightloc[thisRow] = row+1;
    }
  }

  // Construct the final dataframe from the vectors
  DataFrame splitDataFrame = DataFrame::create( _["column"] = column, _["splitvalue"] = splitvalue, _["category"] = category, _["leftloc"] = leftloc, _["rightloc"] = rightloc, _["numobs"] = numobs, _["isterminal"] = isterminal, _["iscategorical"] = iscategorical, _["prediction"] = prediction, _["rss"] = rss, _["mi"] = mi, _["miSD"] = miSD, _["expectedMi"] = expectedMi, _["gc"] = gc, _["expectedGc"] = expectedGc);
  return splitDataFrame;
}

// Tree printing
void AutoTree::inorderPrint() {
  inorderPrint(root, 0);
}

void AutoTree::inorderPrint(node* leaf, int level) {
  if (leaf != NULL) {
    inorderPrint(leaf->left, level+1);
    printNode(leaf);
    Rcout << "Level: " << level << std::endl;
    inorderPrint(leaf->right, level+1);
  }
}

void AutoTree::preorderPrint() {
  Rcout << "PREORDER PRINT" << std::endl;
  Rcout << "------------------" << std::endl;
  preorderPrint(root, 0);
}

void AutoTree::preorderPrint(node* leaf, int level) {
  if (leaf != NULL) {
    printNode(leaf);
    Rcout << "Level: " << level << std::endl;
    preorderPrint(leaf->left, level+1);
    preorderPrint(leaf->right, level+1);
  }
}

void AutoTree::printNode(node* x) {
  Rcout << "----------" << std::endl;
  if (x->isTerminalNode) {
    Rcout << "TERMINAL NODE" << std::endl;
    Rcout << "Prediction: " << x->prediction << std::endl;
  }
  if (x->isCategoricalSplit) {
    Rcout << "Factor: " << x->factor << std::endl;
  }
  else {
    Rcout << "Key: " << x->key << std::endl;
  }
  std::string columnName = x->column;
  Rcout << "Column: " << columnName << std::endl;
  Rcout << "Obs in Node: " << x->obsInNode << std::endl;
  // NumericVector dd = x->response;
  // Rcout << "Response: " << dd << std::endl;
  // NumericMatrix l = x->locations;
  // Rcout << "Locations: " << l << std::endl;
  Rcout << "RSS: " << (double) x->RSS << std::endl;
  Rcout << "mi: " << (double) x->mi << std::endl;
}

/* Helper functions */

// This function already exists in Rcpp but it keeps throwing a strange
// error whenever I use it, so I just wrote my own function to avoid that
// annoying error in Rcpp.
double findMax(NumericVector x) {
  double maximum = x[0];
  for (int i=0; i<x.size(); i++) {
    // Make sure it is not NA
    if (!NumericVector::is_na(x[i])) {
      if (x[i] > maximum) {
        maximum = x[i];
      }
    }
  }
  return maximum;
}

// Matrix subsetting
NumericMatrix matrixSubsetCells(NumericMatrix x, IntegerVector rIndex, IntegerVector cIndex) {
  int nRowOut = rIndex.size();
  int nColOut = cIndex.size();

  NumericMatrix out = Rcpp::no_init(nRowOut, nColOut);
  for (int i=0; i<nRowOut; i++) {
    for (int j=0; j<nColOut; j++) {
      out(i, j) = x(rIndex[i], cIndex[j]);
    }
  }

  return out;
}


// getters and setters
double AutoTree::getAlpha() {
  return alpha;
}
double AutoTree::getBeta() {
  return beta;
}
int AutoTree::getNumTerminalNodes() {
  return numTerminalNodes;
}
int AutoTree::getMinSplit() {
  return minsplit;
}
int AutoTree::getMinBucket() {
  return minbucket;
}
int AutoTree::getMaxDepth() {
  return maxdepth;
}
int AutoTree::getDistPower() {
  return distpower;
}
int AutoTree::getMaxObsMtxCalc() {
  return maxobsMtxCalc;
}
bool AutoTree::getIsLongLat() {
  return islonglat;
}
bool AutoTree::isGearyC() {
  return useGearyC;
}
bool AutoTree::getSaddlepointApproximation() {
  return saddlepointApproximation;
}
bool AutoTree::getUseParallelCalculations() {
  return useParallelCalculations;
}
double AutoTree::getSpatialBandwidth() {
  return spatialBandwidth;
}
SpatialWeights::Type AutoTree::getSpatialWeightsType() {
  return spatialWeightsType;
}

// Destroyal
AutoTree::~AutoTree()
{
  destroyTree();
}

void AutoTree::destroyTree() {
  destroyTree(root);
}

void AutoTree::destroyTree(node* leaf) {
  if (leaf != NULL) {
    destroyTree(leaf->left);
    destroyTree(leaf->right);
    delete leaf;
  }
}
