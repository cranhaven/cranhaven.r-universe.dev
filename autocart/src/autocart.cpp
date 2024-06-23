#include <cmath>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include "AutoTree.h"
#include "SpatialMethods.h"
#include "SplittingMethods.h"

using namespace Rcpp;

//' Create an autocart model
//'
//' @param response A vector of numeric response values with no NA entries.
//' @param data A dataframe for the predictor variables used in the autocart tree.
//' @param locations A two-column matrix with coordinates for the observations the predictor dataframe.
//' @param alpha A scalar value between 0 and 1 to weight autocorrelation against reduction in variance in the tree splitting. A value of 1 indicates full weighting on measures of autocorrelation.
//' @param beta A scalar value between 0 and 1 to weight the shape of the region in the splitting
//' @param control An object of type "autocartControl" returned by the \code{autocartControl} function to control the splitting in the autocart tree.
//' @return An S3 object of class "autocart".
//'
//' @examples
//' # Load some data for an autocart example
//' snow <- na.omit(read.csv(system.file("extdata", "ut2017_snow.csv", package = "autocart")))
//' y <- snow$yr50[1:40]
//' X <- data.frame(snow$ELEVATION, snow$MCMT, snow$PPTWT, snow$HUC)[1:40, ]
//' locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))[1:40, ]
//'
//' # Create an autocart model with 50 trees
//' snow_model <- autocart(y, X, locations, 0.30, 0)
//'
//' @import fields
//' @importFrom RcppParallel RcppParallelLibs
//' @export
// [[Rcpp::export]]
List autocart(NumericVector response, DataFrame data, NumericMatrix locations, double alpha, double beta, Rcpp::Nullable<Rcpp::List> control = R_NilValue) {

  // Obviously make sure this is FALSE when in production
  bool debugOutput = false;

  // Default values for splitting parameters
  int minsplit = 20;
  int minbucket = 7;
  int maxdepth = 30;
  int distpower = 1;
  int maxobsMtxCalc = response.size();
  bool islonglat = true;
  bool givePredAsFactor = true;
  bool retainCoords = true;
  bool useGearyC = false;
  bool saddlepointApproximation = false;
  bool useParallelCalculations = true;
  bool asForest = false;

  SpatialWeights::Type spatialWeightsType = SpatialWeights::Regular;
  std::string spatialWeightsExtract = "default";
  double spatialBandwidthProportion = 1.0;
  double spatialBandwidth;

  NumericMatrix spatialWeightsMatrix;
  NumericMatrix distanceMatrix;

  // Predictor numbers
  int p = data.ncol();

  // Get correct mTry. by default set to floor of predictors/3
  int asForestMTry = p / 3;

  // Warn if locations matrix contains high entries
  bool location_warned = false;
  for (int i=0; i<locations.nrow(); i++) {
    if ((abs(locations(i, 0)) > 40000 || abs(locations(i, 1)) > 40000) && !location_warned) {
      warning("Location entries detected to be numerically large. This may likely cause an overflow error when calculating distances. Consider rescaling your coordinate values to something smaller.");
      location_warned = true;
    }
  }

  // Extract info from autocartControl
  if (control.isNotNull()) {
    List autocartControl = as<List>(control);
    // Make sure it inherits the autocartControl class
    if (!autocartControl.inherits("autocartControl")) {
      stop("The control parameter to autocart must be an object of type \"autocartControl\". This can be obtained from the autocartControl function.");
    }

    minsplit = as<int>(autocartControl["minsplit"]);
    minbucket = as<int>(autocartControl["minbucket"]);
    maxdepth = as<int>(autocartControl["maxdepth"]);
    distpower = as<int>(autocartControl["distpower"]);
    //asForestMTry = as<bool>(autocartControl["asForestMTry"]);
    islonglat = as<bool>(autocartControl["islonglat"]);
    givePredAsFactor = as<bool>(autocartControl["givePredAsFactor"]);
    retainCoords = as<bool>(autocartControl["retainCoords"]);
    useGearyC = as<bool>(autocartControl["useGearyC"]);
    saddlepointApproximation = as<bool>(autocartControl["saddlepointApproximation"]);
    useParallelCalculations = as<bool>(autocartControl["runParallel"]);
    asForest = as<bool>(autocartControl["asForest"]);

    // Make sure that minbucket is sensical compared to minsplit. If minbucket is half of minsplit, then
    // the code will crash.
    if (minbucket >= minsplit / 2) {
      stop("The minbucket parameter should not be above half of minsplit.");
    }

    // If maxobsMtxCalc isn't supplied, then we should default it to be the size of all the data.
    IntegerVector maxobsMtxCalcExtract = autocartControl["maxobsMtxCalc"];
    if (maxobsMtxCalcExtract.length() < 1) {
      maxobsMtxCalc = response.size();
    }
    else {
      maxobsMtxCalc = maxobsMtxCalcExtract[0];
    }

    // Find out which of "spatialBandwidth" or "spatialBandwidthProportion" was supplied. Use the supplied
    // argument to induce the other one.
    // ------------------------------------
    // To do so, we need the maximum distance from any point to any other point upfront.
    double maxDistance;
    if (islonglat) {
      Function dist("rdist.earth");
      distanceMatrix = dist(locations);
      maxDistance = max(distanceMatrix);
    }
    else {
      Function dist("rdist");
      distanceMatrix = dist(locations);
      maxDistance = max(distanceMatrix);
    }
    if (debugOutput) {
      Rcout << "Maximum distance in locations matrix is " << maxDistance << std::endl;
    }

    // Assign to the NULL spatialBandwidth or spatialBandwidthProportion
    NumericVector temp1 = autocartControl["spatialBandwidth"];
    NumericVector temp2 = autocartControl["spatialBandwidthProportion"];
    if (temp1.length() < 1) {
      spatialBandwidthProportion = as<double>(autocartControl["spatialBandwidthProportion"]);
      spatialBandwidth = maxDistance * spatialBandwidthProportion;
      if (debugOutput) {
        Rcout << "Based upon value of " << spatialBandwidthProportion << ", setting spatialBandwidth to " << spatialBandwidth << std::endl;
      }
    }
    else if (temp2.length() < 1) {
      spatialBandwidth = as<double>(autocartControl["spatialBandwidth"]);
      spatialBandwidthProportion = spatialBandwidth / maxDistance;
      if (debugOutput) {
        Rcout << "Based upon value of " << spatialBandwidth << ", setting spatialBandwidthProportion to " << spatialBandwidthProportion << std::endl;
      }
    }

    // Extract the type of weighting we wish to do
    spatialWeightsExtract = as<std::string>(autocartControl["spatialWeightsType"]);
    if (spatialWeightsExtract.compare("default") == 0) {
      spatialWeightsType = SpatialWeights::Regular;
    }
    else if (spatialWeightsExtract.compare("gaussian") == 0) {
      spatialWeightsType = SpatialWeights::Gaussian;
    }
    else if (spatialWeightsExtract.compare("custom") == 0) {
      spatialWeightsType = SpatialWeights::Custom;
    }
    else {
      stop("Can't create autocart tree. Unrecognized spatial weighting scheme.");
    }

    // Assign to spatialWeightsMatrix IF it is supplied. If not, then create your own
    Rcpp::Nullable<Rcpp::NumericMatrix> suppliedWeightsMatrix = autocartControl["customSpatialWeights"];
    if (!suppliedWeightsMatrix.isNotNull()) {
      if (debugOutput) {
        Rcout << "No custom supplied weights matrix!" << std::endl;
      }
      spatialWeightsMatrix = getWeightsMatrix(locations, distpower, islonglat, spatialBandwidth, spatialWeightsType, useParallelCalculations);
    }
    else {
      if (debugOutput) {
        Rcout << "Custom supplied weights matrix!" << std::endl;
      }
      spatialWeightsMatrix = as<Rcpp::NumericMatrix>(suppliedWeightsMatrix);
    }
  }

  // NO AUTOCART CONTROL SUPPLIED
  else {
    double maxDistance;
    if (islonglat) {
      // Distance matrix
      Function dist("rdist.earth");
      distanceMatrix = dist(locations);
      maxDistance = max(distanceMatrix);
      spatialBandwidth = spatialBandwidthProportion * maxDistance;
    }
    else {
      // Distance matrix
      Function dist("rdist");
      distanceMatrix = dist(locations);
      maxDistance = max(distanceMatrix);
      spatialBandwidth = spatialBandwidthProportion * maxDistance;
    }

    // Spatial weights matrix
    spatialWeightsMatrix = getWeightsMatrix(locations, distpower, islonglat, spatialBandwidth, spatialWeightsType, useParallelCalculations);
  }

  // ERROR CHECK
  // ------------------
  // maxobsMtxCalc
  if ((alpha + beta == 1.0) && maxobsMtxCalc < response.size()) {
    stop("When alpha+beta=1.0, all splitting is done by matrix calculations, so maxobsMtxCalc must be the size of all records in the data.");
  }
  // spatialWeightsMatrix
  if (spatialWeightsMatrix.nrow() != spatialWeightsMatrix.ncol()) {
    stop("Spatial weights matrix must have ncol equal to nrow.");
  }
  if (distanceMatrix.nrow() != distanceMatrix.ncol()) {
    stop("Distance matrix must have ncol equal to nrow.");
  }
  if (distanceMatrix.nrow() != spatialWeightsMatrix.nrow()) {
    stop("distanceMatrix and spatialWeightsMatrix must have the same number of rows and columns.");
  }
  if (distanceMatrix.nrow() != locations.nrow()) {
    stop("locations, distanceMatrix, and spatialWeightsMatrix must have the same number of rows.");
  }

  // Ensure that the spatial weights matrix doesn't contain any weird values that would stop the tree from working.
  bool na_found = false;
  bool inf_found = false;
  int sum_na = 0;
  int sum_inf = 0;
  for (int i=0; i<spatialWeightsMatrix.nrow(); i++) {
    for (int j=i; j<spatialWeightsMatrix.ncol(); j++) {
      if (NumericVector::is_na(spatialWeightsMatrix(i, j))) {
        sum_na++;
        na_found = true;
      }
      if (Rcpp::traits::is_infinite<REALSXP>(spatialWeightsMatrix(i, j))) {
        sum_inf++;
        inf_found = true;
      }
    }
  }
  if (na_found) {
    warning("NA value found in spatial weights matrix. Quietly setting this weight to be a 0 weight.");
    Rcout << "(" << sum_na << " NAs were found in the spatial weights matrix)" << std::endl;
  }
  if (inf_found) {
    warning("Infinite value found in spatial weights matrix. Quietly setting this weight to be a 0 weight. Do you have repeat locations in your locations matrix?");
    Rcout << "(" << sum_inf << " infinite weights were found in the spatial weights matrix)" << std::endl;
  }

  // Once saddlepoint approximation has been implemented, you can remove this warning right here
  if (saddlepointApproximation == true) {
    warning("The saddlepoint approximation to Moran's I is a loose end that has not been fully implemented in the autocart package, and will use exact Moran's I instead. To reduce computation time, consider the \"maxobsMtxCalc\" splitting parameter at the moment.");
  }

  // The "createTree" method in AutoTree.cpp does all the hard work in creating the splits
  AutoTree tree(alpha, beta, minsplit, minbucket, maxdepth, distpower, maxobsMtxCalc, islonglat, useGearyC, saddlepointApproximation, useParallelCalculations, asForest, asForestMTry, spatialWeightsType, spatialBandwidth, spatialWeightsMatrix, distanceMatrix);
  tree.createTree(response, data, locations);

  // List members
  NumericVector prediction = tree.predictDataFrame(data);
  DataFrame splitframe = tree.createSplitDataFrame();
  List splitparams = List::create(_["minsplit"] = minsplit, _["minbucket"] = minbucket, _["maxdepth"] = maxdepth, _["distpower"] = distpower, _["islonglat"] = islonglat, _["alpha"] = alpha, _["beta"] = beta, _["useGearyC"] = useGearyC, _["asForest"] = asForest, _["asForestMTry"] = asForestMTry, _["spatialWeightsType"] = spatialWeightsExtract, _["spatialBandwidth"] = spatialBandwidth);

  // If the "givePredAsFactor" is set to true, then convert the prediction vector into a factor and label it from 1 to the number of regions
  // Construct the S3 object that contains information about the model
  List autocartModel;
  if (givePredAsFactor) {
    // Convert the prediction vector to a factor and include it in the output of the model
    NumericVector levs = sort_unique(prediction);
    IntegerVector predAsFactor = match(prediction, levs);
    predAsFactor.attr("levels") = as<CharacterVector>(levs);
    predAsFactor.attr("class") = "factor";

    autocartModel = List::create(_["prediction"] = prediction, _["predAsFactor"] = predAsFactor, _["splitframe"] = splitframe, _["splitparams"] = splitparams);
  }
  else {
    autocartModel = List::create(_["prediction"] = prediction, _["splitframe"] = splitframe, _["splitparams"] = splitparams);
  }

  // The "retainCoords" parameter specifies if we also add a dataframe with the longitude/latitude/prediction of all items
  // that went into training the tree. This is useful when creating a spatial
  // process at the terminal nodes of the tree.
  if (retainCoords) {
    // If coordinates are given as longitude and latitude, we name them with
    // "long" and "lat". If otherwise, then it wouldn't make sense to call them
    // long and lat so we'll use "x" and "y".
    NumericVector x = locations(_, 0);
    NumericVector y = locations(_, 1);
    DataFrame coords;
    if (islonglat) {
      coords = DataFrame::create(_["x"] = x, _["y"] = y, _["pred"] = prediction, _["actual"] = response);
    }
    else {
      coords = DataFrame::create(_["x"] = x, _["y"] = y, _["pred"] = prediction, _["actual"] = response);
    }
    autocartModel.push_back(coords, "coords");
  }

  autocartModel.attr("class") = "autocart";
  return autocartModel;
}

//' Given an autocart model object, predict for new data passed in
//'
//' @param autocartModel An S3 object of type "autocart" returned from the autocart function
//' @param newdata A dataframe with the same amount of columns used to create the autocart model.
//' @return A numeric vector containing the predicted response value for each of the rows in the passed in dataframe.
//'
//' @examples
//' # Load some data for an autocart predict example
//' snow <- na.omit(read.csv(system.file("extdata", "ut2017_snow.csv", package = "autocart")))
//' y <- snow$yr50[1:40]
//' X <- data.frame(snow$ELEVATION, snow$MCMT, snow$PPTWT, snow$HUC)[1:40, ]
//' locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))[1:40, ]
//'
//' # Create an autocart model with 50 trees
//' snow_model <- autocart(y, X, locations, 0.30, 0)
//'
//' # Predict in autocart
//' new_X <- X[1:10, ]
//' new_loc <- locations[1:10, ]
//' autocart_predictions <- predictAutocart(snow_model, new_X)
//' @export
// [[Rcpp::export]]
NumericVector predictAutocart(List autocartModel, DataFrame newdata) {
  // Check to make sure the list is of the correct class
  if (!autocartModel.inherits("autocart")) {
    stop("To predict, input must be an autocart model object.");
  }

  // Check validity of passed in DataFrame
  for (int j=0; j<newdata.length(); j++) {
    // Make sure all numeric
    if (TYPEOF(newdata[j]) != REALSXP && TYPEOF(newdata[j]) != INTSXP) {
      stop("To predict, all dataframe columns must be numeric vectors or factors.");
    }

    // Make sure no NAs exist
    NumericVector temp = newdata[j];
    for (int i=0; i<temp.size(); i++) {
      if (NumericVector::is_na(temp[i])) {
        stop("NA found in dataframe. Consider imputation or removing rows with NA values.");
      }
    }
  }

  NumericVector predictionVector;
  DataFrame splitFrame = as<DataFrame>(autocartModel["splitframe"]);

  // Keep copies of all the columns in splitFrame as Rcpp forces us to unpack them
  // at every step anyway
  CharacterVector column = splitFrame["column"];
  NumericVector splitValue = splitFrame["splitvalue"];
  IntegerVector category = splitFrame["category"];
  IntegerVector leftLoc = splitFrame["leftloc"];
  IntegerVector rightLoc = splitFrame["rightloc"];
  LogicalVector isTerminal = splitFrame["isterminal"];
  LogicalVector isCategorical = splitFrame["iscategorical"];
  NumericVector prediction = splitFrame["prediction"];

  // For each row in the passed in dataframe, append a prediction to the predictionVector
  for (int row=0; row<newdata.nrows(); row++) {

    int splittingRow = 0;
    // Run until the row in the split DataFrame is a terminal node, at which
    // point you can return the prediction for that node
    while (!isTerminal[splittingRow]) {
      // Make split depending on if categorical or not
      if (isCategorical[splittingRow]) {
        int compareFactor = category[splittingRow];

        // Get the value in newdata to compare to the above
        String searchColumnName = column[splittingRow];
        IntegerVector newdataSplitColumn = newdata[searchColumnName];
        int myFactor = newdataSplitColumn[row];

        // For both directions, subtract 1 from what's store in leftloc/rightloc
        // as C++ is 0-indexed
        if (myFactor == compareFactor) {
          // Go right
          splittingRow = rightLoc[splittingRow] - 1;
        }
        else {
          // Go left
          splittingRow = leftLoc[splittingRow] - 1;
        }
      }
      // Continuous
      else {
        double compareValue = splitValue[splittingRow];

        // Get the value in newdata to compare to the above
        String searchColumnName = column[splittingRow];
        NumericVector newdataSplitColumn = newdata[searchColumnName];
        double myValue = newdataSplitColumn[row];

        // For both directions, we subtract one from what's stored in
        // leftloc/rightloc as C++ is 0-indexed
        if (myValue <= compareValue) {
          // Go left
          splittingRow = leftLoc[splittingRow] - 1;
        }
        else {
          // Go right
          splittingRow = rightLoc[splittingRow] - 1;
        }
      }
    }

    // Now that we are at a terminal node, we can make the prediction for
    // this observation.
    predictionVector.push_back(prediction[splittingRow]);
  }

  return predictionVector;
}
