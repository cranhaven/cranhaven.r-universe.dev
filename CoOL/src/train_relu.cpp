// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
using namespace Rcpp;

/****
   Andreas Rieckmann
   Piotr Dworzynski
   Leila Arras
   Claus Ekstrøm
   2021
****/

//' Function used as part of other functions
//' @description relu-function
//' @param x input in the relu function
//'
//' @export
// [[Rcpp::export]]
arma::mat rcpprelu(const arma::mat & x) {
    arma::mat m = x%(x>0);
    return m;
}

//' Function used as part of other functions
//' @description negative relu-function
//' @param x input in the negative relu-function
//'
//' @export
// [[Rcpp::export]]
arma::mat rcpprelu_neg(const arma::mat & x) {
    arma::mat m = x%(x<0);
    return m;
}

//' Function used as part of other functions
//' @description Non-negative neural network
//' @param x A matrix of predictors for the training dataset of shape (nsamples, nfeatures)
//' @param y A vector of output values for the training data with a length similar to the number of rows of x
//' @param c A vector of the data to adjust the analysis for such as calendar time (training data) with the same number of rows as x.
//' @param testx A matrix of predictors for the test dataset of shape (nsamples, nfeatures)
//' @param testy A vector of output values for the test data with a length similar to the number of rows of x
//' @param testc A vector the data to adjust the analysis for such as calendar time (training data) with the same number of rows as x.
//' @param W1_input Input-hidden layer weights of shape (nfeatuers, hidden) 
//' @param B1_input Biases for the hidden layer of shape (1, hidden) 
//' @param W2_input Hidden-output layer weights of shape (hidden, 1) 
//' @param B2_input Bias for the output layer (the baseline risk) af shape (1, 1)
//' @param C2_input Bias for the data to adjust the analysis for 
//' @param lr Initial learning rate
//' @param maxepochs The maximum number of epochs
//' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
//' @param input_parameter_reg Regularisation decreasing parameter value at each iteration for the input parameters
//' @param drop_out To drop connections if their weights reaches zero.
//' @param fix_baseline_risk To fix the baseline risk at a value.
//' @return A list of class "SCL" giving the estimated matrices and performance indicators
//' @author Andreas Rieckmann, Piotr Dworzynski, Leila Arras, Claus Ekstrøm
//'
//' @export
// [[Rcpp::export]]
Rcpp::List cpp_train_network_relu(
  const arma::mat & x,
  const arma::vec & y,
  const arma::vec & c,
  const arma::mat & testx,
  const arma::vec & testy,
  const arma::vec & testc,
  const arma::mat & W1_input,
  const arma::mat & B1_input,
  const arma::mat & W2_input,
  const arma::mat & B2_input,
  const arma::mat & C2_input,
  const arma::vec & ipw,
  double lr=0.01,
  double maxepochs = 100,
  double input_parameter_reg = 0.000001,
  int drop_out = 0,
  double fix_baseline_risk = -1
  ) {

  int nsamples = y.size();
  int nfeatures = x.n_cols;
  int hidden = W1_input.n_cols;
  int sparse_data = 0;
  double mean_y = accu(y) / nsamples;

  Rprintf("%s \n", "CoOL");

  // Loaded initialized weights.
  arma::mat W1(nfeatures, hidden, arma::fill::zeros);  // Filled with standard normals
  W1 = W1_input;
  arma::mat B1(1, hidden, arma::fill::zeros);
  B1 = B1_input;
  arma::mat W2(hidden, 1, arma::fill::zeros);  // Filled with standard normals
  W2 = W2_input;
  arma::mat B2(1, 1, arma::fill::zeros);
  B2 = B2_input;
  if(fix_baseline_risk>=0) {
    B2 = fix_baseline_risk;
  }
  arma::mat C2(1, 1, arma::fill::zeros);
  C2 = C2_input;

  // W1 for the test data parameter qualification
  arma::mat W1_previous_step(nfeatures, hidden, arma::fill::zeros);  // Filled with standard normals
  W1_previous_step = W1_input;

  // Define temporary holders and predicted output
  arma::mat h(1, hidden);
  arma::vec o(nsamples);
 
  arma::vec trainperf(maxepochs, arma::fill::zeros);
  arma::vec testperf(maxepochs, arma::fill::zeros);

  trainperf.replace(0, arma::datum::nan);
  testperf.replace(0, arma::datum::nan);

  // monitor the difference in weights
  arma::vec trainweights(maxepochs, arma::fill::zeros);
  trainweights.replace(0, arma::datum::nan);

  // monitor the baseline risk
  arma::vec baseline_risks(maxepochs, arma::fill::zeros);
  baseline_risks.replace(0, arma::datum::nan);

  arma::vec index = arma::linspace<arma::vec>(0, nsamples-1, nsamples);

  int row;
  // Main loop

  arma::uword epoch=0;
  for (epoch=0; epoch < maxepochs; epoch++) {
    // First we shuffle/permute all row indices before commencing
    arma::vec shuffle = arma::shuffle(index);

    // Step 1: Forward pass to get h and o.

    for (arma::uword rowidx=0; rowidx<x.n_rows; rowidx++) {
      // This is the row we're working on right now
      row = shuffle(rowidx);
      //      row = rowidx;

      // h contains the output from the hidden layer.
      // h has dimension 1 x hidden
      // it is a vector for individual "row" with hidden elements
      h = rcpprelu((x.row(row) * (W1)) + B1);

      // Now do the same to get the output layer
      o(row) = rcpprelu(h * W2 + B2 + c(row)*C2)(0,0); // the relu function is redundant

      // Step 2: Backwards pass to update the parameters W1, B1, B2
      double E_outO = - (y(row) - o(row));
      arma::mat netO_wHO = trans(h);
      arma::mat netO_outH = trans(W2);

      // All calculations done. Now do the updating
  if (drop_out==0) {
      for (size_t g=0; g<W1.n_rows; g++) {
        W1.row(g) = rcpprelu(W1.row(g) - ipw(row) * lr * E_outO * (netO_outH % (h>0)) * x(row, g) - ipw(row) * lr * input_parameter_reg); // L1 regularized - penalized
}}
  if (drop_out==1) {
      for (size_t g=0; g<W1.n_rows; g++) {
        W1.row(g) = rcpprelu(W1.row(g) -  ipw(row) * lr * E_outO * ((W1.row(g)>0)  % netO_outH % (h>0)) * x(row, g) - ipw(row) * lr * input_parameter_reg); // L1 regularized - penalized
}}

      B1 = rcpprelu_neg(B1 - ipw(row) * lr * E_outO * (netO_outH % (h>0)));
      if(fix_baseline_risk<0) {
      B2 = rcpprelu(B2 - ipw(row) * lr / 10 *  E_outO);

      // Update confounder weight
      netO_wHO = c(row);
      C2 = rcpprelu(C2(0,0) - ipw(row) * lr / 10 * E_outO * netO_wHO);

      }
    } // Row
 
    // Compute performance
    arma::mat tmp = x * W1;
    double mean_perform = 0.5*accu(square(y - (rcpprelu(rcpprelu(tmp.each_row() + B1) * W2 + B2(0,0) + C2(0,0) * c ))))/nsamples;

    // Compute performance on the validation (test) set    
    tmp = testx * W1;
    double mean_val_perform = 0.5*accu(square(testy - (rcpprelu(rcpprelu(tmp.each_row() + B1) * W2 + B2(0,0) + C2(0,0) * testc ))))/nsamples;

    trainperf(epoch) = mean_perform;
    testperf(epoch) = mean_val_perform;

    // Calculating the mean squared difference in weight update
    double mean_w_diff = 0;
   for (size_t i=0; i<W1.n_rows; i++) {
      for (size_t g=0; g<W1.n_cols; g++) {
    mean_w_diff = mean_w_diff + (W1(i,g) - W1_previous_step(i,g)) * (W1(i,g) - W1_previous_step(i,g));
    }}

    mean_w_diff = mean_w_diff / (W1.n_rows * W1.n_cols);
    trainweights(epoch) = mean_w_diff;
    W1_previous_step = W1;

    // Monitor the baseline risk
    baseline_risks(epoch) = B2(0,0);

  if (B2(0,0) == 0) {
    sparse_data = 1;
  }

 if (epoch % 10 == 0) {
      Rprintf("%d epochs: Train performance of %f. Baseline risk estimated to %f.\n",epoch, mean_perform, B2(0,0));
  // Warnings:
  if (B2(0,0) > mean_y) {
  Rprintf("Warning: The baseline risk (%f) is higher than mean(Y) (%f)! Consider reducing the regularisation of the input parameters.\n", B2(0,0), mean_y);
   }
  if (sparse_data == 1) {
  Rprintf("Warning: The baseline risk (%f) has at one time been estimated to zero. Data may be too sparse.\n", B2(0,0));
   }}

  }  // End of all epochs
  arma::colvec trainp = trainperf.elem(arma::find_finite(trainperf));
  arma::colvec testp = testperf.elem(arma::find_finite(testperf));
  arma::colvec trainp_weights = trainweights.elem(arma::find_finite(trainweights));
  arma::colvec baseline_risks_monitor = baseline_risks.elem(arma::find_finite(baseline_risks));
  
  Rcpp::List RVAL =  Rcpp::List::create(
    Rcpp::Named("W1")=W1,
    Rcpp::Named("B1")=B1,
    Rcpp::Named("W2")=W2,
    Rcpp::Named("B2")=B2,
    Rcpp::Named("C2")=C2,
    Rcpp::Named("train_performance")=trainp,
    Rcpp::Named("test_performance")=testp,
    Rcpp::Named("weight_performance")=trainp_weights,
    Rcpp::Named("baseline_risk_monitor")=baseline_risks_monitor,
    Rcpp::Named("epochs")=epoch+1
    );

  RVAL.attr("class") = CharacterVector::create("SCL", "list");
  return(RVAL);
}
