
#include "RcppEigen.h"
#include <string>
#include "qpmad/solver.h"



// [[Rcpp::export]]
Rcpp::List solveqpImpl(Eigen::MatrixXd& H,
                       const Eigen::VectorXd& h ,
                       const Eigen::VectorXd& lb  ,
                       const Eigen::VectorXd& ub  ,
                       const Eigen::MatrixXd& A   ,
                       const Eigen::VectorXd& Alb,
                       const Eigen::VectorXd& Aub,
                       int factorizationType,
                       int maxIter,
                       double tol,
                       bool returnInvertedCholeskyFactor,
                       bool returnLagrangeMultipliers)
{

  using namespace Rcpp;


  Eigen::VectorXd x(H.rows());
  x.setConstant(NA_REAL);

  qpmad::Solver   solver;
  qpmad::SolverParameters pars;


  pars.tolerance_ = tol;
  pars.max_iter_ = maxIter;
  pars.return_inverted_cholesky_factor_ = returnInvertedCholeskyFactor;
  pars.hessian_type_ = static_cast<qpmad::SolverParameters::HessianType>(factorizationType);

  int retval = -1;

  Rcpp::String msg;
  try {
      auto return_value = solver.solve(x, H, h, lb, ub, A, Alb, Aub, pars);
      retval = static_cast<int>(return_value);
      switch (return_value) {
      case qpmad::Solver::ReturnStatus::OK:
          msg = "Ok";
          break;
      case qpmad::Solver::ReturnStatus::MAXIMAL_NUMBER_OF_ITERATIONS:
          msg = "Maximal number of iterations reached";
          break;
      default:
          stop("Unhandled return status [%i]", static_cast<int>(return_value));
      }

  } catch (std::exception& e) {
      msg = e.what();
  }

  SEXP lagrangedf = R_NilValue;

  if (retval == 0 && returnLagrangeMultipliers) {
    Eigen::VectorXd lmult;
    Eigen::Matrix<qpmad::MatrixIndex, Eigen::Dynamic, 1> lmultInd;
    Eigen::Matrix<bool, Eigen::Dynamic, 1> islower;

    solver.getInequalityDual(lmult, lmultInd, islower);

    IntegerVector iv(lmultInd.data(), lmultInd.data() + lmultInd.size(), 
                      [](qpmad::MatrixIndex x){return static_cast<int>(x) + 1;});
    LogicalVector bv(islower.data(), islower.data() + islower.size());

    lagrangedf = DataFrame::create(
      _["multiplier"] = wrap(lmult),
      _["index"] = iv,
      _["isLower"] = bv
    );

  }

  return List::create(_["solution"] = x,
                      _["status"] = retval,
                      _["message"] = msg,
                      _["nIter"] = solver.getNumberOfInequalityIterations(),
                      _["lagrangeMult"] =  lagrangedf,
                      _["invHessian"] = returnInvertedCholeskyFactor ? wrap(H) : R_NilValue);
}




