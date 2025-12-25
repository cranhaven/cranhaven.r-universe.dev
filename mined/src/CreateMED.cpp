/*
 * Generate MED by using fast algorithm which will be published.
 * Copyright: Dianpeng Wang <wdp@bit.edu.cn>
 * Date: 2018-05-24
 */

# include <RcppEigen.h>
# include "mathfunc.h"
# include "generateLattice.h"
# include "lkpredictor.h"
# include "MEDfunc.h"
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppEigen;
using namespace Eigen;
using namespace std;


/*
 * export function which can be used to select MED points from candidates
 */
// [[Rcpp::export]]
Rcpp::List SelectMinED(Eigen::MatrixXd & candidates, // candidates 
                     Eigen::VectorXd & candlf, // corresponding logarithm density functions at candidates
                     int n, // number of MED points
                     double gamma = 1, //
                     double s = 2 //
                       )
{
  Eigen::MatrixXd Sigmak = varCPP(candidates);
  return chooseMED(candidates, candlf, n, Sigmak, gamma, s);
}


// [[Rcpp::export]]
Eigen::MatrixXd Lattice(int n, int p)
{
  int m(n);
  try
  {
    if(!isPrime(n))
    {
      throw "n is not a prime, instand using maximum prime number which is less than n!";
    }
    m = n;
  }catch(const char * & e)
  {
    m = reportMaxPrime(n);
  }
  return generateLattice(m, p);
}


Rcpp::List lkpredict(Eigen::MatrixXd DesignPoints,
                     Eigen::VectorXd LfVector,
                     Eigen::MatrixXd PrePoints)
{
  LKPredictor m_pred(DesignPoints, LfVector);
  m_pred.fit();
  VectorXd tempPoint(DesignPoints.cols());
  // List PredictorRes;
  Eigen::VectorXd preVector(PrePoints.rows());
  Eigen::VectorXd preMind(PrePoints.rows());
  for(int i = 0; i < PrePoints.rows(); i++)
  {
    tempPoint = PrePoints.row(i);
    List PredictorRes = m_pred.predictor(tempPoint);
    preVector(i) = (double) PredictorRes[1];
    preMind(i) = (double) PredictorRes[0];
  }
  return Rcpp::List::create(_["pred"] = preVector, _["mind"] = preMind);
}


/*
 * Generating neiberhood points
 */
Eigen::MatrixXd generateAugMatrix(Eigen::MatrixXd & existMatrix,
                                  Eigen::MatrixXd & ExploreDesign,
                                  Eigen::VectorXd & tempPoint,
                                  double Radius,
                                  int n)
{
  // Eigen::VectorXd DistExistMatrix(existMatrix.rows());
  Eigen::VectorXd DistExistMatrix = fastPdist(existMatrix, tempPoint);
  Eigen::MatrixXd AugMatrix(ExploreDesign.rows(), ExploreDesign.cols());
  AugMatrix = (MatrixXd::Constant(ExploreDesign.rows(), ExploreDesign.cols(), 1.0) * 
    tempPoint.asDiagonal()).array() + Radius / sqrt(existMatrix.cols())
    * 2.0 * (ExploreDesign.array() - .5);
  VectorXi orderID(existMatrix.rows());
  int ncl1(round(3.0 * n / 8)), ncl2(round(n / 8.0));
  VectorXi clD(ncl1), clD2(ncl2);
  // Eigen::VectorXd ratio1, ratio2;
  
  orderID = orderCPP(DistExistMatrix);
  clD = orderID.segment(1, ncl1);
  clD2 = clD.segment(0, ncl2);
  Eigen::VectorXd ratio1 = (Radius / subVectElements(DistExistMatrix, clD).array()).min(0.5).matrix();
  Eigen::VectorXd ratio2 = (Radius / subVectElements(DistExistMatrix, clD2).array()).min(0.5).matrix();
  // Eigen::MatrixXd Mlc1, Mlc2;
  Eigen::MatrixXd Mlc1 = (ratio1.asDiagonal() * MatrixXd::Constant(ncl1, existMatrix.cols(), 1.0)).array() * 
    subMatRows(existMatrix, clD).array() + ((1.0 - ratio1.array()).matrix() * tempPoint.transpose()).array();
  Eigen::MatrixXd Mlc2 = ((1.0 + ratio2.array()).matrix() * tempPoint.transpose()).array() - (ratio2.asDiagonal() * MatrixXd::Constant(ncl2, existMatrix.cols(), 1.0)).array() * 
    subMatRows(existMatrix, clD2).array();
  // Eigen::MatrixXd Mlc;
  Eigen::MatrixXd Mlc = bindMatByRows(Mlc1, Mlc2);
  return bindMatByRows(AugMatrix, Mlc);
}


/*
 * compute MED criterion at new point given existing MED points and logarithm density functions
 */
double computeCritAddNewPoint(const Eigen::MatrixXd & existingMED, const Eigen::VectorXd & existinglf,
                              const Eigen::VectorXd & AddPoint, const double Addlf, const double penalty,
                              const double s, const double gamma)
{
  VectorXd DistMat = getLogGenDis(existingMED, AddPoint, s);
  VectorXd tempCrit = 0.5 * gamma * (existinglf.array() + Addlf) + existingMED.cols() * DistMat.array() + penalty;
  return tempCrit.minCoeff();
}



/*
 * This is the main function which is used to generate MED.
 */
// [[Rcpp::export]]
List mined(//int dim, // the dimension of density function
                 Eigen::MatrixXd & initial, // initial design given by user
                 Function logf, // logarithm of density function
                 int K_iter = 0
                   )
{
  int dim = initial.cols();
  int DesignSize = initial.rows();
  int ExDesignSize = reportMaxPrime(100 + 5 * (dim + 1)); // the design size
  // Eigen::MatrixXd NewPoints; // the points new evaluated
  // Eigen::VectorXd Newlf; // the lf values at new evaluated points
  // Eigen::MatrixXd MEDPoints; // the MED points selected in last iteration
  // Eigen::VectorXd MEDlf; // the corresponding lf values
  // Eigen::MatrixXd MED_k; // the MED points selected in current iteration
  // Eigen::VectorXd MEDlf_k; // the corresponding lf values
  // Eigen::MatrixXd EvaluatedPoints;
  // Eigen::VectorXd Evaluatedlf;
  /*
   * In the first step, a lattrice rules is used as the initial design.
   * The corresponding lf values are calculated.
   */
  Eigen::MatrixXd LatticeRules = initial;
  Eigen::MatrixXd ExploreDesignCand = generateLattice(ExDesignSize, dim);
  Eigen::VectorXd ExploreDesignlf(ExDesignSize); // = Eigen::MatrixXd::Zero(1, ExDesignSize);
  for(int i = 0; i < ExDesignSize; i++)
  {
    ExploreDesignlf(i) = 0.0;
  }
  // List SearchExplore;
  int ExploreSize = int(round(ExDesignSize / 2.0));
  Eigen::MatrixXd IdentityMat = Eigen::MatrixXd::Identity(dim, dim);
  // MatrixXd SigmaK = varCPP(LatticeRules);
  List SearchExplore = chooseMED(ExploreDesignCand, ExploreDesignlf, ExploreSize, 
		  IdentityMat, 1.0, 0.0);
  Eigen::MatrixXd ExploreDesign = SearchExplore[0];
  Eigen::VectorXd Latticelf(DesignSize);
  NumericVector LogFunRes; // the result of R function logf
  for(int i = 0; i < DesignSize; i++)
  {
    LogFunRes = logf(LatticeRules.row(i).transpose());
    Latticelf(i) = LogFunRes[0];
  }
  /*
   * define the result of choosing MED points using greedy algorithm
   */
  // List GreedyRes;
  int K(0);
  if(K_iter == 0)
  {
    K = ceil(4 * sqrt(dim));
  }
  else
  {
    K = K_iter;
  }
  double gamma = 1.0 / K;
  MatrixXd SigmaK = varCPP(LatticeRules);
  double s(0.0);
  List GreedyRes = chooseMED(LatticeRules, Latticelf, DesignSize, SigmaK, gamma, s);
  Eigen::MatrixXd MEDPoints = GreedyRes[0];
  Eigen::VectorXd MEDlf = GreedyRes[1];
  Eigen::MatrixXd MED_k = MEDPoints;
  Eigen::VectorXd MEDlf_k = MEDlf;
  Eigen::MatrixXd EvaluatedPoints = MEDPoints;
  Eigen::VectorXd Evaluatedlf = MEDlf;

  /*
   * 
   */
  VectorXd q(2);q << 0.9, 0.1;
  VectorXd q_vector(2);
  Eigen::MatrixXd PairDistMat(DesignSize, DesignSize);
  // Eigen::VectorXd DistAugEva;
  Eigen::VectorXd CurrentPoint(dim); // the point considered in current loop
  Eigen::VectorXi PermIndex(dim);
  // Eigen::MatrixXd AugMatrix;
  // Eigen::VectorXd RadiusVect;
  // Eigen::VectorXd DistCurrentEva;
  // Eigen::VectorXi orderID;
  // Eigen::VectorXi tempID;
  
  /*
   * Explore the support region by considering annealing version of density funcion
   */
  for(int k = 2; k <= K; k++)
  {// begin loop of annealing version
    gamma = (k - 1.0) / (K - 1.0);
    SigmaK = (k - 1.0) / k * SigmaK;
    q_vector = quantileCPP(MEDlf, q);
    s = round(2.0 * (1.0 - exp(-gamma * (q_vector(0) - q_vector(1)))));
    // compute the search radius vector
    PairDistMat = fastPdist(MEDPoints, MEDPoints);
    PairDistMat = setDiagonal(PairDistMat, 1e32);
    Eigen::VectorXd RadiusVect = PairDistMat.rowwise().minCoeff();
    /*
     * From each point in current MED, explore and find out a new candidate at which lf is
     * evaluated.
     */
    for(int j = 0; j < DesignSize; j++)
    {// begin loop of j
      CurrentPoint = MEDPoints.row(j);
      PermIndex = sampleCPP(dim);
      ExploreDesign = subMatCols(ExploreDesign, PermIndex);
      Eigen::MatrixXd AugMatrix = generateAugMatrix(MEDPoints, ExploreDesign, CurrentPoint, RadiusVect(j), DesignSize);
      /*
       * Compress the augmented matrix by considering the distances betwwen the augmented points and points evaluated
       * in previous iterations.
       */
      Eigen::VectorXd DistAugEva = fastPdist(AugMatrix, EvaluatedPoints).rowwise().minCoeff();
      int CompressCount = (DistAugEva.array() > .5 * RadiusVect(j)).count();
      if(CompressCount > DesignSize / 2)
      {
        Eigen::VectorXi CompressIndex(CompressCount);
        int m(0);
        for(int l = 0; l < DistAugEva.size(); l++)
        {
          if(DistAugEva(l) > .5 * RadiusVect(j))
          {
            CompressIndex(m) = l;
            m++;
          }
        }
        AugMatrix = subMatRows(AugMatrix, CompressIndex);
      }
      else
      {
        Eigen::VectorXd mq(1); mq << 0.5;
        Eigen::VectorXd RadiusMedian = quantileCPP(DistAugEva, mq);
        Eigen::VectorXi CompressIndex((DistAugEva.array() > RadiusMedian(0)).count());
        int m(0);
        for(int l = 0; l < DistAugEva.size(); l++)
        {
          if(DistAugEva(l) > RadiusMedian(0))
          {

            CompressIndex(m) = l;
            m++;
          }
        }
        AugMatrix = subMatRows(AugMatrix, CompressIndex);
      }
      if(AugMatrix.rows() == 0)
      {
        break;
      }
      /*
       * predict lf values at AugMatrix using limit kriging predictor.
       */
      Eigen::VectorXd DistCurrentEva = fastPdist(EvaluatedPoints, CurrentPoint);
      Eigen::VectorXi orderID = orderCPP(DistCurrentEva);
      Eigen::VectorXi tempID = orderID.head(DesignSize);
      Eigen::MatrixXd TrainingPoints = subMatRows(EvaluatedPoints, tempID);
      Eigen::VectorXd Traininglf = subVectElements(Evaluatedlf, tempID);
      Rcpp::List AugPred = lkpredict(TrainingPoints, Traininglf, AugMatrix);
      Eigen::VectorXd AddCriterion(AugMatrix.rows());
      Eigen::VectorXd Auglf = AugPred[0];
      Eigen::VectorXd Augmind = AugPred[1];
      for(int e = 0; e < AugMatrix.rows(); e++)
      {
        double penanlty(0.0);
        if(Augmind(e) < 0.1 / DesignSize)
        {
          penanlty = log(Augmind(e));
        }
        if(j == 0)
        {
          AddCriterion(e) = gamma * Auglf(e) + penanlty;
        }
        else
        {
          AddCriterion(e) = computeCritAddNewPoint(MED_k.topRows(j), MEDlf_k.head(j), AugMatrix.row(e), Auglf(e), penanlty,
                             s, gamma);
        }
      }
      int Vind = argMax(AddCriterion);
      Eigen::VectorXd AddPoint = AugMatrix.row(Vind);
      LogFunRes = logf(AddPoint);
      double Addlf = (double) LogFunRes[0];
  
      if(j == 0)
      {
        if(Addlf > MEDlf(0))
        {
          MED_k.row(0) = AddPoint;
          MEDlf_k(0) = Addlf;
        }
      }
      else
      {
        double CriV = computeCritAddNewPoint(MED_k.topRows(j), MEDlf_k.head(j), AddPoint, Addlf, 0.0,
                                         s, gamma);
        double CriO = computeCritAddNewPoint(MED_k.topRows(j), MEDlf_k.head(j), MEDPoints.row(j), MEDlf(j), 0.0,
                                         s, gamma);
        if(CriV > CriO)
        {
          MED_k.row(j) = AddPoint;
          MEDlf_k(j) = Addlf;
        }
      }
      EvaluatedPoints = bindMatByRows(EvaluatedPoints, AddPoint);
      Evaluatedlf.conservativeResize(Evaluatedlf.size() + 1, 1);
      Evaluatedlf(Evaluatedlf.size() - 1) = Addlf;
    }// end loop of j
    GreedyRes = chooseMED(EvaluatedPoints, Evaluatedlf, DesignSize, SigmaK, gamma, s);
    MEDPoints = GreedyRes[0];
    MEDlf = GreedyRes[1];
    MED_k = MEDPoints;
    MEDlf_k = MEDlf;
    SigmaK = varCPP(MED_k);
  }// end loop of annealing version
  
  return List::create(Named("points")=MED_k, Named("logf")=MEDlf_k,
                      Named("cand") = EvaluatedPoints, Named("candlf")=Evaluatedlf);
}


/***R
# f <- function(x)
# {
#   return(.0)
# }
# Ematrix <- MaxProLHD(101, 2)
# generateMED(2, Ematrix$Design, f)
*/
