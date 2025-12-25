// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <Rcpp.h>
# include <RcppParallel.h>
# include "h/macros.hpp"
# include "h/dnyTasking.hpp"
# include "h/paraSort.hpp"
# include "h/vectorMag.hpp"
# include "h/distanceClasses.hpp"
using namespace Rcpp;
using namespace RcppParallel;


namespace KMconstrainedSparse {


template<typename indtype, typename valtype>
struct event: public E<indtype, valtype>
{
  event(): E<indtype, valtype>(){}
};


template<typename indtype, typename valtype>
struct centroid: public E<indtype, valtype>
{
  indtype eventCentroidIndexLow, eventCentroidIndexUp;
  centroid(): E<indtype, valtype>(){}
};




template<typename indtype, typename valtype>
inline void addEventLossToCentriodLoss(
    valtype *rstCentroidLoss, event<indtype, valtype> &x)
{
  for(indtype i = 0, iend = x.size; i < iend; ++i)
    rstCentroidLoss[x.region[i]] += x.loss[i] * x.weight;
}




template<typename indtype>
struct eventCentroidIndex
{
  indtype centroidID, eventID;
};
template<typename indtype>
inline bool byCent(const eventCentroidIndex<indtype> &x,
            const eventCentroidIndex<indtype> &y)
{
  return x.centroidID < y.centroidID;
}
template<typename indtype>
inline bool byEvent(const eventCentroidIndex<indtype> &x,
             const eventCentroidIndex<indtype> &y)
{
  return x.eventID < y.eventID;
}


// D is the head of the distance vector from events to medoids
template<typename indtype, typename valtype>
struct getOrder
{
  valtype *D;
  getOrder(){}
  getOrder(valtype *D): D(D){}
  bool operator() (const indtype i, const indtype j) { return D[i] < D[j]; }
};


// If number of clusters * data size > 2 ^ 32, use orderIndtype <- UINT.
template<typename indtype, typename orderIndtype, typename valtype>
struct assignMedoid
{
  indtype NofCluster;
  indtype NofEvent;
  indtype maxCore;


  valtype *clusterWeightUpperLimit;
  event<indtype, valtype> *eventV;
  vec<eventCentroidIndex<indtype> > &eventCluster; // initial should be 0-1


  void rowToEventAndClusterInd(indtype i, indtype &E, indtype &M)
  {
    E = i / NofCluster;
    M = i % NofCluster;
  }


  vec<valtype> &D;
  vec<orderIndtype> &Dorder;
  vec<orderIndtype> &DorderAux;
  valtype sumOfD;


  void ordering(bool mergeInPlace = false)
  {
    Dorder.resize(D.size());
    for(orderIndtype i = 0, iend = D.size(); i < iend; ++i)
      Dorder[i] = i;
    getOrder<orderIndtype, valtype> cp(&D[0]);
    if(!mergeInPlace)
      paraSort<orderIndtype, getOrder<orderIndtype, valtype> > (
          Dorder, DorderAux, &cp, maxCore);
    else
      paraSort<orderIndtype, getOrder<orderIndtype, valtype> > (
          Dorder, &cp, maxCore);
  }


  // eventCluster must have initial size of NofEvents
  assignMedoid(valtype *clusterWeightUpperLimit,
               event<indtype, valtype> *eventV,
               vec<eventCentroidIndex<indtype> > &eventCluster,
               vec<valtype> &D,
               vec<orderIndtype> &Dorder,
               vec<orderIndtype> &DorderAux,
               bool paraSortInPlaceMerge,
               indtype maxCore):
    maxCore(maxCore),
    clusterWeightUpperLimit(clusterWeightUpperLimit),
    eventV(eventV),
    eventCluster(eventCluster), D(D), Dorder(Dorder),
    DorderAux(DorderAux)
  {
    ordering(paraSortInPlaceMerge);
    sumOfD = 0;
    NofEvent = eventCluster.size();
    vec<bool> eventAssigned(NofEvent, false);
    NofCluster = D.size() / NofEvent;
    vec<valtype> clusterWeight(NofCluster, 0);


    indtype k = 0; // iterates eventMedoid
    for(INT i = 0, iend = Dorder.size(); k < NofEvent and i < iend; ++i)
    {
      indtype tmpEventID = 0;
      indtype tmpCenID = 0;
      rowToEventAndClusterInd(Dorder[i], tmpEventID, tmpCenID);
      valtype weightAfterward = clusterWeight[tmpCenID] + eventV[tmpEventID].weight;
      if(eventAssigned[tmpEventID] or
           weightAfterward / clusterWeightUpperLimit[tmpCenID] - 1 > 1e-5) continue;
      eventAssigned[tmpEventID] = true;
      clusterWeight[tmpCenID] = weightAfterward;
      eventCluster[k].eventID = tmpEventID;
      eventCluster[k].centroidID = tmpCenID;
      sumOfD += D[Dorder[i]];
      ++k;
    }
    std::sort(eventCluster.begin(), eventCluster.end(), byEvent<int>);
  }
};


//before this function, eventCentroidIndex has been sorted by its member centroidID
template<typename indtype, typename valtype, int beta>
inline void updateCentroid(indtype whichCluster,
                    centroid<indtype, valtype> *C,
                    event<indtype, valtype> *eventVbegin,
                    eventCentroidIndex<indtype> *eventCentroidV,
                    vec<bool> &clusterChanged, valtype p)
{
  centroid<indtype, valtype> &c = C[whichCluster];
  if(clusterChanged[whichCluster] == 0 or
       c.eventCentroidIndexLow == c.eventCentroidIndexUp) return;


  std::fill(c.loss, c.loss + c.size, 0);
  valtype sumWeight = 0;
  for(indtype i = c.eventCentroidIndexLow, iend = c.eventCentroidIndexUp; i < iend; ++i)
  {
    auto &tmp = eventVbegin[eventCentroidV[i].eventID];
    sumWeight += tmp.weight;
    addEventLossToCentriodLoss(c.loss, tmp);
  }
  c.mag = 0;
  c.l2norm = 0;
  valtype oneOverSumWeight = 1.0 / sumWeight;
  for(indtype i = 0, iend = c.size; i < iend; ++i)
  {
    c.loss[i] *= oneOverSumWeight;
    if(beta == 2) c.mag += c.loss[i] * c.loss[i];
    else if(beta == 1) c.mag += std::abs(c.loss[i]);
    else if(beta == 3) c.mag += raisePower(std::abs(c.loss[i]), p);
    else if(beta == 4) c.mag += std::pow(std::abs(c.loss[i]), p);
    else if(beta == -1) c.l2norm += c.loss[i] * c.loss[i];
  }
  if(beta == -1) c.l2norm = std::sqrt(c.l2norm);
}




template<typename indtype, typename valtype, int beta>
struct updateCentroidV: public Worker
{
  valtype p;
  vec<centroid<indtype, valtype> > &centroidV;
  event<indtype, valtype> *eventVbegin;
  eventCentroidIndex<indtype> *eventCentroidV;
  vec<bool> &clusterChanged;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      updateCentroid<indtype, valtype, beta> (
          objI, &centroidV[0], eventVbegin, eventCentroidV, clusterChanged, p);
    }
  }


  updateCentroidV(valtype p,
    vec<centroid<indtype, valtype> > &centroidV,
    event<indtype, valtype> *eventVbegin,
    eventCentroidIndex<indtype> *eventCentroidV,
    vec<bool> &clusterChanged, indtype maxCore): p(p),
    centroidV(centroidV), eventVbegin(eventVbegin),
    eventCentroidV(eventCentroidV), clusterChanged(clusterChanged)
  {
    dynamicTasking dt(maxCore, centroidV.size()); dT = &dt;
    parallelFor(0, dT->NofCore, *this);
  }
};




template<typename indtype, typename valtype, int beta>
struct compDfun: public Worker
{
  indtype NofCluster;
  indtype NofEvent;
  valtype p;
  valtype *D;
  event<indtype, valtype> *EV;
  centroid<indtype, valtype> *CV;
  vec<bool> &clusterChanged; // Will be read only. Safe or multithreading.
  dynamicTasking *dT;


  // row -- std::size_t
  void compD(indtype row, valtype &d,
             indtype NofEvent, indtype NofCluster,
             event<indtype, valtype> *EV,
             centroid<indtype, valtype> *CV,
             vec<bool> &clusterChanged)
  {
    indtype whichEvent = row / NofCluster;
    indtype whichCluster = row % NofCluster;
    if(!clusterChanged[whichCluster]) return; // Read only, safe.
    d = minkD<event<indtype, valtype>,
              centroid<indtype, valtype>, 1,
              indtype, valtype, beta, false> (EV[whichEvent], CV[whichCluster], p);
  }


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      compD(objI, D[objI], NofEvent, NofCluster,
            EV, CV, clusterChanged);
    }
  }


  compDfun(indtype NofCluster, indtype NofEvent,
           valtype p,
           valtype *D,
           event<indtype, valtype> *EV,
           centroid<indtype, valtype> *CV,
           vec<bool> &clusterChanged, int maxCore):
    NofCluster(NofCluster), NofEvent(NofEvent),
    p(p), D(D), EV(EV), CV(CV),
    clusterChanged(clusterChanged)
  {
    dynamicTasking dt(maxCore, std::size_t(NofCluster) * NofEvent); dT = &dt;
    parallelFor(0, dT->NofCore, *this);
  }


};


}




template<typename orderIndtype, int beta>
inline List sparseKMconstrainedCppTemplate
(
    List X,
    int d,
    List centroid,
    NumericVector Xw,
    NumericVector maxClusterWeights,
    double minkP,
    int maxCore,
    int convergenceTail,
    double tailConvergedRelaErr,
    int maxIter,
    bool paraSortInplaceMerge,
    bool verbose
)
{
  double *clusterSizeUpperBound = &maxClusterWeights[0];
  // INT N = X.size();


  vec<KMconstrainedSparse::event<int, double> > eventV(X.size());
  if(true) // Initialize eventV.
  {
    for(int i = 0, iend = X.size(); i < iend; ++i)
    {
      List x = X[i];
      IntegerVector region = x[0];
      NumericVector loss = x[1];
      eventV[i].size = region.size();
      eventV[i].region = &region[0];
      eventV[i].loss = &loss[0];
      eventV[i].weight = Xw[i];
      if(beta == -1) // Cosine dissimilarity
      {
        eventV[i].l2norm = std::sqrt(std::inner_product(
          eventV[i].loss, eventV[i].loss + eventV[i].size, eventV[i].loss, 0.0));
      }
    }
  }


  vec<KMconstrainedSparse::centroid<int, double> >
    centroidV(centroid.size());
  vec<double> centroidContainer(centroidV.size() * d, 0);
  if(true)
  {
    // Decide the structure of centroid:
    SEXP tmp = centroid[0];
    bool isSparse = !Rf_isNumeric(tmp);
    if(!isSparse)
    {
      for(int i = 0, iend = centroidV.size(); i < iend; ++i)
      {
        centroidV[i].size = d;
        NumericVector loss = centroid[i];
        double *l = &centroidContainer[0] + INT(i) * d;
        std::copy(loss.begin(), loss.end(), l);
        centroidV[i].loss = l;
        // centroidV[i].mean.assign(loss.begin(), loss.end());
      }
    }
    else
    {
      for(int i = 0, iend = centroidV.size(); i < iend; ++i)
      {
        centroidV[i].size = d;
        List ele = centroid[i];
        IntegerVector rgn = ele[0];
        NumericVector loss = ele[1];
        centroidV[i].loss = &centroidContainer[0] + INT(i) * d;
        for(int j = 0, jend = rgn.size(); j < jend; ++j)
          centroidV[i].loss[rgn[j] - 1] = loss[j];
      }
    }


    // minkMag() is defined in "h/vectorMag.hpp".
    for(int i = 0, iend = centroidV.size(); i < iend; ++i)
    {
      centroidV[i].mag = minkMag<int, double, false> (
        centroidV[i].loss, centroidV[i].size, minkP);
      // beta < 0 uses cosine distance, need l2 norm.
      if(beta == -1) centroidV[i].l2norm = std::sqrt(std::inner_product(
        centroidV[i].loss, centroidV[i].loss + centroidV[i].size,
        centroidV[i].loss, 0.0));
    }
  }


  // Zerobase eventV's region indices.
  if(true)
  {
    for(int i = 0, iend = X.size(); i < iend; ++i)
    {
      for(int j = 0, jend = eventV[i].size; j < jend; ++j)
        --eventV[i].region[j];
    }
  }


  vec<double> D(centroidV.size() * eventV.size());


  vec<KMconstrainedSparse::eventCentroidIndex<int> >
    eventCentroidIndexV(eventV.size());
  {
    for(unsigned i = 0, iend = eventCentroidIndexV.size(); i < iend; ++i)
    {
      eventCentroidIndexV[i].centroidID = 0;
    }
  }


  if(true) // Clustering
  {
    vec<bool> clusterChanged(centroidV.size(), true);
    vec<int> preEventCluster(eventV.size(), 0);
    double preSumOfD = std::numeric_limits<double>::max();


    int Kiter = 0;
    int &relaErrSize = convergenceTail;
    vec<double> relaErrV(relaErrSize);
    double *relaErr = &relaErrV[0];
    for(int i = 0; i < relaErrSize; ++i)
      relaErr[i] = std::numeric_limits<double>::max();


    // std::size_t Dsize = D.size();
    vec<orderIndtype> Dorder, DorderAux;
    while(true)
    {
      KMconstrainedSparse::compDfun<int, double, beta> (
        centroidV.size(), eventV.size(), minkP,
        &D[0], &eventV[0], &centroidV[0], clusterChanged, maxCore);


      KMconstrainedSparse::assignMedoid<int, orderIndtype, double> T(
          clusterSizeUpperBound, &eventV[0], eventCentroidIndexV,
          D, Dorder, DorderAux, paraSortInplaceMerge, maxCore);


      std::size_t howManyDiff = 0;
      clusterChanged.resize(clusterChanged.size(), 0);
      for(int i = 0, iend = eventV.size(); i < iend; ++i)
      {
        if(Kiter == 0 or eventCentroidIndexV[i].centroidID != preEventCluster[i])
        {
          clusterChanged[preEventCluster[i]] = true;
          clusterChanged[eventCentroidIndexV[i].centroidID] = true;
          preEventCluster[i] = eventCentroidIndexV[i].centroidID;
          ++howManyDiff;
        }
      }


      std::sort(eventCentroidIndexV.begin(), eventCentroidIndexV.end(),
                KMconstrainedSparse::byCent<int>);
      double ssDiff = 0;
      if(beta > 0) ssDiff = std::abs(std::pow(T.sumOfD / preSumOfD, 1 / minkP) - 1);
      else ssDiff = std::abs(T.sumOfD / preSumOfD - 1);
      std::copy(relaErr + 1, relaErr + relaErrSize, relaErr);
      relaErr[relaErrSize - 1] = ssDiff;
      bool thresholdMet = true;
      for(int i = 0; i < relaErrSize; ++i)
      {
        thresholdMet = thresholdMet and
          (relaErr[i] < tailConvergedRelaErr);
      }


      preSumOfD = T.sumOfD;
      if(verbose)
      {
        Rcout << howManyDiff << " events reassigned, ";
        if(beta < 0)
        {
          Rcout << "sum of in-cluster cosine dissimilarities = ";
          Rcout << T.sumOfD << ", ";
        }
        else
        {
          Rcout << "total in-cluster Minkowski distance before root = ";
          Rcout << T.sumOfD;
          Rcout << ", ";
        }
        if(howManyDiff != 0)
          Rcout << "relative change = " << ssDiff * 100 << "%\n";
      }


      if(thresholdMet or howManyDiff == 0 or Kiter > maxIter) break;


      int k = 0, j = 0, kprior = 0;
      for(int i = 1, iend = eventV.size(); ; ++i)
      {
        if(i == iend or eventCentroidIndexV[i].centroidID != eventCentroidIndexV[i - 1].centroidID)
        {
          k = eventCentroidIndexV[i - 1].centroidID;
          centroidV[k].eventCentroidIndexUp = i;
          centroidV[k].eventCentroidIndexLow = j;


          for(int u = kprior + 1; u < k; ++u)
          {
            centroidV[u].eventCentroidIndexLow = i;
            centroidV[u].eventCentroidIndexUp = i;
          }


          kprior = k;
          if(i == iend) break;
          j = i;
        }
      }


      KMconstrainedSparse::updateCentroidV<int, double, beta> (minkP,
        centroidV, &eventV[0], &eventCentroidIndexV[0], clusterChanged, maxCore);


      ++Kiter;
    }
  }


  List rst(centroidV.size());
  {
    int j = 0, k = 0;
    for(int i = 1, iend = eventCentroidIndexV.size(); ; ++i)
    {
      if(i >= iend or eventCentroidIndexV[i].centroidID !=
         eventCentroidIndexV[i - 1].centroidID)
      {
        IntegerVector eventID(i - j);
        NumericVector eventToCentroidDistance(i - j);
        for(int u = 0, uend = i - j, v = j; u < uend; ++u, ++v)
        {
          eventID[u] = eventCentroidIndexV[v].eventID;
          eventToCentroidDistance[u] = minkD<
            KMconstrainedSparse::event<int, double>,
            KMconstrainedSparse::centroid<int, double>, 1,
            int, double, beta, true> (eventV[eventID[u]], centroidV[k], minkP);
          ++eventID[u];
        }
        rst[k] = List::create(
          Named("centroid") = NumericVector(centroidV[k].loss, centroidV[k].loss + centroidV[k].size),
          Named("clusterMember") = eventID,
          Named("member2centroidDistance") = eventToCentroidDistance);
        if(i >= iend) break;
        j = i; // The containers need not be initialized.
        ++k;
      }
    }
  }


  // Recover 1-based region indices.
  for(int i = 0, iend = eventV.size(); i < iend; ++i)
  {
    for(int j = 0, jend = eventV[i].size; j < jend; ++j)
      ++eventV[i].region[j];
  }


  return rst;
}




template<typename orderIndtype>
inline List sparseKMconstrainedCppBetaRealized
  (
      List X,
      int d,
      List centroid,
      NumericVector Xw,
      NumericVector maxClusterWeights,
      double minkP,
      int maxCore,
      int convergenceTail,
      double tailConvergedRelaErr,
      int maxIter,
      bool paraSortInplaceMerge,
      bool verbose
  )
{
  if(minkP == 2) // beta == 2
    return sparseKMconstrainedCppTemplate<orderIndtype, 2> (
        X, d, centroid, Xw, maxClusterWeights, minkP, maxCore, convergenceTail,
        tailConvergedRelaErr, maxIter, paraSortInplaceMerge, verbose);


  if(minkP == 1) // l1 norm
    return sparseKMconstrainedCppTemplate<orderIndtype, 1> (
        X, d, centroid, Xw, maxClusterWeights, minkP, maxCore, convergenceTail,
        tailConvergedRelaErr, maxIter, paraSortInplaceMerge, verbose);


  if(minkP == 0) // max norm.
    return sparseKMconstrainedCppTemplate<orderIndtype, 0> (
        X, d, centroid, Xw, maxClusterWeights, minkP, maxCore, convergenceTail,
        tailConvergedRelaErr, maxIter, paraSortInplaceMerge, verbose);


  // p is an integer and 3 <= p <= 35.
  if(std::abs(int(minkP) / minkP - 1) < 1e-10 and minkP >= 3 and minkP <= 35)
    return sparseKMconstrainedCppTemplate<orderIndtype, 3> (
        X, d, centroid, Xw, maxClusterWeights, minkP, maxCore, convergenceTail,
        tailConvergedRelaErr, maxIter, paraSortInplaceMerge, verbose);


  if(minkP == -1e308) // cosine distance
    return sparseKMconstrainedCppTemplate<orderIndtype, -1> (
        X, d, centroid, Xw, maxClusterWeights, minkP, maxCore, convergenceTail,
        tailConvergedRelaErr, maxIter, paraSortInplaceMerge, verbose);


  // Standard Minkowsiki distance.
  return sparseKMconstrainedCppTemplate<orderIndtype, 4> (
      X, d, centroid, Xw, maxClusterWeights, minkP, maxCore, convergenceTail,
      tailConvergedRelaErr, maxIter, paraSortInplaceMerge, verbose);
}




// [[Rcpp::export]]
List sparseKMconstrainedCpp
(
    List X,
    int d,
    List centroid,
    NumericVector Xw,
    NumericVector clusterWeightUpperBound,
    double minkP,
    int maxCore,
    int convergenceTail,
    double tailConvergedRelaErr,
    int maxIter,
    bool paraSortInplaceMerge,
    bool verbose
)
{
  std::size_t Dsize = std::size_t(X.size()) * std::size_t(centroid.size());


  if(Dsize < 255)
    return sparseKMconstrainedCppBetaRealized<unsigned char>
    (
        X, d, centroid, Xw, clusterWeightUpperBound,
        minkP, maxCore, convergenceTail,
        tailConvergedRelaErr,
        maxIter, paraSortInplaceMerge, verbose
    );


  if(Dsize < 65535)
    return sparseKMconstrainedCppBetaRealized<unsigned short>
    (
        X, d, centroid, Xw, clusterWeightUpperBound,
        minkP, maxCore, convergenceTail,
        tailConvergedRelaErr,
        maxIter, paraSortInplaceMerge, verbose
    );


  if(Dsize < 4294967295)
    return sparseKMconstrainedCppBetaRealized<unsigned>
    (
        X, d, centroid, Xw, clusterWeightUpperBound,
        minkP, maxCore, convergenceTail,
        tailConvergedRelaErr,
        maxIter, paraSortInplaceMerge, verbose
    );


  return sparseKMconstrainedCppBetaRealized<std::size_t>
    (
        X, d, centroid, Xw, clusterWeightUpperBound,
        minkP, maxCore, convergenceTail,
        tailConvergedRelaErr,
        maxIter, paraSortInplaceMerge, verbose
    );
}




























