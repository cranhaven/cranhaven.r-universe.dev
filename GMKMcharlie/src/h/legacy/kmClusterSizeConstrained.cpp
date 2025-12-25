// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <Rcpp.h>
# include <RcppParallel.h>
# include "h/macros.hpp"
# include "h/dnyTasking.hpp"
# include "h/paraSort.hpp"
using namespace Rcpp;
using namespace RcppParallel;


namespace kmCluterSizeConstraint
{
// rint means integer for regionID
// eint means integer type for event
// lf means float type for loss
template<typename indtype, typename valtype>
struct event
{
  // indtype size;
  valtype weight;
  // indtype *region;//integer from 0 to ~
  valtype *loss;
};


template<typename indtype, typename valtype>
struct centroid
{
  indtype eventCentroidIndexLow, eventCentroidIndexUp;
  vec<valtype> mean;
};


// template<typename indtype, typename valtype>
// inline valtype minkD(event<indtype, valtype> &x,
//                      centroid<indtype, valtype> &m,
//                      valtype p)
// {
//   valtype rst = 0;
//   for(indtype i = 0, iend = x.size; i < iend; ++i)
//   {
//     indtype k = x.region[i];
//     rst += std::pow(std::abs(x.loss[i] - m.mean[k]), p) -
//       std::pow(std::abs(m.mean[k]), p);
//   }
//   return (rst + m.mag) * x.weight;
// }


template<typename indtype, typename valtype>
inline valtype minkD(event<indtype, valtype> &x,
                     centroid<indtype, valtype> &m, valtype p)
{
  valtype rst = 0;
  for(indtype i = 0, iend = m.mean.size(); i < iend; ++i)
    rst += std::pow(std::abs(x.loss[i] - m.mean[i]), p);
  return rst * x.weight;
}


// template<typename indtype, typename valtype>
// inline void addEventLossToCentriodLoss(
//     vec<valtype> &rstCentroidLoss, event<indtype, valtype> &x)
// {
//   for(indtype i = 0, iend = x.size; i < iend; ++i)
//   {
//     rstCentroidLoss[x.region[i]] += x.loss[i] * x.weight;
//   }
// }


template<typename indtype, typename valtype>
inline void addEventLossToCentriodLoss(
    vec<valtype> &rstCentroidLoss, event<indtype, valtype> &x)
{
  for(indtype i = 0, iend = rstCentroidLoss.size(); i < iend; ++i)
  {
    rstCentroidLoss[i] += x.loss[i] * x.weight;
  }
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


  indtype clusterSizeUpperLimit;
  vec<eventCentroidIndex<indtype> > &eventCluster; // initial should be 0-1


  void rowToEventAndClusterInd(indtype i, indtype &E, indtype &M)
  {
    E = i / NofCluster;
    M = i % NofCluster;
  }


  vec<valtype> &D;
  vec<orderIndtype> &Dorder; // vec<std::size_t>order
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
  assignMedoid(indtype clusterSizeUpperLimit,
               vec<eventCentroidIndex<indtype> > &eventCluster,
               vec<valtype> &D,
               vec<orderIndtype> &Dorder,
               vec<orderIndtype> &DorderAux,
               bool paraSortInPlaceMerge,
               indtype maxCore):
    maxCore(maxCore),
    clusterSizeUpperLimit(clusterSizeUpperLimit),
    eventCluster(eventCluster), D(D), Dorder(Dorder),
    DorderAux(DorderAux)
  {
    ordering(paraSortInPlaceMerge);
    sumOfD = 0;
    NofEvent = eventCluster.size();
    vec<bool> eventAssigned(NofEvent, false);
    NofCluster = D.size() / NofEvent;
    vec<indtype> clusterSize(NofCluster, 0);


    indtype k = 0; // iterates eventMedoid
    for(INT i = 0, iend = Dorder.size(); k < NofEvent and i < iend; ++i)
    {
      indtype tmpEventID = 0;
      indtype tmpCenID = 0;
      rowToEventAndClusterInd(Dorder[i], tmpEventID, tmpCenID);
      if(eventAssigned[tmpEventID] or
           clusterSize[tmpCenID] >= clusterSizeUpperLimit) continue;
      eventAssigned[tmpEventID] = true;
      ++clusterSize[tmpCenID];
      eventCluster[k].eventID = tmpEventID;
      eventCluster[k].centroidID = tmpCenID;
      sumOfD += D[Dorder[i]];
      ++k;
    }
    std::sort(eventCluster.begin(), eventCluster.end(), byEvent<int>);
  }
};


//before this function, eventCentroidIndex has been sorted by its member centroidID
template<typename indtype, typename valtype>
// template<typename indtype, typename valtype>
void updateCentroid(indtype whichCluster, centroid<indtype, valtype> *C,
                    event<indtype, valtype> *eventVbegin,
                    eventCentroidIndex<indtype> *eventCentroidV,
                    vec<bool> &clusterChanged, valtype p)
{
  centroid<indtype, valtype> &c = C[whichCluster];
  if(clusterChanged[whichCluster] == 0 or
       c.eventCentroidIndexLow == c.eventCentroidIndexUp) return;
  vec<valtype> &tmpV = c.mean;
  std::fill(tmpV.begin(), tmpV.end(), 0);
  valtype sumWeight = 0;
  for(indtype i = c.eventCentroidIndexLow, iend = c.eventCentroidIndexUp; i < iend; ++i)
  {
    sumWeight += eventVbegin[eventCentroidV[i].eventID].weight;
    addEventLossToCentriodLoss(tmpV, eventVbegin[eventCentroidV[i].eventID]);
  }
  for(indtype i = 0, iend = tmpV.size(); i < iend; ++i)
  {
    tmpV[i] /= sumWeight;
  }
}


template<typename indtype, typename valtype>
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
      std::size_t objI=0;
      if(!dT->nextTaskID(objI)) break;
      updateCentroid<indtype, valtype> (
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


template<typename indtype, typename valtype>
struct compDfun: public Worker
{
  indtype NofCluster;
  indtype NofEvent;
  valtype p;
  valtype *D;
  event<indtype, valtype> *EV;
  centroid<indtype, valtype> *CV;
  vec<bool> &clusterChanged;
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
    d = minkD(EV[whichEvent], CV[whichCluster], p);
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




template<typename orderIndtype>
List charlieKmeansStoreDclusterSizeConstrained
  (
      NumericMatrix X,
      NumericMatrix centroids,
      NumericVector Xw,
      int clusterSizeUpperBound,
      double minkP,
      int maxCore,
      int convergenceTail,
      double tailConvergedRelaErr,
      int maxIter,
      bool paraSortInplaceMerge,
      bool verbose
  )
{
  vec<kmCluterSizeConstraint::event<int, double> > eventV(X.ncol());
  if(true) // Initialize eventV.
  {
    double r = 1;
    bool weightExist = false;
    if(Xw.size() > 0)
    {
      weightExist = true;
      r = Xw.size() / std::accumulate(Xw.begin(), Xw.end(), 0.0);
    }
    double *x = &X[0];
    int d = X.nrow();
    for(int i = 0, iend = X.ncol(); i < iend; ++i)
    {
      eventV[i].loss = x + INT(i) * d;
      eventV[i].weight = 1;
      if(weightExist) eventV[i].weight = Xw[i] * r;
    }
  }


  vec<kmCluterSizeConstraint::centroid<int, double> >
    centroidV(centroids.ncol());
  if(true) // Initialize centroidV.
  {
    INT d = centroids.nrow();
    for(int i = 0, iend = centroidV.size(); i < iend; ++i)
    {
      double *loss = &centroids[0] + i * d;
      centroidV[i].mean.assign(loss, loss + d);
    }
  }


  // vec<losstype> D(centroidV.size() * eventV.size());
  vec<double> D(centroidV.size() * eventV.size());


  vec<kmCluterSizeConstraint::eventCentroidIndex<int> >
    eventCentroidIndexV(eventV.size());
  if(true)
  {
    for(int i = 0, iend = eventCentroidIndexV.size(); i < iend; ++i)
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
      kmCluterSizeConstraint::compDfun<int, double> (
          centroidV.size(), eventV.size(), minkP,
          &D[0], &eventV[0], &centroidV[0], clusterChanged, maxCore);


      kmCluterSizeConstraint::assignMedoid<int, orderIndtype, double> T(
          clusterSizeUpperBound, eventCentroidIndexV, D, Dorder, DorderAux,
          paraSortInplaceMerge, maxCore);


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
                kmCluterSizeConstraint::byCent<int>);
      double ssDiff = std::abs(std::pow(T.sumOfD / preSumOfD, 1 / minkP) - 1);
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
        Rcout << "total in-cluster Minkowski distance = ";
        Rcout << std::pow(T.sumOfD, 1.0 / minkP) << ", ";
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


      kmCluterSizeConstraint::updateCentroidV<int, double> (
          minkP, centroidV, &eventV[0], &eventCentroidIndexV[0], clusterChanged, maxCore);


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
          eventToCentroidDistance[u] =
            std::pow(minkD(eventV[eventID[u]], centroidV[k], minkP), 1 / minkP);
          ++eventID[u];
        }
        rst[k] = List::create(
          Named("centroid") = centroidV[k].mean,
          Named("clusterMember") = eventID,
          Named("member2centroidDistance") = eventToCentroidDistance);
        if(i >= iend) break;
        j = i; // The containers need not be initialized.
        ++k;
      }
    }
  }


  return rst;
}




// [[Rcpp::export]]
List paraKMsizeConstrained
(
    NumericMatrix X,
    NumericMatrix centroids,
    NumericVector Xw = NumericVector(0),
    int clusterSizeUpperBound = 2147483647,
    double minkP = 2,
    int maxCore = 7,
    int convergenceTail = 5,
    double tailConvergedRelaErr = 1e-4,
    int maxIter = 100,
    bool paraSortInplaceMerge = false,
    bool verbose = true
)
{
  std::size_t Dsize = std::size_t(X.ncol()) * std::size_t(centroids.ncol());


  if(Dsize < 255)
    return charlieKmeansStoreDclusterSizeConstrained<unsigned char>
    (
        X, centroids, Xw, clusterSizeUpperBound,
        minkP, maxCore, convergenceTail,
        tailConvergedRelaErr,
        maxIter, paraSortInplaceMerge, verbose
    );


  if(Dsize < 65535)
    return charlieKmeansStoreDclusterSizeConstrained<unsigned short>
    (
        X, centroids, Xw, clusterSizeUpperBound,
        minkP, maxCore, convergenceTail,
        tailConvergedRelaErr,
        maxIter, paraSortInplaceMerge, verbose
    );


  if(Dsize < 4294967295)
    return charlieKmeansStoreDclusterSizeConstrained<unsigned>
    (
        X, centroids, Xw, clusterSizeUpperBound,
        minkP, maxCore, convergenceTail,
        tailConvergedRelaErr,
        maxIter, paraSortInplaceMerge, verbose
    );


  return charlieKmeansStoreDclusterSizeConstrained<std::size_t>
    (
        X, centroids, Xw, clusterSizeUpperBound,
        minkP, maxCore, convergenceTail,
        tailConvergedRelaErr,
        maxIter, paraSortInplaceMerge, verbose
    );
}




























