// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <Rcpp.h>
# include <RcppParallel.h>
# include "h/macros.hpp"
# include "h/dnyTasking.hpp"
// # include "h/raisePower.hpp"
# include "h/distanceClasses.hpp"
using namespace Rcpp;
using namespace RcppParallel;


namespace KM {


template<typename indtype, typename valtype>
struct event: public E<indtype, valtype>
{
  event(): E<indtype, valtype>(){};
  vec<valtype> DtoEvent; // DtoEvent in event is because some cluster can be exceptionally big,
  // and parallization on clusters would not help
};


template<typename indtype, typename valtype>
struct centroid: public E<indtype, valtype>
{
  bool changed;
  bool toChange;
  indtype eventCentroidIndexLow, eventCentroidIndexUp;
  centroid(): E<indtype, valtype>(){};
};




template<typename indtype, typename valtype>
inline void addEventLossToCentriodLoss(
    valtype *rstCentroidLoss, indtype size, event<indtype, valtype> &x)
{
  for(indtype i = 0; i < size; ++i)
    rstCentroidLoss[i] += x.loss[i] * x.weight;
}




template<typename indtype>
struct eventCentroidIndex
{
  indtype centroidID, eventID;
};
template<typename indtype>
inline bool byCent(const eventCentroidIndex<indtype> &x,
                   const eventCentroidIndex<indtype> &y)
{ return x.centroidID < y.centroidID; }


//before this function, eventCentroidIndex has been sorted by its member centroidID
template<typename indtype, typename valtype, int beta>
inline void updateCentroid(
    centroid<indtype, valtype> &c, event<indtype, valtype> *eventVbegin,
    eventCentroidIndex<indtype> *eventCentroidV)
{
  c.changed = c.toChange;
  c.toChange = false;
  if(!c.changed or c.eventCentroidIndexLow == c.eventCentroidIndexUp) return;
  std::fill(c.loss, c.loss + c.size, 0);
  valtype sumWeight = 0;
  for(indtype i = c.eventCentroidIndexLow,
      iend = c.eventCentroidIndexUp; i < iend; ++i)
  {
    auto &tmp = eventVbegin[eventCentroidV[i].eventID];
    sumWeight += tmp.weight;
    addEventLossToCentriodLoss(c.loss, c.size, tmp);
  }
  valtype oneOverSumWeight = 1.0 / sumWeight;
  for(indtype i = 0, iend = c.size; i < iend; ++i)
    c.loss[i] *= oneOverSumWeight;
  if(beta == -1) // Cosine disimilarity requires l2norm.
    c.l2norm = std::sqrt(std::inner_product(c.loss, c.loss + c.size, c.loss, 0.0));
}


template<typename indtype, typename valtype, int beta>
struct updateCentroidV: public Worker
{
  vec<centroid<indtype, valtype> > &centroidV;
  event<indtype, valtype> *eventVbegin;
  eventCentroidIndex<indtype> *eventCentroidV;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      updateCentroid<indtype, valtype, beta> (centroidV[objI], eventVbegin, eventCentroidV);
    }
  }


  updateCentroidV(vec<centroid<indtype, valtype> > &centroidV,
                  event<indtype, valtype> *eventVbegin,
                  eventCentroidIndex<indtype> *eventCentroidV,
                  indtype maxCore):
    centroidV(centroidV), eventVbegin(eventVbegin),
    eventCentroidV(eventCentroidV)
  {
    dynamicTasking dt(maxCore, centroidV.size()); dT = &dt;
    parallelFor(0, dT->NofCore, *this);
  }
};




//---------------------------------------------update centroid procedure done




// return false if centroid doesn't change, else true
template<typename indtype, typename valtype, int beta>
inline bool findBestCentroidForEvent(
    valtype p,
    event<indtype, valtype> &x,
    eventCentroidIndex<indtype> &eC,
    indtype &xPriorCentriod,
    centroid<indtype, valtype> *begin,
    centroid<indtype, valtype> *end)
{
  indtype whichBest = 0;
  valtype minD = std::numeric_limits<valtype>::max();
  // Rcout << "\n\nevent to centroid distance = \n";
  for(centroid<indtype, valtype> * i = begin; i < end; ++i)
  {
    if (i->changed) x.DtoEvent[i - begin] =
      minkD<event<indtype, valtype>, centroid<indtype, valtype>,
            0, indtype, valtype, beta, false> (x, *i, p);
    // Rcout << x.DtoEvent[i - begin] << ", ";
    valtype d = x.DtoEvent[i - begin];
    if(d < minD)
    {
      minD = d;
      whichBest = i - begin;
    }
  }
  // Rcout << "\n\n";
  eC.centroidID = whichBest;
  if(xPriorCentriod == eC.centroidID) return false;


  begin[xPriorCentriod].toChange = true;
  begin[eC.centroidID].toChange = true;
  xPriorCentriod = eC.centroidID;
  return true;
}




template<typename indtype, typename valtype, int beta>
struct findBestCentroidForEventV: public Worker
{
  valtype p;
  vec<event<indtype, valtype> > &eventV;
  vec<indtype> &eventPriorCentriodV;
  vec<centroid<indtype, valtype> > &centroidV;
  vec<eventCentroidIndex<indtype> > &eventCentroidIndexV;
  indtype *centroidChange;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      eventCentroidIndexV[objI].eventID = objI;
      centroidChange[st] += findBestCentroidForEvent<indtype, valtype, beta> (
        p, eventV[objI], eventCentroidIndexV[objI], eventPriorCentriodV[objI],
        &*centroidV.begin(), &*centroidV.end());
    }
  }


  findBestCentroidForEventV(
    valtype p,
    vec<event<indtype, valtype> > &eventV,
    vec<indtype> &eventPriorCentriodV,
    vec<centroid<indtype, valtype> > &centroidV,
    vec<eventCentroidIndex<indtype> >&eventCentroidIndexV,
    indtype &totalDiff, indtype maxCore):
    p(p),
    eventV(eventV), eventPriorCentriodV(eventPriorCentriodV), centroidV(centroidV),
    eventCentroidIndexV(eventCentroidIndexV)
  {
    vec<indtype> array(maxCore, 0);
    centroidChange = &array[0];
    dynamicTasking dt(maxCore, eventV.size()); dT = &dt;
    parallelFor(0, dT->NofCore, *this);
    totalDiff = std::accumulate(centroidChange, centroidChange + maxCore, 0);
  }
};


}




template<int beta>
inline List KMcppTemplate(
    NumericMatrix X,
    NumericMatrix centroid,
    NumericVector Xw,
    double minkP,
    int maxCore,
    int maxIter,
    bool verbose)
{
  int d = X.nrow();
  int N = X.ncol();
  vec<KM::event<int, double> > eventV(X.ncol());


  if(true) // Initialize eventV.
  {
    for(int i = 0; i < N; ++i)
    {
      eventV[i].size = d;
      eventV[i].loss = &X[0] + INT(i) * d;
      eventV[i].weight = Xw[i];
      if(beta == -1) // Cosine dissimilarity
      {
        eventV[i].l2norm = std::sqrt(std::inner_product(
          eventV[i].loss, eventV[i].loss + d, eventV[i].loss, 0.0));
      }
    }
  }


  if(true) // Initialize event-centroid distances.
  {
    for(int i = 0, iend = eventV.size(); i < iend; ++i)
    {
      eventV[i].DtoEvent.resize(
          centroid.ncol(), std::numeric_limits<double>::max());
    }
  }


  vec<KM::centroid<int, double> > centroidV(centroid.ncol());
  vec<double> centroidContainer(centroid.begin(), centroid.end());
  if(true) // Initialize centroids.
  {
    for(int i = 0, iend = centroidV.size(); i < iend; ++i)
    {
      centroidV[i].loss = &centroidContainer[0] + INT(i) * d;
      centroidV[i].size = d;
      centroidV[i].changed = 1;
      centroidV[i].toChange = 0;
      if(beta == -1)
      {
        centroidV[i].l2norm = std::sqrt(std::inner_product(
          centroidV[i].loss, centroidV[i].loss + centroidV[i].size,
          centroidV[i].loss, 0.0));
      }
    }
  }


  vec<KM::eventCentroidIndex<int> > eventCentroidIndexV(eventV.size());
  if(true) // Initialize eventCentroidIndexV.
  {
    for(int i = 0, iend = eventCentroidIndexV.size(); i < iend; ++i)
      eventCentroidIndexV[i].centroidID = 0;
  }


  if(true) // Clustering.
  {
    vec<int> priorCentroidV(eventV.size(), 0);
    if(verbose) Rcout << "Number of events reassigned: ";
    int Kiter = 0;
    while(true)
    {
      int centroidDiff = 0;
      KM::findBestCentroidForEventV<int, double, beta> (
          minkP, eventV, priorCentroidV, centroidV, eventCentroidIndexV, centroidDiff, maxCore);


      if(verbose) Rcout << centroidDiff << ", ";


      std::sort(eventCentroidIndexV.begin(), eventCentroidIndexV.end(),
                KM::byCent<int>);
      if(centroidDiff == 0 or Kiter > maxIter) break;


      int k = 0, j = 0, kprior = 0;
      for(int i = 1, iend = eventV.size(); ; ++i)
      {
        if(i >= iend or eventCentroidIndexV[i].centroidID != eventCentroidIndexV[i - 1].centroidID)
        {
          k = eventCentroidIndexV[i - 1].centroidID;
          centroidV[k].eventCentroidIndexUp = i;
          centroidV[k].eventCentroidIndexLow = j;


          for(int u = kprior + 1; u < k; ++u)
            // If centriodID is consecutive, this loop is automatically useless
          {
            centroidV[u].eventCentroidIndexLow = i;
            centroidV[u].eventCentroidIndexUp = i;
            // Cluster centroidV[u] is now empty.
          }


          kprior = k;
          if(i >= iend) break;
          j = i;
        }
      }


      KM::updateCentroidV<int, double, beta> (
          centroidV, &*eventV.begin(), &eventCentroidIndexV[0], maxCore);


      ++Kiter;
    }
    if(verbose) Rcout << "\n";
  }


  List rst(centroidV.size());
  {
    int j = 0, k = 0;
    for(int i = 1, iend = eventCentroidIndexV.size(); ; ++i)
    {
      if(i >= iend or eventCentroidIndexV[i].centroidID != eventCentroidIndexV[i - 1].centroidID)
      {
        IntegerVector eventID(i - j);
        NumericVector eventToCentroidDistance(i - j);
        for(int u = 0, uend = i - j, v = j; u < uend; ++u, ++v)
        {
          eventID[u] = eventCentroidIndexV[v].eventID;
          eventToCentroidDistance[u] = minkD<
            KM::event<int, double>, KM::centroid<int, double>, 0,
            int, double, beta, true> (eventV[eventID[u]], centroidV[k], minkP);
          ++eventID[u];
        }
        rst[k] = List::create(
          // Named("centroid") = centroidV[k].mean,
          Named("centroid") = NumericVector(centroidV[k].loss, centroidV[k].loss + d),
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
List KMcpp(
    NumericMatrix X,
    NumericMatrix centroid,
    NumericVector Xw,
    double minkP,
    int maxCore,
    int maxIter,
    bool verbose)
{
  if(minkP == 2) // beta == 2
    return KMcppTemplate<2> (X, centroid, Xw, minkP, maxCore, maxIter, verbose);


  if(minkP == 1) // l1 norm
    return KMcppTemplate<1> (X, centroid, Xw, minkP, maxCore, maxIter, verbose);


  if(minkP == 0) // max norm.
    return KMcppTemplate<0> (X, centroid, Xw, minkP, maxCore, maxIter, verbose);


  // p is an integer and 3 <= p <= 35.
  if(minkP >= 3 and minkP <= 35 and std::abs(int(minkP) / minkP - 1) < 1e-10)
    return KMcppTemplate<3> (X, centroid, Xw, minkP, maxCore, maxIter, verbose);


  if(minkP == -1e308) // cosine distance
    return KMcppTemplate<-1> (X, centroid, Xw, minkP, maxCore, maxIter, verbose);


  // Standard Minkowsiki distance.
  return KMcppTemplate<4> (X, centroid, Xw, minkP, maxCore, maxIter, verbose);
}





















