// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <Rcpp.h>
# include <RcppParallel.h>
# include "h/macros.hpp"
# include "h/dnyTasking.hpp"
# include "h/vectorMag.hpp"
# include "h/distanceClasses.hpp"
using namespace Rcpp;
using namespace RcppParallel;


namespace KMsparse {


template<typename indtype, typename valtype>
struct event: public E<indtype, valtype>
{
  event(): E<indtype, valtype>() {}
  vec<valtype> DtoEvent; // DtoEvent in event is because some cluster can be exceptionally big,
  // and parallization on clusters would not help
};


template<typename indtype, typename valtype>
struct centroid: public E<indtype, valtype>
{
  bool changed;
  bool toChange; // initial 0
  indtype eventCentroidIndexLow, eventCentroidIndexUp;
  centroid(): E<indtype, valtype>() {}
};


template<typename indtype, typename valtype>
inline void addEventLossToCentriodLoss(
    valtype *rstCentroidLoss, event<indtype, valtype> &x)
{
  for(indtype i = 0; i < x.size; ++i)
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
{ return x.centroidID < y.centroidID; }


//before this function, eventCentroidIndex has been sorted by its member centroidID
template<typename indtype, typename valtype, int beta>
inline void updateCentroid(
    centroid<indtype, valtype> &c, event<indtype, valtype> *eventVbegin,
    eventCentroidIndex<indtype> *eventCentroidV, valtype p)
{
  c.changed = c.toChange;
  c.toChange = 0;
  if(!c.changed or c.eventCentroidIndexLow == c.eventCentroidIndexUp) return;
  std::fill(c.loss, c.loss + c.size, 0);
  valtype sumWeight = 0;
  for(indtype i = c.eventCentroidIndexLow,
      iend = c.eventCentroidIndexUp; i < iend; ++i)
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
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      updateCentroid<indtype, valtype, beta> (
          centroidV[objI], eventVbegin, eventCentroidV, p);
    }
  }


  updateCentroidV(
    valtype p,
    vec<centroid<indtype, valtype> > &centroidV,
    event<indtype, valtype> *eventVbegin,
    eventCentroidIndex<indtype> *eventCentroidV,
    indtype maxCore):
    p(p),
    centroidV(centroidV), eventVbegin(eventVbegin),
    eventCentroidV(eventCentroidV)
  {
    dynamicTasking dt(maxCore, centroidV.size()); dT = &dt;
    parallelFor(0, dT->NofCore, *this);
  }
};




//---------------------------------------------update centroid procedure done




// return 0 if centroid doesn't change, else 1
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
  for(centroid<indtype, valtype> * i = begin; i < end; ++i)
  {
    if(i->changed) x.DtoEvent[i - begin] =
      minkD<event<indtype, valtype>,
            centroid<indtype, valtype>, 1,
            int, double, beta, false> (x, *i, p);
    valtype d = x.DtoEvent[i - begin];
    if(d < minD)
    {
      minD = d;
      whichBest = i - begin;
    }
  }
  eC.centroidID = whichBest;
  if(xPriorCentriod == eC.centroidID) return 0;


  begin[xPriorCentriod].toChange = 1;
  begin[eC.centroidID].toChange = 1;
  xPriorCentriod = eC.centroidID;
  return 1;
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
    eventV(eventV), eventPriorCentriodV(eventPriorCentriodV),
    centroidV(centroidV), eventCentroidIndexV(eventCentroidIndexV)
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
List sparseKMtemplate(
    List X,
    int d,
    List centroid,
    NumericVector Xw,
    double minkP,
    int maxCore,
    int maxIter,
    bool verbose)
{
  INT N = X.size();
  vec<KMsparse::event<int, double> > eventV(N);


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


  if(true) // Initialize event-centroid distances.
  {
    for(int i = 0, iend = eventV.size(); i < iend; ++i)
    {
      eventV[i].DtoEvent.resize(centroid.size(), std::numeric_limits<double>::max());
    }
  }


  vec<KMsparse::centroid<int, double> > centroidV(centroid.size());
  vec<double> centroidContainer(centroid.size() * INT(d), 0);
  if(true) // Initialize centroids.
  {
    bool isSparse = true;
    SEXP tmp = centroid[0];
    isSparse = !Rf_isNumeric(tmp);
    if(!isSparse)
    {
      for(int i = 0, iend = centroidV.size(); i < iend; ++i)
      {
        centroidV[i].size = d;
        NumericVector loss = centroid[i];
        double *l = &centroidContainer[0] + INT(i) * d;
        std::copy(loss.begin(), loss.end(), l);
        centroidV[i].loss = l;
        centroidV[i].changed = true;
        centroidV[i].toChange = false;
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
        centroidV[i].changed = true;
        centroidV[i].toChange = false;
      }
    }


    // minkMag() is defined in "h/vectorMag.hpp".
    for(int i = 0, iend = centroidV.size(); i < iend; ++i)
    {
      if(beta != -1)
      {
        centroidV[i].mag = minkMag<int, double, false> (
          centroidV[i].loss, centroidV[i].size, minkP);
      }
      else
      {
        centroidV[i].l2norm = std::sqrt(std::inner_product(
          centroidV[i].loss, centroidV[i].loss + centroidV[i].size,
          centroidV[i].loss, 0.0));
      }
    }


  }


  // Zerobase eventV's region indices
  if(true)
  {
    for(int i = 0, iend = X.size(); i < iend; ++i)
    {
      for(int j = 0, jend = eventV[i].size; j < jend; ++j)
        --eventV[i].region[j];
    }
  }


  vec<KMsparse::eventCentroidIndex<int> > eventCentroidIndexV(eventV.size());
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
      KMsparse::findBestCentroidForEventV<int, double, beta> (
          minkP, eventV, priorCentroidV, centroidV, eventCentroidIndexV, centroidDiff, maxCore);


      if(verbose) Rcout << centroidDiff << ", ";


      std::sort(eventCentroidIndexV.begin(), eventCentroidIndexV.end(), KMsparse::byCent<int>);
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


      KMsparse::updateCentroidV<int, double, beta> (
          minkP, centroidV, &*eventV.begin(), &eventCentroidIndexV[0], maxCore);


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
          eventToCentroidDistance[u] = minkD
            <KMsparse::event<int, double>,
             KMsparse::centroid<int, double>, 1,
             int, double, beta, true> (eventV[eventID[u]], centroidV[k], minkP);
          ++eventID[u];
        }
        rst[k] = List::create(
          Named("centroid") = NumericVector(centroidV[k].loss,
                centroidV[k].loss + centroidV[k].size),
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




// [[Rcpp::export]]
List sparseKMcpp(
    List X,
    int d,
    List centroid,
    NumericVector Xw,
    double minkP,
    int maxCore,
    int maxIter,
    bool verbose)
{
  if(minkP == 2) // beta == 2
    return sparseKMtemplate<2> (X, d, centroid, Xw, minkP, maxCore, maxIter, verbose);


  if(minkP == 1) // l1 norm
    return sparseKMtemplate<1> (X, d, centroid, Xw, minkP, maxCore, maxIter, verbose);


  if(minkP == 0) // max norm.
    return sparseKMtemplate<0> (X, d, centroid, Xw, minkP, maxCore, maxIter, verbose);


  // p is an integer and 3 <= p <= 35, use multiplication rather than std::pow().
  if(std::abs(int(minkP) / minkP - 1) < 1e-10 and minkP >= 3 and minkP <= 35)
    return sparseKMtemplate<3> (X, d, centroid, Xw, minkP, maxCore, maxIter, verbose);


  if(minkP == -1e308) // cosine distance
    return sparseKMtemplate<-1> (X, d, centroid, Xw, minkP, maxCore, maxIter, verbose);


  // Standard Minkowsiki distance.
  return sparseKMtemplate<4> (X, d, centroid, Xw, minkP, maxCore, maxIter, verbose);
}






















