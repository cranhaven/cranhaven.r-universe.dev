// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <Rcpp.h>
# include <RcppParallel.h>
# include "h/macros.hpp"
# include "h/dnyTasking.hpp"
using namespace Rcpp;
using namespace RcppParallel;


namespace kmClassic
{


template<typename indtype, typename valtype>
struct event
{
  valtype weight;
  valtype *loss;
  vec<valtype> DtoEvent; // DtoEvent in event is because some cluster can be exceptionally big,
  // and parallization on clusters would not help
};


template<typename indtype, typename valtype>
struct centroid
{
  bool changed;
  bool toChange; // initial 0
  indtype eventCentroidIndexLow, eventCentroidIndexUp;
  valtype sumMeanSquare;
  vec<valtype> mean;
};


template<typename indtype, typename valtype>
inline valtype eucD(event<indtype, valtype> &x, centroid<indtype, valtype> &m)
{
  valtype rst = 0;
  for(indtype i = 0, iend = m.mean.size(); i < iend; ++i)
    rst += (x.loss[i] - m.mean[i]) * (x.loss[i] - m.mean[i]);
  return rst * x.weight;
}


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
{ return x.centroidID < y.centroidID; }


//before this function, eventCentroidIndex has been sorted by its member centroidID
template<typename indtype, typename valtype>
inline void updateCentroid(
    centroid<indtype, valtype> &c, event<indtype, valtype> *eventVbegin,
    eventCentroidIndex<indtype> *eventCentroidV)
{
  c.changed = c.toChange;
  c.toChange = 0;
  if(!c.changed or c.eventCentroidIndexLow == c.eventCentroidIndexUp) return;
  vec<valtype> &tmpV = c.mean;
  std::fill(tmpV.begin(), tmpV.end(), 0);
  valtype sumWeight = 0;
  for(indtype i = c.eventCentroidIndexLow,
      iend = c.eventCentroidIndexUp; i < iend; ++i)
  {
    sumWeight += eventVbegin[eventCentroidV[i].eventID].weight;
    addEventLossToCentriodLoss(tmpV, eventVbegin[eventCentroidV[i].eventID]);
  }
  c.sumMeanSquare = 0;
  for(indtype i = 0, iend = tmpV.size(); i < iend; ++i)
  {
    tmpV[i] /= sumWeight;
    c.sumMeanSquare += tmpV[i] * tmpV[i];
  }
}


template<typename indtype, typename valtype>
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
      updateCentroid(centroidV[objI], eventVbegin, eventCentroidV);
    }
  }


  updateCentroidV(vec<centroid<indtype, valtype> > &centroidV,
                  event<indtype, valtype> *eventVbegin,
                  eventCentroidIndex<indtype> *eventCentroidV,
                  int maxCore):
    centroidV(centroidV), eventVbegin(eventVbegin),
    eventCentroidV(eventCentroidV)
  {
    dynamicTasking dt(maxCore, centroidV.size()); dT = &dt;
    parallelFor(0, dT->NofCore, *this);
  }
};




//---------------------------------------------update centroid procedure done




// return 0 if centroid doesn't change, else 1
template<typename indtype, typename valtype>
inline bool findBestCentroidForEvent(
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
    if (i->changed) x.DtoEvent[i - begin] = eucD<indtype, valtype> (x, *i);
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




template<typename indtype, typename valtype>
struct findBestCentroidForEventV: public Worker
{
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
      centroidChange[st] += findBestCentroidForEvent<indtype, valtype> (
        eventV[objI], eventCentroidIndexV[objI], eventPriorCentriodV[objI],
        &*centroidV.begin(), &*centroidV.end());
    }
  }


  findBestCentroidForEventV(vec<event<indtype, valtype> > &eventV,
                            vec<indtype> &eventPriorCentriodV,
                            vec<centroid<indtype, valtype> > &centroidV,
                            vec<eventCentroidIndex<indtype> >&eventCentroidIndexV,
                            indtype &totalDiff, indtype maxCore):
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




// [[Rcpp::export]]
List paraKMclassic(
    NumericMatrix X, NumericMatrix centroid,
    NumericVector Xw = NumericVector(0), int maxCore = 7,
    int maxIter = 100, bool verbose = true)
{
  vec<kmClassic::event<int, double> > eventV(X.ncol());
  if(true) // Initialize eventV.
  {
    double r = 1;
    bool weightExist = false;
    if(Xw.size() > 0)
    {
      weightExist = true;
      r = Xw.size() / std::accumulate(Xw.begin(), Xw.end(), 0.0);
    }
    int d = X.nrow();
    int N = X.ncol();
    for(int i = 0; i < N; ++i)
    {
      eventV[i].loss = &X[0] + i * d;
      eventV[i].weight = 1;
      if(weightExist) eventV[i].weight = Xw[i] * r;
    }
  }


  if(true) // Initialize event-centroid distances.
  {
    for(int i = 0, iend = eventV.size(); i < iend; ++i)
    {
      eventV[i].DtoEvent.resize(centroid.ncol(), std::numeric_limits<double>::max());
    }
  }


  // Rcout << "1.1\n";


  vec<kmClassic::centroid<int, double> > centroidV(centroid.ncol());
  if(true) // Initialize centroids.
  {
    int d = centroid.nrow();
    for(int i = 0, iend = centroidV.size(); i < iend; ++i)
    {
      double *loss = &centroid[0] + i * d;
      centroidV[i].mean.assign(loss, loss + d);
      centroidV[i].sumMeanSquare = std::inner_product(
        centroidV[i].mean.begin(), centroidV[i].mean.end(),
        centroidV[i].mean.begin(), 0.0);
      centroidV[i].changed = 1;
      centroidV[i].toChange = 0;
    }
  }


  vec<kmClassic::eventCentroidIndex<int> > eventCentroidIndexV(eventV.size());
  if(true) // Initialize eventCentroidIndexV.
  {
    for(int i = 0, iend = eventCentroidIndexV.size(); i < iend; ++i)
    {
      eventCentroidIndexV[i].centroidID = 0;
    }
  }


  if(true) // Clustering.
  {
    vec<int> priorCentroidV(eventV.size(), 0);
    if(verbose) Rcout << "How many events have been reassigned: ";
    int Kiter = 0;
    while(true)
    {
      int centroidDiff = 0;
      kmClassic::findBestCentroidForEventV<int, double> (
          eventV, priorCentroidV, centroidV, eventCentroidIndexV, centroidDiff, maxCore);


      if(verbose) Rcout << centroidDiff << ", ";


      std::sort(eventCentroidIndexV.begin(), eventCentroidIndexV.end(),
                kmClassic::byCent<int>);
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


      kmClassic::updateCentroidV<int, double> (
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
          eventToCentroidDistance[u] = std::sqrt(eucD(eventV[eventID[u]], centroidV[k]));
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



























