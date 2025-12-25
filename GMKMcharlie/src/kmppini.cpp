// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <Rcpp.h>
# include <random>
# include <RcppParallel.h>
# include "h/dnyTasking.hpp"
# include "h/macros.hpp"
# include "h/distanceClasses.hpp"
using namespace Rcpp;
using namespace RcppParallel;
# define RNG std::mt19937_64


namespace kmppini {


template<typename indtype, typename valtype>
struct event: public E<indtype, valtype>
{
  event(): E<indtype, valtype>(){};
  vec<valtype> d2other;
  event(indtype size) {this->size = size; this->l2norm = 0;}
};




// ee == 0: dense-dense
// ee == 3: sparse-sparse
template<typename indtype, typename valtype, int ee, int beta>
struct event2othersD: public Worker
{
  indtype whichEvent;
  indtype Nevent;
  valtype p;
  event<indtype, valtype> *eventV;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      eventV[whichEvent].d2other[objI] =
        minkD<event<indtype, valtype>, event<indtype, valtype>,
              ee, indtype, valtype, beta, false> (
            eventV[whichEvent], eventV[objI], p);
    }
  }
  event2othersD(indtype thisEvent, event<indtype, valtype> *eventV,
                indtype Nevent, valtype p, indtype maxCore):
    whichEvent(thisEvent), Nevent(Nevent), p(p), eventV(eventV)
  {
    eventV[whichEvent].d2other.resize(Nevent);
    dynamicTasking dt(maxCore, Nevent); dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};


template<typename indtype, typename valtype>
struct collectMinDistance: public Worker
{
  indtype Nevent;
  indtype medoidCollectionSize;
  indtype *medoidCollection;
  indtype *freeEvents; // has size = Nevent - medoidCollectionSize.
  valtype *freeEventsMinD; // Calculated by this function struct.
  event<indtype, valtype> *eventV;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      indtype targetEvent = freeEvents[objI];
      valtype minimalD = std::numeric_limits<valtype>::max();
      for(indtype i = 0; i < medoidCollectionSize; ++i)
      {
        minimalD = std::min<valtype> (
          minimalD, eventV[medoidCollection[i]].d2other[targetEvent]);
      }
      freeEventsMinD[objI] = minimalD;
    }
  }
  collectMinDistance(event<indtype, valtype> *eventV,
                     indtype Nevent,
                     indtype *medoidCollection,
                     indtype medoidCollectionSize,
                     indtype *freeEvents,
                     valtype *freeEventsMinD,
                     indtype maxCore):
    Nevent(Nevent), medoidCollectionSize(medoidCollectionSize),
    medoidCollection(medoidCollection), freeEvents(freeEvents),
    freeEventsMinD(freeEventsMinD), eventV(eventV)
  {
    dynamicTasking dt(maxCore, Nevent - medoidCollectionSize);
    dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};


template<typename indtype, typename valtype, bool stochastic>
inline indtype selectEventBasedOnD(
    valtype *freeEventsMinD, indtype NfreeEvents, RNG &rng)
{
  indtype rst = -1;
  if(stochastic)
  {
    std::uniform_real_distribution<valtype> U(0, 1);
    valtype sumD = std::accumulate(
      freeEventsMinD, freeEventsMinD + NfreeEvents, 0.0);
    for(indtype i = 0; i < NfreeEvents; ++i)
    {
      valtype r = U(rng);
      if(r * sumD <= freeEventsMinD[i]) // Choose it
      {
        rst = i;
        break;
      }
      sumD -= freeEventsMinD[i];
    }
    if(rst == -1) rst = NfreeEvents - 1;
  }
  else
  {
    rst = std::max_element(
      freeEventsMinD, freeEventsMinD + NfreeEvents) - freeEventsMinD;
  }
  return rst; // The event ID would be freeEvents[rst];
}


}




template<typename indtype, typename valtype, bool stochastic, int ee, int beta>
inline void kmppIni(vec<indtype> &medoids, kmppini::event<indtype, valtype> *X,
                    indtype Xsize, indtype firstEvent, indtype K,
                    valtype p, indtype maxCore, std::size_t rngSeed, const bool verbose)
{
  medoids.resize(K);
  medoids.resize(1);
  medoids[0] = firstEvent;
  vec<indtype> freeEvents(Xsize - 1);
  for(indtype i = 0; i < firstEvent; ++i) freeEvents[i] = i;
  for(indtype i = firstEvent + 1; i < Xsize; ++i) freeEvents[i - 1] = i;
  // Xsize is the number of events.
  vec<valtype> freeEventsMinD(Xsize - 1);
  RNG rng(rngSeed);
  if(verbose) Rcout << "Number of centroids found: 1, ";
  for(indtype i = 1; i < K; ++i)
  {
    kmppini::event2othersD<indtype, valtype, ee, beta> (
        medoids[i - 1], X, Xsize, p, maxCore);
    kmppini::collectMinDistance<indtype, valtype> (
      X, Xsize, &medoids[0], medoids.size(),
      &freeEvents[0], &freeEventsMinD[0], maxCore);
    indtype ind = kmppini::selectEventBasedOnD<indtype, valtype, stochastic> (
      &freeEventsMinD[0], freeEventsMinD.size(), rng);
    medoids.push_back(freeEvents[ind]);
    freeEvents.erase(freeEvents.begin() + ind);
    freeEventsMinD.resize(freeEventsMinD.size() - 1);
    if(verbose) Rcout << i + 1 << ", ";
  }
  Rcout << "\n";
}




template<typename indtype, typename valtype, bool stochastic, int ee>
inline void kmppIniReduceBeta(
    vec<indtype> &medoids, kmppini::event<indtype, valtype> *X,
    indtype Xsize, indtype firstEvent, indtype K,
    valtype p, indtype maxCore, std::size_t seed, const bool verbose)
{
  if(p == 2) // beta == 2
    kmppIni<indtype, valtype, stochastic, ee, 2> (medoids, X, Xsize, firstEvent, K, p, maxCore, seed, verbose);
  else if(p == 1)
    kmppIni<indtype, valtype, stochastic, ee, 1> (medoids, X, Xsize, firstEvent, K, p, maxCore, seed, verbose);
  else if(p == 0)
    kmppIni<indtype, valtype, stochastic, ee, 0> (medoids, X, Xsize, firstEvent, K, p, maxCore, seed, verbose);
  else if(std::abs(int(p) / p - 1) < 1e-10 and p >= 3 and p <= 35)
    kmppIni<indtype, valtype, stochastic, ee, 3> (medoids, X, Xsize, firstEvent, K, p, maxCore, seed, verbose);
  else if(p == -1e308) // cosineD
    kmppIni<indtype, valtype, stochastic, ee, -1> (
        medoids, X, Xsize, firstEvent, K, p, maxCore, seed, verbose);
  else kmppIni<indtype, valtype, stochastic, ee, 4> (
      medoids, X, Xsize, firstEvent, K, p, maxCore, seed, verbose);
}




template<typename indtype, typename valtype, bool stochastic>
inline void kmppIniReduceBetaSparse(
    vec<indtype> &medoids, kmppini::event<indtype, valtype> *X,
    indtype Xsize, indtype firstEvent, indtype K,
    valtype p, indtype maxCore, bool sparse, std::size_t seed, const bool verbose)
{
  if(sparse) kmppIniReduceBeta<indtype, valtype, stochastic, 3> (
      medoids, X, Xsize, firstEvent, K, p, maxCore, seed, verbose);
  else kmppIniReduceBeta<indtype, valtype, stochastic, 0> (
      medoids, X, Xsize, firstEvent, K, p, maxCore, seed, verbose);
}




template<typename indtype, typename valtype>
inline void kmppIniReduceBetaSparseSto(
    vec<indtype> &medoids, kmppini::event<indtype, valtype> *X,
    indtype Xsize, indtype firstEvent, indtype K,
    valtype p, indtype maxCore, bool sparse,
    bool stochastic, std::size_t seed, const bool verbose)
{
  if(!stochastic) kmppIniReduceBetaSparse<indtype, valtype, false> (
      medoids, X, Xsize, firstEvent, K, p, maxCore, sparse, seed, verbose);
  else
  {
    kmppIniReduceBetaSparse<indtype, valtype, true> (
        medoids, X, Xsize, firstEvent, K, p, maxCore, sparse, seed, verbose);
  }
}




// [[Rcpp::export]]
IntegerVector KMppIniCpp(
    NumericMatrix X, int firstSelection,
    int K, double minkP, bool stochastic,
    double seed, int maxCore, bool verbose)
{
  std::size_t sed;
  std::memcpy(&sed, &seed, sizeof(sed));
  int N = X.ncol();
  int d = X.nrow();
  vec<kmppini::event<int, double> > eventV(
      N, kmppini::event<int, double> (d));
  for(int i = 0; i < N; ++i)
  {
    eventV[i].size = d;
    eventV[i].loss = &X[0] + INT(i) * d;
  }


  vec<int> medoids;
  // minkP = -1e308 means cosine distance.
  kmppIniReduceBetaSparseSto<int, double> (
    medoids, &eventV[0], eventV.size(), firstSelection - 1, K,
    minkP, maxCore, false, stochastic, sed, verbose);


  IntegerVector rst(K);
  for(int i = 0; i < K; ++i) rst[i] = medoids[i] + 1;
  return rst;
}




// [[Rcpp::export]]
IntegerVector KMppIniSparseCpp(
    List X, int d, int firstSelection,
    int K, double minkP, bool stochastic,
    double seed, int maxCore, bool verbose)
{
  std::size_t sed;
  std::memcpy(&sed, &seed, sizeof(sed));
  int N = X.size();
  vec<kmppini::event<int, double> > eventV(N);
  for(int i = 0; i < N; ++i)
  {
    List ele = X[i];
    IntegerVector rgn = ele[0];
    NumericVector los = ele[1];
    eventV[i].region = &rgn[0];
    eventV[i].loss = &los[0];
    eventV[i].size = rgn.size();
  }


  for(int i = 0; i < N; ++i)
    for(int j = 0; j < eventV[i].size; ++j)
      --eventV[i].region[j];


  vec<int> medoids;
  // minkP = -1e308 means cosine distance.
  kmppIniReduceBetaSparseSto<int, double> (
      medoids, &eventV[0], eventV.size(), firstSelection - 1, K,
      minkP, maxCore, true, stochastic, sed, verbose);


  IntegerVector rst(K);
  for(int i = 0; i < K; ++i) rst[i] = medoids[i] + 1;


  for(int i = 0; i < N; ++i)
    for(int j = 0; j < eventV[i].size; ++j)
      ++eventV[i].region[j];
  return rst;
}



























