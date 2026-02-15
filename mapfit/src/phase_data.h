#ifndef MAPFIT_DATA_TRAITS_H
#define MAPFIT_DATA_TRAITS_H

#include <Rcpp.h>
#include "traits.h"

template <typename VT, typename VW>
struct PHWeightSample{
  using TimeType = VT;
  using WeightType = VW;
  VT time;
  VW weights;
  double maxtime;
  
  PHWeightSample(const VT& _time, const VW& _weights, double _maxtime)
    : time(_time), weights(_weights), maxtime(_maxtime) {}

  inline int size() const {
    using trait = vector_traits<VT>;
    return trait::size(time);
  }
};

template <typename VT, typename VC, typename VI>
struct PHGroupSample{
  using TimeType = VT;
  using CountType = VC;
  using IndicatorType = VI;
  VT time;
  VC counts;
  VI indicators;
  double maxtime;
  int last;
  
  PHGroupSample(
    const VT& _time,
    const VC& _counts,
    const VI& _indicators,
    double _maxtime,
    int _last)
    : time(_time),
      counts(_counts),
      indicators(_indicators),
      maxtime(_maxtime),
      last(_last){}
  
  inline int size() const {
    using trait = vector_traits<VT>;
    return trait::size(time);
  }
};

template <typename VT, typename VC, typename VI>
struct PHGroupSamplePoi{
  using TimeType = VT;
  using CountType = VC;
  using IndicatorType = VI;
  VT time;
  VC counts;
  VI indicators;
  double maxtime;
  int last;
  double omega;
  
  PHGroupSamplePoi(
    const VT& _time,
    const VC& _counts,
    const VI& _indicators,
    double _maxtime,
    int _last,
    double _omega)
    : time(_time),
      counts(_counts),
      indicators(_indicators),
      maxtime(_maxtime),
      last(_last),
      omega(_omega){}
  
  inline int size() const {
    using trait = vector_traits<VT>;
    return trait::size(time);
  }
};

#endif
