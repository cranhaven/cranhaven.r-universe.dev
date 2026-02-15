#ifndef MAPFIT_MAP_DATA_H
#define MAPFIT_MAP_DATA_H

#include <Rcpp.h>
#include "traits.h"

template <typename VT>
struct MAPTimeSample{
  using TimeType = VT;
  VT time;
  double maxtime;
  
  MAPTimeSample(const VT& _time, double _maxtime)
    : time(_time), maxtime(_maxtime) {}

  inline int size() const {
    using trait = vector_traits<VT>;
    return trait::size(time);
  }
};

template <typename VT, typename VC, typename VI>
struct MAPGroupSample{
  using TimeType = VT;
  using CountType = VC;
  using IndicatorType = VI;
  VT time;
  VC counts;
  VI indicators;
  double maxtime;
  int maxcount;
  
  MAPGroupSample(
    const VT& _time,
    const VC& _counts,
    const VI& _indicators,
    double _maxtime,
    int _maxcount)
    : time(_time),
      counts(_counts),
      indicators(_indicators),
      maxtime(_maxtime),
      maxcount(_maxcount){}
  
  inline int size() const {
    using trait = vector_traits<VT>;
    return trait::size(time);
  }
};

#endif
