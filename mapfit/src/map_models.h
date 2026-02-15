#ifndef MAPFIT_MAP_MODELS_H
#define MAPFIT_MAP_MODELS_H

#include "traits.h"

template <typename VecT, typename IVecT, typename MatT>
struct ErlangHMM {
  VecT alpha;
  VecT xi;
  VecT rate;
  IVecT shape;
  MatT P;
  
  ErlangHMM(const VecT& _alpha,
            const VecT& _xi,
            const VecT& _rate,
            const IVecT& _shape,
            const MatT& _P)
    : alpha(_alpha), xi(_xi), rate(_rate), shape(_shape), P(_P) {}

  inline int size() const {
    return vector_traits<VecT>::size(alpha);
  }
};

template <typename VecT, typename MatT>
struct ErlangHMMEres {
  VecT eb;
  VecT ew0;
  VecT ew1;
  MatT en;
  
  inline
    ErlangHMMEres(const VecT& _eb,
                  const VecT& _ew0,
                  const VecT& _ew1,
                  const MatT& _en)
    : eb(_eb), ew0(_ew0), ew1(_ew1), en(_en) {}
};

template <typename VecT, typename MatT, typename iVecT>
struct MAP {
  VecT alpha;
  VecT xi;
  MatT D0;
  MatT D1;
  MatT P0;
  MatT P1;
  iVecT diag;
  double qv;
  
  MAP(const VecT& _alpha,
      const VecT& _xi,
      const MatT& _D0,
      const MatT& _D1,
      const MatT& _P0,
      const MatT& _P1,
      const iVecT& _diag,
      double _qv)
    : alpha(_alpha), xi(_xi), D0(_D0), D1(_D1), P0(_P0), P1(_P1), diag(_diag), qv(_qv) {}
  
  inline int size() const {
    return vector_traits<VecT>::size(alpha);
  }
};

template <typename MAPT>
struct GMMPP {
  MAPT map;
  
  GMMPP(const MAPT& _map) : map(_map) {}
  
  inline int size() const {
    return map.size();
  }
};

template <typename VecT, typename MatT>
struct MAPEres {
  VecT eb;
  VecT ez;
  MatT en0;
  MatT en1;
  
  inline
    MAPEres(const VecT& _eb,
            const VecT& _ez,
            const MatT& _en0,
            const MatT& _en1)
      : eb(_eb), ez(_ez), en0(_en0), en1(_en1) {}
};

template <typename MatT>
struct MAPWorkSpace1 {
  MatT H0;
  MatT H1;
  
  MAPWorkSpace1(int m, int n, const MatT& _H0, const MatT& _H1) :
    H0(_H0), H1(_H1) {}
};

template <typename MatT>
struct ErlangHMMWorkSpace1 {

  ErlangHMMWorkSpace1(int m, int n) {}
};

template <typename MatT>
struct GMMPPWorkSpace {
  MatT G;
  MatT Psi1T;
  MatT Psi2T;
  MatT Psi1N;
  MatT Psi2N;
  MatT tmpm;
  
  GMMPPWorkSpace(
    const MatT& _G,
    const MatT& _Psi1T,
    const MatT& _Psi2T,
    const MatT& _Psi1N,
    const MatT& _Psi2N,
    const MatT& _tmpm) :
    G(_G),
    Psi1T(_Psi1T),
    Psi2T(_Psi2T),
    Psi1N(_Psi1N),
    Psi2N(_Psi2N),
    tmpm(_tmpm) {}
};

#endif
