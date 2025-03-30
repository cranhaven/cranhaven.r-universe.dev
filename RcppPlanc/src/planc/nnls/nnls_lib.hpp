//
// Created by andrew on 3/21/2025.
//

#ifndef NNLS_LIB_H
#define NNLS_LIB_H

#include "../nmf/nmf_lib.hpp"

namespace planc {

    template<typename T, typename eT = typename T::elem_type>
    class NMFLIB_EXPORT nnlslib {

public:
    nnlslib() {
        openblas_pthread_off(get_openblas_handle());
    }

    ~nnlslib() = default;


    static arma::mat runbppnnls(const arma::mat &C, const T &B, const int &ncores);

    static arma::mat bppnnls_prod(const arma::mat &CtC, const arma::mat &CtB, const int& nCores = 2);


};

} // planc

#endif //NNLS_LIB_H
