//
// Created by andrew on 12/12/2023.
//
#include "nmf_lib.inl"
#define X(T) \
template planc::nmfOutput<double> planc::nmflib<T, double>::nmf(const T&x, const arma::uword&k, const arma::uword&niter, const std::string&algo, const int&nCores, const arma::Mat<double>&Winit,const arma::Mat<double>&Hinit); \
template planc::nmfOutput<double> planc::nmflib<T, double>::symNMF(const T&x, const arma::uword&k, const arma::uword&niter, const double& lambda, const std::string&algo, const int&nCores, const arma::Mat<double>&Hinit);
#include "nmf_types.inc"
#undef X
#define X(T) \
template planc::inmfOutput<double> planc::nmflib<T, double>::bppinmf(std::vector<std::shared_ptr<T>> objectlist, const arma::uword &k, const double &lambda, const arma::uword &niter, const bool &verbose, const int &nCores); \
template planc::inmfOutput<double> planc::nmflib<T, double>::bppinmf(std::vector<std::shared_ptr<T>> objectlist, const arma::uword &k, const double &lambda, const arma::uword &niter, const bool &verbose, const std::vector<arma::mat> &HinitList, const std::vector<arma::mat> &VinitList, const arma::mat &Winit, const int &nCores); \
template std::vector<std::shared_ptr<T>> planc::nmflib<T, double>::initMemSharedPtr(std::vector<T> objectList); \
template planc::oinmfOutput<double> planc::nmflib<T, double>::oinmf(std::vector<std::shared_ptr<T>> matPtrVec, const arma::uword &k, const int &nCores, const double &lambda, const arma::uword &maxEpoch, const arma::uword &minibatchSize, const arma::uword &maxHALSIter, const arma::uword &permuteChunkSize, const bool &verbose); \
template planc::oinmfOutput<double> planc::nmflib<T, double>::oinmf(std::vector<std::shared_ptr<T>> matPtrVec, const std::vector<arma::mat> &Hinit, const std::vector<arma::mat> &Vinit, const arma::mat &Winit, const std::vector<arma::mat> &Ainit, const std::vector<arma::mat> &Binit, std::vector<std::shared_ptr<T>> matPtrVecNew, const arma::uword &k, const int& nCores, const double &lambda, const arma::uword &maxEpoch, const arma::uword &minibatchSize, const arma::uword &maxHALSIter, const arma::uword &permuteChunkSize, const bool &verbose); \
template std::vector<arma::Mat<double>> planc::nmflib<T, double>::oinmf_project(std::vector<std::shared_ptr<T>> matPtrVec, const arma::mat &Winit, std::vector<std::shared_ptr<T>> matPtrVecNew, const arma::uword &k, const int& nCores, const double &lambda); \
template planc::uinmfOutput<double> planc::nmflib<T, double>::uinmf(const std::vector<std::shared_ptr<T>> &matPtrVec, const std::vector<std::shared_ptr<T>> &unsharedPtrVec, std::vector<int> whichUnshared, const arma::uword &k, const int& nCores, const arma::vec &lambda, const arma::uword &niter, const bool &verbose);
#include "inmf_types.inc"
#undef X
void planc::openblas_pthread_off(const openblas_handle_t libloc) {
    if (is_openmp()) {
        if (const std::function openblas_parallel = get_openblas_parallel(libloc)) {
            if (openblas_parallel() == 1) {
                const std::function openblas_set = get_openblas_set(libloc);
                openblas_set(1);
            }
        }
    }
}


void planc::openblas_pthread_on(const openblas_handle_t libloc) {
    if (is_openmp()) {
        if (const std::function openblas_parallel = get_openblas_parallel(libloc)) {
            if (openblas_parallel() == 1) {
                const std::function openblas_set = get_openblas_set(libloc);
                openblas_set(0);
            }
        }
    }
}

//
// planc::nmfOutput<double> (*ptrnmf)(const arma::Mat<double>&x, const arma::uword&k, const arma::uword&niter,
//                                    const std::string&algo, const int&nCores, const arma::Mat<double>&Winit,
//                                    const arma::Mat<double>&Hinit)(planc::nmflib<arma::mat>::nmf);
// planc::nmfOutput<double> (*ptrspnmf)(const arma::SpMat<double>&x, const arma::uword&k, const arma::uword&niter,
//                                    const std::string&algo, const int&nCores, const arma::Mat<double>&Winit,
//                                    const arma::Mat<double>&Hinit)(planc::nmflib<arma::sp_mat>::nmf);
// planc::nmfOutput<double> (*ptrsymnmf)(const arma::Mat<double>&x, const arma::uword&k, const arma::uword&niter, const double& lambda,
//                                    const std::string&algo, const int&nCores, const arma::Mat<double>&Hinit)(planc::nmflib<arma::mat>::symNMF);
// planc::nmfOutput<double> (*ptrspsymnmf)(const arma::SpMat<double>&x, const arma::uword&k, const arma::uword&niter, const double& lambda,
//                                    const std::string&algo, const int&nCores, const arma::Mat<double>&Hinit)(planc::nmflib<arma::sp_mat>::symNMF);
// planc::inmfOutput<double> (*ptrinmf)(std::vector<arma::mat> objectlist, arma::uword k, double lambda, arma::uword niter,
//                                      bool verbose, const int &nCores)(planc::nmflib<arma::mat>::bppinmf);
// planc::inmfOutput<double> (*ptrspinmf)(std::vector<arma::sp_mat> objectlist, arma::uword k, double lambda, arma::uword niter,
//                                      bool verbose, const int &nCores)(planc::nmflib<arma::sp_mat>::bppinmf);
// planc::inmfOutput<double> (*ptrh5inmf)(std::vector<planc::H5Mat> objectlist, arma::uword k, double lambda, arma::uword niter,
//                                      bool verbose, const int &nCores)(planc::nmflib<planc::H5Mat, double>::bppinmf);
// planc::inmfOutput<double> (*ptrh5spinmf)(std::vector<planc::H5SpMat> objectlist, arma::uword k, double lambda, arma::uword niter,
//                                      bool verbose, const int &nCores)(planc::nmflib<planc::H5SpMat, double>::bppinmf);
//std::map<std::string, planc::runNMFindex> NMFindexmap{{"W", planc::outW},
//                                                      {"H", planc::outH},
//                                                      {"objErr", planc::objErr}};
