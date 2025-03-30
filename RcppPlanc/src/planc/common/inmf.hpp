#pragma once

#ifdef _OPENMP
#include <omp.h>
#endif
#include <memory>
#include <vector>
#include "data.hpp"
#ifndef ARMA_64BIT_WORD
#define ARMA_64BIT_WORD
#endif

namespace planc {
    template<typename T>
    class INMF {
    protected:
        arma::uword k, nDatasets, nMax, nSum;
        int INMF_CHUNK_SIZE, m; // chunking
        std::vector<arma::uword> ncol_E; // vector of n_i
        std::vector<std::shared_ptr<T>> Ei; // each of size mxn_i
        std::vector<std::unique_ptr<T>> EiT; // each of size n_ixm
        std::vector<std::unique_ptr<arma::mat>> Hi; // each of size n_ixk
        std::vector<std::unique_ptr<arma::mat>> Vi; // each of size mxk
        std::vector<std::unique_ptr<arma::mat>> ViT; // each of size kxm
        std::unique_ptr<arma::mat> W; // mxk
        std::unique_ptr<arma::mat> WT; // kxm
        double lambda, sqrtLambda, objective_err;
        bool cleared;
        std::unique_ptr<arma::sp_mat> tempE;
        void load_whole_E(arma::uword i);

        virtual double computeObjectiveError() {
            // obj_i = ||E_i - (W + V_i)*H_i||_F^2 + lambda * ||V_i*H_i||_F^2
            // Let W + V = L
            // ||E - LH||_F^2 = ||E||_F^2 - 2*Tr(Ht*(Et*L)) + Tr((Lt*L)*(Ht*H))
            // ||V*H||_F^2 = Tr((Vt*V)*(Ht*H))
            //
            //  This way, no giant mxn matrix is created in the process.
            //
            double obj = 0;
            arma::mat* Wptr = this->W.get();
            arma::mat L(this->m, this->k); // (loading) L = W + V
            for (arma::uword i = 0; i < this->nDatasets; ++i) {
                // T* Eptr = this->Ei[i].get();
                // arma::sp_mat* Eptr = this->load_whole_E(i);
                this->load_whole_E(i);
                arma::sp_mat* Eptr = this->tempE.get();
                arma::mat* Hptr = this->Hi[i].get();
                arma::mat* Vptr = this->Vi[i].get();
                L = *Wptr + *Vptr;
                double sqnormE = arma::norm<arma::sp_mat>(*Eptr, "fro");
                sqnormE *= sqnormE;
                arma::mat LtL = L.t() * L; // k x k
                arma::mat HtH = Hptr->t() * *Hptr; // k x k
                double TrLtLHtH = arma::trace(LtL * HtH);
                // T Et = Eptr->t();
                arma::sp_mat Et = Eptr->t();
                arma::mat EtL = Et * L; // n_i x k
                double TrHtEtL = arma::trace(Hptr->t() * EtL);
                arma::mat VtV = Vptr->t() * *Vptr; // k x k
                double TrVtVHtH = arma::trace(VtV * HtH);
                obj += sqnormE - 2 * TrHtEtL + TrLtLHtH + this->lambda * TrVtVHtH;
            }
            return obj;
        }

        void constructObject(std::vector<std::shared_ptr<T>>&inputEi, arma::uword inputk, double inputlambda,
                             bool makeTranspose) {
            this->Ei = inputEi;
            this->k = inputk;
            this->m = this->Ei[0].get()->n_rows;
            try {
                if (this->k > this->m) {
                    throw std::invalid_argument("k must be <= m");
                }
            }
            catch (std::exception&ex) {
#ifdef USING_R
                std::string ex_str = ex.what();
                Rcpp::stop(ex_str);

#else
                throw ex;
#endif
            }
            this->cleared = false;
            this->INMF_CHUNK_SIZE = chunk_size_dense<typename T::elem_type>(k);
            this->nMax = 0;
            this->nSum = 0;
            this->nDatasets = 0;
#ifdef _VERBOSE
            Rcpp::Rcout << "k=" << k << "; m=" << m << std::endl;
#endif
            for (unsigned int i = 0; i < this->Ei.size(); ++i) {
                T* E = this->Ei[i].get();
                if (makeTranspose) {
                    T ET = E->t();
                    std::unique_ptr<T> ETptr = std::make_unique<T>(ET);
                    this->EiT.push_back(std::move(ETptr));
                }
                this->ncol_E.push_back(E->n_cols);
                if (E->n_cols > this->nMax) {
                    this->nMax = E->n_cols;
                }
                this->nSum += E->n_cols;
                this->nDatasets++;
            }
#ifdef _VERBOSE
            Rcpp::Rcout << "nMax=" << this->nMax << "; nSum=" << this->nSum << std::endl;
            Rcpp::Rcout << "nDatasets=" << this->nDatasets << std::endl;
#endif
            // this->initHWV();
            this->lambda = inputlambda;
            this->sqrtLambda = sqrt(lambda); //TODO
            //TODO implement common tasks i.e. norm, reg, etc
        }

        void checkK() {
            for (unsigned int i = 0; i < this->nDatasets; ++i) {
                try {
                    if (this->k != this->Hi[i]->n_cols) {
                        std::string msg = "Preset `k` (" + std::to_string(this->k) +
                                          ") does not match with H[" + std::to_string(i) + "]";
                        throw std::invalid_argument(msg);
                    }
                    if (this->k != this->Vi[i]->n_cols) {
                        std::string msg = "Preset `k` (" + std::to_string(this->k) +
                                          ") does not match with V[" + std::to_string(i) + "]";
                        throw std::invalid_argument(msg);
                    }
                }
                catch (std::exception&ex) {
#ifdef USING_R
                    std::string ex_str = ex.what();
                    Rcpp::stop(ex_str);

#else
                    throw ex;
#endif
                }
            }
            try {
                if (this->k != this->W->n_cols) {
                    std::string msg = "Preset `k` (" + std::to_string(this->k) +
                                      ") does not match with W";
                    throw std::invalid_argument(msg);
                }
            }
            catch (std::exception&ex) {
#ifdef USING_R
                std::string ex_str = ex.what();
                Rcpp::stop(ex_str);

#else
                    throw ex;
#endif
            }
        }

    public:
        INMF(std::vector<std::shared_ptr<T>> Ei, arma::uword k, double lambda, bool makeTranspose = true) {
            this->constructObject(Ei, k, lambda, makeTranspose);
            this->initW();
            this->initV();
            this->INMF::initH();
        }

        INMF(std::vector<std::shared_ptr<T>> Ei, arma::uword k, double lambda,
             const std::vector<arma::mat>&VinitList, const arma::mat&Winit, bool makeTranspose = true) {
            this->constructObject(Ei, k, lambda, makeTranspose);
            this->setW(Winit);
            this->setV(VinitList);
        }

        virtual void setH(std::vector<arma::mat>&Hinit) {
#ifdef _VERBOSE
            Rcpp::Rcout << "Taking initialized H matrices" << std::endl;
#endif
            if (Hinit.size() != this->nDatasets) {
                std::string msg = "Must provide " +
                                  std::to_string(this->nDatasets) +
                                  " H matrices";
                throw std::invalid_argument(msg);
            }
            std::unique_ptr<arma::mat> H;
            for (arma::uword i = 0; i < this->nDatasets; ++i) {
                try {
                    if (Hinit[i].n_cols == 0 && Hinit[i].n_rows == 0) {
                        this->initH();
                        return;
                    }
                    if (Hinit[i].n_cols != this->k || Hinit[i].n_rows != this->ncol_E[i]) {
                        std::string msg = "Each given H must be of size Ei[i].n_cols x " +
                                          std::to_string(this->k);
                        throw std::invalid_argument(msg);
                    }
                }
                catch (std::exception&ex) {
#ifdef USING_R
                    std::string ex_str = ex.what();
                    Rcpp::stop(ex_str);

#else
                    throw ex;
#endif
                }
                H = std::make_unique<arma::mat>();
                *H = Hinit[i];
                this->Hi.push_back(std::move(H));
            }
        }

        virtual void initH() {
#ifdef _VERBOSE
            Rcpp::Rcout << "Randomly initializing H matrices" << std::endl;
#endif
            std::unique_ptr<arma::mat> H;
            for (arma::uword i = 0; i < this->nDatasets; ++i) {
                H = std::make_unique<arma::mat>();
                *H = arma::randu<arma::mat>(this->ncol_E[i], this->k,
                                            arma::distr_param(0, 2));
                this->Hi.push_back(std::move(H));
            }
        }

        void setV(const std::vector<arma::mat>&Vinit, bool transpose = true) {
#ifdef _VERBOSE
            Rcpp::Rcout << "Taking initialized V matrices" << std::endl;
#endif
            try {
                if (Vinit.size() != this->nDatasets) {
                    std::string msg = "Must provide " +
                                      std::to_string(this->nDatasets) +
                                      " V matrices";
                    throw std::invalid_argument(msg);
                }
            }
            catch (std::exception&ex) {
#ifdef USING_R
                std::string ex_str = ex.what();
                Rcpp::stop(ex_str);
#else
                throw ex;
#endif
            }
            std::unique_ptr<arma::mat> V;
            std::unique_ptr<arma::mat> VT;
            for (arma::uword i = 0; i < this->nDatasets; ++i) {
                if (Vinit[i].n_cols == 0 && Vinit[i].n_rows == 0) {
                    this->initV();
                    return;
                }
                if (Vinit[i].n_cols != this->k || Vinit[i].n_rows != this->m) {
                    std::string msg = "All given Vs must be of size " +
                                      std::to_string(this->m) + " x " +
                                      std::to_string(this->k);
                    throw std::invalid_argument(msg);
                }
                V = std::make_unique<arma::mat>();
                *V = Vinit[i];
                if (transpose) {
                    VT = std::make_unique<arma::mat>();
                    *VT = V->t();
                    this->ViT.push_back(std::move(VT));
                }
                this->Vi.push_back(std::move(V));
            }
        }

        void initV() {
#ifdef _VERBOSE
            Rcpp::Rcout << "Randomly initializing V matrices" << std::endl;
#endif
            std::unique_ptr<arma::mat> V;
            std::unique_ptr<arma::mat> VT;
            for (arma::uword i = 0; i < this->nDatasets; ++i) {
                V = std::make_unique<arma::mat>();
                VT = std::make_unique<arma::mat>();
                *V = arma::randu<arma::mat>(this->m, this->k,
                                            arma::distr_param(0, 2));
                *VT = V->t();
                this->Vi.push_back(std::move(V));
                this->ViT.push_back(std::move(VT));
            }
        }

        void setW(const arma::mat&Winit, bool transpose = true) {
#ifdef _VERBOSE
            Rcpp::Rcout << "Taking initialized W matrix" << std::endl;
#endif
            if (Winit.n_cols == 0 && Winit.n_rows == 0) {
                this->initW();
                return;
            }
            if (Winit.n_cols != this->k || Winit.n_rows != this->m) {
                std::string msg = "Given W must be of size " +
                                  std::to_string(this->m) + " x " +
                                  std::to_string(this->k) + " but is " +
                                  std::to_string(Winit.n_rows) + " x " +
                                  std::to_string(Winit.n_cols);
                throw std::invalid_argument(msg);
            }
            this->W = std::make_unique<arma::mat>();
            *this->W = Winit;
            if (transpose) {
                this->WT = std::make_unique<arma::mat>();
                *this->WT = (*this->W).t();
            }
        }

        void initW() {
#ifdef _VERBOSE
            Rcpp::Rcout << "Randomly initializing W matrix" << std::endl;
#endif
            this->W = std::make_unique<arma::mat>();
            this->WT = std::make_unique<arma::mat>();
            *this->W = arma::randu<arma::mat>(this->m, this->k,
                                              arma::distr_param(0, 2));
            *this->WT = (*this->W).t();
        }

        [[nodiscard]] double objErr() const {
            return this->objective_err;
        }

        arma::mat getHi(arma::uword i) {
            return *(this->Hi[i].get());
        }

        std::vector<std::unique_ptr<arma::mat>> getAllH() {
            return std::move(this->Hi);
        }

        arma::mat getVi(arma::uword i) {
            return *(this->Vi[i].get());
        }

        std::vector<std::unique_ptr<arma::mat>> getAllV() {
            return std::move(this->Vi);
        }

        [[nodiscard]] arma::mat getW() const {
            return *(this->W);
        }

        virtual ~INMF() { clear(); }

        void clear() {
            if (!this->cleared) {
                for (unsigned int i = 0; i < Ei.size(); ++i) {
                    Ei[i].reset();
                    // EiT[i].reset();
                }
                for (unsigned int i = 0; i < EiT.size(); ++i) {
                    EiT[i].reset();
                }
                for (auto&i: Hi) {
                    i.reset();
                }
                for (auto&i: Vi) {
                    i.reset();
                }
                for (auto&i: ViT) {
                    i.reset();
                }
                this->W.reset();
                if (this->WT != nullptr) this->WT.reset();
                this->tempE.reset();
            }
            this->cleared = true;
        }
    }; // class INMF

    template<>
    inline void INMF<arma::sp_mat>::load_whole_E(arma::uword i) {
        // Make a real matrix copied from what's pointed by this->Ei[i]
        arma::sp_mat tempSparse = *(this->Ei[i].get());
        // Make new unique_ptr to manage the memory
        auto tempSparseUniquePtr = std::make_unique<arma::sp_mat>(tempSparse);
        this->tempE = std::move(tempSparseUniquePtr);
        // return this->Ei[i].get();
    }

    template<>
    inline void INMF<arma::mat>::load_whole_E(arma::uword i) {
        auto tempSparse = std::make_unique<arma::sp_mat>(*(this->Ei[i]));

        // Get raw pointer before transferring ownership
        // arma::sp_mat* rawPtr = tempSparse.get();

        // Move unique_ptr to a class member to ensure it stays alive
        this->tempE = std::move(tempSparse);

        // return rawPtr;
    }

    template<>
    inline void INMF<H5SpMat>::load_whole_E(arma::uword i) {
        H5SpMat* Eptr = this->Ei[i].get();
        // arma::uword m = Eptr->n_rows;
        arma::uword n = Eptr->n_cols;
        this->tempE.reset();
        arma::sp_mat tempSparse = Eptr->cols(0, n - 1);
        auto tempSparseUniquePtr = std::make_unique<arma::sp_mat>(tempSparse);
        // auto tempSparse = std::make_unique<arma::sp_mat>(m, n);
        // (*tempSparse) = Eptr->cols(0, n - 1);
        // Get raw pointer before transferring ownership
        // arma::sp_mat* rawPtr = tempSparseUniquePtr.get();

        // Move unique_ptr to a class member to ensure it stays alive
        this->tempE = std::move(tempSparseUniquePtr);
        // return rawPtr;
    }

    template<>
    inline void INMF<H5Mat>::load_whole_E(arma::uword i) {
        H5Mat* Eptr = this->Ei[i].get();
        // arma::uword m = Eptr->n_rows;
        arma::uword n = Eptr->n_cols;
        auto out = std::make_unique<arma::sp_mat>(m, n);
        // Load on-disk dense matrix by chunks and convert to sparse and fill
        int numChunks = n / this->INMF_CHUNK_SIZE;
        if (numChunks * this->INMF_CHUNK_SIZE < n) numChunks++;
        for (int i = 0; i < numChunks; ++i) {
            int spanStart = i * this->INMF_CHUNK_SIZE;
            int spanEnd = (i + 1) * this->INMF_CHUNK_SIZE - 1;
            if (spanEnd > n - 1) spanEnd = n - 1;
            arma::mat dense_span = Eptr->cols(spanStart, spanEnd);
            arma::sp_mat sparse_span(dense_span);
            out->cols(spanStart, spanEnd) = sparse_span;
        }
        // Get raw pointer before transferring ownership
        // arma::sp_mat* rawPtr = out.get();
        // Move unique_ptr to a class member to ensure it stays alive
        this->tempE = std::move(out);
        // return rawPtr;
    }

}
