//
// Created by andrew on 5/24/2024.
//

#ifndef PLANC_EMBEDDEDNMFDRIVER_H
#define PLANC_EMBEDDEDNMFDRIVER_H
#include "NMFDriver.hpp"

namespace planc {
    template<typename T>
    class EmbeddedNMFDriver : public NMFDriver<T> {
    protected:
        arma::mat Winit;
        arma::mat Hinit;

        virtual void parseParams(const internalParams<T>&pc) {
            this->A = pc.getMAMat();
            this->Winit = pc.getMWInitMat();
            this->Hinit = pc.getMHInitMat();
            this->m_Afile_name = "internal";
            this->commonParams(pc);
        }

        void loadWHInit(arma::mat&W, arma::mat&H) override {
            if (!Winit.is_empty()) {
                W = this->Winit;
                if (W.n_rows != this->m_m || W.n_cols != this->m_k) {
                    std::throw_with_nested(std::runtime_error("Winit must be of size " +
                                                              std::to_string(this->m_m) + " x " + std::to_string(
                                                                  this->m_k)));
                }
            }
            else {
                W = arma::randu<arma::mat>(this->m_m, this->m_k);
            }
            if (!Hinit.is_empty()) {
                H = this->Hinit;
                if (H.n_rows != this->m_n || H.n_cols != this->m_k) {
                    std::throw_with_nested(std::runtime_error("Hinit must be of size " +
                                                              std::to_string(this->m_n) + " x " + std::to_string(
                                                                  this->m_k)));
                }
            }
            else {
                H = arma::randu<arma::mat>(this->m_n, this->m_k);
            }
        }

    public:
        void loadMat(double t2) override {
            this->m_m = this->A.n_rows;
            this->m_n = this->A.n_cols;
        }

    private:
        template<class NMFTYPE>
        void outRes(NMFTYPE nmfA) {
        }

        void setSeed(int) override {
        }

    public:
        explicit EmbeddedNMFDriver(internalParams<T> pc) : NMFDriver<T>(pc) {
            this->EmbeddedNMFDriver::parseParams(pc);
        }
    };

    template<typename T>
    class symmEmbeddedNMFDriver final : public EmbeddedNMFDriver<T> {
        arma::mat Hinit;

        void parseParams(const internalSymmParams<T>&pc) {
            this->A = pc.getMAMat();
            if (!(this->A.n_rows == this->A.n_cols)) std::throw_with_nested(
                std::runtime_error("Input `x` is not square."));
            this->Hinit = pc.getMHInitMat();
            this->m_Afile_name = "internal";
            this->commonParams(pc);
            if (this->m_k >= this->A.n_rows) std::throw_with_nested(
                std::runtime_error("`k` must be less than `nrow(x)"));
        }

    public:
        explicit symmEmbeddedNMFDriver(internalSymmParams<T> pc) : EmbeddedNMFDriver<T>(pc) {
            this->parseParams(pc);
        }
    };
};
#endif //PLANC_EMBEDDEDNMFDRIVER_H
