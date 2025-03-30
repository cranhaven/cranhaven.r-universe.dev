#pragma once

//#include "parsecommandline.hpp"
#include "plancopts.h"
#include "aoadmm.hpp"
#include "gnsym.hpp"
#include "mu.hpp"
#include "hals.hpp"
#include "bppnmf.hpp"


namespace planc {
    template<typename T>
    class NMFDriver {
    protected:
        int m_argc{};
        char** m_argv{};
        int m_k{};
        arma::uword m_m{}, m_n{};
        std::string m_Afile_name;
        std::string m_outputfile_name;
        std::string m_w_init_file_name;
        std::string m_h_init_file_name;
        int m_num_it{};
        arma::mat LLF{};
        double objErr;

    public:
        virtual ~NMFDriver() = default;

        [[nodiscard]] const arma::mat& getLlf() const {
            return LLF;
        }

        [[nodiscard]] const arma::mat& getRlf() const {
            return RLF;
        }

        [[nodiscard]] const double& getobjErr() const {
            return objErr;
        }

        void setObjErr(double objerr) {
            objErr = objerr;
        }

    protected:
        arma::mat RLF{};

    public:
        [[nodiscard]] T getA() const {
            return A;
        }

        void setA(T a) {
            A = a;
        }

    protected:
        T A;
        arma::fvec m_regW;
        arma::fvec m_regH;
        double m_symm_reg{};
        int m_symm_flag{};
        bool m_adj_rand{};
        algotype m_nmfalgo{};
        double m_sparsity{};
        unsigned int m_compute_error{};
        normtype m_input_normalization{};
        int m_max_luciters{};
        int m_initseed{};
        int nCores{};

        // Variables for creating random matrix
        static constexpr int kW_seed_idx = 1210873;
        static constexpr int kprimeoffset = 17;
        static constexpr int kalpha = 1;
        static constexpr int kbeta = 0;
        static constexpr int kalphaSp = 5;
        static constexpr int kbetaSp = 10;

        template<class NMFTYPE>
        void CallNMF();

        virtual void commonParams(const params&pc) {
            this->m_nmfalgo = pc.getMLucalgo();
            this->m_input_normalization = pc.getMInputNormalization();
            this->m_k = pc.getMK();
            this->nCores = pc.n_cores();
            this->m_sparsity = pc.getMSparsity();
            this->m_num_it = pc.getMNumIt();
            this->m_regW = pc.getMRegW();
            this->m_regH = pc.getMRegH();
            this->m_m = pc.getMGlobalm();
            this->m_n = pc.getMGlobaln();
            this->m_compute_error = pc.m_compute_error;
            this->m_symm_reg = pc.getMSymmReg();
            this->m_symm_flag = 0;
            this->m_adj_rand = pc.m_adj_rand;
            this->m_max_luciters = pc.getMMaxLuciters();
            this->m_initseed = pc.getMInitseed();

            // Put in the default LUC iterations
            if (this->m_max_luciters == -1) {
                this->m_max_luciters = this->m_k;
            }

            // check the conditions for symm nmf
            if (this->m_symm_reg != -1) {
                this->m_symm_flag = 1;
                if (this->m_m != this->m_n) {
                    // TODO{seswar3}: Add file prefix check
                    ERR << "Symmetric Regularization enabled"
                            << " and input matrix is not square"
                            << "::m::" << this->m_m << "::n::" << this->m_n
                            << std::endl;
                    return;
                }
                if ((this->m_nmfalgo != ANLSBPP) && (this->m_nmfalgo != GNSYM)) {
                    ERR << "Symmetric Regularization enabled "
                            << "is only enabled for ANLSBPP and GNSYM"
                            << std::endl;
                    return;
                }
            }
            // pc.printConfig();
        }

        void parseParams(const params&pc) {
            this->m_Afile_name = pc.getMAfileName();
            this->m_outputfile_name = pc.getMOutputfileName();
            this->m_h_init_file_name = pc.getMHInitFileName();
            this->m_w_init_file_name = pc.getMWInitFileName();
            commonParams(pc);
        }

        virtual void loadMat(double t2) {
            tic();
            A.load(this->m_Afile_name, arma::coord_ascii);
            t2 = toc();
            INFO << "Successfully loaded input matrix A " << PRINTMATINFO(A)
                    << "(" << t2 << " s)" << std::endl;
            this->m_m = A.n_rows;
            this->m_n = A.n_cols;;
        }

        void genDense(double t2) {
            {
                setSeed(planc::NMFDriver<arma::mat>::kW_seed_idx);
                std::string rand_prefix("rand_");
                std::string type = this->m_Afile_name.substr(rand_prefix.size());
                assert(type == "normal" || type == "lowrank" || type == "uniform");
                tic();
                if (type == "uniform") {
                    if (this->m_symm_flag) {
                        A = arma::randu<arma::mat>(this->m_m, this->m_n);
                        A = 0.5 * (A + A.t());
                    }
                    else {
                        A = arma::randu<arma::mat>(this->m_m, this->m_n);
                    }
                }
                else if (type == "normal") {
                    if (this->m_symm_flag) {
                        A = arma::randn<arma::mat>(this->m_m, this->m_n);
                        A = 0.5 * (A + A.t());
                    }
                    else {
                        A = arma::randn<arma::mat>(this->m_m, this->m_n);
                    }
                    A.elem(find(A < 0)).zeros();
                }
                else {
                    if (this->m_symm_flag) {
                        auto Wtrue = arma::randu<arma::mat>(this->m_m, this->m_k);
                        A = Wtrue * Wtrue.t();

                        // Free auxiliary variables
                        Wtrue.clear();
                    }
                    else {
                        auto Wtrue = arma::randu<arma::mat>(this->m_m, this->m_k);
                        auto Htrue = arma::randu<arma::mat>(this->m_k, this->m_n);
                        A = Wtrue * Htrue;

                        // Free auxiliary variables
                        Wtrue.clear();
                        Htrue.clear();
                    }
                }
                if (this->m_adj_rand) {
                    A = kalpha * (A) + kbeta;
                    A = ceil(A);
                }
                t2 = toc();
                INFO << "generated random matrix A " << PRINTMATINFO(A)
                        << "(" << t2 << " s)" << std::endl;
            }
        }

        void genSparse(double t2) {
            setSeed(planc::NMFDriver<arma::sp_mat>::kW_seed_idx);
            std::string rand_prefix("rand_");
            std::string type = this->m_Afile_name.substr(rand_prefix.size());
            assert(type == "normal" || type == "lowrank" || type == "uniform");
            tic();
            if (type == "uniform") {
                if (this->m_symm_flag) {
                    double sp = 0.5 * this->m_sparsity;
                    A = arma::sprandu<arma::sp_mat>(this->m_m, this->m_n, sp);
                    A = 0.5 * (A + A.t());
                }
                else {
                    A = arma::sprandu<arma::sp_mat>(this->m_m, this->m_n,
                                                    this->m_sparsity);
                }
            }
            else if (type == "normal") {
                if (this->m_symm_flag) {
                    double sp = 0.5 * this->m_sparsity;
                    A = arma::sprandn<arma::sp_mat>(this->m_m, this->m_n, sp);
                    A = 0.5 * (A + A.t());
                }
                else {
                    A = arma::sprandn<arma::sp_mat>(this->m_m, this->m_n,
                                                    this->m_sparsity);
                }
            }
            else if (type == "lowrank") {
                if (this->m_symm_flag) {
                    double sp = 0.5 * this->m_sparsity;
                    auto mask = arma::sprandu<arma::sp_mat>(this->m_m, this->m_n,
                                                            sp);
                    mask = 0.5 * (mask + mask.t());
                    mask = arma::spones(mask);
                    arma::mat Wtrue = arma::randu(this->m_m, this->m_k);
                    A = arma::sp_mat(mask % (Wtrue * Wtrue.t()));

                    // Free auxiliary space
                    Wtrue.clear();
                    mask.clear();
                }
                else {
                    auto mask = arma::sprandu<arma::sp_mat>(this->m_m, this->m_n,
                                                            this->m_sparsity);
                    mask = arma::spones(mask);
                    arma::mat Wtrue = arma::randu(this->m_m, this->m_k);
                    arma::mat Htrue = arma::randu(this->m_k, this->m_n);
                    A = arma::sp_mat(mask % (Wtrue * Htrue));

                    // Free auxiliary space
                    Wtrue.clear();
                    Htrue.clear();
                    mask.clear();
                }
            }
            // Adjust and project non-zeros
            arma::sp_mat::iterator start_it = A.begin();
            arma::sp_mat::iterator end_it = A.end();
            for (arma::sp_mat::iterator it = start_it; it != end_it; ++it) {
                double curVal = (*it);
                if (this->m_adj_rand) {
                    (*it) = ceil(kalphaSp * curVal + kbeta);
                }
                if ((*it) < 0)
                    (*it) = kbetaSp;
            }
            t2 = toc();
            INFO << "generated random matrix A " << PRINTMATINFO(A)
                    << "(" << t2 << " s)" << std::endl;
        }

        template<class NMFTYPE>
        void outRes(NMFTYPE nmfA) {
            // Save the factor matrices
            if (!this->m_outputfile_name.empty()) {
                std::string WfileName = this->m_outputfile_name + "_W";
                std::string HfileName = this->m_outputfile_name + "_H";

                LLF.save(WfileName, arma::raw_ascii);
                RLF.save(HfileName, arma::raw_ascii);
            }
        }

        virtual void loadWHInit(arma::mat&W, arma::mat&H) {
            if (!this->m_h_init_file_name.empty() && !this->m_w_init_file_name.empty()) {
                W.load(m_w_init_file_name, arma::coord_ascii);
                H.load(m_h_init_file_name, arma::coord_ascii);
                this->m_k = W.n_cols;
            }
            else {
                W = arma::randu<arma::mat>(this->m_m, this->m_k);
                H = arma::randu<arma::mat>(this->m_n, this->m_k);
            }
        }

        virtual void setSeed(int seed) {
            arma::arma_rng::set_seed(seed);
        }

    public:
        explicit NMFDriver(const params&pc) {
            this->parseParams(pc);
        }

        virtual void callNMF() {
            switch (this->m_nmfalgo) {
                case MU:
                    CallNMF<MUNMF<T>>();
                    break;
                case HALS:
                    CallNMF<HALSNMF<T>>();
                    break;
                case ANLSBPP:
                    CallNMF<BPPNMF<T>>();
                    break;
                case AOADMM:
                    CallNMF<AOADMMNMF<T>>();
                    break;
                case GNSYM:
                    CallNMF<GNSYMNMF<T>>();
                    break;
                default:
                    ERR << "Unsupported algorithm " << this->m_nmfalgo << std::endl;
            }
        }
    };

    template<>
    template<class NMFTYPE>
    void NMFDriver<arma::mat>::CallNMF() {
        // Generate/Read data matrix
        double t2 = 0;
        if (!this->m_Afile_name.empty()) loadMat(t2);
        else genDense(t2);

        // Normalize the input matrix
        if (this->m_input_normalization != NONE) {
            tic();
            if (this->m_input_normalization == L2NORM) {
                A = arma::normalise(A);
            }
            else if (this->m_input_normalization == MAXNORM) {
                double maxnorm = 1 / A.max();
                A = maxnorm * A;
            }
            t2 = toc();
            INFO << "Normalized A (" << t2 << "s)" << std::endl;
        }

        // Set parameters and call NMF
        setSeed(this->m_initseed);
        arma::mat W;
        arma::mat H;
        loadWHInit(W, H);
        if (this->m_symm_flag) {
            double meanA = arma::mean(arma::mean(A));
            H = 2 * std::sqrt(meanA / this->m_k) * H;
            W = H;
            if (this->m_symm_reg == 0.0) {
                double symreg = A.max();
                this->m_symm_reg = symreg * symreg;
            }
        }

        NMFTYPE nmfAlgorithm(A, W, H, nCores);
        nmfAlgorithm.num_iterations(this->m_num_it);
        nmfAlgorithm.symm_reg(this->m_symm_reg);
        nmfAlgorithm.updalgo(this->m_nmfalgo);
        // Always compute error for shared memory case
        // nmfAlgorithm.compute_error(this->m_compute_error);

        if (!this->m_regW.empty()) {
            nmfAlgorithm.regW(this->m_regW);
        }
        if (!this->m_regH.empty()) {
            nmfAlgorithm.regH(this->m_regH);
        }

        INFO << "completed constructor" << PRINTMATINFO(A) << std::endl;
        tic();
        nmfAlgorithm.computeNMF();
        t2 = toc();
        INFO << "time taken:" << t2 << std::endl;
        this->LLF = nmfAlgorithm.getLeftLowRankFactor();
        this->RLF = nmfAlgorithm.getRightLowRankFactor();
        this->objErr = nmfAlgorithm.objErr();
        outRes(nmfAlgorithm);
    }

    template<>
    template<class NMFTYPE>
    void NMFDriver<arma::sp_mat>::CallNMF() {
        // Generate/Read data matrix
        double t2 = 0;
        if (!this->m_Afile_name.empty()) loadMat(t2);
        else genSparse(t2);
        // Normalize the input matrix
        if (this->m_input_normalization != NONE) {
            tic();
            if (this->m_input_normalization == L2NORM) {
                A = arma::normalise(A);
            }
            else if (this->m_input_normalization == MAXNORM) {
                double maxnorm = 1 / A.max();
                A = maxnorm * A;
            }
            t2 = toc();
            INFO << "Normalized A (" << t2 << "s)" << std::endl;
        }

        // Set parameters and call NMF
        setSeed(this->m_initseed);
        arma::mat W;
        arma::mat H;
        loadWHInit(W, H);
        if (this->m_symm_flag) {
            double meanA = arma::mean(arma::mean(A));
            H = 2 * std::sqrt(meanA / this->m_k) * H;
            W = H;
            if (this->m_symm_reg == 0.0) {
                double symreg = A.max();
                this->m_symm_reg = symreg * symreg;
            }
        }

        NMFTYPE nmfAlgorithm(A, W, H, nCores);
        nmfAlgorithm.num_iterations(this->m_num_it);
        nmfAlgorithm.symm_reg(this->m_symm_reg);
        nmfAlgorithm.updalgo(this->m_nmfalgo);
        // Always compute error for shared memory case
        // nmfAlgorithm.compute_error(this->m_compute_error);

        if (!this->m_regW.empty()) {
            nmfAlgorithm.regW(this->m_regW);
        }
        if (!this->m_regH.empty()) {
            nmfAlgorithm.regH(this->m_regH);
        }

        INFO << "completed constructor" << PRINTMATINFO(A) << std::endl;
        tic();
        nmfAlgorithm.computeNMF();
        t2 = toc();
        INFO << "time taken:" << t2 << std::endl;
        this->LLF = nmfAlgorithm.getLeftLowRankFactor();
        this->RLF = nmfAlgorithm.getRightLowRankFactor();
        this->objErr = nmfAlgorithm.objErr();
        outRes(nmfAlgorithm);
    }
}
