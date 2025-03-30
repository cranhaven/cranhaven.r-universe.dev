//
// Created by andrew on 12/12/2023.
//

#ifndef PLANC_PLANCOPTS_H
#define PLANC_PLANCOPTS_H

#include "utils.h"

namespace planc {
    struct params {
        virtual ~params() = default;

        params() {
            // common to all algorithms.
            this->nCores = 0;
            this->m_lucalgo = ANLSBPP;
            this->m_input_normalization = NONE;
            this->m_compute_error = false;
            this->m_num_it = 20;
            this->m_num_k_blocks = 0;
            this->m_dim_tree = true;
            this->m_adj_rand = false;
            // file names
            //this->m_Afile_name = NULL;         // X (features) matrix for jointnmf
            //this->m_outputfile_name = NULL;
            //this->m_Sfile_name = NULL;         // S (connection) matrix for jointnmf
            // std::string m_init_file_name;
            // nmf related values
            this->m_k = 20;
            this->m_globalm = 0;
            this->m_globaln = 0;
            this->m_initseed = 193957;
            // algo related values
            this->m_regW = arma::zeros<arma::fvec>(2);
            this->m_regH = arma::zeros<arma::fvec>(2);
            this->m_sparsity = 0.01f;

            // distnmf related values
            this->m_pr = 1;
            this->m_pc = 1;
            this->m_symm_reg = -1;
            this->m_symm_flag = 0;
            this->m_tolerance = -1.;
            // dist ntf
            this->m_num_modes = 1;
            //this->m_dimensions = NULL;
            //this->m_proc_grids = NULL;
            //this->m_regularizers = NULL;
            // LUC params (optional)
            this->m_max_luciters = -1;
            // hiernmf related values
            this->m_num_nodes = 1;
            // jointnmf values
            this->alpha = 0.;
            this->beta = 0.;
            this->feat_type = 2;
            this->conn_type = 0;
            this->m_gamma = 0.;
            this->m_unpartitioned = false;

            // distjointnmf values
            // grid size for the connection matrices (cpr x cpc)
            //this->m_conn_grids = NULL;
            this->m_cpr = 0;
            this->m_cpc = 0;
        }

        void set_n_cores(const int n_cores) {
            nCores = n_cores;
        }

        [[nodiscard]] int n_cores() const {
            return nCores;
        }

        // common to all algorithms.
        algotype m_lucalgo{};
        normtype m_input_normalization{};
        bool m_compute_error{};
        int m_num_it{};
        int nCores{};
        int m_num_k_blocks{};
        bool m_dim_tree{};
        bool m_adj_rand{};

        // file names
        std::string m_Afile_name; // X (features) matrix for jointnmf
        std::string m_outputfile_name;
        std::string m_Sfile_name; // S (connection) matrix for jointnmf
        std::string m_h_init_file_name;
        std::string M_W_init_file_name;

        // nmf related values
        arma::uword m_k{};
        arma::uword m_globalm{};
        arma::uword m_globaln{};
        int m_initseed{};

        // algo related values
        arma::fvec m_regW;
        arma::fvec m_regH;
        float m_sparsity{};
        int m_symm_flag{};

        [[nodiscard]] algotype getMLucalgo() const {
            return m_lucalgo;
        }

        virtual void setMLucalgo(std::string mLucalgo) {
            try {
                m_lucalgo = algomap.at(mLucalgo);
            }
            catch (const std::out_of_range&e) {
                std::throw_with_nested(
                    std::runtime_error("Please choose `algo` from \"anlsbpp\", \"admm\", \"hals\" or \"mu\"."));
            }
        }

        normtype getMInputNormalization() const {
            return m_input_normalization;
        }

        void setMInputNormalization(normtype mInputNormalization) {
            m_input_normalization = mInputNormalization;
        }

        virtual void setMSymmFlag(int msymmflag) {
            m_symm_flag = msymmflag;
        }

        int getMSymmFlag() const {
            return m_symm_flag;
        }

        bool isMComputeError() const {
            return m_compute_error;
        }

        void setMComputeError(bool mComputeError) {
            m_compute_error = mComputeError;
        }

        [[nodiscard]] virtual int getMNumIt() const {
            return m_num_it;
        }

        virtual void setMNumIt(int mNumIt) {
            m_num_it = mNumIt;
        }

        int getMNumKBlocks() const {
            return m_num_k_blocks;
        }

        void setMNumKBlocks(int mNumKBlocks) {
            m_num_k_blocks = mNumKBlocks;
        }

        bool isMDimTree() const {
            return m_dim_tree;
        }

        void setMDimTree(bool mDimTree) {
            m_dim_tree = mDimTree;
        }

        bool isMAdjRand() const {
            return m_adj_rand;
        }

        void setMAdjRand(bool mAdjRand) {
            m_adj_rand = mAdjRand;
        }

        [[nodiscard]] virtual const std::string& getMAfileName() const {
            return m_Afile_name;
        }

        void setMAfileName(const std::string&mAfileName) {
            m_Afile_name = mAfileName;
        }

        virtual const std::string& getMOutputfileName() const {
            return m_outputfile_name;
        }

        void setMOutputfileName(const std::string&mOutputfileName) {
            m_outputfile_name = mOutputfileName;
        }

        const std::string& getMSfileName() const {
            return m_Sfile_name;
        }

        void setMSfileName(const std::string&mSfileName) {
            m_Sfile_name = mSfileName;
        }

        virtual const std::string& getMHInitFileName() const {
            return m_h_init_file_name;
        }

        void setMHInitFileName(const std::string&mHInitFileName) {
            m_h_init_file_name = mHInitFileName;
        }

        virtual const std::string& getMWInitFileName() const {
            return M_W_init_file_name;
        }

        void setMWInitFileName(const std::string&MWInitFileName) {
            M_W_init_file_name = MWInitFileName;
        }

        [[nodiscard]] virtual arma::uword getMK() const {
            return m_k;
        }

        virtual void setMK(arma::uword mK) {
            m_k = mK;
        }

        arma::uword getMGlobalm() const {
            return m_globalm;
        }

        void setMGlobalm(arma::uword mGlobalm) {
            m_globalm = mGlobalm;
        }

        arma::uword getMGlobaln() const {
            return m_globaln;
        }

        void setMGlobaln(arma::uword mGlobaln) {
            m_globaln = mGlobaln;
        }

        int getMInitseed() const {
            return m_initseed;
        }

        void setMInitseed(int mInitseed) {
            m_initseed = mInitseed;
        }

        const arma::fvec& getMRegW() const {
            return m_regW;
        }

        void setMRegW(const arma::fvec&mRegW) {
            m_regW = mRegW;
        }

        const arma::fvec& getMRegH() const {
            return m_regH;
        }

        void setMRegH(const arma::fvec&mRegH) {
            m_regH = mRegH;
        }

        float getMSparsity() const {
            return m_sparsity;
        }

        void setMSparsity(float mSparsity) {
            m_sparsity = mSparsity;
        }

        int getMPr() const {
            return m_pr;
        }

        void setMPr(int mPr) {
            m_pr = mPr;
        }

        int getMPc() const {
            return m_pc;
        }

        void setMPc(int mPc) {
            m_pc = mPc;
        }

        [[nodiscard]] virtual double getMSymmReg() const {
            return m_symm_reg;
        }

        virtual void setMSymmReg(double mSymmReg) {
            m_symm_reg = mSymmReg;
        }

        double getMTolerance() const {
            return m_tolerance;
        }

        void setMTolerance(double mTolerance) {
            m_tolerance = mTolerance;
        }

        int getMNumModes() const {
            return m_num_modes;
        }

        void setMNumModes(int mNumModes) {
            m_num_modes = mNumModes;
        }

        const arma::uvec& getMDimensions() const {
            return m_dimensions;
        }

        void setMDimensions(const arma::uvec&mDimensions) {
            m_dimensions = mDimensions;
        }

        const arma::uvec& getMProcGrids() const {
            return m_proc_grids;
        }

        void setMProcGrids(const arma::uvec&mProcGrids) {
            m_proc_grids = mProcGrids;
        }

        const arma::fvec& getMRegularizers() const {
            return m_regularizers;
        }

        void setMRegularizers(const arma::fvec&mRegularizers) {
            m_regularizers = mRegularizers;
        }

        int getMMaxLuciters() const {
            return m_max_luciters;
        }

        void setMMaxLuciters(int mMaxLuciters) {
            m_max_luciters = mMaxLuciters;
        }

        int getMNumNodes() const {
            return m_num_nodes;
        }

        void setMNumNodes(int mNumNodes) {
            m_num_nodes = mNumNodes;
        }

        double getAlpha() const {
            return alpha;
        }

        void setAlpha(double alpha) {
            params::alpha = alpha;
        }

        double getBeta() const {
            return beta;
        }

        void setBeta(double beta) {
            params::beta = beta;
        }

        int getFeatType() const {
            return feat_type;
        }

        void setFeatType(int featType) {
            feat_type = featType;
        }

        int getConnType() const {
            return conn_type;
        }

        void setConnType(int connType) {
            conn_type = connType;
        }

        double getMGamma() const {
            return m_gamma;
        }

        void setMGamma(double mGamma) {
            m_gamma = mGamma;
        }

        int getMUnpartitioned() const {
            return m_unpartitioned;
        }

        void setMUnpartitioned(int mUnpartitioned) {
            m_unpartitioned = mUnpartitioned;
        }

        const arma::uvec& getMConnGrids() const {
            return m_conn_grids;
        }

        void setMConnGrids(const arma::uvec&mConnGrids) {
            m_conn_grids = mConnGrids;
        }

        int getMCpr() const {
            return m_cpr;
        }

        void setMCpr(int mCpr) {
            m_cpr = mCpr;
        }

        int getMCpc() const {
            return m_cpc;
        }

        void setMCpc(int mCpc) {
            m_cpc = mCpc;
        }

        // distnmf related values
        int m_pr{};
        int m_pc{};
        double m_symm_reg{};
        double m_tolerance{};

        // dist ntf
        int m_num_modes = 1;
        arma::uvec m_dimensions;
        arma::uvec m_proc_grids;
        arma::fvec m_regularizers;

        // LUC params (optional)
        int m_max_luciters{};

        // hiernmf related values
        int m_num_nodes{};

        // jointnmf values
        double alpha{}, beta{};
        int feat_type{}, conn_type{};
        double m_gamma{};
        int m_unpartitioned{};

        // distjointnmf values
        // grid size for the connection matrices (cpr x cpc)
        arma::uvec m_conn_grids;
        int m_cpr{};
        int m_cpc{};
    };

    template<typename T>
    struct internalParams : virtual params {
        // matrix pointers for direct passage
        const T&m_a_mat;
        arma::mat m_h_init_mat;
        arma::mat m_w_init_mat;

        internalParams(const T&mAMat, const arma::mat&mWInitMat, const arma::mat&mHInitMat) : m_a_mat(mAMat),
            m_h_init_mat(mHInitMat),
            m_w_init_mat(mWInitMat) {
        }

        explicit internalParams(T&mAMat) : m_a_mat(mAMat) {
        }

        [[nodiscard]] const T& getMAMat() const {
            return m_a_mat;
        }

        void setMAMat(T&mAMat) {
            m_a_mat = mAMat;
        }

        [[nodiscard]] const arma::mat& getMHInitMat() const {
            return m_h_init_mat;
        }

        void setMHInitMat(const arma::mat&mHInitMat) {
            m_h_init_mat = mHInitMat;
        }

        [[nodiscard]] const arma::mat& getMWInitMat() const {
            return m_w_init_mat;
        }

        void setMWInitMat(const arma::mat&mWInitMat) {
            m_w_init_mat = mWInitMat;
        }


        //[[nodiscard]] const std::string& & getMAfileName() const override{};
        //[[nodiscard]] const std::string & getMOutputfileName() const override = delete;
        //[[nodiscard]] const std::string & getMHInitFileName() const override = delete;
        //[[nodiscard]] const std::string & getMWInitFileName() const override = delete;
    };

    struct symmParams : virtual params {
        void setMLucalgo(std::string mLucalgo) override {
            try {
                m_lucalgo = symmap.at(mLucalgo);
            }
            catch (const std::out_of_range&e) {
                std::throw_with_nested(std::runtime_error("Please choose `algo` from \"anlsbpp\", or \"gnsym\"."));
            }
        }
    };

    template<typename T>
    struct internalSymmParams final : symmParams, internalParams<T> {
        internalSymmParams(const T&mAMat, const arma::mat&mHInitMat)
            : symmParams(), internalParams<T>(mAMat, arma::mat{}, mHInitMat) {
        }

        using internalParams<T>::setMK;
        using internalParams<T>::setMNumIt;
        using symmParams::setMLucalgo;
        using internalParams<T>::setMSymmReg;
        using internalParams<T>::setMSymmFlag;
    };
}

#endif //PLANC_PLANCOPTS_H
