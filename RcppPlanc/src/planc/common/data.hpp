//
// Created by andrew on 12/3/2024.
//
#pragma once
#include <memory>
#include <nmflib_export.h>

namespace planc {
    class NMFLIB_EXPORT H5Mat {
        // A container only for a 2D dense matrix stored in an HDF5 file
        // with accessor function to columns of the matrix that reads and
        // returns a specified chunk of the matrix into memory
    public:
        virtual ~H5Mat() = default;

        typedef double elem_type;
        arma::uword n_cols, n_rows, colChunkSize, rowChunkSize;

    private:
        bool is_initialized = false;
        class H5MatImpl;
        std::shared_ptr<H5MatImpl> m_pimpl;

    public:
        //not thread safe
        H5Mat(const std::string&filename, const std::string&datapath);

        arma::mat cols(arma::uword start, arma::uword end) const;

        arma::mat cols(const arma::uvec&index) const;

        arma::mat rows(arma::uword start, arma::uword end) const;

        // not thread-safe
        H5Mat t() const;

        arma::mat operator*(const arma::mat&other) const {
            auto out = arma::mat(this->n_rows, other.n_cols);
            arma::uword numChunks = this->n_rows / this->rowChunkSize;
            if (numChunks * this->rowChunkSize < this->n_rows) numChunks++;
            for (arma::uword j = 0; j < numChunks; ++j) {
                arma::uword spanStart = j * this->rowChunkSize;
                arma::uword spanEnd = (j + 1) * this->rowChunkSize - 1;
                if (spanEnd > this->n_rows - 1) spanEnd = this->n_rows - 1;
                auto submat = this->rows(spanStart, spanEnd);
                auto res = submat * other;
                out.rows(spanStart, spanEnd) = res;
            }
            return out;
        }
    }; // End of class H5Mat
    class NMFLIB_EXPORT H5SpMat {
    public:
        virtual ~H5SpMat() = default;

        arma::uword x_chunksize, i_chunksize, p_chunksize;
        arma::uword n_rows, n_cols, nnz;
        typedef double elem_type;

    private:
        class H5SpMatImpl;
        std::shared_ptr<H5SpMatImpl> sm_pimpl;

    public:
        // not thread safe
        H5SpMat(const std::string&filename, const std::string&iPath, const std::string&pPath,
                const std::string&xPath, arma::uword n_rows, arma::uword n_cols);

        arma::sp_mat cols(arma::uword start, arma::uword end) const;

        arma::sp_mat cols(const arma::uvec&index) const;

        arma::vec getXByRange(arma::uword start, arma::uword end) const;

        // not thread safe
        H5SpMat t() const;

        arma::sp_mat t_mem() const;

        arma::mat operator*(const arma::mat&other) const {
            arma::sp_mat thisT = this->t_mem();
            arma::mat out(this->n_rows, other.n_cols);
            arma::uword numChunks = this->n_rows / this->x_chunksize;
            if (numChunks * this->x_chunksize < this->n_rows) numChunks++;
            std::cout << "numChunks: " << numChunks << std::endl;
            for (arma::uword j = 0; j < numChunks; ++j) {
                arma::uword spanStart = j * this->x_chunksize;
                std::cout << "spanstart: " << spanStart << std::endl;
                arma::uword spanEnd = (j + 1) * this->x_chunksize - 1;
                if (spanEnd > this->n_rows - 1) spanEnd = this->n_rows - 1;
                std::cout << "spanend: " << spanEnd << std::endl;
                out.rows(spanStart, spanEnd) = thisT.cols(spanStart, spanEnd).t() * other;
                // this is hacky and inefficient and should probably just be replaced with implementing rows() for SpMat
            }
            return out;
        }
    }; // End of class H5SpMat
} // End of namespace planc
namespace arma {
    // always frobenius
    template<typename>
    double norm(const planc::H5Mat&X, const char* method) {
        uword nChunks = X.n_cols / X.colChunkSize;
        if (nChunks * X.colChunkSize < X.n_cols) nChunks++;
        double lnorm = 0;
        for (uword i = 0; i < nChunks; ++i) {
            uword start = i * X.colChunkSize;
            uword end = (i + 1) * X.colChunkSize - 1;
            if (end > X.n_cols - 1) end = X.n_cols - 1;
            mat chunk = X.cols(start, end);
            lnorm += accu(chunk % chunk);
        }
        return std::sqrt(lnorm);
    }

    template<typename>
    double norm(const planc::H5SpMat&X, const char* method) {
        uword nChunks = X.nnz / X.x_chunksize;
        if (nChunks * X.x_chunksize < X.nnz) nChunks++;
        double norm = 0;
        for (uword i = 0; i < nChunks; ++i) {
            uword start = i * X.x_chunksize;
            uword end = (i + 1) * X.x_chunksize - 1;
            if (end > X.nnz - 1) end = X.nnz - 1;
            vec chunk = X.getXByRange(start, end);
            norm += accu(chunk % chunk);
        }
        return std::sqrt(norm);
    }
}
