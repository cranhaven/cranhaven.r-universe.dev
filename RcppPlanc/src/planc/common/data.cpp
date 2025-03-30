//
// Created by andrew on 12/3/2024.
//
#include "utils.hpp"
#include "data.hpp"
#include <filesystem>
#include <highfive/H5File.hpp>
#include <highfive/H5DataSet.hpp>
#include <highfive/H5DataSpace.hpp>

namespace planc {
    class H5Mat::H5MatImpl {
    public:
        [[nodiscard]] arma::uword n_cols1() const {
            return n_cols;
        }

        [[nodiscard]] arma::uword n_rows1() const {
            return n_rows;
        }

        [[nodiscard]] arma::uword col_chunk_size() const {
            return colChunkSize;
        }

        [[nodiscard]] arma::uword row_chunk_size() const {
            return rowChunkSize;
        }

    private:
        std::shared_ptr<HighFive::File> LinkedFile;
        std::string filename, datapath;
        arma::uword n_cols, n_rows, colChunkSize, rowChunkSize;
        std::vector<hsize_t> chunk_dims;

        static std::string increUniqName(const std::string&base) {
            int suffix = 0;
            std::string tempPath = base + std::to_string(suffix) + ".h5";
            while (std::filesystem::exists(tempPath)) {
                suffix++;
                tempPath = base + std::to_string(suffix) + ".h5";
            }
            return tempPath;
        }

    public:
        H5MatImpl(const std::string&filename, const std::string&datapath) {
            this->filename = filename;
            this->datapath = datapath;
            this->LinkedFile = std::make_shared<HighFive::File>(this->filename, HighFive::File::ReadOnly);
            const HighFive::DataSet H5D = this->LinkedFile->getDataSet(this->datapath);
            const HighFive::DataSpace dataspace = H5D.getSpace();
            // Get the rank (number of dimensions) of the H5D
            const size_t i = dataspace.getNumberDimensions();
            // Check if the rank is 2
            if (const unsigned int rank = i; rank != 2u) {
#ifdef USING_R
                Rcpp::Rcout
#else
                 std::cout
#endif
                        << "The H5D does not have a rank of 2." << std::endl;
            }
            const std::vector<size_t> dims = dataspace.getDimensions();
            // dataspace.close();
            this->n_cols = dims[0];
            this->n_rows = dims[1];

            HighFive::DataSetCreateProps cparms = H5D.getCreatePropertyList();
            this->chunk_dims = HighFive::Chunking(cparms).getDimensions();

            this->colChunkSize = chunk_dims[0];
            this->rowChunkSize = chunk_dims[1];
            // cparms.close();

            //#ifdef _VERBOSE
#ifdef USING_R
            Rcpp::Rcout
#else
                std::cout
#endif
                    << "==H5Mat constructed==" << std::endl
                    << "H5File:    " << this->filename << std::endl
                    << "Mat path:  " << this->datapath << std::endl
                    << "Dimension: " << this->n_rows << " x " << this->n_cols << std::endl;
            //#endif
        }

        [[nodiscard]] arma::mat cols(const arma::uword start, const arma::uword end) const {
            try {
                if (start < 0) {
                    throw std::invalid_argument(
                        "`start` must be an unsigned int, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
                if (start > end) {
                    throw std::invalid_argument(
                        "`start` must be less than or equal to `end`, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
                if (end >= this->n_cols) {
                    throw std::invalid_argument(
                        "`end` must be less than the number of columns, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
            }
            catch (std::exception&ex) {
#ifdef USING_R
                const std::string ex_str = ex.what();
                Rcpp::stop(ex_str);
#else
                throw ex;
#endif
            }
            arma::mat chunk(this->n_rows, end - start + 1);
            std::vector<size_t> offset;
            offset.push_back(start);
            offset.push_back(0);
            std::vector<size_t> count;
            count.push_back(end - start + 1);
            count.push_back(this->n_rows);
#pragma omp critical
            {
                const HighFive::DataSet H5D = this->LinkedFile->getDataSet(datapath);
                const HighFive::Selection selected = H5D.select(offset, count);
                selected.read_raw<double>(chunk.memptr());
            }
            // dataspace.close();
            // memspace.close();
            return chunk;
        }

        arma::mat cols(arma::uvec index) const {
            arma::mat out(this->n_rows, index.size());
            // Identify contiguous ranges from `index` and use .cols(start, end) for each range
            arma::uword start = index[0], end = index[0];
            arma::uword outStart = 0, outEnd = 0;
            for (arma::uword i = 1; i < index.size(); ++i) {
                const arma::uword curr = index[i];
                try {
                    if (curr > this->n_cols - 1) {
                        throw std::invalid_argument("Index " + std::to_string(curr) + " is out of range.");
                    }
                }
                catch (std::exception&ex) {
#ifdef USING_R
                    const std::string ex_str = ex.what();
                    Rcpp::stop(ex_str);
#else
                    throw ex;
#endif
                }
                if (curr == end + 1) {
                    // Still contiguous
                    end = curr;
                }
                else {
                    out.cols(outStart, outEnd) = this->cols(start, end);
                    outStart = outEnd + 1;
                    start = curr;
                    end = curr;
                }
                outEnd++;
            }
            out.cols(outStart, outEnd) = this->cols(start, end);
            return out;
        }

        arma::mat rows(const arma::uword start, const arma::uword end) const {
            try {
                if (start < 0) {
                    throw std::invalid_argument(
                        "`start` must be an unsigned int, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
                if (start > end) {
                    throw std::invalid_argument(
                        "`start` must be less than or equal to `end`, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
                if (end >= this->n_rows) {
                    throw std::invalid_argument(
                        "`end` must be less than the number of rows, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
            }
            catch (std::exception&ex) {
#ifdef USING_R
                const std::string ex_str = ex.what();
                Rcpp::stop(ex_str);
#else
                throw ex;
#endif
            }
            arma::mat chunk(end - start + 1, this->n_cols);
            std::vector<size_t> offset;
            offset.push_back(0);
            offset.push_back(start);
            std::vector<size_t> count;
            count.push_back(this->n_cols);
            count.push_back(end - start + 1);
#pragma omp critical
            {
                const HighFive::DataSet H5D = this->LinkedFile->getDataSet(datapath);
                const HighFive::Selection selected = H5D.select(offset, count);
                selected.read_raw<double>(chunk.memptr());
            }
            // dataspace.close();
            // memspace.close();
            return chunk;
        }

        // not thread-safe
        H5Mat t() const {
            // Create new H5 FILE with only the transposed dataset
            const std::string tmpfilename = increUniqName(this->filename + ".dense_transposed.");
            HighFive::File tmpfile(tmpfilename,
                                   HighFive::File::ReadWrite | HighFive::File::Create | HighFive::File::Truncate);
            // Set specified chunk dimension for the new dataset
            std::vector<hsize_t> chunk_dims_new;
            if (this->colChunkSize > this->n_rows) {
                // Mainly happening in small unit test case, but worth checking
                chunk_dims_new.push_back(this->n_rows);
            }
            else {
                chunk_dims_new.push_back(this->colChunkSize);
            }
            if (this->rowChunkSize > this->n_cols) {
                // Mainly happening in small unit test case, but worth checking
                chunk_dims_new.push_back(this->n_cols);
            }
            else {
                chunk_dims_new.push_back(this->rowChunkSize);
            }
            const HighFive::Chunking newChunks(chunk_dims_new);
            HighFive::DataSetCreateProps cparms_new;
            cparms_new.add(newChunks);
            std::array<size_t, 2> tDims{};
            tDims[0] = this->n_rows;
            tDims[1] = this->n_cols;
            const HighFive::DataSpace fspace(tDims);
#ifdef USING_R
            Rcpp::Rcout
#else
            std::cout
#endif
                    << "Creating transposed data at "
                    << tmpfilename << ":data" << std::endl;
            const HighFive::DataSet H5DT = tmpfile.createDataSet<double>("data", fspace, cparms_new);
            // HighFive::DataSet H5DT = this->createDataSet<double>(tempPath, fspace, cparms_new);
            //cparms_new.close();
            // H5::DataSpace dataspace = H5DT.getSpace();
            unsigned int nChunks = this->n_rows / this->colChunkSize;
            if (nChunks * this->colChunkSize < this->n_rows) nChunks++;
            for (unsigned int i = 0; i < nChunks; ++i) {
                arma::uword start = i * this->colChunkSize;
                arma::uword end = (i + 1) * this->colChunkSize - 1;
                if (end > this->n_rows - 1) end = this->n_rows - 1;
                // Read row slices of the current H5D dataset and write to column chunks of the new dataset
                arma::mat chunk = this->rows(start, end).t();
                std::vector<size_t> offset;
                offset.push_back(start);
                offset.push_back(0);
                std::vector<size_t> count;
                count.push_back(end - start + 1);
                count.push_back(this->n_cols);
                // H5DT.select(offset, count).write<double>(*chunk.memptr());
                HighFive::Selection selected = H5DT.select(offset, count);
                selected.write_raw<double>(chunk.memptr());
                // memspace.close();
            }
            // fspace.close();
            // H5DT.close();
            tmpfile.flush();
            return {tmpfilename, "data"};
        } // End of H5Mat.t()
    }; // End of class H5MatImpl
    H5Mat::H5Mat(const std::string&filename, const std::string&datapath) {
        if (!(filename == "" || datapath == "")) { this->is_initialized = true; }
        if (this->is_initialized) {
            m_pimpl = std::make_shared<H5MatImpl>(filename, datapath);
            this->n_cols = m_pimpl->n_cols1();
            this->n_rows = m_pimpl->n_rows1();
            this->colChunkSize = m_pimpl->col_chunk_size();
            this->rowChunkSize = m_pimpl->row_chunk_size();
        }
    }

    arma::mat H5Mat::cols(const arma::uword start, const arma::uword end) const { return m_pimpl->cols(start, end); }

    arma::mat H5Mat::cols(const arma::uvec&index) const { return m_pimpl->cols(index); }

    arma::mat H5Mat::rows(const arma::uword start, const arma::uword end) const { return m_pimpl->rows(start, end); }

    H5Mat H5Mat::t() const { return m_pimpl->t(); }

    class H5SpMat::H5SpMatImpl : public HighFive::File {
        std::string filename, xPath, iPath, pPath;

    public:
        [[nodiscard]] arma::uword nnz1() const {
            return nnz;
        }

        [[nodiscard]] arma::uword x_chunksize1() const {
            return x_chunksize;
        }

        [[nodiscard]] arma::uword i_chunksize1() const {
            return i_chunksize;
        }

        [[nodiscard]] arma::uword p_chunksize1() const {
            return p_chunksize;
        }

    private:
        // The `start` and `end` refer to the start and end of the corresponding
        // arrays, but not the indices of the sparse matrix
        arma::uvec getPByRange(const arma::uword start, const arma::uword end) const {
            arma::uvec p(end - start + 1);
            std::vector<size_t> p_start;
            p_start.push_back(start);
            std::vector<size_t> p_count;
            p_count.push_back(end - start + 1);
#pragma omp critical
            {
                const HighFive::DataSet H5D_P = this->getDataSet(pPath);
                const HighFive::Selection selected_p = H5D_P.select(p_start, p_count);
                selected_p.read_raw<arma::uword>(p.memptr());
            }
            // pDataspace.close();
            // pMemspace.close();
            return p;
        }

        arma::uvec getIByRange(arma::uword const start, arma::uword const end) const {
            arma::uvec i(end - start + 1);
            std::vector<size_t> i_start;
            i_start.push_back(start);
            std::vector<size_t> i_count;
            i_count.push_back(end - start + 1);
#pragma omp critical
            {
                const HighFive::DataSet H5D_I = this->getDataSet(iPath);
                const HighFive::Selection selected_i = H5D_I.select(i_start, i_count);
                selected_i.read_raw<arma::uword>(i.memptr());
            }
            // iDataspace.close();
            // iMemspace.close();
            return i;
        }

    public:
        arma::vec getXByRange(arma::uword const start, arma::uword const end) const {
            arma::vec x(end - start + 1);
            std::vector<size_t> x_start;
            x_start.push_back(start);
            std::vector<size_t> x_count;
            x_count.push_back(end - start + 1);
#pragma omp critical
            {
                const HighFive::DataSet H5D_X = this->getDataSet(xPath);
                const HighFive::Selection selected_x = H5D_X.select(x_start, x_count);
                selected_x.read_raw<double>(x.memptr());
            }
            // xDataspace.close();
            // xMemspace.close();
            return x;
        }

    private:
        static std::string increUniqName(const std::string&base) {
            int suffix = 0;
            std::string tempPath = base + std::to_string(suffix) + ".h5";
            while (std::filesystem::exists(tempPath)) {
                suffix++;
                tempPath = base + std::to_string(suffix) + ".h5";
            }
            return tempPath;
        }

    public:
        arma::uword n_rows, n_cols, nnz;
        arma::uword x_chunksize, i_chunksize, p_chunksize;
        // not thread safe
        H5SpMatImpl(const std::string&filename, const std::string&iPath, const std::string&pPath,
                    const std::string&xPath, const arma::uword n_rows, const arma::uword n_cols) : File(
            filename, ReadWrite) {
            this->filename = filename;
            this->iPath = iPath;
            this->pPath = pPath;
            this->xPath = xPath;
            this->n_rows = n_rows;
            this->n_cols = n_cols;
            const HighFive::DataSet H5D_X = this->getDataSet(xPath);
            HighFive::DataSetCreateProps x_cparms = H5D_X.getCreatePropertyList();
            const std::vector<hsize_t> x_chunkdim = HighFive::Chunking(x_cparms).getDimensions();
            this->x_chunksize = x_chunkdim[0];
            // x_cparms.close();

            const HighFive::DataSpace xDataspace = H5D_X.getSpace();
            const std::vector<size_t> xDims = xDataspace.getDimensions();
            // xDataspace.close();
            this->nnz = xDims[0];

            const HighFive::DataSet H5D_I = this->getDataSet(iPath);
            HighFive::DataSetCreateProps i_cparms = H5D_I.getCreatePropertyList();
            const std::vector<hsize_t> i_chunkdim = HighFive::Chunking(i_cparms).getDimensions();
            this->i_chunksize = i_chunkdim[0];
            // i_cparms.close();
            const HighFive::DataSet H5D_P = this->getDataSet(pPath);
            HighFive::DataSetCreateProps p_cparms = H5D_P.getCreatePropertyList();
            const std::vector<hsize_t> p_chunkdim = HighFive::Chunking(p_cparms).getDimensions();
            this->p_chunksize = p_chunkdim[0];
            // p_cparms.close();
            // #ifdef _VERBOSE
// #ifdef USING_R
//             Rcpp::Rcout
// #else
//             std::cout
// #endif
//                     << "==H5SpMat constructed==" << std::endl
//                     << "H5File:    " << filename << std::endl
//                     << "colptr path:  " << pPath << std::endl
//                     << "rowind path:  " << iPath << std::endl
//                     << "value path:   " << xPath << std::endl
//                     << "Dimension: " << n_rows << " x " << n_cols << std::endl;
//             // #endif
        }

        ~H5SpMatImpl() {
            this->flush();
            //     // this->H5D.close();
            //     // this->H5F.unlink(this->tempPath);
            //     // TODO: Have to find a way to unlink the temp transposed matrix
            //     // this->H5F.close();
        }

        arma::sp_mat cols(arma::uword const start, arma::uword const end) const {
            try {
                if (start < 0) {
                    throw std::invalid_argument(
                        "`start` must be an unsigned int, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
                if (start > end) {
                    throw std::invalid_argument(
                        "`start` must be less than or equal to `end`, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
                if (end >= this->n_cols) {
                    throw std::invalid_argument(
                        "`end` must be less than the number of columns, got (" + std::to_string(start) + ", " +
                        std::to_string(end) + ").");
                }
            }
            catch (std::exception&ex) {
#ifdef USING_R
                const std::string ex_str = ex.what();
                Rcpp::stop(ex_str);
#else
                throw ex;
#endif
            }

            // Construct subsetted colptr
            arma::uvec colptr = this->getPByRange(start, end + 1);

            // Subset rowind and value according to colptr
            const arma::uvec rowind = this->getIByRange(colptr[0], colptr[end - start + 1] - 1);
            const arma::vec value = this->getXByRange(colptr[0], colptr[end - start + 1] - 1);

            colptr -= colptr[0];
            // Construct the subset sparse matrix
            arma::sp_mat chunk(rowind, colptr, value, this->n_rows, end - start + 1);

            return chunk;
        }

        arma::sp_mat cols(arma::uvec index) const {
            arma::sp_mat out(this->n_rows, index.size());
            // Identify contiguous ranges from `index` and use .cols(start, end) for each range
            arma::uword start = index[0], end = index[0];
            arma::uword outStart = 0, outEnd = 0;
            for (arma::uword i = 1; i < index.size(); ++i) {
                const arma::uword curr = index[i];
                try {
                    if (curr > this->n_cols - 1) {
                        throw std::invalid_argument("Index " + std::to_string(curr) + " is out of range.");
                    }
                }
                catch (std::exception&ex) {
#ifdef USING_R
                    const std::string ex_str = ex.what();
                    Rcpp::stop(ex_str);
#else
                    throw ex;
#endif
                }
                if (curr == end + 1) {
                    // Still contiguous
                    end = curr;
                }
                else {
                    out.cols(outStart, outEnd) = this->cols(start, end);
                    outStart = outEnd + 1;
                    start = curr;
                    end = curr;
                }
                outEnd++;
            }
            out.cols(outStart, outEnd) = this->cols(start, end);
            return out;
        }

        // not thread safe
        H5SpMat t() const {
#ifdef USING_R
            Rcpp::Rcout
#else
            std::cout
#endif
                    << "Creating on-disk transposition of the sparse matrix, which is currently poorly supported and slow"
                    << std::endl;
            // Create new H5 FILE with only the transposed dataset
            std::string tmpfilename = this->increUniqName(this->filename + ".sparse_transposed.");
            File tmpfile(tmpfilename, ReadWrite | Create | Truncate);

            // ================= Create the colptr of the transposed matrix =================
            // Read all colptr into memory, it's not big :)
            arma::uvec colptr = this->getPByRange(0, this->n_cols);
            // Go through chunks of rowind and initialize the transposed colptr
            arma::uvec colptrT(this->n_rows + 1);
            arma::uword nChunk = this->nnz / this->i_chunksize;
            if (nChunk * this->i_chunksize < this->nnz) nChunk++;
            for (arma::uword i = 0; i < nChunk; i++) {
                arma::uword start = i * this->i_chunksize;
                arma::uword end = (i + 1) * this->i_chunksize - 1;
                if (end > this->nnz - 1) end = this->nnz - 1;
                arma::uvec rowind_chunk = this->getIByRange(start, end);
                for (unsigned int j: rowind_chunk) {
                    // rowind_chunk[j] gives the number referring to which row of the original
                    // or which column of the transposed matrix have a non-zero value
                    colptrT[j + 1]++;
                }
            }
            // cumsum to get the final colptrT
            colptrT = cumsum(colptrT);

            // Write colptrT to HDF5 file
            // std::string ptempPath = this->increUniqName(this->pPath + "_transposed_");
            std::vector<hsize_t> inputp_chunksize;
            if (this->p_chunksize > this->n_rows + 1) {
                // Mainly happening in small unit test case, but worth checking
                inputp_chunksize.push_back(this->n_rows + 1);
            }
            else {
                inputp_chunksize.push_back(this->p_chunksize);
            }
            HighFive::Chunking p_newChunks(inputp_chunksize);
            HighFive::DataSetCreateProps p_cparms_new;
            p_cparms_new.add(p_newChunks);

            std::vector<size_t> p_new_size;
            p_new_size.push_back(this->n_rows + 1);
            HighFive::DataSpace p_new_dataspace(p_new_size);
            HighFive::DataSet H5D_PT = tmpfile.createDataSet<arma::uword>("colptr", p_new_dataspace, p_cparms_new);

            std::vector<size_t> offset;
            offset.push_back(0);
            HighFive::Selection p_selected = H5D_PT.select(offset, p_new_size);
            p_selected.write_raw<arma::uword>(colptrT.memptr());
            // p_new_dataspace.close();
            // p_new_memspace.close();
            // H5D_PT.close();
            // ================= self.t.colptr done ! =================

            // ================= Create the rowind and value of the transposed matrix =================
            // Pre-create the rowind and value of the transposed matrix
            // Create rowind.T
            // std::string itempPath = this->increUniqName(this->iPath + "_transposed_");
            std::vector<hsize_t> inputi_chunksize;
            inputi_chunksize.push_back(this->i_chunksize);
            HighFive::Chunking i_newChunks(inputi_chunksize);
            HighFive::DataSetCreateProps i_cparms_new;
            i_cparms_new.add(i_newChunks);

            std::vector<size_t> i_new_size;
            i_new_size.push_back(this->nnz);
            HighFive::DataSpace i_new_dataspace(i_new_size);
            // HighFive::DataSet H5D_IT = this->createDataSet<arma::uword>(itempPath,i_new_dataspace, i_cparms_new);
            HighFive::DataSet H5D_IT = tmpfile.createDataSet<arma::uword>("rowind", i_new_dataspace, i_cparms_new);
            // i_cparms_new.close();
            // Create value.T
            // std::string xtempPath = this->increUniqName(this->xPath + "_transposed_");
            std::vector<hsize_t> inputx_chunksize;
            inputx_chunksize.push_back(this->x_chunksize);
            HighFive::Chunking x_newChunks(inputx_chunksize);
            HighFive::DataSetCreateProps x_cparms_new;
            x_cparms_new.add(x_newChunks);

            std::vector<size_t> x_new_size;
            x_new_size.push_back(this->nnz);
            HighFive::DataSpace x_new_dataspace(x_new_size);
            // HighFive::DataSet H5D_XT = this->createDataSet<double>(xtempPath, x_new_dataspace, x_cparms_new);
            HighFive::DataSet H5D_XT = tmpfile.createDataSet<double>("value", x_new_dataspace, x_cparms_new);
            // x_cparms_new.close();

            arma::uvec colptrT_start = colptrT;
            Progress p(this->n_cols, true);
            for (arma::uword i = 0; i < this->n_cols; i++) {
                // Go through each column of the original matrix and fill in the
                // rowind and value of the transposed matrix
                // For each of the original column, get the corresponding fragment from original rowind and value
                arma::uvec rowind_ori_col = this->getIByRange(colptr[i], colptr[i + 1] - 1);
                arma::vec value_ori_col = this->getXByRange(colptr[i], colptr[i + 1] - 1);
                // With the frangment of rowind we get, we know which rows of the
                // original matrix or cols of the transposed matrix have non-zero values
                // Then it is mapped to the positions in colptrT. The values fetched here
                // then map to the positions in rowindT and valueT. The current col index, `i`,
                // will be the value to be filled in rowindT, and the fragment from `value` can
                // be simply copied to valueT at the same positions.
                arma::uvec nnz_idx = colptrT_start.elem(rowind_ori_col);
                arma::uword nnz_idx_size = nnz_idx.size();
                arma::uvec it_value(nnz_idx_size);
                it_value.fill(i);

                std::vector<size_t> coord;
                for (arma::uword j = 0; j < nnz_idx_size; j++) {
                    coord.push_back(nnz_idx[j]);
                }
                // size_t count = nnz_idx_size;
                // HighFive::DataSpace xt_writeDataSpace(1, count);
                // HighFive::DataSpace it_writeDataSpace(1, count);

                HighFive::Selection i_selected = H5D_IT.select(coord);
                HighFive::Selection x_selected = H5D_XT.select(coord);

                i_selected.write_raw<arma::uword>(it_value.memptr());
                x_selected.write_raw<double>(value_ori_col.memptr());

                // Increment it, so that next time when fetching the number `nnz_idx` of the same new-column/old-row,
                // it know the previous position has been filled
                colptrT_start.elem(rowind_ori_col) += 1;

                // delete[] coord;
                p.increment();
            }
            // i_new_dataspace.close();
            // x_new_dataspace.close();
            // H5D_IT.close();
            // H5D_XT.close();
            // ================= self.t.rowind and self.t.value done ! =================
            tmpfile.flush();
            return {tmpfilename, "rowind", "colptr", "value", this->n_cols, this->n_rows};
            // H5SpMat transposedMat(this->filename, itempPath, ptempPath, xtempPath, this->n_cols, this->n_rows);
        } // End of H5SpMat.t()
        // End of class H5SpMat
        arma::sp_mat t_mem() const {
#ifdef USING_R
            Rcpp::Rcout
#else
            std::cout
#endif
                    << "Creating in memory of the sparse matrix, which is currently poorly supported and inefficent" <<
                    std::endl;
            // Create new arma spmat
            // ================= Create the colptr of the transposed matrix =================
            // Read all colptr into memory, it's not big :)
            arma::uvec colptr = this->getPByRange(0, this->n_cols);
            // Go through chunks of rowind and initialize the transposed colptr
            arma::uvec colptrT(this->n_rows + 1);
            arma::uword nChunk = this->nnz / this->i_chunksize;
            if (nChunk * this->i_chunksize < this->nnz) nChunk++;
            for (arma::uword i = 0; i < nChunk; i++) {
                arma::uword start = i * this->i_chunksize;
                arma::uword end = (i + 1) * this->i_chunksize - 1;
                if (end > this->nnz - 1) end = this->nnz - 1;
                arma::uvec rowind_chunk = this->getIByRange(start, end);
                for (unsigned int j: rowind_chunk) {
                    // rowind_chunk[j] gives the number referring to which row of the original
                    // or which column of the transposed matrix have a non-zero value
                    colptrT[j + 1]++;
                }
            }
            // cumsum to get the final colptrT
            colptrT = cumsum(colptrT);

            // ================= Create the rowind and value of the transposed matrix =================
            // Pre-create the rowind and value of the transposed matrix

            size_t i_new_size;
            i_new_size = this->nnz;
            arma::uvec rowind(i_new_size);
            // Create value.T
            size_t x_new_size;
            x_new_size = this->nnz;
            arma::vec value(x_new_size);

            arma::uvec colptrT_start = colptrT;
            Progress p(this->n_cols, true);
            for (arma::uword i = 0; i < this->n_cols; i++) {
                // Go through each column of the original matrix and fill in the
                // rowind and value of the transposed matrix
                // For each of the original column, get the corresponding fragment from original rowind and value
                arma::uvec rowind_ori_col = this->getIByRange(colptr[i], colptr[i + 1] - 1);
                arma::vec value_ori_col = this->getXByRange(colptr[i], colptr[i + 1] - 1);
                // With the frangment of rowind we get, we know which rows of the
                // original matrix or cols of the transposed matrix have non-zero values
                // Then it is mapped to the positions in colptrT. The values fetched here
                // then map to the positions in rowindT and valueT. The current col index, `i`,
                // will be the value to be filled in rowindT, and the fragment from `value` can
                // be simply copied to valueT at the same positions.
                arma::uvec nnz_idx = colptrT_start.elem(rowind_ori_col);
                arma::uword nnz_idx_size = nnz_idx.size();
                arma::uvec it_value(nnz_idx_size);
                it_value.fill(i);

                std::vector<size_t> coord;
                for (arma::uword j = 0; j < nnz_idx_size; j++) {
                    coord.push_back(nnz_idx[j]);
                }
                // size_t count = nnz_idx_size;
                // HighFive::DataSpace xt_writeDataSpace(1, count);
                // HighFive::DataSpace it_writeDataSpace(1, count);

                rowind.elem(arma::conv_to<arma::uvec>::from(coord)) = it_value;
                value.elem(arma::conv_to<arma::uvec>::from(coord)) = value_ori_col;
                // Increment it, so that next time when fetching the number `nnz_idx` of the same new-column/old-row,
                // it know the previous position has been filled
                colptrT_start.elem(rowind_ori_col) += 1;

                // delete[] coord;
                p.increment();
            }

            // ================= self.t.rowind and self.t.value done ! =================
            return {rowind, colptrT, value, this->n_cols, this->n_rows};
            // H5SpMat transposedMat(this->filename, itempPath, ptempPath, xtempPath, this->n_cols, this->n_rows);
        } // End of H5SpMat.t()
        // End of class H5SpMat
    };


    H5SpMat::H5SpMat(const std::string&filename, const std::string&iPath, const std::string&pPath,
                     const std::string&xPath, arma::uword n_rows, arma::uword n_cols) : n_rows(n_rows), n_cols(n_cols) {
        sm_pimpl = std::make_shared<H5SpMatImpl>(filename, iPath, pPath, xPath, n_rows, n_cols);
        this->nnz = sm_pimpl->nnz1();
        this->x_chunksize = sm_pimpl->x_chunksize1();
        this->i_chunksize = sm_pimpl->i_chunksize1();
        this->p_chunksize = sm_pimpl->p_chunksize1();
    }

    arma::sp_mat H5SpMat::cols(const arma::uword start, const arma::uword end) const {
        return sm_pimpl->cols(start, end);
    }

    arma::sp_mat H5SpMat::cols(const arma::uvec&index) const { return sm_pimpl->cols(index); }
    // not thread safe
    H5SpMat H5SpMat::t() const { return sm_pimpl->t(); }
    arma::sp_mat H5SpMat::t_mem() const { return sm_pimpl->t_mem(); }

    arma::vec H5SpMat::getXByRange(const arma::uword start, const arma::uword end) const {
        return sm_pimpl->getXByRange(start, end);
    }


    // arma::sp_mat cols(arma::uvec index) {
    //     arma::sp_mat out(this->n_rows, index.size());
    //     arma::uvec colptr = this->getPByRange(0, this->n_cols);
    //     arma::uword idx;
    //     for (arma::uword i = 0; i < index.size(); ++i) {
    //         idx = index[i];
    //         if (idx >= this->n_cols) {
    //             throw std::invalid_argument("Index " + std::to_string(idx) + " is out of range.");
    //         }
    //         arma::uvec rowind = this->getIByRange(colptr[idx], colptr[idx + 1] - 1);
    //         arma::vec value = this->getXByRange(colptr[idx], colptr[idx + 1] - 1);
    //         arma::uvec colptr_i = arma::zeros<arma::uvec>(2);
    //         colptr_i[1] = rowind.size();
    //         out.col(i) = arma::sp_mat(rowind, colptr_i, value, this->n_rows, 1);
    //     }
    //     return out;
    // }
} // End of namespace planc
