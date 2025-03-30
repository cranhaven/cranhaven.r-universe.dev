#pragma once
/* Copyright Ramakrishnan Kannan 2017 */

#include <argparse/argparse.hpp>
#include <armadillo>
#include <iostream>
#include <sstream>
#include <string>
#include "plancopts.h"
#include "parsecommandline.h"
#include "utils.h"

namespace planc {
class ParseCommandLine : argparse::ArgumentParser {
 protected:
    params* clStruct = new params;
    bool parsed = false;

    void parseDimArray() {
        // debug_hook();
        if (clStruct->m_num_modes == 0) {
            for (int i = 0; i < this->clStruct->m_dimensions.size(); ++i) {
                clStruct->m_num_modes = i;
                clStruct->m_conn_grids = arma::zeros<arma::uvec>(clStruct->m_num_modes);
                clStruct->m_dimensions = arma::zeros<arma::uvec>(clStruct->m_num_modes);
                clStruct->m_regularizers = arma::zeros<arma::fvec>(2 * clStruct->m_num_modes);
                clStruct->m_proc_grids = arma::ones<arma::uvec>(clStruct->m_num_modes);
            }
        }
    }
 public:
  /**
   * Constructor that takes the number of arguments and the
   * command line parameters.
   * @param[in] argc - number of arguments
   * @param[in] **argv - command line parameters.
   */
  ParseCommandLine() : argparse::ArgumentParser("planc") {
      this->add_argument("-h", "--help")
            .flag();
      // call print_usage
      this->add_argument("-a", "--algorithm")
            .default_value("ANLSBPP")
            .choices("MU", "HALS", "ANLSBPP", "NAIVEANLSBPP", "AOADMM",
                     "NESTEROV", "CPALS", "GNSYM", "R2", "PGD", "PGNCG")
            .help("algorithm");
      this->add_argument("--input_normalization")
              .default_value("NONE")
              .choices("NONE","L2NORM","MAXNORM")
              .help("normalization");
      this->add_argument("-d", "--dims")
              .nargs(argparse::nargs_pattern::at_least_one)
              .default_value("2 0")
              .scan<'i', int>()
              .help("dimensions");
      this->add_argument("--mat_type")
              .nargs(2)
              .default_value("2 0")
              .scan<'i', int>()
              .help("mat type");
      this->add_argument("-e", "--error")
            .implicit_value(true)
            .default_value(false)
            .help("compute error");
      this->add_argument("--unpartitioned")
              .implicit_value(true)
              .default_value(false)
              .help("unpartitioned");
      this->add_argument("--symm")
              .default_value(-1.)
              .scan<'g', double>()
              .help("symmetric reg param");
      this->add_argument("--alpha")
              .default_value(0.)
              .scan<'g', double>()
              .help("alpha hyperparam");
      this->add_argument("--beta")
              .default_value(0.)
              .scan<'g', double>()
              .help("beta hyperparam");
      this->add_argument("--momentum")
              .default_value(0.)
              .scan<'g', double>()
              .help("momentum hyperparam");
      this->add_argument("--adjrand")
              .implicit_value(true)
              .default_value(false)
              .help("adjrand");
      this->add_argument("--dimtree")
              .default_value(true)
              .help("dimtree");
      this->add_argument("-l", "--tolerance")
            .default_value(-1.)
            .scan<'g', double>()
            .help("tolerance");
      this->add_argument("--luciters")
              .default_value(-1)
              .scan<'i', int>()
              .help("luciters");
      this->add_argument("-t", "--iterations")
              .default_value(20)
              .scan<'i', int>()
              .help("iterations");
      this->add_argument("-k", "--lowrankk")
              .default_value(20)
              .scan<'i', int>()
              .help("iterations");
      this->add_argument("-s", "--sparsity")
              .default_value(0.01f)
              .scan<'g', float>()
              .help("sparsity");
      this->add_argument("-n", "--num_nodes")
              .default_value(1)
              .scan<'i', int>()
              .help("nodes");
      this->add_argument("-i", "--input")
              .default_value("")
              .help("input 1");
      this->add_argument("-c", "--connections")
              .default_value("")
              .help("connections");
      this->add_argument("-o", "--output")
              .default_value("")
              .help("output");
      //this->add_argument("-j", "--input2")
      //        .default_value("")
      //        .help("input 2");
      //clStruct->m_Bfile_name = this->get("-j");
      this->add_argument("--seed")
              .default_value(193957)
              .scan<'i', int>()
              .help("seed");
      this->add_argument("-r", "--regs")
              .nargs(argparse::nargs_pattern::at_least_one)
              .default_value("4 0")
              .scan<'g', float>()
              .help("regularizers");
      this->add_argument("-p", "--processors")
              .nargs(argparse::nargs_pattern::at_least_one)
              .default_value("2 0")
              .scan<'i', int>()
              .help("processor grid");
      this->add_argument("-q", "--connection_grid")
              .nargs(argparse::nargs_pattern::at_least_one)
              .default_value("2 0")
              .scan<'i', int>()
              .help("processor grid");
      this->add_argument("--numkblocks")
              .default_value(1)
              .scan<'i', int>()
              .help("numkblocks");
  }
  void initClStruct() {
      clStruct->m_lucalgo = algomap[this->get("-a")];
      clStruct->m_input_normalization = normmap[this->get("--input_normalization")];
      clStruct->feat_type = this->get("--mat_type")[0];
      clStruct->conn_type = this->get("--mat_type")[1];
      clStruct->m_compute_error = this->get<bool>("-e");
      clStruct->m_unpartitioned = this->get<bool>("--unpartitioned");
      clStruct->m_symm_reg = this->get<double>("--symm");
      clStruct->alpha = this->get<double>("--alpha");
      clStruct->beta = this->get<double>("--beta");
      clStruct->m_gamma = this->get<double>("--momentum");
      clStruct->m_adj_rand = this->get<bool>("--adjrand");
      clStruct->m_dim_tree = this->get<bool>("--dimtree");
      clStruct->m_tolerance = this->get<double>("-l");
      clStruct->m_max_luciters = this->get<int>("--luciters");
      clStruct->m_num_it = this->get<int>("-t");
      clStruct->m_k = this->get<int>("-k");
      clStruct->m_sparsity = this->get<float>("-s");
      clStruct->m_num_nodes = this->get<int>("-n");
      clStruct->m_Afile_name = this->get("-i");
      clStruct->m_Sfile_name = this->get("-c");
      clStruct->m_outputfile_name = this->get("-o");
      clStruct->m_initseed = this->get<int>("--seed");
      clStruct->m_regularizers = this->get("-r");
      clStruct->m_proc_grids = this->get("-p");
      clStruct->m_conn_grids = this->get("-q");
      clStruct->m_num_k_blocks = this->get<int>("--numkblocks");
      //clStruct->m_pr = 1;
      //clStruct->m_pc = 1;
      //clStruct->m_cpr = 0; // default is single grid
      //clStruct->m_cpc = 0; // default is single grid
      //clStruct->m_regW = arma::zeros<arma::fvec>(2);
      //clStruct->m_regH = arma::zeros<arma::fvec>(2);
      if (clStruct->m_dimensions.size() == 2) {
          clStruct->m_globalm = clStruct->m_dimensions(0);
          clStruct->m_globaln = clStruct->m_dimensions(1);
          clStruct->m_regW(0) = clStruct->m_regularizers(0);
          clStruct->m_regW(1) = clStruct->m_regularizers(1);
          clStruct->m_regH(0) = clStruct->m_regularizers(2);
          clStruct->m_regH(1) = clStruct->m_regularizers(3);
          clStruct->m_pr      = clStruct->m_proc_grids(0);
          clStruct->m_pc      = clStruct->m_proc_grids(1);
          clStruct->m_cpr     = clStruct->m_conn_grids(0);
          clStruct->m_cpc     = clStruct->m_conn_grids(1);
      }
  }

  /// print the configuration received through the command line paramters

  void printConfig(int opt = 0) {
    switch (opt) {
      case JOINTNMF:
        INFO << "a::" << clStruct->m_lucalgo << "::i::" << clStruct->m_Afile_name
             << "::c::" << clStruct->m_Sfile_name
             << "::o::" << clStruct->m_outputfile_name
             << "::m::" << clStruct->m_globalm << "::n::" << clStruct->m_globaln
             << "::t::" << clStruct->m_num_it
             << "::error::" << clStruct->m_compute_error
             << "::tol::" << clStruct->m_tolerance
             << "::regW::" << clStruct->m_regW << "::regH::" << clStruct->m_regH
             << "::sparsity::" << clStruct->m_sparsity
             << "::input normalization::" << clStruct->m_input_normalization
             << "::num_k_blocks::" << clStruct->m_num_k_blocks
             << "::adj_rand::" << clStruct->m_adj_rand
             << "::initseed::" << clStruct->m_initseed
             << "::alpha::" << clStruct->alpha << "::beta::" << clStruct->beta
             << std::endl;
        break;
      case DISTJOINTNMF:
        INFO << "a::" << clStruct->m_lucalgo << "::i::" << clStruct->m_Afile_name
             << "::c::" << clStruct->m_Sfile_name
             << "::o::" << clStruct->m_outputfile_name
             << "::m::" << clStruct->m_globalm << "::n::" << clStruct->m_globaln
             << "::t::" << clStruct->m_num_it
             << "::error::" << clStruct->m_compute_error
             << "::tol::" << clStruct->m_tolerance
             << "::regW::" << clStruct->m_regW << "::regH::" << clStruct->m_regH
             << "::sparsity::" << clStruct->m_sparsity
             << "::input normalization::" << clStruct->m_input_normalization
             << "::num_k_blocks::" << clStruct->m_num_k_blocks
             << "::adj_rand::" << clStruct->m_adj_rand
             << "::initseed::" << clStruct->m_initseed
             << "::alpha::" << clStruct->alpha << "::beta::" << clStruct->beta
             << "::pr::" << clStruct->m_pr << "::pc::" << clStruct->m_pc
             << "::cpr::" << clStruct->m_cpr << "::cpc::" << clStruct->m_cpc
             << std::endl;
        break;
      default:
        INFO << "a::" << clStruct->m_lucalgo << "::i::" << clStruct->m_Afile_name
                  << "::k::" << clStruct->m_k << "::m::" << clStruct->m_globalm
                  << "::n::" << clStruct->m_globaln << "::t::" << clStruct->m_num_it
                  << "::pr::" << clStruct->m_pr << "::pc::" << clStruct->m_pc
                  << "::error::" << clStruct->m_compute_error  << "::tol::" << clStruct->m_tolerance
                  << "::regW::"
                  << "l2::" << clStruct->m_regW(0) << "::l1::" << clStruct->m_regW(1)
                  << "::regH::"
                  << "l2::" << clStruct->m_regH(0) << "::l1::" << clStruct->m_regH(1)
                  << "::num_k_blocks::" << clStruct->m_num_k_blocks
                  << "::dimensions::" << clStruct->m_dimensions
                  << "::procs::" << clStruct->m_proc_grids
                  << "::regularizers::" << clStruct->m_regularizers
                  << "::input normalization::" << clStruct->m_input_normalization
                  << "::symm_reg::" << clStruct->m_symm_reg
                  << "::luciters::" << clStruct->m_max_luciters
                  << "::adj_rand::" << clStruct->m_adj_rand
                  << "::initseed::" << clStruct->m_initseed
                  << "::dimtree::" << clStruct->m_dim_tree << std::endl;
    }
  }

  void print_usage(int opt = 0) {
    switch (opt) {
      case JOINTNMF:
        INFO << std::endl;
        INFO << "jointnmf usage:" << std::endl;
        INFO << "for short arguments like -i do not use equals sign, eg -t 10"
            << std::endl
            << "for long arguments like --seed give key=value pair or key value"
            << ",eg --seed=17 or --seed 17"
            << std::endl
            << "algorithm codes 2-ANLSBPP, 9-PGD, 10-PGNCG"
            << std::endl
            << "for array arguments like -r use quotes"
            << ", eg -r \"0.5 0.3 0.0 0.1\""
            << std::endl
            << "specify features matrix via -i and connections matrix via -c"
            << std::endl
            << R"(specify sparsity via --mat_type "0 0" denotes sparse/sparse, "1 1" dense/dense, etc)"
            << std::endl
            << "hyperparameter options are --alpha, --beta, --momentum"
            << std::endl << std::endl;

        INFO << "Sample usages:" << std::endl;
        // Give an example for every algo type
        // PGD with default matrix types
        INFO << "Usage 1: ./jointnmf -a 9 -k 15 -i rand_lowrank -c rand_lowrank "
            << "-d \"50 300\" -t 10 --alpha 0.5 --momentum 0.9" << std::endl;
        // ANLS with differing matrix types
        INFO << "Usage 2: ./jointnmf -a 2 -k 15 -i rand_normal -c rand_uniform "
            << R"(-d "50 300" -t 10 --alpha 0.5 --mat_type "0 1" --sparsity 0.05)" << std::endl;
        // PGNCG with reading input files
        INFO << "Usage 2: ./jointnmf -a 10 -k 15 -i feat.mtx -c conns.mtx "
            << R"(-d "1024 2300" -t 10 --alpha 0.5 --mat_type "0 0")" << std::endl;
        break;
      case DISTJOINTNMF:
        INFO << std::endl;
        INFO << "distjointnmf usage:" << std::endl;
        INFO << "for short arguments like -i do not use equals sign, eg -t 10"
            << std::endl
            << "for long arguments like --seed give key=value pair or key value"
            << ",eg --seed=17 or --seed 17"
            << std::endl
            << "algorithm codes 2-ANLSBPP2D, 9-PGD, 10-PGNCG"
            << std::endl
            << "for array arguments like -r use quotes"
            << ", eg -r \"0.5 0.3 0.0 0.1\""
            << std::endl
            << "specify features matrix via -i and connections matrix via -c"
            << std::endl
            << R"(specify sparsity via --mat_type "0 0" denotes sparse/sparse, "1 1" dense/dense, etc (default is dense/dense))"
            << std::endl
            << "specify whether sparse matrix is already unpartitioned via --unpartitioned 1 and --unpartitioned 0 if not unpartitioned"
            << std::endl
            << "--conn_grid is used to define the second grid for the connections matrix"
            << std::endl
            << "Examples, --conn_grid \"5 3\" is a processor grid of size 5x3"
            << std::endl << std::endl;
        INFO << "Sample usages:" << std::endl;
        // Give an example for every algo type
        // PGD with default matrix types and single grid
        INFO << "Usage 1: mpirun -np 8 ./distjointnmf -a 9 -k 15 -i rand_lowrank -c rand_lowrank "
            << R"(-d "150 300" -p "2 4" -t 10 --alpha 0.5 --momentum 0.9 )" << std::endl;
        // ANLS with differing matrix types and double grid
        INFO << "Usage 2: mpirun -np 16 ./distjointnmf -a 16 -k 15 -i rand_normal -c rand_uniform"
            << R"(-d "150 300" -t 10 --alpha 0.5 --mat_type "0 1" )"
            << R"(-p "2 8" --conn_grid "4 4" --sparsity 0.05)" << std::endl;
        // PGNCG with reading input files
        INFO << "Usage 2: mpirun -np 2 ./distjointnmf -a 10 -k 15 -i feat.mtx -c conns.mtx"
            << R"(-d "1024 2300" -t 10 --alpha 0.5 --mat_type "0 0" )"
            << "-p \"1 2\" --unpartitioned 1" << std::endl;
        break;
      default:
        INFO << std::endl;
        INFO << "distnmf/distntf/nmf/ntf usage:" << std::endl;
        INFO << "for short arguments like -i do not use equals sign, eg -t 10"
            << std::endl
            << "for long arguments like --pr give key=value pair or key value"
            << ",eg --pr=4 or --pr 4"
            << std::endl
            << "algorithm codes 0-MU2D, 1-HALS2D, 2-ANLSBPP2D,"
            << "3-NAIVEANLSBPP (deprecated),"
            << "4-AOADMM2D, 5-NESTEROV, 6-CPALS, 7-GNSYM2D"
            << std::endl << std::endl;

        INFO << "Sample usages:" << std::endl;
        // mpirun -np 12 distnmf algotype lowrank m n numIteration pr pc
        INFO << "Usage 1: mpirun -np 6 distnmf -a 0/1/2/3 -k 50"
            << "-i rand_uniform/rand_normal/rand_lowrank"
            << R"(-d "21600 14400" -t 10 -p "3 2" )"
            << "--normalization \"l2\" "
            << "-r \"0.0001 0 0 0.0001\" " << std::endl;
        // mpirun -np 12 distnmf algotype lowrank AfileName numIteration pr pc
        INFO << "Usage 2: mpirun -np 6 distnmf -a 0/1/2/3 -k 50"
            << "-i Ainput -t 10 -p \"3 2\" "
            << "--normalization \"max\" "
            << "-r \"0.0001 0 0 0.0001\" " << std::endl;
        // mpirun -np 12 distnmf algotype lowrank Afile nmfoutput numIteration pr pc
        INFO << "Usage 3: mpirun -np 6 distnmf -a 0/1/2/3 -k 50"
            << "-i Ainput -o nmfoutput -t 10 -p \"3 2\" "
            << "-r \"0.0001 0 0 0.0001\" " << std::endl;
        // mpirun -np 12 distnmf algotype lowrank Afile nmfoutput numIteration pr pc
        // s
        INFO << "Usage 4: mpirun -np 6 distnmf -a 0/1/2/3 -k 50"
            << "-i Ainput -o nmfoutput -t 10 -p \"3 2\" --sparsity 0.3"
            << "-r \"0.0001 0 0 0.0001\" " << std::endl;
        // for symmetric nmf case
        // always a square grid for now
        // mpirun -np 16 distnmf algotype lowrank Afile nmfoutput numIteration pr pc
        // s
        INFO << "Usage 4: mpirun -np 16 distnmf -a 0/1/2/3 -k 50"
            << "-i Ainput -o nmfoutput -t 10 -p \"4 4\" --sparsity 0.3"
            << "--symm 0" << std::endl;
        // Shared memory nmf case
        INFO << "Usage 5: ./nmf -a 2 -d \"100 100\" -k 10 -t 10"
            << "-e 1 -i rand_lowrank --symm 2.0" << std::endl;
        // Distributed ntf case
        INFO << "Usage 6: mpirun -np 16 distntf -i rand_lowrank "
            << R"(-d "256 256 256 256" -p "2 2 2 2" )"
            << "-k 64 -t 30 -e 1 -a 5 --dimtree 1" << std::endl;
        // Shared memory ntf case
        INFO << "Usage 6: ./ntf -i rand_lowrank -d \"256 256 256 256\" "
            << "-k 64 -t 30 -e 1 -a 5 --dimtree 1" << std::endl;
        // Mention all the options
        INFO << std::endl;
        INFO << "\tThe following command line options are available:" << std::endl;
        INFO << "\t-a algonum, --algo algonum" << std::endl
            << "\t\t Integer reprsenting update algorithm." << std::endl
            << "\t\t 0 - Multiplicative Updating (MU)" << std::endl
            << "\t\t 1 - Hierarchical Alternating Least Squares (HALS)"
            << std::endl
            << "\t\t 2 - Block Principal Pivoting (BPP)" << std::endl
            << "\t\t 3 - Naive ANLS BPP (deprecated)" << std::endl
            << "\t\t 4 - Alternating Direction Method of Multipliers (ADMM)"
            << std::endl
            << "\t\t 5 - Nesterov Method (NES)" << std::endl
            << "\t\t\t Only available for ntf and distntf." << std::endl
            << "\t\t 6 - CANDECOMP/PARAFAC (CPALS)" << std::endl
            << "\t\t\t Only available for ntf and distntf." << std::endl
            << "\t\t 7 - Gauss-Newton using Conjugate Gradients (GNCG)"
            << std::endl
            << "\t\t\t Only available for nmf and distnmf for symmetric matrices."
            << std::endl;
        INFO << "\t-i inputdata, --input inputdata" << std::endl
            << "\t\t Input data can be a file name or synthetic"
            << " (rand_uniform, rand_normal, or rand_lowrank)." << std::endl;
        INFO << "\t-k rank, --lowrank rank" << std::endl
            << "\t\t Integer representing rank of the approximation." << std::endl;
        /* Only keeping -p option (deprecated)
        INFO << "\t-m matrix_rows" << std::endl
            << "\t\t Number of rows of synthetic input matrix"
            << " (use -d instead)." << std::endl;
        INFO << "\t-m matrix_cols" << std::endl
            << "\t\t Number of columns of synthetic input matrix."
            << " (use -d instead)." << std::endl;
        */
        INFO << "\t-d dims, --dimensions dims" << std::endl
            << "\t\t Dimensions of the input data. Examples," << std::endl
            << "\t\t -d \"10 6\" is an input matrix of size 10x6" << std::endl
            << "\t\t -d \"10 8 6\" is an input tensor of size 10x8x6" << std::endl
            << "\t\t This option is ignored when reading from a file."
            << std::endl;
        INFO << "\t-p grid, --processors grid" << std::endl
            << "\t\t Dimensions of the processor grid."
            << " Should be the same shape as the input. Examples," << std::endl
            << "\t\t -p \"5 3\" is a processor grid of size 5x3"
            << " and is appropriate for matrices." << std::endl
            << "\t\t -p \"4 1\" is a processor grid of size 4x1"
            << " and is appropriate for matrices." << std::endl
            << "\t\t -p \"5 3 2\" is a processor grid of size 5x3x2"
            << " and is appropriate for a 3D tensor." << std::endl;
        INFO << "\t-e [0/1], --error [0/1]" << std::endl
            << "\t\t Flag to compute error after every inner iteration."
            << " This flag is always set to true for shared memory computations"
            << " and the NES method." << std::endl;
        INFO << "\t-o outputstr, --output outputstr" << std::endl
            << "\t\t String to prepend output files with." << std::endl;
        INFO << "\t-r regs, --regularizer regs" << std::endl
            << "\t\t l2 and l1 regularizer to apply to the factor matrices."
            << "Need two values per input mode. Examples," << std::endl
            << "\t\t -r \"2.5 4 0 0.1\" applies the following regularization"
            << std::endl
            << "\t\t\t Factor on mode 1: 2.5 l2 reg, 4.0 l1 reg" << std::endl
            << "\t\t\t Factor on mode 2: 0.0 l2 reg, 0.1 l1 reg" << std::endl
            << "\t\t -r \"2.5 4 1.0 0.1 0 0\" applies the following regularization"
            << std::endl
            << "\t\t\t Factor on mode 1: 2.5 l2 reg, 4.0 l1 reg" << std::endl
            << "\t\t\t Factor on mode 2: 1.0 l2 reg, 0.1 l1 reg" << std::endl
            << "\t\t\t Factor on mode 3: 0.0 l2 reg, 0.0 l1 reg" << std::endl
            << std::endl;
        INFO << "\t-s dens, --sparsity dens" << std::endl
            << "\t\t Density of synthetic random matrix in the sparse case."
            << std::endl;
        INFO << "\t-t maxiters, --iter maxiters" << std::endl
            << "\t\t Maximum number of outer iterations to run." << std::endl;
        INFO << "\t--numkblocks numk" << std::endl
            << "\t\t Compute matrix multiply with blocks of the factor matrix to"
            << " save memory in distnmf. Default is set to 1." << std::endl;
        INFO << "\t--normalization [\"l2\"/\"max\"]" << std::endl
            << "\t\t Normalizes the synthetic input matrices in NMF." << std::endl
            << "\t\t\t l2: Normalizes the columns of the input matrix" << std::endl
            << "\t\t\t max: Normalizes all entries of the input matrix by"
            << " max value in the input." << std::endl;
        INFO << "\t--dimtree [0/1]" << std::endl
            << "\t\t Utilize dimension trees for MTTKRPs in ntf and distntf."
            << std::endl;
        INFO << "\t--symm alpha" << std::endl
            << "\t\t Set symmetric regularization for nmf/distnmf."
            << "Alpha controls the behavior as,"<< std::endl
            << "\t\t\t alpha = -1: Disable symmetric regularization (default)"
            << std::endl
            << "\t\t\t alpha = 0.0: Enable symmetric regularization with default"
            << " penalty factor alpha" << std::endl
            << "\t\t\t alpha > 0.0: Enable symmetric regularization with"
            << " user given penalty factor alpha" << std::endl;
        INFO << "\t--adjrand" << std::endl
            << "\t\t Adjust synthetically generated matrices elementwise."
            << " WARNING: This operation can increase rank of matrices"
            << " generated using rand_lowrank." << std::endl;
        INFO << "\t--luciters itr" << std::endl
            << "\t\t Set the number of inner iterations for certain update"
            << " algorithms (ADMM and GNCG)." << std::endl;
        INFO << "\t--seed sd" << std::endl
            << "\t\t Random seed for factor matrix initialization."
            << " WARNING: Only repeatable for running with the same grid size."
            << std::endl;
        INFO << "\t--unpartitioned 0/1" << std::endl
            << "\t\tspecify whether sparse matrix is already unpartitioned via"
            << "--unpartitioned 1 and --unpartitioned 0 if not unpartitioned"
            << std::endl;
    }
  }
  /// returns the low rank. Passed as parameter --lowrank or -k
  arma::uword lowrankk() { return clStruct->m_k; }
  /// return global rows. Passed as parameter -d
  arma::uword globalm() { return clStruct->m_globalm; }
  //// returns the global columns. Passed as parameter -d
  arma::uword globaln() { return clStruct->m_globaln; }
  /**
   * L2 regularization as the first parameter and L1 as second
   * for left lowrank factor W. Passed as parameter --regularizer
   * with pair of values in double quotes for W and H "l2W l1W l2H l1H"
   */
  arma::fvec regW() { return clStruct->m_regW; }
  /**
   * L2 regularization as the first parameter and L1 as second
   * for right lowrank factor H. Passed as parameter --regularizer
   * with pair of values in double quotes for W and H "l2W l1W l2H l1H"
   */
  arma::fvec regH() { return clStruct->m_regH; }
  /// Returns the NMF algorithm to run. Passed as parameter --algo or -a
  algotype lucalgo() { return clStruct->m_lucalgo; }
  /**
   *  Returns the process grid configuration.
   * Passed as parameter --processors or -p
   */

  arma::uvec processor_grids() { return clStruct->m_proc_grids; }
  /**
   * Returns the vector regularizers for all the modes.
   * It will 2 times the mode values. The first entry is
   * L2 regularization and second value is L1 for every mode.
   * Passed as parameter --regularizers "L2 L1" for every mode.
   */
  arma::fvec regularizers() { return clStruct->m_regularizers; }
  /**
   *  Returns vector of dimensions for every mode.
   * Passed as parameter -d or --dimensions
   */
  arma::uvec dimensions() { return clStruct->m_dimensions; }
  int num_k_blocks() { return clStruct->m_num_k_blocks; }
  /// Returns number of iterations. passed as -t or --iter
  int iterations() { return clStruct->m_num_it; }
  /// Returns error tolerance for stopping NMF iterations. Passed as -l or --tolerance
  double tolerance() { return clStruct->m_tolerance; }
  /// Returns number of nodes to compute in a H2NMF tree. Passed as -n or --nodes
  int nodes() { return clStruct->m_num_nodes; }
  /// Input parameter for generating sparse matrix. Passed as -s or --sparsity
  float sparsity() { return clStruct->m_sparsity; }
  /// Returns input file name. Passed as -i or --input
  std::string input_file_name() { return clStruct->m_Afile_name; }
  /// Returns connection file name. Passed as -c or --connection_matrix
  std::string conn_file_name() { return clStruct->m_Sfile_name; }
  /**
   * Returns output file name. Passed as -o or --output.
   * Every mode will appended as _mode.
   */
  std::string output_file_name() { return clStruct->m_outputfile_name; }
  /**
   * Returns the number of processor rows.
   * Used for distributed NMF. The first parameter of -p.
   */
  int pr() { return clStruct->m_pr; }
    /**
   * Returns the number of processor columns.
   * Used for distributed NMF. The second parameter of -p.
   */
  int pc() { return clStruct->m_pc; }
  /// Getter functions for the second grid (JointNMF)
  int cpr() { return clStruct->m_cpr; }
  int cpc() { return clStruct->m_cpc; }
  /// Returns number of modes in tensors. For matrix it is two.
  int num_modes() { return clStruct->m_num_modes; }
  /**
   * Enable dimension tree or not. By default we use dimension trees
   * for more than three modes. Passed as parameter --dimtree 1
   */
  bool dim_tree() { return clStruct->m_dim_tree; }
  /**
   * Enable adjusting random matrix creationg by using a pointwise
   * non-linear function. X(i,j) = ceil(alpha*X(i,j) + beta)
   */
  bool adj_rand() { return clStruct->m_adj_rand; }
  /// Returns whether to compute error not. Passed as parameter -e or --error
  bool compute_error() { return clStruct->m_compute_error; }
  /// To column normalize the input matrix.
  normtype input_normalization() { return clStruct->m_input_normalization; }
  /// Returns the value of the symmetric regularizer
  double symm_reg() { return clStruct->m_symm_reg; }
  /// return the maximum number of CG iterations to take
  int max_luciters() { return clStruct->m_max_luciters; }
  /// Initialisation seed for starting point of W, H matrices
  int initseed() { return clStruct->m_initseed; }
  // JointNMF parameters
  double joint_alpha() { return clStruct->alpha; }
  // JointNMF parameters
  double joint_beta() { return clStruct->beta; }
  // PGD momentum parameters
  double gamma() { return clStruct->m_gamma; }
  double unpartitioned() { return clStruct->m_unpartitioned; }
  bool feat_typesity() { return clStruct->feat_type; }
  bool conn_typesity() { return clStruct->conn_type; }

  params getPlancParams(int l_argv, const char * const *l_argc) {
      if (!this->parsed) {
          this->parse_args(l_argv, l_argc);
          this->initClStruct();
          this->parsed = true;
      }
      this->printConfig();
      return *clStruct;
  }

};  // ParseCommandLine
}  // namespace planc
