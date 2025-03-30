#pragma once
#include "config.h"
#include <unordered_map>
#include <string>

extern "C" {
#include "hw_detect.h"
}

/* Copyright 2016 Ramakrishnan Kannan */
// utility functions

// #ifndef _VERBOSE
// #define _VERBOSE 1
// #endif

enum algotype {
    MU, HALS, ANLSBPP, NAIVEANLSBPP, AOADMM,
    NESTEROV, CPALS, GNSYM, R2, PGD, PGNCG
};

extern std::unordered_map<std::string, algotype> algomap;
extern std::unordered_map<std::string, algotype> symmap;


enum normtype { NONE, L2NORM, MAXNORM };

extern std::unordered_map<std::string, normtype> normmap;


enum helptype { NMF, DISTNMF, NTF, DISTNTF, JOINTNMF, DISTJOINTNMF, HIERNMF };


#include <cmath>
#include <iostream>
#include <vector>

template<typename T>
arma::uword chunk_size_dense(arma::uword rank) {
#ifdef _OPENMP
    return (get_l1_data_cache() / (rank * sizeof(T)));
#else
    return (get_l2_data_cache() / (rank * sizeof(T)));
#endif
}

// using namespace std;


constexpr auto EPSILON_1EMINUS16 = 0.00000000000000001;
constexpr auto EPSILON_1EMINUS8 = 0.00000001;
constexpr auto EPSILON = 0.000001;
constexpr auto EPSILON_1EMINUS12 = 1e-12;
constexpr auto NUMBEROF_DECIMAL_PLACES = 12;
constexpr auto RAND_SEED = 100;
constexpr auto RAND_SEED_SPARSE = 100;
constexpr auto WTRUE_SEED = 1196089;
constexpr auto HTRUE_SEED = 1230587;


#define PRINTMATINFO(A) "::" #A "::" << (A).n_rows << "x" << (A).n_cols

#define PRINTMAT(A) PRINTMATINFO((A)) << std::endl << (A)

typedef std::vector<int> STDVEC;
#ifndef ULONG
typedef uint64_t ULONG;
#endif

void absmat(const arma::fmat* X);


template<typename FVT>
inline void fillVector(const FVT value, std::vector<FVT>* a) {
    for (unsigned int ii = 0; ii < a->size(); ii++) {
        (*a)[ii] = value;
    }
}
