#ifndef UTILS_H
#define UTILS_H

#include <vector>
#include <algorithm>    //
#include <chrono>
#include <random>       // std::default_random_engine, random_shuffle
using namespace std;

typedef vector<double> VectD;
typedef vector <VectD> MatD;

/*****************************/
template<class A, class B>
void shuffle_X_y (vector<A> & X, vector<B> & Y, unsigned seed = 0)
{
    std::default_random_engine generator;
    if (seed == 0)
        generator. seed (std::chrono::system_clock::now().time_since_epoch().count());
    else
        generator. seed (seed);

      // using the same seed for both vectors
      shuffle (X.begin(), X.end(), generator);
      shuffle (Y.begin(), Y.end(), generator);
}


/*****************************/
template<typename T>
void copy_vector (vector<T> & V1, const vector<T> & V2)
{
    V1. clear();
    V1. reserve (V2. size ());
    for (const T & val : V2)
        V1.push_back (val);
}

vector<unsigned> random_bernoulli (unsigned n, double p, unsigned seed);
double get_random(double min, double max, unsigned seed);
vector <double> random_vector (unsigned size, double min, double max, unsigned seed);
double get_random_normal(double mean, double std, unsigned seed);
vector <double> random_normal_vector (unsigned long size, double mean, double std, unsigned seed);

std::vector<double> get_col (const std::vector<std::vector<double>> & A, unsigned long j);
std::vector<std::vector<double>> Transpose (const std::vector<std::vector<double>> & M);


MatD reshape (const VectD & A, unsigned axis);
void matrix_dot (MatD & A, double a);
// hammart product
VectD matrix_dot (const VectD & A, const VectD & B);
VectD matrix_dot (const MatD & A, const VectD & B);
MatD matrix_dot (const MatD & A, const MatD & B);
VectD matrix_sum (const VectD & A, const VectD & B);
MatD matrix_sum (const MatD & A, const MatD & B);
VectD matrix_sum (const VectD & A, const VectD & B, const VectD & C, const VectD & D);
MatD matrix_sum (const MatD & A, const MatD & B, const MatD & C, const MatD & D);

VectD matrix_mean (const MatD &A);

double sum_vect (const vector<double> & Vect);
double min_vect (const std::vector<double> & Vect);
double max_vect (const std::vector<double> & Vect);
std::vector<std::vector<double>> Normalise (std::vector<std::vector<double>> & mat);
std::vector <double> r_score (const std::vector<std::vector<double> > &pred, const std::vector<std::vector<double> > &real);
//double sum_vect (const std::vector<double> & A);

// activation functions and their derivatives
double sigmoid (double x);
double deriveSigmoid (double x);
double relu (double x);
double deriveRELU (double x);
double tanh_ac (double x);
double deriveTanh (double x);

// activation functions and their derivatives for vectors
VectD sigmoid_v (const VectD & A);
VectD relu_v (const VectD & A);
VectD tanh_v (const VectD & A);
VectD sigmoid_diff (const VectD & A);
VectD relu_diff (const VectD & A);
VectD tanh_diff (const VectD & A);
VectD vect_activation (const VectD & A, const string & f);
VectD diff_activation (const VectD & A, const string & f);

vector<string> split_str(const std::string& str, char delim = ' ');
std::vector<std::string> split (const std::string &, const char &);
vector<double> split_d (const string &line, const char & sep);

#endif
