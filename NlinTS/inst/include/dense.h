/**
 ** Feed forward layer
 ** Jule 1, 2020
 ** Author: Youssef Hmamouche
 **/


#ifndef DENSE_H
#define DENSE_H

#include <Rcpp.h>
#include<vector>
#include<string>

#include"layer.h"
#include"utils.h"

using namespace std;


class Dense: public Layer
{
private:

    unsigned n_neurons;
    string activation;
    double learning_rate_init; // initial learning rate
    unsigned input_dim;
    unsigned bias;
    string algo;
    unsigned seed;
    double drop;
    bool output_layer;
    bool input_layer;
    // Parameters for the Adam algorithm
    double beta_1; // decay rates for M
    double beta_2; // decay rates for V

    // last sum of input (without activation function)
    VectD net;

    // last input
    VectD input;
    // output
    VectD O;
    // errors
    VectD E;

    // learning rate of each weight
    MatD alpha;

    MatD M; //gradient
    MatD V; //squared gradient

    // Weights of the neurons related to the previous layer
    MatD W;

    // drop out mask
    vector<unsigned int> drop_mask;

    // Gradients of Weights
    MatD DeltaW;
    MatD changeW;


public:

    Dense (unsigned n_neurons, string activation, double learning_rate_init_, bool bias_,  const string & alg, unsigned seed, double drop = 0.0);
    Dense (const Dense & denseLayer);
    Dense (){}

    void set_input_dim (const vector<unsigned> & in_dim);

    bool contains_bias ();
    bool is_output ();
    void set_output_layer (bool last);

    bool is_input (){return input_layer;}
    void set_input_layer (bool last) {input_layer = last;}

    vector<unsigned> get_output_dim ();
    vector<unsigned> get_input_dim ();

    tensorD simulate (const tensorD & input, bool store);

    void computeErrors(const tensorD & nextErrors);
    void updateWeights (unsigned numb_iter, unsigned batch_size);
    tensorD get_output ();
    tensorD get_errors ();

    tensorD get_weights ();
    string getType ();

    string get_activation (){ return activation;}
    string get_algo (){ return algo;}
    double get_learning_rate (){ return learning_rate_init;}
    double get_drop (){ return drop;}
    unsigned get_seed (){ return seed;}

    void set_weights (const tensorD & We)
    {
        W = We[0];
    }

    void summary ()
    {
        Rcpp::Rcout << "======== Dense layer Summary  ==========\n";
        Rcpp::Rcout << "   Input size: "<< 1 << " " << " " << 1 << " " <<input_dim << endl;
        Rcpp::Rcout << "   Output size: "<< 1 << " " << " " << 1 << " " <<n_neurons << endl;
        Rcpp::Rcout << "===============================\n";

    }
};


#endif // LAYER_H
