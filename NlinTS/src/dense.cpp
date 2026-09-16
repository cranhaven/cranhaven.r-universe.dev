/*
  This file is part of NlinTS. NlinTS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 2 of the License, or
  (at your option) any later version.
  NlinTS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
*/

#include<Rcpp.h>
#include <vector>
#include <math.h>
#include "../inst/include/dense.h"
#include "../inst/include/utils.h"

using namespace std;

/***********************************/
Dense::Dense(unsigned _n_neurons, string _activation, double learning_rate_init_, bool bias_,  const string & alg, unsigned _seed, double _drop)
    :n_neurons (_n_neurons),
    activation (_activation),
    learning_rate_init (learning_rate_init_),
    algo (alg),
    seed (_seed),
    drop (_drop),
    output_layer (false),
    beta_1 (0.9),
    beta_2 (0.999)

{

    if (algo. compare ("sgd") != 0 and algo. compare ("adam") != 0)
    {
        Rcpp::Rcout << "Error, optimization algo not known in dense layer constructor, it must be in choice: [sgd, adam]." << endl;
        Rcpp::stop ("\n.");
    }

    /* convert bias (bool) to unsigned */
    bias = bias_?1:0;
}

/*********** copy constructor **************/
Dense::Dense (const Dense & denseLayer)
{
    n_neurons = denseLayer. n_neurons;
    activation = denseLayer. activation;
    learning_rate_init = denseLayer. learning_rate_init;
    output_layer = denseLayer. output_layer;
    algo = denseLayer. algo;
    bias = denseLayer. bias;
    seed = denseLayer. seed;
    drop = denseLayer. drop;
    beta_1 = denseLayer. beta_1;
    beta_2 = denseLayer. beta_2;
}

/***********************************/
void Dense::set_input_dim (const vector<unsigned> & in_dim)
{

    if (in_dim. size () != 3 or in_dim[0] != 1 or in_dim[1] != 1)
    {
        Rcpp::Rcout << "Error in dense layer when setting the input dimension, the input should be a tensor of 1 dimension.\n";
        Rcpp::stop ("\n.");
    }
    this->input_dim = in_dim[2];
    this->input.reserve (in_dim[2] + bias);

    /* Allocate memory for the vectors of the layer */
    this->net. reserve (n_neurons);
    this->E. reserve (n_neurons);
    this->O. reserve (n_neurons);
    //this->alpha. resize (n_neurons);
    this->M. resize(n_neurons);
    this->V. resize (n_neurons);
    this->W. resize (n_neurons);
    //this->DeltaW. resize (n_neurons);
    this->changeW. resize (n_neurons);

    // init  parameters (including adam parameters)
    for (unsigned long i = 0; i < this->n_neurons; ++i)
    {
        //this->alpha[i]. resize (this->input_dim + this->bias, learning_rate_init);
        //this->DeltaW[i]. resize (this->input_dim + this->bias, 0.0);
        this->changeW[i]. resize (this->input_dim + this->bias, 0.0);
        //this->W[i] =  random_vector (this->input_dim + this->bias);
        double variance = double(input_dim + n_neurons + bias) / 2.0;
        this->W[i] =  random_normal_vector (this->input_dim + this->bias, 0, 1.0 / variance, seed*(i+1));
        this->M[i]. resize (this->input_dim + this->bias, 0.0);
        this->V[i]. resize (this->input_dim + this->bias, 0.0);
    }
}

bool Dense::contains_bias() {return bool( bias);}

bool Dense::is_output(){return output_layer;}

void Dense::set_output_layer(bool last){output_layer = last;}

vector<unsigned> Dense::get_output_dim(){return {1,1,n_neurons};}

vector<unsigned> Dense::get_input_dim(){return {1,1,input_dim};}
/***********************************/
tensorD Dense::simulate (const tensorD & input_, bool store)
{
    if (input_. size () > 1 or input_[0]. size () > 1)
    {
        Rcpp::Rcout << "Input of the dense layer is not correct. \n";
        Rcpp::Rcout << "The input matrix is of size: (" << input_. size () << ", " << input_[0]. size () << ").\n";
        Rcpp::Rcout << "The input of the layer is: (1, " << input_dim << ").\n";
        Rcpp::stop ("\n.");
    }

    if (input_[0][0]. size () != this->input_dim)
    {
        Rcpp::Rcout << "      The input of the dense layer is not correct.. \n";
        Rcpp::Rcout << "      The input dimension must be: " << this->input_dim << ".\n";
        Rcpp::Rcout << "      The input line is of size: " <<input_. size () << ".\n";
        Rcpp::stop ("\n.");
    }

    MatD output (1);
    VectD input__;
    input__ = input_[0][0];

    if (this->bias)
        input__.insert (input__. begin (), 1);

    // output without activation function
    output[0] = matrix_dot (this->W, input__);

    if (store)
    {
        // store the last input
        this->input = input__;

        // apply dropout
        if (drop > 0)
        {
            drop_mask = random_bernoulli (n_neurons, 1- drop, seed);
            for (unsigned k = 0; k < n_neurons; ++k)
                if (drop_mask[k] == 0)
                    output[0][k] = 0;
        }

        net = output[0];
        // activate the output
        output[0] = vect_activation (output[0], activation);
        this->O = output[0];
    }

    else
    {
        if (drop > 0)
            for (unsigned k = 0; k < n_neurons; ++k)
                output[0][k] *= double (1- drop);

        // activate the output
        output[0] = vect_activation (output[0], activation);
    }

    return {output};
}

/***********************************/
void Dense::computeErrors(const tensorD & nextErrors)
{
    if (nextErrors. size () > 1 or nextErrors[0]. size () > 1)
    {
        Rcpp::Rcout << "Error to backpropagate to the dense layer is not correct. Matrix of 1 row is required. \n";
        Rcpp::Rcout << "The output errors matrix contains " << nextErrors. size () << ".\n";
        Rcpp::stop ("\n.");
    }

    /* nextErrors: errors of the next layer, and if the layer is an output layer, it is : (expected out - output) */
    if (nextErrors[0][0]. size () != this->n_neurons)
    {
        Rcpp::Rcout << "Error in computing the error, output dimensions are not correct.\n";
        Rcpp::Rcout << "Expecting " <<  this->n_neurons << " as output dimensions \n";
        Rcpp::Rcout << "While, the given errors are of size: " << nextErrors[0][0]. size ();
    }

    E = matrix_dot (nextErrors[0][0], diff_activation (net, activation));

    //  E * Transpose (I)
    for (unsigned i = 0; i < this->n_neurons; ++i)
        for (unsigned j = 0; j < input_dim + bias; ++j)
            changeW[i][j] += this->E[i] *  this->input[j];
}

/***********************************/
void Dense::updateWeights (unsigned numb_iter,  unsigned batch_size)
{
    double eps = 1e-8;

    const float bc1 = 1.0f - std::pow(this->beta_1, numb_iter + 1);
    const float bc2 = 1.0f - std::pow(this->beta_2, numb_iter + 1);

    if (algo == "adam")
    {
        for (unsigned j = 0; j < this->n_neurons; ++j)
        {
             for (unsigned i = 0; i < input_dim + bias; ++i)
                {
                    const double g = this->changeW[j][i] / batch_size;

                    //Rcpp::Rcout <<numb_iter << "   " <<  this->M[j][i] << "   " <<  this->V[j][i] << "  " << g << endl;
                    this->M[j][i] = (beta_1 * M[j][i]) + ((1 - beta_1) * g) ;
                    this->V[j][i] = (beta_2 * V[j][i]) + ((1 - beta_2) * g * g) ;

                    double m_hat = this->M[j][i]   / (bc1);
                    double v_hat = this->V[j][i]  / (bc2);

                    double delta_alpha = m_hat / (sqrt (v_hat) + eps);

                    W[j][i] = W[j][i] - delta_alpha  * learning_rate_init;

                    changeW[j][i] = 0;
                }
          }
    }
    else
    {
        for (unsigned j = 0; j < this->n_neurons; ++j)
        {
            for (unsigned i = 0; i < input_dim + bias; ++i)
            {
                W[j][i] = W[j][i] - (learning_rate_init  * changeW[j][i]  / batch_size);
                // initialize changes of weights
                changeW[j][i] = 0;
            }
         }
    }
}

/*********************************/
tensorD Dense::get_output()
{
    MatD out (1);
    out[0] = O;
    return {out};
}

/*********************************/
// return the errors to backpropagate to the previous layer: gradient of input
tensorD Dense::get_errors ()
{
    MatD A (1);
    A[0] = matrix_dot (Transpose (this->W), this->E);

    if (this->bias == 1)
        A[0]. erase (A[0]. begin ());

    return {A};
}

/*********************************/
tensorD Dense::get_weights(){return {this->W};}

/*********************************/
string Dense::getType(){return "dense";}
