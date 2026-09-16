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

#include <iostream>
#include <fstream>
#include<Rcpp.h>
#include <math.h>       /* log */

#include "../inst/include/network.h"
#include "../inst/include/utils.h"
#include "../inst/include/dense.h"


/******** add a layer to the network **************/
void Network::addLayer(Layer * layer)
{
    vector<unsigned> in_dim;

    bool is_input = false;
    if (this->layers. size () == 0)
    {
      in_dim = input_dim;
      is_input = true;
    }

    else
        in_dim = this->layers.back ()->get_output_dim ();

    // add the layer
    this->layers.push_back (layer);

    // set if it is an input layer
    this->layers.back ()->set_input_layer(is_input);

    // set input dimension
    this->layers.back ()->set_input_dim (in_dim);

    // consider this layer as output layer
    this->layers. back ()->set_output_layer(true);

    // set the previous layer as hidden layer
    if (this->layers.size () > 1)
        this->layers [this->layers. size () - 2]-> set_output_layer (false);

     nb_layers ++;
}

/******** Propagate an input through the signal **************/
MatD Network::simulate(const MatD & _input, bool store)
{
    tensorD input_signal ({_input});

    for (unsigned i = 0; i < nb_layers; ++i)
    {
        input_signal = this->layers[i]-> simulate (input_signal, store);
    }

    return (input_signal[0]);
}

/*********************************************************/
void Network::backpropagation(const VectD & derived_error)
{
    tensorD propagatedErrors;
    int num_layers = int(this->layers.size ());

    for (int i = num_layers - 1; i >= 0; --i)
    {
        // hidden layer
        if (i < (num_layers - 1))
            propagatedErrors = this->layers[i+1]-> get_errors ();

        // Output layer
        else
            propagatedErrors = {{derived_error}};

        this->layers[i]->computeErrors (propagatedErrors);
        propagatedErrors. clear ();
    }
}

/*******************************************************/
void Network::updateWeight (unsigned numb_iter, unsigned batch_size)
{
    for (unsigned i = 0; i < this->layers. size (); ++i)
        this->layers[i]->updateWeights (numb_iter, batch_size);
}

/************* loss function (mse) *******************/
double Network::univariate_loss (const VectD &preds, const VectD &real)
{
    if (preds. size () != real. size ())
    {
        Rcpp::Rcout << "Error in calculating the loss function, preds and real have not the same size. \n";
        Rcpp::stop ("\n.");
    }
    double mse = 0;
    for (unsigned i = 0; i < preds. size (); ++i)
        mse += (real [i] - preds[i]) * (real [i] - preds[i]);

    return mse / preds. size ();
}

/************* average loss function *******************/
double Network::average_loss(const MatD &preds, const MatD &real)
{
    if (preds. size () != real. size ())
    {
        Rcpp::Rcout << "Error in calculating the average_loss function, preds and real have not the same size. \n";
        Rcpp::stop ("\n.");
    }

    double avg_mse = 0;
     for (unsigned i = 0; i < preds. size (); ++i)
         avg_mse += univariate_loss (preds[i], real [i]);

     return avg_mse /  preds. size ();
}


/************* compute derived output error between a predicted and real vectors ********************/
VectD Network::compute_derived_error (const VectD & real, const VectD & predicted, const string & loss)
{
    if (real. size () != predicted. size ())
    {
        Rcpp::Rcout << "Error when computing the output error of the network, the real and predicted vectors don't have the same size.\n";
        Rcpp::stop ("\n.");
    }
    VectD error (real. size (), 0);
    if (loss == "mse")
    {
        for (unsigned i = 0; i < real. size (); ++i)
            error[i] = predicted[i] - real[i];
    }
    else if (loss == "binary_cross_entropy")
    {
        if (real. size () > 1)
        {
            Rcpp::Rcout << "Error, binary_cross_entropy is used for 1 class prediction.\n";
            Rcpp::stop ("\n.");
        }

        if (predicted[0] == 0.0)
            error[0] = (1 - real[0]) / (1 - predicted[0]);
        else if (predicted[0] == 1.0)
            error[0] = - (real[0] / predicted[0]);
        else
            error[0] = - (real[0] / predicted[0]) + (1.0 - real[0]) / (1.0 - predicted[0]);
    }

    return error;
}


/************** Train the network: one epoch  *********************/
void Network::train(const MatD &X, const MatD &y, unsigned numb_iter, unsigned batch_size,  const string & loss_function)
{
    //VectD predictions, mean_gradient_errors;
    for (unsigned i = 0; i < y. size (); ++i)
    {
        backpropagation (compute_derived_error (y[i], this->simulate ({{X[i]}}, true)[0], loss_function));

        if ((i % batch_size) == 0 or i == X. size () - 1)
        {
            updateWeight (numb_iter*y.size() + i, batch_size);
        }
    }
}

/************** Train the network : multiple epochs *********************/
void Network::fit (const MatD & X, const MatD & y, unsigned n_iters, unsigned batch_size,  const string & loss_function, bool shuffle /*= true*/, unsigned seed /*=0*/)
{
    MatD X_shuffled (X);
    MatD y_shuffled (y);

    for (unsigned i = 0; i < n_iters; ++i)
    {
        if (shuffle)
            shuffle_X_y (X_shuffled, y_shuffled, seed);
        this->train (X_shuffled, y_shuffled, i, batch_size, loss_function);
    }
}

/********** test the network *************/
MatD Network::predict(const MatD &X)
{
    MatD predictions (X. size ());

    for (unsigned i = 0; i < X. size (); ++i)
        predictions[i] = this->simulate ({{X[i]}}, false)[0];

    return predictions;
}


/********** Comput the R² score *************/
VectD Network::score(const MatD &X, const MatD &y)
{
    return r_score (y, predict (X));
}

/********************************************/
VectD Network::input_features_scores()
{
    VectD scores;

    scores = matrix_mean (this->layers[0]->get_weights()[0]);

    if (this->layers[0]->contains_bias())
            scores.erase (scores.begin());

    return scores;
}

/*************************/
void Network::summary()
{
    unsigned i = 1;
    Rcpp::Rcout << "====================================================\n";
    Rcpp::Rcout << "Layer_type" << std::setw(20) << "Input_shape"<< std::setw(20)  <<"Output_dim\n";
    Rcpp::Rcout << "====================================================\n";

    string layer_type, layer_input, layer_output;
    for (const auto & layer: this->layers)
    {
        layer_type = layer->getType();
        layer_input = "(";
        for (auto a : layer->get_input_dim())
        {
             layer_input += to_string (a);
             layer_input += ", ";
        }
        layer_input. pop_back();
        layer_input. pop_back();
        layer_input. push_back(')');

        layer_output = "(";
        for (auto a : layer->get_output_dim())
        {
             layer_output += to_string (a);
             layer_output += ", ";
        }
        layer_output. pop_back();
        layer_output. pop_back();
        layer_output. push_back(')');

        Rcpp::Rcout << layer_type << std::setw (30 - layer_type. size ()) << layer_input << std::setw (20)<< layer_output << "\n";

        if (i < this->nb_layers)
            Rcpp::Rcout << "----------------------------------------------------\n";
        ++i;
    }
    Rcpp::Rcout << "====================================================\n";
}

/************************************************/
// save the model in a text file
void Network::save(const string & filename)
{
    ofstream file (filename.c_str());

    unsigned i = 1;
    for (const auto & layer: this->layers)
    {
        if (layer->getType() == "poolingMax" or layer->getType() == "flattener" or layer->getType() == "normalizer1D" or layer->getType() == "activation" or layer->getType() == "drop_out")
        {
            file << "Layer_" << i << ":" << layer->getType() << "\n";
            file << "Input_shape:"  << layer->get_input_dim()[0] << "\t" << layer->get_input_dim()[1] << "\t" << layer->get_input_dim()[2] << "\n";
            file << "Output_shape:" << layer->get_output_dim()[0] << "\t" << layer->get_output_dim()[1] << "\t" << layer->get_output_dim()[2] << "\n";

            if (layer->getType() == "poolingMax")
                file << "Filters size:" << layer->get_kernel_size()[0] << "\t" << layer->get_kernel_size()[1] << "\n";

            if (layer->getType() == "activation")
                file << "Activation:" << layer->get_activation() << "\n";
        }

        else
        {
            file << "Layer_" << i << ":" << layer->getType() << "\n";
            file << "Input_shape:"  << layer->get_input_dim()[0] << "\t" << layer->get_input_dim()[1] << "\t" << layer->get_input_dim()[2] << "\n";
            file << "Output_shape:" << layer->get_output_dim()[0] << "\t" << layer->get_output_dim()[1] << "\t" << layer->get_output_dim()[2] << "\n";
            file << "Bias:" << layer->contains_bias() << "\n";
            file << "Optimization algorithm:" << layer->get_algo() << "\n";
            file << "Learning rate:" << layer->get_learning_rate () << "\n";
            file << "Activation:" << layer->get_activation() << "\n";
            file << "Drop-out:" << layer->get_drop() << "\n";
            file << "Seed:" << layer->get_seed() << "\n";

            if (layer->getType() == "conv2D")
            {
                file << "Pad:" << layer->get_pad () << "\n";
                file << "Stride:" << layer->get_stride () << "\n";
                file << "Filters size:" << layer->get_kernel_size()[0] << "\t" << layer->get_kernel_size()[1] << "\n";
                file << "Number of filters:" << layer->get_numb_filters() << "\n";
            }
        }

        if (layer->getType() == "conv2D" or layer->getType() == "dense" or layer->getType() == "lstm")
        {
            unsigned j = 1;
            for (const MatD & W: layer->get_weights())
            {
                file << "Weights_"<< j << ":\n";
                for (const auto & row : W)
                {
                    for (unsigned i (0); i < W[0]. size () - 1; ++i)
                        file << row[i] << "\t";

                    file << row[W[0]. size () - 1] << "\n";
                }
                ++j;
            }
        }

        ++i;
        file << "=============================================\n";
    }
    file. close ();
}

/************************************************/
void Network::add_dense_layer (ifstream & file)
{
    string line;
    getline(file, line);
    VectU input_dim;
    for (auto a: split_d (split_str (line, ':')[1], '\t'))
        input_dim. push_back (unsigned (a));

    getline(file, line);
    VectU output_dim;
    for (auto a: split_d (split_str (line, ':')[1], '\t'))
        output_dim. push_back (unsigned (a));


    getline(file, line);
    unsigned bias = stoi (split_str (line, ':')[1]);

    getline(file, line);
    string algo = split_str (line, ':')[1];

    getline(file, line);
    double Learning_rate = stod (split_str (line, ':')[1]);

    getline(file, line);
    string activ_function = split_str (line, ':')[1];

    getline(file, line);
    double drop = stod (split_str (line, ':')[1]);

    getline(file, line);
    unsigned seed = stoi (split_str (line, ':')[1]);

    if (this-> get_nb_layers () == 0)
        this-> set_input_dim (input_dim);

    this-> addLayer (new Dense (output_dim[2], activ_function, Learning_rate, bias, algo, seed, drop));

    tensorD weights (1);
    getline(file, line);
    if (split_str(line, ':')[0]. compare ("Weights_" + to_string (1)) != 0)
    {
        Rcpp::Rcout << "Error reading weights from file.\n";
        Rcpp::stop ("\n.");
    }
    for (unsigned j = 0; j < unsigned(output_dim[2]); ++j)
    {
        getline(file, line);
        weights[0]. push_back (split_d (line, '\t'));
    }

    this->layers.back ()-> set_weights (weights);
}

/************************************************/
// load the network from a text file
void Network::load(const string & fileName)
{
    for (auto layer : this->layers)
        delete layer;

    this->layers. clear ();
    nb_layers = 0;

    ifstream file (fileName.c_str(), std::ifstream::in);
    string line;

    while (getline(file, line))
    {
        if (split_str (line, '_')[0]. compare ("Layer") == 0)
        {
            if (split_str (line, ':')[1] == "dense")
                add_dense_layer (file);
        }
    }
    file. close ();
}
