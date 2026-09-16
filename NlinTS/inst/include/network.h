/**
 **  class for a neural network
 ** Jule 5, 2020
 ** Author: Youssef Hmamouche
  Copyright (c) 2020 Youssef Hmamouche.
  This file is part of NlinTS. NlinTS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 2 of the License, or
  (at your option) any later version.
  NlinTS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  */

#ifndef NETWORK_H
#define NETWORK_H

#include<vector>
#include <iomanip>
#include"layer.h"

using namespace std;

class Network
{
private:
    vector<unsigned> input_dim;

    VectD input;
    unsigned nb_layers;

    void add_dense_layer(ifstream &file);

public:
    vector<Layer*> layers;
    /****************************************/
    Network (const vector<unsigned> & _input_dim)://, unsigned batch_size):
        input_dim (_input_dim),
        nb_layers (0)
        //batch_size (batch_size)
    {}

    Network ():
        nb_layers (0)
    {}

    void set_input_dim (const vector<unsigned> & _input_dim)
    {
        input_dim = _input_dim;
    }

    VectU get_input_dim ()
    {
        return input_dim;
    }
    ~Network()
    {
        for (auto layer : this->layers)
            delete layer;
    }

    unsigned get_nb_layers (){return nb_layers;}
    void addLayer (Layer * layer);
    MatD simulate (const MatD & input, bool store);
    void backpropagation (const vector <double> & output);
    void updateWeight (unsigned numb_iter, unsigned batch_size);
    double univariate_loss (const VectD&preds, const VectD&real);
    double average_loss (const MatD &preds, const MatD &real);
    VectD compute_derived_error (const VectD & real, const VectD & predicted, const string & loss);
    void train (const MatD &X, const MatD &y, unsigned, unsigned,  const string &);
    void fit (const MatD & X, const MatD & y, unsigned n_iters, unsigned batch_size,  const string & loss_function, bool shuffle, unsigned seed);
    MatD predict (const MatD & X);
    VectD score(const MatD &X, const MatD &y);

    VectD input_features_scores ();

    void summary ();

    void save (const string & filename);

    void load(const string &filename);
};

#endif // NETWORK_H
