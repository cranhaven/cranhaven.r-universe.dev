/**
 ** Abstract class for layers of a network
 ** Jule 5, 2020
 ** Author: Youssef Hmamouche


  This file is part of NlinTS. NlinTS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 2 of the License, or
  (at your option) any later version.
  NlinTS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

 **/

#ifndef LAYER_H
#define LAYER_H

#include<vector>
#include<string>

using namespace std;

typedef vector<double> VectD;
typedef vector <VectD> MatD;
typedef vector <MatD> tensorD;

typedef vector<unsigned> VectU;
typedef vector <VectU> MatU;
typedef vector <MatU> tensorU;

class Layer
{
 public:
    virtual ~Layer()=0;
    virtual void set_input_dim (const vector<unsigned> & in_dim)=0;
    virtual bool contains_bias ()=0;
    virtual bool is_output ()=0;
    virtual bool is_input ()=0;
    virtual void set_output_layer (bool last)=0;
    virtual void set_input_layer (bool last)=0;
    virtual tensorD get_output ()=0;

    virtual vector<unsigned> get_output_dim ()=0;
    virtual vector<unsigned> get_input_dim ()=0;
    //virtual vector<double> simulate (const vector<double> & input, bool store)=0;
    virtual tensorD simulate (const tensorD & input, bool store)=0;
    virtual void computeErrors(const tensorD  & nextErrors)=0;
    virtual void updateWeights (unsigned numb_iter, unsigned batch_size)=0;

    virtual tensorD get_errors ()=0;
    virtual string getType ()=0;

    virtual string get_algo ()=0;
    virtual double get_learning_rate ()=0;
    virtual double get_drop ()=0;

    // non pure virtual functions
    virtual string get_activation () {return "";}
    virtual  tensorD get_weights () {return tensorD(0);}
    virtual unsigned get_pad (){ return 0;}
    virtual unsigned get_seed (){ return 0;}
    virtual VectU get_kernel_size (){ return VectU (0);}
    virtual double get_numb_filters (){ return 0;}
    virtual double get_stride (){ return 0;}


    virtual  void set_weights (const tensorD & ) {}
    virtual void summary (){}
};
inline Layer::~Layer() {}

#endif // LAYER_H
