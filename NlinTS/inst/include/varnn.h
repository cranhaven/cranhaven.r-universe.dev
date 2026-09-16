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

#ifndef VARNN_H
#define VARNN_H

#include <cstdlib>
#include <memory>

#include"struct.h"
#include"network.h"


using namespace std;
using namespace Struct;

class VARNN
{
  private:

      std::vector<unsigned> sizeOfLayers;
      unsigned  lag;  // lag parameter
      bool bias;
      double learning_rate_init; // initial learning rate
      std::vector<string> activations;
      string algo; //backpropagation algorithm
      unsigned seed; // for random generation of the weights of the network
      unsigned long  Nb_Ln ;    // numbre of observations
      unsigned long  Nb_Cl ;    // nombre of variables
      string activation; //activation function
      unsigned  numLayers;

      Network mlp;
      std::vector<double> SSR;
      Struct::CMatDouble inputMat ;    // input data

  public:
      VARNN (const vector<unsigned> & sizeOfLayers,
             unsigned p,
             bool bias,
             double learning_rate_init,
             const  std::vector<string> & activations,
             const string & algo,
             unsigned seed);
      VARNN(){}
     ~VARNN(){}

      /*********************************************************/
      void fit (const CMatDouble & M, unsigned iterations, unsigned batch_size);

      /*********************************************************/
      Struct::CMatDouble forecast (const Struct::CMatDouble & M);

      void save (const string & filename) {mlp. save (filename);}
      void load (const string & filename) {mlp. load (filename);};

      Struct::CVDouble getMSE ();
      Struct::CVDouble getMAE ();

      // Sum of squared errors
      std::vector<double> getSSR ();
};

#endif // VARNN_H
