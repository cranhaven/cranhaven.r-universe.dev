/**
 This file is part of NlinTS. NlinTS is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 2 of the License, or
 (at your option) any later version.
 NlinTS is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 **/

#include "../inst/include/varnnExport.h"

using namespace std;

VARNN_Export::VARNN_Export (unsigned p,
							Rcpp::IntegerVector size_Layers,
							Rcpp::StringVector activations,
							double learning_rate_init,
							string  algo,
							bool bias,
							unsigned seed)
{

	// Rccpp vectors to classical vectors
    vector<unsigned> sizeOfLayers_ = Rcpp::as <vector<unsigned> > (size_Layers);
    vector<string> activations_ = Rcpp::as <vector<string> > (activations);

    // Create the model
    Obj = VARNN (sizeOfLayers_, p, bias,  learning_rate_init, activations_, algo, seed);
  }


void VARNN_Export::fit (Rcpp::DataFrame Df, unsigned iters, unsigned batch_size)
{

    vector<vector<double> > Mat = Rcpp::as < vector<vector<double> > > (Df);
    unsigned nc = Mat.size ();
    Struct::CMatDouble M (nc);

    unsigned i = 0;

    // Conversion from R dataframe to  C++ table.
    while(i < nc)
    {
        for (auto Value = Mat[i].begin () ; Value != Mat[i].end () ; ++Value)
        M[i].push_back (*Value);
        i++;
    }

    Obj.fit (M, iters, batch_size);
}


Rcpp::DataFrame VARNN_Export::forecast (Rcpp::DataFrame DF)
  {

    unsigned i = 0;
    vector<vector<double> > Mat = Rcpp::as < vector<vector<double> > > (DF);
    unsigned  nc = Mat.size();
    Struct::CMatDouble P (nc);

     // Conversion des données dataFrame en matrice en C++
     while(i < nc)
     {
         for (auto Value = Mat[i].begin () ; Value != Mat[i].end () ; ++Value)
         {
             P[i].push_back (*Value);
         }
         i++;
     }

      Rcpp::List list (nc);
      Rcpp::DataFrame dataFrame;

      Struct::CMatDouble SM = Obj.forecast (P);

      for ( unsigned j = 0; j < nc; j++)
          list[j] = Rcpp::wrap( SM[j].begin(), SM[j].end() ) ;

      dataFrame = list;

      Rcpp::CharacterVector ch = DF.names ();

      if (ch.size() > 0)
          dataFrame.names() = ch;

      return dataFrame;

 }

Rcpp::NumericVector VARNN_Export::getSSR ()
{
    return (Rcpp::wrap (Obj.getSSR()));
}
