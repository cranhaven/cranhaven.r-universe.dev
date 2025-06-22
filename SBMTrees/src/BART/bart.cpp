/*
 *  BART: Bayesian Additive Regression Trees
 *  Copyright (C) 2017 Robert McCulloch and Rodney Sparapani
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/GPL-2
 */

/*
 *  Modifications by Jungang Zou, 2024.
 *  - To make it easier to compile, I move most of the function definitions in this 
 *  .cpp file to the separate .h file, and merge them with declaration.
 *
 *  These modifications comply with the terms of the GNU General Public License 
 *  version 2 (GPL-2).
 */


#include "treefuns.h"
#include "bart.h"

//public functions
void bart::pr() //print to screen
{
   cout << "*****bart object:\n";
   cout << "m: " << m << std::endl;
   cout << "t[0]:\n " << t[0] << std::endl;
   cout << "t[m-1]:\n " << t[m-1] << std::endl;
   cout << "prior and mcmc info:\n";
   pi.pr();
   if(dart){
     cout << "*****dart prior (On):\n";
     cout << "a: " << a << std::endl;
     cout << "b: " << b << std::endl;
     cout << "rho: " << rho << std::endl;
     cout << "augmentation: " << aug << std::endl;
   }
   else cout << "*****dart prior (Off):\n";
   if(p) cout << "data set: n,p: " << n << ", " << p << std::endl;
   else cout << "data not set\n";
}
