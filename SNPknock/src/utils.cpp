/*
  This file is part of SNPknock.

    Copyright (C) 2017-2019 Matteo Sesia

    SNPknock is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SNPknock is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with SNPknock.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef UTILS_CPP
#define UTILS_CPP

#include "utils.h"

int weighted_choice(double R, const std::vector<double> & weights) {
  for(unsigned int i=0; i<weights.size(); i++) {
    if(R < weights[i]) return(i);
    R -= weights[i];
  }
  return(weights.size()-1);
}

#endif
