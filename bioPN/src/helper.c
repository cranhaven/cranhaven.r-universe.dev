// -*- mode: C; c-indent-level: 2; c-basic-offset: 2; tab-width: 8 -*-
//
// Copyright (C) 2009-2014 Roberto Bertolusso and Marek Kimmel
//
// This file is part of bioPN.
//
// bioPN is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// bioPN is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with bioPN. If not, see <http://www.gnu.org/licenses/>.

#include <R.h>


void PrintfTime(double dSeconds)
{
  if (dSeconds > 86400)
    Rprintf ("%6.2fd", dSeconds/86400);
  else if (dSeconds > 3600)
    Rprintf ("%6.2fh", dSeconds/3600);
  else if (dSeconds > 60)
    Rprintf ("%6.2fm", dSeconds/60);
  else
    Rprintf ("%6.2fs", dSeconds);
}
