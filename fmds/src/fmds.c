//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL. 
//

#define R

#include "fmds.h"

// main for fmds library

double fmds( 
  const size_t n, double** delta, double** w, 
  const size_t p, double** z, int** fz,
  const size_t h, double** q, double** b, 
  double** d, 
  const bool anchor, const bool power, const size_t degree, const size_t ninner, double* iknots, const int knotstype, const int approach,
  const size_t MAXITER, const double FCRIT, const double ZCRIT, size_t* lastiter, double* lastdif, const bool echo )
{
  double result = 0.0;

  if ( isnull( w ) ) {
    if ( fz == NULL && q == NULL ) { // no restrictions
      if ( approach != 0 ) result = ordmds( n, delta, p, z, d, approach, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
      else if ( knotstype != 0 ) result = splmds( n, delta, p, z, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
      else if ( power == true ) result = powmds( n, delta, p, z, d, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
      else if ( anchor == true ) result = linmds( n, delta, p, z, d, anchor, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
      else result = mds( n, delta, p, z, d, anchor, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
    }
    else if ( fz != NULL ) { // fixed coordinates restriction
      if ( approach != 0 ) { // ordinal
      }
      else if ( knotstype != 0 ) { // spline
      }
      else if ( power == true ) { // power
      }
      else if ( anchor == true ) { // linear
      }
      else { // none
      }
    }
    else if ( q != NULL ) { // variables restriction
      if ( approach != 0 ) { // ordinal
      }
      else if ( knotstype != 0 ) { // spline
      }
      else if ( power == true ) { // power
      }
      else if ( anchor == true ) { // linear
      }
      else {
      }
    }
  }
  else {
    if ( fz == NULL && q == NULL ) { // no restrictions
      if ( approach != 0 ) result = ordwgtmds( n, delta, w, p, z, d, approach, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
      else if ( knotstype != 0 ) result = splwgtmds( n, delta, w, p, z, d, degree, ninner, iknots, anchor, knotstype, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
      else if ( power == true ) result = powwgtmds( n, delta, w, p, z, d, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
      else if ( anchor == true ) result = linwgtmds( n, delta, w, p, z, d, anchor, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
      else result = wgtmds( n, delta, w, p, z, d, MAXITER, FCRIT, ZCRIT, lastiter, lastdif, echo );
    }
    else if ( fz != NULL ) { // fixed coordinates restriction
      if ( approach != 0 ) { // ordinal
      }
      else if ( knotstype != 0 ) { // spline
      }
      else if ( power == true ) { // power
      }
      else if ( anchor == true ) { // linear
      }
      else { // none
      }
    }
    else if ( q != NULL ) { // variables restriction
      if ( approach != 0 ) { // ordinal
      }
      else if ( knotstype != 0 ) { // spline
      }
      else if ( power == true ) { // power
      }
      else if ( anchor == true ) { // linear
      }
      else {
      }
    }
  }

  return( result );
} // fmds
