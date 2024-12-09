//
// Copyright (c) 2020 Frank M.T.A. Busing (e-mail: busing at fsw dot leidenuniv dot nl)
// FreeBSD or 2-Clause BSD or BSD-2 License applies, see Http://www.freebsd.org/copyright/freebsd-license.html
// This is a permissive non-copyleft free software license that is compatible with the GNU GPL.
//

#ifndef FMDU_H
#define FMDU_H

#define R

#include "flib.h"


extern double mdu( const size_t n, const size_t m, double** delta, const size_t p, double** x, int** fx, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double wgtmdu( const size_t n, const size_t m, double** delta, double** w, const size_t p, double** x, int** fx, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

extern double mduneg( const size_t n, const size_t m, double** delta, const size_t p, double** x, int** fx, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double wgtmduneg( const size_t n, const size_t m, double** delta, double** w, const size_t p, double** x, int** fx, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

extern double rowresmdu( const size_t n, const size_t m, double** delta, const size_t p, const size_t h, double** q, double** b, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double rowreswgtmdu( const size_t n, const size_t m, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

extern double rowresmduneg( const size_t n, const size_t m, double** delta, const size_t p, const size_t h, double** q, double** b, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double rowreswgtmduneg( const size_t n, const size_t m, double** delta, double** w, const size_t p, const size_t h, double** q, double** b, double** y, int** fy, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

extern double colresmdu( const size_t n, const size_t m, double** delta, const size_t p, double** x, int** fx, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double colreswgtmdu( const size_t n, const size_t m, double** delta, double** w, const size_t p, double** x, int** fx, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

extern double colresmduneg( const size_t n, const size_t m, double** delta, const size_t p, double** x, int** fx, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double colreswgtmduneg( const size_t n, const size_t m, double** delta, double** w, const size_t p, double** x, int** fx, const size_t h, double** q, double** b, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

extern double resmdu( const size_t n, const size_t m, double** delta, const size_t p, const size_t hx, double** qx, double** bx, const size_t hy, double** qy, double** by, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double reswgtmdu( const size_t n, const size_t m, double** delta, double** w, const size_t p, const size_t hx, double** qx, double** bx, const size_t hy, double** qy, double** by, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

extern double resmduneg( const size_t n, const size_t m, double** delta, const size_t p, const size_t hx, double** qx, double** bx, const size_t hy, double** qy, double** by, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );
extern double reswgtmduneg( const size_t n, const size_t m, double** delta, double** w, const size_t p, const size_t hx, double** qx, double** bx, const size_t hy, double** qy, double** by, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

extern double external( const size_t n, const size_t m, double** delta, double** w, const size_t p, double** const fixed, double** const z, double** d, const size_t MAXITER, const double FCRIT, size_t* lastiter, double* lastdif, const bool echo );

#endif
