#ifndef FL_VIT_H_
#define FL_VIT_H_


#include <Rinternals.h>


/*************************************************************************/
/*************************************************************************/
//UTILITY FUNCTIONS.
/*************************************************************************/
/*************************************************************************/

int GetInt(SEXP p, int default_val, int* err_code);

double GetNumeric(SEXP p, double default_val, int* err_code);


/*************************************************************************/
/*************************************************************************/
//FUSED LASSO FUNCTIONS.
/*************************************************************************/
/*************************************************************************/

double ShiftPcwsQuad(double* inp_segs, int n_inp_segs);

void AddQuadPcwsQuad(double* inp_segs, int n_inp_segs, double qd_root,
		     double qd_scale);

int L2L1VitArgmax(double* inp_segs, int n_inp_segs, double* ret_segs,
		  int* n_ret_segs, double lambda2, double* mid_seg);

void L2L1VitMsgMax(double* inp_segs, int n_inp_segs, double* x_opt,
		   double* y_opt);

int L2L1VitFwd(double lam2, double* o, double* wts, double* vit_msg1,
	       double* vit_msg2, double* back_ptrs, int* nsegs, int n_o,
	       int vit_msg_len, double obs_min, double obs_max,
	       double* last_beta);

SEXP L2L1Vit(SEXP obsSeq, SEXP obsWts, SEXP lambda2, SEXP retPath,
	     SEXP maxSegs, SEXP nSegs, SEXP backPtrs);

SEXP L2L1VitPath(SEXP obsSeq, SEXP lambda2, SEXP retPath, SEXP maxSegs,
		 SEXP segmentVec);


#endif
