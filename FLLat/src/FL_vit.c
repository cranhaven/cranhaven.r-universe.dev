#include "FL_vit.h"
#include <R.h>
#include <Rdefines.h>


/*************************************************************************/
/*************************************************************************/
//SOME CONSTANTS AND UTILITY FUNCTIONS.
/*************************************************************************/
/*************************************************************************/

#define FLV_X 0
#define FLV_Y 1
#define FLV_DYL 2
#define FLV_DYR 3
#define FL_SEGSZ 4

#define FL_ENDPT_KNOT 1
#define FL_ENDPT_KNOT_FUDGE 1e-4

// Shift the Viterbi messages toward zero this often.
#define FL_VIT_SHIFT_FREQ 1000

int GetInt(SEXP p, int default_val, int* err_code) {

  if (p == R_NilValue) {
    if (err_code) *err_code = 1;
    return default_val;
  } else if (IS_INTEGER(p)) {
    return INTEGER(p)[0];
  } else if (IS_LOGICAL(p)) {
    if (LOGICAL(p)[0]) return 1;
    else return 0;
  } else if (IS_NUMERIC(p)) {
    return (int)(REAL(p)[0]);
  } else {
    if (err_code) *err_code = 2;
    return default_val;
  }
  
}

double GetNumeric(SEXP p, double default_val, int* err_code) {

  if (p == R_NilValue) {
    if (err_code) *err_code = 1;
    return default_val;
  } else if (IS_INTEGER(p)) {
    return INTEGER(p)[0];
  } else if (IS_LOGICAL(p)) {
    if (LOGICAL(p)[0]) return 1.0;
    else return 0.0;
  } else if (IS_NUMERIC(p)) {
    return REAL(p)[0];
  } else {
    if (err_code) *err_code = 2;
    return default_val;
  }
  
}


/*************************************************************************/
/*************************************************************************/
//FUSED LASSO FUNCTIONS.
/*************************************************************************/
/*************************************************************************/

double ShiftPcwsQuad(double* inp_segs, int n_inp_segs) {

  double max_y = R_NegInf;
  
  for (int k = 0; k < n_inp_segs; k++) {
    if (max_y < inp_segs[FLV_Y + FL_SEGSZ*k]) {
      max_y = inp_segs[FLV_Y + FL_SEGSZ*k];
    }
  }
  
  for (int k = 0; k < n_inp_segs; k++) {
    inp_segs[FLV_Y + FL_SEGSZ*k] -= max_y;
  }
  
  return max_y;
  
}

void AddQuadPcwsQuad(double* inp_segs, int n_inp_segs, double qd_root,
		     double qd_scale) {

  double a = 2.0*qd_scale;
  double b = -2.0 * qd_root * qd_scale;
  
  for (int i = 0; i < n_inp_segs; i++, inp_segs += FL_SEGSZ) {
    double d1 = a * inp_segs[FLV_X] + b;
    inp_segs[FLV_Y] += qd_scale * (inp_segs[FLV_X] - qd_root) * (inp_segs[FLV_X] - qd_root);
    inp_segs[FLV_DYL] += d1;
    inp_segs[FLV_DYR] += d1;
  }
  
}

int L2L1VitArgmax(double* inp_segs, int n_inp_segs, double* ret_segs,
		  int* n_ret_segs, double lambda2, double* mid_seg) {

  int n_seg = n_inp_segs;
  int n_rs = 0;
  double *rs = ret_segs;
  
  double neg_lam2 = -lambda2;
  
  // Some temporary variables used in the computations below.
  double d1, seg_width, a1, x_new, y_new, t1;
  
  int left_added = -1;
  
  mid_seg[0] = inp_segs[0];
  mid_seg[1] = inp_segs[ FL_SEGSZ*(n_seg-1) ];
  
  double *is_lst = inp_segs + ( FL_SEGSZ * (n_seg-1) );
  
  int j = 0;
  while (j < n_seg) {
    
    double *is = inp_segs + (FL_SEGSZ*j);
    
    double dyR = is[FLV_DYR];
    double dyL = is[FLV_DYL];
    double x = is[FLV_X];
    double y = is[FLV_Y];
    
    if (left_added < 0) {
      
      if (j == 0 && dyL <= lambda2) {
	
	left_added = j;
	// The derivative of lambda2 is never achieved:
	// Copy the left knot entirely.
	for (int k = 0; k < FL_SEGSZ; k++) rs[k] = is[k];
	// Increment the pointers.
	rs += FL_SEGSZ;
	n_rs++;
	
	j--;
	
      } else if ( dyR <= lambda2 || dyL <= lambda2 ) {
        // This knot has left and right tangents that contain lambda2, so
	// we do not need to add a knot.
	left_added = j;
	
#ifdef FL_ENDPT_KNOT
	if (j > 0) {
	  // Insert a knot at the left-most position.
	  
	  rs[FLV_X] = inp_segs[FLV_X]; // The x position of the leftmost knot.
	  // Compute the y position of the leftmost knot.
	  rs[FLV_Y] = y + (inp_segs[FLV_X] - x) * lambda2;
	  
	  // Set the left and right derivatives to lambda2.
          rs[FLV_DYL] = rs[FLV_DYR] = lambda2;
          
          n_rs++;
	  rs += FL_SEGSZ;
	}
#endif
	
	mid_seg[0] = rs[FLV_X] = x; // Take the x position of the current knot.
        rs[FLV_Y] = y; // Keep the y-value as well in this case.
        rs[FLV_DYR] = dyR; // Keep the right derivative.
        rs[FLV_DYL] = lambda2; // Set the left derivative to lambda2.
	
	n_rs++;
	rs += FL_SEGSZ;
        
        j--;
	
      } else if (j+1 < n_seg && lambda2 >= is[FL_SEGSZ + FLV_DYL]) {
	// If the left-derivative the next knot to the right is less than
	// lambda2, then the derivative lambda2 will be achieved somewhere
	// between.
	
	left_added = j;
        d1 = is[FL_SEGSZ + FLV_DYL] - dyR;
        seg_width = is[FL_SEGSZ+FLV_X] - x;
        
        a1 = seg_width * (lambda2 - dyR) / d1;
        mid_seg[0] = x_new = x + a1;
        
        t1 = y + dyR  * (x_new - x);
        y_new = t1 + (d1 / seg_width) * ( .5 * (x_new*x_new + x*x) - x_new * x );

#ifdef FL_ENDPT_KNOT
	// Insert a knot at the left-most position.
        rs[FLV_X] = inp_segs[FLV_X];
        rs[FLV_Y] = y_new + (inp_segs[FLV_X] - x_new) * lambda2;
        rs[FLV_DYR] = rs[FLV_DYL] = lambda2;
	
        n_rs++;
	rs += FL_SEGSZ;
#endif
        
        rs[0] = x_new; 
        rs[1] = y_new;
        rs[FLV_DYR] = rs[FLV_DYL] = lambda2;
	
        n_rs++;
	rs += FL_SEGSZ;
        
        j--;

      }
      
    } else {

      if ( dyR <= neg_lam2 ) {
	// This knot has left and right tangents that contain lambda2, so
	// we do not need to add a knot.
        
        mid_seg[1] = rs[FLV_X] = x;
        rs[FLV_Y] = y;
        rs[FLV_DYR] = neg_lam2;
        rs[FLV_DYL] = dyL;
	
	rs += 4;
        n_rs++;
	
#ifdef FL_ENDPT_KNOT
	if (j < n_seg-1) {
	  // Insert a knot at the right-most position.
          rs[FLV_X] = is_lst[FLV_X];
          rs[FLV_Y] = y + (is_lst[FLV_X] - x) * neg_lam2;
          rs[FLV_DYR] = rs[FLV_DYL] = neg_lam2;
	  
	  rs += FL_SEGSZ;
          n_rs++;
	}
#endif
	
	break;
	
      } else if (j+1 < n_seg && is[FL_SEGSZ + FLV_DYL] <= neg_lam2) {
	// If the left-derivative the next knot to the right is less than
	// -lambda2, then the derivative -lambda2 will be achieved
	// somewhere between.
	
	d1 = is[FL_SEGSZ + FLV_DYL] - dyR;
        seg_width = is[FL_SEGSZ+FLV_X] - x;
        
        a1 = seg_width * (neg_lam2 - dyR) / d1;
        x_new = x + a1;
        
        t1 = y + dyR  * (x_new - x);
        y_new = t1 + (d1 / seg_width) * ( .5 * (x_new*x_new + x*x) - x_new * x );
	
	if (left_added != j) {
	  // Copy the left knot entirely.
	  for (int k = 0; k < FL_SEGSZ; k++) rs[k] = is[k];
	  // Increment the pointers.
	  rs += FL_SEGSZ;
	  n_rs++;
	}
	
	mid_seg[1] = rs[FLV_X] = x_new;
        rs[FLV_Y] = y_new;
        rs[FLV_DYL] = rs[FLV_DYR] = neg_lam2;

	rs += FL_SEGSZ;
        n_rs++;
        
#ifdef FL_ENDPT_KNOT
        rs[FLV_X] = is_lst[FLV_X];
        rs[FLV_Y] = y_new + (is_lst[FLV_X] - x_new) * neg_lam2;
        rs[FLV_DYL] = rs[FLV_DYR] = neg_lam2;

        n_rs++;
	rs += FL_SEGSZ;
#endif

	break;

      } else if (left_added != j) {
	// This is not the last segment.  We add this knot unchanged.

	for (int k = 0; k < FL_SEGSZ; k++) rs[k] = is[k];

	rs += FL_SEGSZ;
        n_rs++;

      }

    }
    
    j++;

  }
  
  if (left_added < 0) {
    return -1; // Check concavity.
  }
  
  *n_ret_segs = n_rs;
  return 1;
  
}

void L2L1VitMsgMax(double* inp_segs, int n_inp_segs, double* x_opt,
		   double* y_opt) {

  double *is = inp_segs;
  
  for (int j = 0; j < n_inp_segs; j++, is += FL_SEGSZ) {
    
    double dyR = is[FLV_DYR];
    double dyL = is[FLV_DYL];
    double x = is[FLV_X];
    double y = is[FLV_Y];
    
    if (dyL >= 0.0 && dyR <= 0.0) {
      if (x_opt) *x_opt = x;
      if (y_opt) *y_opt = y;
      return;
    } else if (j+1 < n_inp_segs && dyR >= 0.0 && 0.0 >= is[FL_SEGSZ + FLV_DYL]) {
      double d1 = is[FL_SEGSZ + FLV_DYL] - dyR;
      double seg_width = is[FL_SEGSZ+FLV_X] - x;
      double x_new = x + seg_width * (0.0 - dyR) / d1;
      if (y_opt) {
	double t1 = y + dyR  * (x_new - x);
	*y_opt = t1 + (d1 / seg_width) * ( .5 * (x_new*x_new + x*x) - x_new * x );
      }
      if (x_opt) *x_opt = x_new;
      return;
    }
    
  }
  
  return;
  
}

int L2L1VitFwd(double lam2, double* o, double* wts, double* vit_msg1,
	       double* vit_msg2, double* back_ptrs, int* nsegs, int n_o,
	       int vit_msg_len, double obs_min, double obs_max,
	       double* last_beta) {
  
  vit_msg1[0] = obs_min - FL_ENDPT_KNOT_FUDGE;
  vit_msg1[1] = vit_msg1[2] = vit_msg1[3] = 0.0;
  vit_msg1[4] = obs_max + FL_ENDPT_KNOT_FUDGE;
  vit_msg1[5] = vit_msg1[6] = vit_msg1[7] = 0.0;

  int vit1_len = 2, vit2_len = -1;

  if (nsegs) nsegs[0] = vit1_len;

  if (R_FINITE(o[0])) {
    AddQuadPcwsQuad(vit_msg1, vit1_len, o[0], ((wts) ? -wts[0] : -1.0));
  }

  for (int i = 1; i < n_o; i++) {

    double *bp = back_ptrs + (2*i);
    int r1 = L2L1VitArgmax(vit_msg1, vit1_len, vit_msg2, &vit2_len, lam2, bp);

    if (r1 != 1) {
      return r1;
    }
    if (vit2_len + 2 > vit_msg_len) {
      return -100;
    }
    if (R_FINITE(o[i])) {
      AddQuadPcwsQuad(vit_msg2, vit2_len, o[i], ((wts) ? -wts[i] : -1.0));
    }
    // Shift the messages toward zero for numerical accuracy.
    if (i % FL_VIT_SHIFT_FREQ == 0) {
      ShiftPcwsQuad(vit_msg2, vit2_len);
    }

    double *v3 = vit_msg1;
    vit_msg1 = vit_msg2;
    vit_msg2 = v3;

    int n1 = vit1_len;
    vit1_len = vit2_len;
    vit2_len = n1;

    if (nsegs) nsegs[i] = vit1_len;

  }

  if (last_beta) {
    L2L1VitMsgMax(vit_msg1, vit1_len, last_beta, NULL);
  }

  return 1;
  
}

SEXP L2L1Vit(SEXP obsSeq, SEXP obsWts, SEXP lambda2, SEXP retPath,
	     SEXP maxSegs, SEXP nSegs, SEXP backPtrs) {
  
  int max_segs = GetInt(maxSegs, 0, 0);

  double *o = REAL(obsSeq);
  double *wts = REAL(obsWts);
  double lam2 = GetNumeric(lambda2, 0, 0);

  int n_obs = LENGTH(obsSeq);
  int n_protect = 0;

  double *back_ptrs = REAL(backPtrs);

  SEXP vit_msg1_sexp = R_NilValue;
  PROTECT(vit_msg1_sexp = NEW_NUMERIC(4*(max_segs+10))); n_protect++;
  double *vit_msg1 = REAL(vit_msg1_sexp);
  SEXP vit_msg2_sexp = R_NilValue;
  PROTECT(vit_msg2_sexp = NEW_NUMERIC(4*(max_segs+10))); n_protect++;
  double *vit_msg2 = REAL(vit_msg2_sexp);
 
  int *n_segs = INTEGER(nSegs);

  double obs_min = R_PosInf, obs_max = R_NegInf;
  for (int i = 0; i < n_obs; i++) {
    if (R_FINITE(o[i])) {
      if (o[i] < obs_min) obs_min = o[i];
      else if (o[i] > obs_max) obs_max = o[i];
    }
  }

  SEXP ret_sxp;
  PROTECT(ret_sxp = NEW_INTEGER(1)); n_protect++;

  double *rp = REAL(retPath);

  int r1 = L2L1VitFwd(lam2, o, wts, vit_msg1, vit_msg2, back_ptrs, n_segs,
		      n_obs, max_segs, obs_min, obs_max, (rp + (n_obs-1)));
  
  if (r1 != 1) {
    INTEGER(ret_sxp)[0] = r1;
    UNPROTECT(n_protect);
    return ret_sxp;
  }

  for (int i = n_obs-2; i >= 0; i--) {
    double *bp = back_ptrs + (2*(i+1));
    if (rp[i+1] > bp[1]) {
      rp[i] = bp[1];
    } else if (rp[i+1] < bp[0]) {
      rp[i] = bp[0];
    } else {
      rp[i] = rp[i+1];
    }
  }

  INTEGER(ret_sxp)[0] = 1;
  UNPROTECT(n_protect);
  return ret_sxp;
  
}

SEXP L2L1VitPath(SEXP obsSeq, SEXP lambda2, SEXP retPath, SEXP maxSegs,
		 SEXP segmentVec) {
  
  int segmented_ret = (segmentVec != R_NilValue) ? 1 : 0;

  int max_segs = GetInt(maxSegs, 0, 0);

  double *all_obs = REAL(obsSeq);

  int n_obs = LENGTH(obsSeq);
  int n_protect = 0;

  SEXP back_ptrs_sexp = R_NilValue;
  PROTECT(back_ptrs_sexp = NEW_NUMERIC(2*n_obs)); n_protect++;
  double *back_ptrs = REAL(back_ptrs_sexp);
  int *fused_segs1 = NULL;
  int *fused_segs2 = NULL;

  double *o2 = NULL, *wts2 = NULL, *o3 = NULL, *wts3 = NULL;

  SEXP vit_msg1_sexp = R_NilValue;
  PROTECT(vit_msg1_sexp = NEW_NUMERIC(4*(max_segs+10))); n_protect++;
  double *vit_msg1 = REAL(vit_msg1_sexp);
  SEXP vit_msg2_sexp = R_NilValue;
  PROTECT(vit_msg2_sexp = NEW_NUMERIC(4*(max_segs+10))); n_protect++;
  double *vit_msg2 = REAL(vit_msg2_sexp);
 
  SEXP ret_sxp;
  PROTECT(ret_sxp = NEW_INTEGER(1)); n_protect++;

  double obs_min = R_PosInf, obs_max = R_NegInf;
  for (int i = 0; i < n_obs; i++) {

    if (R_FINITE(all_obs[i])) {
      if (all_obs[i] < obs_min) obs_min = all_obs[i];
      if (all_obs[i] > obs_max) obs_max = all_obs[i];
    }

  }

  int n_lam2 = LENGTH(lambda2);
  int n_o = n_obs;
  double *o = all_obs;

  double *wts = NULL;

  for (int lam2i = 0; lam2i < n_lam2; lam2i++) {

    double lam2 = REAL(lambda2)[lam2i];

    double beta_hat = 0.0;

    int r1 = L2L1VitFwd(lam2, o, wts, vit_msg1, vit_msg2, back_ptrs, NULL,
			n_o, max_segs, obs_min, obs_max, &beta_hat);

    if (r1 != 1) {
      INTEGER(ret_sxp)[0] = r1;
      UNPROTECT(n_protect);
      return ret_sxp;
    }

    int *fs = fused_segs1;

    int nfsd2 = 0;
    if (o2 == NULL || segmented_ret) {
      // We haven't allocated the buffers for the fused observations yet.
      double btht = beta_hat;

      if (n_o == 1) {
	nfsd2 = 1;
      } else {
	for (int i = n_o-2; i >= 0; i--) {
	  double *bp = back_ptrs + (2*(i+1));

	  if (btht > bp[1]) {
	    btht = bp[1];
	    nfsd2++;
	  } else if (btht < bp[0]) {
	    btht = bp[0];
	    nfsd2++;
	  }

	  if (i == 0) {
	    nfsd2++;
	  }
	}
      }

      SEXP o2_sexp = R_NilValue;
      PROTECT(o2_sexp = NEW_NUMERIC(nfsd2)); n_protect++;
      o2 = REAL(o2_sexp);
      SEXP wts2_sexp = R_NilValue;
      PROTECT(wts2_sexp = NEW_NUMERIC(nfsd2)); n_protect++;
      wts2 = REAL(wts2_sexp);

      SEXP fused_segs1_sexp = R_NilValue;
      PROTECT(fused_segs1_sexp = NEW_INTEGER(2*(nfsd2+1))); n_protect++;
      fused_segs1 = INTEGER(fused_segs1_sexp);
      SEXP fused_segs2_sexp = R_NilValue;
      PROTECT(fused_segs2_sexp = NEW_INTEGER(2*(nfsd2+1))); n_protect++;
      fused_segs2 = INTEGER(fused_segs2_sexp);
    }

    double *fit_v = NULL;

    if (segmented_ret) {
      SEXP tmp_sxp;
      PROTECT(tmp_sxp = NEW_NUMERIC(nfsd2));
      SET_VECTOR_ELT(retPath, lam2i, tmp_sxp);
      UNPROTECT(1);

      fit_v = REAL(VECTOR_ELT(retPath, lam2i));
    } else {
      fit_v = REAL(retPath) + n_obs * lam2i;
    }

    int seg_R = (fs) ? fs[0] : (n_obs-1);
    int seg_L = (fs) ? fs[1] : (n_obs-1);

    int n_fused2 = 0;
    fused_segs2[0] = seg_R;

    if (fs) fs += 2;

    if (segmented_ret) {
      fit_v[(nfsd2-1) - n_fused2] = beta_hat;
    } else {
      for (int k = seg_L; k <= seg_R; k++) {
	fit_v[k] = beta_hat;
      }
    }

    if ( !R_FINITE(o[n_o-1]) ) {
      o2[n_fused2] = wts2[n_fused2] = 0;
    } else if (wts) {
      o2[n_fused2] = o[n_o-1]*wts[n_o-1];
      wts2[n_fused2] = wts[n_o-1];
    } else {
      o2[n_fused2] = o[n_o-1];
      wts2[n_fused2] = 1.0;
    }

    if (n_o == 1) {
      n_fused2 = 1;
      fused_segs2[0] = n_obs - 1;
      fused_segs2[1] = 0;

      o2[0] = o[0] * wts[0];
      wts2[0] = wts[0];
    }

    for (int i = n_o-2; i >= 0; i--) {
      
      seg_R = (fs) ? fs[0] : i;
      seg_L = (fs) ? fs[1] : i;

      double *bp = back_ptrs + (2*(i+1));

      if (beta_hat > bp[1]) {
	beta_hat = bp[1];

	fused_segs2[2*n_fused2 + 1] = seg_R+1;
	n_fused2++;

	o2[n_fused2] = wts2[n_fused2] = 0.0;
	fused_segs2[2*n_fused2] = seg_R;
      } else if (beta_hat < bp[0]) {
	beta_hat = bp[0];

	fused_segs2[2*n_fused2 + 1] = seg_R+1;
	n_fused2++;

	o2[n_fused2] = wts2[n_fused2] = 0.0;
	fused_segs2[2*n_fused2] = seg_R;
      }

      if (R_FINITE(o[i])) {
	if (wts) {
	  o2[n_fused2] += o[i]*wts[i];
	  wts2[n_fused2] += wts[i];
	} else {
	  o2[n_fused2] += o[i];
	  wts2[n_fused2] += 1.0;
	}
      }

      if (segmented_ret) {
	fit_v[(nfsd2-1) - n_fused2] = beta_hat;
      } else {
	for (int k = seg_L; k <= seg_R; k++) {
	  fit_v[k] = beta_hat;
	}
      }

      if (i == 0) {
	fused_segs2[2*n_fused2 + 1] = seg_L;
	n_fused2++;
      }

      if(fs) fs += 2;

    }

    obs_min = R_PosInf;
    obs_max = R_NegInf;

    if (o3 == NULL) {
      SEXP o3_sexp = R_NilValue;
      PROTECT(o3_sexp = NEW_NUMERIC(n_fused2)); n_protect++;
      o3 = REAL(o3_sexp);
      SEXP wts3_sexp = R_NilValue;
      PROTECT(wts3_sexp = NEW_NUMERIC(n_fused2)); n_protect++;
      wts3 = REAL(wts3_sexp);
    }

    for (int i = 0; i < n_fused2; i++) {
      if ( wts2[n_fused2-1-i] > 0.0 ) {
	double z = o2[n_fused2-1-i] / wts2[n_fused2-1-i];
	if(z < obs_min) obs_min = z;
	if(z > obs_max) obs_max = z;

	o3[i] = z;
      } else {
	o3[i] = NA_REAL;
      }
      wts3[i] = wts2[n_fused2-1-i];
    }

    if (n_o == 1) {
      obs_max = obs_min + FL_ENDPT_KNOT_FUDGE;
      obs_min -= FL_ENDPT_KNOT_FUDGE;
    }

    if (segmented_ret) {
      SEXP tmp_sxp, seg_dim;
      
      PROTECT(tmp_sxp = NEW_INTEGER(2*nfsd2));

      PROTECT(seg_dim=NEW_INTEGER(2));
      INTEGER(seg_dim)[0] = 2;
      INTEGER(seg_dim)[1] = nfsd2;

      SET_DIM(tmp_sxp,seg_dim);

      SET_VECTOR_ELT(segmentVec, lam2i, tmp_sxp);
      UNPROTECT(2);

      int *seg_v = INTEGER(VECTOR_ELT(segmentVec, lam2i));
      for (int k = 0; k < nfsd2; k++) {
	seg_v[1+2*k] = fused_segs2[(nfsd2-1-k)*2]+1;
	seg_v[2*k] = fused_segs2[1+(nfsd2-1-k)*2]+1;
      }
    }

    o = o3;
    wts = wts3;

    fs = fused_segs2;
    fused_segs2 = fused_segs1;
    fused_segs1 = fs;

    n_o = n_fused2;
    
  }

  INTEGER(ret_sxp)[0]  = 1;
  UNPROTECT(n_protect);
  return ret_sxp;
  
}
