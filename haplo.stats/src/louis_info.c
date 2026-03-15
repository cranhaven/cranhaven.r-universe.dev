/* $Author: schaid $ */
/* $Date: 2007/02/27 20:18:43 $ */
/* $Header: /projects/genetics/cvs/cvsroot/haplo.stats/src/louis_info.c,v 1.8 2007/02/27 20:18:43 schaid Exp $ */
/* $Locker:  $ */
/* 
 * $Log: louis_info.c,v $
 * Revision 1.8  2007/02/27 20:18:43  schaid
 * changed long to int
 *
 * Revision 1.7  2004/02/02 17:08:41  schaid
 * removed some dead code
 *
 * Revision 1.6  2004/01/23 15:11:54  schaid
 * added parens for explicit precedence
 *
 * Revision 1.5  2003/10/09 16:55:56  schaid
 * added errmsg function
 *
 * Revision 1.4  2003/10/09 14:00:16  sinnwell
 * fix $Log keyword commenting
 *
 * Revision 1.3  2003/10/07 21:30:17  schaid
 * changed from Salloc to Calloc and Free (trying to be R compatible)
 *
 * Revision 1.2  2003/10/02 21:52:21  sinnwell
 * included S2R.h for portability to R.
 *
 * Revision 1.1  2003/09/16 16:02:31  schaid
 * Initial revision
 * */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <R.h>
#include <R_ext/Error.h>

static double **double_vec_to_mat(double *Yvec, int nrow, int ncol);
static double **double_matrix(int nrow, int ncol);
static double *double_vec(int n);
static void errmsg(char *string);

/* Louis Information matrix for GLM regression coefficients and 
   estimated haplotype frequencies by EM algorithm                */


void louis_info(
     int *len_tot,      /* total length of: indx_subj, resid, vfunc, wt, h1, h2 */
     int *indx_subj,    /* vec of subj indices                                  */
     double *resid,      /* vec of standardized residuals                        */
     double *vfunc,      /* vec of var function                                  */
     double *wt,         /* vec of weights                                       */
     double *xvec,       /* vec of xmatrix, col-major order (len=len_tot * ncov) */
     int *ncov,         /* number of glm covariates                             */
     int *h1,           /* vec of haplo index code for subjects' first haplo    */
     int *h2,           /* vec of haplo index code for subjects' second haplo   */
     int *hap_base,     /* scalar baseline haplotype index                      */
     int *nhap,         /* number of haplotypes (including base) = len hap_freq */
     double *hap_freq,   /* vec of haplo frequencies                             */
     double *info11,     /* infor matrix of GLM coefs, as vector col-major       */
     double *info12,     /* info matrix of GLM coef, hap freq, in vec col-major  */
     double *info22)    /* info matrix of hap freqs, in vec col-major           */
{


  double **amat, **bmat, **cmat, **abmat, **x;
  double *tempvec1, *tempvec2;

  double t1, t2, t3, t4, baseFrqSqr;
  int i, r, r2, c, c1, c2, h, indx_start, indx_end, subj_id, nh, size_max;

  /* set up working vectors and matrices */

  nh = *nhap - 1;

  size_max = nh > *ncov ? nh : *ncov;
  tempvec1 = double_vec(size_max);
  tempvec2 = double_vec(size_max);
  amat =  double_matrix(size_max, size_max);
  bmat =  double_matrix(size_max, size_max);
  abmat = double_matrix(size_max, size_max);
  cmat =  double_matrix(size_max, size_max);

  /* design matrix, converted from S vec to matrix */

  x = double_vec_to_mat(xvec, *len_tot, *ncov);

  /********************  compute info 11 *********************************************/

  /* zero-out working arrays */
  for(r=0;r<*ncov;r++){
    for(c=0;c<*ncov;c++){
      abmat[r][c] = 0.0;
      cmat[r][c] = 0.0;
    }
  }


  indx_start = 0;
  indx_end = *len_tot;

  while (indx_start < indx_end) {
  
    i = indx_start;
    subj_id = indx_subj[i];
 
    /* zero-out working vec */
   for(r=0;r< *ncov;r++)
      tempvec1[r] = 0.0;

   /* loop for each subject  */
    do {
      t1 = vfunc[i] - resid[i]*resid[i];
      t2 = wt[i] * resid[i];
  
      for(r=0;r< *ncov; r++){
        for(c=0;c< *ncov; c++){
          abmat[r][c]+= t1*wt[i] * x[i][r] * x[i][c]; 
	}

       tempvec1[r]+= t2*x[i][r];	
      }

  
      i++;
     } while ( (i<indx_end) && (indx_subj[i]==subj_id) );

    indx_start = i;
 
      for(r=0;r< *ncov; r++){
        for(c=0;c< *ncov;c++){
         cmat[r][c]+=tempvec1[r]*tempvec1[c]; 
	}
      }
  
  }

  i= -1;
  for(c=0;c< *ncov;c++){
    for(r=0;r< *ncov;r++){
      i++;
      info11[i] = abmat[r][c] + cmat[r][c];
    }
  }

  /********************  compute info 12 *********************************************/

  /* zero-out working arrays */
  for(r=0;r<*ncov;r++){
    for(c=0;c<nh;c++){
      bmat[r][c] = 0.0;
      cmat[r][c] = 0.0;
    }
  }


  indx_start = 0;
  indx_end = *len_tot;

  while (indx_start < indx_end) {
  
    i = indx_start;
    subj_id = indx_subj[i];

    /* zero-out working vec */
   for(r=0;r< *ncov;r++)
      tempvec1[r] = 0.0;

   for(c=0;c<nh;c++)
     tempvec2[c] = 0.0;


   /* loop for each subject  */
   do {
      t1 = wt[i] * resid[i];
  
      for(r=0;r<*ncov;r++){
        tempvec1[r] += t1 * x[i][r]; 
      }


      /* instead of looping over all possible haplotypes, we  use h1,h2
         to directly index info matrix,  because many h1,h2 will have 
         derivg = 0, and second deriv = 0, and hence contribute 0 to amat, bmat. 
         To do this, we need to account for exclusion of haplo_base from info matrix.
         Since h1, h2 are indices 0,...,(nhap-1), and base can occur in this
         sequence, the array index will be h1 if h1 < base, or (h1-1) if h1 > base.  */

      if( (h1[i]!= (*hap_base)) && (h2[i]!=(*hap_base) ) ){            /* both haps != base */

        if(h1[i]==h2[i]){
          c = h1[i] < *hap_base ? h1[i] : (h1[i]-1);
          t2 = 2.0/hap_freq[h1[i]];
          tempvec2[c] += wt[i] * t2;
          for(r=0;r<*ncov;r++){
            bmat[r][c] += t1 * x[i][r] * t2; 
	  }
	}
        else {
          c1 = h1[i] < *hap_base ? h1[i] : (h1[i]-1);
          t2 = 1.0/hap_freq[h1[i]];
          tempvec2[c1] += wt[i] * t2;

          c2 = h2[i] < *hap_base ? h2[i] : (h2[i]-1);
          t3 = 1.0/hap_freq[h2[i]];
          tempvec2[c2] += wt[i] * t3;

          for(r=0;r<*ncov;r++){
            bmat[r][c1] += t1 * x[i][r] * t2;
            bmat[r][c2] += t1 * x[i][r] * t3;
	  }
	}
      }

      else if ( (h1[i]== (*hap_base)) && (h2[i]==(*hap_base)) ){      /* both haps = base */
        t2 = -2.0/hap_freq[*hap_base];
	for(c=0;c<nh;c++){
	  tempvec2[c] += wt[i] * t2;
          for(r=0;r<*ncov;r++){
            bmat[r][c] += t1 *x[i][r] * t2;
	  }
	}
      }

      else {                                             /* only 1 hap = base */
        h = h1[i]== *hap_base ? h2[i] : h1[i];
        c2 = h < *hap_base ? h : (h-1);
        t2 =  1.0/hap_freq[h];
        t3 = -1.0/hap_freq[*hap_base];
	for(c=0;c<nh;c++){
          t4 = c==c2 ? (t2+t3) : t3;
	  tempvec2[c] += wt[i] * t4;
         for(r=0;r<*ncov;r++){
            bmat[r][c] += t1 *x[i][r] * t4;
	  }
	}
       
      }

      i++;
     } while ( (i<indx_end) && (indx_subj[i]==subj_id) );

   indx_start = i;
 
      for(r=0;r< *ncov; r++){
        for(c=0;c<nh;c++){
          cmat[r][c]+= tempvec1[r]*tempvec2[c];
	}
      }
  
  }

  i= -1;
  for(c=0;c<nh;c++){
    for(r=0;r<*ncov;r++){
      i++;
      info12[i] = cmat[r][c] - bmat[r][c];
    }
  }



  /********************  compute info 22 *********************************************/

  /* zero-out working arrays */
  for(r=0;r<nh;r++){
    for(c=0;c<nh;c++){
      amat[r][c] = 0.0;
      bmat[r][c] = 0.0;
      cmat[r][c] = 0.0;
    }
  }


  indx_start = 0;
  indx_end = *len_tot;

  while (indx_start < indx_end) {
  
    i = indx_start;
    subj_id = indx_subj[i];

  /* zero-out working vec */
   for(r=0;r< nh;r++)
      tempvec1[r] = 0.0;


   baseFrqSqr = hap_freq[*hap_base] * hap_freq[*hap_base];

   /* loop for each subject  */
   do {

     if(h1[i] < 0 || h2[i] < 0){                         /* skip if either haplo 'missing' */    
       i++;
        continue; 
     }

     if((h1[i]!=*hap_base) && (h2[i]!=*hap_base) ){            /* both haps != base */
    
       if(h1[i]==h2[i]){                                  /* homozygous */
         r = h1[i] < *hap_base ? h1[i] : (h1[i] - 1);
         tempvec1[r] += wt[i] * 2.0/hap_freq[h1[i]];   
         amat[r][r]  += wt[i] * 2.0/(hap_freq[h1[i]]*hap_freq[h1[i]]);
         bmat[r][r]  += wt[i] * 4.0/(hap_freq[h1[i]]*hap_freq[h1[i]]); 

        } 
      else 
        {                                                /* heterozygous */
	  r = h1[i] < *hap_base ? h1[i] : (h1[i]-1);    /* r,c = row, col index */
	  c = h2[i] < *hap_base ? h2[i] : (h2[i]-1);  
	     
          tempvec1[r] += wt[i] * 1.0/hap_freq[h1[i]];
          tempvec1[c] += wt[i] * 1.0/hap_freq[h2[i]];

	  amat[r][r] += wt[i] * 1.0/(hap_freq[h1[i]]*hap_freq[h1[i]]);
	  amat[c][c] += wt[i] * 1.0/(hap_freq[h2[i]]*hap_freq[h2[i]]);

          bmat[r][r] += wt[i] *  1.0/(hap_freq[h1[i]]*hap_freq[h1[i]]);
          bmat[r][c] += wt[i] *  1.0/(hap_freq[h1[i]]*hap_freq[h2[i]]);
          bmat[c][c] += wt[i] *  1.0/(hap_freq[h2[i]]*hap_freq[h2[i]]);
          bmat[c][r] += wt[i] *  1.0/(hap_freq[h1[i]]*hap_freq[h2[i]]);
        
        }
     }
     else if ( (h1[i]==(*hap_base)) && ((h2[i]==*hap_base))) {     /* both haps = base */
       t1 = wt[i] * (-2.0/hap_freq[*hap_base]) ;
       t2 = wt[i] * (2.0/hap_freq[*hap_base]) * (2.0/hap_freq[*hap_base]);
       t3 = wt[i] * 2.0/baseFrqSqr;
       for(r=0;r<nh;r++){
         tempvec1[r] += t1;
	 for(c=0;c<nh;c++){
           amat[r][c] += t3 ;
           bmat[r][c] += t2;
	 }
       }
     }
     else {                                               /* only 1 hap = base */

       h = h1[i]== *hap_base ? h2[i] : h1[i];
       r2 = h < *hap_base ? h : (h-1);

       t1 = -1.0/hap_freq[*hap_base];
       t2 =  t1 + 1.0/hap_freq[h];


       t3 = 1.0/baseFrqSqr;
       t4 = t3 + 1.0/(hap_freq[h]*hap_freq[h]);

       for(r=0;r<nh;r++){
          tempvec1[r] += wt[i] * (r==r2 ? t2 : t1);
         for(c=0;c<nh;c++){
           amat[r][c] += wt[i] * ( ( (r==r2) && (c==r2)) ? t4 : t3) ;
           bmat[r][c] += wt[i] * (r==r2 ? t2 : t1) * (c==r2 ? t2 : t1);
	 }
       }
 
     }
 

     i++;
 
    } while ( (i<indx_end) && (indx_subj[i]==subj_id) );

   indx_start = i;
 
      for(r=0;r<nh; r++){
        for(c=0;c<nh;c++){
          cmat[r][c] += tempvec1[r]*tempvec1[c];
	}
      }

  }

 i= -1;
  for(c=0;c<nh;c++){
    for(r=0;r<nh;r++){
      i++;
      info22[i] = amat[r][c] - bmat[r][c] + cmat[r][c];
    }
  }

  R_Free(tempvec1);
  R_Free(tempvec2);

  for(i=0;i<size_max;i++) {
    R_Free(amat[i]);
    R_Free(bmat[i]);
    R_Free(abmat[i]);
    R_Free(cmat[i]); 
  }

  R_Free(amat);
  R_Free(bmat);
  R_Free(abmat);
  R_Free(cmat);
 
  for(i=0;i< (*len_tot); i++){
    R_Free(x[i]);
  }
  R_Free(x);

  return;

}




/***********************************************************************************/

static double **double_vec_to_mat(double *Yvec, int nrow, int ncol){

   int i,j,k;
   double **Y;

   Y=double_matrix(nrow,ncol);
   k=0;
   for (j=0;j<ncol;j++){
      for (i=0;i<nrow;i++){
         Y[i][j]=Yvec[k];
         k++;
      }
   }
   return Y;
}


/*********************************************************************************/

static double **double_matrix(int nrow, int ncol){
/* allocate double matrix with subscript range m[0 ..(nrow-1)][0..(ncol-1)] */
        int i;
        double **m;

        /* allocate pointers to rows */
        m=(double **) R_Calloc(nrow, double *);
        if (!m) errmsg("mem alloc failure 1 in double_matrix");
  
	/* allocate vec of memory for each row */
        for(i=0;i<nrow;i++) {
          m[i]=(double *) R_Calloc(ncol, double);
          if(!m[i]) errmsg("mem alloc failure 2 in double_matrix");
	}

        /* return pointer to array of pointers to rows */
        return m;
}

/*********************************************************************************/

static double *double_vec(int n){
/* allocate double vec with subscript range v[0 ..(nrow-1)] */
        double *v;
        v =(double *) R_Calloc(n, double);
          if(!v) errmsg("mem alloc failure in double_vec");
        return v;
}


/***********************************************************************************/
static void errmsg(char *string){

  /* Function to emulate "stop" of S+ - see page 134, S Programing, by
     Venables and Ripley */
  /* replace problem with Rf_error for 4.1.x
     PROBLEM "%s", string RECOVER(NULL_ENTRY);
  */
  Rf_error(string, "%s");
}


