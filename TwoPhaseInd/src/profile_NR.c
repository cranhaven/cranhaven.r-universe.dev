

/* this part of code is to perform the profile newton-raphson algorithm based on Dai, et. al 2008. */

# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <string.h>
# include <R.h>
# include <R_ext/Error.h>
# include <R_ext/RS.h>
# include <Rmath.h>
# include <R_ext/Lapack.h>
# include <R_ext/BLAS.h>
# include "profile_NR.h"  


void profile_NR_noind(
           long *n_subject,
           long *subj_id,
           double *y,
           double *x,
           double *z,
           long *NXYcount,
           long *extracov,
           double *covvec,
           long *n_extracov,
           double *beta,
           double *varmat,
           double *diff_factor,
           long *maxit,
           long *verbose,
           long *converged) {

  /* n_subject     the number of subjects in the 2nd phase cohort */
  /* subj_id       the vector of ids for 2nd phase subject        */
  /* y             the vector of binary outcomes                  */
  /* x             the vector of treatment variables              */
  /* z             the vector of the environment variable independent of x */
  /* NXYcount      the vector of first-phase stratum counts, 4 entries:   (N00, N10, N01, N11) , where N10 is count of x=1 y=0 in the full cohort   */
  /* extracovar    the indicator of having the extra covariate    */
  /* covvec        the vector of extra-covariates: note the order of this vector is subj_1_cov_1, subj_1_cov_2, .... subj_2_cov1,subj_2_cov2...             */
  /* n_extracov    the number of the extra covariates             */
  /* beta          the vector storing the output parameters       */
  /* varmat        the vector storing the variance matrix         */
  /* maxit         the maximal number of iteration                */
  /* diff_factor   the differentiation factor used                */
  /* verbose       the indicator of printing out                  */
  /* converged      the indicator of converge, 0:not converged      */

  double *zx0, *zx1, *z1, *x1,*pyx, *y1, **X1, **X, *pzx, *pzxdiffvec, *pyxnew,*pzxnew,*fit,*fit1,*tt,eta,*err1,*err, **infmat,**ss, *infmatvec,**infmatinv,pzxsum1, pzxsum0, *wgt1, *score, *betanew, *betavec,diff, dif,a,b,c,d,change,epsilon;
  long i,j,k,l,*Nyx, nx0, nx1, n_cov, n11=0, n00=0, n01=0, n10=0, count1=0, count2=0,count,it, itt;
  //FILE *file; 
  //file = fopen("debug.txt", "w"); 
  *verbose=2;
  *converged=0;
  epsilon =pow(10,-8);
  nx0=0;
  nx1=0;
  for (i=0;i<*n_subject;i++) { if (x[i]==1) nx1++; else nx0++; }

  zx0=double_vec(nx0);
  zx1=double_vec(nx1);
  z1=double_vec(*n_subject*2); 
  x1=double_vec(*n_subject*2);  
  y1=double_vec(*n_subject*2); 
  pzx=double_vec(*n_subject*2); 
  pzxdiffvec=double_vec(*n_subject*2);
  pyx=double_vec(*n_subject*2); 
  pyxnew=double_vec(*n_subject*2);  
  pzxnew=double_vec(*n_subject*2); 
  Nyx=long_vec(*n_subject*2);
  fit1=double_vec(*n_subject*2);
  fit=double_vec(*n_subject);
  tt=double_vec(*n_subject*2);
  if (*extracov==0) n_cov=4; else n_cov=4+ *n_extracov;
  double *varmat1=double_vec(n_cov*n_cov);
  X1=double_matrix(*n_subject*2,n_cov);
  X=double_matrix(*n_subject,n_cov); 
  wgt1=double_vec(*n_subject*3);
  score=double_vec(n_cov);
  betanew=double_vec(n_cov);
  betavec=double_vec(n_cov);
  err1=double_vec(*n_subject*2);
  err=double_vec(*n_subject);
  infmat=double_matrix(n_cov,n_cov);
  ss=double_matrix(2,n_cov);
  infmatvec=double_vec(n_cov*(n_cov));
  infmatinv=double_matrix(n_cov,n_cov);

  for (i=0;i<*n_subject;i++) {
    if ((x[i]==0.0) & (y[i]==0.0)) n00 ++;
    if ((x[i]==1.0) & (y[i]==0.0)) n10 ++;
    if ((x[i]==0.0) & (y[i]==1.0)) n01 ++;
    if ((x[i]==1.0) & (y[i]==1.0)) n11 ++;

    if (x[i] ==0) {
      zx0[count1] = z[i];
      count1++; 
    } else {
      zx1[count2] = z[i];
      count2++;
    } 
  }



  for (i=0;i<*n_subject*2;i++) {     
    if (i<count2) {
            z1[i] = zx1[i]; 
            x1[i] = 1;
            y1[i] = 1; 
      
    } else {
      if (i<*n_subject) {
         z1[i]= zx0[i-count2]; 
         x1[i]= 0;
         y1[i]= 1;
      
      } else {
	if (i < (*n_subject+count2)) {
              z1[i] = zx1[i-*n_subject] ; 
              x1[i] = 1;
              y1[i] = 0;
      
        } else {
              z1[i] = zx0[i-*n_subject-count2]; 
              x1[i] = 0;
              y1[i] = 0;
      
        }
      }
    }
  }

 /*   if (*verbose==1) { 
       print_vector_double(z1,*n_subject*2,file);
       print_vector_double(x1,*n_subject*2,file);
       print_vector_double(y1,*n_subject*2,file);
     }
 
 if (*verbose==1) fprintf(file,"\nn11=%i\n\n", n11); 
 if (*verbose==1) fprintf(file,"\nn10=%i\n\n", n10); 
 if (*verbose==1) fprintf(file,"\nn01=%i\n\n", n01); 
 if (*verbose==1) fprintf(file,"\nn00=%i\n\n", n00);
 */
  count=0;
  for (i=0;i<*n_subject*2;i++) { 
    if ((x1[i]==0.0) & (y1[i]==0.0)) {
                 Nyx[i]=NXYcount[0]-n00;
                 pyx[i]=(1.0*NXYcount[0])/(NXYcount[0] +NXYcount[2]);
           
                 
    } 
    if ((x1[i]==1.0) & (y1[i]==0.0)) {
                 Nyx[i]=NXYcount[1]-n10;
                 pyx[i]=(1.0*NXYcount[1])/(NXYcount[1] +NXYcount[3]);
           
    } 
    if ((x1[i]==0.0) & (y1[i]==1.0)) {
                 Nyx[i]=NXYcount[2]-n01;
                 pyx[i]=(1.0*NXYcount[2])/(NXYcount[0] +NXYcount[2]);
           
    } 
    if ((x1[i]==1.0) & (y1[i]==1.0)) {
                 Nyx[i]=NXYcount[3]-n11;
                 pyx[i]=(1.0*NXYcount[3])/(NXYcount[1] +NXYcount[3]);
           
    } 

    X1[i][0] = 1;
    X1[i][1] = x1[i];
    X1[i][2] = z1[i];
    X1[i][3] = x1[i]*z1[i];

    if (*extracov==1) {
      for (j=4;j<n_cov;j++) {
	X1[i][j] = covvec[count];
        count++;
      }
      if (count== (*n_subject*(*n_extracov))) count=0;
    }

  }

/*   if (*verbose==1) { 
       print_vector_long(Nyx,*n_subject*2,file);
       print_vector_double(pyx,*n_subject*2,file);
     }
*/


  count=0; 
  for (i=0;i<*n_subject;i++) { 
    X[i][0] = 1;
    X[i][1] = x[i];
    X[i][2] = z[i];
    X[i][3] = x[i]*z[i];

    if (*extracov==1) {
      for (j=4;j<n_cov;j++) {
	X[i][j] = covvec[count];
        count++;
      }
    }
  }

/*
     if (*verbose==1) { 
       print_matrix_double(X1,*n_subject*2,n_cov,file);
       print_matrix_double(X,*n_subject,n_cov,file);
     }
*/ 

   double cutoff1 = pow(10,-6);
   double cutoff2 = pow(10,-11);
   double error;
  
  

  /* now start to update covariate distribution in each cycle */

    diff=1.0;
    itt=0;
    while (diff > cutoff1 && itt < *maxit) {
      itt++;
      //fprintf(file,"\nUpdate covariate: the full iteration number =%i\n\n", itt);    
      for (i=0;i < *n_subject*2;i++) {
	    eta =0.0;
        for (j=0;j<n_cov;j++) eta += beta[j]*X1[i][j];
        fit1[i] = exp(eta)/(1+exp(eta));
        err1[i] = y1[i]-fit1[i];
	    if (y1[i]==0) fit1[i]=1-fit1[i]; 
      }

      for (i=0;i < *n_subject;i++) {
	    eta =0.0;
        for (j=0;j<n_cov;j++) eta += beta[j]*X[i][j];
        fit[i] = exp(eta)/(1+exp(eta));
        err[i] = y[i] - fit[i];
	    if (y[i]==0) fit[i]=1-fit[i]; 
      }
      error=0;
      for (i=0;i< *n_subject;i++) {
      	error +=fabs(err[i]);
      }
      //fprintf(file,"The error is %.10e\n",error);
      // print_vector_double(err,*n_subject,file);
      //print_vector_double(fit,*n_subject,file);
      
      pzxsum0=0.0;
      pzxsum1=0.0;
      for (i=0;i < *n_subject*2;i++) {
        if (i <*n_subject) tt[i] = Nyx[i]*fit1[i]/pyx[i] + Nyx[i+*n_subject]*fit1[i+*n_subject]/pyx[i+*n_subject];
	    else tt[i] = Nyx[i]*fit1[i]/pyx[i] + Nyx[i-*n_subject]*fit1[i-*n_subject]/pyx[i-*n_subject];
        if (x1[i]==0) {
           pzx[i] = 1/(NXYcount[0]+NXYcount[2]-tt[i]); 
           pzxsum0 += pzx[i];
        } else {
           pzx[i] = 1/(NXYcount[1]+NXYcount[3]-tt[i]);
           pzxsum1 += pzx[i];
        }
      }

      for (i=0;i < *n_subject*2;i++) {
	  if (x1[i]==0)  pzx[i]= pzx[i]*2/pzxsum0;  else  pzx[i]= pzx[i]*2/pzxsum1;
      }


//      if (*verbose==1) print_vector_double(pzx,*n_subject*2,file); 
  
      it=0;
      dif=1;

      while (dif>cutoff2 && it < *maxit) {
	    it++;
//        if (*verbose==1)   fprintf(file,"\nthe EM iteration number =%i\n\n", it);
        for (i=0;i<*n_subject*3;i++) {
	      if (i<*n_subject) wgt1[i]=1.0; 
          else  wgt1[i]=Nyx[i-*n_subject]*fit1[i-*n_subject]*pzx[i-*n_subject]/pyx[i-*n_subject];
        }
        pzxsum0=0.0;
        pzxsum1=0.0; 
        for (i=0;i<*n_subject*2;i++) {
	      if (i<*n_subject) pzxnew[i] = 1.0 + wgt1[i+*n_subject] + wgt1[i+*n_subject*2];              
          else  pzxnew[i] = wgt1[i] + wgt1[i+*n_subject] + 1.0;

          if (x1[i]==0) pzxsum0 += pzxnew[i]; else pzxsum1 += pzxnew[i];          
        }


        for (i=0;i < *n_subject*2;i++) {
	      if (x1[i]==0)  pzxnew[i]= pzxnew[i]*2/pzxsum0;  else  pzxnew[i]= pzxnew[i]*2/pzxsum1;
        }

        a=0.0;
        b=0.0;
        c=0.0;
        d=0.0;

        for (i=0;i<*n_subject*2;i++) {
          if ((x1[i]==0) & (y1[i]==0)) a += fit1[i]*pzxnew[i];
          if ((x1[i]==1) & (y1[i]==0)) b += fit1[i]*pzxnew[i];
          if ((x1[i]==0) & (y1[i]==1)) c += fit1[i]*pzxnew[i];
          if ((x1[i]==1) & (y1[i]==1)) d += fit1[i]*pzxnew[i];
        }

        for (i=0;i<*n_subject*2;i++) {
          if ((x1[i]==0) & (y1[i]==0)) pyx[i]= a; 
          if ((x1[i]==1) & (y1[i]==0)) pyx[i]= b; 
          if ((x1[i]==0) & (y1[i]==1)) pyx[i]= c; 
          if ((x1[i]==1) & (y1[i]==1)) pyx[i]= d; 
        }


//        if (*verbose==1) print_vector_double(pzx,*n_subject*2,file); 
//        if (*verbose==1) print_vector_double(pyx,*n_subject*2,file); 
        
        for (i=0;i<*n_subject*2;i++) {
	      pzxdiffvec[i]=fabs(pzx[i]-pzxnew[i]);
	      if (i==0) dif=pzxdiffvec[i];
          else if (dif<pzxdiffvec[i]) dif=pzxdiffvec[i];
          pzx[i]=pzxnew[i];
        }
//        if (*verbose==1) print_vector_double(pzxdiffvec,*n_subject*2,file);
        //if (it % 10 ==0) fprintf(file,"\n%d: dif1= %.10e",it,dif);
      }


      for   (j=0;j<n_cov;j++) score[j]=0.0;     
      for (i=0;i<*n_subject*3;i++) {
	    if (i<*n_subject) {
            wgt1[i]=1; 
            for (j=0;j<n_cov;j++) score[j]+= X[i][j]*wgt1[i]*err[i];
        } else  {
            wgt1[i]=Nyx[i-*n_subject]*fit1[i-*n_subject]*pzx[i-*n_subject]/pyx[i-*n_subject];
            for (j=0;j<n_cov;j++) score[j]+= X1[i-*n_subject][j]*wgt1[i]*err1[i-*n_subject]; 
        }
      }

//      if (*verbose==1) print_vector_double(wgt1,*n_subject*3,file);
//      if (*verbose==1) print_vector_double(score,n_cov,file);

      /* now start numerical differentiation */


      for (i=0;i<n_cov;i++) {
	    for (j=0;j<2;j++) {
	      for (k=0;k<n_cov;k++) ss[j][k]=0.0;
        }
        for (j=0;j<2;j++) { 
          d=pow(-1,j+1)*(*diff_factor); 
          for (k=0;k<n_cov;k++) {
	        if (k!=i) betanew[k]=beta[k]; else betanew[k]=beta[k]+d;
          }
          for (k=0;k < *n_subject*2;k++) {
	        eta =0.0;
            for (l=0;l<n_cov;l++) eta += betanew[l]*X1[k][l];
            fit1[k] = exp(eta)/(1+exp(eta));
            err1[k] = y1[k]-fit1[k];
	        if (y1[k]==0) fit1[k]=1-fit1[k]; 
          }

          for (k=0;k < *n_subject;k++) {
	        eta =0.0;
            for (l=0;l<n_cov;l++) eta += betanew[l]*X[k][l];
            fit[k] = exp(eta)/(1+exp(eta));
            err[k] = y[k] - fit[k];
	        if (y[k]==0) fit[k]=1-fit[k]; 
          }
 
          pzxsum1=0.0;
          pzxsum0=0.0;
          for (k=0;k < *n_subject*2;k++) {
            if (k <*n_subject) tt[k] = Nyx[k]*fit1[k]/pyx[k] + Nyx[k+*n_subject]*fit1[k+*n_subject]/pyx[k+*n_subject];
	        else tt[k] = Nyx[k]*fit1[k]/pyx[k] + Nyx[k-*n_subject]*fit1[k-*n_subject]/pyx[k-*n_subject];
              if (x1[k]==0) {
                 pzx[k] = 1/(NXYcount[0]+NXYcount[2]-tt[k]); 
                 pzxsum0 += pzx[k];
              } else {
                 pzx[k] = 1/(NXYcount[1]+NXYcount[3]-tt[k]);
                 pzxsum1 += pzx[k];
              }
          }
          for (k=0;k < *n_subject*2;k++) {
	         if (x1[k]==0)  pzx[k]= pzx[k]*2/pzxsum0;  else  pzx[k]= pzx[k]*2/pzxsum1;
          }

            
          it=0;
          dif=1.0;
 
          for (k=0;k<*n_subject*2;k++) pyxnew[k]=pyx[k];
           
          while (dif> cutoff2 && it < *maxit) {
	        it++;
//            if (*verbose==1) fprintf(file,"\nthe EM iteration number =%i\n\n", it);
            for (k=0;k<*n_subject*3;k++) {
	           if (k<*n_subject) wgt1[k]=1; 
               else  wgt1[k]=Nyx[k-*n_subject]*fit1[k-*n_subject]*pzx[k-*n_subject]/pyxnew[k-*n_subject];
            }
            pzxsum0=0.0;
            pzxsum1=0.0;
            for (k=0;k<*n_subject*2;k++) {
	          if (k<*n_subject) pzxnew[k] = wgt1[k]+ wgt1[k+*n_subject] + wgt1[k+*n_subject*2];              
              else  pzxnew[k] = wgt1[k] + wgt1[k-*n_subject] + wgt1[k+*n_subject];
              if (x1[k]==0) pzxsum0 += pzxnew[k]; else pzxsum1 += pzxnew[k];          
            }
            a=0.0;
            b=0.0;
            c=0.0;
            d=0.0;

            for (k=0;k<*n_subject*2;k++) {
               if (x1[k]==0) pzxnew[k] = 2*pzxnew[k]/pzxsum0; else pzxnew[k]=2*pzxnew[k]/pzxsum1; 
               if ((x1[k]==0) & (y1[k]==0)) a += fit1[k]*pzxnew[k];
               if ((x1[k]==1) & (y1[k]==0)) b += fit1[k]*pzxnew[k];
               if ((x1[k]==0) & (y1[k]==1)) c += fit1[k]*pzxnew[k];
               if ((x1[k]==1) & (y1[k]==1)) d += fit1[k]*pzxnew[k];
            }

            for (k=0;k<*n_subject*2;k++) {
               if ((x1[k]==0) & (y1[k]==0)) pyxnew[k]= a; 
               if ((x1[k]==1) & (y1[k]==0)) pyxnew[k]= b; 
               if ((x1[k]==0) & (y1[k]==1)) pyxnew[k]= c; 
               if ((x1[k]==1) & (y1[k]==1)) pyxnew[k]= d; 
            }
            
            for (k=0;k<*n_subject*2;k++) {
	          pzxdiffvec[k]=fabs(pzxnew[k]-pzx[k]);
	          if (k==0) dif=pzxdiffvec[k];
              else if (dif<pzxdiffvec[k]) dif=pzxdiffvec[k];
                 pzx[k]=pzxnew[k];
             }
             //if (it % 10 ==0) fprintf(file,"\n%d :dif2= %.10e",it,dif);
          }


//          if (*verbose==1) print_vector_double(pyxnew,*n_subject*2,file);
//          if (*verbose==1) print_vector_double(pzx,*n_subject*2,file);

          for (k=0;k<*n_subject*3;k++) {
	        if (k<*n_subject) {
               wgt1[k]=1; 
               for (l=0;l<n_cov;l++) ss[j][l]+= X[k][l]*wgt1[k]*err[k];
             } else  {
                  wgt1[k]=Nyx[k-*n_subject]*fit1[k-*n_subject]*pzx[k-*n_subject]/pyxnew[k-*n_subject];
                  for (l=0;l<n_cov;l++) ss[j][l]+= X1[k-*n_subject][l]*wgt1[k]*err1[k-*n_subject]; 
             }
          }
//          if (*verbose==1) print_vector_double(wgt1,*n_subject*3,file);
	
      

	   }	
//       if (*verbose==1) print_matrix_double(ss,2,n_cov,file);
       for (j=0;j<n_cov;j++) infmat[i][j] = (ss[0][j]-ss[1][j])/(2*(*diff_factor));
        
       }
//       if (*verbose==1)  {
//	     print_matrix_double(infmat,n_cov,n_cov,file); }
      /* now update beta */
 
      count=0; 
      for (i=0;i<n_cov;i++) {
            for (j=0;j<n_cov;j++) {
                infmatvec[count]=infmat[i][j];
                count++;
	    }
      }


      //dqrinv_(infmatvec,&n_cov,&epsilon,varmat);      
      varmat1=dqrinv(infmatvec,n_cov,epsilon);       

      count=0; 
      for (i=0;i<n_cov;i++) {
            for (j=0;j<n_cov;j++) { 
                infmatinv[i][j]=varmat1[count];
                count++;
            }
      }


      for (i=0;i<n_cov;i++) {
	    change=0.0;
            for (j=0;j<n_cov;j++) {  
	      change+=infmatinv[i][j]*score[j];
            }
            betanew[i]=beta[i]+change;
      }

      
    
      //print_vector_double(beta,n_cov,file);
      //print_vector_double(betanew,n_cov,file);
      
      for (i=0;i<n_cov;i++) {
	    betavec[i]=fabs(beta[i]-betanew[i]);
	    if (i==0) diff=betavec[i];
        else if (diff<betavec[i]) diff=betavec[i];
        beta[i] = betanew[i];
      }
      //if (*verbose==2)    fprintf(file,"\nthe current diff= %8.9f",diff);
    }

    //converge indicator
    if (diff<cutoff1) *converged=1; 

for (i=0;i<n_cov*n_cov;i++){
		varmat[i]=varmat1[i];
	}
	
//   fclose(file);


}





void profile_NR_ind(
           long *n_subject,
           long *subj_id,
           double *y,
           double *x,
           double *z,
           long *NXYcount,
           long *extracov,
           double *covvec,
           long *n_extracov,
           double *beta,
           double *varmat,
           double *diff_factor,
           long *maxit,
           long *verbose,
           long *converged) {

  /* n_subject     the number of subjects in the 2nd phase cohort */
  /* subj_id       the vector of ids for 2nd phase subject        */
  /* y             the vector of binary outcomes                  */
  /* x             the vector of treatment variables              */
  /* z             the vector of the environment variable independent of x */
  /* NXYcount      the vector of first-phase stratum counts       */
  /* extracovar    the indicator of having the extra covariate    */
  /* covvec        the vector of extra-covariates                 */
  /* n_extracov    the number of the extra covariates             */
  /* beta          the vector storing the output parameters       */
  /* varmat        the vector storing the variance matrix         */
  /* maxit         the maximal number of iteration                */
  /* diff_factor   the differentiation factor used                */
  /* verbose       the indicator of printing out                  */
  /* converged      the indicator of converge, 0:not converged      */

  double *z1, *pyx, *y1, *x1, *pz, *pznew,*pzdiffvec, *pyxnew, **infmat, **ss, *fit,*fit1, *tt, **X1, **X, pzsum, *wgt1, *score, *betanew, *betavec, *err1,*err,*infmatvec,**infmatinv;
  double diff,eta, change,dif,a,b,c,d,epsilon;
  long i,j,k,l,N,n_cov,count,*Nyx, n11=0, n00=0, n01=0, n10=0,itt,it;
//  FILE *file; 
//  file = fopen("debug.txt", "w"); 
  *converged=0;
  epsilon =pow(10,-8);
  z1=double_vec(*n_subject*4); 
  x1=double_vec(*n_subject*4);  
  y1=double_vec(*n_subject*4); 
  pz=double_vec(*n_subject*4); 
  pznew=double_vec(*n_subject*4);
  pzdiffvec=double_vec(*n_subject*4);
  pyx=double_vec(*n_subject*4); 
  pyxnew=double_vec(*n_subject*4); 
  Nyx=long_vec(*n_subject*4);
  fit1=double_vec(*n_subject*4);
  fit=double_vec(*n_subject);
  tt=double_vec(*n_subject*4);
  

  if (*extracov==0) n_cov=4; else n_cov=4+ *n_extracov;
  double *varmat1=double_vec(n_cov*n_cov);
  //fprintf(file,"\nthe number of covariates=%i\n\n", n_cov);   
  X1=double_matrix(*n_subject*4,n_cov);
  X=double_matrix(*n_subject,n_cov); 
  wgt1=double_vec(*n_subject*5);
  score=double_vec(n_cov);
  betanew=double_vec(n_cov);
  betavec=double_vec(n_cov);
  err1=double_vec(*n_subject*4);
  err=double_vec(*n_subject);
  infmat=double_matrix(n_cov,n_cov);
  infmatinv=double_matrix(n_cov,n_cov);
  infmatvec=double_vec(n_cov*n_cov);
  ss=double_matrix(2,n_cov);
  
  for (i=0;i<*n_subject;i++) {
    if ((x[i]==0.0) & (y[i]==0.0)) n00 ++;
    if ((x[i]==1.0) & (y[i]==0.0)) n10 ++;
    if ((x[i]==0.0) & (y[i]==1.0)) n01 ++;
    if ((x[i]==1.0) & (y[i]==1.0)) n11 ++;
  }
  for (i=0;i<*n_subject*4;i++) {     
    if (i<*n_subject) {
            z1[i] = z[i]; 
            x1[i] = 1;
            y1[i] = 1; 
    } else {
	if (i < (*n_subject*2)) {
              z1[i] = z[i-*n_subject]; 
              x1[i] = 0;
              y1[i] = 1; 
        } else {
            if (i < (*n_subject*3)) {  
              z1[i] = z[i-*n_subject*2]; 
              x1[i] = 1;
              y1[i] = 0;
              
            } else {               
                 z1[i] = z[i-*n_subject*3]; 
                 x1[i] = 0;
                 y1[i] = 0;
            }              
         }
      }
  }
  
 



  count=0;  

  for (i=0;i<*n_subject*4;i++) { 
    if ((x1[i]==0) & (y1[i]==0)) {
                 Nyx[i]=NXYcount[0]-n00;
                 pyx[i]=(1.0*NXYcount[0])/(NXYcount[0] +NXYcount[2]);
    } 
    if ((x1[i]==1) & (y1[i]==0)) {
                 Nyx[i]=NXYcount[1]-n10;
                 pyx[i]=(1.0*NXYcount[1])/(NXYcount[1] +NXYcount[3]);
    } 
    if ((x1[i]==0) & (y1[i]==1)) {
                 Nyx[i]=NXYcount[2]-n01;
                 pyx[i]=(1.0*NXYcount[2])/(NXYcount[0] +NXYcount[2]);
    } 
    if ((x1[i]==1) & (y1[i]==1)) {
                 Nyx[i]=NXYcount[3]-n11;
                 pyx[i]=(1.0*NXYcount[3])/(NXYcount[1] +NXYcount[3]);
    } 

    X1[i][0] = 1;
    X1[i][1] = x1[i];
    X1[i][2] = z1[i];
    X1[i][3] = x1[i]*z1[i];

    if (*extracov==1) {
      for (j=4;j<n_cov;j++) {
	X1[i][j] = covvec[count];
        count++;
      }
      if (count== (*n_subject*(*n_extracov))) count=0;
    }    
  }


  count=0; 
  for (i=0;i<*n_subject;i++) { 
    X[i][0] = 1;
    X[i][1] = x[i];
    X[i][2] = z[i];
    X[i][3] = x[i]*z[i];

    if (*extracov==1) {
      for (j=4;j<n_cov;j++) {
	X[i][j] = covvec[count];
        count++;
      }
    }
  }

/* if (*verbose==1) { 
       print_vector_long(Nyx,*n_subject*4,file);
       print_vector_double(pyx,*n_subject*4,file);
     }

    
     if (*verbose==1)   print_matrix_double(X1,*n_subject*4,n_cov,file);
     if (*verbose==1)   print_matrix_double(X,*n_subject,n_cov,file);
*/     
 

     N=0;
     for (i=0;i<4;i++) N+=NXYcount[i];

 
  double cutoff1 = pow(10,-6);
  double cutoff2 = pow(10,-11);
  
  /* now start to update covariate distribution in each cycle */

  diff=1.0;
  itt=0;
  while (diff > cutoff1 && itt < *maxit) {
      itt++;
      //fprintf(file,"\nthe full iteration number =%i\n\n", itt);  
      pzsum=0.0;
      for (i=0;i < *n_subject*4;i++) {
	eta =0.0;
        for (j=0;j<n_cov;j++) eta += beta[j]*X1[i][j];
        fit1[i] = exp(eta)/(1+exp(eta));
        err1[i] = y1[i]-fit1[i];
	if (y1[i]==0) fit1[i]=1-fit1[i]; 
      }
      for (i=0;i < *n_subject;i++) {
	eta =0.0;
        for (j=0;j<n_cov;j++) eta += beta[j]*X[i][j];
        fit[i] = exp(eta)/(1+exp(eta));
        err[i] = y[i] - fit[i];
	if (y[i]==0) fit[i]=1-fit[i]; 
      }
 
 
      for (i=0;i < *n_subject*4;i++) {
        if (i <*n_subject) tt[i] = Nyx[i]*fit1[i]/pyx[i] + Nyx[i+*n_subject]*fit1[i+*n_subject]/pyx[i+*n_subject] +Nyx[i+*n_subject*2]*fit1[i+*n_subject*2]/pyx[i+*n_subject*2] + Nyx[i+*n_subject*3]*fit1[i+*n_subject*3]/pyx[i+*n_subject*3];
	else {
	  if (i < *n_subject*2) tt[i]=tt[i- *n_subject]; else {
                if (i < *n_subject*3) tt[i]=tt[i- *n_subject*2]; else tt[i]=tt[i- *n_subject*3];
          }
        }        
        pz[i] = 1/(1.0*N-tt[i]); 
        pzsum += pz[i];        
      }
  
      for (i=0;i < *n_subject*4;i++) {
	pz[i]= pz[i]*4/pzsum;  
      }

//        if (*verbose==1) print_vector_double(pz,*n_subject*4,file);
     
      it=0;
      dif=1;
      while (dif>cutoff2 && it < *maxit) {
	it++;
//         if (*verbose==1) fprintf(file,"\nthe EM iteration number =%i\n\n", it);
        for (i=0;i<*n_subject*5;i++) {
	  if (i<*n_subject) wgt1[i]=1; 
          else  wgt1[i]=Nyx[i-*n_subject]*fit1[i-*n_subject]*pz[i-*n_subject]/pyx[i-*n_subject];
        }
        pzsum=0.0; 
        for (i=0;i<*n_subject*4;i++) {
	  if (i<*n_subject) pznew[i] = wgt1[i] + wgt1[i+*n_subject] + wgt1[i+*n_subject*2] + wgt1[i+*n_subject*3] + wgt1[i+*n_subject*4];       
          else {
	     if (i < *n_subject*2) pznew[i]=pznew[i- *n_subject]; else {
                if (i < *n_subject*3) pznew[i]= pznew[i- *n_subject*2]; else pznew[i]=pznew[i- *n_subject*3];
             }
          }     

          pzsum += pznew[i];          
        }
        
        a=0.0;
        b=0.0;
        c=0.0;
        d=0.0;

        for (i=0;i<*n_subject*4;i++) {
          pznew[i] = 4*pznew[i]/pzsum; 
          if ((x1[i]==0) & (y1[i]==0)) a += fit1[i]*pznew[i];
          if ((x1[i]==1) & (y1[i]==0)) b += fit1[i]*pznew[i];
          if ((x1[i]==0) & (y1[i]==1)) c += fit1[i]*pznew[i];
          if ((x1[i]==1) & (y1[i]==1)) d += fit1[i]*pznew[i];
        }
//        if (*verbose==1) print_vector_double(pznew,*n_subject*4,file);
        for (i=0;i<*n_subject*4;i++) {
          if ((x1[i]==0) & (y1[i]==0)) pyx[i]= a; 
          if ((x1[i]==1) & (y1[i]==0)) pyx[i]= b; 
          if ((x1[i]==0) & (y1[i]==1)) pyx[i]= c; 
          if ((x1[i]==1) & (y1[i]==1)) pyx[i]= d; 
        }

        for (i=0;i<*n_subject*4;i++) {
	      pzdiffvec[i]=fabs(pz[i]-pznew[i]);
	      if (i==0) dif=pzdiffvec[i];
          else if (dif<pzdiffvec[i]) dif=pzdiffvec[i];
          pz[i]=pznew[i];
        }

      }


//      if (*verbose==1) print_vector_double(pz,*n_subject*4,file);

      for   (j=0;j<n_cov;j++) score[j]=0.0;     

      for (i=0;i<*n_subject*5;i++) {
	if (i<*n_subject) {
            wgt1[i]=1; 
            for (j=0;j<n_cov;j++) score[j]+= X[i][j]*wgt1[i]*err[i];
        } else  {
            wgt1[i]=Nyx[i-*n_subject]*fit1[i-*n_subject]*pz[i-*n_subject]/pyx[i-*n_subject];
            for (j=0;j<n_cov;j++) score[j]+= X1[i-*n_subject][j]*wgt1[i]*err1[i-*n_subject]; 
        }
      }

//      if (*verbose==1) print_vector_double(score,n_cov,file);
      
      /* now start numerical differentiation */

     
      for (i=0;i<n_cov;i++) {
	for (j=0;j<2;j++) {
	  for (k=0;k<n_cov;k++) ss[j][k]=0.0;
        }
        for (j=0;j<2;j++) { 
          pzsum=0.0;
          d=pow(-1,j+1)*(*diff_factor); 
          for (k=0;k<n_cov;k++) {
	    if (k!=i) betanew[k]=beta[k]; else betanew[k]=beta[k]+d;
          }
          for (k=0;k < *n_subject*4;k++) {
	    eta =0;
            for (l=0;l<n_cov;l++) eta += betanew[l]*X1[k][l];
            fit1[k] = exp(eta)/(1+exp(eta));
            err1[k] = y1[k]-fit1[k];
	    if (y1[k]==0) fit1[k]=1-fit1[k]; 
          }

          for (k=0;k < *n_subject;k++) {
	    eta =0;
            for (l=0;l<n_cov;l++) eta += betanew[l]*X[k][l];
            fit[k] = exp(eta)/(1+exp(eta));
            err[k] = y[k] - fit[k];
	    if (y[k]==0) fit[k]=1-fit[k]; 
          }
 
          for (k=0;k < *n_subject*4;k++) {
                  if (k <*n_subject) tt[k] = Nyx[k]*fit1[k]/pyx[k] + Nyx[k+*n_subject]*fit1[k+*n_subject]/pyx[k+*n_subject]+Nyx[k+*n_subject*2]*fit1[k+*n_subject*2]/pyx[k+*n_subject*2] + Nyx[k+*n_subject*3]*fit1[k+*n_subject*3]/pyx[k+*n_subject*3] ;
	          else {
	                  if (k < *n_subject*2) tt[k]=tt[k- *n_subject]; else {
                            if (k < *n_subject*3) tt[k]=tt[k- *n_subject*2]; else tt[k]=tt[k- *n_subject*3];
                          }
                  }                       
                  pz[k] = 1.0/(1.0*N-tt[k]); 
                  pzsum += pz[k];
	  } 
	
          for (k=0;k < *n_subject*4;k++) {
	         pz[k]= pz[k]*4/pzsum;  
          }

//          if (*verbose==1) print_vector_double(pz,*n_subject*4,file);
          it=0;
          dif=1;
 
          for (k=0;k<*n_subject*4;k++) pyxnew[k]=pyx[k];
          while (dif>cutoff2 && it < *maxit) {
	     it++;
  //            if (*verbose==1) fprintf(file,"\nthe EM iteration number =%i\n\n", it);
             for (k=0;k<*n_subject*5;k++) {
	      if (k<*n_subject) wgt1[k]=1; 
              else  wgt1[k]=Nyx[k-*n_subject]*fit1[k-*n_subject]*pz[k-*n_subject]/pyxnew[k-*n_subject];
             }
             pzsum=0.0;
             for (k=0;k<*n_subject*4;k++) {
	      if (k<*n_subject) pznew[k] = wgt1[k] + wgt1[k+*n_subject] + wgt1[k+*n_subject*2] + wgt1[k+*n_subject*3] + wgt1[k+*n_subject*4];
               else {
	        if (k < *n_subject*2) pznew[k]=pznew[k- *n_subject]; else {
                  if (k < *n_subject*3) pznew[k]=pznew[k- *n_subject*2]; else pznew[k]=pznew[k- *n_subject*3];
                }
	       }     

             pzsum += pznew[k];          
	     }



           a=0.0;
           b=0.0;
           c=0.0;
           d=0.0;

           for (k=0;k<*n_subject*4;k++) {
               pznew[k] = 4*pznew[k]/pzsum; 
               if ((x1[k]==0) & (y1[k]==0)) a += fit1[k]*pznew[k];
               if ((x1[k]==1) & (y1[k]==0)) b += fit1[k]*pznew[k];
               if ((x1[k]==0) & (y1[k]==1)) c += fit1[k]*pznew[k];
               if ((x1[k]==1) & (y1[k]==1)) d += fit1[k]*pznew[k];
           }

              
           for (k=0;k<*n_subject*4;k++) {
               if ((x1[k]==0) & (y1[k]==0)) pyxnew[k]= a; 
               if ((x1[k]==1) & (y1[k]==0)) pyxnew[k]= b; 
               if ((x1[k]==0) & (y1[k]==1)) pyxnew[k]= c; 
               if ((x1[k]==1) & (y1[k]==1)) pyxnew[k]= d; 
           }
           
           for (k=0;k<*n_subject*4;k++) {
	           pzdiffvec[k]=fabs(pz[k]-pznew[k]);
	           if (k==0) dif=pzdiffvec[k];
               else if (dif<pzdiffvec[k]) dif=pzdiffvec[k];
               pz[k]=pznew[k];
           }
	  }
 
          for (k=0;k<*n_subject*5;k++) {
	     if (k<*n_subject) {
                  wgt1[k]=1; 
                  for (l=0;l<n_cov;l++) ss[j][l]+= X[k][l]*wgt1[k]*err[k];
             } else  {
                  wgt1[k]=Nyx[k-*n_subject]*fit1[k-*n_subject]*pz[k-*n_subject]/pyxnew[k-*n_subject];
                  for (l=0;l<n_cov;l++) ss[j][l]+= X1[k-*n_subject][l]*wgt1[k]*err1[k-*n_subject]; 
             }
          }
          
	}
//          if (*verbose==1)  {
//	   print_matrix_double(ss,2,n_cov,file); }

           for (j=0;j<n_cov;j++) infmat[i][j] = (ss[0][j]-ss[1][j])/(2*(*diff_factor));
      }
      /* now update beta */
//         if (*verbose==1)  {
//	   print_matrix_double(infmat,n_cov,n_cov,file); }
      count=0; 
      for (i=0;i<n_cov;i++) {
            for (j=0;j<n_cov;j++) {
                infmatvec[count]=infmat[i][j];
                count++;
	    }
      }

      //dqrinv_(infmatvec,&n_cov,&epsilon,varmat);
      varmat1=dqrinv(infmatvec,n_cov,epsilon);      
	  
      count=0; 
      for (i=0;i<n_cov;i++) {
            for (j=0;j<n_cov;j++) { 
                infmatinv[i][j]=varmat1[count];
                count++;
            }
      }


      for (i=0;i<n_cov;i++) {
	    change=0.0;
            for (j=0;j<n_cov;j++) {  
	      change+=infmatinv[i][j]*score[j];
            }
            betanew[i]=beta[i]+change;
      }

       
       //print_vector_double(beta,n_cov,file);
       //print_vector_double(betanew,n_cov,file);
        
  
      for (i=0;i<n_cov;i++) {
	    betavec[i]=fabs(beta[i]-betanew[i]);
	    if (i==0) diff=betavec[i];
        else if (diff<betavec[i]) diff=betavec[i];
        beta[i]=betanew[i];
      }
  }
     //converge indicator
    if (diff<cutoff1) *converged=1;
	
	for (i=0;i<n_cov*n_cov;i++){
		varmat[i]=varmat1[i];
	}
	
//    fclose(file);
}    





  double *dqrinv(double *xvec, long n, double tol){
	long i,j,count,rank,*pivot,info;
	double *x,*y,*coef,*qraux,*work,*outvec;
	pivot=long_vec(n);	
	x=double_vec(n*n);
	y=double_vec(n*n);
	coef=double_vec(n*n);
	qraux=double_vec(n);
	work=double_vec(2*n);
	outvec=double_vec(n*n);
	count=0;
	rank=1;
	info=0;
	for (i=0;i<n;i++){
		pivot[i]=i+1;
		qraux[i]=0;
	}
	for (i=0;i<2*n;i++){
		work[i]=0;
	}

	for (i=0;i<n;i++){
		for (j=0;j<n;j++){
			x[j+i*n]=xvec[count];
			coef[j+i*n]=0;
			y[j+i*n]=0;			
			if (i==j) y[j+i*n]=1;
			count++;
		}
	}

	F77_CALL(dqrdc2)(x,&n,&n,&n,&tol,&rank,qraux,pivot,work);
	F77_CALL(dqrcf)(x,&n,&n,qraux,y,&n,coef,&info);

	count=0;

	for (i=0;i<n;i++){
		for (j=0;j<n;j++){
			outvec[count]=coef[j+i*n];
			count++;
		}
	}
	return outvec;
}









 double **double_vec_to_mat(double *Yvec, long nrow, long ncol){
	   long i,j,k=0;
	   double **Y;
           Y=double_matrix(nrow,ncol);
	   for (i=0;i<nrow;i++){
		   for (j=0;j<nrow;j++) {
			    Y[i][j]=Yvec[k];
				k++;
		   }
	   }
	   return Y;
}


long *long_mat_to_vec(long **Ymat, long nrow, long ncol){
	   long i,j,count=0;
	   long *Y;
           Y=long_vec(nrow*ncol);
	   for (i=0;i<nrow;i++){
		   for (j=0;j<ncol;j++) {
			   Y[count]= Ymat[i][j];
				count++;
		   }
	   }
	   return Y;
}



long **long_vec_to_mat(long *Yvec, long nrow, long ncol){
	   long i,j,k;
	   long **Y;
           Y = long_matrix(nrow,ncol);
           k=0;
	   for (i=0;i<nrow;i++){
		   for (j=0;j<ncol;j++) {
			    Y[i][j]=Yvec[k];
				k++;
		   }
	   }
	   return Y;
}





double **double_matrix(long nrow, long ncol){
	 long i;
	 double **m;
	 m=(double **) Calloc(nrow, double *);
	 //if (!m) errmsg("mem alloc failure 1 in double_matrix");
	 for (i=0;i<nrow;i++) {
		  m[i]=(double *) Calloc(ncol,double);
		  //if (!m[i]) errmsg("mem alloc failure 2 in double_matrix");
	 }
	 return m;
}





long **long_matrix(long nrow, long ncol){
	 long i;
	 long **m;
	 m=(long **) Calloc(nrow, long *);
	 //if (!m) errmsg("mem alloc failure 1 in long_matrix");
	 for (i=0;i<nrow;i++) {
		  m[i]=(long *) Calloc(ncol,long);
		  //if (!m[i]) errmsg("mem alloc failure 2 in long_matrix");
	 }
	 return m;
}




double *double_vec(long n){

	 double *v;
	 v=(double *) Calloc(n, double);
	 //if (!v) errmsg("mem alloc failure in double_vec");
	 return v;
}

long *long_vec(long n){
	 long *v;
	 v=(long *) Calloc(n, long);
	 //if (!v) errmsg("mem alloc failure in long_vec");
	 return v;
}


long max_long(long x, long y){
     long z;
     z=(x>=y)?x:y;
     return z;
}


long min_long(long x, long y){
     long z;
     z=(x>=y)?y:x;
     return z;
}




double max_double(double x, double y){
     double z;
     z=(x>=y)?x:y;
     return z;
}



double min_double(double x, double y){
     double z;
     z=(x>=y)?y:x;
     return z;
}


void print_matrix_long(long **m, long nrow, long ncol, FILE *file){
  long i,j;
  for (i=0;i<nrow;i++) {
    for (j=0;j<ncol;j++){
          if (j==0) fprintf(file,"\n%i",m[i][j]);
          else fprintf(file,"\t%i",m[i][j]);
    }
  }
}


void print_matrix_double(double **m, long nrow, long ncol, FILE *file){
  long i,j;
  for (i=0;i<nrow;i++) {
    for (j=0;j<ncol;j++){
          if (j==0) fprintf(file,"\n%.4e",m[i][j]);
          else fprintf(file,"\t%.4e",m[i][j]);
    }
  }
}



void print_vector_double(double *m, long n, FILE *file){
    long j; 
    for (j=0;j<n;j++){
          if (j==0) fprintf(file,"\n%.4e",m[j]);
          else fprintf(file,"\t%.4e",m[j]);
    }
  
}



void print_vector_long(long *m, long n, FILE *file){
    long j;
    for (j=0;j<n;j++){
          if (j==0) fprintf(file,"\n%i",m[j]);
          else fprintf(file,"\t%i",m[j]);
    }
    return;
  
}


/***********************************************************************************/


