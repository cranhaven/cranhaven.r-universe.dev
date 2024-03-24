
#include <R.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h> 
#include <fcntl.h>
#include <string.h>
#include <math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sf_gamma.h>


int Fonction_HiC_R(int *taille,int *K,char** distrib,double* matrice,int* tchap,double* Jvect,int* t_est_mat,char** modele)
     {
       int taille_matrice = *taille;
       int Kmax = *K;
       char *loi=*distrib;
       char *model=*modele;
       int i, j, k, l, E, t, taille_mat_extr, taille_mat_Exterieur_extr, i_coin, j_coin, taille_coin,n_coin, taille_mu;
       double somme, Ybar, logYbar, logphiplusYbar, logmuhat, logphiplusmuhat, loglambdahat, somme2, somme_carr;
       double max, min, mu_hat, var_hat, phi_hat, lambda_hat,mu;
       int ind_max, ind_min;
       int u;
       int kk;
       int Kchap;
       double *vecteur;
       double *J;
       double **t_est;
       double **resultats;
       double **y, **Delta;
       double **Exterieur, **T, **D, **R, **Tcarr, **Dcarr, **Rcarr;
       double **I, **t_matrice;
       
       // Allocation memoire pour chacune des matrices et pour chaque vecteur
       y=(double **)malloc(sizeof(double *)*taille_matrice);

       gsl_matrix * ylect = gsl_matrix_alloc (taille_matrice, taille_matrice);

       Delta=(double **)malloc(sizeof(double *)*taille_matrice);
       Exterieur=(double **)malloc(sizeof(double *)*taille_matrice);
       D=(double **)malloc(sizeof(double *)*taille_matrice);
       T=(double **)malloc(sizeof(double *)*taille_matrice);
       R=(double **)malloc(sizeof(double *)*taille_matrice);
       Dcarr=(double **)malloc(sizeof(double *)*taille_matrice);
       Tcarr=(double **)malloc(sizeof(double *)*taille_matrice);
       Rcarr=(double **)malloc(sizeof(double *)*taille_matrice);
       
       for (i=0;i<taille_matrice;i++)
	 {
	   *(y+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(Delta+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(Exterieur+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(T+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(D+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(R+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(Tcarr+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(Dcarr+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(Rcarr+i)=(double *)malloc(sizeof(double)*taille_matrice);
	 }
       I=(double **)malloc(sizeof(double *)*Kmax);
       t_matrice=(double **)malloc(sizeof(double *)*(Kmax-1));
       for (i=0;i<Kmax-1;i++)
	 {
	   *(I+i)=(double *)malloc(sizeof(double)*taille_matrice);
	   *(t_matrice+i)=(double *)malloc(sizeof(double)*taille_matrice);
	 }
       *(I+Kmax-1)=(double *)malloc(sizeof(double)*taille_matrice);
       
       vecteur=(double *)malloc(sizeof(double)*(taille_matrice-1));
       J=(double *)malloc(sizeof(double)*Kmax);
       
       t_est=(double **)malloc(sizeof(double *)*Kmax);
       resultats=(double **)malloc(sizeof(double *)*Kmax);
       for (i=0;i<Kmax;i++)
	 {
	   *(t_est+i)=(double *)malloc(sizeof(double)*Kmax);
	   *(resultats+i)=(double *)malloc(sizeof(double)*(Kmax+2));
	 }
       

       if ((strcmp(model,"D")!=0)&&(strcmp(model,"Dplus")!=0))
	  {
	   return EXIT_FAILURE;
	  }

       for (i=0;i<taille_matrice;i++)
	 {
	   for (j=0;j<taille_matrice;j++)
	     {
	       y[i][j]=matrice[i*taille_matrice+j];
	       Delta[i][j] = -1E100;
	       Exterieur[i][j] = -1E100;
	       T[i][j] = -1E100;
	       D[i][j] = -1E100;
	       R[i][j] = -1E100;
	       Tcarr[i][j] = -1E100;
	       Dcarr[i][j] = -1E100;
	       Rcarr[i][j] = -1E100;
	     }
	 }
	   
  if (strcmp(loi,"P")==0)
	 {
	   /////  Calcul des sommes dans des trapezes
	   T[0][0]=y[0][0];
	   D[0][0]=y[0][0];
	   if (D[0][0]!=0) Delta[0][0]=D[0][0]*(log(D[0][0])-1);

	   for (k=1;k<taille_matrice;k++)
	     {
	       T[0][k]=T[0][(k-1)]+y[0][k];
	       D[k][k]=y[k][k];
	       somme=0;
	       for (i=0;i<=k;i++) somme=somme+y[i][k];
	       T[k][k]=T[(k-1)][(k-1)]+somme;
	       D[0][k]=T[k][k];
	       if (D[0][k]!=0) Delta[0][k]=D[0][k]*(log(D[0][k])-log((pow(k,2)+k)/2)-1);
	       if (D[k][k]!=0) Delta[k][k]=D[k][k]*(log(D[k][k])-1);
	     }

	   if(strcmp(model,"Dplus")==0)
	     {
	       for (i=1;i<(taille_matrice-1);i++)
		 {
		   for (j=(i+1);j<taille_matrice;j++)
		     {
		       somme=0;
		       for (k=0;k<=i;k++) somme=somme+y[k][j];
		       T[i][j]=T[i][(j-1)]+somme;
		       R[i][j]=T[(i-1)][j]-T[(i-1)][(i-1)];
		       D[i][j]=T[j][j]-T[(i-1)][j];
		       taille_mat_extr=(j-i+1)*(j-i)/2+(j-i+1);
		       if ((D[i][j]!=0)&&(taille_mat_extr!=0))
			 {
			   Delta[i][j] = D[i][j]*(log(D[i][j])-log(taille_mat_extr)-1);
			 }
		    
		       taille_mat_extr=i*(j-i+1);
		       if ((R[i][j]!=0)&&(taille_mat_extr!=0))
			 {
			   Exterieur[i][j]=R[i][j]*(log(R[i][j])-log(taille_mat_extr)-1);
			 }
		     }
		 }
	     }
	   else if (strcmp(model,"D")==0)
	     {
	     
	       ///// Calcul de lambda_hat
	       n_coin=(int)floor(taille_matrice/4);
	       taille_coin=(n_coin+1)*n_coin/2;
	       j_coin=3*n_coin;
	       i_coin=n_coin;
	       somme=0.;
	       
	       for(i=0; i<i_coin; i++)
		 {
		   for (j=j_coin; j<taille_matrice; j++)
		     {
		       if ((j-j_coin)>=i)
			 {
			   somme=somme+y[i][j];
			 }
		     }
		 }
	       lambda_hat=somme/taille_coin;
	       loglambdahat=log(lambda_hat);
   
	       for (i=1;i<(taille_matrice-1);i++)
		 {
		   for (j=(i+1);j<taille_matrice;j++)
		     {
		       somme=0;
		       for (k=0;k<=i;k++) somme=somme+y[k][j];
		       T[i][j]=T[i][(j-1)]+somme;
		       R[i][j]=T[(i-1)][j]-T[(i-1)][(i-1)];
		       D[i][j]=T[j][j]-T[(i-1)][j];
		       taille_mat_extr=(j-i+1)*(j-i)/2+(j-i+1);
		       if ((D[i][j]!=0)&&(taille_mat_extr!=0))
			 {
			   Delta[i][j] = D[i][j]*(log(D[i][j])-log(taille_mat_extr)-1);
			 }
		       taille_mat_extr=i*(j-i+1);
		       Exterieur[i][j] = R[i][j]*loglambdahat-taille_mat_extr*lambda_hat;
			 
		     }
		 }   
	     }
	 }
	 	      
		 
	

  else if (strcmp(loi,"B")==0)
    {
      ///// Calcul de phi_hat et de mu_hat

      n_coin=(int)floor(taille_matrice/4);
      
      taille_coin=(n_coin+1)*n_coin/2;
      j_coin=3*n_coin;
      i_coin=n_coin;
      somme=0.; somme_carr=0;
      
      for(i=0; i<i_coin; i++)
	{
	  for (j=(j_coin-1); j<taille_matrice; j++)
	    {
	      if ((j-j_coin)>=i)
		{
		  somme=somme+y[i][j];
		  somme_carr=somme_carr+ pow(y[i][j],2);
		}
	    }
	}
      mu_hat=somme/taille_coin;
      var_hat=somme_carr/(taille_coin-1)-taille_coin*pow(mu_hat,2)/(taille_coin-1);
      phi_hat=pow(mu_hat,2)/(var_hat-mu_hat);

       /////  Calcul des sommes dans des trapezes
	   T[0][0]=y[0][0];
	   D[0][0]=y[0][0];
	   if (((phi_hat+D[0][0])>0)&&(D[0][0]>0)) Delta[0][0]=-(phi_hat+D[0][0])*log(phi_hat+D[0][0])+D[0][0]*log(D[0][0]);

	   for (k=1;k<taille_matrice;k++)
	     {
	       T[0][k]=T[0][(k-1)]+y[0][k];
	       D[k][k]=y[k][k];
	       somme=0;
	       for (i=0;i<=k;i++) somme=somme+y[i][k];
	       T[k][k]=T[(k-1)][(k-1)]+somme;
	       D[0][k]=T[k][k];
	       taille_mu=((pow(k+1,2)+k+1)/2);
	       mu=D[0][k]/taille_mu;
	       if (D[0][k]>0) Delta[0][k]=-(taille_mu*phi_hat+D[0][k])*log(phi_hat+mu)+D[0][k]*log(mu);
	       if (D[k][k]>0) Delta[k][k]=-(phi_hat+D[k][k])*log(phi_hat+D[k][k])+D[k][k]*log(D[k][k]);
	     }

	   if(strcmp(model,"Dplus")==0)
	     {
	       for (i=1;i<(taille_matrice-1);i++)
		 {
		   for (j=(i+1);j<taille_matrice;j++)
		     {
		       somme=0;
		       for (k=0;k<=i;k++) somme=somme+y[k][j];
		       T[i][j]=T[i][(j-1)]+somme;
		       R[i][j]=T[(i-1)][j]-T[(i-1)][(i-1)];
		       D[i][j]=T[j][j]-T[(i-1)][j];
		       taille_mat_extr=(j-i+1)*(j-i)/2+(j-i+1);
		       if ((D[i][j]>0)&&(taille_mat_extr!=0))
			 {
			   mu=D[i][j]/taille_mat_extr;
			   Delta[i][j] = -(taille_mat_extr*phi_hat+D[i][j])*log(phi_hat+mu)+D[i][j]*log(mu);
			 }
		       taille_mat_extr=i*(j-i+1);
		       if ((R[i][j]>0)&&(taille_mat_extr!=0))
			 {
			   mu=R[i][j]/taille_mat_extr;
			   Exterieur[i][j]=-(taille_mat_extr*phi_hat+R[i][j])*log(phi_hat+mu)+R[i][j]*log(mu);
			 }
		     }
		 }
	     }
	   else if (strcmp(model,"D")==0)
	     {
	       for (i=1;i<(taille_matrice-1);i++)
		 {
		   for (j=(i+1);j<taille_matrice;j++)
		     {
		       somme=0;
		       for (k=0;k<=i;k++) somme=somme+y[k][j];
		       T[i][j]=T[i][(j-1)]+somme;
		       R[i][j]=T[(i-1)][j]-T[(i-1)][(i-1)];
		       D[i][j]=T[j][j]-T[(i-1)][j];
		       taille_mat_extr=(j-i+1)*(j-i)/2+(j-i+1);
		       if ((D[i][j]>0)&&(taille_mat_extr!=0))
			 {
			   mu=D[i][j]/taille_mat_extr;
			   Delta[i][j] = -(taille_mat_extr*phi_hat+D[i][j])*log(phi_hat+mu)+D[i][j]*log(mu);
			 }
		       taille_mat_extr=i*(j-i+1);
		       if (((phi_hat+mu_hat)>0)&&(mu_hat!=0))
			 {
			   Exterieur[i][j] = -(taille_mat_extr*phi_hat+R[i][j])*log(phi_hat+mu_hat)+R[i][j]*log(mu_hat);
			 }
		     }
		 }
	     }
    }

  else if (strcmp(loi,"G")==0)
    {
      n_coin=(int)floor(taille_matrice/4);
      taille_coin=(n_coin+1)*n_coin/2;
      j_coin=3*n_coin;
      i_coin=n_coin;
      somme=0.;
      
      for(i=0; i<i_coin; i++)
	{
	  for (j=j_coin; j<taille_matrice; j++)
	    {
	      if ((j-j_coin)>=i)
		{
		  somme=somme+y[i][j];
		}
	    }
	}
      mu_hat=somme/taille_coin;

      	   /////  Calcul des sommes dans des trapezes
	   T[0][0]=y[0][0];
	   D[0][0]=y[0][0];
	   Tcarr[0][0]=pow(y[0][0],2);
	   Dcarr[0][0]=pow(y[0][0],2);
	   Delta[0][0]=0;
	 
	   for (k=1;k<taille_matrice;k++)
	     {
	       T[0][k]=T[0][(k-1)]+y[0][k];
	       Tcarr[0][k]=Tcarr[0][(k-1)]+pow(y[0][k],2);
	       D[k][k]=y[k][k];
	       Dcarr[k][k]=pow(y[k][k],2);
	       somme=0;somme_carr=0;
	       for (i=0;i<=k;i++) 
		 {
		   somme=somme+y[i][k];
		   somme_carr=somme_carr+pow(y[i][k],2);
		 }
	       T[k][k]=T[(k-1)][(k-1)]+somme;
	       Tcarr[k][k]=Tcarr[(k-1)][(k-1)]+somme_carr;
	       D[0][k]=T[k][k];
	       Dcarr[0][k]=Tcarr[k][k];
	       mu=D[0][k]/((pow(k+1,2)+k+1)/2);
	       Delta[0][k]=-(Dcarr[0][k]-D[0][k]*mu);
	       Delta[k][k]=0;
	     }
	   if(strcmp(model,"Dplus")==0)
	     {
	       for (i=1;i<(taille_matrice-1);i++)
		 {
		   for (j=(i+1);j<taille_matrice;j++)
		     {
		       somme=0;somme_carr=0;
		       for (k=0;k<=i;k++) 
			 {
			   somme=somme+y[k][j];
			   somme_carr=somme_carr+pow(y[k][j],2);
			 }
		       T[i][j]=T[i][(j-1)]+somme;
		       R[i][j]=T[(i-1)][j]-T[(i-1)][(i-1)];
		       D[i][j]=T[j][j]-T[(i-1)][j];
		       Tcarr[i][j]=Tcarr[i][(j-1)]+somme_carr;
		       Rcarr[i][j]=Tcarr[(i-1)][j]-Tcarr[(i-1)][(i-1)];
		       Dcarr[i][j]=Tcarr[j][j]-Tcarr[(i-1)][j];
		       taille_mat_extr=(j-i+1)*(j-i)/2+(j-i+1);
		      
		       mu=D[i][j]/taille_mat_extr;
		       Delta[i][j] = -(Dcarr[i][j]-D[i][j]*mu);
			      
		       taille_mat_extr=i*(j-i+1);
		       mu=R[i][j]/taille_mat_extr;
		       Exterieur[i][j]=-(Rcarr[i][j]-pow(mu,2)*taille_mat_extr);
		     }
		 }
	     }
	   else if (strcmp(model,"D")==0)
	     {
	       for (i=1;i<(taille_matrice-1);i++)
		 {
		   for (j=(i+1);j<taille_matrice;j++)
		     {
		       somme=0;somme_carr=0;
		       for (k=0;k<=i;k++) 
			 {
			   somme=somme+y[k][j];
			   somme_carr=somme_carr+pow(y[k][j],2);
			 }
		       T[i][j]=T[i][(j-1)]+somme;
		       R[i][j]=T[(i-1)][j]-T[(i-1)][(i-1)];
		       D[i][j]=T[j][j]-T[(i-1)][j];
		       Tcarr[i][j]=Tcarr[i][(j-1)]+somme_carr;
		       Rcarr[i][j]=Tcarr[(i-1)][j]-Tcarr[(i-1)][(i-1)];
		       Dcarr[i][j]=Tcarr[j][j]-Tcarr[(i-1)][j];
		       taille_mat_extr=(j-i+1)*(j-i)/2+(j-i+1);
		      
		       mu=D[i][j]/taille_mat_extr;
		       Delta[i][j] = -(Dcarr[i][j]-D[i][j]*mu);
			      
		       taille_mat_extr=i*(j-i+1);
		       Exterieur[i][j]=-(Rcarr[i][j]-pow(mu_hat,2)*taille_mat_extr);
		     }
		 }
	     }
	   free(Dcarr);
	   free(Tcarr);
	   free(Rcarr);
    }

	   /////// Programmation dynamique ///////////		
	   for (i=0; i<Kmax-1; i++)
	     {
	       for (j=0; j<taille_matrice; j++)
		 {
		   I[i][j] = -1E100;
		   t_matrice[i][j] = -1;
		 }
	     }
      
	   for (j=0; j<taille_matrice; j++)
	     {
	       I[Kmax-1][j] = -1E100;
	     }
      
	   for (j=0; j<taille_matrice-1; j++)
	     {
	       vecteur[j] = -1E100;
	       I[0][j] = Delta[0][j];
	     }
	   I[0][taille_matrice-1] = Delta[0][taille_matrice-1];
	   
	   for (k=1; k<Kmax-1; k++)
	     {
	       for (l=k; l<taille_matrice; l++)
		 {
		   for (i=0; i<taille_matrice-1; i++)
		     {
		       vecteur[i] = -1E100;
		     }
	      
		   for (u=1; u<=l; u++)
		     {
		       vecteur[u-1] = I[k-1][u-1] + Delta[u][l] + Exterieur[u][l];
		     }
		   ind_max = 0;
		   max = vecteur[0];
		   for (u=0; u<taille_matrice-1; u++)
		     {
		       if (vecteur[u]>max)
			 {
			   max = vecteur[u];
			   ind_max = u;
			 }
		     }
		   I[k][l] = max;
		   t_matrice[k-1][l] = ind_max;
		 }
	     }
      
	   for (i=0; i<taille_matrice-1; i++)
	     {
	       vecteur[i] = -1E100;
	     }
	   ind_max = 0;
	   max = vecteur[0];
	   for (u=1; u<taille_matrice; u++)
	     {
	       vecteur[u-1] = I[Kmax-2][u-1] + Delta[u][taille_matrice-1] + Exterieur[u][taille_matrice-1];
	     }
	   for (i=0; i<taille_matrice-1; i++)
	     {
	       if (vecteur[i]>max)
		 {
		   max = vecteur[i];
		   ind_max = i;
		 }
	     }
      
	   I[Kmax-1][taille_matrice-1] = vecteur[ind_max];
      
	   t_matrice[Kmax-2][taille_matrice-1] = ind_max;
      
	   ind_max = 0;
	   max = I[0][taille_matrice-1];
	   for (i=0; i<Kmax; i++)
	     {
	       J[i] = I[i][taille_matrice-1];
	  
	       if (J[i]>max)
		 {
		   max = J[i];
		   ind_max = i;
		 }
	     } 
  
  for (i=0;i<Kmax;i++)
    {
      for (j=0;j<Kmax;j++)
	{
	  t_est[i][j] = -1;
	}
    }
       
  for (i=0; i<Kmax; i++)
    {
      t_est[i][i] = taille_matrice - 1;
    }
  
  /////// Calcul des change-points /////////	
  for (kk=1; kk<Kmax; kk++)
    {
      for (k=kk-1; k>=0; k--)
	{
	  t_est[kk][k] = t_matrice[k][(int)t_est[kk][k+1]];
	}
    }
  
  
  //////// Calcul de Kchap /////////
  
  Kchap = ind_max;
  
 for (i=0; i<Kmax; i++)
    {
      tchap[i] = (int) t_est[Kchap][i]+1;
      Jvect[i] = J[i];
      for (j=0; j<Kmax; j++)
	{
	  t_est_mat[i*Kmax+j]=(int) t_est[i][j]+1;
	}
    }

   	free(y);
     	free(Delta);
     	free(Exterieur);
     	free(t_matrice);
     	free(I);
     	free(vecteur);
     	free(resultats);
     	free(J);
     	free(t_est);
	free(T);
	free(D);
	free(R);

  return(EXIT_SUCCESS);
     }

	  

 







































































      
