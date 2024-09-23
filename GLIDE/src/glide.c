//#define MATHLIB_STANDALONE 1
#ifndef USE_FC_LEN_T
#define USE_FC_LEN_T
#endif
#include <Rconfig.h>
#include <R_ext/BLAS.h>
#ifndef FCONE
#define FCONE
#endif
# include <stdio.h>
# include <stdlib.h>
# include <R.h>
# include <R_ext/RS.h>
# include <Rmath.h>
# include <math.h>
# include <string.h>
# include "glide.h"
# include <R_ext/Lapack.h>


//void myrnorm(int n,double miu,double sigma, double *z)
void compute_z(int *nsnp,
               double *v_cormat,
               int *np,
               double *zsim){
    int i,j,ii,jj,count,count1;
    double *vect1,*vect3,*vect4,*mu,sig,*inv_cormat; //sig is a scal
    inv_cormat=double_vec((*nsnp)*(*nsnp));
    vect1=double_vec((*np)*(*nsnp)); //the longest one np*nsnp
    vect3=double_vec(*nsnp); //*nsnp
    vect4=double_vec(*nsnp); //*nsnp
    mu=double_vec(*np);
 
//FILE *file;///

    myrnorm(*np,0,1,zsim);
//    for (i=0;i<*np;i++) //zsim[,1]
//    {
//	zsim[i]=myrnorm1(0,1);
//    }

    count1=*np; //index of zsim

    int m=*nsnp;
    //printf("\n%s %d %s\n","compute zcores, total",*nsnp,"iterations...");
    for (i=1;i<m;i++)
    {      
	//if (i % 10 ==0)
     //   {
     //       printf("%d %s",i,"...");
     //       fflush(stdout);
    //    }
        count=0;
        for (ii=0;ii<i;ii++)
        {
            for (jj=0;jj<i;jj++)
            {
                vect1[count]=v_cormat[ii*(*nsnp)+jj]; //cormat[1:(k-1),1:(k-1)
                count++;
            }
        }
        dqrinv(vect1,i, pow(10,-8), inv_cormat);
        mydgemm(*np,i,i,zsim,inv_cormat,vect1); //vect1=zsim[,1:(k-1)] %*% ginv(cormat[1:(k-1),1:(k-1)])
        count=0;
        for(ii=0;ii<i;ii++)
        {
            vect3[count]=v_cormat[*nsnp*i+ii]; //cormat[1:(k-1),k]
            count++;
        }
        mydgemm(*np,1,i,vect1,vect3,mu); //mu=zsim[,1:(k-1)] %*% ginv(cormat[1:(k-1),1:(k-1)]) %*% cormat[1:(k-1),k]

        if (i<*nsnp-1)
        {
            mydgemm(1,i,i,vect3,inv_cormat,vect4); //vect4=cormat[1:(k-1),k] %*% ginv(cormat[1:(k-1),1:(k-1)])
            sig=0;
            for (jj=0;jj<i;jj++)
            {
                sig+=vect4[jj]*vect3[jj]; //sig=cormat[1:(k-1),k] %*% ginv(cormat[1:(k-1),1:(k-1)]) %*% cormat[1:(k-1),k])
            }
            sig=1-sig;
            sig=sqrt(sig);
          
            for (jj=0;jj<*np;jj++)
            {
	       //zsim[count1+jj]=myrnorm1(mu[jj],sig);
		myrnorm(1,mu[jj],sig,zsim+count1+jj);
            }
            count1+=*np;
        }else //last loop
        {
            for (jj=0;jj<*np;jj++)
            {             
		zsim[count1+jj]=mu[jj];
            }
        }
    }
   
   // printf("\n%s\n","Compute p-values...");
 
    for (i=0;i<*nsnp*(*np);i++)
    {
        vect1[i]=2*(1-pnorm(fabs(zsim[i]),0,1,1,0));         //zsim[,k]=2*(1-pnorm(abs(zsim[,k])))
    }

    //printf("%s\n","Sort p-values...");
    //zsim[k,] <- zsim[k,order(zsim[k,])]

//    count1=0;
//    for (i=0;i<*np;i++)
//    {
//        for (j=0;j<*nsnp;j++)
//        {
//            vect5[j]=vect1[j*(*np)+i];
//        }
//        qsort(vect5,*nsnp,sizeof(double),compare);
//        for (j=0;j<*nsnp;j++)
//        {
//	    zsim[count1]=vect5[j];
//            count1++;
//        }
//    }
    count1=0;
    for (i=0;i<*np;i++)
    {
	for (j=0;j<*nsnp;j++)
	{
	  zsim[count1]=vect1[j*(*np)+i];//re oder by row
	  count1++;
	}
	qsort(zsim+count1-(*nsnp),*nsnp,sizeof(double),compare);
    }

   
    //file = fopen("debug.txt", "w");
    //print_vector_double(vect1,*nsnp*(*np),file);
    //fclose(file);
    //    printf("%d\t%d\n",*n_subject,*nsnp);
    
    //file = fopen("debug1.txt", "w");
    //print_vector_double(vect3,*np*(*nsnp),file);
    //fclose(file);
    //file = fopen("debug2.txt", "w");
    //print_vector_double(vect3,*np*(*nsnp),file);
    //fclose(file);
    //file = fopen("debug3.txt", "w");
    //print_vector_double(vect3,(m-1),file);
    //fclose(file);
    //file = fopen("debug4.txt", "w");
    //print_vector_double(mu,*np,file);
    //fclose(file);

   Free(mu);
   Free(vect1);
   Free(vect3);
   Free(vect4);
   Free(inv_cormat); 
}

               

void generate_modlematrix(int nrow,
                              int ncol,
                              double *v_xmat, //second col is grs
                              double *v_genotype,
                              int col,//which snp
                              char opt,
			      double *resmat) {
    
    //printf("%d\t%d\n",*n_subject,*ncol_xmat);
    double *resmat1;
	
    resmat1=double_vec(nrow*(ncol+1));
    int i,j,count;
	
    for(count=0;count<nrow*(ncol+1);count++)
    {
        if (count<2*nrow)
        {
            resmat1[count]=v_xmat[count];
        }else if (count >=2*nrow && count < 3*nrow) //insert snp
        {
            resmat1[count]=v_genotype[count-2*nrow+col*nrow];
        }else
        {
            resmat1[count]=v_xmat[count-nrow];
        }
    }
	
    if (opt == 'r' || opt =='R') //transpoes
    {
        for (i=0;i<nrow;i++)
        {
            for (j=0;j<ncol+1;j++)
            {
                resmat[i*(ncol+1)+j]=resmat1[j*nrow+i];
            }
        }
    }else
    {
        for (count=0;count<nrow*(ncol+1);count++)
        {
            resmat[count]=resmat1[count];
        }
    }
	
    Free(resmat1);
}

void compute_cormat_col(int *nsnp,
                     int *n_subject,
		     int *colnumber,
                     int *ncol_xmat,
                     double *v_yfit,
                     double *v_y,
                     double *v_xmat,
                     double *v_genotype,
                     double *v_cormat_col) {
    int i,j,ii,jj;
    int n_cor=*ncol_xmat+1; //size of xmat1 and xmat2 with a snp
    double *v_xmat1,*v_xmat2,*v_xmat1t,*v_xmat2t;
    v_xmat1=double_vec(*n_subject*n_cor);
    v_xmat2=double_vec(*n_subject*n_cor);
    v_xmat1t=double_vec(*n_subject*n_cor);
    v_xmat2t=double_vec(*n_subject*n_cor);
    double *v_xmat_yfit_1_yfit;
    v_xmat_yfit_1_yfit=double_vec(*n_subject*n_cor);
    double *bread1,*bread2,*bread,*inv_bread;
    bread1=double_vec(n_cor*n_cor);
    bread2=double_vec(n_cor*n_cor);
    bread=double_vec(4*n_cor*n_cor);
    inv_bread=double_vec(4*n_cor*n_cor);
    double *score1,*score2,*score,*scoret;
    score=double_vec(*n_subject*2*n_cor);
    scoret=double_vec((*n_subject)*2*n_cor);
    score1=double_vec(*n_subject*n_cor);
    score2=double_vec(*n_subject*n_cor);
    double *beef;
    beef=double_vec(4*n_cor*n_cor);
    double *inv_bread_beef;
    inv_bread_beef=double_vec(4*n_cor*n_cor);
    double *v_covmat;
    v_covmat=double_vec(4*n_cor*n_cor);
    
    int count=0;
    char optc='C'; //by col
    char optr='R'; //by row

    double *v_yfit_1_yfit=double_vec(*n_subject);
    double *v_y_yfit=double_vec(*n_subject);
    
//FILE *file; /////
    for (i=0;i<*n_subject;i++)
    {
        v_yfit_1_yfit[i]=v_yfit[i]*(1-v_yfit[i]);
        v_y_yfit[i]=v_y[i]-v_yfit[i];
    }
    
    i=*colnumber-1; //for specific column
//    for (i=0;i<*nsnp-1;i++)
    //for (i=0;i<1;i++)
//    {
        for (j=i+1;j<*nsnp;j++)
	//for (j=1;j<4;j++)
        {
            generate_modlematrix(*n_subject,*ncol_xmat,v_xmat,v_genotype,i,optc,v_xmat1);
            generate_modlematrix(*n_subject,*ncol_xmat,v_xmat,v_genotype,i,optr,v_xmat1t);
            generate_modlematrix(*n_subject,*ncol_xmat,v_xmat,v_genotype,j,optc,v_xmat2);
            generate_modlematrix(*n_subject,*ncol_xmat,v_xmat,v_genotype,j,optr,v_xmat2t);

            multiplicationbyrow(v_xmat1,*n_subject,n_cor,v_yfit_1_yfit,v_xmat_yfit_1_yfit);
            mydgemm(n_cor,n_cor,*n_subject,v_xmat1t,v_xmat_yfit_1_yfit,bread1);

            multiplicationbyrow(v_xmat2,*n_subject,n_cor,v_yfit_1_yfit,v_xmat_yfit_1_yfit);
            mydgemm(n_cor,n_cor,*n_subject,v_xmat2t,v_xmat_yfit_1_yfit,bread2);
            
            //combine bread1 and bread2
            count=0;
            for (ii=0;ii<n_cor;ii++)
            {
                for (jj=0;jj<n_cor;jj++)
                {
                    bread[count]=bread1[ii*n_cor+jj];
                    count++;
                }
                count=count+n_cor;
            }
            for (ii=0;ii<n_cor;ii++)
            {
                count=count+n_cor;
                for (jj=0;jj<n_cor;jj++)
                {
                    bread[count]=bread2[ii*n_cor+jj];
                    count++;
                }
            }
            dqrinv(bread, n_cor*2, pow(10,-8), inv_bread);
            
            multiplicationbyrow(v_xmat1,*n_subject,n_cor,v_y_yfit,score1);
            multiplicationbyrow(v_xmat2,*n_subject,n_cor,v_y_yfit,score2);
            //cbind score1 and score2
            for (count=0;count<(*n_subject)*2*n_cor;count++)
            {
                if (count<(*n_subject)*n_cor)
                {
                    score[count]=score1[count];
                }else
                {
                    score[count]=score2[count-(*n_subject)*n_cor];
                }
            }
            //transpose
            for (ii=0;ii<*n_subject;ii++)
            {
                for (jj=0;jj<2*n_cor;jj++)
                {
                    scoret[ii*2*n_cor+jj]=score[jj*(*n_subject)+ii];
                }
            }

            
            mydgemm(2*n_cor,2*n_cor,*n_subject,scoret,score,beef);
            mydgemm(2*n_cor,2*n_cor,2*n_cor,inv_bread,beef,inv_bread_beef);
            mydgemm(2*n_cor,2*n_cor,2*n_cor,inv_bread_beef,inv_bread,v_covmat);
            
//            v_cormat[i*(*nsnp)+j]=v_cormat[j*(*nsnp)+i]=v_covmat[(n_cor+2)*(2*n_cor)+2]/sqrt(v_covmat[2*2*n_cor+2]*v_covmat[(n_cor+2)*2*n_cor+n_cor+2]);
			v_cormat_col[j]=v_covmat[(n_cor+2)*(2*n_cor)+2]/sqrt(v_covmat[2*2*n_cor+2]*v_covmat[(n_cor+2)*2*n_cor+n_cor+2]);
            //printf("%d %d %f\n",i,j,v_cormat[i*(*nsnp)+j]);    
        }
//    }
  
    //file = fopen("debug.txt", "w");
    //print_vector_double(v_xmat_yfit_1_yfit,*n_subject*n_cor,file);
    //print_vector_double(bread1,n_cor*n_cor,file);
    //print_vector_double(v_cormat,*nsnp*(*nsnp),file);
    //fclose(file);  
    Free(v_yfit_1_yfit);
    Free(v_y_yfit);    
    Free(v_xmat1);
    Free(v_xmat1t);
    Free(v_xmat2);
    Free(v_xmat2t);
    Free(v_xmat_yfit_1_yfit);
    Free(bread1);
    Free(bread2);
    Free(bread);
    Free(inv_bread);
    Free(score);
    Free(scoret);
    Free(score1);
    Free(score2);
    Free(beef);
    Free(inv_bread_beef);
    Free(v_covmat);

    //printf("%s","\n");
 
    
}



void compute_cormat(int *nsnp,
                     int *n_subject,
                     int *ncol_xmat,
                     double *v_yfit,
                     double *v_y,
                     double *v_xmat,
                     double *v_genotype,
                     double *v_cormat) {
    int i,j,ii,jj;
    int n_cor=*ncol_xmat+1; //size of xmat1 and xmat2 with a snp
    double *v_xmat1,*v_xmat2,*v_xmat1t,*v_xmat2t;
    v_xmat1=double_vec(*n_subject*n_cor);
    v_xmat2=double_vec(*n_subject*n_cor);
    v_xmat1t=double_vec(*n_subject*n_cor);
    v_xmat2t=double_vec(*n_subject*n_cor);
    double *v_xmat_yfit_1_yfit;
    v_xmat_yfit_1_yfit=double_vec(*n_subject*n_cor);
    double *bread1,*bread2,*bread,*inv_bread;
    bread1=double_vec(n_cor*n_cor);
    bread2=double_vec(n_cor*n_cor);
    bread=double_vec(4*n_cor*n_cor);
    inv_bread=double_vec(4*n_cor*n_cor);
    double *score1,*score2,*score,*scoret;
    score=double_vec(*n_subject*2*n_cor);
    scoret=double_vec((*n_subject)*2*n_cor);
    score1=double_vec(*n_subject*n_cor);
    score2=double_vec(*n_subject*n_cor);
    double *beef;
    beef=double_vec(4*n_cor*n_cor);
    double *inv_bread_beef;
    inv_bread_beef=double_vec(4*n_cor*n_cor);
    double *v_covmat;
    v_covmat=double_vec(4*n_cor*n_cor);
    
    int count=0;
    char optc='C'; //by col
    char optr='R'; //by row

    double *v_yfit_1_yfit=double_vec(*n_subject);
    double *v_y_yfit=double_vec(*n_subject);
    
//FILE *file; /////
    for (i=0;i<*n_subject;i++)
    {
        v_yfit_1_yfit[i]=v_yfit[i]*(1-v_yfit[i]);
        v_y_yfit[i]=v_y[i]-v_yfit[i];
    }
    
    //printf("%s %d %s\n","Total", *nsnp, "iterations...");
    for (i=0;i<*nsnp-1;i++)
    //for (i=0;i<1;i++)
    {
        //if (i % 10 ==0)
        //{
        //    printf("%d %s",i,"...");
        //    fflush(stdout);
        //}
        for (j=i+1;j<*nsnp;j++)
	//for (j=1;j<4;j++)
        {
            generate_modlematrix(*n_subject,*ncol_xmat,v_xmat,v_genotype,i,optc,v_xmat1);
            generate_modlematrix(*n_subject,*ncol_xmat,v_xmat,v_genotype,i,optr,v_xmat1t);
            generate_modlematrix(*n_subject,*ncol_xmat,v_xmat,v_genotype,j,optc,v_xmat2);
            generate_modlematrix(*n_subject,*ncol_xmat,v_xmat,v_genotype,j,optr,v_xmat2t);
            multiplicationbyrow(v_xmat1,*n_subject,n_cor,v_yfit_1_yfit,v_xmat_yfit_1_yfit);
            mydgemm(n_cor,n_cor,*n_subject,v_xmat1t,v_xmat_yfit_1_yfit,bread1);

            multiplicationbyrow(v_xmat2,*n_subject,n_cor,v_yfit_1_yfit,v_xmat_yfit_1_yfit);
            mydgemm(n_cor,n_cor,*n_subject,v_xmat2t,v_xmat_yfit_1_yfit,bread2);  
            //combine bread1 and bread2
            count=0;
            for (ii=0;ii<n_cor;ii++)
            {
                for (jj=0;jj<n_cor;jj++)
                {
                    bread[count]=bread1[ii*n_cor+jj];
                    count++;
                }
                count=count+n_cor;
            }
            for (ii=0;ii<n_cor;ii++)
            {
                count=count+n_cor;
                for (jj=0;jj<n_cor;jj++)
                {
                    bread[count]=bread2[ii*n_cor+jj];
                    count++;
                }
            }
            dqrinv(bread, n_cor*2, pow(10,-8), inv_bread); 
            multiplicationbyrow(v_xmat1,*n_subject,n_cor,v_y_yfit,score1);
			multiplicationbyrow(v_xmat2,*n_subject,n_cor,v_y_yfit,score2);
            //cbind score1 and score2
            for (count=0;count<(*n_subject)*2*n_cor;count++)
            {
                if (count<(*n_subject)*n_cor)
                {
                    score[count]=score1[count];
                }else
                {
                    score[count]=score2[count-(*n_subject)*n_cor];
                }
            }
            //transpose
            for (ii=0;ii<*n_subject;ii++)
            {
                for (jj=0;jj<2*n_cor;jj++)
                {
                    scoret[ii*2*n_cor+jj]=score[jj*(*n_subject)+ii];
                }
            }

            
            mydgemm(2*n_cor,2*n_cor,*n_subject,scoret,score,beef);
            mydgemm(2*n_cor,2*n_cor,2*n_cor,inv_bread,beef,inv_bread_beef);
            mydgemm(2*n_cor,2*n_cor,2*n_cor,inv_bread_beef,inv_bread,v_covmat);
            
            v_cormat[i*(*nsnp)+j]=v_cormat[j*(*nsnp)+i]=v_covmat[(n_cor+2)*(2*n_cor)+2]/sqrt(v_covmat[2*2*n_cor+2]*v_covmat[(n_cor+2)*2*n_cor+n_cor+2]);
            //printf("%d %d %f\n",i,j,v_cormat[i*(*nsnp)+j]);    
        }
    }
    for (i=0;i<(*nsnp);i++)
    {
        v_cormat[i*(*nsnp)+i]=1;
    }

    //file = fopen("debug.txt", "w");
    //print_vector_double(v_xmat_yfit_1_yfit,*n_subject*n_cor,file);
    //print_vector_double(bread1,n_cor*n_cor,file);
    //print_vector_double(v_cormat,*nsnp*(*nsnp),file);
    //fclose(file);  
    Free(v_yfit_1_yfit);
    Free(v_y_yfit);    
    Free(v_xmat1);
    Free(v_xmat1t);
    Free(v_xmat2);
    Free(v_xmat2t);
    Free(v_xmat_yfit_1_yfit);
    Free(bread1);
    Free(bread2);
    Free(bread);
    Free(inv_bread);
    Free(score);
    Free(scoret);
    Free(score1);
    Free(score2);
    Free(beef);
    Free(inv_bread_beef);
    Free(v_covmat);

    //printf("%s","\n");
    //double a[5][3] = {{1,1,1},{2,3,4},{3,5,2},{4,2,5},{5,4,3}};
    //double a[5*3] = {1,2,3,4,5,1,3,5,2,4,1,4,2,5,3};
    //double a[3*5]={1,1,1,2,3,4,3,5,2,4,2,5,5,4,3};
    //double a[3*3]={1,0,0,2,0,0,3,0,0};
    //double b[3*3]={1,0,0,1,0,0,1,0,0};
    //double b[3][2] = {{-10,-3},{12,14},{14,12}};
    //double b[3*2]={-10,12,14,-3,14,12};
    //double *test;
    //char test1='T';
    //test=mydgemm(test1,3,2,3,a,b);
    
    //    double a[3*3]={1,1,0,0,1,0,0,0,1};
    //    double *test;
    //    test=dqrinv(a, 3, pow(10,-8));
    

    
}


double **double_matrix(int nrow, int ncol){
    int i;
    double **m;
    m=(double **) Calloc(nrow, double *);
    //if (!m) errmsg("mem alloc failure 1 in double_matrix");
    for (i=0;i<nrow;i++) {
        m[i]=(double *) Calloc(ncol,double);
        //if (!m[i]) errmsg("mem alloc failure 2 in double_matrix");
    }
    return m;
}

double *double_vec(int n){
    
    double *v;
    v=(double *) Calloc(n, double);
    //if (!v) errmsg("mem alloc failure in double_vec");
    return v;
}

void print_matrix_double(double **m, int nrow, int ncol, FILE *file){
    int i,j;
    for (i=0;i<nrow;i++) {
        for (j=0;j<ncol;j++){
            if (j==0) fprintf(file,"\n%.4e",m[i][j]);
            else fprintf(file,"\t%.4e",m[i][j]);
        }
    }
}

void print_vector_double(double *m, int n, FILE *file){
    int j;
    for (j=0;j<n;j++){
        //if (j==0) fprintf(file,"\n%.4e",m[j]);
        fprintf(file,"%f\n",m[j]);
    }
    
}
double **double_vec_to_mat(double *Yvec, int nrow, int ncol, char opt){
    int i,j,k;
    double **Y;
    Y = double_matrix(nrow,ncol);
    k=0;
    if (opt == 'c' || opt =='C') //by col first
    {
        for (i=0;i<ncol;i++){ //R: first column,then row
            for (j=0;j<nrow;j++) {
                Y[j][i]=Yvec[k];
                k++;
            }
        }
    }else
    {
        for (i=0;i<nrow;i++){ //R: first row,then col
            for (j=0;j<ncol;j++) {
                Y[j][i]=Yvec[k];
                k++;
            }
        }

    }
	   	   return Y;
}

double *double_mat_to_vec(double **Ymat, int nrow, int ncol,char opt){
    int i,j,count=0;
    double *Y;
    Y=double_vec(nrow*ncol);
	if (opt == 'c' || opt =='C') //by col first
    {
        for (j=0;j<ncol;j++) {
            for (i=0;i<nrow;i++){
                Y[count]= Ymat[i][j];
                count++;
            }
        }
    }else //by row
    {
        for (i=0;i<nrow;i++){
            for (j=0;j<ncol;j++) {
                Y[count]= Ymat[i][j];
                count++;
            }
        }
    }
    
	   return Y;
}


int *int_vec(int n){
    int *v;
    v=(int *) Calloc(n, int);
    //if (!v) errmsg("mem alloc failure in int_vec");
    return v;
}


//static void errmsg(char *string){
    
    /* Function to emulate "stop" of S+ - see page 134, S Programing, by
     Venables and Ripley */
    
    //PROBLEM "%s", string RECOVER(NULL_ENTRY);
//}


void mydgemm (int M,int N,int K,double *A, double *B, double *C)
{
    //if transpose douleb_vector by row
    char TRANSA = 'N'; //by column
    char TRANSB = 'N';
    double ALPHA = 1.;
    double BETA = 0.;
    int LDA = M;
    int LDB = K;
    int LDC = M;
    //double *C;
    //C=double_vec(M*N);
    F77_CALL(dgemm)(&TRANSA, &TRANSB, &M, &N, &K, &ALPHA, A, &LDA, B, &LDB, &BETA, C, &LDC FCONE FCONE);
}

void dqrinv(double *xvec, int n, double tol, double *outvec){
    int i,j,count,rank,*pivot,info;
    double *x,*y,*coef,*qraux,*work;
    pivot=int_vec(n);
    x=double_vec(n*n);
    y=double_vec(n*n);
    coef=double_vec(n*n);
    qraux=double_vec(n);
    work=double_vec(2*n);
    //outvec=double_vec(n*n);
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
    Free(pivot);
    Free(x);
    Free(y);
    Free(coef);
    Free(qraux);
    Free(work);
}



void multiplicationbyrow(double *A,int m,int n,double *B,double *C)
{
    //row m,col n
    int i,j;
    //double *C=double_vec(m*n);
    for (i=0;i<m;i++)
    {
        for (j=0;j<n;j++)
        {
            C[j*m+i]=A[j*m+i]*B[i];
        }
    }
}

void myrnorm(int n,double miu,double sigma, double *z)
{
    
    int i;
//    double *z=double_vec(n);
    GetRNGstate();
    for (i=0;i<n;i++)
    {
        z[i]=rnorm(miu,sigma);
    }
    PutRNGstate();
}

double myrnorm1(double miu,double sigma)
{
    double z;    
    GetRNGstate();
    z=rnorm(miu,sigma);
    PutRNGstate();
    return z;
}

//for qsort
int compare (const void * a, const void * b)
{
    if ( *(double*)a < *(double*)b )
        return -1;
    else if (*(double*)a == *(double*)b )
        return 0;
    else
        return 1;
}



