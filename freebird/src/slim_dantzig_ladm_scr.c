#include <stdio.h>
#include <stdlib.h>
#include <R.h>
// #include "math.h"
#include "mymath.h"
#include <time.h>


void dantzig_ladm_scr(double *Y0, double *X0, double *Y, double *X, double *XX, int *idx_scr, int num_scr, int dim, double *alp, double *beta, double * mu, double *T, double rho, int *ite, double lambda, int max_ite, double prec, int intercept, int flag, int nlamb)
{
    int j,k,m,w_idx,size_a,size_a1,size_a_pre;
    double gap_ext,max_dif,alp_dif,beta_dif,mu_dif,threshold,tmpd;
    int * idx_tmp;


    //start = clock();
    double *beta0 = (double*) malloc(num_scr*sizeof(double));
    double *beta1 = (double*) malloc(num_scr*sizeof(double));
    int *idx_a = (int*) malloc(num_scr*sizeof(int)); //active set
    int *idx_i = (int*) malloc(num_scr*sizeof(int)); //inactive set
    int *idx_a1 = (int*) malloc(num_scr*sizeof(int)); //active set
    int *idx_i1 = (int*) malloc(num_scr*sizeof(int)); //inactive set
    double *beta_tild = (double*) malloc(num_scr*sizeof(double));
    double *beta_pre = (double*) malloc(num_scr*sizeof(double));
    double *alp0 = (double*) malloc(num_scr*sizeof(double));
    double *alp_pre = (double*) malloc(num_scr*sizeof(double));
    double *alp_tild = (double*) malloc(num_scr*sizeof(double));
    double *mu0 = (double*) malloc(num_scr*sizeof(double));
    double *mu_grad = (double*) malloc(num_scr*sizeof(double));
    double *X_beta = (double*) malloc(num_scr*sizeof(double));
    double *y_i = (double*) malloc(num_scr*sizeof(double));
    double *Xy = (double*) malloc(num_scr*sizeof(double));
    double *g = (double*) malloc(num_scr*sizeof(double));
    double *r = (double*) malloc(num_scr*sizeof(double));
    //double *X = (double*) malloc(num_scr*num_scr*sizeof(double));
    //double *Y = (double*) malloc(num_scr*sizeof(double));
    //stop = clock();
    //time1 += stop - start;

    //start = clock();
    size_a = 0;
    for (j=0; j<num_scr; j++) {
        alp0[j] = alp[idx_scr[j]];
        beta0[j] = beta[idx_scr[j]];
        mu0[j] = mu[idx_scr[j]];
        if (beta0[j]==0) {
            idx_i[j] = 1;
        }
        else{
            idx_a[size_a] = j;
            size_a++;
            idx_i[j] = 0;
        }
        //printf("j=%d,alp=%f,beta=%f,mu=%f \n",j,alp0[j],beta0[j],mu0[j]);
    }

    //stop = clock();
    //time2 += stop - start;
    //start = clock();

    if(nlamb==0){
        for (j=0; j<num_scr; j++) {
            //if(j==0) printf("row%d ",j);
            for (k=j; k<num_scr; k++) {
                X[j*dim+k] = X0[idx_scr[j]*dim+idx_scr[k]];
                X[k*dim+j] = X[j*dim+k];
                //    if(j==0) printf(" %f ",X[j*num_scr+k]);
            }
            //if(j==0) printf("\n");
            Y[j] = Y0[idx_scr[j]];
        }
    }
    else{
        if(flag==1){
            for (j=0; j<num_scr; j++) {
                //if(j==0) printf("row%d ",j);
                for (k=j; k<num_scr; k++) {
                    X[j*dim+k] = X0[idx_scr[j]*dim+idx_scr[k]];
                    X[k*dim+j] = X[j*dim+k];
                    //    if(j==0) printf(" %f ",X[j*num_scr+k]);
                }
                //if(j==0) printf("\n");
                Y[j] = Y0[idx_scr[j]];
            }
        }
    }
    for (j=0; j<num_scr; j++) {
        X_beta[j] = 0;
        //if(j==0) printf("row%d ",j);
        for (k=0; k<num_scr; k++) {
            X_beta[j] += X[j*dim+k]*beta0[k];
            //    if(j==0) printf(" %f ",X[j*num_scr+k]);
        }
        //if(j==0) printf("\n");
    }

    if(nlamb==0){
        if(flag==1){
            for (j=0; j<num_scr; j++) {
                for (k=j; k<num_scr; k++) {
                    XX[j*dim+k] = 0;
                    for (m=0; m<num_scr; m++) {
                        XX[j*dim+k] += X[m*dim+k] * X[j*dim+m];
                    }
                    XX[k*dim+j] = XX[j*dim+k];
                }
            }
            *T = 0;
            for (j=0; j<num_scr; j++) {
                tmpd = 0;
                for (k=0; k<num_scr; k++) {
                    tmpd += fabs(XX[j*dim+k]);
                }
                *T = max(*T,tmpd);
            }
        }
    }
    else{
        if(flag==1){
            for (j=0; j<num_scr; j++) {
                for (k=j; k<num_scr; k++) {
                    XX[j*dim+k] = 0;
                    for (m=0; m<num_scr; m++) {
                        XX[j*dim+k] += X[m*dim+k] * X[j*dim+m];
                    }
                    XX[k*dim+j] = XX[j*dim+k];
                }
            }
            *T = 0;
            for (j=0; j<num_scr; j++) {
                tmpd = 0;
                for (k=0; k<num_scr; k++) {
                    tmpd += fabs(XX[j*dim+k]);
                }
                *T = max(*T,tmpd);
            }
        }
    }
    //stop = clock();
    //time3 += stop - start;
    //start = clock();
    //printf("T=%f \n",T);
    gap_ext = 1;
    *ite = 0;
    max_dif = 1;
    while((gap_ext !=0 || max_dif > prec) && *ite < max_ite){
        // update alpha
        alp_dif = 0;
        for(j=0; j<num_scr; j++){
            alp_pre[j] = alp0[j];
            alp_tild[j] = Y[j]-X_beta[j]-mu0[j];
            if (alp_tild[j]<=-lambda){
                alp0[j]=-lambda;
            }
            else {
                if (alp_tild[j]>=lambda){
                    alp0[j] = lambda;
                }
                else
                    alp0[j] = alp_tild[j];
            }
            alp_dif += fabs(alp_pre[j] - alp0[j]);
        }
        max_dif = alp_dif;

        // update omega
        for(j=0; j<num_scr; j++){
            y_i[j] = Y[j]-alp0[j]-mu0[j];
        }
        for(j=0; j<num_scr; j++){
            Xy[j] = 0;
            for(k=0; k<num_scr; k++){
                Xy[j] += X[j*dim+k]*y_i[k];
            }
        }
        size_a_pre = size_a;
        for(j=0; j<num_scr; j++){
            g[j] = 0;
            for(k=0; k<size_a; k++){
                w_idx = idx_a[k];
                g[j] += XX[w_idx*dim+j]*beta0[w_idx];
            }
            g[j] = g[j] - Xy[j];
        }
        //printf("Y=%f,y=%f,Xy=%f,g=%f \n",Y[0],y_i[0],Xy[0],g[0]);
        threshold = 1/(rho*(*T));
        lineaization_lasso_dantzig(XX, Xy, beta0, beta1, beta_tild, g, idx_a, idx_scr, &size_a, idx_a1, idx_i1, &size_a1, *T, threshold, intercept, num_scr);
        beta_dif = 0;
        for(j=0; j<num_scr; j++){
            beta_dif += fabs(beta0[j] - beta1[j]);
            beta0[j] = beta1[j];
        }
        max_dif = max_dif>beta_dif ? max_dif : beta_dif;
        size_a = size_a1;
        idx_tmp = idx_a; idx_a = idx_a1; idx_a1 = idx_tmp;
        idx_tmp = idx_i; idx_i = idx_i1; idx_i1 = idx_tmp;
        gap_ext = size_a - size_a_pre;

        // update mu
        mu_dif = 0;
        for(j=0; j<num_scr; j++){
            X_beta[j]=0;
            for(k=0; k<size_a; k++){
                w_idx = idx_a[k];
                X_beta[j]+=X[w_idx*dim+j]*beta1[w_idx];
            }
            mu_grad[j]=(alp0[j]+X_beta[j]-Y[j]);
            mu0[j] += mu_grad[j];
            mu_dif = fabs(mu_grad[j])>mu_dif ? fabs(mu_grad[j]) : mu_dif;
        }
        max_dif = max_dif>mu_dif ? max_dif : mu_dif;
        //if(ite_ext == 0)
        //    printf("te_ext=%d,alp=%f,beta=%f,mu=%f \n",ite_ext,alp[0],beta0[0],mu[0]);
        //if(ite_ext % 1000 == 999)
        //printf("ite_ext=%d,alp_dif=%f,beta_dif=%f,mu_dif=%f \n",ite_ext,alp_dif,beta_dif,mu_dif);
        (*ite)++;
    }

    //for (j=0; j<dim; j++) {
    //    alp0[j] = 0;
    //    beta[j] = 0;
    //    mu0[j] = 0;
    //}

    for (j=0; j<num_scr; j++) {
        alp[idx_scr[j]] = alp0[j];
        beta[idx_scr[j]] = beta0[j];
        mu[idx_scr[j]] = mu0[j];
    }
    //stop = clock();
    //time0 += stop - start;
    //printf("alp=%f,beta=%f,mu=%f \n\n",alp[0],beta0[0],mu[0]);

    //start = clock();
    free(beta0);
    free(beta1);
    free(idx_a);
    free(idx_i);
    free(idx_a1);
    free(idx_i1);
    free(beta_tild);
    free(beta_pre);
    free(alp0);
    free(alp_pre);
    free(alp_tild);
    free(mu0);
    free(mu_grad);
    free(X_beta);
    free(y_i);
    free(Xy);
    free(g);
    free(r);
    //free(X);
    //free(Y);
    //stop = clock();
    //time1 += stop - start;
}

void slim_dantzig_ladm_scr(double *Y0, double *X0, double *XX, double *beta, int *n, int *d, double *rho0, int *ite_int, int *ite_int1, int *ite_int2, int *num_scr_1, int *num_scr_2, int *idx_scr, int * idx_scr_1, int * idx_scr_2, double * gamma, double * lambda, int * nnlambda, int *max_ite, double *prec, int * intercept)
{
    int j,k,m,ndata,dim,nlambda,ite1,ite2,ite,max_ite0,max_ite1,max_ite2,num_scr,num_scr1,num_scr2,num_scr1_tmp,num_scr2_tmp,flag,flag1,flag2;
    double T,T1,T2,rho,zero,eps,eps1,eps2,ilambda;

    //time0 = 0;
    //time1 = 0;
    //time2 = 0;
    //time3 = 0;
    dim = *d;
    ndata = *n;
    rho = *rho0;
    T = *gamma;
    T1 = 0;
    T2 = 0;
    nlambda = *nnlambda;
    num_scr = dim;
    num_scr1 = *num_scr_1;
    num_scr2 = *num_scr_2;
    max_ite1 = *max_ite;
    max_ite2 = *max_ite;
    max_ite0 = ceil(max_ite2/10);
    eps1 = *prec;
    eps2 = *prec;
    eps = eps2*10;
    zero = 0;


    double *beta0 = (double*) malloc(dim*sizeof(double));
    double *alp = (double*) malloc(dim*sizeof(double));
    double *mu = (double*) malloc(dim*sizeof(double));
    int *idx_scr1 = (int*) malloc(dim*sizeof(int));
    int *idx_scr2 = (int*) malloc(dim*sizeof(int));
    double *X = (double*) malloc(dim*dim*sizeof(double));
    double *Y = (double*) malloc(dim*sizeof(double));
    double *X1 = (double*) malloc(dim*dim*sizeof(double));
    double *Y1 = (double*) malloc(dim*sizeof(double));
    double *X2 = (double*) malloc(dim*dim*sizeof(double));
    double *Y2 = (double*) malloc(dim*sizeof(double));
    double *XX1 = (double*) malloc(dim*dim*sizeof(double));
    double *XX2 = (double*) malloc(dim*dim*sizeof(double));

    for(j=0; j<dim; j++) {
        beta0[j] = 0;
        alp[j] = 0;
        mu[j] = 0;
    }
    for(j=0; j<num_scr1; j++) {
        idx_scr1[j] = idx_scr_1[j]-1;
    //    printf("%d=%d,%d ",j,idx_scr1[j],idx_scr_1[j]);
    }
    //printf("num_scr1=%d,j=%d \n \n",num_scr1,j);
    for(j=0; j<num_scr2; j++) {
        idx_scr2[j] = idx_scr_2[j]-1;
    //    printf("%d=%d,%d ",j,idx_scr2[j],idx_scr_2[j]);
    }
    //printf("num_scr2=%d,j=%d \n \n",num_scr2,j);
    for(j=0; j<num_scr; j++) {
        idx_scr[j] = idx_scr[j]-1;
    //    printf("%d=%d ",j,idx_scr[j]);
    }
    //printf("num_scr=%d,j=%d \n \n",num_scr,j);
    flag = 0;
    flag1 = 1;
    flag2 = 1;
    for(m=0; m<nlambda; m++) {
        ilambda = lambda[m];//*ndata

        dantzig_ladm_scr(Y0,X0,Y1,X1,XX1,idx_scr1,num_scr1,dim,alp,beta0,mu,&T1,rho,&ite1,ilambda,max_ite1,eps1,*intercept,flag1,m);
        //printf("m=%d,alp=%f,beta=%f,mu=%f,ite1=%d \n",m,alp[0],beta0[0],mu[0],ite1);
        dantzig_ladm_scr(Y0,X0,Y2,X2,XX2,idx_scr2,num_scr2,dim,alp,beta0,mu,&T2,rho,&ite2,ilambda,max_ite2,eps2,*intercept,flag2,m);
        //printf("m=%d,alp=%f,beta=%f,mu=%f,ite2=%d \n",m,alp[0],beta0[0],mu[0],ite2);
        dantzig_ladm_scr(Y0,X0,Y,X,XX,idx_scr,num_scr,dim,alp,beta0,mu,&T,rho,&ite,ilambda,max_ite0,eps,*intercept,flag,m);
        //printf("m=%d,alp=%f,beta=%f,mu=%f,ite=%d \n",m,alp[0],beta0[0],mu[0],ite);

        num_scr1_tmp = num_scr1;
        num_scr2_tmp = num_scr2;
        for(k=0;k<dim;k++){
            beta[m*dim+k] = beta0[k];
            if(beta0[k]!=0){
                for(j=0; j<num_scr1; j++) {
                    if(idx_scr1[j] == k) break;
                }
                if(j==num_scr1){
                    idx_scr1[num_scr1] = k;
                    num_scr1++;
                }

                for(j=0; j<num_scr2; j++) {
                    if(idx_scr2[j] == k) break;
                }
                if(j==num_scr2){
                    idx_scr2[num_scr2] = k;
                    num_scr2++;
                }
            }
        }
        if(num_scr1>num_scr1_tmp){
        //    printf("before, flag1=%d,num_scr1=%d \n",flag1,num_scr1);
        //    free(XX1);
        //    double *XX1 = (double*) malloc(num_scr1*num_scr1*sizeof(double));
            flag1=1;
        //    printf("after, flag1=%d,num_scr1=%d \n",flag1,num_scr1);
        }
        else{
        //    printf("before, flag1=%d,num_scr1=%d \n",flag1,num_scr1);
        //    free(XX1);
        //    double *XX1 = (double*) malloc(num_scr1*num_scr1*sizeof(double));
            flag1 = 0;
        //    printf("after, flag1=%d,num_scr1=%d \n",flag1,num_scr1);
        }
        if(num_scr2>num_scr2_tmp){
        //    printf("before, flag2=%d,num_scr2=%d  \n",flag2,num_scr2);
        //    free(XX2);
        //    double *XX2 = (double*) malloc(num_scr2*num_scr2*sizeof(double));
            flag2=1;
        //    printf("after, flag2=%d,num_scr2=%d  \n",flag2,num_scr2);
        }
        else{
        //    printf("before, flag2=%d,num_scr2=%d  \n",flag2,num_scr2);
        //    free(XX2);
        //    double *XX2 = (double*) malloc(num_scr2*num_scr2*sizeof(double));
            flag2 = 0;
        //    printf("after, flag2=%d,num_scr2=%d  \n",flag2,num_scr2);
        }
        //flag = 0;

        ite_int[m] = ite;
        ite_int1[m] = ite1;
        ite_int2[m] = ite2;
    }
    //printf("C time0=%f,time1=%f,time2=%f,time3=%f \n",((double)(time0))/CLOCKS_PER_SEC,((double)(time1))/CLOCKS_PER_SEC,((double)(time2))/CLOCKS_PER_SEC,((double)(time3))/CLOCKS_PER_SEC);
    free(beta0);
    free(alp);
    free(mu);
    free(idx_scr1);
    free(idx_scr2);
    //free(XX);
    free(X);
    free(Y);
    free(X1);
    free(Y1);
    free(X2);
    free(Y2);
    free(XX1);
    free(XX2);
}


void slim_dantzig_ladm_scr2(double *Y0, double *X0, double *XX, double *beta, int *n, int *d, double *rho0, int *ite_int, int *ite_int1, int *num_scr_1, int *idx_scr, int * idx_scr_1, double * gamma, double * lambda, int * nnlambda, int *max_ite, double *prec, int * intercept)
{
    int j,k,m,ndata,dim,nlambda,ite1,ite,max_ite0,max_ite1,num_scr,num_scr1,num_scr1_tmp,flag,flag1;
    double T,T1,rho,zero,eps,eps1,ilambda;

    //time0 = 0;
    //time1 = 0;
    //time2 = 0;
    //time3 = 0;
    dim = *d;
    ndata = *n;
    rho = *rho0;
    T = *gamma;
    T1 = 0;
    nlambda = *nnlambda;
    num_scr = dim;
    num_scr1 = *num_scr_1;
    max_ite1 = *max_ite;
    max_ite0 = ceil(max_ite1/10);
    eps1 = *prec;
    eps = eps1*10;
    zero = 0;


    double *beta0 = (double*) malloc(dim*sizeof(double));
    double *alp = (double*) malloc(dim*sizeof(double));
    double *mu = (double*) malloc(dim*sizeof(double));
    int *idx_scr1 = (int*) malloc(dim*sizeof(int));
    double *X = (double*) malloc(dim*dim*sizeof(double));
    double *Y = (double*) malloc(dim*sizeof(double));
    double *X1 = (double*) malloc(dim*dim*sizeof(double));
    double *Y1 = (double*) malloc(dim*sizeof(double));
    double *XX1 = (double*) malloc(dim*dim*sizeof(double));

    for(j=0; j<dim; j++) {
        beta0[j] = 0;
        alp[j] = 0;
        mu[j] = 0;
    }
    for(j=0; j<num_scr1; j++) {
        idx_scr1[j] = idx_scr_1[j]-1;
        //    printf("%d=%d,%d ",j,idx_scr1[j],idx_scr_1[j]);
    }
    //printf("num_scr1=%d,j=%d \n \n",num_scr2,j);
    for(j=0; j<num_scr; j++) {
        idx_scr[j] = idx_scr[j]-1;
        //    printf("%d=%d ",j,idx_scr[j]);
    }
    //printf("num_scr=%d,j=%d \n \n",num_scr,j);
    flag = 0;
    flag1 = 1;
    for(m=0; m<nlambda; m++) {
        ilambda = lambda[m];//*ndata

        dantzig_ladm_scr(Y0,X0,Y1,X1,XX1,idx_scr1,num_scr1,dim,alp,beta0,mu,&T1,rho,&ite1,ilambda,max_ite1,eps1,*intercept,flag1,m);
        //printf("m=%d,alp=%f,beta=%f,mu=%f,ite1=%d \n",m,alp[0],beta0[0],mu[0],ite1);
        dantzig_ladm_scr(Y0,X0,Y,X,XX,idx_scr,num_scr,dim,alp,beta0,mu,&T,rho,&ite,ilambda,max_ite0,eps,*intercept,flag,m);
        //printf("m=%d,alp=%f,beta=%f,mu=%f,ite=%d \n",m,alp[0],beta0[0],mu[0],ite);

        num_scr1_tmp = num_scr1;
        for(k=0;k<dim;k++){
            beta[m*dim+k] = beta0[k];
            if(beta0[k]!=0){
                for(j=0; j<num_scr1; j++) {
                    if(idx_scr1[j] == k) break;
                }
                if(j==num_scr1){
                    idx_scr1[num_scr1] = k;
                    num_scr1++;
                }
            }
        }
        if(num_scr1>num_scr1_tmp){
            //    printf("before, flag1=%d,num_scr1=%d \n",flag1,num_scr1);
            //    free(XX1);
            //    double *XX1 = (double*) malloc(num_scr1*num_scr1*sizeof(double));
            flag1=1;
            //    printf("after, flag1=%d,num_scr1=%d \n",flag1,num_scr1);
        }
        else{
            //    printf("before, flag1=%d,num_scr1=%d \n",flag1,num_scr1);
            //    free(XX1);
            //    double *XX1 = (double*) malloc(num_scr1*num_scr1*sizeof(double));
            flag1 = 0;
            //    printf("after, flag1=%d,num_scr1=%d \n",flag1,num_scr1);
        }
        //flag = 0;

        ite_int[m] = ite;
        ite_int1[m] = ite1;
    }
    //printf("C time0=%f,time1=%f,time2=%f,time3=%f \n",((double)(time0))/CLOCKS_PER_SEC,((double)(time1))/CLOCKS_PER_SEC,((double)(time2))/CLOCKS_PER_SEC,((double)(time3))/CLOCKS_PER_SEC);
    free(beta0);
    free(alp);
    free(mu);
    free(idx_scr1);
    //free(XX);
    free(X);
    free(Y);
    free(X1);
    free(Y1);
    free(XX1);
}
