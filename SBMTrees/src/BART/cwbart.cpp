/*
 *  BART: Bayesian Additive Regression Trees
 *  Copyright (C) 2017 Robert McCulloch and Rodney Sparapani
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/GPL-2
 */

/*
 *  Modifications by Jungang Zou, 2024.
 *  - To make it easier to compile, I commented the output function with %zu.
 *
 *  These modifications comply with the terms of the GNU General Public License 
 *  version 2 (GPL-2).
 */


#include "Rcpp.h"

#include "bart.h"
#include "tree.h"
#include "treefuns.h"
#include "info.h"
#include "bartfuns.h"
#include "bd.h"

#define TRDRAW(a, b) trdraw(a, b)
#define TEDRAW(a, b) tedraw(a, b)

// [[Rcpp::export]]
RcppExport SEXP cwbart(
    SEXP sexp_in,
    SEXP sexp_ip,
    SEXP sexp_inp,
    SEXP sexp_ix,
    SEXP sexp_iy,
    SEXP sexp_ixp,
    SEXP sexp_im,
    SEXP sexp_inc,
    SEXP sexp_ind,
    SEXP sexp_iburn,
    SEXP sexp_ipower,
    SEXP sexp_ibase,
    SEXP sexp_itau,
    SEXP sexp_inu,
    SEXP sexp_ilambda,
    SEXP sexp_isigest,
    SEXP sexp_iw,
    SEXP sexp_idart,
    SEXP sexp_itheta,
    SEXP sexp_iomega,
    SEXP sexp_igrp,
    SEXP sexp_ia,
    SEXP sexp_ib,
    SEXP sexp_irho,
    SEXP sexp_iaug,
    SEXP sexp_inkeeptrain,
    SEXP sexp_inkeeptest,
    SEXP sexp_inkeeptestme,
    SEXP sexp_inkeeptreedraws,
    SEXP sexp_inprintevery,
    SEXP sexp_Xinfo,
    bool verbose = false
)
{
  //--------------------------------------------------
  //process args
  size_t n = Rcpp::as<int>(sexp_in);
  size_t p = Rcpp::as<int>(sexp_ip);
  size_t np = Rcpp::as<int>(sexp_inp);
  Rcpp::NumericVector  xv(sexp_ix);
  double *ix = &xv[0];
  Rcpp::NumericVector  yv(sexp_iy); 
  double *iy = &yv[0];
  Rcpp::NumericVector  xpv(sexp_ixp);
  double *ixp = &xpv[0];
  size_t m = Rcpp::as<int>(sexp_im);
  Rcpp::IntegerVector _nc(sexp_inc);
  int *numcut = &_nc[0];
  size_t nd = Rcpp::as<int>(sexp_ind);
  size_t burn = Rcpp::as<int>(sexp_iburn);
  double mybeta = Rcpp::as<double>(sexp_ipower);
  double alpha = Rcpp::as<double>(sexp_ibase);
  double tau = Rcpp::as<double>(sexp_itau);
  double nu = Rcpp::as<double>(sexp_inu);
  double lambda = Rcpp::as<double>(sexp_ilambda);
  double sigma=Rcpp::as<double>(sexp_isigest);
  Rcpp::NumericVector  wv(sexp_iw); 
  double *iw = &wv[0];
  bool dart;
  if(Rcpp::as<int>(sexp_idart)==1) dart=true;
  else dart=false;
  double a = Rcpp::as<double>(sexp_ia);
  double b = Rcpp::as<double>(sexp_ib);
  double rho = Rcpp::as<double>(sexp_irho);
  bool aug;
  if(Rcpp::as<int>(sexp_iaug)==1) aug=true;
  else aug=false;
  double theta = Rcpp::as<double>(sexp_itheta);
  double omega = Rcpp::as<double>(sexp_iomega);
  Rcpp::IntegerVector _grp(sexp_igrp);
  int *grp = &_grp[0];
  size_t nkeeptrain = Rcpp::as<int>(sexp_inkeeptrain);
  size_t nkeeptest = Rcpp::as<int>(sexp_inkeeptest);
  size_t nkeeptestme = Rcpp::as<int>(sexp_inkeeptestme);
  size_t nkeeptreedraws = Rcpp::as<int>(sexp_inkeeptreedraws);
  size_t printevery = Rcpp::as<int>(sexp_inprintevery);
  Rcpp::NumericMatrix Xinfo(sexp_Xinfo);
  
   //return data structures (using Rcpp)
   Rcpp::NumericVector trmean(n); //train
   Rcpp::NumericVector temean(np);
   Rcpp::NumericVector sdraw(nd+burn);
   Rcpp::NumericMatrix trdraw(nkeeptrain,n);
   Rcpp::NumericMatrix tedraw(nkeeptest,np);
//   Rcpp::List list_of_lists(nkeeptreedraws*treesaslists);
   Rcpp::NumericMatrix varprb(nkeeptreedraws,p);
   Rcpp::IntegerMatrix varcnt(nkeeptreedraws,p);

   //random number generation
   arn gen;

   bart bm(m);

   if(Xinfo.size()>0) {
     xinfo _xi;
     _xi.resize(p);
     for(size_t i=0;i<p;i++) {
       _xi[i].resize(numcut[i]);
       //Rcpp::IntegerVector cutpts(Xinfo[i]);
       for(size_t j=0;j<(size_t)numcut[i];j++) _xi[i][j]=Xinfo(i, j);
     }
     bm.setxinfo(_xi);
   }


   for(size_t i=0;i<n;i++) trmean[i]=0.0;
   for(size_t i=0;i<np;i++) temean[i]=0.0;
   if(verbose)
     printf("*****Into main of wbart\n");
   //-----------------------------------------------------------

   size_t skiptr,skipte,skipteme,skiptreedraws;
   if(nkeeptrain) {skiptr=nd/nkeeptrain;}
   else skiptr = nd+1;
   if(nkeeptest) {skipte=nd/nkeeptest;}
   else skipte=nd+1;
   if(nkeeptestme) {skipteme=nd/nkeeptestme;}
   else skipteme=nd+1;
   if(nkeeptreedraws) {skiptreedraws = nd/nkeeptreedraws;}
   else skiptreedraws=nd+1;

   //--------------------------------------------------
   //print args
   if(verbose){
     printf("*****Data:\n");
     printf("y1,yn: %lf, %lf\n",iy[0],iy[n-1]);
     printf("x1,x[n*p]: %lf, %lf\n",ix[0],ix[n*p-1]);
     if(np) printf("xp1,xp[np*p]: %lf, %lf\n",ixp[0],ixp[np*p-1]);
     //printf("*****Number of Trees: %zu\n",m);
     printf("*****Prior:beta,alpha,tau,nu,lambda: %lf,%lf,%lf,%lf,%lf\n",
            mybeta,alpha,tau,nu,lambda);
     printf("*****sigma: %lf\n",sigma);
     printf("*****w (weights): %lf ... %lf\n",iw[0],iw[n-1]);
     cout << "*****Dirichlet:sparse,theta,omega,a,b,rho,augment: " 
          << dart << ',' << theta << ',' << omega << ',' << a << ',' 
          << b << ',' << rho << ',' << aug << endl;
     //printf("*****nkeeptrain,nkeeptest,nkeeptestme,nkeeptreedraws: %zu,%zu,%zu,%zu\n",
    //        nkeeptrain,nkeeptest,nkeeptestme,nkeeptreedraws);
     //printf("*****printevery: %zu\n",printevery);
     
   }
   
   //--------------------------------------------------
   //heterbart bm(m);
   bm.setprior(alpha,mybeta,tau);
   bm.setdata(p,n,ix,iy,numcut);
   bm.setdart(a,b,rho,aug,dart,theta,omega);

   //--------------------------------------------------
   //sigma
   //gen.set_df(n+nu);
   double *svec = new double[n];
   for(size_t i=0;i<n;i++) svec[i]=iw[i]*sigma;

   //--------------------------------------------------

   std::stringstream treess;  //string stream to write trees to  
   treess.precision(10);
   treess << nkeeptreedraws << " " << m << " " << p << endl;
   // dart iterations
   std::vector<double> ivarprb (p,0.);
   std::vector<size_t> ivarcnt (p,0);

   //--------------------------------------------------
   //temporary storage
   //out of sample fit
   double* fhattest=0; //posterior mean for prediction
   if(np) { fhattest = new double[np]; }
   double restemp=0.0,rss=0.0;

   //--------------------------------------------------
   //mcmc
   if(verbose)
     printf("\nMCMC\n");
   //size_t index;
   size_t trcnt=0; //count kept train draws
   size_t tecnt=0; //count kept test draws
   size_t temecnt=0; //count test draws into posterior mean
   size_t treedrawscnt=0; //count kept bart draws
   bool keeptest,keeptestme,keeptreedraw;

   time_t tp;
   int time1 = time(&tp);
   xinfo& xi = bm.getxinfo();
   size_t total=nd+burn;
   
   for(size_t i=0;i<total;i++) {
     if(verbose){
       //printf("iteration %zu\n",i);
       //if(i%printevery==0) printf("done %zu (out of %zu)\n",i,total);
      }
      
      if(i==(burn/2)&&dart) bm.startdart();
      //draw bart
      bm.draw(sigma,gen);
      
      if(i>=burn) {
         for(size_t k=0;k<n;k++) trmean[k]+=bm.f(k);
         if(nkeeptrain && (((i-burn+1) % skiptr) ==0)) {
            //index = trcnt*n;;
            //for(size_t k=0;k<n;k++) trdraw[index+k]=bm.f(k);
            for(size_t k=0;k<n;k++) TRDRAW(trcnt,k)=bm.f(k);
            trcnt+=1;
         }
         keeptest = nkeeptest && (((i-burn+1) % skipte) ==0) && np;
         keeptestme = nkeeptestme && (((i-burn+1) % skipteme) ==0) && np;
         if(keeptest || keeptestme) bm.predict(p,np,ixp,fhattest);
         if(keeptest) {
            //index=tecnt*np;
            //for(size_t k=0;k<np;k++) tedraw[index+k]=fhattest[k];
            for(size_t k=0;k<np;k++) TEDRAW(tecnt,k)=fhattest[k];
            tecnt+=1;
         }
         if(keeptestme) {
            for(size_t k=0;k<np;k++) temean[k]+=fhattest[k];
            temecnt+=1;
         }
         keeptreedraw = nkeeptreedraws && (((i-burn+1) % skiptreedraws) ==0);
         if(keeptreedraw) {
//	   #ifndef NoRcpp
//	   Rcpp::List lists(m*treesaslists);
//	   #endif

            for(size_t j=0;j<m;j++) {
	      treess << bm.gettree(j);
/*      
	      #ifndef NoRcpp
	      varcount.row(treedrawscnt)=varcount.row(treedrawscnt)+bm.gettree(j).tree2count(p);
	      if(treesaslists) lists(j)=bm.gettree(j).tree2list(xi, 0., 1.);
	      #endif
*/
	    }
      #ifndef NoRcpp
//	    if(treesaslists) list_of_lists(treedrawscnt)=lists;
	    ivarcnt=bm.getnv();
	    ivarprb=bm.getpv();
	    size_t k=(i-burn)/skiptreedraws;
	    for(size_t j=0;j<p;j++){
	      varcnt(k,j)=ivarcnt[j];
	      //varcnt(i-burn,j)=ivarcnt[j];
	      varprb(k,j)=ivarprb[j];
	      //varprb(i-burn,j)=ivarprb[j];
	    }
      #else
	    varcnt.push_back(bm.getnv());
	    varprb.push_back(bm.getpv());
	    #endif

            treedrawscnt +=1;
         }
      }
   }
   int time2 = time(&tp);
   printf("time: %ds\n",time2-time1);
   for(size_t k=0;k<n;k++) trmean[k]/=nd;
   for(size_t k=0;k<np;k++) temean[k]/=temecnt;
   printf("check counts\n");
   //printf("trcnt,tecnt,temecnt,treedrawscnt: %zu,%zu,%zu,%zu\n",trcnt,tecnt,temecnt,treedrawscnt);
   //--------------------------------------------------
   //PutRNGstate();

   if(fhattest) delete[] fhattest;
   if(svec) delete [] svec;

   //--------------------------------------------------
   //return
#ifndef NoRcpp
   Rcpp::List ret;
   //ret["sigma"]=sdraw;
   ret["yhat.train.mean"]=trmean;
   ret["yhat.train"]=trdraw;
   ret["yhat.test.mean"]=temean;
   ret["yhat.test"]=tedraw;
   //ret["varcount"]=varcount;
   ret["varcount"]=varcnt;
   ret["varprob"]=varprb;

   //for(size_t i=0;i<m;i++) {
    //  bm.gettree(i).pr();
   //}

   Rcpp::List xiret(xi.size());
   for(size_t i=0;i<xi.size();i++) {
      Rcpp::NumericVector vtemp(xi[i].size());
      std::copy(xi[i].begin(),xi[i].end(),vtemp.begin());
      xiret[i] = Rcpp::NumericVector(vtemp);
   }

   Rcpp::List treesL;
   //treesL["nkeeptreedraws"] = Rcpp::wrap<int>(nkeeptreedraws); //in trees
   //treesL["ntree"] = Rcpp::wrap<int>(m); //in trees
   //treesL["numx"] = Rcpp::wrap<int>(p); //in cutpoints
   treesL["cutpoints"] = xiret;
   treesL["trees"]=Rcpp::CharacterVector(treess.str());
//   if(treesaslists) treesL["lists"]=list_of_lists;
   ret["treedraws"] = treesL;

   return ret;
#else

#endif

}
