/*
 *  BART: Bayesian Additive Regression Trees
 *  Copyright (C) 2017-2018 Robert McCulloch, Rodney Sparapani
 *                          and Charles Spanbauer
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
 *  - To make it easier to compile, I move the function definitions in the separate 
 *  .cpp file to this file, and merge them with declaration.
 *  
 *  - Add initial version of aug.
 *
 *  These modifications comply with the terms of the GNU General Public License 
 *  version 2 (GPL-2).
 */

#ifndef GUARD_bart_h
#define GUARD_bart_h

#include "tree.h"
#include "treefuns.h"
#include "info.h"
#include "bartfuns.h"
#include "bd.h"
#include <ctime>

class bart {
public:
   bart():m(200),t(m),pi(),p(0),n(0),x(0),y(0),xi(),allfit(0),r(0),ftemp(0),di(),dartOn(false),aug(false) {};
   bart(size_t im):m(im),t(m),pi(),p(0),n(0),x(0),y(0),xi(),allfit(0),r(0),ftemp(0),di(),dartOn(false),aug(false) {};
   bart(const bart& ib):m(ib.m),t(m),pi(ib.pi),p(0),n(0),x(0),y(0),xi(),allfit(0),r(0),ftemp(0),di(),dartOn(false),aug(false)
   {
     this->t = ib.t;
   };
   ~bart(){
     if(allfit) delete[] allfit;
     if(r) delete[] r;
     if(ftemp) delete[] ftemp;
    };
   //------------------------------
   //operators
   bart& operator=(const bart& rhs){
     if(&rhs != this) {
     
     this->t = rhs.t;
     this->m = t.size();
     
     this->pi = rhs.pi;
     
     p=0;n=0;x=0;y=0;
     xi.clear();
     
     if(allfit) {delete[] allfit; allfit=0;}
     if(r) {delete[] r; r=0;}
     if(ftemp) {delete[] ftemp; ftemp=0;}
     
   }
   return *this;};
   //------------------------------
   //get,set
   size_t getm() {return m;}
   void setm(size_t m) {{
     t.resize(m);
     this->m = t.size();
     
     if(allfit && (xi.size()==p)) predict(p,n,x,allfit);
     }};
   
   void makexinfo_bart(size_t p, size_t n, double *x, xinfo& xi, int *nc)
   {
     double xinc;
     
     //compute min and max for each x
     std::vector<double> minx(p,INFINITY);
     std::vector<double> maxx(p,-INFINITY);
     double xx;
     for(size_t i=0;i<p;i++) {
       for(size_t j=0;j<n;j++) {
         xx = *(x+p*j+i);
         if(xx < minx[i]) minx[i]=xx;
         if(xx > maxx[i]) maxx[i]=xx;
       }
     }
     //make grid of nc cutpoints between min and max for each x.
     xi.resize(p);
     for(size_t i=0;i<p;i++) {
       xinc = (maxx[i]-minx[i])/(nc[i]+1.0);
       xi[i].resize(nc[i]);
       for(size_t j=0;j<nc[i];j++) xi[i][j] = minx[i] + (j+1)*xinc;
     }
   }
   

   void setdata(size_t p, size_t n, double *x, double *y, int* nc){
     this->p=p; this->n=n; this->x=x; this->y=y;
     if(xi.size()==0) makexinfo_bart(p,n,&x[0],xi,nc);
     
     if(allfit) delete[] allfit;
     allfit = new double[n];
     predict(p,n,x,allfit);
     
     if(r) delete[] r;
     r = new double[n];
     
     if(ftemp) delete[] ftemp;
     ftemp = new double[n];
     
     di.n=n; di.p=p; di.x = &x[0]; di.y=r;
     if(nv.size() > 0){
       //cout << "nv:"<<nv[0] << std::endl;
       //cout << "pv:"<<pv[0] << std::endl;
       /*for(size_t j=0;j<p;j++){
        nv[j] = (0);
        pv[j] = (1/(double)p);
        }*/
     }else{
       for(size_t j=0;j<p;j++){
         nv.push_back(0);
         pv.push_back(1/(double)p);
       }
     }
   }
   void setdata(size_t p, size_t n, double *x, double *y, size_t numcut=100){
     int* nc = new int[p];
     for(size_t i=0; i<p; ++i) nc[i]=numcut;
     this->setdata(p, n, x, y, nc);
     delete [] nc;
   }
   void setpi(pinfo& pi) {this->pi = pi;}
   void setprior(double alpha, double beta, double tau)
      {pi.alpha=alpha; pi.mybeta = beta; pi.tau=tau;}
   void setdart(double _a, double _b, double _rho, bool _aug, bool _dart, 
		double _theta=0., double _omega=1.) {
     this->a=_a; this->b=_b; this->rho=_rho; this->aug=_aug; 
     this->dart=_dart; this->omega=_omega; 
     if(_theta==0.){
       this->const_theta=false;
       this->theta=1.;
     }
     else{
       this->const_theta=true;
       this->theta=_theta;
     }
}
   void startdart() {this->dartOn=!(this->dartOn);}
   void settau(double tau) {pi.tau=tau;}
   tree& gettree(size_t i ) { return t[i];}
   xinfo& getxinfo() {return xi;}
   void setxinfo(xinfo& _xi){
     size_t p=_xi.size();
     xi.resize(p);
     for(size_t i=0;i<p;i++) {
       size_t nc=_xi[i].size();
       xi[i].resize(nc);
       for(size_t j=0;j<nc;j++) xi[i][j] = _xi[i][j];
     }
   };
   std::vector<size_t>& getnv() {return nv;}
   std::vector<double>& getpv() {return pv;}
   double gettheta() {return theta;}
   //------------------------------
   //public methods
   void birth(size_t i, size_t nid,size_t v, size_t c, double ml, double mr)
         {t[i].birth(nid,v,c,ml,mr);}
   void death(size_t i,size_t nid, double mu)
         {t[i].death(nid,mu);}
   void pr();
   void tonull() {for(size_t i=0;i!=t.size();i++) t[i].tonull();}
   
   void fit2(tree& t, xinfo& xi, size_t p, size_t n, double *x,  double* fv)
   {
     tree::tree_p bn;
     for(size_t i=0;i<n;i++) {
       //cout << "i:" << i << std::endl;
       bn = t.bn(x+i*p,xi);
       
       fv[i] = bn->gettheta();
       
     }
   }
   
   void predict(size_t p, size_t n, double *x, double *fp){ 
     double *fptemp = new double[n];
     
     for(size_t j=0;j<n;j++) fp[j]=0.0;
     for(size_t j=0;j<m;j++) {
       fit2(t[j],xi,p,n,x,fptemp);
       for(size_t k=0;k<n;k++) fp[k] += fptemp[k];
     }
     
     delete[] fptemp;
    }
   
   bool cansplit_bart(tree::tree_p n, xinfo& xi)
   {
     int L,U;
     bool v_found = false; //have you found a variable you can split on
     size_t v=0;
     while(!v_found && (v < xi.size())) { //invar: splitvar not found, vars left
       L=0; U = xi[v].size()-1;
       n->rg(v,&L,&U);
       if(U>=L) v_found=true;
       v++;
     }
     return v_found;
   }
   
   void allsuff_bart(tree& x, xinfo& xi, dinfo& di, tree::npv& bnv, std::vector<size_t>& nv, std::vector<double>& syv)
   {
     tree::tree_cp tbn; //the pointer to the bottom node for the current observations
     size_t ni;         //the  index into vector of the current bottom node
     double *xx;        //current x
     
     bnv.clear();
     x.getbots(bnv);
     
     typedef tree::npv::size_type bvsz;
     bvsz nb = bnv.size();
     nv.resize(nb);
     syv.resize(nb);
     
     std::map<tree::tree_cp,size_t> bnmap;
     for(bvsz i=0;i!=bnv.size();i++) {bnmap[bnv[i]]=i;nv[i]=0;syv[i]=0.0;}
     
     for(size_t i=0;i<di.n;i++) {
       xx = di.x + i*di.p;
       tbn = x.bn(xx,xi);
       ni = bnmap[tbn];
       
       ++(nv[ni]);
       syv[ni] += di.y[i];
     }
   }
   
   void drmu_bart(tree& t, xinfo& xi, dinfo& di, pinfo& pi, double sigma, rn& gen)
   {
     tree::npv bnv;
     std::vector<size_t> nv;
     std::vector<double> syv;
     allsuff_bart(t,xi,di,bnv,nv,syv);
     
     for(tree::npv::size_type i=0;i!=bnv.size();i++) 
       bnv[i]->settheta(drawnodemu_bart(nv[i],syv[i],pi.tau,sigma,gen));
   }
   
   void getgoodvars_bart(tree::tree_p n, xinfo& xi,  std::vector<size_t>& goodvars)
   {
     goodvars.clear();
     int L,U;
     for(size_t v=0;v!=xi.size();v++) {//try each variable
       L=0; U = xi[v].size()-1;
       n->rg(v,&L,&U);
       if(U>=L) goodvars.push_back(v);
     }
   }
   
   void bprop_bart(tree& x, xinfo& xi, pinfo& pi, tree::npv& goodbots, double& PBx, tree::tree_p& nx, size_t& v, size_t& c, double& pr, std::vector<size_t>& nv, std::vector<double>& pv, bool aug, rn& gen)
   {
     //draw bottom node, choose node index ni from list in goodbots
     size_t ni = floor(gen.uniform()*goodbots.size());
     nx = goodbots[ni]; //the bottom node we might birth at
     
     //draw v,  the variable
     std::vector<size_t> goodvars; //variables nx can split on
     int L,U; //for cutpoint draw
     // Degenerate Trees Strategy (Assumption 2.2)
     if(!aug){
       getgoodvars_bart(nx,xi,goodvars);
       gen.set_wts(pv);
       v = gen.discrete();
       L=0; U=xi[v].size()-1;
       if(!std::binary_search(goodvars.begin(),goodvars.end(),v)){ // if variable is bad
         c=nx->getbadcut(v); // set cutpoint of node to be same as next highest interior node with same variable
       }
       else{ // if variable is good
         nx->rg(v,&L,&U);
         c = L + floor(gen.uniform()*(U-L+1)); // draw cutpoint usual way
       }
     }
     // Modified Data Augmentation Strategy (Mod. Assumption 2.1)
     // Set c_j = s_j*E[G] = s_j/P{picking a good var}
     // where  G ~ Geom( P{picking a good var} )
     else{
       std::vector<size_t> allvars; //all variables
       std::vector<size_t> badvars; //variables nx can NOT split on
       std::vector<double> pgoodvars; //vector of goodvars probabilities (from S, our Dirichlet vector draw)
       std::vector<double> pbadvars; //vector of badvars probabilities (from S,...)
       getgoodvars_bart(nx,xi,goodvars);
       //size_t ngoodvars=goodvars.size();
       size_t nbadvars=0; //number of bad vars
       double smpgoodvars=0.; //P(picking a good var)
       double smpbadvars=0.; //P(picking a bad var)
       //	size_t nbaddraws=0; //number of draws at a particular node
       //this loop fills out badvars, pgoodvars, pbadvars, 
       //there may be a better way to do this...
       for(size_t j=0;j<pv.size();j++){
         allvars.push_back(j);
         if(goodvars[j-nbadvars]!=j) {
           badvars.push_back(j);
           pbadvars.push_back(pv[j]);
           smpbadvars+=pv[j];
           nbadvars++;
         }
         else {
           pgoodvars.push_back(pv[j]);
           smpgoodvars+=pv[j];
         }
       }
       //set the weights for variable draw and draw a good variable
       gen.set_wts(pgoodvars);
       v = goodvars[gen.discrete()];
       if(nbadvars!=0){ // if we have bad vars then we need to augment, otherwise we skip
         //gen.set_p(smpgoodvars); // set parameter for G
         //nbaddraws=gen.geometric(); // draw G = g ~ Geom
         // for each bad variable, set its c_j equal to its expected count
         /*
          gen.set_wts(pbadvars); 
          for(size_t k=0;k!=nbaddraws;k++) {
          nv[badvars[gen.discrete()]]++;
          }
          */
         for(size_t j=0;j<nbadvars;j++)
           nv[badvars[j]]=nv[badvars[j]]+(1/smpgoodvars)*(pv[badvars[j]]/smpbadvars); 	  
       }
       /*
        size_t vi = floor(gen.uniform()*goodvars.size()); //index of chosen split variable
        v = goodvars[vi];
        */
       
       //draw c, the cutpoint
       //int L,U;
       L=0; U = xi[v].size()-1;
       nx->rg(v,&L,&U);
       c = L + floor(gen.uniform()*(U-L+1)); //U-L+1 is number of available split points
     }
     //--------------------------------------------------
     //compute things needed for metropolis ratio
     
     double Pbotx = 1.0/goodbots.size(); //proposal prob of choosing nx
     size_t dnx = nx->depth();
     double PGnx = pi.alpha/pow(1.0 + dnx,pi.mybeta); //prior prob of growing at nx
     
     double PGly, PGry; //prior probs of growing at new children (l and r) of proposal
     if(goodvars.size()>1) { //know there are variables we could split l and r on
       PGly = pi.alpha/pow(1.0 + dnx+1.0,pi.mybeta); //depth of new nodes would be one more
       PGry = PGly;
     } else { //only had v to work with, if it is exhausted at either child need PG=0
       if((int)(c-1)<L) { //v exhausted in new left child l, new upper limit would be c-1
         PGly = 0.0;
       } else {
         PGly = pi.alpha/pow(1.0 + dnx+1.0,pi.mybeta);
       }
       if(U < (int)(c+1)) { //v exhausted in new right child r, new lower limit would be c+1
         PGry = 0.0;
       } else {
         PGry = pi.alpha/pow(1.0 + dnx+1.0,pi.mybeta);
       }
     }
     
     double PDy; //prob of proposing death at y
     if(goodbots.size()>1) { //can birth at y because splittable nodes left
       PDy = 1.0 - pi.pb;
     } else { //nx was the only node you could split on
       if((PGry==0) && (PGly==0)) { //cannot birth at y
         PDy=1.0;
       } else { //y can birth at either l or r
         PDy = 1.0 - pi.pb;
       }
     }
     
     double Pnogy; //death prob of choosing the nog node at y
     size_t nnogs = x.nnogs();
     tree::tree_p nxp = nx->getp();
     if(nxp==0) { //no parent, nx is the top and only node
       Pnogy=1.0;
     } else {
       if(nxp->ntype() == 'n') { //if parent is a nog, number of nogs same at x and y
         Pnogy = 1.0/nnogs;
       } else { //if parent is not a nog, y has one more nog.
         Pnogy = 1.0/(nnogs+1.0);
       }
     }
     
     pr = (PGnx*(1.0-PGly)*(1.0-PGry)*PDy*Pnogy)/((1.0-PGnx)*Pbotx*PBx);
   }
   
   double drawnodemu_bart(size_t n, double sy, double tau, double sigma, rn& gen)
   {
     double s2 = sigma*sigma;
     double b = n/s2;
     double a = 1.0/(tau*tau);
     return (sy/s2)/(a+b) + gen.normal()/sqrt(a+b);
   }
   
   double lh_bart(size_t n, double sy, double sigma, double tau)
   {
     double s2 = sigma*sigma;
     double t2 = tau*tau;
     double k = n*t2+s2;
     return -.5*log(k) + ((t2*sy*sy)/(2.0*s2*k));
   }
   
   double pgrow_bart(tree::tree_p n, xinfo& xi, pinfo& pi)
   {
     if(cansplit_bart(n,xi)) {
       return pi.alpha/pow(1.0+n->depth(),pi.mybeta);
     } else {
       return 0.0;
     }
   }
   
   void dprop_bart(tree& x, xinfo& xi, pinfo& pi,tree::npv& goodbots, double& PBx, tree::tree_p& nx, double& pr, rn& gen)
   {
     //draw nog node, any nog node is a possibility
     tree::npv nognds; //nog nodes
     x.getnogs(nognds);
     size_t ni = floor(gen.uniform()*nognds.size());
     nx = nognds[ni]; //the nog node we might kill children at
     
     //--------------------------------------------------
     //compute things needed for metropolis ratio
     
     double PGny; //prob the nog node grows
     size_t dny = nx->depth();
     PGny = pi.alpha/pow(1.0+dny,pi.mybeta);
     
     //better way to code these two?
     double PGlx = pgrow_bart(nx->getl(),xi,pi);
     double PGrx = pgrow_bart(nx->getr(),xi,pi);
     
     double PBy;  //prob of birth move at y
     if(nx->ntype()=='t') { //is the nog node nx the top node
       PBy = 1.0;
     } else {
       PBy = pi.pb;
     }
     
     double Pboty;  //prob of choosing the nog as bot to split on when y
     int ngood = goodbots.size();
     if(cansplit_bart(nx->getl(),xi)) --ngood; //if can split at left child, lose this one
     if(cansplit_bart(nx->getr(),xi)) --ngood; //if can split at right child, lose this one
     ++ngood;  //know you can split at nx
     Pboty=1.0/ngood;
     
     double PDx = 1.0-PBx; //prob of a death step at x
     double Pnogx = 1.0/nognds.size();
     
     pr =  ((1.0-PGny)*PBy*Pboty)/(PGny*(1.0-PGlx)*(1.0-PGrx)*PDx*Pnogx);
   }
   
   double getpb_bart(tree& t, xinfo& xi, pinfo& pi, tree::npv& goodbots)
   {
     double pb;  //prob of birth to be returned
     tree::npv bnv; //all the bottom nodes
     t.getbots(bnv);
     for(size_t i=0;i!=bnv.size();i++)
       if(cansplit_bart(bnv[i],xi)) goodbots.push_back(bnv[i]);
     if(goodbots.size()==0) { //are there any bottom nodes you can split on?
       pb=0.0;
     } else {
       if(t.treesize()==1) pb=1.0; //is there just one node?
       else pb=pi.pb;
     }
    return pb;
   }
   
   void getsuff_bart(tree& x, tree::tree_p nx, size_t v, size_t c, xinfo& xi, dinfo& di, size_t& nl, double& syl, size_t& nr, double& syr)
   {
     double *xx;//current x
     nl=0; syl=0.0;
     nr=0; syr=0.0;
     
     for(size_t i=0;i<di.n;i++) {
       xx = di.x + i*di.p;
       if(nx==x.bn(xx,xi)) { //does the bottom node = xx's bottom node
         if(xx[v] < xi[v][c]) {
           nl++;
           syl += di.y[i];
         } else {
           nr++;
           syr += di.y[i];
         }
       }
     }
     
   }
   
   void getsuff_bart(tree& x, tree::tree_p l, tree::tree_p r, xinfo& xi, dinfo& di, size_t& nl, double& syl, size_t& nr, double& syr)
   {
     double *xx;//current x
     nl=0; syl=0.0;
     nr=0; syr=0.0;
     
     for(size_t i=0;i<di.n;i++) {
       xx = di.x + i*di.p;
       tree::tree_cp bn = x.bn(xx,xi);
       if(bn==l) {
         nl++;
         syl += di.y[i];
       }
       if(bn==r) {
         nr++;
         syr += di.y[i];
       }
     }
   }
   
   
   bool bd_bart(tree& x, xinfo& xi, dinfo& di, pinfo& pi, double sigma, 
           std::vector<size_t>& nv, std::vector<double>& pv, bool aug, rn& gen)
   {
     tree::npv goodbots;  //nodes we could birth at (split on)
     double PBx = getpb_bart(x,xi,pi,goodbots); //prob of a birth at x
     
     if(gen.uniform() < PBx) { //do birth or death
       
       //--------------------------------------------------
       //draw proposal
       tree::tree_p nx; //bottom node
       size_t v,c; //variable and cutpoint
       double pr; //part of metropolis ratio from proposal and prior
       bprop_bart(x,xi,pi,goodbots,PBx,nx,v,c,pr,nv,pv,aug,gen);
       
       //--------------------------------------------------
       //compute sufficient statistics
       size_t nr,nl; //counts in proposed bots
       double syl, syr; //sum of y in proposed bots
       getsuff_bart(x,nx,v,c,xi,di,nl,syl,nr,syr);
       
       //--------------------------------------------------
       //compute alpha
       double alpha=0.0, lalpha=0.0;
       double lhl, lhr, lht;
       if((nl>=5) && (nr>=5)) { //cludge?
         lhl = lh_bart(nl,syl,sigma,pi.tau);
         lhr = lh_bart(nr,syr,sigma,pi.tau);
         lht = lh_bart(nl+nr,syl+syr,sigma,pi.tau);
         
         alpha=1.0;
         lalpha = log(pr) + (lhl+lhr-lht) + log(sigma);
         lalpha = std::min(0.0,lalpha);
       }
       
       //--------------------------------------------------
       //try metrop
       double mul,mur; //means for new bottom nodes, left and right
       double uu = gen.uniform();
       bool dostep = (alpha > 0) && (log(uu) < lalpha);
       if(dostep) {
         mul = drawnodemu_bart(nl,syl,pi.tau,sigma,gen);
         mur = drawnodemu_bart(nr,syr,pi.tau,sigma,gen);
         x.birthp(nx,v,c,mul,mur);
         nv[v]++;
         return true;
       } else {
         return false;
       }
     } else {
       //--------------------------------------------------
       //draw proposal
       double pr;  //part of metropolis ratio from proposal and prior
       tree::tree_p nx; //nog node to death at
       dprop_bart(x,xi,pi,goodbots,PBx,nx,pr,gen);
       
       //--------------------------------------------------
       //compute sufficient statistics
       size_t nr,nl; //counts at bots of nx
       double syl, syr; //sum at bots of nx
       getsuff_bart(x, nx->getl(), nx->getr(), xi, di, nl, syl, nr, syr);
       
       //--------------------------------------------------
       //compute alpha
       double lhl, lhr, lht;
       lhl = lh_bart(nl,syl,sigma,pi.tau);
       lhr = lh_bart(nr,syr,sigma,pi.tau);
       lht = lh_bart(nl+nr,syl+syr,sigma,pi.tau);
       
       double lalpha = log(pr) + (lht - lhl - lhr) - log(sigma);
       lalpha = std::min(0.0,lalpha);
       
       //--------------------------------------------------
       //try metrop
       double mu;
       if(log(gen.uniform()) < lalpha) {
         mu = drawnodemu_bart(nl+nr,syl+syr,pi.tau,sigma,gen);
         nv[nx->getv()]--;
         x.deathp(nx,mu);
         return true;
       } else {
         return false;
       }
     }
   }
   
   
   
   void draw_theta0_bart(bool const_theta, double& theta, std::vector<double>& lpv,
                         double a, double b, double rho, rn& gen){
     // Draw sparsity parameter theta_0 (Linero calls it alpha); see Linero, 2018
     // theta / (theta + rho ) ~ Beta(a,b)
     // Set (a=0.5, b=1) for sparsity
     // Set (a=1, b=1) for non-sparsity
     // rho = p usually, but making rho < p increases sparsity
     if(!const_theta){
       size_t p=lpv.size();
       double sumlpv=0.,lse;
       
       std::vector<double> lambda_g (1000,0.);
       std::vector<double> theta_g (1000,0.);
       std::vector<double> lwt_g (1000,0.);
       for(size_t j=0;j<p;j++) sumlpv+=lpv[j];
       for(size_t k=0;k<1000;k++){
         lambda_g[k]=(double)(k+1)/1001.;
         theta_g[k]=(lambda_g[k]*rho)/(1.-lambda_g[k]);
         double theta_log_lik=lgamma(theta_g[k])-(double)p*lgamma(theta_g[k]/(double)p)+(theta_g[k]/(double)p)*sumlpv;
         double beta_log_prior=(a-1.)*log(lambda_g[k])+(b-1.)*log(1.-lambda_g[k]);
         //      cout << "SLP: " << sumlogpv << "\nTLL: " << theta_log_lik << "\nBLP: " << beta_log_prior << '\n';
         lwt_g[k]=theta_log_lik+beta_log_prior;      
       }
       
       double mx=lwt_g[0],sm=0.;
       for(size_t i=0;i<lwt_g.size();i++) if(lwt_g[i]>mx) mx=lwt_g[i];
       for(size_t i=0;i<lwt_g.size();i++){
         sm += std::exp(lwt_g[i]-mx);
       }
       lse= mx+log(sm);
       
       for(size_t k=0;k<1000;k++) {
         lwt_g[k]=exp(lwt_g[k]-lse);
         //      cout << "LWT: " << lwt_g[k] << '\n';
       }
       gen.set_wts(lwt_g);    
       theta=theta_g[gen.discrete()];
     } 
   }
   
   void fit3(tree& t, xinfo& xi, size_t p, size_t n, double *x,  double* fv)
   {
     tree::tree_p bn;
     for(size_t i=0;i<n;i++) {
       //cout << "i:" << i << std::endl;
       bn = t.bn(x+i*p,xi);
       
       fv[i] = bn->gettheta();
       
     }
   }
   
   void draw_s_bart(std::vector<size_t>& nv, std::vector<double>& lpv, double& theta, rn& gen){
     size_t p=nv.size();
     // Now draw s, the vector of splitting probabilities
     std::vector<double> _theta(p);
     for(size_t j=0;j<p;j++) _theta[j]=theta/(double)p+(double)nv[j];
     //gen.set_alpha(_theta);
     lpv=gen.log_dirichlet(_theta);
   }
   
   
   void draw(double sigma, rn& gen){
     for(size_t j=0;j<m;j++) {
       fit3(t[j],xi,p,n,x,ftemp);
       for(size_t k=0;k<n;k++) {
         allfit[k] = allfit[k]-ftemp[k];
         r[k] = y[k]-allfit[k];
       }
       aug = (aug != 0);
       bd_bart(t[j],xi,di,pi,sigma,nv,pv,aug,gen);
       drmu_bart(t[j],xi,di,pi,sigma,gen);
       fit3(t[j],xi,p,n,x,ftemp);
       for(size_t k=0;k<n;k++) allfit[k] += ftemp[k];
     }
     if(dartOn) {
       draw_s_bart(nv,lpv,theta,gen);
       draw_theta0_bart(const_theta,theta,lpv,a,b,rho,gen);
       for(size_t j=0;j<p;j++) pv[j]=::exp(lpv[j]);
     }
   }
//   void draw_s(rn& gen);
   double f(size_t i) {return allfit[i];}
protected:
   size_t m;  //number of trees
   std::vector<tree> t; //the trees
   pinfo pi; //prior and mcmc info
   //data
   size_t p,n; //x has dim p, n obserations
   double *x,*y;  //x is column stack, pxn
   xinfo xi; //cutpoint info
   //working
   double *allfit; //if the data is set, should be f(x)
   double *r;
   double *ftemp;
   dinfo di;
   bool dart,dartOn,aug,const_theta;
   double a,b,rho,theta,omega;
   std::vector<size_t> nv;
   std::vector<double> pv, lpv;
};

#endif
