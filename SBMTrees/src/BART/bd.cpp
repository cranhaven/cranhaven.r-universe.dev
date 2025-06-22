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

#include "bd.h"

bool cansplit_bd(tree::tree_p n, xinfo& xi)
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

double drawnodemu_bd(size_t n, double sy, double tau, double sigma, rn& gen)
{
  double s2 = sigma*sigma;
  double b = n/s2;
  double a = 1.0/(tau*tau);
  return (sy/s2)/(a+b) + gen.normal()/sqrt(a+b);
}

double lh_bd(size_t n, double sy, double sigma, double tau)
{
  double s2 = sigma*sigma;
  double t2 = tau*tau;
  double k = n*t2+s2;
  return -.5*log(k) + ((t2*sy*sy)/(2.0*s2*k));
}

void getgoodvars_bd(tree::tree_p n, xinfo& xi,  std::vector<size_t>& goodvars)
{
  goodvars.clear();
  int L,U;
  for(size_t v=0;v!=xi.size();v++) {//try each variable
    L=0; U = xi[v].size()-1;
    n->rg(v,&L,&U);
    if(U>=L) goodvars.push_back(v);
  }
}

void getsuff_bd(tree& x, tree::tree_p nx, size_t v, size_t c, xinfo& xi, dinfo& di, size_t& nl, double& syl, size_t& nr, double& syr)
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

void getsuff_bd(tree& x, tree::tree_p l, tree::tree_p r, xinfo& xi, dinfo& di, size_t& nl, double& syl, size_t& nr, double& syr)
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


void bprop_bd(tree& x, xinfo& xi, pinfo& pi, tree::npv& goodbots, double& PBx, tree::tree_p& nx, size_t& v, size_t& c, double& pr, std::vector<size_t>& nv, std::vector<double>& pv, bool aug, rn& gen)
{
  //draw bottom node, choose node index ni from list in goodbots
  size_t ni = floor(gen.uniform()*goodbots.size());
  nx = goodbots[ni]; //the bottom node we might birth at
  
  //draw v,  the variable
  std::vector<size_t> goodvars; //variables nx can split on
  int L,U; //for cutpoint draw
  // Degenerate Trees Strategy (Assumption 2.2)
  if(!aug){
    getgoodvars_bd(nx,xi,goodvars);
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
    getgoodvars_bd(nx,xi,goodvars);
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

double pgrow_bd(tree::tree_p n, xinfo& xi, pinfo& pi)
{
  if(cansplit_bd(n,xi)) {
    return pi.alpha/pow(1.0+n->depth(),pi.mybeta);
  } else {
    return 0.0;
  }
}

void dprop_bd(tree& x, xinfo& xi, pinfo& pi,tree::npv& goodbots, double& PBx, tree::tree_p& nx, double& pr, rn& gen)
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
  double PGlx = pgrow_bd(nx->getl(),xi,pi);
  double PGrx = pgrow_bd(nx->getr(),xi,pi);
  
  double PBy;  //prob of birth move at y
  if(nx->ntype()=='t') { //is the nog node nx the top node
    PBy = 1.0;
  } else {
    PBy = pi.pb;
  }
  
  double Pboty;  //prob of choosing the nog as bot to split on when y
  int ngood = goodbots.size();
  if(cansplit_bd(nx->getl(),xi)) --ngood; //if can split at left child, lose this one
  if(cansplit_bd(nx->getr(),xi)) --ngood; //if can split at right child, lose this one
  ++ngood;  //know you can split at nx
  Pboty=1.0/ngood;
  
  double PDx = 1.0-PBx; //prob of a death step at x
  double Pnogx = 1.0/nognds.size();
  
  pr =  ((1.0-PGny)*PBy*Pboty)/(PGny*(1.0-PGlx)*(1.0-PGrx)*PDx*Pnogx);
}

double getpb_bd(tree& t, xinfo& xi, pinfo& pi, tree::npv& goodbots)
{
  double pb;  //prob of birth to be returned
  tree::npv bnv; //all the bottom nodes
  t.getbots(bnv);
  for(size_t i=0;i!=bnv.size();i++)
    if(cansplit_bd(bnv[i],xi)) goodbots.push_back(bnv[i]);
    if(goodbots.size()==0) { //are there any bottom nodes you can split on?
      pb=0.0;
    } else {
      if(t.treesize()==1) pb=1.0; //is there just one node?
      else pb=pi.pb;
    }
    return pb;
}


bool bd(tree& x, xinfo& xi, dinfo& di, pinfo& pi, double sigma, 
	std::vector<size_t>& nv, std::vector<double>& pv, bool aug, rn& gen)
{
   tree::npv goodbots;  //nodes we could birth at (split on)
   double PBx = getpb_bd(x,xi,pi,goodbots); //prob of a birth at x

   if(gen.uniform() < PBx) { //do birth or death

      //--------------------------------------------------
      //draw proposal
      tree::tree_p nx; //bottom node
      size_t v,c; //variable and cutpoint
      double pr; //part of metropolis ratio from proposal and prior
      bprop_bd(x,xi,pi,goodbots,PBx,nx,v,c,pr,nv,pv,aug,gen);

      //--------------------------------------------------
      //compute sufficient statistics
      size_t nr,nl; //counts in proposed bots
      double syl, syr; //sum of y in proposed bots
      getsuff_bd(x,nx,v,c,xi,di,nl,syl,nr,syr);

      //--------------------------------------------------
      //compute alpha
      double alpha=0.0, lalpha=0.0;
      double lhl, lhr, lht;
      if((nl>=5) && (nr>=5)) { //cludge?
         lhl = lh_bd(nl,syl,sigma,pi.tau);
         lhr = lh_bd(nr,syr,sigma,pi.tau);
         lht = lh_bd(nl+nr,syl+syr,sigma,pi.tau);
   
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
         mul = drawnodemu_bd(nl,syl,pi.tau,sigma,gen);
         mur = drawnodemu_bd(nr,syr,pi.tau,sigma,gen);
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
      dprop_bd(x,xi,pi,goodbots,PBx,nx,pr,gen);

      //--------------------------------------------------
      //compute sufficient statistics
      size_t nr,nl; //counts at bots of nx
      double syl, syr; //sum at bots of nx
      getsuff_bd(x, nx->getl(), nx->getr(), xi, di, nl, syl, nr, syr);

      //--------------------------------------------------
      //compute alpha
      double lhl, lhr, lht;
      lhl = lh_bd(nl,syl,sigma,pi.tau);
      lhr = lh_bd(nr,syr,sigma,pi.tau);
      lht = lh_bd(nl+nr,syl+syr,sigma,pi.tau);

      double lalpha = log(pr) + (lht - lhl - lhr) - log(sigma);
      lalpha = std::min(0.0,lalpha);

      //--------------------------------------------------
      //try metrop
      double mu;
      if(log(gen.uniform()) < lalpha) {
         mu = drawnodemu_bd(nl+nr,syl+syr,pi.tau,sigma,gen);
	 nv[nx->getv()]--;
         x.deathp(nx,mu);
         return true;
      } else {
         return false;
      }
   }
}
