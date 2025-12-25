#include <R.h>
#include <vector>
#include <cmath>
#include <iostream>
#include <random>

using namespace std;

double dpoislog(double k, double lambda);

void sampleBranching(std::vector<double> &ts, std::vector<double> &marks, double M0,   double mu, double logK, double alpha, double c, double p, std::vector<int> &branching);

double kappaBranchingPosterior(std::vector<double> &marks, std::vector<int> &numtriggered, double logK, double alpha, double M0);

double hBranchingPosterior(std::vector<double> &zs, double c, double p);
	
void estimateETASBranching(std::vector<double> &ts, std::vector<double> &marks, std::vector<int> &branching, double maxTime, int sims, int numMCMCSamples, double mu, double logK, double alpha, double c, double p, std::vector<double> &mus, std::vector<double> &logKs, std::vector<double> &alphas, std::vector<double> &cs, std::vector<double> &ps);


double ETASposterior(std::vector<double> &ts, std::vector<double> &marks, double maxTime, double M0, double mu, double logK, double alpha, double c, double p);

void estimateETASDirect(std::vector<double> &ts, std::vector<double> &marks, double maxTime, double M0, int sims, double mu, double logK, double alpha, double c, double p, std::vector<double> &mus, std::vector<double> &logKs, std::vector<double> &alphas, std::vector<double> &cs, std::vector<double> &ps);


double hBranchingPosteriorInteraction(std::vector<double> &ts, std::vector<double> &marks, std::vector<double> &zs, double maxTime, std::vector<double> &kappaevals, double c, double p);

double kappaBranchingPosteriorInteraction(std::vector<double> &ts, std::vector<double> &marks, std::vector<int> &numtriggered, double M0, std::vector<double> &Hevals, double logK, double alpha);

void estimateETASBranchingInteraction(std::vector<double> &ts, std::vector<double> &marks, std::vector<int> &branching, double maxTime, double M0, int sims, int numMCMCSamples, double mu, double logK, double alpha, double c, double p, std::vector<double> &mus, std::vector<double> &logKs, std::vector<double> &alphas, std::vector<double> &cs, std::vector<double> &ps);


double dpoislog(double k, double lambda) {
	if (k==0) {
		return(k * log(lambda) - 0 - lambda);
	}
	if (k==1) {
		return(k * log(lambda) - 0 - lambda);
	}
	if (k==2) {
		return(k * log(lambda) - 0.6931472 - lambda);
	}
	if (k==3) {
		return(k * log(lambda) - 1.791759 - lambda);
	}
    return k * log(lambda) - lgamma(k + 1.0) - lambda;
}
  
  
  //maybe change this to avoid using push_back/clear. Instead, use [] to put elements into vector. See http://stackoverflow.com/questions/20168051/why-push-back-is-slower-than-operator-for-an-previous-allocated-vector
void sampleBranching(std::vector<double> &ts, std::vector<double> &marks, double M0,  double mu, double logK, double alpha, double c, double p, std::vector<int> &branching) {
	int n = ts.size();
	int parent;
	double temp;
	double K = exp(logK);
		
    std::random_device rd;
    std::mt19937 gen(rd());
    
	branching.clear();
	branching.reserve(n);
	branching.push_back(0);
	
	std::vector<double> probs;
	probs.reserve(n);
	
	std::vector<double> kappaevals;
	kappaevals.reserve(n);
	
	//precompute
	for (int i = 0; i < n; i++) {
		kappaevals.push_back(K*exp(alpha*(marks[i]-M0)));
	}
	
	for (int i = 1; i < n; i++) {
		probs.clear();
		probs.push_back(mu);
		
		for (int j = 0; j < i; j++) { //check this iteratures through...
			temp = kappaevals[j]*(p-1)*pow(c,p-1)*pow(ts[i]-ts[j]+c,-p);
			probs.push_back(temp);
		}
		std::discrete_distribution<> d(probs.begin(), probs.end()); //correct?
		
		/*for (int j = 0; j < probs.size(); j++) {
			cout << probs[j] << " ";
		}
		cout << "\n";*/
		
		parent = d(gen);
		branching.push_back(parent); 
	}
}
	

double kappaBranchingPosterior(std::vector<double> &marks, std::vector<int> &numtriggered, double logK, double alpha, double M0) {
	if (alpha < 0 || alpha > 10) {
		return(-9999999);
	}
	
	double loglik = 0;
	int n = marks.size();
	double lambda;
	
	for (int i = 0 ; i < n; i++) {
		lambda = exp(logK + alpha*(marks[i]-M0));
		loglik += dpoislog(numtriggered[i], lambda);
	}
	return(loglik);
}
	
double hBranchingPosterior(std::vector<double> &zs, double c, double p) {
	if (c <= 0 || p <= 1 || c > 10 || p > 10) {
		return(-9999999);
	}
	double loglik = 0;
	int n = zs.size();
	for (int i=0; i<n; i++) {
		//loglik += (p-1)*pow(c,p-1) * pow(zs[i]+c, -p);
		loglik += log(p-1) + (p-1)*log(c) - p*log(zs[i]+c);
	}
	return(loglik);
}
	
	

void estimateETASBranching(std::vector<double> &ts, std::vector<double> &marks, std::vector<int> &branching, double maxTime, double M0, int sims, int numMCMCSamples, double mu, double logK, double alpha, double c, double p, std::vector<double> &mus, std::vector<double> &logKs, std::vector<double> &alphas, std::vector<double> &cs, std::vector<double> &ps) {
	
	int n = ts.size(), numbackground;
	double currposterior,newposterior, newc,newp,newlogK,newalpha;
	double mualpha = 0.1, mubeta = 0.1; //prior params
	
    std::random_device rd;
    std::mt19937 gen(rd());
    std::normal_distribution<> rnorm(0, 0.1);
    std::uniform_real_distribution<> runif(0, 1);
    
	for (int s=0; s<sims; s++) {
	
		sampleBranching(ts,marks,M0,mu,logK,alpha,c,p,branching);
		
		std::vector<int> numtriggered(n,0);
		std::vector<double> z;
		z.reserve(n);
		
		numbackground = 0;
		for (int i=0; i<n; i++) {
			if (branching[i] > 0) {
				numtriggered[branching[i]-1]++; //worry aobut indexing here...
				z.push_back(ts[i]-ts[branching[i]-1]);
			} else {
				numbackground++;
			}
		}
		
		
  		std::gamma_distribution<> rgamma(mualpha+numbackground, 1/(mubeta+maxTime));
		mu = rgamma(gen);
		mus.push_back(mu);
		
		currposterior = hBranchingPosterior(z,c,p);
		for (int i=0; i < numMCMCSamples; i++) {
			newc = c + rnorm(gen);
			newp = p + rnorm(gen);
			newposterior = hBranchingPosterior(z,newc,newp);
			if (runif(gen) < exp(newposterior-currposterior)) {
				c = newc;
				p = newp;
				currposterior = newposterior;
			}
		}
		cs.push_back(c);
		ps.push_back(p);
		
		currposterior = kappaBranchingPosterior(marks,numtriggered,logK,alpha,M0);
		for (int i=0; i < 100; i++) {
			newlogK = logK + rnorm(gen);
			newalpha = alpha + rnorm(gen);
			newposterior = kappaBranchingPosterior(marks,numtriggered,newlogK,newalpha,M0);
			if (runif(gen) < exp(newposterior-currposterior)) {
				logK = newlogK;
				alpha = newalpha;
				currposterior = newposterior;
			}
		}
		logKs.push_back(logK);
		alphas.push_back(alpha);
		
		if (s % 100 == 0) {
			Rprintf("Generated %d samples...\n",s);
		}
	}
}

/************** DIRECT FUNCTIONS***********/
//DOENST HAVE MU PRIOR IN YET, NEED DGAMMA
double ETASposterior(std::vector<double> &ts, std::vector<double> &marks, double maxTime, double M0, double mu, double logK, double alpha, double c, double p) {
	if (c<0 || p<1 || c>10 || p>10 || alpha <0 || alpha >10 || mu < 0 || mu > 10) {
		return(-999999);
	}
	
	double part1, part2, part3, temp;
	double K = exp(logK);
	int n = ts.size();
	
	part1 = log(mu);
	for (int i = 1; i < n; i++) {
		temp = mu;
		for (int j = 0; j < i; j++) {
			temp +=  ((p-1)*pow(c,p-1)*K*exp(alpha*(marks[j]-M0)))/pow(ts[i]-ts[j]+c,p);
		}
		part1 += log(temp);
	}
	
	part2 = mu*maxTime;
	
	part3 = 0;
	for (int i = 0; i<n; i++) {
		temp = (1 - pow(c,p-1)/pow(maxTime-ts[i]+c,p-1));
		part3 += K*exp(alpha*(marks[i]-M0))*temp;
	}
	
	//double prior = log(dgamma(mu,0.1,0.1)) + log(dnorm(alpha,0,10));
	double prior=0;
	return(prior + part1 - part2 - part3);
}



void estimateETASDirect(std::vector<double> &ts, std::vector<double> &marks, double maxTime, double M0, int sims, double mu, double logK, double alpha, double c, double p, std::vector<double> &mus, std::vector<double> &logKs, std::vector<double> &alphas, std::vector<double> &cs, std::vector<double> &ps) {
	double currposterior,newposterior, newmu,newc,newp,newlogK,newalpha;
	
    std::random_device rd;
    std::mt19937 gen(rd());
    std::normal_distribution<> murnorm(0, 0.05);
    std::normal_distribution<> logKrnorm(0, 0.15);
    std::normal_distribution<> alpharnorm(0, 0.15);
    std::normal_distribution<> crnorm(0, 0.25);
    std::normal_distribution<> prnorm(0, 0.30);
    
    std::uniform_real_distribution<> runif(0, 1);
    
    currposterior = ETASposterior(ts, marks, maxTime, M0, mu, logK, alpha, c, p);
	
	for (int s=0; s<sims; s++) {
		newmu = mu + murnorm(gen);
		newposterior = ETASposterior(ts, marks, maxTime, M0, newmu, logK, alpha, c, p);
		if (runif(gen) < exp(newposterior-currposterior)) {
			mu = newmu;
			currposterior = newposterior;
		}
		mus.push_back(mu);
		
		newlogK = logK + logKrnorm(gen);
		newposterior = ETASposterior(ts, marks, maxTime, M0, mu, newlogK, alpha, c, p);
		if (runif(gen) < exp(newposterior-currposterior)) {
			logK = newlogK;
			currposterior = newposterior;
		}
		logKs.push_back(logK);

		newalpha = alpha + alpharnorm(gen);
		newposterior = ETASposterior(ts, marks, maxTime, M0, mu, logK, newalpha, c, p);
		if (runif(gen) < exp(newposterior-currposterior)) {
			alpha = newalpha; 
			currposterior = newposterior;
		}
		alphas.push_back(alpha);
		
		newc = c + crnorm(gen);
		newposterior = ETASposterior(ts, marks, maxTime, M0, mu, logK, alpha, newc, p);
		if (runif(gen) < exp(newposterior-currposterior)) {
			c = newc;
			currposterior = newposterior;
		}
		cs.push_back(c);
		
		newp = p + prnorm(gen);
		newposterior = ETASposterior(ts, marks, maxTime, M0, mu, logK, alpha, c, newp);
		if (runif(gen) < exp(newposterior-currposterior)) {
			p = newp;
			currposterior = newposterior;
		}
		ps.push_back(p);
		
		if (s % 100 == 0) {
			Rprintf("Generated %d samples so far...\n",s);
		}
	}
}

/******** BRANCHING FUNCTIONS WITH INTERACTION (IE WITHOUT INFINITE TIME APPROXIMATION**/

//this shows up when we cant do the infinite time approximatoin...
double hBranchingPosteriorInteraction(std::vector<double> &ts, std::vector<double> &marks, std::vector<double> &z, double maxTime, std::vector<double> &kappaevals, double c, double p) {
	if (c <= 0 || p <= 1 || c > 10 || p > 10) {
		return(-9999999);
	}
	
	double loglik = 0, temp;
	int n = ts.size();
	for (int i = 0; i < n; i++) {
		//temp = K*exp(alpha*(marks[i]-M0));
		temp = 1 - (pow(c,p-1) / pow(maxTime-ts[i]+c,p-1)); //H
		loglik = loglik - kappaevals[i]*temp;
	}
	for (int i = 0; i < z.size(); i++) {
		loglik += log(p-1) + (p-1)*log(c) - p*log(z[i]+c);
	}
	return(loglik);
}
	
double kappaBranchingPosteriorInteraction(std::vector<double> &ts, std::vector<double> &marks, std::vector<int> &numtriggered, double M0, std::vector<double> &Hevals, double logK,  double alpha) {
	if (alpha < 0 || alpha > 10) {
		return(-9999999);
	}
	double K = exp(logK);
	double loglik = 0, temp;
	int n = ts.size();
	for (int i = 0; i < n; i++) {
		temp = K*exp(alpha*(marks[i]-M0));
		//temp2 = 1 - (pow(c,p-1) / pow(maxTime-ts[i]+c,p-1)); //H
		loglik = loglik - temp*Hevals[i];
		loglik += numtriggered[i] * log(temp);
	}
	return(loglik);
}


void estimateETASBranchingInteraction(std::vector<double> &ts, std::vector<double> &marks, std::vector<int> &branching, double maxTime, double M0, int sims, int numMCMCSamples, double mu, double logK, double alpha, double c, double p, std::vector<double> &mus, std::vector<double> &logKs, std::vector<double> &alphas, std::vector<double> &cs, std::vector<double> &ps) {
	//cout << "Interaction sampler\n";
	int n = ts.size(), numbackground;
	double currposterior,newposterior, newc,newp,newlogK,newalpha;
	double mualpha = 0.1, mubeta = 0.1; //prior params
	std::vector<double> kappaevals;
	kappaevals.reserve(n);
	std::vector<double> Hevals;
	Hevals.reserve(n);
	std::vector<double> z;
	z.reserve(n);
	
    std::random_device rd;
    std::mt19937 gen(rd());
    std::normal_distribution<> rnorm(0, 0.1);
    std::uniform_real_distribution<> runif(0, 1);
    
    double K;
	for (int s=0; s<sims; s++) {
	
		sampleBranching(ts,marks,M0,mu,logK,alpha,c,p,branching);
		
		z.clear();
		std::vector<int> numtriggered(n,0);
		numbackground = 0;
		for (int i=0; i<n; i++) {
			if (branching[i] > 0) {
				numtriggered[branching[i]-1]++; //worry aobut indexing here...
				z.push_back(ts[i]-ts[branching[i]-1]);
			} else {
				numbackground++;
			}
		}
		
		
  		std::gamma_distribution<> rgamma(mualpha+numbackground, 1/(mubeta+maxTime));
		mu = rgamma(gen);
		mus.push_back(mu);
		
		K = exp(logK);
		kappaevals.clear();
		for (int i=0; i<n; i++) {
			kappaevals.push_back(K*exp(alpha*(marks[i]-M0)));
		}
		
		//try sampling c and p seperately, not together...
		currposterior = hBranchingPosteriorInteraction(ts,marks,z,maxTime,kappaevals,c,p);
		for (int i=0; i < numMCMCSamples; i++) {
			newc = c + rnorm(gen);
			newp = p + rnorm(gen);
			newposterior = hBranchingPosteriorInteraction(ts,marks,z,maxTime,kappaevals,newc,newp);
			if (runif(gen) < exp(newposterior-currposterior)) {
				c = newc;
				p = newp;
				currposterior = newposterior;
			}
		}
		cs.push_back(c);
		ps.push_back(p);

		Hevals.clear();
		for (int i=0; i<n; i++) {
			Hevals.push_back(1 - (pow(c,p-1) / pow(maxTime-ts[i]+c,p-1)));
		}
		
		currposterior = kappaBranchingPosteriorInteraction(ts,marks,numtriggered,M0,Hevals,logK,alpha);
		for (int i=0; i < 100; i++) {
			newlogK = logK + rnorm(gen);
			newalpha = alpha + rnorm(gen);
			newposterior = kappaBranchingPosteriorInteraction(ts,marks,numtriggered,M0,Hevals,newlogK,newalpha);
			if (runif(gen) < exp(newposterior-currposterior)) {
				logK = newlogK;
				alpha = newalpha;
				currposterior = newposterior;
			}
		}
		logKs.push_back(logK);
		alphas.push_back(alpha);
		
		if (s % 100 == 0) {
			Rprintf("Generated %d samples so far...\n",s);
		}
	}
}


extern "C" {
	//if approx==1 then use infinite time approximation
	void estimateETASBranchingC(double *ts, double *marks, int *branching, int *n, double *maxTime, double *M0, int *sims, int *numMCMCSamples, int *approx, double *mu, double *logK, double *alpha, double *c, double *p, double *mus, double *logKs, double *alphas, double *cs, double *ps) {
		std::vector<double> vts(ts,ts + *n);
		std::vector<double> vmarks(marks,marks + *n);
		std::vector<int> vbranching(branching,branching + *n);
		
   	 	std::vector<double> vmus;
    	std::vector<double> vlogKs;
    	std::vector<double> valphas;
   	 	std::vector<double> vcs;
    	std::vector<double> vps;
    	vmus.reserve(*sims);
    	vlogKs.reserve(*sims);
    	valphas.reserve(*sims);
    	vcs.reserve(*sims);
    	vps.reserve(*sims);
    
    	if (*approx==1) {
			estimateETASBranching(vts, vmarks, vbranching, *maxTime, *M0,*sims, *numMCMCSamples, *mu, *logK, *alpha, *c, *p, vmus, vlogKs, valphas, vcs, vps);
		} else {
			estimateETASBranchingInteraction(vts, vmarks, vbranching, *maxTime, *M0,*sims, *numMCMCSamples, *mu, *logK, *alpha, *c, *p, vmus, vlogKs, valphas, vcs, vps);
		}
		std::copy(vmus.begin(),vmus.end(),mus);
        std::copy(vlogKs.begin(),vlogKs.end(),logKs);
        std::copy(valphas.begin(),valphas.end(),alphas);
		std::copy(vcs.begin(),vcs.end(),cs);
        std::copy(vps.begin(),vps.end(),ps);
		return;
	}
	
		void estimateETASDirectC(double *ts, double *marks, int *n, double *maxTime, double *M0, int *sims, double *mu, double *logK, double *alpha, double *c, double *p, double *mus, double *logKs, double *alphas, double *cs, double *ps) {
		std::vector<double> vts(ts,ts + *n);
		std::vector<double> vmarks(marks,marks + *n);
		
   	 	std::vector<double> vmus;
    	std::vector<double> vlogKs;
    	std::vector<double> valphas;
   	 	std::vector<double> vcs;
    	std::vector<double> vps;
    	vmus.reserve(*sims);
    	vlogKs.reserve(*sims);
    	valphas.reserve(*sims);
    	vcs.reserve(*sims);
    	vps.reserve(*sims);
    
    
		estimateETASDirect(vts, vmarks, *maxTime, *M0,*sims, *mu, *logK, *alpha, *c, *p, vmus, vlogKs, valphas, vcs, vps);
		
		std::copy(vmus.begin(),vmus.end(),mus);
        std::copy(vlogKs.begin(),vlogKs.end(),logKs);
        std::copy(valphas.begin(),valphas.end(),alphas);
		std::copy(vcs.begin(),vcs.end(),cs);
        std::copy(vps.begin(),vps.end(),ps);
		return;
	}
	
	void estimateBranchingC(double *ts, double *marks, int *n, double *M0, int *sims, double *mu, double *logK, double *alpha, double *c, double *p, int *branchings) {
		std::vector<int> vbranching;
		std::vector<int> vbranchings;
		vbranching.reserve(*n);
		vbranchings.reserve(*n * *sims);
		
		std::vector<double> vts(ts,ts + *n);
		std::vector<double> vmarks(marks,marks + *n);
		
		for (int i = 0; i < *sims; i++) {
			sampleBranching(vts, vmarks, *M0, *mu, *logK, *alpha, *c, *p, vbranching);
			//cout << vbranching.size() << " " << vbranchings.size() << "\n";
			
			for (int j = 0; j < *n; j++) {
				vbranchings.push_back(vbranching[j]);
			}
		}
        std::copy(vbranchings.begin(),vbranchings.end(),branchings);
		return;
	}
	
	void ETASlikelihood(double *ts, double *marks, int *n, double *maxTime, double *M0, double *mu, double *logK, double *alpha, double *c, double *p, double *loglik) {
		std::vector<double> vts(ts,ts + *n);
		std::vector<double> vmarks(marks,marks + *n);
	
		*loglik = ETASposterior(vts, vmarks, *maxTime, *M0, *mu, *logK, *alpha, *c, *p);
	}
}

    
