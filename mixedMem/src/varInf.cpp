#include "varInf.h"

// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]
double varInfC(mm_model model, int print,
               int printMod, int stepType, int maxTotalIter,
               int maxEIter, int maxAlphaIter, int maxThetaIter, int maxLSIter,
               double elboTol, double alphaTol, double thetaTol, double aNaught,
               double tau, int bMax, double bNaught, double bMult, int vCutoff, NumericVector holdConst)
{

    /*
    * Initializations
    */
    NumericVector iterReached(3); //Vector to keep track of whether any of the iteration limits are hit in the E-Step or M-Steps
    double converged_T = 1.0; //convergence criteria: (old_elbo_T - elbo_T)/old_elbo_T
    double old_elbo_T = 0.0; //value of elbo from previous iteration
    double elbo_T = compute_ELBO(model); //updated Elbo
    int nT = 0; //count of Total EM Steps

    //only run an E-step
    if (stepType == 0) {
        elbo_T = eStep_C(model, elbo_T, maxEIter, elboTol, iterReached);
        if((nT % printMod == 0) && (print==1)) {
            Rcout<<"E-Step: "<<elbo_T<<endl;
        }
    } else {     //go through E and M steps

        while((converged_T > elboTol) && (nT < maxTotalIter)) {

            nT++; //increment count of iterations

            //print if necessary
            if((nT % printMod == 0) && (print == 1)) {
                Rcout<<"Iter: "<<nT<<" Elbo: "<< elbo_T<<endl;
            }

            //store old Elbo
            old_elbo_T = elbo_T;

            // E Step
            elbo_T = eStep_C(model, elbo_T, maxEIter, elboTol, iterReached); //defined in eStep.cpp

            //print if necessary
            if((nT % printMod == 0) && (print==1)) {
                Rcout<<"E-Step: "<<elbo_T<<endl;
            }

            //M-step; choice of which parameters to update handled inside mStep_C function
            elbo_T = mStep_C(model, elbo_T, stepType, maxAlphaIter, maxThetaIter, maxLSIter,
                             alphaTol, thetaTol, aNaught, tau, bMax, bNaught, bMult, vCutoff, holdConst, iterReached); //defined in mStep.cpp

            //print if necessary
            if((nT % printMod == 0) && (print == 1)) {
                Rcout<<"M-Step: "<<elbo_T<<endl;
            }

            //update convergence criteria
            converged_T = (old_elbo_T - elbo_T)/old_elbo_T;
        }
    }

    //print results
    Rcout <<"Fit Complete! Elbo: " <<elbo_T<< " Iter: " << nT<<endl;

    //check if any of the iteration limits were hit
    if (nT == maxTotalIter) {
        Rcout<< "Warning: Max Total Iterations Reached!" <<endl;
    }

    if( iterReached[0] == 1) {
        Rcout<< "Warning: Max E-Step Iterations Reached!" <<endl;
    }

    if( iterReached[1] == 1) {
        Rcout << "Warning: Max Alpha Iterations Reached!" <<endl;
    }

    if ( iterReached[2] == 1 ) {
        Rcout<< "Warning: Max Theta Iterations Reached!" <<endl;
    }
    return(elbo_T);
}


double compute_ELBO(mm_model model)
{
    double t1,t2,t3,t4;
    double phi_sum;
    double elbo;
    int i,j,k,r,n;
    int T = model.getT();
    int K = model.getK();
    int J = model.getJ();
    double back_term;
    double dg_phi_sum;
    double phi_ik, delta_ijrnk;
    int Nijr;


    //Calculate first line and second line
    t1 = 0.0;
    t2 = 0.0;
    t3 = 0.0;
    t4 = 0.0;


    t1 = T*lgamma(sum(model.getAlpha())) - T*sum(lgamma(model.getAlpha()));
    for(i = 0; i < T; i++) {
        phi_sum = 0.0;
        for(k = 0; k < K; k++) {
            phi_sum += model.getPhi(i,k);
        }
        dg_phi_sum = boost::math::digamma(phi_sum);

        t4 += lgamma(phi_sum);
        for(k = 0; k < K; k++) {
            phi_ik = model.getPhi(i,k);
            back_term = (boost::math::digamma(phi_ik) - dg_phi_sum);
            t1+= (model.getAlpha(k)-1)*back_term;

            t4 += -lgamma(phi_ik);
            t4 += (phi_ik-1)*back_term;

            for(j = 0; j < J; j++) {
                for(r = 0; r < model.getR(j); r++) {
                    Nijr = model.getN(i,j,r);
                    for(n = 0; n < Nijr; n++) {
                        delta_ijrnk = model.getDelta(i,j,r,n,k);
                        t2 += delta_ijrnk*back_term;
                        t4 += delta_ijrnk*log(delta_ijrnk);
                    }
                }
            }
        }
    }

    //compute 3rd line
    t3 = compute_logf(model);

    elbo = t1+t2+t3-t4;

    //debug!
    if(!(elbo > -INFINITY)) {
        Rcout<< t1 <<" "<<t2 <<" "<<t3 <<" "<<t4 <<endl<<"Alpha: "<<endl;
        for(k = 0; k < K; k++) {
            Rcout<<model.getAlpha(k)<<" ";
        }
        Rcout<<endl;
    }
    return(elbo);
}


double compute_logf(mm_model model)
{
    double logf = 0.0;
    double back_term;
    int i,j,k,r,n,v, Nijr;

    for(i = 0; i < model.getT(); i++) {
        for(j = 0; j < model.getJ(); j++) {
            if(model.getDist(j) == BERNOULLI) {
                n = 0;
                v = 0;
                for(r = 0; r < model.getR(j); r++) {
                    for(k = 0; k < model.getK(); k++) {
                        logf += ( model.getObs(i,j,r,n) ? model.getDelta(i,j,r,n,k)*log(model.getTheta(j,k,v)) :
                                  model.getDelta(i,j,r,n,k)*log(1.0 - model.getTheta(j,k,v))) ;
                    }
                }
            } //end bernoulli
            else if(model.getDist(j)== MULTINOMIAL) {
                n = 0;
                for(r = 0; r < model.getR(j); r++) {
                    for(k = 0; k < model.getK(); k++) {
                        logf += model.getDelta(i,j,r,n,k)*log(model.getTheta(j,k,model.getObs(i,j,r,n)));
                    }
                }
            } //end Multinomial
            else if(model.getDist(j) == RANK) {
                for(r = 0; r < model.getR(j); r++) {
                    Nijr = model.getN(i,j,r);
                    for(k = 0; k < model.getK(); k++) {
                        back_term = 0.0;
                        for(n = 0; n < Nijr; n++) {
                            logf += -model.getDelta(i,j,r,n,k)*log(1.0 - back_term);
                            logf += model.getDelta(i,j,r,n,k)*log(model.getTheta(j,k,model.getObs(i,j,r,n))) ;
                            back_term += model.getTheta(j,k,model.getObs(i,j,r,n));
                        }
                    }
                }
            } //end Rank
        }
    }
    return(logf);
}


double alpha_Objective(mm_model model, vec alph)
{
    double objective;
    int T = model.getT();
    int K = model.getK();
    int i;
    int k;
    double back_term;
    double phi_sum;
    double dg_phi_sum;
    double sum_lgamma_alpha = 0.0;

    for(k = 0; k < K; k++) {
        sum_lgamma_alpha += lgamma(alph(k));
    }

    objective = T*lgamma(sum(alph)) - T*sum_lgamma_alpha;
    for(i = 0; i < T; i++) {
        phi_sum = 0.0;
        for(k = 0; k < K; k++) {
            phi_sum += model.getPhi(i,k);
        }
        dg_phi_sum = boost::math::digamma(phi_sum);

        for(k = 0; k < K; k++) {
            back_term = (boost::math::digamma(model.getPhi(i,k)) - dg_phi_sum);
            objective += (alph(k) - 1.0)*back_term;
        }
    }
    return objective;
}

double alpha_Objective(mm_model model)
{
    int T = model.getT();
    int K = model.getK();
    int i;
    int k;
    double back_term;
    double phi_sum;
    double dg_phi_sum;

    double objective;
    objective = T*lgamma(sum(model.getAlpha())) - T*sum(lgamma(model.getAlpha()));
    for(i = 0; i < T; i++) {
        phi_sum = 0.0;
        for(k = 0; k < K; k++) {
            phi_sum += model.getPhi(i,k);
        }
        dg_phi_sum = boost::math::digamma(phi_sum);

        for(k = 0; k < K; k++) {
            back_term = (boost::math::digamma(model.getPhi(i,k)) - dg_phi_sum);
            objective += (model.getAlpha(k)-1)*back_term;
        }
    }
    return objective;
}

