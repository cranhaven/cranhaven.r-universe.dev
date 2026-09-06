#include "eStep.h"

using namespace boost::math;

// [[Rcpp::depends(BH)]]
double eStep_C(mm_model model, double elbo_E, int maxEIter, double elboTol, NumericVector iterReached) {
    int nE = 0;
    double old_elbo_E;
    int T = model.getT();
    int K = model.getK();
    int J = model.getJ();
    int i, j, r, n, Nijr, k;

    double phi_sum; //helper which will be used to store sum of phi
    double phi_sum_dg;
    double delta_sum;
    double converged_E=1.0; //flag for convergence
    double placeholder;

    while ((converged_E > elboTol) && (nE < maxEIter))
    {
        //UPDATE!
        //update elbo and increment E step count
        old_elbo_E = elbo_E;
        nE++;

        /*
        * Update Delta
        */
        for(i = 0; i < T; i++)
        {
            //get sum_k phi_{ik} which will be used in the delta updates
            phi_sum = 0.0;
            for(k = 0; k < K; k++)
            {
                phi_sum += model.getPhi(i,k);
            }

            phi_sum_dg = digamma(phi_sum); //digamma of the sum of phi's


            for(j = 0; j < J; j++)
            {
                for(r = 0; r < model.getR(j); r++)
                {
                    Nijr = model.getN(i,j,r);
                    for(n = 0; n < Nijr; n++)
                    {
                        delta_sum = 0.0;
                        //update deltas
                        for(k = 0; k <  K; k++)
                        {
                            placeholder = exp(digamma(model.getPhi(i,k)) - phi_sum_dg + dl_ddelta(model, i, j,r,n,k));
                            model.setDelta(i,j,r,n,k, placeholder);
                            delta_sum += model.getDelta(i,j,r,n,k);
                        }
                        model.normalizeDelta(i,j,r,n,delta_sum);
                    }
                }
            }
        }//end update delta

        /*
        * Update phi's
        */
        for(i = 0; i < T; i++)
        {
            for(k = 0; k < K; k++)
            {
                model.setPhi(i,k, model.getAlpha(k));
            }
        }

        for(i = 0; i < T; i++)
        {
            for(j = 0; j< J; j++)
            {
                for(r = 0; r < model.getR(j); r++)
                {
                    Nijr = model.getN(i,j,r);
                    for(n = 0; n < Nijr; n++)
                    {
                        for(k = 0; k < K; k ++)
                        {
                            model.incPhi(i,k, model.getDelta(i,j,r,n,k)) ;
                        }
                    }
                }
            }
        }

        //calculate convergence criteria
        elbo_E = compute_ELBO(model);
        converged_E = (old_elbo_E- elbo_E)/old_elbo_E;
    }
    if (nE == maxEIter) {
        Rcout<< "Max E Steps Reached!" <<std::endl;
        iterReached[0] = 1;
    }
    return elbo_E;
}


//referred to as  dL/ddelta in derivations
double dl_ddelta(mm_model model, int i, int j, int r, int n, int k)
{
    double dl_dd = 0.0;

    if(model.getDist(j) == BERNOULLI)
    {
        dl_dd += (model.getObs(i,j,r,n) ? log(model.getTheta(j,k,0)) : log(1-model.getTheta(j,k,0)));
    } // end binomial
    else if(model.getDist(j) == MULTINOMIAL)
    {
        dl_dd +=log(model.getTheta(j,k,model.getObs(i,j,r,n)));
    } //end multinomial
    else if(model.getDist(j) == RANK)
    {
        int eta;
        double back_term = 0.0;
        dl_dd += log(model.getTheta(j,k,model.getObs(i,j,r,n)));
        //term original from the denominator that gets subtracted off
        for(eta = 0; eta < n; eta++)
        {
            back_term += model.getTheta(j,k,model.getObs(i,j,r,eta));
        }

        dl_dd += -log(1.0-back_term);
    }//end rank
    return(dl_dd);
}

