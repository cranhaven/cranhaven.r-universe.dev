#include "mStep.h"


using namespace boost::math;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]

vec getGrad(mm_model model)
{
    int i,c,k;
    int K = model.getK();
    int T = model.getT();
    double sum_phi = 0.0;
    vec grad = vec(K);

    double sum_alpha = sum(model.getAlpha());
    double dg_sum_alpha = digamma(sum_alpha);
    for(k = 0; k < K; k++) {
        grad(k) = T*(dg_sum_alpha - digamma(model.getAlpha(k)));
        for(i = 0; i < T; i++) {
            sum_phi = 0.0;
            for(c = 0; c < K; c++) {
                sum_phi += model.getPhi(i,c);
            }
            grad(k) += digamma(model.getPhi(i,k)) - digamma(sum_phi);
        }
    }
    return grad;
}

mat getHess(mm_model model)
{
    int k;
    int K = model.getK();
    double tri_gam_eval;
    mat hess = mat(K,K);
    double sum_alpha = sum(model.getAlpha());
    tri_gam_eval = trigamma(sum_alpha);
    hess.ones();
    hess = hess*tri_gam_eval*model.getT();
    for(k = 0; k < K; k++) {
        hess(k,k) -= trigamma(model.getAlpha(k))*model.getT();
    }
    return hess;
}

double mStep_C(mm_model model, double elbo_T, int stepType, int maxAlphaIter, int maxThetaIter, int maxLSIter,
               double alphaTol, double thetaTol, double aNaught, double tau,
               int bMax, double bNaught, double bMult, int vCutoff, NumericVector holdConst, NumericVector iterReached)
{
    int K = model.getK();
    vec grad = vec(K);
    mat hess = mat(K, K);
    vec update = vec(K);
    vec new_alpha = vec(K);
    int k;


    double conv_crit_m = 1.0;
    double old_obj;
    double new_obj;
    int nLS;

    /*
    * <=== Fit Alpha ===>
    */

    int nA = 0;
    double a;

    //Update Alpha
    if (stepType == 2 || stepType == 3) {
        while( (conv_crit_m > alphaTol) & (nA <= maxAlphaIter) ) {
            old_obj = alpha_Objective(model);
            nA++;


            //create gradient
            grad = getGrad(model);
            //create hessian
            hess = getHess(model);

            update = solve(hess,-grad);

            for(k = 0; k < K; k++) {
                new_alpha(k) = model.getAlpha(k);
            }
            a = aNaught;
            new_obj= alpha_Objective(model, (new_alpha + a * update) );

            //if the steps is already too big such that a theta is negative (which leads the objective function to be NAN)
            //then just set ob_old to the new objective +1
            new_obj = (any( (new_alpha + a * update) < 0 ) ? old_obj - 1.0: new_obj);

            nLS = 0;
            while( (old_obj - new_obj > 0) & (nLS < maxLSIter)) {
                nLS++;
                a *= tau; //scale back a

                //if in a feasible space, then check objective function
                if(all((new_alpha + a * update)>0)) {
                    new_obj = alpha_Objective(model, (new_alpha + a * update));
                }
            }

            //update alpha in model
            if(nLS < maxLSIter) {
                for(k = 0; k < K; k++) {
                    model.incAlpha(k, a*update(k));
                }
            } else {
                new_obj = alpha_Objective(model);
            }

            //check for convergence
            conv_crit_m = fabs((old_obj - new_obj)/old_obj);
        }

        if( nA == maxAlphaIter ) {
            Rcout << "Max Alpha Steps Reached!!" << std::endl;
            iterReached[1] = 1;
        }
    } // End Alpha Update

    if ( (stepType == 1) || (stepType == 3) ) {
        updateTheta(model, maxThetaIter, maxLSIter, thetaTol, aNaught, tau,
                    bMax, bNaught, bMult, vCutoff, holdConst, iterReached);
    }

    double elbo = compute_ELBO(model);
    return elbo;
} //end m-step



void updateTheta(mm_model model, int maxThetaIter,
                 int maxLSIter, double thetaTol, double aNaught,
                 double tau, int bMax,
                 double bNaught, double bMult, int vCutoff, NumericVector holdConst, NumericVector iterReached)
{

    int i,j,r,n,k,v;
    int J = model.getJ();
    int K = model.getK();


    for(j = 0; j < J; j++) {
        if(model.getDist(j) == BERNOULLI) {
            double denom, numer;
            n = 0;
            v=0;
            for(k = 0; k < K; k++) {
                if(is_true( all(k != holdConst) ) ) {
                    numer = 0.0;
                    denom =0.0;
                    for(i = 0; i < model.getT(); i++) {
                        for(r = 0; r<model.getR(j); r++) {
                            if(model.getObs(i,j,r,n)) {
                                numer += model.getDelta(i,j,r,n,k);
                            }
                            denom += model.getDelta(i,j,r,n,k);
                        }
                    }

                    //Check updates too close to numerical 1 or 0
                    //bump defined in settings.h
                    if((numer/denom)>(1.0 - BUMP)) {
                        model.setTheta(j,k,v,1.0 - BUMP);
                    } else if((numer / denom) < BUMP) {
                        model.setTheta(j,k,v, BUMP);
                    } else {
                        model.setTheta(j,k,v, numer / denom);
                    }
                }
            }
        }//end Bernoulli
        else if(model.getDist(j) == MULTINOMIAL) {
            double theta_sum;
            n = 0;
            for(k = 0; k < K; k++) {
                if(is_true( all(k != holdConst) ) ) {
                    theta_sum = 0.0;
                    for(v = 0; v< model.getV(j); v++) {
                        model.setTheta(j,k,v,0.0);
                    }

                    for(i = 0; i < model.getT(); i++) {
                        for(r = 0; r<model.getR(j); r++) {
                            model.incTheta(j,k,model.getObs(i,j,r,n), model.getDelta(i,j,r,n,k));
                            theta_sum +=model.getDelta(i,j,r,n,k);
                        }
                    }
                    model.normalizeTheta(j,k,theta_sum);
                }
            }
        } else if(model.getDist(j) == RANK) {
            update_PL_Theta(model, j, maxThetaIter, maxLSIter, thetaTol, aNaught,
                            tau, bMax, bNaught, bMult, vCutoff, holdConst, iterReached);
        }
    }
}


void update_PL_Theta(mm_model model, int j, int maxThetaIter,
                     int maxLSIter, double thetaTol, double aNaught,
                     double tau, double bMax, double bNaught, double bMult, int vCutoff, NumericVector holdConst, NumericVector iterReached)
{
    int V = model.getV(j);
    double b;
    int b_power, k;
    vec theta = vec(V);
    vec grad = vec(V);
    vec update = vec(V);
    mat hess = mat(V,V);
    mat m = mat(1,1);
    mat H_inv_g = mat(V,V);
    mat H_inv_ones = mat(V,V);
    vec A = vec(V);
    A.ones();
    double conv_crit, obj_old, obj_new, a;
    int nLS, nTheta,v;

    for(k = 0; k < model.getK(); k++) {
        if(is_true( all(k != holdConst) ) ) {
            //set b for approximate penalty
            b = bNaught;

            //iteratively step b higher and solve IPP
            for(b_power = 0; b_power < bMax; b_power++) {
                b *= bMult; //increase b
                conv_crit = 1.0;
                nTheta = 0;
                while(conv_crit > thetaTol&& (nTheta < maxThetaIter)) {
                    nTheta++;
                    grad = getGradPL(model, j,k, b);

                    //if Vj is too large, use gradient ascent instead of newton step
                    if(V > vCutoff) {
                        //constrained gradient ascent
                        update = -grad + sum(grad)/V;
                    } else {
                        //calculate newton step involving matrix inversion
                        hess = getHessPL(model,j,k,b);

                        if( ( (int) arma::rank(hess) ) < V) {
                            hess.eye();
                            update = -grad + sum(grad)/V;
                        } else {
                            solve(H_inv_g,hess, grad);
                            solve(H_inv_ones,hess, A);
                            update = -H_inv_g + H_inv_ones*(sum(H_inv_g)/sum(H_inv_ones)); //search direction
                        }
                    }

                    /*
                    * Line Search Section
                    */

                    //set theta vec to current estimates of theta from the model
                    for(v = 0; v < V; v++) {
                        theta(v) = model.getTheta(j,k,v);
                    }

                    a = aNaught;
                    obj_old = rank_Objective(model,theta,j,k,b);
                    obj_new = rank_Objective(model, (theta+a*update),j,k,b);

                    //if the steps is already too big such that a theta is negative (which leads the objective function to be NAN)
                    //then just set ob_old to the new objective +1
                    obj_new = (obj_new !=obj_new ? obj_old +1.0: obj_new);

                    nLS = 0;
                    while((obj_old - obj_new <0) && (nLS < maxLSIter)) {
                        nLS++;
                        a *= tau; //scale back a

                        //if in a feasible space, then check objective function
                        if(all((theta+a*update)>0)) {
                            obj_new = rank_Objective(model, (theta+a*update),j,k,b);
                        }
                    }
                    if(nLS < maxLSIter) {
                        for(v = 0; v < model.getV(j); v++) {
                            model.setTheta(j,k,v, theta(v)+a*update(v));
                        }

                    }
                    conv_crit = fabs((obj_old - obj_new)/obj_old);
                } //end update at specific level of b
                if( nTheta == maxThetaIter) {
                    iterReached[2] = 1;
                }

            } //end interior point for one j,k
        } //end holdConst if
    } //end loop
}


vec getGradPL(mm_model model, int j, int k, double b)
{
    int v, i,r,n, eta;
    double back_term;
    int Vj = model.getV(j);
    vec grad = vec(Vj);
    grad.zeros();


    //begin summing
    for(i = 0; i < model.getT(); i++) {
        for(r =0; r < model.getR(j); r++) {
            back_term = 0.0;
            for(n = 0; n < model.getN(i,j,r); n++) {
                grad(model.getObs(i,j,r,n)) -= model.getDelta(i,j,r,n,k)/model.getTheta(j,k,model.getObs(i,j,r,n));
                for(eta = 0; eta < n; eta++) {
                    grad(model.getObs(i,j,r,eta)) -= model.getDelta(i,j,r,n,k)/(1.0-back_term);
                }
                back_term += model.getTheta(j,k,model.getObs(i,j,r,n));
            }
        }
    }

    for(v =0; v < Vj; v++) {
        grad(v) -=1.0/(b*model.getTheta(j,k,v));
    }
    return grad;
}


mat getHessPL(mm_model model, int j, int k, double b)
{
    int v1, i, r, n;
    double back_term;
    int Vj = model.getV(j);
    mat hess = mat(Vj,Vj);
    hess.zeros();
    int eta1, eta2;

    for(i = 0; i < model.getT(); i ++) {
        for(r = 0; r < model.getR(j); r++) {

            back_term = 0.0;
            for(n = 0; n < model.getN(i,j,r); n++) {
                for(eta1 = 0; eta1 < n; eta1++) {
                    for(eta2 = 0; eta2 < eta1; eta2++) {
                        hess(model.getObs(i,j,r,eta1),model.getObs(i,j,r,eta2)) -= model.getDelta(i,j,r,n,k)/pow(1.0-back_term,2);
                        hess(model.getObs(i,j,r,eta2),model.getObs(i,j,r,eta1)) = hess(model.getObs(i,j,r,eta1),model.getObs(i,j,r,eta2))+0.0;
                    }

                    hess(model.getObs(i,j,r,eta1),model.getObs(i,j,r,eta1)) -= model.getDelta(i,j,r,n,k)/pow(1.0-back_term,2);
                }


                hess(model.getObs(i,j,r,n),model.getObs(i,j,r,n)) -= -model.getDelta(i,j,r,n,k)/pow(model.getTheta(j,k,model.getObs(i,j,r,n)),2);
                back_term += model.getTheta(j,k,model.getObs(i,j,r,n));
            }
        }
    }

    for(v1 = 0; v1 < Vj; v1++) {
        hess(v1,v1) += 1.0/(b*pow(model.getTheta(j,k,v1),2));
    }
    return hess;
}


double rank_Objective(mm_model model, vec theta, int j, int k, double b)
{
    double objective=0.0;
    int i, r, Nijr, n;
    double back_term;
    for(i = 0; i < model.getT(); i++) {
        for(r = 0; r < model.getR(j); r++) {
            Nijr = model.getN(i,j,r);
            back_term =0.0;
            for(n = 0; n < Nijr; n++) {
                objective -= -model.getDelta(i,j,r,n,k)*log(1.0-back_term);
                objective -= model.getDelta(i,j,r,n,k)*log(theta(model.getObs(i,j,r,n)));
                back_term += theta(model.getObs(i,j,r,n));
            }
        }
    }
    objective -=sum(log(theta))/b;
    return objective;
}

