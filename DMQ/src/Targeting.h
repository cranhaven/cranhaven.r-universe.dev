#ifndef TARGETING_H
#define TARGETING_H
arma::vec Find_Xi0(arma::vec vQ_0, arma::vec vTau, double dPhi, double dGamma, int iTau_star, arma::vec vScaling);
double h_fun(int l, int j, int iJ, arma::vec vTau);
double g_fun(int l, int j, int iJ, arma::vec vTau);
#endif
