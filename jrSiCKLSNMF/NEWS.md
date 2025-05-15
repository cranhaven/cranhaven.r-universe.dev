# jrSiCKLSNMF 1.2.2

* Bug fix for L2 Norm option (changed arma::mat sumdenom=arma::sum(denomnumer);
to arma::mat sumdenom=arma::sum(H%denomnumer);)
