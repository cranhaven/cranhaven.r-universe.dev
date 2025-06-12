
//simulate missing y for a single subject

functions {
  real klpdf(real err, vector res, real h, int n) {
    real tmp;
    real rst;

    rst = 0;
    for (i in 1:n) {
      tmp = (res[i] - err) / h;
      tmp = (tmp/2)^2;
      rst = rst + exp(-tmp);
    }

    rst = log(rst/n/h);      
    //return
    return(rst);
  }

  real cond_lpdf(vector ymis, vector yobs, matrix coef,
                 int ny, real[] mu, real[] sigma, int[] imis, int[] inx,
                 int assumenormal, int nres, matrix residual, real[] h) {
    real rst;
    real cmu;
    real csigma;
    vector[ny] ally;

    //generate vector of y
    for (i in 1:ny) {
      if (1 == imis[i]) {
        //missing 
        ally[i] = ymis[inx[i]];
      } else {
        //observed
        ally[i] = yobs[inx[i]];
      }
    }

    rst = 0;
    for (i in 1:ny) {
      csigma = sigma[i];
      cmu    = mu[i];
      //previous y
      if (1 < i) {
        cmu  = cmu + coef[i,3] * ally[i-1];
      }

      // likelihood
      if (1 == assumenormal) {
        rst = rst + normal_lpdf(ally[i] | cmu, csigma);
      } else {
        rst = rst + klpdf(ally[i] - cmu, residual[,i], h[i], nres);
      }
    }
    return rst;
  }
}

data {
  int<lower=1>          NY;   //total no. y
  int<lower=0>          NOBS; //no of observed y 
  vector[NOBS+1]        YOBS; //add dummy y to handle nobs=0
  int<lower=1>          NX;   //no. covariates
  vector[NX]            X;    //covariates
  matrix[NY, NX+3]      COEF; //coefficents 1st column is sigma

  int<lower=0, upper=1> IMIS[NY];
  int<lower=1>          INX[NY];

  //residuals
  int<lower=0, upper=1> ASSUMENORMAL;
  int<lower=1>          NRES;
  matrix[NRES,NY]       RESIDUAL;
  real                  H[NY]; 
}

transformed data {
  int<lower = 0> NMIS;
  real MU[NY];
  real SIGMA[NY];

  NMIS = NY - NOBS;
  for (i in 1:NY) {
    SIGMA[i] = COEF[i, 1];
    MU[i]    = COEF[i, 2];
    //covariates
    for (k in 1:NX) {
      MU[i] = MU[i] + X[k] * COEF[i, 3+k];
    }
  }
}

parameters{
  vector[NMIS] YMIS;
}

model {
  YMIS ~ cond(YOBS, COEF, NY, MU, SIGMA, IMIS, INX, ASSUMENORMAL, NRES, RESIDUAL, H);
}
