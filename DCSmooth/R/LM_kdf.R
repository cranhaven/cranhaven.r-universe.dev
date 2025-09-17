################################################################################
#                                                                              #
#   DCSmooth Package: Lookup functions for LM Bandwidth Selection Parameter    #
#                                                                              #
################################################################################

kdf <- function(l, m, d)
{
  S <- 0
  for(i in 0:m)
  {
    S1 <- 0
    for(j in i:(l+m))
    {
      S1<-S1+(-1)^(j-i)*choose(l+m-i, j-i)/(2*d+j+1)*2^(2*d+j+1)
    }
  S<-S+2*choose(m, i)*S1/(2*d+i)
  }
  return(S)
}


### lookup tables for smootslm
p3_lookup <- list(function(u) {3 / 8 * (3 - 5 * u^2)},
                  function(u) {3 - 10 * u^2 + 7 * u^4},
                  function(u) {1 - 5 * u^2 + 7 * u^4 - 3 * u^6},
                  function(u) {3 - 20 * u^2 + 42 * u^4 - 36 *
                      u^6 + 11 * u^8})

p1p3_lookup <- matrix(list(function(d) {kdf(0,0,d)},
                           function(d) {kdf(0,0,d)-
                               2 * kdf(2,0,d)+
                               kdf(2,2,d)},
                           function(d) {kdf(0,0,d)+
                               4 * kdf(2,2,d)+
                               kdf(4,4,d)-
                               4 * kdf(2,0,d)+
                               2 * kdf(4,0,d)-
                               4 * kdf(4,2,d)},
                           function(d) {kdf(0,0,d)+
                               9 * kdf(2,2,d)+
                               9 * kdf(4,4,d)+
                               kdf(6,6,d)-
                               6 * kdf(2,0,d)+
                               6 * kdf(4,0,d)-
                               2 * kdf(6,0,d)-
                               18 * kdf(4,2,d)+
                               6 * kdf(6,2,d)-
                               6 * kdf(6,4,d)},
                           function(d) {9 * kdf(0,0,d)-
                               30 * kdf(2,0,d)+
                               25 * kdf(2,2,d)},
                           function(d) {9*kdf(0,0,d)+
                               100*kdf(2,2,d)+
                               49*kdf(4,4,d)-
                               60*kdf(2,0,d)+
                               42*kdf(4,0,d)-
                               140*kdf(4,2,d)},
                           function(d) {kdf(0,0,d)+
                               25*kdf(2,2,d)+
                               49*kdf(4,4,d)+
                               9*kdf(6,6,d)-
                               10*kdf(2,0,d)+
                               14*kdf(4,0,d)-
                               6*kdf(6,0,d)-
                               70*kdf(4,2,d)+
                               30*kdf(6,2,d)-
                               42*kdf(6,4,d)},
                           function(d) {9*kdf(0,0,d)+
                               400*kdf(2,2,d)+
                               42^2*kdf(4,4,d)+
                               36^2*kdf(6,6,d)+
                               11^2*kdf(8,8,d)-
                               120*kdf(2,0,d)+
                               252*kdf(4,0,d)-
                               216*kdf(6,0,d)+
                               66*kdf(8,0,d)-
                               40*42*kdf(4,2,d)+
                               40*36*kdf(6,2,d)-
                               40*11*kdf(8,2,d)-
                               84*36*kdf(6,4,d)+
                               84*36*kdf(8,4,d)-
                               72*11*kdf(8,6,d)}), ncol = 2)

lookup_3kerns <- matrix(list(function(u) {u},
                             function(u) {u - u ** 3},
                             function(u) {-u + 2 * u ** 3 - u ** 5},
                             function(u) {-u + 3 * u ** 3 - 3 *
                                 u ** 5 + u ** 7},
                             function(u) {1 - 3 * u ** 2},
                             function(u) {-1 + 6 * u ** 2 - 5 *
                                 u ** 4},
                             function(u) {-1 + 9 * u ** 2 - 15 *
                                 u ** 4 + 7 * u ** 6},
                             function(u) {-1 + 12 * u ** 2 - 30 *
                                 u ** 4 + 28 * u ** 6 - 9 * u ** 8}),
                        ncol = 2)


lookup_3kerns_kdf <- matrix(list(function(d) {kdf(1,1,d)},
                                 function(d) {kdf(1,1,d)-
                                     2*kdf(3,1,d)+
                                     kdf(3,3,d)},
                                 function(d) {kdf(1,1,d)+
                                     4*kdf(3,3,d)+
                                     kdf(5,5,d)-
                                     4*kdf(3,1,d)+
                                     2*kdf(5,1,d)-
                                     4*kdf(5,3,d)},
                                 function(d) {kdf(1,1,d)+9*kdf(3,3,d)+9*kdf(5,5,d)+kdf(7,7,d)
                                   -6*kdf(3,1,d)+6*kdf(5,1,d)-2*kdf(7,1,d)-18*kdf(5,3,d)
                                   +6*kdf(7,3,d)-6*kdf(7,5,d)},
                                 function(d) {kdf(0,0,d)-
                                     6*kdf(2,0,d)+
                                     9*kdf(2,2,d)},
                                 function(d) {kdf(0,0,d)+
                                     36*kdf(2,2,d)+
                                     25*kdf(4,4,d)-
                                     12*kdf(2,0,d)+
                                     10*kdf(4,0,d)-
                                     60*kdf(4,2,d)},
                                 function(d) {kdf(0,0,d)+
                                     81*kdf(2,2,d)+
                                     225*kdf(4,4,d)+
                                     49*kdf(6,6,d)-
                                     18*kdf(2,0,d)+
                                     30*kdf(4,0,d)-
                                     14*kdf(6,0,d)-
                                     270*kdf(4,2,d)+
                                     126*kdf(6,2,d)-
                                     210*kdf(6,4,d)},
                                 function(d) {kdf(0,0,d)+
                                     144*kdf(2,2,d)+
                                     900*kdf(4,4,d)+
                                     784*kdf(6,6,d)+
                                     81*kdf(8,8,d)-
                                     24*kdf(2,0,d)+
                                     60*kdf(4,0,d)-
                                     56*kdf(6,0,d)+
                                     18*kdf(8,0,d)-
                                     720*kdf(4,2,d)+
                                     672*kdf(6,2,d)-
                                     216*kdf(8,2,d)-
                                     1680*kdf(6,4,d)+
                                     540*kdf(8,4,d)-
                                     504*kdf(8,6,d)}),
                            ncol = 2)

InfR_lookup <- matrix(c(function(w, d) {w^((5 - 2 * d) / (7 - 2 * d))}, function(w, d) {w^((9 - 2 * d) / (11 - 2 * d))},
                        function(w, d) {w^((5 - 2 * d) / (9 - 2 * d))}, function(w, d) {w^((9 - 2 * d) / (13 - 2 * d))},
                        function(w, d) {w^(1 / 2)}, function(w, d) {w^(1 / 2)}),
                      ncol = 3, dimnames = list(c("1", "3"), c("Opt",
                                                               "Nai", "Var")))


InfRM_lookup <- matrix(c(function(w, n, d) {w * n^((2 - 4 * d) / ((5 - 2 * d) * (7 - 2 * d)))}, function(w, n, d) {w * n^((2 - 4 * d) / ((9 - 2 * d) * (11 - 2 * d)))},
                         function(w, n, d) {w * n^((4 - 8 * d) / ((5 - 2 * d) * (9 - 2 * d)))}, function(w, n, d) {w * n^((4 - 8 * d) / ((9 - 2 * d) * (13 - 2 * d)))},
                         function(w, n, d) {w * n^((1 - 2 * d) / (10 - 4* d))}, function(w, n, d) {w *n^((1 - 2 * d) / (18 - 4 * d))}),
                       ncol = 3, dimnames = list(c("1", "3"), c("Opt",
                                                                "Nai", "Var")))

InfR2_lookup <- matrix(c(function(w, d) {w^((7 - 2 * d) / (9 - 2 * d))}, function(w, d) {w^((9 - 2 * d) / (11 - 2 * d))},
                         function(w, d) {w^((7 - 2 * d) / (11 - 2 * d))}, function(w, d) {w^((9 - 2 * d) / (13 - 2 * d))},
                         function(w, d) {w^(1 / 2)}, function(w, d) {w^(1 / 2)}),
                       ncol = 3, dimnames = list(NULL, c("Opt", "Nai", "Var")))


lookup_alg <- matrix(c("NP", "Opt", "Y", "NP", "Nai", "Y", "NP", "Nai", "N", "AR", "Nai",
                       "N", "NP", "Opt", "N", "AR", "Opt", "N", "MA", "Opt", "N", "MA", "Nai",
                       "N", "ARMA", "Opt", "N", "ARMA", "Nai", "N"), ncol = 10,
                     dimnames = list(c("Mcf", "InfR", "bvc"), c("A", "B", "N", "NA",
                                                                "O", "OA", "OM", "NM", "OAM", "NAM")))

lookup <- list(lookup_alg = lookup_alg, InfR_lookup = InfR_lookup,
               p1p3_lookup = p1p3_lookup, p3_lookup = p3_lookup,
               lookup_3kerns = lookup_3kerns, lookup_3kerns_kdf = lookup_3kerns_kdf,
               InfR2_lookup = InfR2_lookup,
               InfRM_lookup = InfRM_lookup)