# `viralx_knn_shap()` works

    Code
      print(viralx_knn_shap(vip_featured, hiv_data, knn_hyperparameters, vip_train,
        vip_new, orderings))
    Output
                                               min           q1       median
      knn + normalized: cd_2019 = 169   -201.28030 -173.8138930 -104.9176358
      knn + normalized: cd_2021 = 275   -120.26822  -45.3576192  -38.5373389
      knn + normalized: cd_2022 = 127      0.00000    0.0000000    0.0000000
      knn + normalized: vl_2019 = 11390 -160.61934 -138.2480709  -95.3029992
      knn + normalized: vl_2021 = 1690   -24.46446   -6.4308959   -3.3161782
      knn + normalized: vl_2022 = 0       -8.01577   -0.7820023   -0.7820023
                                                mean          q3        max
      knn + normalized: cd_2019 = 169   -121.5727587 -89.7357789 -73.960879
      knn + normalized: cd_2021 = 275    -50.5531006 -37.0374238 -14.113936
      knn + normalized: cd_2022 = 127      0.0000000   0.0000000   0.000000
      knn + normalized: vl_2019 = 11390  -95.3029992 -54.0275298 -33.299919
      knn + normalized: vl_2021 = 1690    -6.4308959  -2.1360036   6.184192
      knn + normalized: vl_2022 = 0       -0.6758936   0.9654131   4.415332

