# `viralx_nn_shap()` prints as expected

    Code
      print(viralx_nn_shap(vip_featured, hiv_data, hu, plty, epo, vip_train, vip_new,
        orderings))
    Output
                                              min          q1      median        mean
      nn + normalized: cd_2019 = 342   -164.72833  -56.889665  -42.173630  -42.173630
      nn + normalized: cd_2021 = 331   -236.32511 -128.486450 -122.558854 -109.395823
      nn + normalized: vl_2019 = 38960 -344.24515 -287.474804 -198.119827 -188.952695
      nn + normalized: vl_2021 = 5113   -10.57575   -5.613339   -4.329173    6.429772
      nn + normalized: vl_2022 = 0      -49.31398  -28.331051  -28.326192  -26.877325
                                                q3        max
      nn + normalized: cd_2019 = 342    -0.1193238   0.000000
      nn + normalized: cd_2021 = 331   -11.0495673  -5.613326
      nn + normalized: vl_2019 = 38960 -62.1992564 -54.682688
      nn + normalized: vl_2021 = 5113   22.7176668  37.468234
      nn + normalized: vl_2022 = 0     -26.8773248   0.000000

