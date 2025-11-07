#include <testthat.h>
#include "param-indexer.h"

context("param_indexer") {
  test_that("param_indexer works"){
    param_indexer const index{3, 2, 4};

    expect_true(index.n_cov_risk() == 3);
    expect_true(index.n_cov_traject() == 2);
    expect_true(index.n_causes() == 4);

    expect_true(index.risk() == index.risk(0));
    expect_true(index.risk(0) == 0);
    expect_true(index.risk(1) == 3);
    expect_true(index.risk(2) == 6);
    expect_true(index.risk(3) == 9);

    expect_true(index.traject() == index.traject(0));
    expect_true(index.traject(0) == 12);
    expect_true(index.traject(1) == 14);
    expect_true(index.traject(2) == 16);
    expect_true(index.traject(3) == 18);

    expect_true(index.cov_traject(0) == 0);
    expect_true(index.cov_traject(1) == 2);
    expect_true(index.cov_traject(2) == 4);
    expect_true(index.cov_traject(3) == 6);

    expect_true(index.vcov() == 20);

    expect_true(index.n_par_wo_vcov() == 20);
    expect_true(index.n_par<false>() == 84);
    expect_true(index.n_par<true>() == 56);

    std::vector<std::string> const names_full
      {"beta[1,1]","beta[2,1]","beta[3,1]",
       "beta[1,2]","beta[2,2]","beta[3,2]",
       "beta[1,3]","beta[2,3]","beta[3,3]",
       "beta[1,4]","beta[2,4]","beta[3,4]",
       "gamma[1,1]", "gamma[2,1]",
       "gamma[1,2]", "gamma[2,2]",
       "gamma[1,3]", "gamma[2,3]",
       "gamma[1,4]", "gamma[2,4]",
       "vcov[1,1]", "vcov[2,1]", "vcov[3,1]", "vcov[4,1]", "vcov[5,1]", "vcov[6,1]", "vcov[7,1]", "vcov[8,1]",
       "vcov[1,2]", "vcov[2,2]", "vcov[3,2]", "vcov[4,2]", "vcov[5,2]", "vcov[6,2]", "vcov[7,2]", "vcov[8,2]",
       "vcov[1,3]", "vcov[2,3]", "vcov[3,3]", "vcov[4,3]", "vcov[5,3]", "vcov[6,3]", "vcov[7,3]", "vcov[8,3]",
       "vcov[1,4]", "vcov[2,4]", "vcov[3,4]", "vcov[4,4]", "vcov[5,4]", "vcov[6,4]", "vcov[7,4]", "vcov[8,4]",
       "vcov[1,5]", "vcov[2,5]", "vcov[3,5]", "vcov[4,5]", "vcov[5,5]", "vcov[6,5]", "vcov[7,5]", "vcov[8,5]",
       "vcov[1,6]", "vcov[2,6]", "vcov[3,6]", "vcov[4,6]", "vcov[5,6]", "vcov[6,6]", "vcov[7,6]", "vcov[8,6]",
       "vcov[1,7]", "vcov[2,7]", "vcov[3,7]", "vcov[4,7]", "vcov[5,7]", "vcov[6,7]", "vcov[7,7]", "vcov[8,7]",
       "vcov[1,8]", "vcov[2,8]", "vcov[3,8]", "vcov[4,8]", "vcov[5,8]", "vcov[6,8]", "vcov[7,8]", "vcov[8,8]"};

    std::vector<std::string> const names_upper
      {"beta[1,1]","beta[2,1]","beta[3,1]",
       "beta[1,2]","beta[2,2]","beta[3,2]",
       "beta[1,3]","beta[2,3]","beta[3,3]",
       "beta[1,4]","beta[2,4]","beta[3,4]",
       "gamma[1,1]", "gamma[2,1]",
       "gamma[1,2]", "gamma[2,2]",
       "gamma[1,3]", "gamma[2,3]",
       "gamma[1,4]", "gamma[2,4]",
       "vcov[1]", "vcov[2]", "vcov[3]", "vcov[4]", "vcov[5]", "vcov[6]", "vcov[7]", "vcov[8]", "vcov[9]", "vcov[10]", "vcov[11]", "vcov[12]", "vcov[13]", "vcov[14]", "vcov[15]", "vcov[16]", "vcov[17]", "vcov[18]", "vcov[19]", "vcov[20]", "vcov[21]", "vcov[22]", "vcov[23]", "vcov[24]", "vcov[25]", "vcov[26]", "vcov[27]", "vcov[28]", "vcov[29]", "vcov[30]", "vcov[31]", "vcov[32]", "vcov[33]", "vcov[34]", "vcov[35]", "vcov[36]"
      };

    {
       auto res = index.param_names<false>();
       expect_true(res.size() == names_full.size());
       for(size_t i = 0; i < names_full.size(); ++i)
         expect_true(res[i] == names_full[i]);
    }

    auto res = index.param_names<true>();
    expect_true(res.size() == names_upper.size());
    for(size_t i = 0; i < names_upper.size(); ++i)
      expect_true(res[i] == names_upper[i]);
  }
}
