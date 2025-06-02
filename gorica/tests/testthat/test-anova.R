# ANOVA VIA LM OBJECT



sesamesim$site <- as.factor(sesamesim$site)
anov <- lm(sesamesim$postnumb~sesamesim$site-1)

z_fit_PMPb <- c(5.83122886978393e-14, 0.541552815155994, 2.10188257240928e-07,
                0.000797254493720143, 0.372913017620641, 0.00499842630098939,
                0.0486570021865279, 0.0260170984907087, 0.00506417556310323)

set.seed(100)
z_gor<-gorica(anov, "site1=site2=site3=site4=site5;
      site2>site5>site1>site3=site4;
     site1=site2>site3=site4>site5;
     site1<site2>site3<site4>site5;
     site1=site5>site3=site4<site2;
     site2>site3>site4;
     (site1,site2,site5)>(site3,site4);
     site2>(site1,site3,site4,site5)", iterations = 1000)

test_that("gorica and bain give similar results for anova", {
   expect_equivalent(z_gor$fit$gorica_weights, z_fit_PMPb, tolerance = .2)
   expect_true(sum(order(z_gor$fit$gorica_weights) == order(z_fit_PMPb)) > 5)
})
