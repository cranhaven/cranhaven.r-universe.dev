## Demo: Shiny App
library("distreg.vis")
library("gamlss")
library("bamlss")

# A few models
normal_gamlss <- gamlss(NO ~ binomial1 + ps(norm2),
                 sigma.formula = ~ binomial1 + ps(norm2),
                 data = model_fam_data(),
                 trace = FALSE)

beta_bamlss <- bamlss(list(beta ~ binomial1 + s(norm2),
                    ~ binomial1 + s(norm2)),
               data = model_fam_data(fam_name = "beta"),
               family = beta_bamlss(),
               verbose = FALSE)

multinomial_bamlss <- bamlss(list(multinomial ~ binomial1 + s(norm2)),
                      data = model_fam_data(fam_name = "multinomial"),
                      family = multinomial_bamlss(),
                      verbose = FALSE)

# Start the App
distreg.vis::vis()
