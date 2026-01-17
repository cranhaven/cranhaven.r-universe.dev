rm(list = ls())
devtools::load_all()

library(dplyr)
designpath <- system.file("extdata", "spdesigns", "designs", package = "simulateDCE")

destype <- "spdesign"
resps <- 4000 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"
bcoeff <- list(
  bx1A = 0.1, ## very high asc
  bx1B = -0.1,
  bx2A = 0.2,
  bx2B = -0.2
)


desisiongroups <- c(0, 0.3, 1)

ul <- list(
  uA =
    list(
      v1 = V.1 ~ bx1A * alt1.x1 + bx2A * alt1.x2,
      v2 = V.2 ~ bx1A * alt2.x1 + bx2A * alt2.x2
    ),
  uB = list(
    v1 = V.1 ~ bx1B * alt1.x1 + bx2B * alt1.x2,
    v2 = V.2 ~ bx1B * alt2.x1 + bx2B * alt2.x2
  )
)

simplesim <- sim_all(nosim = nosim, resps = resps, designpath = designpath, bcoeff = bcoeff, u = ul, designtype = "spdesign", decisiongroups = desisiongroups)

testdata <- simplesim[["twoattr"]][[1]][["data"]] %>%
  distinct(group, Choice_situation, .keep_all = T) %>%
  arrange(Choice_situation)

## estimate models

library(apollo)

apollo_initialise()


apollo_control <- list(
  modelName       = "Clogit_simpledesign",
  modelDescr      = "Simple conditional logit model for group 1 ",
  indivID         = "ID",
  nCores          = 1,
  outputDirectory = "/~"
)


## group 1

database <- simplesim[["twoattr"]][[1]][["data"]]
apollo_beta <- c(
  b_x1 = 0,
  b_x2 = 0
)

apollo_fixed <- c()





# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs <- apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P <- list()

  ### Define settings for MNL model component that are generic across classes
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
    choiceVar    = CHOICE
  )




  ### Compute class-specific utilities
  V <- list()
  V[["alt1"]] <- b_x1 * alt1_x1 + b_x2 * alt1_x2
  V[["alt2"]] <- b_x1 * alt2_x1 + b_x2 * alt2_x2

  mnl_settings$utilities <- V
  # mnl_settings$componentName = paste0("Class_",s)



  ### Compute probabilities using MNL model
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)

  ### Take product across observation for same individual
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional starting values search
# apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings = list(estimationRoutine = "BFGS", hessianRoutine = "numDeriv"))



apollo_modelOutput(model)




### latent class model

apollo_control <- list(
  modelName       = "Latent Class logit_simpledesign",
  modelDescr      = "Latent Class  logit model",
  indivID         = "ID",
  nCores          = 1,
  outputDirectory = "/~"
)


## group 1

database <- simplesim[["twoattr"]][[1]][["data"]]

apollo_beta <- c(
  b_x1_a = 0,
  b_x2_a = 0,
  b_x1_b = 0,
  b_x2_b = 0,
  delta_a = 0,
  delta_b = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed <- c("delta_b")

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars <- function(apollo_beta, apollo_inputs) {
  lcpars <- list()

  lcpars[["b_x1"]] <- list(b_x1_a, b_x1_b)
  lcpars[["b_x2"]] <- list(b_x2_a, b_x2_b)

  V <- list()
  V[["class_a"]] <- delta_a
  V[["class_b"]] <- delta_b


  classAlloc_settings <- list(
    classes      = c(class_a = 1, class_b = 2),
    utilities    = V
  )

  lcpars[["pi_values"]] <- apollo_classAlloc(classAlloc_settings)

  return(lcpars)
}



# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs <- apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #


# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P <- list()

  ### Define settings for MNL model component that are generic across classes
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
    choiceVar    = CHOICE
  )

  ### Loop over classes
  for (s in 1:2) {
    ### Compute class-specific utilities
    V <- list()
    V <- list()
    V[["alt1"]] <- b_x1[[s]] * alt1_x1 + b_x2[[s]] * alt1_x2
    V[["alt2"]] <- b_x1[[s]] * alt2_x1 + b_x2[[s]] * alt2_x2



    mnl_settings$utilities <- V
    # mnl_settings$componentName = paste0("Class_",s)

    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_", s)]] <- apollo_mnl(mnl_settings, functionality)

    ### Take product across observation for same individual
    P[[paste0("Class_", s)]] <- apollo_panelProd(P[[paste0("Class_", s)]], apollo_inputs, functionality)
  }

  ### Compute latent class model probabilities
  lc_settings <- list(inClassProb = P, classProb = pi_values)
  P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional starting values search
apollo_beta <- apollo_searchStart(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs) # ,estimate_settings =list(estimationRoutine="bhhh" , hessianRoutine= "maxLik")

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)
