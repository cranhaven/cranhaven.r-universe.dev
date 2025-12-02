## definition of implemented MCMC algorithms 
## these names should be exported or made visible in some way
## when building the package version
## file name starts with 'z' so that chains and steps have been
## defined before
## Note: adaptive algorithm'steps need to hold the required
## info from the past trough their parameters list
## see, eg, AMHario example

# Random Walk HM with gaussian proposal
RWHM <- list(name = "RWHM",
			chain = RWHM_chain,		# simulation of a single chain
			step = RWHM_step,
			q_pdf = gaussian_pdf,
			q_proposal = gaussian_proposal)

# gaussian IID sampler ("fake" MCMC for testing)
IID_norm <- list(name = "IID gauss",
			chain = IID_chain,
			step = IID_step,
			q_pdf = NULL,
			q_proposal = q_proposal_ISnorm)
			
# Independence Sampler HM with gaussian proposal
HMIS_norm <- list(name = "HMIS gauss",
			chain = HMIS_norm_chain,
			step = HM_step,
			q_pdf = q_pdf_ISnorm,
			q_proposal = q_proposal_ISnorm)

			
# Adaptive-Metropolis (AM) Haario (2001)
# probably have to write a specific HM_step handling adaptive cov
# at each step, for the EntropyParallel usage  
AMHaario <- list(name = "AM Haario",
			chain = AMHaario_chain,
			step = HM_step,
			q_pdf = gaussian_pdf,
			q_proposal = gaussian_proposal)



# define in the same way your own MCMC...

