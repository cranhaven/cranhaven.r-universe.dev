############################ Functions for heritability analysis ##############################



######## Packages that we need ########

# You only need to install once #

#install.packages("lme4", repos="http://cran.r-project.org")
#install.packages("cplm", repos="http://cran.r-project.org")
#install.packages("pbapply", repos="http://cran.r-project.org")
#install.packages("statmod", repos="http://cran.r-project.org")

# install.packages("R2admb")
# install.packages("glmmADMB",
#                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
#                         getOption("repos")),
#                 type="source")

# if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("DESeq2")
# BiocManager::install("SummarizedExperiment")
# BiocManager::install("BiocGenerics")


requireNamespace("lme4", quietly = TRUE)
requireNamespace("tweedie", quietly = TRUE)
requireNamespace("cplm", quietly = TRUE)
requireNamespace("pbapply", quietly = TRUE)
requireNamespace("DESeq2", quietly = TRUE)
requireNamespace("MASS", quietly = TRUE)
requireNamespace("SummarizedExperiment", quietly = TRUE)
requireNamespace("BiocGenerics", quietly = TRUE)


#' @import DESeq2
#' @import MASS
#' @import stats
#' @import utils
#' @importFrom SummarizedExperiment assay
#' @importFrom pbapply pbsapply
#' @importFrom tweedie rtweedie
#' @importFrom cplm cpglmm
#' @importFrom cplm cpglm
#' @importFrom cplm VarCorr
#' @importFrom lme4 lmer

###############################################################################
### Simulate negative binomial distributed data matrix 

# (INTERNAL)
getNBReads <- function(vec.num.rep, alpha_g, sigma2_g, phi_g){
    # Simulate possibly unbalanced counts from a NB mixed effect model (NBMM).
    # Under NBMM, an observed number of reads aligned to feature/gene \eqn{g}, 
    # \eqn{Y_{gsr}}, follows a negative binomial distribution with mean 
    # \eqn{\mu_{gs}} and variance \eqn{\mu_{gs}+\phi_{g} \mu_{gs}^2}, where 
    # \eqn{\phi_{g}} is the dispersion parameter, shared across strains. The 
    # generalized linear model uses a \eqn{\log}-link: 
    # \eqn{\log(\mu_{gs}) = \alpha_{g}+ b_{gs}, \;\;
    # b_{gs}\sim N(0, \sigma^{2}_{g}).}
    #
    # vec.num.rep: a vector of replicate numbers for each strain.
    # phi_g: dispersion parameter in the NB model, \eqn{\phi_{g}}; common across 
    #   strains. The greater the value of this parameter, the greater is the 
    #   variance.
    # sigma2_g: variance of the strain random effect, \eqn{\sigma^{2}_{g}}.
    # alpha_g: intercept term in the GLMM, \eqn{\alpha_{g}}.
    #
    # [OUTPUT]
    # NBcounts: a 1 by N matrix with NB counts. N is the total number of samples.
    #   Column names are sample names of the form "Ss_r", where S stands for 
    #   sample, s is the strain number, r is the replicate number within the 
    #   strain. 
    
    if(sigma2_g <0){
        stop("Random effect variance needs to be non-negative.")
    }
    if(phi_g <= 0){
        stop("Invalid dispersion value.")
    }
    
    num.strains <- length(vec.num.rep)
    strain.means <- exp(alpha_g + rnorm(num.strains, sd = sqrt(sigma2_g)))
    
    NBcounts <- lapply(1:num.strains, function(x){
        counts.x <- MASS::rnegbin(n = vec.num.rep[x],
                                  mu = strain.means[x],
                                  theta = 1/phi_g)
        return(counts.x)
    })
    NBcounts <- matrix(do.call(c, NBcounts), nrow = 1)
    
    sample.names <- lapply(1:num.strains, function(x){
        sample.x <- paste0("S", x, "_", 1:(vec.num.rep[x]))
        return(sample.x)
    })
    sample.names <- as.vector(do.call(c, sample.names))
    colnames(NBcounts) <- sample.names
    
    return(NBcounts)
}

#' Simulate a count matrix from negative binomial mixed effect models (NBMM).
#' 
#' Simulate a (possibly unbalanced) count matrix from NBMM.
#' Under NBMM, an observed number of reads aligned to feature/gene \eqn{g}, 
#' \eqn{Y_{gsr}}, follows a negative binomial (NB) distribution with mean 
#' \eqn{\mu_{gs}} and variance \eqn{\mu_{gs}+\phi_{g} \mu_{gs}^2}, where 
#' \eqn{\phi_g} is the dispersion parameter, shared across strains. The 
#' generalized linear model uses a \eqn{\log}-link:\cr
#'  \eqn{\log(\mu_{gs}) = \alpha_g+ b_{gs}, \;\;b_{gs}\sim N(0, \sigma^2_g).}
#' 
#' @param vec.num.rep A vector of replicate numbers for each strain.
#' @param alphas Intercept vector \eqn{\alpha_g}'s, 
#' \eqn{1 \times \texttt{num.features}}{1 x num.features}.
#' @param sigma2s Random effect variance vector \eqn{\sigma^2_g}'s, 
#' \eqn{1 \times \texttt{num.features}}{1 x num.features}.
#' @param phis Dispersion parameter in NB models, \eqn{\phi_g}'s, a
#' \eqn{1 \times \texttt{num.features}}{1 x num.features} vector.
#' @return A \eqn{G \times N}{G x N} matrix with NB reads. \eqn{N} is the 
#'   total number of samples; \eqn{G} is the number of features. Column names 
#'   are sample names of the form "Ss_r", where S stands for sample, s is the 
#'   strain number, r is the replicate number within the strain. Row names 
#'   are the feature names of the form "Gene g", where g is the feature index.
#' @examples
#' ## Generate a sequencing dataset with 5 features and 6 strains. 
#' ## Assign parameter values.
#' rep.num <- c(3, 5, 2, 3, 4, 2)
#' a0s <- c(-1, 1, 2, 5, 10)
#' sig2s <- c(10, 0.2, 0.1, 0.03, 0.01)
#' phis <- c(0.5, 1, 0.05, 0.01, 0.1)
#' 
#' set.seed(1234)
#' ## Generate reads:
#' nbData <- getReadMatrix.NB(rep.num, a0s, sig2s, phis)
#' @export
getReadMatrix.NB <- function(vec.num.rep, alphas, sigma2s, phis){
    
    num.probes <- length(alphas)
    CountMatrix <- lapply(1:num.probes, function(x){
        probe.x <- getNBReads(vec.num.rep, alphas[x], sigma2s[x], phis[x])
    })
    CountMatrix <- do.call(rbind, CountMatrix)
    rownames(CountMatrix) <- paste0("Gene ", 1:num.probes)
    return(CountMatrix)
}



###############################################################################
### Simulate compound Poisson distributed data matrix 

# (INTERNAL)
getCPReads <- function(vec.num.rep, alpha_g, sigma2_g, p_g, phi_g){   
    # Simulate possibly unbalanced reads from a CP mixed effect model.
    # For a CP random variable \eqn{Y_{gsr}} with mean \eqn{\mu_{gs}}, its 
    # variance can be expressed as \eqn{\phi_g\mu_{gs}^{p_g}}, for some 
    # \eqn{1<p_g<2}. Under the CPMM, with a \eqn{\log}-link, the regression on
    # the mean has the same form as the NBMM: 
    # \eqn{\log(\mu_{gs}) = \alpha_g+ b_{gs}, \;\;
    # b_{gs}\sim N(0, \sigma^2_g).} 
    #
    # vec.num.rep: a vector of replicate numbers for each strain.
    # alpha_g: intercept, \eqn{\alpha_g}}.
    # sigma2_g: random effect variance, \eqn{\sigma^2_g}}. 
    # p_g: Power parameter in CP models, \eqn{p_g}.
    # phi_g: Dispersion parameter in CP models, \eqn{phi_g}.
    #
    # [OUTPUT]
    # CPcounts: a 1 by N matrix with CP reads. N is the total number of samples.
    #   Column names are sample names of the form "Ss_r", where S stands for 
    #   sample, s is the strain number, r is the replicate number within the 
    #   strain. 
    
    
    if(abs(p_g - 1.5) >= 0.5){
        stop("The tweedie parameter p needs to satisfy 1<p<2.")
    }
    if(sigma2_g <0){
        stop("Random effect variance needs to be non-negative.")
    }
    if(phi_g <= 0){
        stop("Invalid dispersion value.")
    }
    
    num.strains <- length(vec.num.rep)
    mus <- exp(alpha_g + rnorm(num.strains, sd = sqrt(sigma2_g)))
    
    CPcounts <- lapply(1:length(mus), function(x){
        counts.x <- tweedie::rtweedie(vec.num.rep[x], xi = p_g, mu = mus[x], 
                                      phi = phi_g)
    })
    CPcounts <- matrix(do.call(c, CPcounts), nrow = 1)
    
    sample.names <- lapply(1:num.strains, function(x){
        sample.x <- paste0("S", x, "_", 1:(vec.num.rep[x]))
        return(sample.x)
    })
    sample.names <- as.vector(do.call(c, sample.names))
    
    colnames(CPcounts) <- sample.names
    
    return(CPcounts)
}


#' Simulate a read matrix from compound Poisson mixed effect models (CPMM).
#' 
#' Simulate a (possibly unbalanced) read matrix from CPMM.
#' For a compound Poisson (CP) random variable \eqn{Y_{gsr}} with mean 
#' \eqn{\mu_{gs}}, its variance can be expressed as 
#' \eqn{\phi_g\mu_{gs}^{p_g}}, for some \eqn{1<p_g<2}. Under the CPMM, with 
#' a \eqn{\log}-link, the regression on the mean has the form:\cr
#'  \eqn{\log(\mu_{gs}) = \alpha_g+ b_{gs}, \;\;b_{gs}\sim N(0, \sigma^2_g).} 
#' 
#' @param vec.num.rep A vector of replicate numbers for each strain.
#' @param alphas Intercept vector \eqn{\alpha_g}'s, 
#' \eqn{1 \times \texttt{num.features}}{1 x num.features}.
#' @param sigma2s Random effect variance vector \eqn{\sigma^2_g}'s, 
#' \eqn{1 \times \texttt{num.features}}{1 x num.features}.
#' @param ps Tweedie parameter in CP models, \eqn{p_g}'s, a 
#' \eqn{1 \times \texttt{num.features}}{1 x num.features} vector.
#' @param phis Dispersion parameter in CP models, \eqn{\phi_g}'s, a 
#' \eqn{1 \times \texttt{num.features}}{1 x num.features} vector.
#' @return A \eqn{G \times N}{G x N} matrix with CP reads. \eqn{N} is the 
#'   total number of samples; \eqn{G} is the number of features. Column names 
#'   are sample names of the form "Ss_r", where S stands for sample, s is the 
#'   strain number, r is the replicate number within the strain. Row names 
#'   are the feature names of the form "Gene g", where g is the feature index.
#'   
#' @examples
#' ## Generate a sequencing dataset with 5 features and 6 strains. 
#' ## Assign parameter values.
#' rep.num <- c(3, 5, 2, 3, 4, 2)
#' a0s <- c(-1, 1, 2, 5, 10)
#' sig2s <- c(10, 0.2, 0.1, 0.03, 0.01)
#' ps <- rep(1.5, 5)
#' phis <- c(1.5, 1, 0.5, 0.1, 0.1)
#' 
#' set.seed(1234)
#' ## Generate reads:
#' cpData <- getReadMatrix.CP(rep.num, a0s, sig2s, ps, phis)
#' ## Generate strain names:
#' str <- sapply(1:length(rep.num), function(x){
#'   str.x <- paste0("S", x)
#'   return(rep(str.x, rep.num[x]))
#' })
#' str <- do.call(c, str)
#' @export
getReadMatrix.CP <- function(vec.num.rep, alphas, sigma2s, ps, phis){
    
    num.probes <- length(alphas)
    CountMatrix <- lapply(1:num.probes, function(x){
        probe.x <- getCPReads(vec.num.rep, alphas[x], 
                              sigma2s[x], ps[x], phis[x])
    })
    CountMatrix <- do.call(rbind, CountMatrix)
    rownames(CountMatrix) <- paste0("Gene ", 1:num.probes)
    return(CountMatrix)
}



###############################################################################
### Functions to fit GLMM and compute NB VPC 

#' Fit negative binomial mixed models (NBMM) for one or more features.
#' 
#' Fit NBMM for one or more features and output the fit parameters. 
#' It is used before the function computeVPC.NB(). This function also allows 
#' to test the presence of heritability via random effect variance of the model. To fit a NBMM, the glmmADMB package is needed.
#' 
#' @param CountMatrix Sequencing count matrix for a list of features. Each row 
#' is for one feature, and the columns are for samples.
#' @param Strains Strain labels for the samples.
#' @param test TRUE or FALSE (default). Test the presence of heritability 
#' through examining the random effect variance \eqn{\sigma_g^2 = 0}{}.
#' @return A list with two objects. The first object is a \eqn{G \times 3}{G x 3} 
#' matrix indicating the fitted parameters for each feature. The columns are 
#' ordered by \eqn{\alpha_g, \sigma_g^2, \phi_g}. 
#' Row names are feature names. If the argument test is set to
#' be true, the second object of the list consists of p-values for testing 
#' the hypothesis that random effects \eqn{\sigma_a^2 = 0}{sigma_a2 = 0}; 
#' otherwise, the second object is NULL. 
#' @examples
#' \donttest{
#' ## Compute vpc for each feature under NBMM. This will take a while on the
#' ##  entire dataset. For the purpose of illustration, here we only fit on 
#' ##  the first 2 features.
#' result.nb <- fit.NB(simData[1:2, ], strains)
#' }
#' @export
fit.NB <- function(CountMatrix, Strains, test = FALSE){
   
    if(is.null(dim(CountMatrix))){
        print('Fitting a single feature.')
        CountMatrix <- matrix(CountMatrix, nrow = 1)
    }
    
    GeneIDs <- rownames(CountMatrix)
    paras <- t(pbapply::pbsapply(1:nrow(CountMatrix), function(x){
        CountVector <- CountMatrix[x, ]
        GeneID <- GeneIDs[x]
        dat_sub <- data.frame(expr = as.numeric(CountVector), strain = Strains)
        model_sub <- try({lme4::glmer.nb(formula = expr ~ 1 + (1|strain), 
                                         data = dat_sub, verbose = F,
                                         tol = 1e-4)}, silent=T)
        
        if (class(model_sub) != "try-error"){
            sum_model_sub <- summary(model_sub)
            sigma_a2 <- unlist(sum_model_sub$varcor)
            as <- sum_model_sub$coefficients[1]
            phi <- 1/lme4::getME(model_sub, "glmer.nb.theta")
            para_sub <- c(as, sigma_a2, phi)
        }else{
            print(paste("Fitting problem for feature", x,"returning NA"))
            para_sub <- rep(NA, 3)
        }
        
        if (test){
            model_sub_red <- try({MASS::glm.nb(formula = expr ~ 1, 
                                               data = dat_sub, link = 'log')}, 
                                 silent = TRUE)
            if (class(model_sub) != "try-error" & 
                class(model_sub_red) != "try-error"){
                test.stat <- 2*logLik(model_sub) - 2*logLik(model_sub_red)
                if (test.stat<1e-6) {test.stat <- 0} 
                # test.stat <- round(test.stat, digits = 1e-6)
                pval <- 0.5*pchisq(test.stat, df = 1, lower.tail = FALSE) + 
                    0.5*as.numeric(test.stat == 0)
            }else{
                print(paste("Cannot do test for feature", x,"fitting problem."))
                pval <- NA
            }
            para_sub <- c(para_sub, pval)
        }
        
        return(para_sub)
    
    }))
    
    paras1 <- matrix(paras[ , 1:3], ncol = 3)
    rownames(paras1) <- GeneIDs
    colnames(paras1) <- c("alpha_g", "sigma2_g", "phi_g")
    
    if (test){
        return(list(paras = paras1, pvals = paras[ , 4]))
    }else{
        return(list(paras = paras1, pvals = NULL))
    }
    
}






# (INTERNAL)
compute1NBVPC <- function(alpha_g, sigma2_g, phi_g){
    # Calculate the negative binomial icc 
    #
    # alpha_g: intercept.
    # sigma2_g: variance of the random factor.
    # phi_g: dispersion. 
    #
    # [OUTPUT]
    # vpc: a numerical value for variance partition coefficient computed based
    #   on negative binomial mixture model (NBMM).
    
    if(is.na(sigma2_g)){
        vpc <- NA
    }else{
        if(sigma2_g < 0){
            stop("Random effect variance needs to be non-negative.")
        }
        if(phi_g <= 0){
            stop("Invalid dispersion value.")
        }
        
        denom <- exp(sigma2_g) - 1 + exp(sigma2_g)*phi_g + exp(-alpha_g - sigma2_g / 2)
        if(denom == 0){
            vpc <- NA
        }else{
            vpc <- (exp(sigma2_g) - 1) / denom
        }
    }
    
    return(vpc)
    
}


#' Calculate the negative binomial (NB) variance partition coefficient (VPC)
#' for one or more features.
#' 
#' Calculate the NB VPC for one or more features following the model fitting 
#' function fit.NB().
#' 
#' @param para A \eqn{G \times 3}{G x 3} matrix of negative binomial fit 
#' parameters for \eqn{G} features, \eqn{G\geq 1}. The column order is intercept 
#' \eqn{\alpha_g}, random effect \eqn{\sigma_g^2 (\sigma_g^2\geq0)}, 
#'  and dispersion \eqn{\phi (\phi>0)}.
#' @return A \eqn{G \times 1}{G x 1} matrix consisting of VPC for
#' \eqn{G} features based on negative binomial mixed model. Column name 
#' is "NB-fit"; row names are the feature names. 
#' 
#' @examples
#' ## Compute VPC for each feature under negative binomial mixed model.
#' vpc.nb <- computeVPC.NB(para_nb)
#' 
#' ## Visulize the distribution of the VPCs. 
#' hist(vpc.nb, breaks = 50, col = "cyan")
#' 
#' ## Plot sorted VPCs.
#' plot(sort(vpc.nb), ylab = "Heritability (h2)", ylim = c(0,1), 
#' main = "Sorted NB VPC scores")
#' abline(h = 0.9, lty = 2, col = "red")
#' text(50, 0.92, "h2 = 0.9", col = "red")
#' @export
computeVPC.NB <- function(para){
    
    if(is.null(dim(para))){
        vpcs <- compute1NBVPC(para[1], para[2], para[3])
    }else{
        vpcs <- apply(para, 1, function(x){
            vpc <- compute1NBVPC(x[1], x[2], x[3])
            return(vpc)
        })
    }
    
    vpcs <- matrix(vpcs, ncol = 1)
    rownames(vpcs) <- rownames(para)
    colnames(vpcs) <- 'NB-fit'
    
    return(vpcs)
}





###############################################################################
### Fit compound Poisson models and estimated VPCs.

#' Fit compound Poisson mixed effect models (CPMM) for one or more features.
#' 
#' Fit a CPMM for one or more features and output the fit parameters. 
#' It is used before the function computeVPC.CP(). This function also allows 
#' to test the presence of heritability via random effect variance of the model.
#' 
#' @param CountMatrix Sequencing count matrix for one or more features. Each 
#' row is for one feature, and the columns are for samples. 
#' @param Strains Strain labels for the samples. 
#' @param test TRUE or FALSE (default). Test the presence of heritability 
#' through examining the random effect variance \eqn{\sigma_g^2 = 0}{}.
#' @param optimizer A character string that determines which optimization 
#' routine is to be used. Possible choices are "nlminb" (default), 
#' "L-BFGS-B", and "bobyqa". 
#' @return A list with two objects. The first object is a 
#' \eqn{G \times 4}{G x 4} matrix indicating the fitted parameters for each 
#' feature. The columns are ordered by intercept \eqn{\alpha_g}, tweedie 
#' parameter \eqn{p_g}, random effect variance \eqn{\sigma^2_g}, and dispersion
#' \eqn{\phi_g}. Row names are feature names. If the argument test is set to
#' be true, the second object of the list consists of p-values for testing 
#' the hypothesis that random effects \eqn{\sigma_a^2 = 0}{sigma_a2 = 0}; 
#' otherwise, the second object is NULL. 
#' 
#' @examples
#' ## Fit CPMM for the first two features and test the presence of 
#' ## heritability. 
#' result.cp <- fit.CP(simData[1:2, ], strains, test = TRUE)
#' ## Extract parameters
#' para.cp <- result.cp[[1]]
#' ## Extract p-values
#' pval.cp <- result.cp[[2]]
#' 
#' @export
fit.CP <- function(CountMatrix, Strains, test = FALSE, optimizer = "nlminb"){
    # Fit a compound Poisson mixed effect model for a list of probes/genes and 
    #   output the fit parameters.
    #
    # CountMatrix: sequencing count matrix for a list of probes/genes. Each row 
    #   is for one probe/gene, and the columns are for samples. 
    # Strains: strain label for the samples. 
    # test: Logical argument indicating whether to do a test for significance of 
    #   random effects.
    # optimizer: A character string that determines which optimization routine is
    #   to be used. Possible choices are "nlminb" (default), "L-BFGS-B", and 
    #   "bobyqa".
    #
    # [OUTPUT]
    # Return a list with two members. The first is a G by 4 matrix 
    #   indicating the fitted parameters for each gene. The columns are ordered by 
    #   "alphas", "p", sigma_a2", "phi". Row names are gene names; the second 
    #   member of the list consists of p-values for testing the hypothesis that 
    #   sigma2_g = 0.
    
    
    if(is.null(dim(CountMatrix))){
        print('Fitting a single feature.')
        CountMatrix <- matrix(CountMatrix, nrow = 1)
    }
    
    paras <- t(pbapply::pbsapply(1:nrow(CountMatrix), function(x){
        
        CountVector <- CountMatrix[x, ]
        dat_sub <- data.frame(expr = as.numeric(CountVector), strain = Strains)
        
        fit <- tryCatch({
            fit1 <- cpglmm(expr ~ 1 + (1|strain), data = dat_sub, 
                           optimizer = optimizer)
        }, error=function(err){
            fit1 <- try({cpglmm(expr ~ 1 + (1|strain), data = dat_sub, 
                                optimizer = optimizer)}) 
            return(fit1)
        })
        
        if (class(fit) != "try-error"){
            as <- fit$fixef
            sigma_a2 <- as.numeric(cplm::VarCorr(fit)$strain)
            p <- fit$p
            phi <- fit$phi
            
            para <- c(as, sigma_a2, p, phi)
        }else{
            print(paste("Fitting problem for feature", x, "returning NA"))
            para <- rep(NA, 4)
        }
        
        ### Fitting the reduced model for testing significance of the random effect ###
        if (test){
            fit.red <- tryCatch({
                fit2 <- cplm::cpglm(expr ~ 1, data = dat_sub, optimizer = optimizer)
            }, error=function(err){
                fit2 <- try({cplm::cpglm(expr ~ 1, data = dat_sub, optimizer = optimizer)})
                return(fit2)
            })
            
            if (class(fit.red) != "try-error" & class(fit) != "try-error"){
                test.stat <- 2*cplm::logLik(fit)+cplm::AIC(fit.red)-2
                
                if (test.stat<1e-6) {test.stat <- 0}
                pval <- 0.5*pchisq(test.stat, df = 1, lower.tail = FALSE) + 
                    0.5*as.numeric(test.stat == 0)
            }else{
                print(paste("Cannot do test for feature", x, "fitting problem."))
                pval <- NA
            }
            
            para <- c(para, pval)
        }
        
        
        return(para)
    }))
    
    paras1 <- matrix(paras[,1:4], ncol = 4)
    rownames(paras1) <- rownames(CountMatrix)  
    colnames(paras1) <- c("alpha_g", "sigma2_g", "p_g", "phi_g")
    
    if (test){
        return(list(paras = paras1,  pvals = paras[ , 5]))
    }else{
        return(list(paras = paras1,  pvals = NULL))
    }
    
}






# (INTERNAL)
compute1CPVPC <- function(alpha_g, sigma2_g, p_g, phi_g){
    # Calculate the compound Poisson variance partition coefficient.
    #
    # alpha_g: intercept.
    # sigma2_g: variance of the random factor.
    # p_g: power index.
    # phi_g: dispersion
    #
    # [OUTPUT]
    # vpc: a numerical value for variance partition coefficient computed based
    #   on compound Poisson mixture model (CPMM).
    
    if(is.na(sigma2_g)){
        vpc <- NA
    }else{
        if(abs(p_g - 1.5) >= 0.5){
            stop("The tweedie parameter p needs to satisfy 1<p<2.")
        }
        if(sigma2_g <0){
            stop("Random effect variance needs to be non-negative.")
        }
        if(phi_g <= 0){
            stop("Invalid dispersion value.")
        }
        
        vpc.numerator <- exp(2 * alpha_g + 2 * sigma2_g ) - exp(2 * alpha_g + sigma2_g)
        
        denom <- vpc.numerator + phi_g * exp(p_g * alpha_g + p_g^2 * sigma2_g / 2)
        if(denom == 0){
            vpc <- NA
        }else{
            vpc <- vpc.numerator/denom
        }
    }
    
    return(vpc)
}


#' Calculate the compound Poisson (CP) variance partition coefficient (VPC) 
#' for one or more features.
#' 
#' Calculate the CP VPC for one or more features following the model fitting 
#' function fit.CP().
#' 
#' @param para A \eqn{G \times 4}{G x 4} matrix of CP fit parameters for 
#' \eqn{G} features, \eqn{G\geq 1}. The column order is intercept 
#' \eqn{\alpha_g}, random effect \eqn{\sigma_g^2 (\sigma_g^2\geq0)}, 
#' tweedie parameter \eqn{p_g (1<p_g<2)},
#'  and dispersion \eqn{\phi (\phi>0)}.
#' @return A \eqn{G \times 1}{G x 1} matrix consisting of VPC for
#'   G features based on compound Poisson mixed models. Column name is 
#'   "CP-fit"; row names are the feature names.
#'   
#' @examples
#' ## Compute VPC for each feature under compound Poisson mixed models. 
#' vpc.cp <- computeVPC.CP(para_cp) 
#' 
#' ## Visulize the distribution of the VPCs. 
#' hist(vpc.cp, breaks = 50, col = "cyan")
#' 
#' ## Plot sorted VPCs.
#' plot(sort(vpc.cp), ylab = "Heritability (h2)", ylim = c(0,1), main = "Sorted CP VPC scores")
#' abline(h = 0.9, lty = 2, col = "red")
#' text(50, 0.92, "h2 = 0.9", col = "red")
#' @export
computeVPC.CP <- function(para){
    
    if(is.null(dim(para))){
        vpcs <- compute1CPVPC(para[1], para[2], para[3], para[4])
    }else{
        vpcs <- apply(para, 1, function(x){
            vpc <- compute1CPVPC(x[1], x[2], x[3], x[4])
            return(vpc)
        })
    }
    
    vpcs <- matrix(vpcs, ncol = 1)
    rownames(vpcs) <- rownames(para)
    colnames(vpcs) <- 'CP-fit'
    
    return(vpcs)
}




###############################################################################
### Fit linear mixed models and compute VPCs.

# (INTERNAL)
fitandcompute1lmerVPC <- function(CountVector, Strains, PriorWeight = NULL, test = FALSE){
    # Compute the VPC value for one feature
    #
    # CountVector: sequencing counts for the feature.
    # Strains: strain labels for each sample.
    # PriorWeight: weights used in the lmer function.
    #
    # [OUTPUT]
    # Return a list with two members. The first is a numerical value indicating 
    #   the variance partition coefficient (vpc) under a linear mixed model 
    #   (LMM); the second member of the list is the p-values from testing 
    #   the hypothesis that there is no random effect.
    
    dat_sub <- data.frame(expr = CountVector, strain = Strains)
    model_sub <- lme4::lmer(formula = expr ~ 1 + (1|strain), data = dat_sub, 
                            weights = PriorWeight)
    sum_model_sub <- summary(model_sub)
    residual_var <- (sum_model_sub$sigma)^2
    random_intercept_var <- unlist(sum_model_sub$varcor)
    vpc <- random_intercept_var/(residual_var + random_intercept_var)
    
    if (test){
        model_sub_red <- lm(formula = expr ~ 1, data = dat_sub, 
                            weights = PriorWeight)
        test.stat <- 2*logLik(model_sub) - 2*logLik(model_sub_red)
        if (test.stat<1e-6) {test.stat <- 0}
        pval <- 0.5*pchisq(test.stat, df = 1, lower.tail = FALSE) + 
            0.5*as.numeric(test.stat == 0)
        return(list(vpc = vpc, pval = pval))
    }else{
        return(vpc)
    }  
}


#' Fit linear mixed models (LMM) and compute the VPC values for one or more 
#' features.
#' 
#' Fit the Gaussian-like data to LMM and compute the VPC values for 
#' one or more features.
#' 
#' @param CountMatrix Sequencing count matrix for one or more features. Each 
#' row is for one feature, and the columns are for samples. 
#' @param Strains Strain labels for the samples. 
#' @param PriorWeights Weights used in the lmer function in the package lme4. 
#' It is an optional vector used in the fitting process. 
#' @param test TRUE or FALSE (default). Test the presence of heritability 
#' through examining the random effect variance \eqn{\sigma_g^2 = 0}{}.
#' @param VPCname Name of the VPC result, default = "LMM".
#' @return A list with two objects. The first object is a 
#' \eqn{1 \times G}{1 x G} vector indicating the variance partition coefficients
#' (VPC). If the argument test is set to be true, the second object of 
#' the list consists of p-values for testing the hypothesis that random 
#' effects \eqn{\sigma_a^2 = 0}{sigma_a2 = 0}; otherwise, the second 
#' object is NULL. 
#' 
#' @examples
#' ## Compute VPC for the first two features under linear mixed models for Gaussian-like datasets. 
#' 
#' ## Provide normalized data and include hypothesis testing on presence of
#' ## heritability:
#' result.vst <- fitComputeVPC.lmer(simData_vst[1:2,], strains, test = TRUE)
#' ## Extract parameters
#' vpc.vst <- result.vst[[1]]
#' ## Extract p-values
#' pval.vst <- result.vst[[2]]
#' 
#' ## Visulize the distribution of p-values.
#' hist(pval.vst, breaks = 30, col = "cyan")
#' @export
fitComputeVPC.lmer <- function(CountMatrix, Strains, PriorWeights = NULL, 
                               test = FALSE, VPCname = "LMM"){
    if(is.null(dim(CountMatrix))){
        print('Fitting a single feature.')
        CountMatrix <- matrix(CountMatrix, nrow = 1)
    }
    
    VPC <- pbapply::pbsapply(1:nrow(CountMatrix), function(x){
        vpc.x <- fitandcompute1lmerVPC(CountMatrix[x, ], Strains, 
                                       PriorWeights[x, ], test = test)
        if (test){
            return(c(vpc.x$vpc, vpc.x$pval))
        }else{
            return(vpc.x)
        }
    })
    VPC <- as.matrix(VPC)
    if(test){
        VPC = t(VPC)
        pvals <- matrix(VPC[,2], ncol = 1)
        rownames(pvals) <- rownames(CountMatrix)
        colnames(pvals) <- 'P-value'
    }
    
    vpcs = as.matrix(VPC[,1], ncol = 1)
    
    rownames(vpcs) <- rownames(CountMatrix)
    colnames(vpcs) <- VPCname
    
    if (test){
        return(list(vpcs=vpcs, pvals=pvals))
    }else{
        return(list(vpcs=vpcs, pvals=NULL))
    }
}






#' Compute variance partition coefficition (VPC) confidence intervals (CI) 
#' for one or more features.
#' 
#' Compute VPC CI based on parametric bootstrap for one or more features.
#' 
#' @param CountMatrix A \eqn{G\times N} count matrix. \eqn{G} is the number of 
#' features; \eqn{N} is the total number of samples.
#' @param Strains A \eqn{1\times N} vector of strain labels corresponding to 
#' each sample.
#' @param which.features A \eqn{1\times k} vector of select feature numbers 
#' for which CI is desired. \eqn{k\leq G}.
#' @param num.boot Number of bootstraps.
#' @param method Which method should be used, "CP-fit", "NB-fit" (default), 
#' or "VST". "VST" method bootstraps data under negative binomial mixed models.
#' @param alpha A numerical value between 0 and 1, indicating the significance 
#' level of the CI. The CI will be \eqn{100*(1-\alpha)}{100*(1-alpha)} 
#' percent CI. Default value is 0.05.
#' @param optimizer A character string that determines which optimization 
#' routine is to be used. It is only used for method = "CP-fit". Possible 
#' choices are "nlminb" (default), "L-BFGS-B", and "bobyqa".
#' @return A list of two objects. The first object is a \eqn{k \times 2} 
#' matrix containing the CI. The second object consists of a 
#' \eqn{k \times}num.boot matrix of all bootsrapped VPC values.
#' 
#' 
#' @export
getBootCI = function(CountMatrix, Strains, which.features, num.boot,
                     method="NB-fit", alpha=0.05, optimizer = "nlminb"){
    
    num.features <- length(which.features)
    
    all.vpcs = matrix(NA, nrow = num.features, ncol=num.boot)
    vec.num.rep = as.numeric(table(Strains))
    
    if (method=="NB-fit"){
        print("Getting initial point estimates using the NB-fit method")
        fit = fit.NB(CountMatrix[which.features,], Strains)
        if(num.features == 1){fit$paras <- matrix(fit$paras, nrow = 1)}
        
        for (i in 1:num.features){
            print(paste("Bootstraping feature",i))
            boot.data = getReadMatrix.NB(vec.num.rep, 
                                         rep(fit$paras[i,1],num.boot), 
                                         rep(fit$paras[i,2],num.boot),
                                         rep(fit$paras[i,3],num.boot))
            fit.i = fit.NB(boot.data, Strains)
            all.vpcs[i,] = computeVPC.NB(fit.i$paras)
        }
        
        intervals = cbind( apply(all.vpcs, 1, quantile, probs = alpha/2),
                           apply(all.vpcs, 1, quantile, probs = 1-alpha/2))
        
        return(list(intervals = intervals, all.vpcs = all.vpcs))
    }else if (method=="CP-fit"){
        print("Getting initial point estimates using the CP-fit method")
        fit = fit.CP(CountMatrix[which.features,], Strains, optimizer = optimizer)
        if(num.features == 1){fit$paras <- matrix(fit$paras, nrow = 1)}
        
        for (i in 1:num.features){
            print(paste("Bootstraping feature",i))
            boot.data = getReadMatrix.CP(vec.num.rep, 
                                         rep(fit$paras[i,1],num.boot), 
                                         rep(fit$paras[i,2],num.boot),
                                         rep(fit$paras[i,3],num.boot),
                                         rep(fit$paras[i,4],num.boot))
            fit.i = fit.CP(boot.data, Strains, optimizer = optimizer)
            all.vpcs[i,] = computeVPC.CP(fit.i$paras)
        }
        
        intervals = cbind( apply(all.vpcs, 1, quantile, probs = alpha/2),
                           apply(all.vpcs, 1, quantile, probs = 1-alpha/2))
        
        return(list(intervals = intervals, all.vpcs = all.vpcs))
    }else if (method=="VST"){
        print("Getting initial point estimates using the VST method")
        fit = fit.NB(CountMatrix[which.features,], Strains)
        if(num.features == 1){fit$paras <- matrix(fit$paras, nrow = 1)}
        
        for (i in 1:num.features){
            print(paste("Bootstraping feature",i))
            boot.data = getReadMatrix.NB(vec.num.rep, 
                                         rep(fit$paras[i,1],num.boot), 
                                         rep(fit$paras[i,2],num.boot),
                                         rep(fit$paras[i,3],num.boot))
            
            cds <- DESeq2::DESeqDataSetFromMatrix(boot.data, 
                                                  data.frame(strain = Strains), 
                                                  formula(~strain))
            
            geoMeans <- apply(boot.data, 1, function(cnt){
                # Alternative method for calculating the geometric means to allow minimum count to be zero for all features. 
                if(all(cnt == 0)){ 
                    return(0)
                }else{
                    return(exp(mean(log(cnt[cnt != 0]))))
                }
            })
            cds <- DESeq2::estimateSizeFactors(cds, geoMeans = geoMeans)
            
            if (sum(is.na(sizeFactors(cds)))>0){   
                cds <- DESeq2::DESeqDataSetFromMatrix(1+boot.data, 
                                                      data.frame(strain = Strains), 
                                                      formula(~strain)) 
                # 1 added to avoid too many low counts in those cases
                # This adds a small variation to the data
                cds <- DESeq2::estimateSizeFactors(cds)
            }
            cds <- DESeq2::estimateDispersions(cds, fitType = "local")
            vsd <- DESeq2::varianceStabilizingTransformation(cds, fitType = "local")
            vsd <- SummarizedExperiment::assay(vsd)
            
            all.vpcs[i,] = fitComputeVPC.lmer(vsd, Strains)$vpcs
        }
        
        intervals = cbind( apply(all.vpcs, 1, quantile, probs = alpha/2),
                           apply(all.vpcs, 1, quantile, probs = 1-alpha/2))
        
        return(list(intervals = intervals, all.vpcs = all.vpcs))
    }else{
        stop("Invalid method type. Possible methods are: NB-fit, CP-fit, and VST.")
    }
}


