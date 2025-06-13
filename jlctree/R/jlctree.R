#' Fits Joint Latent Class Tree (JLCT) model.
#'
#' Fits Joint Latent Class Tree model. 
#'  This is the main function that is normally called by the user.
#'  See \code{\link{jlctree-package}} for more details.
#'
#' @param survival a two-sided formula object; required. The left side of the formula corresponds
#'      to a \code{Surv()} object of type ``counting'' for left-truncated right-censored (LTRC) data,
#'      or of type ``right'' for right-censored data.
#'      The right side of the formula specifies the names of covariates to include in the survival model,
#'      excluding the longitudinal outcome.
#' @param classmb one-sided formula describing the covariates in the class-membership tree construction; required.
#'      Covariates used for tree construction are separated by \code{+} on the right of \code{~}.
#' @param fixed two-sided linear formula object for the fixed-effects in the linear mixed-effects model for 
#'      longitudinal outcomes; required.
#'      The longitudinal outcome is on the left of \code{~} and the covariates are separated by \code{+} 
#'      on the right of \code{~}.
#' @param random one-sided formula for the node-specific random effects in the linear mixed-effects model for 
#'      longitudinal outcomes; optional. 
#'      If missing, there are no node-specific random effects in the fitted linear mixed-effects model.
#'      Covariates with a random effect are separated by \code{+} on the right of \code{~}.
#' @param subject name of the covariate representing the subject identifier; optional. 
#'      If missing, there are no subject-specific random intercepts in the fitted linear mixed-effects model for
#'      longitudinal outcomes.
#' @param data the dataset; required.
#' @param parms parameter list of Joint Latent Class Tree model parameters. 
#'      See also \code{jlctree.control}.
#' @param control \code{rpart} control parameters. See also \code{rpart.control}.
#'
#' @return  A list with components:
#'  \item{tree}{an \code{rpart} object, containing the constructed Joint Latent Class tree.}
#'  \item{control}{the \code{rpart.control} parameters.}
#'  \item{parms}{the \code{jlctree.control} parameters.}
#'  \item{lmmmodel}{an \code{lme4} object, containing the linear mixed-effects effects model
#'      with fixed effects, node-specific random effects (if valid), 
#'      and subject-specific random intercepts (if valid). 
#'      Returned when \code{fity} is TRUE.}
#'  \item{coxphmodel_diffh_diffs}{a \code{coxph} object, containing a Cox PH model
#'      with different hazards and different slopes across terminal nodes.
#'      Returned when \code{fits} is TRUE.}
#'   \item{coxphmodel_diffh}{a \code{coxph} object, containing a Cox PH model 
#'      with different hazards but same slopes across terminal nodes.
#'      Returned when \code{fits} is TRUE.}
#'   \item{coxphmodel_diffs}{a \code{coxph} object, containing a Cox PH model 
#'      with same hazards but different slopes across terminal nodes.
#'      Returned when \code{fits} is TRUE.}
#'
#' @seealso \code{\link{jlctree-package}, \link{jlctree.control}, \link{rpart.control}}
#' @examples 
#'  # Time-to-event in LTRC format:
#'  data(data_timevar)
#'  tree <- jlctree(survival=Surv(time_L, time_Y, delta)~X3+X4+X5,
#'                  classmb=~X1+X2, fixed=y~X1+X2+X3+X4+X5, random=~1,
#'                  subject='ID',data=subset(data_timevar, ID<=30),
#'                  parms=list(maxng=4, fity=FALSE, fits=FALSE))
#'
#'  # Time-to-event in right-censored format:
#'  data(data_timeinv)
#'  tree <- jlctree(survival=Surv(time_Y, delta)~X3+X4+X5,
#'                  classmb=~X1+X2, fixed=y~X1+X2+X3+X4+X5, random=~1,
#'                  subject='ID', data=subset(data_timeinv, ID<=30),
#'                  parms=list(maxng=4, fity=FALSE, fits=FALSE))
#'
#' @importFrom rpart rpart rpart.control prune
#' @importFrom survival coxph Surv
#' @importFrom lme4 lmer
#' @importFrom stats as.formula formula model.frame terms lm
#' @export


jlctree <- function
(survival, classmb, fixed, random,
 subject, data,
 parms=list(), control=list()
){

    # send warning if the longitudinal outcome is specified in survival.
    survvars <- labels(terms(survival)) 
    if(length(survvars)==0)
        survvars <- '1'
    yvar <- deparse(fixed[[2]]) 
    if(yvar %in% survvars){
        warning(paste0("'survival' should not include the 
                       longitudinal outcome. Remove ",
                       yvar, " from 'survival'"))
        survvars <- setdiff(survvars, yvar)
    }

    # parameters 
    if(is.null(parms$min.nevents))
        parms$min.nevents <- length(survvars)
    parms  <- do.call("jlctree.control", parms)

    if (length(survival[[2]])==4){
        parms$LTRC <- TRUE
    } else if (length(survival[[2]])==3){
        parms$LTRC <- FALSE
    } else {
        stop("Invalid survival argument.")
    }


#    # check if longitudinal and survival covariates are truly time-invariant.
#    if(!parms$LTRC){
#        ID <- data[,subject]; IDorder <- order(ID); ID <- ID[IDorder]
#        IDdiff <- diff(ID); 
#        for (vname in c(yvar,survvars)){
#            val <- data[,vname][IDorder]
#            valdiff <- diff(val)
#            if(any(valdiff[IDdiff==0]!=0))
#                warning(paste0(vname, " is time varying. Data should be in LTRC format. "))
#        }
#    }
#

    survobj <- deparse(survival[[2]])
    rpartf <- paste0('cbind(', paste0(c(survobj, yvar, survvars), collapse=','),')',
                     paste0(as.character(classmb), collapse=''))
    rparty <- model.frame(formula(rpartf), data=data)

    # standardize covariates for splitting related coxph model fitting, 
    # so we can ignore coxph fittings with too high variances. 
    if (parms$stable){
        start_col <- 3+as.numeric(parms$LTRC)
        end_col <- ncol(rparty[,1])
        rparty[,1][,c(start_col:end_col)] <- scale(rparty[,1][,c(start_col:end_col)])
    }

    # rpart only takes in numerical parameters
    parms$lrt <- (parms$test.stat=='lrt')
    parms$test.stat <- NULL

    # rpart.control
    control <- do.call('rpart.control', control)

    # Fit rpart 
    survlist <- list(eval=surve, split=survs, init=survi)
    tree <- rpart(rparty, control=control, method=survlist, parms=parms, model=TRUE)
    parms$test.stat <- ifelse(parms$lrt, 'lrt', 'wald')

    # Prune to have at most parms$maxng nodes.
    tree <- prune_tree(tree, parms$maxng)
    data$node <- as.factor(tree$where)
    RET <- list(tree=tree, control=control, parms=parms)


    # Fit lmm model.
    if(parms$fity){
        if(missing(random)){
            cat(paste0('Argument "random" is missing. No node-specific random effects in linear mixed-effects model for ', yvar,'.\n'))
            node_ranef <- ''
        } else {
            randvars <- labels(terms(random)); 
            if (length(randvars) == 0) 
                randvars  <- '1'

            if(length(unique(tree$where)) > 1){
                node_ranef <- paste0('+ (', paste0(randvars, collapse='+') ,'|node)')
            } else {
                warning(paste0('Drop node-specific random effects from linear mixed-effects model for ', yvar,
                              ', since there is only one node.'))

                node_ranef <- ''
            }
        }

        if (missing(subject)){
            cat(paste0('Argument "subject" is missing. No subject-specific random intercepts in linear mixed-effects model for ', yvar,'.\n'))
            subj_ef <- ''
        } else {
            if(any(table(data[,subject]) > 1)){
                subj_ef <-  paste0('+ (1|', subject,')')
            } else {
                warning(paste0('Drop subject-specific random intercepts from linear mixed-effects model for ', yvar,
                              ', since there is only one observation per subject.'))

                subj_ef <-  ''
            }
        }

        lmerf <- as.formula(paste0(Reduce(paste0, deparse(fixed)), node_ranef,  subj_ef ))

        if (node_ranef == '' & subj_ef == '' ){
            lmmmodel <- lm(lmerf, data=data)
        } else{ 
            lmmmodel <- lmer(lmerf, data=data)
        }

        RET$lmmmodel <- lmmmodel
    }

    # Fit three versions of cox model:
    # diffh_diffs: different cox slopes and different baseline hazards.
    # diffh: different baseline hazards, 
    # diffs: different cox slopes.
    if(parms$fits){
        if(length(unique(tree$where)) == 1){
            coxphf_diffs <- coxphf_diffh <- coxph_formula_diffh_diffs <- 
                as.formula(paste0(deparse(survival[[2]]),'~',
                                  paste0(survvars, collapse='+')))

        } else {
            coxphf_diffh_diffs <- as.formula(paste0(deparse(survival[[2]]), '~',
                                                    paste0(paste0(survvars,'*node'), collapse='+'),
                                                    '+', 'strata(node)'))
            coxphf_diffh <- as.formula(paste0(deparse(survival[[2]]), '~',
                                              paste0(paste0(survvars), collapse='+'),
                                              '+', 'strata(node)'))
            coxphf_diffs <- as.formula(paste0(deparse(survival[[2]]), '~',
                                              paste0(paste0(survvars, '*node'), collapse='+')))
        }

        coxphmodel_diffh_diffs <- tryCatch(suppressWarnings(coxph(coxphf_diffh_diffs, data, model=TRUE)), 
                                           error=function(e) NULL)
        coxphmodel_diffh <- tryCatch(suppressWarnings(coxph(coxphf_diffh, data, model=TRUE)), 
                                     error=function(e) NULL)
        coxphmodel_diffs <- tryCatch(suppressWarnings(coxph(coxphf_diffs, data, model=TRUE)), 
                                     error=function(e) NULL)

        RET$coxphmodel_diffh_diffs <- coxphmodel_diffh_diffs
        RET$coxphmodel_diffh <- coxphmodel_diffh
        RET$coxphmodel_diffs <- coxphmodel_diffs
    }

    class(RET) <- 'jlctree'
    return(RET)

}
