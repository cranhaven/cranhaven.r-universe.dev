## Dynamically generate all the restriction functions of
## OU and Brownian motions at package-preperation time.

pkg_env = environment()

restriction_names   = list(
  's' = 'sym',
  'l' = 'logspd',
  'c' = 'spd',
  'd' = 'diag',
  'e' = 'logdiag',
  'k' = '+++PLACEHOLDER+++',
  '0' = 'zero',
  'f' = 'fixed'
)

avail_restrict_cmds = list(
  M=c('s','l','c','d','e','k','0','f'),
  V=c('0','k','f'),
  L=c('k','d','f')
)

special_cases = list(brn=list(M='0',V='0'))

## a: k^2 term
## b: k   term
## c: k*(k+1)/2 term
## d: 1   term
npar2k = function(y,a,b,c,d) {
  if (2L*a+c!=0)
    -(2L*b+c-as.integer(sqrt(16L*a*(y-d)+4L*b*(b+c)+c*c+8L*c*(y-d))))%/%(2L*(2L*a+c))
  else
    -(2L*(d-y))%/%(2L*b+c)
}


## Returns NULL if and only if either
##  1. all of m, v, l are 'k'
##  2. all of m, v, l are 'f'
##  3. one of m, v are zero but not both.
get_name_stub = function (pat) {
  if ((pat[['M']] == '0' && pat[['V']] != '0')||(pat[['M']] != '0' && pat[['V']] == '0')) {
    return(NULL)
  }
  if (pat[['M']] == 'f' && pat[['V']] == 'f' && pat[['L']] == 'f') {
    return(NULL)
  }
  match = NULL
  for (i in seq_along(special_cases)) {
    matched = T
    names_matched = list()
    for (n in c('M','V','L'))
      if (n %in% names(special_cases[[i]])) {
        if (special_cases[[i]][[n]] != pat[[n]]) {
          matched = F
        } else {
          names_matched = c(names_matched, list(n))
        }
      }
    if (matched) {
      match = i
      for (n in names_matched)
        pat[[n]] = NULL
      break;
    }
  }
  done_part = if (!is.null(match)) names(special_cases)[match]
              else                 done_part = 'ou'
  pat[which(pat=='k')] = NULL
  for (i in seq_along(pat)) {
    if (names(pat)[i] == 'M') {
      done_part = paste0(done_part, '_', restriction_names[[pat[[i]]]], 'H')
    } else if (names(pat)[i] == 'V') {
      done_part = paste0(done_part, '_', restriction_names[[pat[[i]]]], 'theta')
    } else if (names(pat)[i] == 'L') {
      done_part = paste0(done_part, '_', restriction_names[[pat[[i]]]], 'Sig')
    } else {
      stop('Error compiling the names of restriction functions')
    }
  }
  if (done_part == 'ou') return(NULL)
  else                   return(list(par    = done_part,
                                     jac    = paste0('d', done_part),
                                     hess   = paste0('h', done_part),
                                     nparams= paste0('nparams_',done_part)))
}


mkcmd = function (pat) {
  s = ''
  for (i in seq_along(pat)) s = paste0(s, names(pat)[i], pat[[i]])
  return(s)
}

cmd2abcd = function (cmd) {
  cmd_s = strsplit(cmd, '')[[1]]
  curstate = 1L
  i = 1L
  res = list(a=0L,b=0L,c=0L,d=0L)
  repeat{
    if (curstate == 1L) {
      switch(cmd_s[[i]],
             M={curstate = 2L},
             V={curstate = 3L},
             L={curstate = 4L})
    } else if (curstate == 2L) { #M
      switch(cmd_s[[i]],
             s= {res[['c']]=res[['c']]+1L},
             l= {res[['c']]=res[['c']]+1L},
             c= {res[['c']]=res[['c']]+1L},
             d= {res[['b']]=res[['b']]+1L},
             e= {res[['b']]=res[['b']]+1L},
             k= {res[['a']]=res[['a']]+1L},
             '0'= {NULL;},
             f= {NULL;})
      curstate = 1L
    } else if (curstate == 3L) { #V
      switch(cmd_s[[i]],
             k= {res[['b']]=res[['b']]+1L},
             '0'= {NULL;},
             f= {NULL;})
      curstate = 1L
    } else if (curstate == 4L) { #L
      switch(cmd_s[[i]],
             d= {res[['b']]=res[['b']]+1L},
             k= {res[['c']]=res[['c']]+1L},
             '0'= {NULL;},
             f= {NULL;})
      curstate = 1L
    } else stop("Error pasing command line in cmd2abcd")
    i=i+1
    if (i > length(cmd_s)) break;
  }
  res
}

build_par = function (pat) {
  cmd = mkcmd(pat)
  body = quote({
    parfn
    BUILD_FIXED__;
    f=function (par, ...) {
      mode(par) = 'double'
      parfn(.Call(Rparamrestrict, s, par, npar2k(length(par),a,b,c,d), FIXEDPART__), ...)
    }
    attr(f,'srcref') = NULL
    f
  })
  outerargs = alist(parfn=)
  tosub = c(list('s'=cmd),
            cmd2abcd(cmd),
            list('FIXEDPART__'  =quote(NULL),
                 'BUILD_FIXED__'=quote(NULL)))
  outerargs = c(outerargs, fixed_arg <- mk_fixed_arg(pat))
  if (0L != length(fixed_arg)) {
    tosub_f = alist(SUB_H__    =NULL,
                    SUB_THETA__=NULL,
                    SUB_SIG__  =NULL)
    cnt = 1L
    for (i in seq_along(names(fixed_arg))) {
      if (names(fixed_arg)[i] == 'H')
        tosub_f[['SUB_H__']]     = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(H)) stop('The argument `H` must be numeric')
            mode(H)='double';
            H
          }
        }, list(j=cnt))
      else if (names(fixed_arg)[i] == 'theta')
        tosub_f[['SUB_THETA__']] = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(theta)) stop('The argument `theta` must be numeric')
            mode(theta)='double';
            theta
          }
        }, list(j=cnt))
      else if (names(fixed_arg)[i] == 'Sig')
        tosub_f[['SUB_SIG__']]   = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(Sig)) stop('The argument `Sig` must be numeric')
            mode(Sig)='double';
            Sig
          }
        }, list(j=cnt))
      else stop('Error building fixed-parameter restriction functions')
      cnt = cnt+1L
    }
    tosub[['BUILD_FIXED__']]        = substitute({
      fixedpart = list()
      SUB_H__
      SUB_THETA__
      SUB_SIG__
    }, tosub_f)
    tosub[['FIXEDPART__']]          = quote(fixedpart)
  }
  body = substituteDirect(body, tosub)
  substitute(
    `function`(A, B),
    list(A=as.pairlist(outerargs), B=body))
}

build_jac = function (pat) {
  cmd = mkcmd(pat)
  body = quote({
    jacfn
    BUILD_FIXED__;
    f=function (par, ...) {
      mode(par) = 'double'
      k = npar2k(length(par),a,b,c,d)
      .Call(Rpostjacrestrict, s, par,
            jacfn(.Call(Rparamrestrict, s, par, k, FIXEDPART__), ...), k)
    }
    attr(f,'srcref') = NULL
    f
  })
  outerargs = alist(jacfn=)
  tosub = c(list('s'=cmd),
            cmd2abcd(cmd),
            list('FIXEDPART__'  =quote(NULL),
                 'BUILD_FIXED__'=quote(NULL)))
  outerargs = c(outerargs, fixed_arg <- mk_fixed_arg(pat))
  if (0L != length(fixed_arg)) {
    tosub_f = alist(SUB_H__    =NULL,
                    SUB_THETA__=NULL,
                    SUB_SIG__  =NULL)
    cnt = 1L
    for (i in seq_along(names(fixed_arg))) {
      if (names(fixed_arg)[i] == 'H')
        tosub_f[['SUB_H__']]     = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(H)) stop('The argument `H` must be numeric')
            mode(H)='double';
            H
          }
        }, list(j=cnt))
      else if (names(fixed_arg)[i] == 'theta')
        tosub_f[['SUB_THETA__']] = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(theta)) stop('The argument `theta` must be numeric')
            mode(theta)='double';
            theta
          }
        }, list(j=cnt))
      else if (names(fixed_arg)[i] == 'Sig')
        tosub_f[['SUB_SIG__']]   = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(Sig)) stop('The argument `Sig` must be numeric')
            mode(Sig)='double';
            Sig
          }
        }, list(j=cnt))
      else stop('Error building fixed-parameter restriction functions')
      cnt = cnt+1L
    }
    tosub[['BUILD_FIXED__']]        = substitute({
      fixedpart = list()
      SUB_H__
      SUB_THETA__
      SUB_SIG__
    }, tosub_f)
    tosub[['FIXEDPART__']]          = quote(fixedpart)
  }
  body = substituteDirect(body, tosub)
  substitute(
    `function`(A, B),
    list(A=as.pairlist(outerargs), B=body))
}


mk_fixed_arg = function (pat) {
  stopifnot(names(pat)[1] == 'M')
  stopifnot(names(pat)[2] == 'V')
  stopifnot(names(pat)[3] == 'L')
  A = alist()
  if (pat[[1]]=='f')  A = c(A, alist(H=))
  if (pat[[2]]=='f')  A = c(A, alist(theta=))
  if (pat[[3]]=='f')  A = c(A, alist(Sig=))
  return(A)
}


build_hess = function (pat) {
  outerargs = alist(hessfn=)
  body = quote({
    hessfn;
    JAC_GUARD_EVAL__;
    BUILD_FIXED__;
    f= function (par, ...) {
      mode(par) = 'double';
      k = npar2k(length(par), A__, B__, C__, D__)
      par_orig = .Call(Rparamrestrict, CMD__, par, k, FIXEDPART__);
      Hes = hessfn(par_orig, ...);
      MAKE_JACTHIS__;
      MAKE_JACLOWER__;
      list(V   = .Call(Rposthessrestrict, CMD__, par, Hes[['V']], k,
                       RJACLOWER__, RJLOWEROFFSET_V__,   RJACTHIS__, RJTHISOFFSET_R_V__, RJTHISOFFSET_C__),
           w   = .Call(Rposthessrestrict, CMD__, par, Hes[['w']], k,
                       RJACLOWER__, RJLOWEROFFSET_W__,   RJACTHIS__, RJTHISOFFSET_R_W__, RJTHISOFFSET_C__),
           Phi = .Call(Rposthessrestrict, CMD__, par, Hes[['Phi']], k,
                       RJACLOWER__, RJLOWEROFFSET_PHI__, RJACTHIS__, RJTHISOFFSET_R_PHI__, RJTHISOFFSET_C__))
    }
    attr(f, 'srcref') = NULL
    f
  })
  cmd = mkcmd(pat)
  to_sub = list(
    BUILD_FIXED__          = quote(NULL),
    FIXEDPART__            = quote(NULL),
    JAC_GUARD_EVAL__       = quote(NULL),
    MAKE_JACTHIS__         = quote(NULL),
    MAKE_JACLOWER__        = quote(NULL),
    CMD__                  = cmd,
    RJACLOWER__            = quote(NULL),
    RJLOWEROFFSET_V__      = quote(NULL),
    RJLOWEROFFSET_W__      = quote(NULL),
    RJLOWEROFFSET_PHI__    = quote(NULL),
    RJACTHIS__             = quote(NULL),
    RJTHISOFFSET_R_V__     = quote(NULL),
    RJTHISOFFSET_R_W__     = quote(NULL),
    RJTHISOFFSET_R_PHI__   = quote(NULL),
    RJTHISOFFSET_C__       = quote(NULL)
  )
  ## Detect if there are any cholesky or log-cholesky. If yes, add jacfn as argument
  if (grepl('Ml', cmd, fixed = TRUE) || grepl('Mc', cmd, fixed = TRUE)) {
    outerargs = c(outerargs, alist(jacfn=))
    to_sub[['JAC_GUARD_EVAL__']]    = quote(jacfn)
    to_sub[['MAKE_JACLOWER__']]     = quote(
    {
      J = jacfn(par_orig, ...);
      ku = INFO__$mod$rawmod$dimtab[INFO__$node_id];
      kv = INFO__$mod$rawmod$dimtab[INFO__$parent_id];
    })
    to_sub[['RJACLOWER__']]         = quote(J)
    to_sub[['RJLOWEROFFSET_V__']]   = quote(ku*kv+ku)
    to_sub[['RJLOWEROFFSET_W__']]   = quote(ku*kv)
    to_sub[['RJLOWEROFFSET_PHI__']] = quote(0L)
  }
  if (grepl('Me', cmd, fixed = TRUE)) {
    to_sub[['MAKE_JACTHIS__']]     = quote(
    {
      Jthis = INFO__[['reparametrisation_jacobian']];
      gstart = INFO__$mod$gausssegments[INFO__$node_id,'start'];
      gend   = INFO__$mod$gausssegments[INFO__$node_id,'end'];
      pstart = INFO__$mod$parsegments[INFO__$parfn_id,'start'];
      phid = dim(Hes[['Phi']])[1];
      wd   = dim(Hes[['w']])[1];
    })
    to_sub[['RJACTHIS__']]           = quote(Jthis)
    to_sub[['RJTHISOFFSET_R_V__']]   = quote(gstart+phid+wd-1L)
    to_sub[['RJTHISOFFSET_R_W__']]   = quote(gstart+phid-1L)
    to_sub[['RJTHISOFFSET_R_PHI__']] = quote(gstart-1L)
    to_sub[['RJTHISOFFSET_C__']]     = quote(pstart-1L)
  }
  ## If there is fixed part, add to formal arguments, and add a list builder on
  ## the outer function.
  outerargs = c(outerargs, fixed_arg <- mk_fixed_arg(pat))
  if (0L != length(fixed_arg)) {
    tosub_f = alist(SUB_H__    =NULL,
                    SUB_THETA__=NULL,
                    SUB_SIG__  =NULL)
    cnt = 1L
    for (i in seq_along(names(fixed_arg))) {
      if (names(fixed_arg)[i] == 'H')
        tosub_f[['SUB_H__']]     = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(H)) stop('The argument `H` must be numeric')
            mode(H)='double';
            H
          }
        }, list(j=cnt))
      else if (names(fixed_arg)[i] == 'theta')
        tosub_f[['SUB_THETA__']] = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(theta)) stop('The argument `theta` must be numeric')
            mode(theta)='double';
            theta
          }
        }, list(j=cnt))
      else if (names(fixed_arg)[i] == 'Sig')
        tosub_f[['SUB_SIG__']]   = substitute({
          fixedpart[[j]] = {
            if (!is.numeric(Sig)) stop('The argument `Sig` must be numeric')
            mode(Sig)='double';
            Sig
          }
        }, list(j=cnt))
      else stop('Error building fixed-parameter restriction functions')
      cnt = cnt+1L
    }
    to_sub[['BUILD_FIXED__']]        = substitute({
      fixedpart = list()
      SUB_H__
      SUB_THETA__
      SUB_SIG__
    }, tosub_f)
    to_sub[['FIXEDPART__']]          = quote(fixedpart)
  }
  abcd = cmd2abcd(cmd)
  to_sub[['A__']] = abcd$a
  to_sub[['B__']] = abcd$b
  to_sub[['C__']] = abcd$c
  to_sub[['D__']] = abcd$d
  body = substituteDirect(body, to_sub)
  substitute(`function`(args, body), list(args=as.pairlist(outerargs), body=body))
}

build_nparams = function (pat) {
  cmd  = mkcmd(pat)
  abcd = cmd2abcd(cmd)
  substitute(function (k) { a*k*k+b*k+c*(k*(k+1L))%/%2L+d }, abcd)
}


#' @name parameter_restriction
#' @rdname parameter_restriction
#' @usage avail_restrictions
#' @export
avail_restrictions = list(nparams=character(0),
                          par    =character(0),
                          jac    =character(0),
                          hess   =character(0))

## Now build all the no-measurement-error-variance OU/Brownian restrictor functions.
for (m in avail_restrict_cmds[['M']]) {
  for (v in avail_restrict_cmds[['V']]) {
    for (l in avail_restrict_cmds[['L']]) {
      pat = list('M'=m,'V'=v,'L'=l)
      name_stub = get_name_stub(pat)
      if (is.null(name_stub)) next
      assign(name_stub[['nparams']],  eval(build_nparams(pat)))
      assign(name_stub[['par']],      eval(build_par(pat)))
      assign(name_stub[['jac']],      eval(build_jac(pat)))
      assign(name_stub[['hess']],     eval(build_hess(pat)))
      ## Change the enclosing environment of these function back to the package's environment.
      eval(substitute({environment(f) = pkg_env}, list(f = as.name(name_stub[['nparams']]))))
      eval(substitute({environment(f) = pkg_env}, list(f = as.name(name_stub[['par']]))))
      eval(substitute({environment(f) = pkg_env}, list(f = as.name(name_stub[['jac']]))))
      eval(substitute({environment(f) = pkg_env}, list(f = as.name(name_stub[['hess']]))))
      ## Fix print.function's behaviour.
      eval(substitute({attr(f, "srcref") = NULL}), list(f= as.name(name_stub[['nparams']])))
      eval(substitute({attr(f, "srcref") = NULL}), list(f= as.name(name_stub[['par']])))
      eval(substitute({attr(f, "srcref") = NULL}), list(f= as.name(name_stub[['jac']])))
      eval(substitute({attr(f, "srcref") = NULL}), list(f= as.name(name_stub[['hess']])))
      avail_restrictions$nparams = c(avail_restrictions$nparams, name_stub[['nparams']])
      avail_restrictions$par     = c(avail_restrictions$par,     name_stub[['par']])
      avail_restrictions$jac     = c(avail_restrictions$jac,     name_stub[['jac']])
      avail_restrictions$hess    = c(avail_restrictions$hess,    name_stub[['hess']])
    }
  }
}

## User convenience functions for getting the OU parameter restrictors that they want.
human_name_to_cmdchr = list(
  'symmetric' = 's',
  'logspd'    = 'l',
  'spd'       = 'c',
  'diag'      = 'd',
  'logdiag'   = 'e',
  'zero'      = '0'
)

#' Convenience function for constructing restricted/reparameterised OU parameterisation function.
#' 
#' \code{get_restricted_ou} is a convenience function for constructing restricted/reparameterised
#' OU parameterisation.
#' 
#' \code{get_restricted_ou} is intended to provide a more convenient way to construct the
#' restrictions functions, restricted Jacobian and Hessian, than the more flexible methods
#' described in \code{\link{parameter_restriction}}.
#'
#' If either one of \code{H}, \code{theta} is 'zero' but not both, the function stops with error.
#' This is because former is statistically not sensible, and the latter can be done by directly
#' passing a vector of zero to the \code{theta} argument.
#'
#' If lossmiss is \code{NULL}, the returned functions does not have capability to handle missing or
#' lost values.
#' 
#' @param  H        One of \code{NULL}, 'symmetric', 'logspd', 'spd', 'diag', 'logdiag', 'zero', or a
#'                  numerical vector specifying fixed parameters.
#' @param  theta    One of \code{NULL}, 'zero', or a numerical vector specifying fixed parameters.
#' @param  Sig      One of \code{NULL}, 'diag', or a numerical vector specifying fixed parameters.
#' @param  lossmiss One of \code{NULL}, 'zap', 'halt'.
#' @return        A list containing the following elements:
#'                \item{par}{A reparameterisation function conforming to the format required by the \code{parfns}
#'                           argument of \code{glinv}.}
#'                \item{jac}{A Jacobian function of the above reparameterisation function conforming to the format
#'                           required by the \code{parjacs} argument of \code{glinv}.}
#'                \item{hess}{A Hessian function of the above reparameterisation function conforming to the format
#'                            required by the \code{parhess} argument of \code{glinv}.}
#'                \item{nparams}{A function which accepts one integer argument, the total number of dimensions
#'                               of the multivariate traits, and returns the number of parameters of the restricted
#'                               model.}
#' @examples
#' ### --- STEP 1: Make an example tree
#' set.seed(0x5EEDL, kind='Super-Duper')
#' ntips = 200
#' k     = 2                 # No. of trait dimensions
#' tr    = ape::rtree(ntips) 
#' x0    = rnorm(k)
#' 
#' ### --- STEP 2: Make a model which has unrestricted H, fixed theta and diagonal Sigma_x'.
#' repar = get_restricted_ou(H=NULL, theta=c(3,1), Sig='diag', lossmiss=NULL)
#' mod   = glinv(tr, x0, X=NULL,
#'               pardims =repar$nparams(k),
#'               parfns  =repar$par,
#'               parjacs =repar$jac,
#'               parhess =repar$hess)
#' # Actually, to save typing, the following short-cut call is the same as the above:
#' # mod = glinv(tr, x0, X=NULL, repar=repar)
#' 
#' ### --- STEP 3: Set up parameters; H, theta, and sig_x needs to be concatenated
#' H     = matrix(c(1,0,0,-1), k)
#' theta = c(3,1)
#' sig   = matrix(c(0.25,0,0,0.25), k)
#' sig_x = t(chol(sig))
#' par_truth = c(H=H, sig_x=c(0.5,0.5))
#' 
#' ### --- STEP 4: Get a simulated data set to toy with
#' X = rglinv(mod, par=par_truth)
#' set_tips(mod, X)
#' 
#' ### --- STEP 5: Make an unrestricted model object to compare with the one
#' ### whose parameters are restricted.
#' mod_unrestricted = glinv(tr, x0, X,
#'                          pardims=nparams_ou(k),
#'                          parfns=oupar,
#'                          parjacs=oujac,
#'                          parhess=ouhess)
#' 
#' 
#' ### --- STEP 6: Confirm this is indeed the same as typing everything manually
#' ## Does the restricted model gives the same likelihood as the unrestricted? (Yes, it does.)
#' LIK   = lik(mod)(par_truth)
#' LIK_unrestricted = lik(mod_unrestricted)(c(H,theta,sig_x[lower.tri(sig_x, diag=TRUE)]))
#' print(LIK == LIK_unrestricted)
#' # [1] TRUE
#' ## We can as well type everything manually as follows. This mod_manual should be
#' ## the same as the mod object; just a different way of calling the glinv function.
#' mod_manual = glinv(tr, x0, X,
#'                    pardims  = nparams_ou_fixedtheta_diagSig(k),
#'                    parfns   = ou_fixedtheta_diagSig(oupar,   theta=c(3,1)),
#'                    parjacs  = dou_fixedtheta_diagSig(oujac,  theta=c(3,1)),
#'                    parhess  = hou_fixedtheta_diagSig(ouhess, theta=c(3,1)))
#' LIK_manual = lik(mod_manual)(par_truth)
#' print(LIK == LIK_manual) #It's really the same
#' # [1] TRUE
#' 
#' @export
get_restricted_ou = function (H=NULL, theta=NULL, Sig=NULL, lossmiss = 'halt') {
  pat = list(M='k', V='k', L='k')
  fixed_args = list()
  if (!is.null(H)) {
    if (is.character(H)) {
      s = human_name_to_cmdchr[[H]]
      if (is.null(s) || !(s %in% avail_restrict_cmds[[1]]))
        stop(sprintf('`%s` restriction for H is not supported', H))
      pat[[1]] = s
    } else if (is.numeric(H)) {
      fixed_args[['H']] = as.double(H)
      pat[[1]] = 'f'
    } else {
      stop('`H` must be either a string or a numeric vector to be fixed.')
    }
  } else {
    pat[[1]] = 'k'
  }
  if (!is.null(theta)) {
    if (is.character(theta)) {
      s = human_name_to_cmdchr[[theta]]
      if (is.null(s) || !(s %in% avail_restrict_cmds[[2]]))
        stop(sprintf('`%s` restriction for theta is not supported', theta))
      pat[[2]] = s
    } else if (is.numeric(theta)) {
      fixed_args[['theta']] = as.double(theta)
      pat[[2]] = 'f'
    } else {
      stop('`theta` must be either a string or a numeric vector to be fixed.')
    }
  } else {
    pat[[2]] = 'k'
  }
  if (!is.null(Sig)) {
    if (is.character(Sig)) {
      s = human_name_to_cmdchr[[Sig]]
      if (is.null(s) || !(s %in% avail_restrict_cmds[[3]]))
        stop(sprintf('`%s` restriction for Sig is not supported', Sig))
      pat[[3]] = s
    } else if (is.numeric(Sig)) {
      fixed_args[['Sig']] = as.double(Sig)
      pat[[3]] = 'f'
    } else {
      stop('`Sig` must be either a string or a numeric vector to be fixed.')
    }
  } else {
      pat[[3]] = 'k'
  }
  ## If any of H or theta is zero but not both (which is Brownian motion) then throw an error.
  if ((pat[[1]] == '0' && pat[[2]] != '0') || (pat[[1]] != '0' && pat[[2]] == '0'))
    stop('zero H but non-zero theta, or non-zero H but zero theta is not supported. The former does not make statistical sense and the latter can be done by using fixed theta instead of zero theta.')
  if (is.null(lossmiss)) {
    ouparfn  = oupar
    oujacfn  = oujac
    ouhessfn = ouhess
  } else if (lossmiss == 'halt') {
    ouparfn  = ou_haltlost(oupar)
    oujacfn  = dou_haltlost(oujac)
    ouhessfn = hou_haltlost(ouhess)
  } else if (lossmiss == 'zap') {
    ouparfn  = ou_zaplost(oupar)
    oujacfn  = dou_zaplost(oujac)
    ouhessfn = hou_zaplost(ouhess)
  } else {
    stop('`lossmiss` is invalid')
  }
  if (pat[[1]] == 'k' && pat[[2]] == 'k' && pat[[3]] == 'k')
    return(list(par=oupar, jac=oujac, hess=ouhess, nparams=nparams_ou))

  cmd = mkcmd(pat)
  namestub = get_name_stub(pat)
  
  ## Now call the function names in namestub with the correct arguments.
  par_re     = get(namestub[['par']])
  jac_re     = get(namestub[['jac']])
  hess_re    = get(namestub[['hess']])
  nparams    = get(namestub[['nparams']])
  
  if (all(sapply(fixed_args, is.null)))
    fixed_args = list()
  if (grepl('Ml', cmd, fixed = TRUE) || grepl('Mc', cmd, fixed = TRUE)) {
    ouparfn  = do.call(par_re,  c(list(parfn=ouparfn),                  fixed_args))
    ouhessfn = do.call(hess_re, c(list(hessfn=ouhessfn, jacfn=oujacfn), fixed_args))
    oujacfn  = do.call(jac_re,  c(list(jacfn=oujacfn),                  fixed_args))
  } else {
    ouparfn  = do.call(par_re,  c(list(parfn=ouparfn),   fixed_args))
    ouhessfn = do.call(hess_re, c(list(hessfn=ouhessfn), fixed_args))
    oujacfn  = do.call(jac_re,  c(list(jacfn=oujacfn),   fixed_args))
  }
  list(par=ouparfn, jac=oujacfn, hess=ouhessfn, nparams=nparams)
}

rm('mk_fixed_arg')
rm('m')
rm('v')
rm('l')
rm('pat')
rm('name_stub')
rm('pkg_env')
rm('cmd2abcd')
rm('build_par')
rm('build_jac')
rm('build_hess')
rm('build_nparams')


## Now deal with the documentation and exporting all of those functions...

#' @rawNamespace exportPattern("^ou_.*$")
#' @rawNamespace exportPattern("^dou_.*$")
#' @rawNamespace exportPattern("^hou_.*$")
#' @rawNamespace exportPattern("^brn.*$")
#' @rawNamespace exportPattern("^dbrn.*$")
#' @rawNamespace exportPattern("^hbrn.*$")
#' @rawNamespace exportPattern("^nparams_ou_.*$")
#' @rawNamespace exportPattern("^nparams_brn.*$")
NULL

#' Restrict the parameters space of OU and Brownian motion models.
#'
#' \code{ou_diagH}, \code{ou_diagH_fixedtheta_diagSig}, etc., restricts the OU model's
#' parameters. For example, \code{ou_diagH} restricts the drift \eqn{H} to diagonal matrix,
#' and \code{ou_diagH_fixedtheta_diagSig} further restricts theta to be a constant and
#' \eqn{\Sigma_x'} to be diagonal. A Brownian motion model can be made by these restriction.
#' 
#' \subsection{How reparametrisation and restriction works}{
#' 
#' In the simplest form, without any restriction or reparametrisation, the user typically
#' needs to pass \code{oupar}, \code{oujac}, \code{ouhess}, all of which are simply
#' functions which maps from the OU parameters \eqn{(H,\theta,\Sigma_x')} to the Gaussian
#' paramters \eqn{(\Phi_i,w_i,V'_i)} for each node. For example:
#' \preformatted{
#'         mod.full = glinv(tree, x0, my_data,
#'                          parfns  = oupar,
#'                          pardims = nparams_ou(k),
#'                          parjacs = oujac,
#'                          parhess = ouhess)
#' }
#' If one would like to restrict \eqn{H} to only positively definite diagonal matrices,
#' then the call should become
#' \preformatted{
#'         mod.pddiag = glinv(tree, x0, my_data,
#'                            parfns  = ou_logdiagH(oupar),
#'                            pardims = nparams_ou_logdiagH(k),
#'                            parjacs = dou_logdiagH(oujac),
#'                            parhess = hou_logdiagH(ouhess))
#' }
#' Note that there is a naming convention that \code{ou_*} should be applied to `oupar`,
#' \code{dou_*} to `oujac`, and \code{hou_*} to `ouhess`. \code{d} stands for `derivative'
#' and \code{h} stands for `Hessian'.
#' 
#' In the above call, ou_logdiagH(oupar) accepts the \code{oupar} function as argument
#' and returns a new function. This new function behaves the same way as oupar itself,
#' except that it expects its first argument (which is the model parameters) to be of
#' lower dimension, only consisting of \eqn{(h,\theta,\Sigma_x')} where \eqn{h} is the
#' diagonal vector of \eqn{H}. The following example should be illustrative:
#' \preformatted{
#'         f = ou_logdiagH(oupar)
#'         par.full = list(H     = matrix(c(3,0,0,2),2,2), # diagonal matrix
#'                         theta = c(4,5),
#'                         sig_x = c(1,0.1,1))
#'         par.restricted = list(H     = log(diag(par.full$H)),
#'                               theta = par.full$theta,
#'                               sig_x = par.full$sig_x)
#'         print(all.equal(f(unlist(par.restricted),1,NULL,NULL),
#'                         oupar(unlist(par.full),1,NULL,NULL)))
#'         # [1] TRUE
#' }
#' }
#'
#' \subsection{Pre-defined restrictions}{
#' The following table summarises all the pre-defined \code{ou_*} functions. See \code{\link{oupar}}
#' for precise meaning of the \eqn{(H,\theta,\Sigma_x')} mentioned below.
#' \tabular{ll}{
#'   \strong{R function}   \tab \strong{Parameter Format after Restriction}\cr
#'   \code{brn*}           \tab \eqn{\Sigma_x'}. The Brownian motion. \eqn{H} and \eqn{\theta} are zero, thus missing.\cr
#'   \code{*_diagH_*}      \tab \eqn{(h,\theta,\Sigma_x')}, with \eqn{h=diag(H)}, and H is a diagonal matrix\cr
#'   \code{*_logdiagH_*}   \tab \eqn{(log(h),\theta,\Sigma_x')}, with \eqn{h=diag(H)}, and H is a diagonal matrix\cr
#'   \code{*_symH_*}       \tab \eqn{(L,\theta,\Sigma_x')}, with \eqn{L} being lower-triangular part of the symmetric matrix \eqn{H}\cr
#'   \code{*_spdH_*}       \tab \eqn{(L,\theta,\Sigma_x')}, with \eqn{L} being Cholesky factor of the S.P.D. matrix \eqn{H}\cr
#'   \code{*_logspdH_*}    \tab \eqn{(L',\theta,\Sigma_x')} where \eqn{L'} equals \eqn{L}, except that on the diagonals \eqn{L'_i} = \eqn{log L_i}\cr
#'   \code{*_fixedH_*}     \tab \eqn{(\theta,\Sigma_x')}. \eqn{H} is constant, hence missing\cr
#'   \code{*_fixedtheta_*} \tab \eqn{(H,\Sigma_x')}. \eqn{\theta} is constant, hence missing\cr
#'   \code{*_fixedSig_*}   \tab \eqn{(H,\theta)}. \eqn{\Sigma_x} is constant, hence missing\cr
#'   \code{*_diagSig_*}    \tab \eqn{(H,\theta,s)} where \eqn{s=diag(\Sigma_x'}, with \eqn{\Sigma_x'} being a diagonal matrix.
#' }
#' By Cholesky factor, we mean the only the non-zero part of the lower-triangular Cholesky factor. Restricting \eqn{\Sigma_x'} to a diagonal matrix
#' means that \eqn{\Sigma_x} is also diagonal; and the variance of the Brownian motion is \eqn{log(diag(\Sigma_x'))}. In other words, the diagonal
#' restriction is placed on \eqn{\Sigma_x'}, not \eqn{\Sigma_x}.
#' }
#' \subsection{Finding a list of these restriction functions}{
#' One can use \code{print(avail_restrictions)} to see a list of all of these restriction function names.
#' }
#' \subsection{Calling these restriction functions}{
#'  All \code{*ou_*} or \code{*brn*} functions accepts the same arguemnts as \code{ou_logdiagH},
#'  \code{dou_logdiagH}, \code{hou_logdiagH}, \code{nparams_ou_logdiagH} as shown in the Usage
#'  and Arguments section, except that:
#'  \enumerate{
#'     \item If the reparametrisation contains any Cholesky decomposition (in other words, the function name
#'           contains \code{spd} or \code{logspd}) then in the Hessian-level reparameterisation function
#'           (named \code{hou_*}) an extra argument \code{jacfn} is required.
#'     \item If the reparametrisation contains any fixed parameters, extra arguments \code{H}, \code{theta},
#'           or \code{Sig} are required, depending what is fixed.
#'  }
#'  For example, in the Usage section, \code{ou_logspdH_fixedtheta} takes an extra argument \code{theta} because
#'  of (2), and \code{hou_spdH_fixedSig} takes extra argument two extra arguments because of both (1) and (2) are
#'  true.
#' }
#' 
#' @name parameter_restriction
#' @usage brn_diagSig(parfn)
#' @usage ou_logdiagH(parfn)
#' @usage dou_logdiagH(jacfn)
#' @usage hou_logdiagH(hessfn)
#' @usage ou_logdiagH_diagSig(parfn)
#' @usage ou_logspdH_fixedtheta(parfn, theta)
#' @usage ou_spdH_fixedSig(parfn, Sig)
#' @usage ou_fixedH_diagSig(parfn, H)
#' @usage dou_logdiagH_diagSig(jacfn)
#' @usage dou_logspdH_fixedtheta(jacfn, theta)
#' @usage dou_spdH_fixedSig(jacfn, Sig)
#' @usage dou_fixedH_diagSig(jacfn, H)
#' @usage hou_logdiagH_diagSig(hessfn)
#' @usage hou_logspdH_fixedtheta(hessfn, jacfn, theta)
#' @usage hou_spdH_fixedSig(hessfn, jacfn, Sig)
#' @usage hou_spdH_fixedtheta_fixedSig(hessfn, jacfn, theta, Sig)
#' @usage hou_fixedH_diagSig(hessfn, H)
#' @usage nparams_ou_logdiagH(k)
#' @usage nparams_brn(k)
#' @usage nparams_ou_spdH_fixedSig(k)
#' @param parfn   A function that maps from the user-parametrisation to the underlying Gaussian parameters.
#'                Each of them returns a vector of concatenated \eqn{(\Phi, w, V')}, where \eqn{V'} is the lower triangular
#'                part of \eqn{V}, and accepts four arguments: a vector of parameters whose length is specified
#'                by the \code{pardims} argument to the \code{glinv_gauss} function, the branch length leading to the currently processing node, 
#'                a vector of factors with three levels indicating which dimensions are missing or lost in the mother of
#'                the current node, and a vector of factors with the same three levels indicating missingness of the current
#'                node.
#' @param jacfn   A function that accepts the same arguments as \code{parfn} and returns the Jacobian
#'                of \code{parfn}.
#' @param hessfn  A function that accepts the same arguments as \code{parfns} and returns a list of three 3D arrays,
#'                named \code{Phi}, \code{w}, \code{V} respectively inside the list. \code{((hessfn)(...))$Phi[m,i,j]}
#'                contains the cross second-order partial derivative of \eqn{\Phi_m} (here we treat the matrix
#'                \eqn{\Phi} as a column-major-flattened vector) with respect to the \eqn{i}-th and\eqn{j}-th parameters
#'                in the joint \eqn{(H,\theta,\Sigma_x')} vector, and
#'                \code{((hessfn)(...))$w[m,i,j]} and \code{((hessfn)(...))$V[m,i,j]}
#'                analogously contains second-order derivative with respect to \eqn{w_m} and \eqn{V'_m}.
#' @param H       A numerical vector containing the (flattened) fixed parameter \eqn{H}.
#' @param theta   A numerical vector containing the (flattened) fixed parameter \eqn{theta}.
#' @param Sig     A numerical vector containing the (flattened) fixed parameter \eqn{\Sigma_x'}.
#' @param k       An integer. The total number of dimensions of the multivariate traits.
#' @rdname parameter_restriction
#' @aliases brn brn_diagSig brn_fixedSig dbrn dbrn_diagSig dbrn_fixedSig dou_diagH dou_diagH_diagSig dou_diagH_fixedSig dou_diagH_fixedtheta dou_diagH_fixedtheta_diagSig dou_diagH_fixedtheta_fixedSig dou_diagSig dou_fixedH dou_fixedH_diagSig dou_fixedH_fixedSig dou_fixedH_fixedtheta dou_fixedH_fixedtheta_diagSig dou_fixedSig dou_fixedtheta dou_fixedtheta_diagSig dou_fixedtheta_fixedSig dou_logdiagH dou_logdiagH_diagSig dou_logdiagH_fixedSig dou_logdiagH_fixedtheta dou_logdiagH_fixedtheta_diagSig dou_logdiagH_fixedtheta_fixedSig dou_logspdH dou_logspdH_diagSig dou_logspdH_fixedSig dou_logspdH_fixedtheta dou_logspdH_fixedtheta_diagSig dou_logspdH_fixedtheta_fixedSig dou_spdH dou_spdH_diagSig dou_spdH_fixedSig dou_spdH_fixedtheta dou_spdH_fixedtheta_diagSig dou_spdH_fixedtheta_fixedSig dou_symH dou_symH_diagSig dou_symH_fixedSig dou_symH_fixedtheta dou_symH_fixedtheta_diagSig dou_symH_fixedtheta_fixedSig hbrn hbrn_diagSig hbrn_fixedSig hou_diagH hou_diagH_diagSig hou_diagH_fixedSig hou_diagH_fixedtheta hou_diagH_fixedtheta_diagSig hou_diagH_fixedtheta_fixedSig hou_diagSig hou_fixedH hou_fixedH_diagSig hou_fixedH_fixedSig hou_fixedH_fixedtheta hou_fixedH_fixedtheta_diagSig hou_fixedSig hou_fixedtheta hou_fixedtheta_diagSig hou_fixedtheta_fixedSig hou_logdiagH hou_logdiagH_diagSig hou_logdiagH_fixedSig hou_logdiagH_fixedtheta hou_logdiagH_fixedtheta_diagSig hou_logdiagH_fixedtheta_fixedSig hou_logspdH hou_logspdH_diagSig hou_logspdH_fixedSig hou_logspdH_fixedtheta hou_logspdH_fixedtheta_diagSig hou_logspdH_fixedtheta_fixedSig hou_spdH hou_spdH_diagSig hou_spdH_fixedSig hou_spdH_fixedtheta hou_spdH_fixedtheta_diagSig hou_spdH_fixedtheta_fixedSig hou_symH hou_symH_diagSig hou_symH_fixedSig hou_symH_fixedtheta hou_symH_fixedtheta_diagSig hou_symH_fixedtheta_fixedSig nparams_brn nparams_brn_diagSig nparams_brn_fixedSig nparams_ou_diagH nparams_ou_diagH_diagSig nparams_ou_diagH_fixedSig nparams_ou_diagH_fixedtheta nparams_ou_diagH_fixedtheta_diagSig nparams_ou_diagH_fixedtheta_fixedSig nparams_ou_diagSig nparams_ou_fixedH nparams_ou_fixedH_diagSig nparams_ou_fixedH_fixedSig nparams_ou_fixedH_fixedtheta nparams_ou_fixedH_fixedtheta_diagSig nparams_ou_fixedSig nparams_ou_fixedtheta nparams_ou_fixedtheta_diagSig nparams_ou_fixedtheta_fixedSig nparams_ou_logdiagH nparams_ou_logdiagH_diagSig nparams_ou_logdiagH_fixedSig nparams_ou_logdiagH_fixedtheta nparams_ou_logdiagH_fixedtheta_diagSig nparams_ou_logdiagH_fixedtheta_fixedSig nparams_ou_logspdH nparams_ou_logspdH_diagSig nparams_ou_logspdH_fixedSig nparams_ou_logspdH_fixedtheta nparams_ou_logspdH_fixedtheta_diagSig nparams_ou_logspdH_fixedtheta_fixedSig nparams_ou_spdH nparams_ou_spdH_diagSig nparams_ou_spdH_fixedSig nparams_ou_spdH_fixedtheta nparams_ou_spdH_fixedtheta_diagSig nparams_ou_spdH_fixedtheta_fixedSig nparams_ou_symH nparams_ou_symH_diagSig nparams_ou_symH_fixedSig nparams_ou_symH_fixedtheta nparams_ou_symH_fixedtheta_diagSig nparams_ou_symH_fixedtheta_fixedSig ou_diagH ou_diagH_diagSig ou_diagH_fixedSig ou_diagH_fixedtheta ou_diagH_fixedtheta_diagSig ou_diagH_fixedtheta_fixedSig ou_diagSig ou_fixedH ou_fixedH_diagSig ou_fixedH_fixedSig ou_fixedH_fixedtheta ou_fixedH_fixedtheta_diagSig ou_fixedSig ou_fixedtheta ou_fixedtheta_diagSig ou_fixedtheta_fixedSig ou_logdiagH ou_logdiagH_diagSig ou_logdiagH_fixedSig ou_logdiagH_fixedtheta ou_logdiagH_fixedtheta_diagSig ou_logdiagH_fixedtheta_fixedSig ou_logspdH ou_logspdH_diagSig ou_logspdH_fixedSig ou_logspdH_fixedtheta ou_logspdH_fixedtheta_diagSig ou_logspdH_fixedtheta_fixedSig ou_spdH ou_spdH_diagSig ou_spdH_fixedSig ou_spdH_fixedtheta ou_spdH_fixedtheta_diagSig ou_spdH_fixedtheta_fixedSig ou_symH ou_symH_diagSig ou_symH_fixedSig ou_symH_fixedtheta ou_symH_fixedtheta_diagSig ou_symH_fixedtheta_fixedSig
NULL



