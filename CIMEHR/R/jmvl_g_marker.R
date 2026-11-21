.jmvl_g_marker_check<-function(data,vars){d<-as.data.frame(data);m<-setdiff(unique(vars),names(d));if(length(m))stop("Missing columns: ",paste(m,collapse=", "),call.=FALSE);if(anyNA(d[[vars[1L]]]))stop("Subject id cannot be missing.",call.=FALSE);d}
.jmvl_g_marker_inv<-function(A,tol=1e-8){z<-svd((as.matrix(A)+t(as.matrix(A)))/2);if(!length(z$d)||max(z$d)<=0)stop("Observed information is singular.",call.=FALSE);k<-z$d>tol*max(z$d);if(!all(k))stop("JMVL-G parameters are not locally identified (singular observed information).",call.=FALSE);z$v%*%(t(z$u)/z$d)}
.jmvl_g_marker_logsumexp<-function(x){a<-max(x);a+log(sum(exp(x-a)))}
.jmvl_g_marker_gh<-function(n){
  if(n<3L)stop("`gh_order` must be at least 3.",call.=FALSE)
  J<-matrix(0,n,n);off<-sqrt(seq_len(n-1L)/2);J[cbind(seq_len(n-1L),2:n)]<-off;J[cbind(2:n,seq_len(n-1L))]<-off
  e<-eigen(J,symmetric=TRUE);o<-order(e$values);x<-e$values[o];w<-sqrt(pi)*(e$vectors[1,o]^2)
  list(z=sqrt(2)*x,w=w/sqrt(pi))
}

.jmvl_g_marker_sym <- function(A) (A+t(A))/2
.jmvl_g_marker_pd_inverse <- function(A,tol=1e-8) {
  A <- .jmvl_g_marker_sym(A); ee<-eigen(A,symmetric=TRUE)
  if(any(!is.finite(ee$values)) || min(ee$values)<=tol*max(ee$values)) {
    dd<-diag(A)
    if(any(!is.finite(dd)) || any(dd<=0))
      stop('Observed information is not sufficiently positive definite; no certified Wald SE.')
    sc<-sqrt(dd);R<-A/outer(sc,sc);er<-eigen(R,symmetric=TRUE)
    if(any(!is.finite(er$values)) || min(er$values)<=tol*max(er$values))
      stop('Observed information is not sufficiently positive definite; no certified Wald SE.')
    return(tcrossprod(sweep(er$vectors,2,1/sqrt(er$values),'*'))/outer(sc,sc))
  }
  tcrossprod(sweep(ee$vectors,2,1/sqrt(ee$values),'*'))
}
.jmvl_g_marker_gradient <- function(fn,x,step=1e-5) {
  out<-numeric(length(x))
  for(j in seq_along(x)) {h<-step*max(1,abs(x[j]));xp<-xm<-x;xp[j]<-xp[j]+h;xm[j]<-xm[j]-h;out[j]<-(fn(xp)-fn(xm))/(2*h)}
  out
}
.jmvl_g_marker_subject_scores <- function(fn,x,step=1e-5) {
  out<-matrix(0,length(fn(x)),length(x))
  for(j in seq_along(x)) {h<-step*max(1,abs(x[j]));xp<-xm<-x;xp[j]<-xp[j]+h;xm[j]<-xm[j]-h;out[,j]<-(fn(xp)-fn(xm))/(2*h)}
  out
}
.jmvl_g_marker_hessian <- function(fn,x,step=2e-4) {
  p<-length(x);H<-matrix(0,p,p);f0<-fn(x);hs<-step*pmax(1,abs(x))
  for(j in seq_len(p)) {
    xp<-xm<-x;xp[j]<-xp[j]+hs[j];xm[j]<-xm[j]-hs[j]
    H[j,j]<-(fn(xp)-2*f0+fn(xm))/hs[j]^2
    if(j>1)for(k in seq_len(j-1)){
      pp<-pm<-mp<-mm<-x
      pp[c(j,k)]<-pp[c(j,k)]+hs[c(j,k)];mm[c(j,k)]<-mm[c(j,k)]-hs[c(j,k)]
      pm[j]<-pm[j]+hs[j];pm[k]<-pm[k]-hs[k];mp[j]<-mp[j]-hs[j];mp[k]<-mp[k]+hs[k]
      H[j,k]<-H[k,j]<-(fn(pp)-fn(pm)-fn(mp)+fn(mm))/(4*hs[j]*hs[k])
    }
  }; H
}
.jmvl_g_marker_integrate_subjects <- function(m,exposure,linear_rate,k,rsum,r2,
                                              su,sv,rho,se,order=31L,details=FALSE) {

  n<-length(m);c<-sv*rho/su;t2<-sv^2*(1-rho^2);e2<-se^2
  den<-e2+k*t2;kb<-k>0;yb<-numeric(n);yb[kb]<-rsum[kb]/k[kb]
  ssec<-pmax(r2-ifelse(kb,rsum^2/pmax(k,1),0),0)
  yconst<-numeric(n);yconst[kb]<--.5*(k[kb]*log(2*pi)+(k[kb]-1)*log(e2)+log(den[kb])+ssec[kb]/e2)
  A<-m+k*c*yb/den;B<-1/su^2+k*c^2/den;lambda<-exposure*exp(linear_rate)
  mode<-(A-lambda)/(B+lambda)
  for(it in seq_len(100L)) {
    mu<-lambda*exp(pmin(mode,700));st<-(A-B*mode-mu)/(B+mu);st<-pmax(-5,pmin(5,st));mode<-mode+st
    if(max(abs(st))<2e-12)break
  }
  if(it==100L)stop('Adaptive posterior mode did not converge.')
  s<-1/sqrt(B+lambda*exp(mode));gh<-.jmvl_g_marker_gh(order)
  z<-matrix(rep(gh$z,each=n),nrow=n);u<-mode+s*z
  joint<-m*(linear_rate+u)-exposure*exp(pmin(linear_rate+u,700))-.5*(u/su)^2-log(su)-.5*log(2*pi)+yconst-.5*k*(yb-c*u)^2/den
  terms<-sweep(joint+.5*z^2,2L,log(gh$w),'+');mx<-apply(terms,1L,max);ee<-exp(terms-mx);tot<-rowSums(ee)
  ll<-mx+log(tot)+log(s)+.5*log(2*pi)
  if(!details)return(ll)
  ww<-ee/tot
  list(loglik=ll,mean_u=rowSums(ww*u),second_u=rowSums(ww*u^2),mode=mode,scale=s)
}
.jmvl_g_marker_make_evaluator <- function(W,X,y,yi,m,exposure) {
  n<-nrow(W);p<-ncol(X);q<-ncol(W);k<-tabulate(yi,nbins=n)
  sums<-function(v){out<-numeric(n);zz<-rowsum(v,yi,reorder=FALSE);out[as.integer(rownames(zz))]<-zz[,1];out}
  unpack<-function(par)list(beta=par[seq_len(p)],gamma=par[p+seq_len(q)],su=exp(par[p+q+1L]),sv=exp(par[p+q+2L]),rho=tanh(par[p+q+3L]),se=exp(par[p+q+4L]))
  rules<-list();bb<-gg<-r<-lr<-NULL
  evaluate<-function(par,order=31L,details=FALSE) {
    if(any(!is.finite(par))||any(abs(par[(p+q+1L):(p+q+4L)])>20))return(rep(-1e100,n))
    a<-unpack(par)
    if(!identical(a$beta,bb,num.eq=FALSE)){r<<-y-drop(X%*%a$beta);bb<<-a$beta}
    if(!identical(a$gamma,gg,num.eq=FALSE)){lr<<-drop(W%*%a$gamma);gg<<-a$gamma}
    o<-as.character(order);if(is.null(gh<-rules[[o]])){gh<-tryCatch(.jmvl_g_marker_gh(order),error=function(e)NULL);if(!is.null(gh))rules[[o]]<<-gh<-list(z=gh$z,lw=log(gh$w))}
    ll<-if(!is.null(gh))jmvl_g_integrate_cpp(r,yi,m,exposure,lr,k,a$su,a$sv,a$rho,a$se,gh$z,gh$lw,details)
    if(is.null(ll))return(tryCatch(.jmvl_g_marker_integrate_subjects(m,exposure,drop(W%*%a$gamma),k,sums(r),sums(r^2),a$su,a$sv,a$rho,a$se,order,details),
                                   error=function(e){if(details)stop(e);rep(-1e100,n)}))
    ll
  };list(evaluate=evaluate,unpack=unpack,n=n,p=p,q=q)
}
.jmvl_g_marker_certify_fit <- function(ev,start,gh_order,optimizer_control) {
  n<-ev$n;p<-ev$p;ord<-max(21L,as.integer(gh_order));if(ord>71L)ord<-71L
  ctl<-modifyList(list(maxit=1500L,reltol=1e-11),optimizer_control)
  ctl$reltol<-min(ctl$reltol,1e-11)
  par<-start;history<-list();tol<-1e-7;score_tol<-1e-6
  repeat {
    fn<-function(x)-sum(ev$evaluate(x,ord));gr<-function(x).jmvl_g_marker_gradient(fn,x)
    op<-optim(par,fn,gr,method='BFGS',control=ctl)
    par<-op$par

    for(refine in seq_len(6L)){
      gg<-gr(par);if(max(abs(gg)*pmax(1,abs(par)))/n<=1e-8)break
      HH<-.jmvl_g_marker_hessian(fn,par)
      eig<-eigen(HH,symmetric=TRUE,only.values=TRUE)$values
      if(min(eig)<=0)break
      delta<-drop(tryCatch(solve(HH,gg),error=function(e).jmvl_g_marker_pd_inverse(HH)%*%gg));fac<-1;v0<-fn(par)
      while(fac>2^-15 && fn(par-fac*delta)>v0+1e-10)fac<-fac/2
      par<-par-fac*delta
    }
    chk<-min(81L,ord+10L);low<-ev$evaluate(par,ord,TRUE);high<-ev$evaluate(par,chk,TRUE)
    score_lo<-.jmvl_g_marker_subject_scores(function(x)ev$evaluate(x,ord),par)
    score_hi<-.jmvl_g_marker_subject_scores(function(x)ev$evaluate(x,chk),par)
    qe<-max(abs(low$loglik-high$loglik))
    mae<-max(abs(low$mean_u-high$mean_u),abs(low$second_u-high$second_u))
    me<-max(abs(low$mean_u-high$mean_u)/pmax(1,abs(high$mean_u)),abs(low$second_u-high$second_u)/pmax(1,abs(high$second_u)))
    ge<-max(abs(score_lo-score_hi))
    history[[length(history)+1L]]<-data.frame(stage='pre_refinement',order=ord,check_order=chk,loglik_max_error=qe,posterior_absolute_error=mae,posterior_relative_error=me,score_max_error=ge)
    if(qe<=tol && mae<=tol && ge<=score_tol)break
    if(chk==81L)stop('Adaptive quadrature did not meet likelihood/moment/score tolerances by 81 nodes.')
    ord<-chk
  }
  final_order<-chk;fn<-function(x)-sum(ev$evaluate(x,final_order));gr<-function(x).jmvl_g_marker_gradient(fn,x)
  for(refine in seq_len(4L)){
    gg<-gr(par);if(max(abs(gg)*pmax(1,abs(par)))/n<=1e-8)break
    HH<-.jmvl_g_marker_hessian(fn,par);delta<-drop(tryCatch(solve(HH,gg),error=function(e).jmvl_g_marker_pd_inverse(HH)%*%gg));fac<-1;v0<-fn(par)
    while(fac>2^-15&&fn(par-fac*delta)>v0+1e-10)fac<-fac/2
    par<-par-fac*delta
  }

  final_low<-ev$evaluate(par,ord,TRUE);final_high<-ev$evaluate(par,final_order,TRUE)
  final_qe<-max(abs(final_low$loglik-final_high$loglik))
  final_mae<-max(abs(final_low$mean_u-final_high$mean_u),abs(final_low$second_u-final_high$second_u))
  final_me<-max(abs(final_low$mean_u-final_high$mean_u)/pmax(1,abs(final_high$mean_u)),
                abs(final_low$second_u-final_high$second_u)/pmax(1,abs(final_high$second_u)))
  final_ge<-max(abs(.jmvl_g_marker_subject_scores(function(x)ev$evaluate(x,ord),par)-
                      .jmvl_g_marker_subject_scores(function(x)ev$evaluate(x,final_order),par)))
  history[[length(history)+1L]]<-data.frame(stage='final_parameter',order=ord,check_order=final_order,
                                            loglik_max_error=final_qe,posterior_absolute_error=final_mae,posterior_relative_error=final_me,score_max_error=final_ge)
  if(final_qe>tol||final_mae>tol||final_ge>score_tol){
    if(final_order>=81L)stop('Final-parameter quadrature certification failed at 81 nodes.')
    return(.jmvl_g_marker_certify_fit(ev,par,final_order,optimizer_control))
  }
  H1<-.jmvl_g_marker_hessian(fn,par,2e-4);H2<-.jmvl_g_marker_hessian(fn,par,1e-4)
  V1<-.jmvl_g_marker_pd_inverse(H1);V2<-.jmvl_g_marker_pd_inverse(H2)
  slo<-sqrt(diag(V1));shi<-sqrt(diag(V2));h_err<-max(abs(H1-H2))/max(1,max(abs(H2)))
  se_step<-max(abs(slo[seq_len(p)]/shi[seq_len(p)]-1))
  Horder<-.jmvl_g_marker_hessian(function(x)-sum(ev$evaluate(x,ord)),par,1e-4)
  Vorder<-.jmvl_g_marker_pd_inverse(Horder);se_order<-max(abs(sqrt(diag(Vorder))[seq_len(p)]/shi[seq_len(p)]-1))
  score<-gr(par);scaled<-max(abs(score)*pmax(1,abs(par)))/n
  if(scaled>1e-6||h_err>1e-4||se_step>5e-4||se_order>5e-4)stop('Numerical certification failed for score/Hessian/SE stability.')
  list(par=par,value=fn(par),H=H2,V=V2,order=final_order,optimizer_code=op$convergence,
       diagnostics=list(quadrature_checks=do.call(rbind,history),quadrature_loglik_tol=tol,quadrature_score_tol=score_tol,
                        scaled_score=scaled,hessian_relative_step_error=h_err,hessian_symmetry_error=max(abs(H2-t(H2))),
                        hessian_min_eigenvalue=min(eigen(H2,symmetric=TRUE,only.values=TRUE)$values),
                        beta_se_relative_step_error=se_step,beta_se_relative_order_error=se_order,numerically_certified=TRUE,
                        numerical_certification_scope='integration, stationarity, local positive definite information, and numerical SE stability; not statistical model validity',
                        gh_requested=as.integer(gh_order),gh_initial_used=max(21L,min(71L,as.integer(gh_order)))))
}

.fit_jmvl_g_marker<-function(
    data,id="id",time="time",outcome="Y",obs_indicator="R",
    visit_covs=c("Z","X"),long_covs=c("Z","X"),random_covs=NULL,
    process=c("marker","visit"),entry_col=NULL,censor_col=NULL,
    include_time=TRUE,gh_order=9L,optimizer_control=list(maxit=1500L,reltol=1e-9)){
  process<-match.arg(process);d<-.jmvl_g_marker_check(data,c(id,time,outcome,obs_indicator,visit_covs,long_covs,entry_col,censor_col))
  ids<-unique(d[[id]]);si<-match(d[[id]],ids);first<-match(ids,d[[id]]);n<-length(ids)
  for(v in visit_covs)if(any(is.finite(d[[v]])&abs(d[[v]]-d[[v]][first][si])>1e-10,na.rm=TRUE))stop("Exponential gap-time implementation requires time-invariant process covariates: ",v,call.=FALSE)
  ft<-is.finite(d[[time]]);if(!any(ft))stop("No finite event times.",call.=FALSE)
  entry<-if(is.null(entry_col))rep(-Inf,n)else as.numeric(d[[entry_col]][first]);censor<-if(is.null(censor_col))rep(max(d[[time]][ft]),n)else as.numeric(d[[censor_col]][first])
  if(anyNA(entry)||any(entry==Inf)||any(!is.finite(censor)))stop("Entry and censoring times must be finite for every subject.",call.=FALSE)
  origin<-replace(entry,entry==-Inf,0)
  if(any(censor<origin))stop("Entry time cannot exceed censoring time (without entry_col the entry is the time origin 0).",call.=FALSE)
  tt<-d[[time]]
  if(is.null(entry_col)&&any(ft&tt<0))stop("Visit times cannot be negative without entry_col (the exposure starts at the time origin 0).",call.=FALSE)
  inwin<-ft&tt>entry[si]&tt<=censor[si]
  marker<-ft & !is.na(d[[obs_indicator]]) & d[[obs_indicator]]==1;event<-(if(process=="marker")marker else ft)&inwin
  m<-tabulate(si[event],nbins=n);exposure<-censor-origin
  oy<-which(marker&inwin&is.finite(d[[outcome]]))
  if(!length(oy))stop("No observed longitudinal outcomes.",call.=FALSE)
  Wmf<-stats::model.frame(stats::reformulate(visit_covs),d[first,,drop=FALSE],na.action=stats::na.fail)
  W<-stats::model.matrix(stats::terms(Wmf),Wmf);q<-ncol(W)
  rhs<-unique(c(long_covs,if(include_time)time));Ymf<-stats::model.frame(stats::reformulate(rhs,response=outcome),d[oy,,drop=FALSE],na.action=stats::na.fail)
  X<-stats::model.matrix(stats::terms(Ymf),Ymf);y<-stats::model.response(Ymf);yi<-si[oy];p<-ncol(X)
  yrows<-split(seq_along(y),factor(yi,levels=seq_len(n)))
  pos<-exposure>0
  pg<-try(stats::glm.fit(W[pos,,drop=FALSE],m[pos],family=stats::poisson(),offset=log(exposure[pos])),silent=TRUE);g0<-if(inherits(pg,"try-error")||any(!is.finite(pg$coefficients)))rep(0,q)else pg$coefficients
  bl<-stats::lm.fit(X,y);b0<-bl$coefficients;b0[!is.finite(b0)]<-0;se0<-sqrt(max(sum(bl$residuals^2)/max(1,bl$df.residual),1e-4));sy<-max(stats::sd(y),se0,0.1)
  ev<-.jmvl_g_marker_make_evaluator(W,X,y,yi,m,exposure)
  unpack<-ev$unpack
  start<-c(b0,g0,log(0.35*sy),log(0.35*sy),0,log(se0))
  cert<-.jmvl_g_marker_certify_fit(ev,start,gh_order,optimizer_control)
  op<-list(par=cert$par,value=cert$value,convergence=cert$optimizer_code)
  H<-cert$H;Vall<-cert$V;nll<-function(par)-sum(ev$evaluate(par,cert$order))
  a<-unpack(op$par);beta<-a$beta;names(beta)<-colnames(X)
  Vb<-Vall[seq_len(p),seq_len(p),drop=FALSE];dimnames(Vb)<-list(names(beta),names(beta))

  delta_diag<-c(rep(1,p+q),a$su,a$sv,1-a$rho^2,a$se)
  Vnatural<-sweep(sweep(Vall,1L,delta_diag,"*"),2L,delta_diag,"*")
  natural_names<-c(paste0("beta:",names(beta)),paste0("gamma:",colnames(W)),
                   "jmvlg:process_re_sd","jmvlg:outcome_re_sd",
                   "jmvlg:process_outcome_re_correlation","jmvlg:residual_sd")
  dimnames(Vnatural)<-list(natural_names,natural_names)
  grad<-numeric(length(op$par));for(j in seq_along(grad)){h<-1e-5*(1+abs(op$par[j]));aa<-bb<-op$par;aa[j]<-aa[j]+h;bb[j]<-bb[j]-h;grad[j]<-(nll(aa)-nll(bb))/(2*h)}
  ans<-list(method_key="jmvl_g_marker",method="JMVL-G exponential gap-time, correlated Gaussian RE (certified adaptive quadrature)",estimand="conditional Gaussian longitudinal fixed effect",
            coefficients=beta,vcov=Vb,se=setNames(sqrt(pmax(diag(Vb),0)),names(beta)),vcov_type="inverse observed marginal-likelihood information",
            nuisance=list(gamma=setNames(a$gamma,colnames(W)),sd_process_re=a$su,sd_outcome_re=a$sv,correlation=a$rho,residual_sd=a$se,logLik=-op$value),
            inference=list(vcov_all=Vnatural),converged=TRUE,
            diagnostics=c(list(ok=TRUE,process=process,n_subjects=n,n_visit_events=sum(inwin),n_marker_events=sum(marker&inwin),n_process_events=sum(event),n_outcomes=length(y),zero_event_subjects=sum(m==0),
                               gh_order=cert$order,quadrature="analytic conditional Gaussian integration plus one-dimensional patient-adaptive Gauss-Hermite",gradient_max=max(abs(grad)),information_condition=kappa(H),optimizer_code=op$convergence,
                               censor_source=if(is.null(censor_col))"common maximum observed time"else censor_col), cert$diagnostics),
            assumptions=c("Selected event gaps have constant exponential hazard conditional on the process random effect","Process and outcome random effects are bivariate Gaussian",
                          "Marker is the default process; choosing visit does not model additional marker-level MNAR missingness","Every subject, including zero-event subjects and empty windows (entry = censoring), contributes exposure over its risk window (entry, censoring]; a visit at the entry time is not an event or outcome row"),call=match.call())
  class(ans)<-c("jmvl_g_marker");ans
}

JMVL_G <- function(data,process=c('marker','visit'),...) {
  process<-match.arg(process);fit_method(method=if(process=='visit')'jmvl_g_visit' else 'jmvl_g_marker',data=data,...)
}
