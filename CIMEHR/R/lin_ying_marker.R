.lin_ying_marker_stable_risk <- function(log_weight, values, times, entry, censor, covariance = TRUE, plan = NULL) {
  values <- as.matrix(values); n <- nrow(values)
  stopifnot(length(log_weight) == n, length(entry) == n, length(censor) == n)
  if (anyNA(log_weight) || any(log_weight == Inf) || any(!is.finite(values)))
    stop("Non-finite risk-set inputs.", call. = FALSE)
  ly_risk_moments_cpp(log_weight, values, times, entry, censor, covariance, plan)
}

.lin_ying_marker_stable_interval <- function(times, log_weight, values, entry, censor) {
  values <- as.matrix(values)
  if (anyNA(log_weight)||any(log_weight==Inf)||any(!is.finite(values)))
    stop("Non-finite interval-sum inputs.",call.=FALSE)
  ly_stable_interval_cpp(times, log_weight, values, entry, censor)
}

.lin_ying_marker_stable_compensator <- function(eta,V,tu,dn,logs0,vbar,entry,censor){
  z<-.lin_ying_marker_stable_interval(tu,log(dn)-logs0,vbar,entry,censor)
  scale<-exp(eta+z$log_sum)
  (V-z$mean)*scale
}

.lin_ying_marker_check<-function(data,vars){d<-as.data.frame(data);m<-setdiff(unique(vars),names(d));if(length(m))stop("Missing columns: ",paste(m,collapse=", "),call.=FALSE);if(anyNA(d[[vars[1L]]]))stop("Subject id cannot be missing.",call.=FALSE);d}
.lin_ying_marker_inv<-function(A,tol=sqrt(.Machine$double.eps)){z<-svd(as.matrix(A));if(!length(z$d)||max(z$d)==0)stop("Singular Lin--Ying equation.",call.=FALSE);k<-z$d>tol*max(z$d);if(!all(k))stop("Lin--Ying beta is not identified by the centered design.",call.=FALSE);z$v%*%(t(z$u)/z$d)}
.lin_ying_marker_jac<-function(fun,x){J<-matrix(NA_real_,length(fun(x)),length(x));for(j in seq_along(x)){h<-1e-5*(1+abs(x[j]));a<-b<-x;a[j]<-a[j]+h;b[j]<-b[j]-h;J[,j]<-(fun(a)-fun(b))/(2*h)};J}
.lin_ying_marker_gamma <- function(g, V, ei, tu, em, dn, entry, censor, individual = FALSE, information = FALSE, plan = NULL) {

  eta<-drop(V%*%g)
  risk<-.lin_ying_marker_stable_risk(eta,V,tu,entry,censor,information,plan)
  vb<-risk$mean;ls0<-risk$log_sum;tr<-ly_gamma_terms_cpp(V,ei,em,vb,dn,risk$covariance,ls0,eta)
  if(!tr$finite)stop("Empty risk set at an event time.",call.=FALSE)
  U<-tr$U
  if(individual)U<-U-.lin_ying_marker_stable_compensator(eta,V,tu,dn,ls0,vb,entry,censor)
  list(objective=tr$objective,score=colSums(U),individual=U,
       information=tr$information,event_times=tu,log_sum=ls0,risk_mean=vb)
}
.lin_ying_marker_segment_loop<-function(entry,censor,et,y,tu,by_subject){
  seg_l<-seg_r<-seg_y<-numeric();seg_subject<-integer();s<-0L;tl<-c(-Inf,tu)
  for(i in seq_along(entry)){
    lo<-findInterval(entry[i],tu)+1L;hi<-findInterval(censor[i],tu)
    rr<-by_subject[[i]];if(hi<lo||!length(rr))next
    rr<-rr[order(et[rr])];st<-et[rr]
    mid<-if(length(st)>1L)(st[-1L]+st[-length(st)])/2 else numeric()
    starts<-c(lo,findInterval(mid,tu)+1L);ends<-c(findInterval(mid,tu),hi)
    for(j in seq_along(rr)){
      a<-max(lo,starts[j]);b<-min(hi,ends[j])
      if(b<a)next
      s<-s+1L;seg_l[s]<-tl[a];seg_r[s]<-tu[b];seg_y[s]<-y[rr[j]];seg_subject[s]<-i
    }
  }
  list(subject=seg_subject,left=seg_l,right=seg_r,value=seg_y)
}
.lin_ying_marker_segments<-function(entry,censor,et,ei,y,tu){loop<-function()list(loop=list(et=et,y=y,by_subject=split(seq_along(y),factor(ei,levels=seq_along(entry)))))
if(!is.null(oldClass(et))||!is.null(oldClass(y))||!is.null(oldClass(entry))||!is.null(oldClass(censor))||!(is.double(et)||is.integer(et))||!(is.double(y)||is.integer(y)||is.logical(y)))return(loop())
o<-order(ei,et);s<-ei[o];st<-et[o];m<-length(o)
nx<-s[-1L]==s[-m];w<-which(nx);sm<-as.double(st[w+1L])+st[w]
if(is.integer(st)&&any(abs(sm)>.Machine$integer.max))return(loop())
mid<-integer(m-1L);mid[w]<-findInterval(sm/2,tu)
lo<-findInterval(entry[s],tu)+1L;hi<-findInterval(censor[s],tu)
a<-pmax(lo,c(lo[1L],ifelse(nx,mid+1L,lo[-1L])));b<-pmin(hi,c(ifelse(nx,mid,hi[-m]),hi[m]))
k<-hi>=lo&b>=a
left<-c(-Inf,tu)[a[k]];right<-as.double(tu[b[k]])
list(subject=s[k],left=left,right=right,value=as.double(y[o][k]),plan=ly_risk_plan_cpp(tu,left,right))}
.lin_ying_marker_profile <- function(g,V,Xsub,entry,censor,tu,h,xplan){
  eta<-drop(V%*%g)

  xrisk<-.lin_ying_marker_stable_risk(eta,Xsub,tu,entry,censor,FALSE,xplan)
  if(!is.null(h$loop))h<-.lin_ying_marker_segment_loop(entry,censor,h$loop$et,h$loop$y,tu,h$loop$by_subject)
  if(!length(h$subject))stop("Nearest-history nuisance mean is undefined at an event time.",call.=FALSE)
  seg_log<-unname(eta)[h$subject]
  yrisk<-.lin_ying_marker_stable_risk(seg_log,matrix(h$value,ncol=1L),tu,h$left,h$right,FALSE,h$plan)
  if(any(!is.finite(xrisk$log_sum))||any(!is.finite(yrisk$log_sum)))
    stop("Nearest-history nuisance mean is undefined at an event time.",call.=FALSE)
  list(ybar=drop(yrisk$mean),xbar=xrisk$mean,
       x_log_sum=xrisk$log_sum,y_log_sum=yrisk$log_sum,
       nearest_history=list(subject=h$subject,left=h$left,right=h$right,
                            log_weight=seg_log,value=h$value))
}

# Case-weight derivative of the outcome equation at fixed gamma and beta.
.lin_ying_marker_outcome_case_derivative <- function(beta,Xsub,eta,entry,censor,y,oi,tm,tu,profile) {
  Xsub<-as.matrix(Xsub);n<-nrow(Xsub);p<-ncol(Xsub);K<-length(tu)
  centered<-Xsub[oi,,drop=FALSE]-profile$xbar[tm,,drop=FALSE]
  residual<-y-profile$ybar[tm]-drop(centered%*%beta)
  direct<-cimehr_group_sum(centered*residual,oi,n)
  by_time<-cimehr_group_sum(cbind(residual,centered),tm,K)
  residual_sum<-by_time[,1L]
  centered_sum<-by_time[,-1L,drop=FALSE]

  # d xbar_i(t) = exp(eta_i) I_i(t) (X_i - xbar(t)) / S_x(t).
  xb<-drop(profile$xbar%*%beta)
  xvalues<-cbind(residual_sum,centered_sum,
                 profile$xbar*residual_sum-centered_sum*xb)
  xs<-.lin_ying_marker_stable_interval(tu,-profile$x_log_sum,xvalues,entry,censor)
  xcase<-(-Xsub*xs$mean[,1L]+
            xs$mean[,1L+seq_len(p),drop=FALSE]*drop(Xsub%*%beta)+
            xs$mean[,1L+p+seq_len(p),drop=FALSE])*exp(eta+xs$log_sum)

  # A patient's nearest-history outcome is constant on each stored segment.
  # d ybar_i(t) = exp(eta_i) I_segment(t) (Y_segment-ybar(t)) / S_y(t).
  h<-profile$nearest_history
  ys<-.lin_ying_marker_stable_interval(tu,-profile$y_log_sum,
                                       cbind(centered_sum,centered_sum*profile$ybar),h$left,h$right)
  segment_case<-(ys$mean[,p+seq_len(p),drop=FALSE]-
                   ys$mean[,seq_len(p),drop=FALSE]*h$value)*
    exp(h$log_weight+ys$log_sum)
  ycase<-cimehr_group_sum(segment_case,h$subject,n)
  individual<-direct+xcase+ycase
  if(any(!is.finite(individual)))
    stop("Non-finite Lin--Ying patient case derivative.",call.=FALSE)
  list(individual=individual,direct=direct,x_profile=xcase,y_profile=ycase)
}

.fit_lin_ying_marker<-function(
    data,id="id",time="time",outcome="Y",obs_indicator="R",
    visit_covs=c("Z","X"),long_covs=c("Z","X"),random_covs=NULL,
    process=c("marker","visit"),entry_col=NULL,censor_col=NULL,
    optimizer_control=list(maxit=1000L),inference="stacked",return_subject_scores=FALSE){
  inference<-match.arg(inference,"stacked")
  process<-match.arg(process);d<-.lin_ying_marker_check(data,c(id,time,outcome,obs_indicator,visit_covs,long_covs,entry_col,censor_col))
  ids<-unique(d[[id]]);si<-match(d[[id]],ids);first<-match(ids,d[[id]]);n<-length(ids)
  V<-as.matrix(d[first,visit_covs,drop=FALSE]);Xsub<-as.matrix(d[first,long_covs,drop=FALSE]);storage.mode(V)<-storage.mode(Xsub)<-"double"
  if(any(!is.finite(V))||any(!is.finite(Xsub)))stop("Baseline process/outcome covariates must be finite.",call.=FALSE)
  for(v in unique(c(visit_covs,long_covs)))if(any(is.finite(d[[v]])&abs(d[[v]]-d[[v]][first][si])>1e-10,na.rm=TRUE))stop("Lin--Ying requires time-invariant covariates: ",v,call.=FALSE)
  ft<-is.finite(d[[time]]);if(!any(ft))stop("No visits.",call.=FALSE);entry<-if(is.null(entry_col))rep(-Inf,n)else as.numeric(d[[entry_col]][first]);censor<-if(is.null(censor_col))rep(max(d[[time]][ft]),n)else as.numeric(d[[censor_col]][first])
  if(anyNA(entry)||any(entry==Inf)||any(!is.finite(censor))||any(entry>censor))stop("Invalid entry/censor times: censoring must be finite and entry must be non-missing and not exceed censoring.",call.=FALSE)
  vt<-d[[time]]
  inw<-ft&vt>entry[si]&vt<=censor[si]
  marker<-inw&!is.na(d[[obs_indicator]])&d[[obs_indicator]]==1;ev<-if(process=="marker")marker else inw
  oy<-marker&is.finite(d[[outcome]])
  er<-which(ev);or<-which(oy);if(!length(er)||!length(or))stop("No selected events/outcomes.",call.=FALSE)
  ei<-si[er];eet<-d[[time]][er];oi<-si[or];ot<-d[[time]][or];y<-d[[outcome]][or]
  gtu<-sort(unique(eet));gem<-match(eet,gtu);gdn<-tabulate(gem,nbins=length(gtu));gp<-ly_risk_plan_cpp(gtu,entry,censor);.gl<-.gv<-NULL
  .gamma<-function(g,information=FALSE){if(!is.null(.gv)&&(!information||!is.null(.gv$information))&&identical(g,.gl,num.eq=FALSE))return(.gv);.gv<<-.lin_ying_marker_gamma(g,V,ei,gtu,gem,gdn,entry,censor,information=information,plan=gp);.gl<<-g;.gv}
  og<-local({.lin_ying_marker_gamma<-function(g,V,ei,et,entry,censor).gamma(g)
  stats::optim(rep(0,ncol(V)),function(g).lin_ying_marker_gamma(g,V,ei,eet,entry,censor)$objective,gr=function(g)-.lin_ying_marker_gamma(g,V,ei,eet,entry,censor)$score,method="BFGS",control=optimizer_control,hessian=TRUE)})

  .pg<-og$par
  for(.it in seq_len(25L)){
    .pp<-.gamma(.pg)
    if(max(abs(.pp$score))<=1e-9*(1+length(er)))break
    .H<-.gamma(.pg,TRUE)$information
    .st<-tryCatch(drop(solve(.H,.pp$score)),error=function(e)NULL);if(is.null(.st))break
    .f<-1;.acc<-FALSE
    while(.f>=2^-20){.c<-.pg+.f*.st;.cv<-.gamma(.c)$objective
    if(is.finite(.cv)&&.cv<=.pp$objective+1e-12*(1+abs(.pp$objective))){.pg<-.c;.acc<-TRUE;break};.f<-.f/2}
    if(!.acc)break
  }
  og$par<-.pg
  gs<-.gamma(og$par);if(max(abs(gs$score))>1e-5*(1+length(er)))stop("Rate nuisance did not converge.",call.=FALSE)
  tu<-sort(unique(ot));tm<-match(ot,tu);seg<-.lin_ying_marker_segments(entry,censor,ot,oi,y,tu);xp<-ly_risk_plan_cpp(tu,entry,censor)
  prof<-.lin_ying_marker_profile(og$par,V,Xsub,entry,censor,tu,seg,xp);XC<-Xsub[oi,,drop=FALSE]-prof$xbar[tm,,drop=FALSE];YC<-y-prof$ybar[tm]
  beta<-drop(.lin_ying_marker_inv(crossprod(XC))%*%crossprod(XC,YC));names(beta)<-colnames(Xsub)
  q<-length(og$par);p<-length(beta)
  gi<-.lin_ying_marker_gamma(og$par,V,ei,gtu,gem,gdn,entry,censor,TRUE,plan=gp)$individual
  scores<-function(par){g<-par[seq_len(q)];b<-par[q+seq_len(p)];at<-identical(unname(g),og$par,num.eq=FALSE);pr<-if(at)prof else .lin_ying_marker_profile(g,V,Xsub,entry,censor,tu,seg,xp);xx<-Xsub[oi,,drop=FALSE]-pr$xbar[tm,,drop=FALSE];rr<-y-pr$ybar[tm]-drop(xx%*%b);cbind(if(at)gi else .lin_ying_marker_gamma(g,V,ei,gtu,gem,gdn,entry,censor,TRUE,plan=gp)$individual,cimehr_group_sum(xx*rr,oi,n))}
  par<-c(og$par,beta)
  A<- -.lin_ying_marker_jac(function(z)colSums(scores(z)),par)
  outcome_case<-.lin_ying_marker_outcome_case_derivative(beta,Xsub,drop(V%*%og$par),
                                                         entry,censor,y,oi,tm,tu,prof)
  psi<-cbind(gi,outcome_case$individual)
  meat<-crossprod(psi);Ai<-.lin_ying_marker_inv(A);VV<-Ai%*%meat%*%t(Ai)
  joint_names<-c(paste0("gamma:",visit_covs),paste0("beta:",names(beta)))
  dimnames(A)<-dimnames(meat)<-dimnames(VV)<-list(joint_names,joint_names)
  ix<-q+seq_len(p);Vb<-VV[ix,ix,drop=FALSE];dimnames(Vb)<-list(names(beta),names(beta))
  Vg<-VV[seq_len(q),seq_len(q),drop=FALSE];dimnames(Vg)<-list(visit_covs,visit_covs)
  ans<-list(method_key="lin_ying_marker",method="Lin--Ying semiparametric nearest-history estimator",estimand="marginal longitudinal covariate effect with unspecified time trend",coefficients=beta,vcov=Vb,se=setNames(sqrt(pmax(diag(Vb),0)),names(beta)),
            vcov_type="case-derivative stacked patient sandwich (Breslow-profiled rate, empirical risk-set and nearest-history profile means, outcome)",nuisance=list(gamma=setNames(og$par,visit_covs),
                                                                                                                                                                      gamma_vcov=Vg,gamma_se=setNames(sqrt(pmax(diag(Vg),0)),visit_covs),baseline_rate=list(time=gs$event_times,jump=gdn*exp(-gs$log_sum)),event_time_mean=list(time=tu,ybar=prof$ybar,xbar=prof$xbar)),
            inference=list(bread=A,meat=meat,vcov_all=VV,coefficients_all=setNames(par,joint_names),
                           scope="Joint gamma-beta covariance with direct patient-case influence of both empirical profile means",
                           subject_scores=if(isTRUE(return_subject_scores)) {
                             dimnames(psi)<-list(as.character(ids),joint_names);psi
                           } else NULL),converged=TRUE,
            diagnostics=list(ok=TRUE,process=process,n_subjects=n,n_visit_events=sum(inw),n_marker_events=sum(marker,na.rm=TRUE),n_process_events=length(er),n_outcomes=length(or),zero_event_subjects=sum(tabulate(ei,nbins=n)==0),
                             censor_source=if(is.null(censor_col))"common maximum observed time"else censor_col,gamma_score_max=max(abs(gs$score)),centered_design_rank=qr(XC)$rank,stacked_condition=kappa(A),
                             stacked_score_sum_max=max(abs(colSums(psi))),
                             profile_case_correction_sum_max=max(abs(colSums(outcome_case$x_profile+outcome_case$y_profile))),
                             empirical_profile_uncertainty_included=TRUE,upstream_uncertainty_included=TRUE,
                             inference=inference),
            assumptions=c("The selected event process has a proportional-rate model","The marginal mean has an unrestricted smooth time trend and time-invariant covariate effects","Nearest-history smoothing is valid with adequate within-subject longitudinal support","All subjects remain in the appropriate risk sets"),call=match.call())
  class(ans)<-c("lin_ying_marker");ans
}

Lin_Ying <- function(data,process=c('marker','visit'),...) {
  process<-match.arg(process);fit_method(method=if(process=='visit')'lin_ying_visit' else 'lin_ying_marker',data=data,...)
}
