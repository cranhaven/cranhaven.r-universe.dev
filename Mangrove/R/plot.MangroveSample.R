plot.MangroveSample <-
function(x,...){
	plotSampledPrev(x$samples,obs_prev=x$Ncases,exp_prev=x$K*x$N,...)
	}

