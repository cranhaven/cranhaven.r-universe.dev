`summary.elrm` <-
function(object,...)
{
    inferences = as.data.frame(cbind(round(as.numeric(object$coeffs),5),round(as.numeric(object$p.values),5),round(as.numeric(object$p.values.se),5),object$mc.size));
    
    results = data.frame(row.names=names(object$coeffs), inferences);
    names(results) = c("estimate","p-value","p-value_se","mc_size");
    
    message("\nCall:\n");
    message(object$call.history);
	message('\n');
    message("Results:\n");
    message(paste0(capture.output(results), collapse = "\n"));
    message('\n');
	message(object$ci.level,"% Confidence Intervals for Parameters\n",sep="");
    message(paste0(capture.output(object$coeffs.ci), collapse = "\n"));
}
