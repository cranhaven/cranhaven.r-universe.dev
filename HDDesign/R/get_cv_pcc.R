get_cv_pcc <-
function(alpha, cv_data, p1) 
{
	sum(sapply(1:10, get_cv_pcc_single, alpha, cv_data, p1))/length(cv_data$y)
}
