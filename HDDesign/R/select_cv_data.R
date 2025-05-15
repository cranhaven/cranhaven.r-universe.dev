select_cv_data <-
function(cv_data, TFidx)
{
	data=list()
	data$x=cv_data$x[TFidx,]
	data$y=cv_data$y[TFidx]
	data
}
