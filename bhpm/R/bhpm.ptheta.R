# bhpm.BB.ptheta
# Model bhpm.BB
# R. Carragher
# Date: 29/06/2018

Id <- "$Id: bhpm.ptheta.R,v 1.6 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.ptheta <- function(raw)
{
	if (is.null(raw)) {
		message("NULL raw data");
		return(NULL)
	}

	model = attr(raw, "model")

	if (is.null(model)) {
		message("Missing model attribute");
		return(NULL)
	}

	summ = bhpm.cluster.ptheta(raw)

	return(summ)
}
