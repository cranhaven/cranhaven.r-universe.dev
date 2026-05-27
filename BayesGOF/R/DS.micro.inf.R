DS.micro.inf <-
function(DS.GF.obj, y.0, n.0 = NULL, e.0 = NULL){
	fam = DS.GF.obj$fam
	switch(fam,
		"Normal" = {
			DS.micro.inf.nnu(DS.GF.obj, y.0 = y.0, se.0 = n.0)
		 },
		 "Binomial" = {
			DS.micro.inf.bbu(DS.GF.obj, y.0 = y.0, n.0 = n.0)
		 },
		 "Poisson" = {
			if(is.null(e.0) == TRUE){
				DS.micro.inf.pgu(DS.GF.obj, y.0 = y.0)
				} else {
				DS.micro.inf.pge(DS.GF.obj, y.0 = y.0, e.0 = e.0)
				}
			 }
		)
	}
