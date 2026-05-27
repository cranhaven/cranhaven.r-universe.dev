DS.macro.inf <-
function(DS.GF.obj, num.modes =1, method = c("mean","mode"),
						iters = 25, exposure = NULL){
	fam = DS.GF.obj$fam
	switch(fam,
		"Normal" = {
			DS.macro.inf.nnu(DS.GF.obj, num.modes , iters , method, pred.type = "prior")
		 },
		 "Binomial" = {
			DS.macro.inf.bbu(DS.GF.obj, num.modes , iters , method, pred.type = "prior")
		 },
		 "Poisson" = {
			if(is.null(exposure) == TRUE){
				DS.macro.inf.pgu(DS.GF.obj, num.modes , iters , method, pred.type = "prior")
				} else {
				DS.macro.inf.pge(DS.GF.obj, num.modes, iters, exposure = exposure, method, pred.type = "prior")
				}
			 }
		)
	}