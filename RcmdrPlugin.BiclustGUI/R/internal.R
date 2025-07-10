.onAttach <- function(libname, pkgname){
    if (!interactive()) return()
    putRcmdr("slider.env", new.env())    
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if (!pkgname %in% plugins) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        if("package:Rcmdr" %in% search()) {
            if(!getRcmdr("autoRestart")) {
                closeCommander(ask=FALSE, ask.save=TRUE)
                Commander()
            }
        }
        else {
            Commander()
        }
    }
	
	
}


if(getRversion() >= "2.15.1"){
	globalVariables(c("x","matrixdata","method_class","annotation","pData","temp","RcmdrPlugin.BiclustGUI","top",
					"temp.env","clusterTab","plotdiagTab","button.command","discrcheck","discrquant","bincheck",
					"discr.checkVariable","bin.checkVariable","tab1","tab2","temp.correct","appendVariable",
					"appendFrame","sel.result","whichRadioVariable","whichRadioFrame","dimRadioVariable",
					"saveVariable","saveCheck","saveShow","temp.data","radioTransformVariable","dimRadioFrame",
					"quantileVariable","quantilesCheck","radioTransformFrame","temp.correct2",
					"radioTransform2Variable","quantile2Variable","quantilesCheck2","discr.quantVariable",
					"radioTransform2Frame","backgroundVariable","backgroundCheck","reorderVariable",
					"reorderCheck","zeroBCVariable","zeroBCCheck","temp.correct1","methodshow","temp.object",
					"biclustering.objects","biclustGUI_biclusteringsearchdata","radiotypeVariable",
					"radiodiscoVariable","global.variable.list","radiotypeFrame","radiodiscoFrame",
					"pushOutput","putRExcel",
					"radioType1Frame","radioType1Variable","radioType2Frame","radioType2Variable"))
	
}

#if(getRversion() >= "2.15.1"){
#	globalVariables(c("x","matrixdata","extractBic","isa.biclust","binarize","discretize",
#					"method_class","annotation","pData","temp","biclust","BCBimax",""))
#	
#}





