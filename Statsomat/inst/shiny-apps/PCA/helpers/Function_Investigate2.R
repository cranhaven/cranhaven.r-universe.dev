# Adapted from FactoInvestigate
Investigate2 <-
  function(res, file = "Investigate.Rmd", document = c("html_document"), Iselec = "contrib", Vselec = "cos2", Rselec = "contrib", Cselec = "cos2", Mselec = "cos2", Icoef = 1, Vcoef = 1, Rcoef = 1, Ccoef = 1, Mcoef = 1, 
           ncp = NULL, time = "10s", nclust = -1, mmax = 10, nmax = 10, hab = NULL, ellipse = TRUE, display.HCPC = TRUE, out.selec = TRUE, remove.temp = TRUE, parallel = TRUE, cex = 0.7, openFile = TRUE, keepRmd = FALSE, 
           codeGraphInd = NULL, codeGraphVar=NULL, codeGraphCA = NULL, options = NULL, language="auto") {
    if(!is.character(file)) {return(warning("the parameter 'file' has to be a character chain giving the name of the .Rmd file to write in"))}
    
    old.encoding <- .Options$encoding
    options(encoding = "UTF-8")
    
    # VERIFICATIONS    if(!is.numeric(Iselec) & !is.character(Iselec)) {return(warning("the argument 'Iselec' should be a numeric or character vector"))}
    if(!is.numeric(Vselec) & !is.character(Vselec)) {return(warning("the argument 'Vselec' should be a numeric or character vector"))}
    if(!is.numeric(Rselec) & !is.character(Rselec)) {return(warning("the argument 'Rselec' should be a numeric or character vector"))}
    if(!is.numeric(Cselec) & !is.character(Cselec)) {return(warning("the argument 'Cselec' should be a numeric or character vector"))}
    if(!is.numeric(Mselec) & !is.character(Mselec)) {return(warning("the argument 'Mselec' should be a numeric or character vector"))}
    
    if(!is.numeric(Icoef)) {return(warning("the argument 'Icoef' must be numeric"))}
    if(!is.numeric(Vcoef)) {return(warning("the argument 'Vcoef' must be numeric"))}
    if(!is.numeric(Rcoef)) {return(warning("the argument 'Rcoef' must be numeric"))}
    if(!is.numeric(Ccoef)) {return(warning("the argument 'Ccoef' must be numeric"))}
    if(!is.numeric(Mcoef)) {return(warning("the argument 'Mcoef' must be numeric"))}
    
    if(Icoef < 0) {return(warning("the argument 'Icoef' must be positive"))}
    if(Vcoef < 0) {return(warning("the argument 'Vcoef' must be positive"))}
    if(Rcoef < 0) {return(warning("the argument 'Rcoef' must be positive"))}
    if(Ccoef < 0) {return(warning("the argument 'Ccoef' must be positive"))}
    if(Mcoef < 0) {return(warning("the argument 'Mcoef' must be positive"))}
    
    if(!is.character(time)) {return(warning("the argument 'time' has to be a character chain"))}
    if(length(grep("[sL]", time)) == 0) {return(warning("the argument 'time' must specifie the desired unity : add 's' for second or 'L' for the number of repetitions"))}
    
    if(!is.numeric(ncp) & !is.null(ncp)) {return(warning("the argument 'ncp' must be numeric"))}
    if(!is.null(ncp)) {if(ncp < 0) {return(warning("the argument 'ncp' must be positive"))}}
    if(!is.numeric(cex)) {return(warning("the argument 'cex' must be numeric"))} 
    if(!is.null(ncp)) {if(cex < 0) {return(warning("the argument 'cex' must be positive"))}}
    if(!is.numeric(nclust)) {return(warning("the argument 'nclust' must be numeric"))} 
    
    if(!is.numeric(hab) & !is.character(hab) & !is.null(hab)) {return(warning("the argument 'hab' should be the name or the index of the variable used to color the individuals"))}
    
    if(!is.logical(remove.temp)) {return(warning("the argument 'remove.temp' must be logical"))}
    if(!is.logical(ellipse)) {return(warning("the argument 'ellipse' must be logical"))}
    if(!is.logical(display.HCPC)) {return(warning("the argument 'display.HCPC' must be logical"))}
    if(!is.logical(out.selec)) {return(warning("the argument 'out.selec' must be logical"))}
    if(!is.logical(parallel)) {return(warning("the argument 'parallel' must be logical"))}
    
    # verification of the file extension (Rmarkdown only works with .Rmd !!)
    if(length(grep(".Rmd", file, ignore.case=TRUE)) == 0) {file = paste(file, ".Rmd", sep = "")}
    
    # INITIALISATION
    language <- tolower(language)
    if(!language %in% c("auto", "en", "fr")) {return(warning("the language must be 'auto', 'en' or 'fr'"))}
    if (language!="auto") {
      saveLANG <- Sys.getenv("LANG")
      Sys.setenv(LANG=language)
    }
    if(document == "Word" | document == "word" | document == "doc" | document == "docx" | document == "Word_document") 
    {document = "word_document"}
    if(document == "html" | document == "HTML" | document == "HTML_document") 
    {document ="html_document"}
    if(document == "pdf" | document == "PDF") 
    {document = "pdf_document"}
    if(document == "word_document") 
    {options = "r, echo = FALSE, fig.height = 3.5, fig.width = 5.5"}
    else 
    {options = "r, echo = FALSE, fig.align = 'center', fig.height = 3.5, fig.width = 5.5"}
    
    t = Sys.time()
    compteur = 0
    analyse = whichFacto(res)
    if(!analyse %in% c("PCA", "CA", "MCA", "HCPC", "HCPCshiny")) {return(warning("the parameter 'res' has to be an object of class 'PCA', 'CA', 'MCA' or 'HCPC'"))}
    
    param = getParam(res)
    cat("-- ", gettext("creation of the .Rmd file",domain="R-FactoInvestigate"), " (", gettext("time spent",domain="R-FactoInvestigate"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n\n", sep = "")
    createRmd2(res, analyse=analyse, file, document)
    writeRmd(paste0("library(FactoMineR)\nload('", as.character(getwd()),"/Workspace.RData')"), file = file, start = TRUE, stop = TRUE, options = "r, echo = FALSE")
    
    if (analyse %in% c("PCA","CA","MCA")){
      
      ## Code for outliers out
      
      cat("-- ", gettext("analysis of the inertia",domain="R-FactoInvestigate"), " (", gettext("time spent",domain="R-FactoInvestigate"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
      compteur = compteur + 1
      
      writeRmd("## ", compteur, ". ", gettext("Inertia distribution",domain="R-FactoInvestigate"), file = file, sep = "")
      
      ncp = inertiaDistrib(res, file = file, ncp = ncp, time = time, figure.title = paste("Figure", compteur), graph = FALSE, options = options)
      cat(ncp, gettext("component(s) carrying information",domain="R-FactoInvestigate"), ":", gettext("total inertia of",domain="R-FactoInvestigate"), paste(round(res$eig[ncp, 3], 1), "%", sep = ""), "\n\n")
      
      dim2plot = ncp
      # in case if ncp is odd
      if(ncp %% 2 != 0) {
        if(nrow(res$eig) > ncp) {
          dim2plot = ncp + 1
        }
      }
      
      
      if(param$ncp.mod < dim2plot) {
        switch(analyse,
               PCA = { res = PCA(param$data, quanti.sup = param$quanti.sup, quali.sup = param$quali.sup, ind.sup = param$ind.sup, graph = FALSE, 
                                 scale.unit = param$scale, row.w = param$row.w, col.w = param$col.w, ncp = dim2plot)
               },
               
               CA = { res = CA(param$data, quanti.sup = param$quanti.sup, quali.sup = param$quali.sup, row.sup = param$row.sup, col.sup = param$col.sup, 
                               graph = FALSE, row.w = param$row.w, ncp = dim2plot)
               },
               
               CaGalt = {},
               
               MCA = {res = MCA(param$data, quanti.sup = param$quanti.sup, quali.sup = param$quali.sup, ind.sup = param$ind.sup, graph = FALSE, 
                                row.w = param$row.w, ncp = dim2plot)
               },
               
               MFA = { res = MFA(param$data, group =param$group, type = param$type, ind.sup = param$ind.sup, graph = FALSE, 
                                 row.w = param$row.w, num.group.sup = param$num.group.sup, ncp = dim2plot)
               },
               
               # HMFA = {},
               
               # DMFA = {},
               
               # FAMD = {},
               
               # GPA = {},
               
               # HCPC = {}
        )
        
        param = getParam(res)
      }
      cat("-- ", gettext("components description",domain="R-FactoInvestigate"), " (", gettext("time spent",domain="R-FactoInvestigate"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
      if (!is.null(codeGraphInd) || !is.null(codeGraphVar) || !is.null(codeGraphCA)) endq <- 1
      else endq <- ceiling(ncp/2)
      for(q in 1:endq) {
        dim = c(2 * q - 1, 2 * q)
        writeRmd("\n- - -", file = file, end = "\n\n")
        
        compteur = compteur + 1
        if(ncp >= dim[2]) {
          cat(gettext("plane",domain="R-FactoInvestigate"), paste(dim[1], ":", dim[2], sep = ""), "\n")
          writeRmd("## ", compteur, ". ", gettext("Description of the plane",domain="R-FactoInvestigate"), " ", dim[1], ":", dim[2], file = file, sep = "")
        } else {
          cat(gettext("dim.",domain="R-FactoInvestigate"), dim[1], "\n")
          writeRmd("### ", compteur, ". ", gettext("Description of the dimension",domain="R-FactoInvestigate"), " ", dim[1], file = file, sep = "")
        }
        if(dim[1] == nrow(res$eig)) {dim = dim - 1}
        
        factoGraph(res, file = file, dim = dim, hab = hab, ellipse = ellipse, Iselec = Iselec, Vselec = Vselec, Rselec = Rselec, Cselec = Cselec, Mselec = Mselec, 
                   Icoef = Icoef, Vcoef = Vcoef, Rcoef = Rcoef, Ccoef = Ccoef, Mcoef = Mcoef, figure.title = paste("Figure", compteur), graph = FALSE, cex = 0.7, 
                   codeGraphInd = codeGraphInd, codeGraphVar = codeGraphVar ,codeGraphCA=codeGraphCA, options = options)
        # } else factoGraph(res, file = file, dim = dim, hab = hab, ellipse = ellipse, Iselec = Iselec, Vselec = Vselec, Rselec = Rselec, Cselec = Cselec, Mselec = Mselec, 
        # Icoef = Icoef, Vcoef = Vcoef, Rcoef = Rcoef, Ccoef = Ccoef, Mcoef = Mcoef, figure.title = paste("Figure", compteur), graph = FALSE, cex = 0.7, 
        # options = options)
        desc = dim
        if(dim[2] == nrow(res$eig)) {desc = dim[2]}
        if(dim[1] == ncp) {desc = dim[1]}
        description(res, file = file, dim = dim, desc = desc, Iselec = Iselec, Vselec = Vselec, Rselec = Rselec, Cselec = Cselec, Icoef = Icoef, Vcoef = Vcoef, Rcoef = Rcoef, Ccoef = Ccoef, nmax = nmax, mmax = mmax)
      }
      cat("\n")
      writeRmd("\n- - -", file = file, end = "\n\n")
      
      if(display.HCPC) {
        cat("-- ", gettext("classification",domain="R-FactoInvestigate"), " (", gettext("time spent",domain="R-FactoInvestigate"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
        if(sum(log(dimActive(res))^2) < 83.38) {
          compteur = compteur + 1
          writeRmd("## ", compteur,". Classification", end = "\n\n", file = file, sep = "")
          
          if(analyse %in% c("CA", "CaGalt")) {
            res.hcpc = classif(res, file = file, nclust = nclust, selec = Rselec, coef = Rcoef, nmax = nmax, mmax = mmax, figure.title = paste("Figure", compteur), graph = FALSE, options = options)
          } else {
            res.hcpc = classif(res, file = file, nclust = nclust, selec = Iselec, coef = Icoef, nmax = nmax, mmax = mmax, figure.title = paste("Figure", compteur), graph = FALSE, options = options)
          }
          cat(length(levels(res.hcpc$data.clust$clust)), gettext("clusters",domain="R-FactoInvestigate"), "\n\n")
        } else {
          compteur = compteur + 1
          writeRmd("## ", compteur,". Classification", end = "\n\n", file = file, sep = "")
          
          cat(gettext("dataset too heavy",domain="R-FactoInvestigate"), "\n\n")
          writeRmd(gettext("The dataset is too large to perform the classification",domain="R-FactoInvestigate"), end = ".\n", file = file)
          res.hcpc = NULL
        }
      } else {
        res.hcpc = NULL
      }
      cat("-- ", gettext("annexes writing",domain="R-FactoInvestigate"), " (", gettext("time spent",domain="R-FactoInvestigate"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n\n", sep = "")
      writeRmd("\n- - -", file = file, end = "\n\n")
      writeRmd("## Annexes", file = file)
      
      if(sum(log(dimActive(res)) ^ 2) < 83.38) {
        if(sum(unlist(sapply(dimdesc(res, axes = 1:ncp), lapply, nrow))) <= 50) {
          writeRmd("dimdesc(res, axes = 1:", ncp, ")", sep = "", file = file, start = TRUE, stop = TRUE, options = "r, comment = ''")
          compteur = compteur + 1
          writeRmd("**", paste("Figure", compteur), " - ", gettext("List of variables characterizing the dimensions of the analysis",domain="R-FactoInvestigate"), end = ".**\n\n", file = file, sep = "")
        }
        writeRmd("\n", file = file)
      }
      
      if(display.HCPC & !is.null(res.hcpc)) {
        if(sum(unlist(sapply(res.hcpc$desc.var, lapply, nrow))) <= 50) {
          writeRmd("res.hcpc$desc.var", sep = "", file = file, start = TRUE, stop = TRUE, options = "r, comment = ''")
          compteur = compteur + 1
          writeRmd("**", paste("Figure", compteur), " - ", gettext("List of variables characterizing the clusters of the classification",domain="R-FactoInvestigate"), end = ".**\n\n", file = file, sep = "")
        }
      }
      
      save(res, param, ncp, cex, res.hcpc, file = "Workspace.RData")
      rm(res, param, res.hcpc)
    }
    if (analyse %in% c("HCPC")){
      compteur = compteur + 1
      classif(res, file = file, nclust = nclust, figure.title = paste("Figure", compteur), graph = TRUE, options = options)
      save(res, file = "Workspace.RData")
    }
    writeRmd(file = file)
    script = scriptRmd(file)
    
    cat("-- ", gettext("saving data",domain="R-FactoInvestigate"), " (", gettext("time spent",domain="R-FactoInvestigate"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n\n", sep = "")
    
    cat("-- ", gettext("outputs compilation",domain="R-FactoInvestigate"), " (", gettext("time spent",domain="R-FactoInvestigate"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n\n", sep = "")
    options(encoding = old.encoding)
    if (openFile==TRUE) readRmd(file, document)
    if(remove.temp & (!keepRmd)) {
      file.remove("Workspace.RData")
      file.remove(file)
    }
    cat("-- ", gettext("task completed",domain="R-FactoInvestigate"), " (", gettext("time spent",domain="R-FactoInvestigate"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
    cat(gettext("This interpretation of the results was carried out automatically",domain="R-FactoInvestigate"),", \n",gettext("it cannot match the quality of a personal interpretation",domain="R-FactoInvestigate"),"\n",sep="")
    if (language!="auto") Sys.setenv(LANG=saveLANG)
    
  }