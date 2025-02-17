createRmd2 <-
  function(res, analyse = "PCA", file = "", document = c("word_document", "pdf_document", "html_document")) {
    if(!is.character(file)) {return(warning("the parameter 'file' has to be a character chain giving the name of the .Rmd file to write in"))}
    
    if(any(!document %in% c("word_document", "pdf_document", "html_document"))) 
    {return(warning("the parameter 'document' should only take 'word_document', 'pdf_document' or 'html_document' as value"))}
    document = unique(document)
    
    if(!analyse %in% c("PCA", "CA", "CaGalt", "MCA", "MFA", "DMFA", "FAMD", "GPA", "HCPC", "HCPCshiny"))
    {return(warning("the parameter 'res' has to be an object of class 'PCA', 'CA', 'CaGalt', 'MCA', 'MFA', 'DMFA', 'FAMD', 'GPA' or 'HCPC'"))}
    if (!analyse %in% c("HCPC", "HCPCshiny")) param = getParam(res)
    
    # initialisation du fichier Rmd
    cat(file = file, append = FALSE)
    switch(analyse,
           PCA = {
             ind = param$ind
             var = param$var
             quanti.sup = param$quanti.sup
             quali.sup = param$quali.sup
             ind.sup = param$ind.sup
             
             ## 2 Rmd chunks takes out 
             
             if(!is.null(quanti.sup)) {
               if(length(quanti.sup) == 1) {
                 writeRmd(gettext(", 1 quantitative variable is considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(paste(",", length(quanti.sup), gettext("quantitative variables are considered as illustrative",domain="R-FactoInvestigate")), file = file, end = NULL)
               }
             }
             if(!is.null(quali.sup)) {
               if(length(quali.sup) == 1) {
                 writeRmd(gettext(", 1 qualitative variable is considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(quali.sup), gettext("qualitative variables are considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
             if(!is.null(ind.sup)) {
               if(length(ind.sup) == 1) {
                 writeRmd(gettext(", 1 individual is considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(ind.sup), gettext("individuals are considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
           },
           
           CA = {
             row = param$row
             col = param$col
             row.sup = param$row.sup
             col.sup = param$col.sup
             quanti.sup = param$quanti.sup # verifier integration dans le script
             quali.sup = param$quali.sup
             
             writeRmd("---\ntitle: '", gettext("Correspondence Analysis",domain="R-FactoInvestigate"), "'\nauthor: '", gettext("Dataset",domain="R-FactoInvestigate")," ", 
                      strsplit(as.character(res$call$call), "\\[")[[2]][1], "'\n", "output:",
                      paste("\n  ", document, ": default", sep = "", collapse = ""), sep = "", file = file, end = "\n---\n")
             writeRmd(gettext("This dataset contains",domain="R-FactoInvestigate"), row + length(row.sup), gettext("rows and",domain="R-FactoInvestigate"),
                      col + length(col.sup), gettext("columns",domain="R-FactoInvestigate"), file = file, end = NULL)
             
             if(!is.null(row.sup)) {
               if(length(row.sup) == 1) {
                 writeRmd(gettext(", 1 row is considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(row.sup), gettext("rows are considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
             if(!is.null(col.sup)) {
               if(length(col.sup) == 1) {
                 writeRmd(gettext(", 1 column is considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(col.sup), gettext("columns are considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
             if(!is.null(quanti.sup)) {
               if(length(quanti.sup) == 1) {
                 writeRmd(gettext(", 1 additional variable is quantitative and considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(quanti.sup), gettext("additional variables are quantitative and considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
             if(!is.null(quali.sup)) {
               if(length(quali.sup) == 1) {
                 writeRmd(gettext(", 1 additional variable is qualitative and considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(quali.sup), gettext("additional variables are qualitative and considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
           },
           
           CaGalt = {},
           
           MCA = {
             ind = param$ind
             var = param$var
             quanti.sup = param$quanti.sup
             quali.sup = param$quali.sup
             ind.sup = param$ind.sup
             
             writeRmd("---\ntitle: '", gettext("Multiple Correspondence Analysis",domain="R-FactoInvestigate"), "'\nauthor: '", gettext("Dataset",domain="R-FactoInvestigate")," ",
                      strsplit(as.character(res$call$call), "\\[")[[2]][1], "'\n", "output:",
                      paste("\n  ", document, ": default", sep = "", collapse = ""), sep = "", file = file, end = "\n---\n")
             writeRmd(gettext("This dataset contains",domain="R-FactoInvestigate"), ind + length(ind.sup), gettext("individuals and",domain="R-FactoInvestigate"),
                      var + length(quanti.sup) + length(quali.sup), "variables", file = file, end = NULL)
             
             if(!is.null(quanti.sup)) {
               if(length(quanti.sup) == 1) {
                 writeRmd(gettext(", 1 quantitative variable is considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(quanti.sup), gettext("quantitative variables are considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
             if(!is.null(quali.sup)) {
               if(length(quali.sup) == 1) {
                 writeRmd(gettext(", 1 qualitative variable is considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(quali.sup), gettext("qualitative variables are considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
             if(!is.null(ind.sup)) {
               if(length(ind.sup) == 1) {
                 writeRmd(gettext(", 1 individual is considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               } else {
                 writeRmd(",", length(ind.sup), gettext("individuals are considered as illustrative",domain="R-FactoInvestigate"), file = file, end = NULL)
               }
             }
           },
           
           MFA = {},
           
           HMFA = {},
           
           DMFA = {},
           
           FAMD = {},
           
           GPA = {},
           
           HCPC = {
             writeRmd("---\ntitle: '", gettext("Classification",domain="R-FactoInvestigate"), "'\nauthor: '", gettext("Dataset",domain="R-FactoInvestigate")," ",
                      strsplit(as.character(res$call$call), "\\[")[[2]][1], "'\n", "output:",
                      paste("\n  ", document, ": default", sep = "", collapse = ""), sep = "", file = file, end = "\n---\n")
           })
    
    ## Drop next 2 line
    # writeRmd(".", file = file)
     writeRmd("\n- - -\n", file = file) # saut de ligne pour separation dans le compte-rendu
  }