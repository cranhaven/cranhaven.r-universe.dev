#taxa (taxon): One or more populations grouped by taxonomists
#Itis: Integrated taxonomic information system
#' @export
bioinfer.JGR <- function (bio.data = NULL, subset1.name = NULL, subset1.val = NULL, 
    subset2.name = NULL, subset2.val = NULL, siteID = NULL, taxonName = NULL, 
    taxonCount = NULL, coefBioInferData = FALSE, coefBioInferDataName = NULL, 
    coefRegData = FALSE, coefRegDataName = NULL, saveResults = TRUE, 
    resultName = NULL, tname.new = NULL, dupe.sel = NULL, pecboORtrait = NULL) 
{
    if (!exists("globenv") | is.null(tname.new)) 
        globenv = globalenv()
    if (!is.null(resultName)) 
        globenv[["resultName"]] = resultName
    if (!is.null(pecboORtrait)) 
        globenv[["pecboORtrait"]] = pecboORtrait
    if (!is.null(bio.data)) {
        names0 <- names(bio.data)
        if (names0[2] %in% c("PHYLUM", "SUBPHYLUM", "SUPERCLASS", "CLASS",
                             "SUBCLASS", "INFRACLASS", "SUPERORDER",
                             "INFRAORDER", "SUPERFAMILY", "FAMILY",
                             "SUBFAMILY", "TRIBE", "SUBTRIBE", "GENUS", 
                             "SPECIES")) {
          names0[2] <- paste(names0[2], ".orig",sep = "")
          names(bio.data) <- names0
          taxonName <- names0[2]
        }
        globenv[["bcnt"]] = gisdt.subset(bio.data, subset1.name = subset1.name, 
                 subset1.val = subset1.val, subset2.name = subset2.name, 
                 subset2.val = subset2.val)
    }
    if (coefBioInferData) {
        # envir = environment() and lazyloading in Description file were added good practice(flags).
        data(list = coefBioInferDataName, envir = environment())
        coefs = get(coefBioInferDataName)
        globenv[["coefs"]] = coefs
    }
    else if (coefRegData) {
        coefs = get(coefRegDataName)
        globenv[["coefs"]] = coefs
    }


    if (is.null(dupe.sel)) 
    {
        if (is.null(tname.new)) 
        {
            globenv[["tlevs"]] = load.itis(globenv)
            #tname is the name matrix consisting of taxa
            tname = sort(unique(as.character(globenv[["bcnt"]][[taxonName]])))
            df.parse = parse.taxon.name(tname)

            # get.valid.names splits matrix into list of valid taxa and list of invalid names
            globenv[["parse.list"]] = get.valid.names(df.parse, globenv)
            #print(globenv[["parse.list"]])
            if (nrow(globenv[["parse.list"]][["TRUE"]]) > 0) 
            {
                
                # resolve.mult: if taxon displayed in mult lists, only show in 1st.
                globenv[["parse.list"]] = resolve.mult(globenv[["parse.list"]], globenv) #problem area!
                  
                if (nrow(globenv[["parse.list"]][["TRUE"]]) > 0) 
                {
                  tmiss = unique(globenv[["parse.list"]][["TRUE"]][, 2])
                  tmiss = tmiss[nchar(tmiss) > 0]
                  b = .jnew("org/neptuneinc/cadstat/plots/BiologicalInferencesTaxaNameUnrecog")
                  .jcall(b, "Ljavax/swing/JFrame;", "getMyGUI", length(tmiss), tmiss)
                  return(invisible())
                }
            }
        }
        else 
        {
            globenv[["parse.list"]] = incorp.correct(tname.new,globenv[["parse.list"]])
        }
    }


    if (is.null(dupe.sel)) {

        globenv[["fulltab"]] = make.fulltab1(globenv[["parse.list"]][["FALSE"]], 
            globenv)
        globenv[["dupe.list"]] = locate.dupes(globenv[["fulltab"]])
        if (length(globenv[["dupe.list"]]$isav) > 0) {
            d = .jnew("org.neptuneinc.cadstat.plots.BiologicalInferencesTaxaNameDupITIS")
            .jcall(d, "Ljavax/swing/JFrame;", "getMyGUI", length(globenv[["dupe.list"]]$sumstr), 
                globenv[["dupe.list"]]$sumstr, 0)
            return(invisible())
        }
    }
    else {
      w <- regexpr("-", globenv[["dupe.list"]]$sumstr)
      tdup <- substring(globenv[["dupe.list"]]$sumstr, 1, w-1)
      ntax <- length(unique(tdup))
      if  (length(dupe.sel) == ntax) {
        isel <- numeric(0)
        for (i in 1:length(dupe.sel)) {
          isel <- c(isel, match(dupe.sel[i], globenv[["dupe.list"]]$sumstr))
        }
        if (sum(duplicated(tdup[isel])) > 0) {
          cat("Please select only one choice per taxon name.\n")
          goagain <- TRUE
        }
        else goagain <- FALSE
      }
      else {
        cat("Please select one choice per taxon name.\n")
        goagain <- TRUE
      }
      if (goagain) {
        d = .jnew("org.neptuneinc.cadstat.plots.BiologicalInferencesTaxaNameDupITIS")
        .jcall(d, "Ljavax/swing/JFrame;", "getMyGUI", length(globenv[["dupe.list"]]$sumstr), 
               globenv[["dupe.list"]]$sumstr, 0)
        return(invisible())
      }
      else {
        globenv[["fulltab"]] = remove.dupes(globenv[["fulltab"]], 
            globenv[["dupe.list"]], dupe.sel)
      }
    }


    finaltab = make.species(globenv[["parse.list"]][["FALSE"]], 
        globenv[["fulltab"]])
    output.tax.table(finaltab, globenv[["tlevs"]])
    names0 = names(globenv[["bcnt"]])
    bcnt.new = merge(globenv[["bcnt"]], finaltab, by.x = names0[2], 
        by.y = "taxaname.orig")
    bcnt.tax = bcnt.new[, c(names0, globenv[["tlevs"]], "SPECIES")]
    if (globenv[["pecboORtrait"]] == "pecbo") {
        bcnt.otu = get.otu(bcnt.tax, globenv[["coefs"]])
        ss = makess(bcnt.otu)
        result = mlsolve(ss, globenv[["coefs"]])
    }
    else {
        bcnt.otu = get.otu(bcnt.tax, globenv[["coefs"]][["TAXON"]])
        result = trait.stat(bcnt.otu, globenv[["coefs"]])
    }


    #Fix to set the environment and pass it in. Found in datamerge/bioinfer/glm/pca.fa
    pos <- 1
    envir = as.environment(pos)

    assign( "sum.otu.txt", bcnt.otu, envir = envir)
    assign(globenv[["resultName"]], result, envir = envir)
    cat("Results are saved in ", globenv[["resultName"]], 
        ".\n", sep = "")
    rm(globenv)
    return(invisible())
  }


# https://cran.r-project.org/web/packages/bio.infer/bio.infer.pdf