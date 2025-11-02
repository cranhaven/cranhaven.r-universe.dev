#'
#' Load, Fit, and plot
#'
#' @param expDesign a data Frame that contains minimum a column with the files direction (name of the column files) and another with a shorter name to be used inside the function.
#' @param rigidity an integer number specifying the rigidity parameter to be used.
#' @param outputdir a character string that specifies the directory in which to save the results form the function.
#' @param nstates the number of states to be fitted in the model. A standard setting would use 3 states (Homozygous1, Heterozygous, and Homozygous2).
#' @param seqlengths a named vector with the chromosome lenghts of the organism that the user is working with.
#' @param eps the threshold of the difference between the parameters value between the previous and actuay iteration to stope de EM algorithm.
#' @param max.iter maximum number of iterations of the EM algorithm before to stop in case that eps has not been achieved.
#' @param  autotune Logical value if the R-value should be tuned by our algorithm. This will take longer as it needs a first training with the rigidity value provided by the user and then the optimization step is carried. Finally, a training using the optimum R will be performed and results for the optimum R will be returned.
#' @param max_rigidity If autotune true, R values will be explored up the value given in this parameter. Default = 2^9
#' @param average_coverage If autotune true, for conservative results set it to the lowest average coverage of a sample in your experiment, or evne to the lowest average coverage in a (sufficiently large) region in one of your samples. The lower the value, the more conservative (higher) our estimates of the false positive segments rates. If it is not provided it will be computed as the average of all data points.
#' @param crossovers_per_megabase If autotune true,  for conservative results set it to the highest ratio of a sample in your experiment. The higher the value, the more conservative (higher) our estimates of the false positive segments rates. If it is not provided it will be computed as the average of all samples.
#' @param trace logical value. Whether or not to keep track of the parameters for the HMM along the iterations. Deafault FALSE
#' @param tiles length of the tiles by which the genome will be segmented in order to compute the ratio of COs in the complete dataset.
#' @param all logical value. Whether to use the complete data set to fit the rHMM. default TRUE.
#' @param random Logical value. Choose randomly a subset of the complete dataset to fit the rHMM. Default FALSE
#' @param nsamples if random TRUE, how many samples should be taken randomly.
#' @param post.processing Logical value. Whether to run an extra step that fine maps the segment borthers. Default TRUE
#' @param specific Logical value to specify which samples to take.
#' @param save.results Logical value, whether to generate and save the plots and igv files.
#' @param verbose Logical, whether to print info to console.
#' @return Matrix m x n. M number of samples and N chromosomes.
#'
#' @return RTIGER object
#' @usage RTIGER(expDesign, rigidity=NULL, outputdir=NULL, nstates = 3,
#' seqlengths = NULL, eps=0.01, max.iter=50, autotune = FALSE,
#' max_rigidity = 2^9, average_coverage = NULL,
#' crossovers_per_megabase = NULL, trace = FALSE,
#' tiles = 4e5, all = TRUE, random = FALSE, specific = FALSE,
#' nsamples = 20, post.processing = TRUE, save.results = TRUE, verbose = TRUE)
#'
#' @examples
#'\dontrun{
#' data("ATseqlengths")
#' sourceJulia()
#' path = system.file("extdata",  package = "RTIGER")
#' files = list.files(path, full.names = TRUE)
#' nam = sapply(list.files(path ), function(x) unlist(strsplit(x, split = "[.]"))[1])
#' expDesign = data.frame(files = files, name = nam)
#' names(ATseqlengths) = paste0("Chr", 1:5)
#' myres = RTIGER(expDesign = expDesign,
#'                outputdir = "/home/campos/Documents/outputjulia/",
#'                seqlengths = ATseqlengths,
#'                rigidity = 4,
#'                max.iter = 2,
#'                trace = FALSE,
#'                save.results = TRUE)
#'}
#'
#' @export RTIGER
#'

RTIGER = function(expDesign,
                  rigidity=NULL,
                  outputdir=NULL,
                  nstates = 3,
                  seqlengths = NULL,
                  eps=0.01,
                  max.iter=50,
                  autotune = FALSE,
                  max_rigidity = 2^9,
                  average_coverage = NULL,
                  crossovers_per_megabase = NULL,
                  trace = FALSE,
                  tiles = 4e5,
                  all = TRUE,
                  random = FALSE,
                  specific = FALSE,
                  nsamples = 20,
                  post.processing = TRUE,
                  save.results = TRUE,
                  verbose = TRUE){
  # Checks
  if(any(seqlengths < tiles)) stop("Your tiling distance is larger than some of your chromosomes. Reduce the tiling parameter.\n")
  if(is.null(rigidity)) stop("Rigidity must be specified. This is a data specific parameter. Check vignette.\n")
  if(!is.integer(rigidity))  rigidity = as.integer(rigidity)
  if(is.null(outputdir) & save.results ) stop("Outputdir must be specified. The results are automatichally saved inside the folder.\n")
  if(!is.null(outputdir)) if(!file.exists(outputdir)) if(verbose) cat(paste0("The new directory: ", outputdir, " will be created.\n"))
  if(!is.integer(nstates)) nstates = as.integer(nstates)
  if(is.null(seqlengths)) stop("seqlengths are necessary to create the Genomic Ranges object to store the data. Please, introduce the chromosome lengths of your organism.\n")
  if(!is.integer(max.iter)) max.iter = as.integer(max.iter)
  if(save.results){
    # if(sum(c("Gviz", "rtracklayer") %in% rownames(installed.packages())) != 2) stop("To save the results you need to have installed Gviz and rtracklayer.\n
    #                                                                                 Currently you are missing them.")
    requireNamespace("Gviz")
    requireNamespace("rtracklayer")
  }
  if(autotune){
    post_post.processing = post.processing
    post.processing = FALSE
  }

  # Load data
  if(verbose) cat("Loading data and generating RTIGER object.\n")
  newn = paste("Sample", 1:nrow(expDesign), sep = "_")
  names(newn) = expDesign$name
  expDesign$OName = expDesign$name
  expDesign$name = newn
  myDat = generateObject(experimentDesign = expDesign,nstates = nstates,rigidity = rigidity, seqlengths = seqlengths, verbose = verbose)
  info = myDat@info
  # params = myDat@params
  # Check length observations

  obs.l = sapply(myDat@matobs, function(x) sapply(x, ncol))
  if(any(obs.l < 2*rigidity)) stop("Some of your observations is smaller than 2 times rigidity. Decrease your rigidity value.")

  # Fit and decode
  if(verbose) cat("\n\nFitting the parameters and Viterbi decoding. \n")
  if(verbose) cat("post processing value is:", post_post.processing,"\n")
  if(verbose) cat("R value autotune is:", autotune,"\n")
  if(autotune & verbose) cat("Fitting an RTIGER object with the R provided by the user.\n")
  myDat = fit(rtigerobj = myDat,
              max.iter = max.iter,
              eps = eps,
              trace = trace,
              all = all,
              random = random,
              specific = specific,
              nsamples = nsamples,
              post.processing = post.processing
              )
  if(autotune){
    if(verbose) cat("Optimizing the R parameter.\n")
    opt_R = optimize_R(myDat, max_rigidity = max_rigidity, average_coverage = average_coverage,
                       crossovers_per_megabase = crossovers_per_megabase)
    if(verbose) cat("Best R value:", opt_R,"\n")
    myDat@params$rigidity = as.integer(opt_R)
    myDat = fit(rtigerobj = myDat,
                max.iter = max.iter,
                eps = eps,
                trace = trace,
                all = all,
                random = random,
                specific = specific,
                nsamples = nsamples,
                post.processing = post_post.processing
    )

  }
  if(all(info$sample_names == expDesign$name)) info$sample_names = expDesign$OName
  myDat@info$expDesign = expDesign


  if(verbose) cat("Number of iterations run: ", myDat@num.iter, "\n\n")
  if(myDat@num.iter == max.iter) cat("--------------------------\n
  Warning!! The maximum number of iterations were needed without reaching convergence.\n
                                     We recommend to increase the number of iterations.
                                     \n\n--------------------------\n\n")

  if(save.results){
    # require(Gviz)
    # require(rtracklayer)
    if(!dir.exists(outputdir)) dir.create(outputdir)
    if(verbose) cat("Plotting samples Genotypes.\n")
    for(samp in info$sample_names){
      # f.name = names(newn)[newn %in% samp]
      sampdir = file.path(outputdir, samp)
      myx = paste0("GenotypePlot_",samp, ".pdf")
      if(!dir.exists(sampdir)) dir.create(sampdir)
      on = file.path(sampdir, myx)
      pdf(on)
      for(chr in info$part_names){
        ren = newn[as.character(samp)]
        plotGenotype(myDat, ren, chr, ratio = TRUE, window = 10)
      }
      dev.off()
    }



    # Plotting CO number per chormosome
    if(verbose) cat("PLotting CO number per chromosome. \n")
    myf = file.path(outputdir, "COs-per-Chromosome.pdf")
    plotCOs(myDat, myf)

    # Plotting CO number per Sample
    cos = calcCOnumber(myDat)
    cos = colSums(cos)
    cos = melt(cos)
    rev.newn = myDat@info$expDesign$OName
    names(rev.newn) = myDat@info$expDesign$name
    cos$Sample = rev.newn[rownames(cos)]
    colnames(cos) = c( "COs", "Sample")
    myf = file.path(outputdir, "CO-count-perSample.pdf")
    pdf(myf)

    p <- ggplot(data=cos, aes(x=Sample, y=COs)) +
      geom_bar(stat="identity") +
      # geom_bar() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ylab("Number of COs")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.y = element_text(angle = 45))+
      coord_flip()
    print(p)
    dev.off()

    # Create output
    if(verbose) cat("Creating bed and IGV output formats.\n")
    for(samp in info$sample_names){

      export2IGV(myDat, sample = samp, dir = outputdir, ratio = TRUE, newn = newn)
    }


    # Goodness of fit ---------------------------------------------------------

    if(nstates < 4){
      if(verbose) cat("Plotting goodness of fit.\n")
      vit = myDat@Viterbi
      if(length(vit) >= 10)vit = vit[sample(1:length(vit), ceiling(.1*length(vit)))]

      hetrat = unlist(lapply(vit, function(x) x$P1.Allele.Count[x$Viterbi == "het"]/x$total[x$Viterbi == "het"] * 100))
      patrat = unlist(lapply(vit, function(x) x$P1.Allele.Count[x$Viterbi == "pat"]/x$total[x$Viterbi == "pat"]* 100))
      matrat = unlist(lapply(vit, function(x) x$P1.Allele.Count[x$Viterbi == "mat"]/x$total[x$Viterbi == "mat"]* 100))
      if(nstates == 3 & any(c(length(hetrat), length(patrat), length(matrat)) == 0)){
        if(verbose) cat("Your data probably comes form a back-crossed population. Please fit the model with nstates = 2.\n
          The plot Goodness-Of-Fit.pdf might be erroneous.")
        myl = list(hetrat, patrat, matrat)
        myl = myl[which(c(length(hetrat), length(patrat), length(matrat)) != 0)]
        hetrat = myl[[1]]
        patrat = myl[[2]]
      }
      if(nstates < 4){
        alphas = as.vector(myDat@params$paraBetaAlpha)
        names(alphas) = rownames(myDat@params$paraBetaAlpha)
        betas = as.vector(myDat@params$paraBetaBeta)
        names(betas) = rownames(myDat@params$paraBetaBeta)
        x = 0:100
        y = NULL
        ecolors = c("red","violet","blue")
        for (e_state in names(alphas)) { y = cbind(y,dbb(x,100,alphas[e_state],betas[e_state])) }
        colnames(y) = names(alphas)

        myf = file.path(outputdir, "Goodness-Of-Fit.pdf")
        pdf(myf)

        if(length(patrat) > 0){
          hist(patrat, probability = TRUE, col = rgb(1,0,0,0.25), main = "P1 homozygous states", xlab = "Allele ratio", xlim = c(0,100))
          points(x,y[,"pat"],type="l",col=ecolors[1])
          legend("topleft",c("Fitted P1 distribution"),
                 lty = 1, col = c("red"), cex = .7)
        }

        if(length(hetrat) > 0){
          hist(hetrat, probability =   TRUE, col = rgb( 0.744,0.34,0.844,0.25), main = "Heterozygous states", xlab = "Allele ratio", xlim = c(0,100))
          points(x,y[,"het"],type="l",col=ecolors[2])
          legend("topleft",c( "Fitted Heterozygous\n distribution"),
                 lty = 1, col = c( "violet"), cex = .7)
        }

        if(length(matrat) > 0){
          hist(matrat, probability = TRUE, col = rgb(0,0,1,0.25), main = "P2 homozygous states", xlab = "Allele ratio", xlim = c(0,100))
          points(x,y[,"mat"],type="l",col=ecolors[3])
          legend("topleft",c("Fitted P2 distribution"),
                 lty = 1, col = c("blue"), cex = .7)
        }


        # for (e_state in 1:nstates){}

        # legend("topright", c("P1 allele count ratio", "Heterozygous allele\n count ratio", "P2 allele count ratio")[1:nstates],
        # fil = c(rgb(1,0,0,0.25), rgb( 0.744,0.34,0.844,0.25), rgb(0,0,1,0.25))[1:nstates], cex = .7)
        dev.off()

      }

    }

  }


# Return object -----------------------------------------------------------


   return(myDat)
}
