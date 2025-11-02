#'
#' Load data
#'
#' @param experimentDesign a data Frame that contains minimum a column with the files direction (name of the column files) and another with a shorter name to be used inside the function.
#' @param rigidity an integer number specifying the rigidity parameter to be used.
#' @param nstates the number of states to be fitted in the model. A standard setting would use 3 states (Homozygous1, Heterozygous, and Homozygous2).
#' @param seqlengths a named vector with the chromosome lenghts of the organism that the user is working with.
#' @param verbose logical value. Whether to print info messages.
#'
#' @return RTIGER object
#' @usage generateObject(experimentDesign = NULL,nstates = 3, rigidity=NULL,
#' seqlengths = NULL, verbose = TRUE)
#'
#' @examples
#'
#' data("ATseqlengths")
#' path = system.file("extdata",  package = "RTIGER")
#' files = list.files(path, full.names = TRUE)
#' nam = sapply(list.files(path ), function(x) unlist(strsplit(x, split = "[.]"))[1])
#' expDesign = data.frame(files = files, name = nam)
#' names(ATseqlengths) = paste0("Chr", 1:5)
#' myres = generateObject(experimentDesign = expDesign,
#'               seqlengths = ATseqlengths,
#'               rigidity = 10
#')
#'
#'
#' @export generateObject
#'

generateObject = function(experimentDesign = NULL, nstates = 3, rigidity = NULL, seqlengths = NULL, verbose = TRUE){
  if( is.null(experimentDesign) ){
    stop("No file information found!")
  }
  if(is.null(rigidity)) stop("No rigidity information added. Please, select a rigidity value.\n")
  if(is.null(seqlengths)) stop("Chromosome lengths are mandatory in order to know the genome length. Please, add chromosome length as a vector.\n")

  if(verbose) cat("Using", nstates, "states for fitting.\n")
  myCol = experimentDesign[, c("files", "name")]

  rawGR = sapply(1:nrow(myCol), function(i){
    samp = as.character(myCol$files[i])
    print(paste("Loading file: ", myCol$files[i]))

    f <- read.delim(file =samp, header = FALSE)

    f = checkfileColumns(f, samp)

    if(!all( f$V1 %in% names(seqlengths))) stop("Seqlength names do not match with seqnames in file. Change seqlength names to proper naming.\n
                                                Your seqnames look like this: ", unique(f$V1),"\n
                                                Yur seqlengths names are: ", names(seqlengths),"\n
                                                                                   We recommendo to change your seqlengths names to: ", unique(f$V1))

    myG = GRanges(seqnames =  f$V1,
                  ranges = IRanges(start = f$V2, end = f$V2),
                  P1.Allele = f$V3,
                  P1.Allele.Count = f$V4,
                  P2.Allele = f$V5,
                  total = f$V6 + f$V4,
                  seqlengths = seqlengths
    )
    myG = sort(myG)

    return(myG)
  }) # end sapply rawGR

  names(rawGR) = myCol$name


  obs = lapply(rawGR, function(samp){
    chrs = lapply(seqlevels(samp), function(chr){
      myG = samp[seqnames(samp) == chr]
      p = as.matrix(mcols(myG[, c("P1.Allele.Count", "total")]))
      colnames(p) = c("paternal", "total")
      # p[p[,"total"] == 0,] = NA

      p = t(p)
      colnames(p) = paste(seqnames(myG), start(myG), sep = "_")

      return(p)
    })
    names(chrs) = seqlevels(samp)

    return(chrs)
  })

  info = create_info(obs)

  params = generate_params(rigidity = rigidity, nstates = nstates)

  myObj <- .RTIGER(matobs =  obs, params = params, info = info, Viterbi = rawGR, Probabilities = list(), num.iter = 0)
  return(myObj)
  }
