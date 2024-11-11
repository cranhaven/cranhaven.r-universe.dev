### R code from vignette source 'Howto.Rtex'

###################################################
### code chunk number 1: Howto.Rtex:24-25
###################################################
  library(Rgb)


###################################################
### code chunk number 2: Howto.Rtex:101-102 (eval = FALSE)
###################################################
##   help("Annotation")


###################################################
### code chunk number 3: Howto.Rtex:109-119 (eval = FALSE)
###################################################
##   download.file(
##     "http://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/cytoBand.txt.gz",
##     destfile = "cytoBand.txt.gz"
##   )
##   track <- track.bands.UCSC(
##     file = "cytoBand.txt.gz",
##     .organism = "Human",
##     .assembly = "hg19"
##   )
##   saveRDT(track, file="cytoBands.rdt")


###################################################
### code chunk number 4: Howto.Rtex:124-125 (eval = FALSE)
###################################################
##   help("Annotation")


###################################################
### code chunk number 5: Howto.Rtex:138-141
###################################################
  file <- system.file("extdata/Cosmic_ATM.gtf.gz", package="Rgb")
  tt <- track.table.GTF(file)
  saveRDT(tt, file="custom.rdt")


###################################################
### code chunk number 6: Howto.Rtex:151-156
###################################################
  data(hsGenes)
  class(hsGenes)
  head(hsGenes)
  tt <- track.table(hsGenes)
  saveRDT(tt, file="custom.rdt")


###################################################
### code chunk number 7: Howto.Rtex:163-169
###################################################
  track <- track.bam(
    bamPath = system.file("extdata/ATM.bam", package="Rgb"),
    .organism = "Human",
    .assembly = "hg19"
  )
  saveRDS(track, file="sequencing.rds")


###################################################
### code chunk number 8: Howto.Rtex:184-185 (eval = FALSE)
###################################################
##   tk.browse()


###################################################
### code chunk number 9: Howto.Rtex:208-214
###################################################
  data(hsBands)
  data(hsGenes)
  dl <- drawable.list()
  dl$add(file=NA, track=hsBands)
  dl$add(file=NA, track=track.table(hsGenes))
  browsePlot(dl, chrom="1", start=0, end=10e6)


###################################################
### code chunk number 10: Howto.Rtex:219-221 (eval = FALSE)
###################################################
##   dl$fix.files()
##   dl$fix.param()


###################################################
### code chunk number 11: Howto.Rtex:226-230
###################################################
  pdf("Rgb_tests.pdf")
  browsePlot(dl, chrom="1", start=0, end=10e6)
  browsePlot(dl, chrom="8", start=50e6, end=60e6)
  dev.off()


###################################################
### code chunk number 12: Howto.Rtex:244-250
###################################################
  data(hsGenes)
  genes <- track.table(hsGenes)
  genes$slice(chrom="12", start=45e6, end=48e6)
  system.time(
    for(i in 1:10000) genes$slice("12", 25e6, 118e6)
  )


###################################################
### code chunk number 13: Howto.Rtex:257-264
###################################################
  data(hsBands)
  data(hsGenes)
  print(hsBands)
  genes <- track.table(hsGenes)
  hsBands$cross(genes, type="count")[1:5]
  hsBands$cross(genes, colname="genes", type="name", maxElements=5)
  print(hsBands)


###################################################
### code chunk number 14: Howto.Rtex:273-279
###################################################
  # Drawable data format
  data(hsGenes)
  genes <- track.table(hsGenes)
  
  # Draw
  genes$draw(chrom="8", start=15e6, end=20e6)


###################################################
### code chunk number 15: Howto.Rtex:284-286
###################################################
  print(genes$defaultParams()[1:5])
  genes$draw(chrom="8", start=15e6, end=20e6, colorVal="blue")


###################################################
### code chunk number 16: Howto.Rtex:291-298
###################################################
  # Session persistent
  print(genes$defaultParams()[["mar"]])
  genes$setParam("mar", c(1.5, 5.0, 0.2, 1.0))
  genes$draw(chrom="8", start=15e6, end=20e6)
  
  # Save to file with custom parameters
  saveRDT(genes, file="genes.rdt")


###################################################
### code chunk number 17: Howto.Rtex:313-314 (eval = FALSE)
###################################################
##   help("setRefClass")


###################################################
### code chunk number 18: Howto.Rtex:318-321
###################################################
  data(hsBands)
  hsBands$fill(1:5, "stain", LETTERS[1:5])
  hsBands$getColNames()


###################################################
### code chunk number 19: Howto.Rtex:325-331
###################################################
  data(hsBands)
  a <- hsBands
  a$getColNames()
  a$delColumns("stain")
  hsBands$getColNames()
  hsCopy <- hsBands$copy()


###################################################
### code chunk number 20: Howto.Rtex:335-338
###################################################
  classDefinition <- getRefClass("sliceable")
  classDefinition$methods()
  classDefinition$help("draw")


###################################################
### code chunk number 21: Howto.Rtex:342-350
###################################################
  # All "track.table" objects are "drawable" objects
  class(hsBands)
  is(hsBands, "drawable")
  
  # Many "track.table" methods are defined by "drawable" class
  dw <- getRefClass("drawable")
  tl <- getRefClass("track.table")
  intersect(dw$methods(), tl$methods())


###################################################
### code chunk number 22: Howto.Rtex:381-386
###################################################
  library(Rgb)
  df <- data.frame(colA=letters[1:5], colB=5:1)
  rt <- refTable(df)
  rt <- refTable(colA=letters[1:5], colB=5:1)
  print(rt)


###################################################
### code chunk number 23: Howto.Rtex:391-398
###################################################
  library(Rgb)
  data(hsGenes)
  rf <- refTable(hsGenes)
  rf$extract(1:5)
  rf$extract(c(TRUE, rep(FALSE, 799)))
  rf$extract(expression(name == "RDX"))
  rf$extract(expression(chrom == "X" & grepl("^AR", name)))


###################################################
### code chunk number 24: Howto.Rtex:403-404 (eval = FALSE)
###################################################
##   example(topic="refTable-class", package="Rgb")


###################################################
### code chunk number 25: Howto.Rtex:414-419
###################################################
  library(Rgb)
  tl <- track.table(name=letters[1:5], chrom=1:5, strand="+", start=1:5, end=2:6)
  df <- data.frame(chrom=1:5, strand="+", start=1:5, end=2:6)
  tl <- track.table(df, .makeNames=TRUE, .organism="Human", warn=FALSE)
  print(tl)


###################################################
### code chunk number 26: Howto.Rtex:431-434
###################################################
  library(Rgb)
  data(hsBands)
  hsBands$draw("1", 0, 150e6)


###################################################
### code chunk number 27: Howto.Rtex:437-440
###################################################
  hsBands$getParam("drawFun")
  hsBands$setParam("label", FALSE)
  hsBands$draw("1", 0, 150e6)


###################################################
### code chunk number 28: Howto.Rtex:447-451
###################################################
  library(Rgb)
  data(hsBands)
  hsBands$setParam("label", FALSE)
  hsBands$draw("1", 0, 150e6, label=TRUE)


###################################################
### code chunk number 29: Howto.Rtex:458-464
###################################################
  library(Rgb)
  data(hsBands)
  hsBands$getParam("drawFun")
  names(hsBands$defaultParams())
  hsBands$setParam("drawFun", "draw.points")
  names(hsBands$defaultParams())


###################################################
### code chunk number 30: Howto.Rtex:481-484
###################################################
  library(Rgb)
  system.file("extdata/ATM.bam", package="Rgb")
  system.file("extdata/ATM.bam.bai", package="Rgb")


###################################################
### code chunk number 31: Howto.Rtex:497-503
###################################################
  track <- track.bam(
    bamPath = system.file("extdata/ATM.bam", package="Rgb"),
    .organism = "Human",
    .assembly = "hg19"
  )
  saveRDS(track, file="ATM.rds")


###################################################
### code chunk number 32: Howto.Rtex:508-509 (eval = FALSE)
###################################################
##   tk.browse()


###################################################
### code chunk number 33: Howto.Rtex:522-528
###################################################
  track <- track.bands.UCSC(
    file = "cytoBand.txt.gz",
    .organism = "Human",
    .assembly = "hg19"
  )
  saveRDT(track, file="cytoBands.rdt")


###################################################
### code chunk number 34: Howto.Rtex:533-539
###################################################
  track <- track.exons.CCDS(
    file = "CCDS.current.txt",
    .organism = "Human",
    .assembly = "hg19"
  )
  saveRDT(track, file="exons.rdt")


###################################################
### code chunk number 35: Howto.Rtex:565-567
###################################################
  file <- system.file("extdata/Cosmic_ATM.gtf.gz", package="Rgb")
  track <- track.table.GTF(file, .organism="Human", .assembly="hg19")


###################################################
### code chunk number 36: Howto.Rtex:572-574
###################################################
  print(track)
  track$draw("11", 108.5e6, 108.6e6)


###################################################
### code chunk number 37: Howto.Rtex:579-580
###################################################
  track$draw("11", 108.5e6, 108.6e6, maxElements=100)


###################################################
### code chunk number 38: Howto.Rtex:585-587
###################################################
  track$setParam("maxElements", 100)
  track$draw("11", 108.5e6, 108.6e6)


###################################################
### code chunk number 39: Howto.Rtex:592-595
###################################################
  newNames <- track$extract(,"gene_id")
  track$fill(, "name", newNames)
  track$draw("11", 108.5e6, 108.6e6)


###################################################
### code chunk number 40: Howto.Rtex:600-602
###################################################
  track$name <- "COSMIC ATM"
  track$draw("11", 108.5e6, 108.6e6)


###################################################
### code chunk number 41: Howto.Rtex:607-608
###################################################
  saveRDT(track, file="COSMIC_ATM.rdt")


###################################################
### code chunk number 42: Howto.Rtex:623-631
###################################################
  dl <- drawable.list(
    files = c(
      "cytoBands.rdt",
      "ATM.rds",
      "exons.rdt",
      "COSMIC_ATM.rdt"
    )
  )


###################################################
### code chunk number 43: Howto.Rtex:636-638 (eval = FALSE)
###################################################
##   dl$fix.param()
##   dl$fix.files()


###################################################
### code chunk number 44: Howto.Rtex:643-645
###################################################
  print(dl)
  dl$getByNames("UCSC bands")


###################################################
### code chunk number 45: Howto.Rtex:650-652
###################################################
  target <- dl$getByNames("UCSC bands")[[1]]
  names(target$defaultParams())


###################################################
### code chunk number 46: Howto.Rtex:657-661
###################################################
  dl$getByNames("CCDS exons")[[1]]$setParam("height", 0.5)
  target <- dl$getByNames("ATM.bam")[[1]]
  target$setParam("maxRange", 8000)
  target$setParam("ylim", c(0, 50))


###################################################
### code chunk number 47: Howto.Rtex:666-667
###################################################
  browsePlot(dl, chrom="11", start=108225450, end=108225660)


###################################################
### code chunk number 48: Howto.Rtex:672-673 (eval = FALSE)
###################################################
##   tk.browse(dl)


###################################################
### code chunk number 49: Howto.Rtex:682-684
###################################################
  exons <- readRDT("exons.rdt")
  print(exons)


###################################################
### code chunk number 50: Howto.Rtex:689-691
###################################################
  loci <- exons$extract(expression(grep("^ATM ", transcript)))
  print(head(loci))


###################################################
### code chunk number 51: Howto.Rtex:696-699
###################################################
  exonTable <- exons$extract()
  print(head(exonTable))
  loci <- subset(exonTable, grepl("^ATM ", transcript))


###################################################
### code chunk number 52: Howto.Rtex:704-713 (eval = FALSE)
###################################################
##   pdf("ATM.pdf", width=12)
##   for(i in 1:nrow(loci)) {
##     browsePlot(dl,
##       chrom = loci[i,"chrom"],
##       start = loci[i,"start"] - 150,
##       end = loci[i,"end"] + 150
##     )
##   }
##   dev.off()


###################################################
### code chunk number 53: Howto.Rtex:742-754
###################################################
  gpl <- read.table(
    file = "GPL10855-34953.txt",
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE
  )
  gsm <- read.table(
    file = "GSM589609-38201.txt",
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE
  )


###################################################
### code chunk number 54: Howto.Rtex:759-761
###################################################
  head(gpl)
  head(gsm)


###################################################
### code chunk number 55: Howto.Rtex:766-768
###################################################
  nrow(gpl) == nrow(gsm)
  all(gpl$ID == gsm$ID_REF)


###################################################
### code chunk number 56: Howto.Rtex:773-784
###################################################
  cgh <- track.table(
    name = gpl$ID,
    chrom = gpl$RANGE_GB,
    start = as.integer(gpl$RANGE_START),
    end = as.integer(gpl$RANGE_END),
    strand = gpl$RANGE_STRAND,
    value = gsm$VALUE,
    .name = "GSM589609",
    .organism = "Arabidopsis thaliana",
    .assembly = "TAIR9"
  )


###################################################
### code chunk number 57: Howto.Rtex:789-790
###################################################
  cgh


###################################################
### code chunk number 58: Howto.Rtex:795-800
###################################################
  cgh$getLevels("chrom")
  cgh$chromosomes()
  cgh$setLevels("chrom", newLevels=c(1:5, "C", "M"))
  cgh$chromosomes()
  cgh


###################################################
### code chunk number 59: Howto.Rtex:807-808
###################################################
  cgh$draw(chrom="1", start=16125e3, end=16127e3)


###################################################
### code chunk number 60: Howto.Rtex:813-817 (eval = FALSE)
###################################################
##   help(draw.boxes)
##   help(draw.points)
##   help(draw.hist)
##   help(draw.bg)


###################################################
### code chunk number 61: Howto.Rtex:822-824
###################################################
  cgh$setParam("drawFun", "draw.points")
  cgh$draw(chrom="1", start=16125e3, end=16127e3)


###################################################
### code chunk number 62: Howto.Rtex:829-830
###################################################
  cgh$defaultParams()$ylim


###################################################
### code chunk number 63: Howto.Rtex:835-836
###################################################
  cgh$draw(chrom="1", start=16125e3, end=16127e3, ylim=c(0, 500))


###################################################
### code chunk number 64: Howto.Rtex:841-843
###################################################
  cgh$setParam("ylim", c(0, 500))
  cgh$draw(chrom="1", start=16125e3, end=16127e3)


###################################################
### code chunk number 65: Howto.Rtex:848-850
###################################################
  cgh$setParam("yaxt", "s")
  cgh$draw(chrom="1", start=16125e3, end=16127e3)


###################################################
### code chunk number 66: Howto.Rtex:855-856
###################################################
  saveRDT(cgh, file="GSM589609.rdt")


###################################################
### code chunk number 67: Howto.Rtex:873-879
###################################################
  tab <- read.table(
    file = "TAIR9_AGI_marker.data",
    sep = "\t",
    header = FALSE,
    stringsAsFactors = FALSE
  )


###################################################
### code chunk number 68: Howto.Rtex:884-894
###################################################
  mrk <- track.table(
    name = tab$V2,
    chrom = tab$V5,
    start = tab$V3,
    end = tab$V4,
    strand = NA,
    .name = "Genetic markers",
    .organism = "Arabidopsis thaliana",
    .assembly = "TAIR9"
  )


###################################################
### code chunk number 69: Howto.Rtex:899-902
###################################################
  mrk$setLevels("chrom", newLevels=c(1:5, "C", "M"))
  mrk$draw(chrom="1", start=16124e3, end=16130e3)
  saveRDT(mrk, file="GeneticMarkers.rdt")


###################################################
### code chunk number 70: Howto.Rtex:914-917
###################################################
  gtf <- read.gtf("TAIR9_GFF3_genes.gff")
  head(gtf)
  dim(gtf)


###################################################
### code chunk number 71: Howto.Rtex:922-923
###################################################
  table(gtf$feature)


###################################################
### code chunk number 72: Howto.Rtex:928-938
###################################################
  gtf <- read.gtf("TAIR9_GFF3_genes.gff", features="exon")
  trk <- track.table.GTF(
    file = "TAIR9_GFF3_genes.gff",
    name = "Exons",
    attr = "split",
    features = "exon",
    .organism = "Arabidopsis thaliana",
    .assembly = "TAIR9"
  )
  trk


###################################################
### code chunk number 73: Howto.Rtex:943-945
###################################################
  trk$delColumns(c("source","feature","score","frame"))
  trk


###################################################
### code chunk number 74: Howto.Rtex:950-951
###################################################
  trk$setLevels("chrom", c(1:5, "C", "M"))


###################################################
### code chunk number 75: Howto.Rtex:956-957
###################################################
  trk$draw(chrom="1", start=16150e3, end=16158e3)


###################################################
### code chunk number 76: Howto.Rtex:970-973
###################################################
  exn <- new("track.exons")
  exn$import(trk)
  exn


###################################################
### code chunk number 77: Howto.Rtex:978-981
###################################################
  exn$buildGroupSize("Parent", "exonCount")
  exn$buildGroupPosition("Parent", "exonNumber")
  exn


###################################################
### code chunk number 78: Howto.Rtex:986-988
###################################################
  newNames <- paste(exn$extract(,"Parent"), exn$extract(,"exonNumber"), sep="#")
  exn$fill(, "name", newNames)


###################################################
### code chunk number 79: Howto.Rtex:993-999
###################################################
  exn$setParam("groupBy", "Parent")
  exn$setParam("groupPosition", "exonNumber")
  exn$setParam("groupSize", "exonCount")

  exn$draw(chrom="1", start=16150e3, end=16158e3)
  saveRDT(exn, file="TAIR9 exons.rdt")


###################################################
### code chunk number 80: Howto.Rtex:1008-1013
###################################################
  dl <- drawable.list()
  dl$add(file="GeneticMarkers.rdt")
  dl$add(file="TAIR9 exons.rdt")
  dl$add(file="GSM589609.rdt")
  browsePlot(dl, chrom="1", start=16123e3, end=16158e3)


###################################################
### code chunk number 81: Howto.Rtex:1016-1017 (eval = FALSE)
###################################################
##   tk.browse(dl)


###################################################
### code chunk number 82: Howto.Rtex:1024-1030
###################################################
  gsm <- readRDT("GSM589609.rdt")
  exn <- readRDT("TAIR9 exons.rdt")
  gen <- gsm$extract(expression(which.max(value)), asObject=TRUE)
  gen$cross(exn, type="Parent", colname="gene")
  gen
  exn$extract(expression(Parent == gen$extract(,"gene")))


###################################################
### code chunk number 83: Howto.Rtex:1035-1039
###################################################
  atg <- exn$extract(expression(Parent == gen$extract(,"gene")), asObject=TRUE)
  atg$cross(gsm, type="count")
  atg$cross(gsm, type="count", colname="probeCount")
  atg


###################################################
### code chunk number 84: Howto.Rtex:1044-1057
###################################################
  atg$addColumn(
    content = rep(as.double(NA), atg$getRowCount()),
    name = "expr"
  )
  for(i in 1:atg$getRowCount()) {
    probes <- gsm$slice(
      chrom = atg$extract(i, "chrom"),
      start = atg$extract(i, "start"),
      end = atg$extract(i, "end")
    )
    atg$fill(i, "expr", mean(probes$value))
  }
  atg


###################################################
### code chunk number 85: Howto.Rtex:1062-1072
###################################################
  expr <- list()
  for(i in 1:atg$getRowCount()) {
    probes <- gsm$slice(
      chrom = atg$extract(i, "chrom"),
      start = atg$extract(i, "start"),
      end = atg$extract(i, "end")
    )
    expr[[i]] <- probes$value
  }
  boxplot(expr, varwidth=TRUE, log="y")


###################################################
### code chunk number 86: Howto.Rtex:1095-1107
###################################################
  # Really simple drawing function, just drawing lines
  draw.custom <- function(slice, start, end, ...) {
     draw.bg(start=start, end=end, ...)
  	segments(x0=slice$start, x1=slice$end, y0=0:1, y1=1:0)
  }
    
  # Edit a track to use it
  data(hsBands)
  hsBands$setParam("drawFun", "draw.custom")
  
  # Let's draw
  hsBands$draw("1", 0, 100e6)


###################################################
### code chunk number 87: Howto.Rtex:1120-1141
###################################################
  # Define a new class, just drawing red boxes
  setRefClass(
    Class = "track.custom",
    contains = "track.bands",
    methods = list(
      defaultParams = function(...) {
        params <- callSuper(...)
        params$colorVal <- "red"
        params$colorFun <- function(slice) NULL
        return(params)
      }
    )
  )
  
  # Class switch
  data(hsBands)
  obj <- new("track.custom")
  obj$import(hsBands)
  
  # Let's draw
  obj$draw("1", 0, 100e6)


###################################################
### code chunk number 88: Howto.Rtex:1160-1190
###################################################
  setRefClass(
    Class = "track.scale",
    contains = "drawable",
    methods = list(
      defaultParams = function(...) {
        # Define new class defaults
        params <- callSuper(...)
        params$col <- "lightblue"
        params$points <- 500L
        return(params)
      },
      draw = function(chrom, start=NA, end=NA, ...) {
        # Aggregate and prioritize drawing parameters
        argList <- callParams(chrom, start, end, ...)
        
        # Plot background, using drawing parameters
        do.call(what=draw.bg, args=argList)
        
        # Data points
        x <- seq(from=start, to=end, length.out=argList$points)
        y <- cos(x)
        
        # Plot, using drawing parameters
        lines(x=x, y=y, col=argList$col)
      }
    )
  )
  object <- new("track.scale")
  object$draw("11", 0, 10e6)
  object$defaultParams()


###################################################
### code chunk number 89: Howto.Rtex:1195-1210 (eval = FALSE)
###################################################
## defaultParams = function(...) {
##   # Get inherited defaults
##   params <- callSuper(...)
##   
##   # Get draw.bg defaults
##   form <- formals("draw.bg")
##   form <- form[ setdiff(names(form), c("start", "end", "...")) ]
##   for(fname in names(form)) params[[ fname ]] <- eval(form[[ fname ]])
##   
##   # Define new class defaults
##   params$col <- "lightblue"
##   params$points <- 500L
##   
##   return(params)
## }


###################################################
### code chunk number 90: Howto.Rtex:1219-1222
###################################################
  saveRDS(object, "custom.rds")
  dl <- drawable.list(files="custom.rds")
  browsePlot(dl, chrom="1", start=10e6, end=100e6)


