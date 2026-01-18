#' Visualises genetic distances.
#'
#' plotAMids is used to visualise the relationship amongst individuals and references.
#'
#' @param AMids Dataframe of genetic distances calculated by calculateAMids or calculateAMidsArith.
#' 
#' @param phenoFile Optional file with phenotype, color and order information for individuals and populations. An example file, called CorPheno, is contained in the 'extdata' folder with the package.
#' 
#' @param columnPlot Takes values 'I' or 'C'. 'I' is the default option. 'I' plots the normalised euclidean distances whereas 'C' plots the crude distances.
#' 
#' @param quantilePlot Logical. Takes values TRUE or FALSE. TRUE is the default option. If columnPlot is 'C', TRUE will plot the quantiles, FALSE will plot the raw values.
#' 
#' @param colorPlot Colors for the AMids. Possible choices are 'RedBl', 'RedBlGr' and 'BLBrewer'. The user can also provide a vector of colors.
#' 
#' @param sepLinesPop Logical. Takes values TRUE or FALSE. The default is TRUE.
#' If TRUE, a line demarcating populations is plotted.
#' 
#' @param plotIndNames Logical. Takes values TRUE or FALSE. The default is FALSE. 
#' If TRUE, the individual ids are plotted on the left axis.
#' 
#' @param legColor Logical. Takes values TRUE or FALSE. The default is TRUE. 
#' If TRUE, the legend for the colour gradient will be plotted in the top left.
#' 
#' @param legRef Logical. Takes values TRUE or FALSE. The default is TRUE. If TRUE, text giving names of references will be plotted along the x axis.
#' 
#' @param legPheno Logical. Takes values TRUE or 'no. The default is TRUE. If TRUE, will plot colour blocks relating to the population, dataset and regional origin of data if sample IDs have been given these in the Corpheno file, if not present, will plot them under 'Unspecified'.
#' 
#' @param legAxisPop Logical. Takes values TRUE or FALSE. The default is TRUE. 
#' If TRUE text will be plotted giving name and number of populations for samples used on the right y axis of the plot.
#' 
#' @param legData Logical. Takes values TRUE or FALSE. The default is FALSE.
#' If TRUE, the reference to the dataset used to create the reference is appended to the reference population name on the bottom x axis.
#' 
#' @param bmar Takes numeric value. Changes the size of the bottom outer margin of the plot. The default is empty. 
#' For more see ?par()
#' 
#' @param lmar Takes numeric value. Changes the size of the left outer margin of the plot. The default is empty.
#' For more see ?par()
#'
#' @param tmar Takes numeric value. Changes the size of the top outer margin of the plot. The default is empty.
#' For more see ?par()
#' 
#' @param rmar Takes numeric value. Changes the size of the right outer margin of the plot. The default is empty. 
#' For more see ?par()
#'
#' @param cexref Takes numeric value. Controls text size of reference names on y axis. Default is 0.9.
#' 
#' @param cexind Takes numeric value. Controls text size of sample names on y axis. Default is 0.8.
#' Individual sample IDs need plotIndNames = "yes" to display, this is set to FALSE by default.
#'
#'
#' 
#' @examples
#' \dontrun{
#' Refs <- system.file('data', package = 'AncestryMapper')
#' tpeds <- system.file('extdata', package = 'AncestryMapper')
#' Corpheno <- system.file('extdata', 'CorPheno', package = 'AncestryMapper')
#' All00Frq <- system.file ('data', 'MinMaxFreq.rda', package = 'AncestryMapper')
#' 
#' genetic.distance <- calculateAMidsArith(pathTotpeds = tpeds,
#'                                    NameOut = 'Example',
#'                                    pathToAriMedoids = Refs,
#'                                    pathAll00 = All00Frq)
#' 
#' plotAMids(AMids = genetic.distance, phenoFile = Corpheno, columnPlot = "I")
#' }
#' @rdname plotAMids
#' @export
#' 
plotAMids <- function(AMids, phenoFile, columnPlot = "I", quantilePlot = TRUE, colorPlot = "BlBrewer",  sepLinesPop = TRUE, plotIndNames = FALSE, 
    legColor = TRUE, legRef = TRUE, legPheno = TRUE, legAxisPop = TRUE, legData = FALSE, bmar, lmar, tmar, rmar, cexref = 0.9, cexind = 0.8) {


    #if(missing(phenoFile)) phenoFile <- system.file('extdata','CorPheno',package="AncestryMapper")

    Pheno <- read.table(phenoFile, header = T, as.is = T, comment.char = "")
    
    sepLineRef = FALSE
    aid <- AMids
    Is <- gsub('I_','',colnames(aid)[grep('I_',colnames(aid))])
   
    ICCol <- grep('I_|C_',colnames(aid),invert=T)
    PopOrd <- Pheno[,c('Pheno_Pop','Order')]
    PopOrd <- unique(PopOrd)
    PopOrd <- PopOrd[order(PopOrd[,2]),]
    Unordref <- Is[!(Is%in%PopOrd[, 1])]
    if(length(Unordref)==1) print(paste0(length(Unordref),' reference without entry and order in Corpheno, it will be plotted at the beginning of the Y axis.'))
    if(length(Unordref)>1) print(paste0(length(Unordref),' references without entry and order in Corpheno, these will be plotted at the beginning of the Y axis.'))
    PopOrd <- PopOrd[PopOrd[,1]%in%Is,]
    #Dealing with undefined refs
    if(length(Unordref)>=1){
        #nrnewrow <- 1:length(Unordref)
        nrnewrow <- length(Unordref)
        #PopOrd <- rbind(nrnewrow,PopOrd)
        newrows <- data.frame(matrix(0,nrow=nrnewrow,ncol=2))
        colnames(newrows) <- colnames(PopOrd)
        PopOrd <- rbind(newrows,PopOrd)
        #PopOrd[nrnewrow,1] <- Unordref
        PopOrd[1:nrnewrow,1] <- Unordref
        #PopOrd[nrnewrow,2] <- 0
    }
    CorColOrd <- c(paste0('I_',PopOrd[,1]),paste0('C_',PopOrd[,1]),colnames(aid)[ICCol])
    aid <- aid[,CorColOrd]
    ICCol <- grep('I_|C_',colnames(aid),invert=T)
    colnames(aid)[ICCol] <- 'Id'
    aid <- aid[!duplicated(aid$Id),]
    colnames(aid) <- gsub('1000.Genomes','1KG',colnames(aid))
    
    
    
    aid <- merge(aid, Pheno, by.x = "Id", by.y = "UNIQID", all.x = T)
    
    aid$Order[is.na(aid$Pheno_Pop)] <- min(aid$Order[!(is.na(aid$Pheno_Pop))]) -1
    aid$Colors_Pop[is.na(aid$Pheno_Pop)] <- 'Grey'
    aid$Colors_Data[is.na(aid$Pheno_Pop)] <- 'Grey'
    aid$Colors_Region[is.na(aid$Pheno_Pop)] <- 'Grey'
    aid$Pheno_Pop[is.na(aid$Pheno_Pop)] <- 'Unspecified'
    
    aid <- aid[order(aid$Order, aid$Pheno_Pop, aid$Id), ]
    aid <- aid[nrow(aid):1, ]
    RedBl <- c("#2400D9", "#191DF7", "#2957FF", "#3D87FF", "#57B0FF", "#75D3FF", "#99EBFF", "#BDF9FF", "#EBFFFF", "#FFFFEB", "#FFF2BD", "#FFD699", "#FFAC75", 
        "#FF7857", "#FF3D3D", "#F72836", "#D91630", "#A60021")
    RedBl <- rev(RedBl)
    RedBlGr <- c(RedBl[1:9], gray.colors(100 - length(RedBl)), RedBl[10:18])
    BlBrewer <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58", "black")
    colImage <- switch(colorPlot, BlBrewer = BlBrewer, RedBl = RedBl, BlBrewer = BlBrewer, RedBlGr)
    plotI <- aid[grep(paste("^", columnPlot, "_", sep = ""), names(aid), value = T)]
    dimnames(plotI)[[1]] <- aid$Id

    if(legData==FALSE){
        for(z in 1:length(dimnames(plotI)[[2]])){
                sprf <- strsplit(dimnames(plotI)[[2]][z],'\\.')
                sprf[[1]] <- sprf[[1]][-length(sprf[[1]])]
                dimnames(plotI)[[2]][z] <- paste(sprf[[1]],collapse='.')
                
                
            }
    
    }
    
    if (quantilePlot) {
        breaks.in <- quantile(unlist(plotI), probs = seq(0, 1, length.out = (length(colImage) + 1)))
        breaks.in[1] <- breaks.in[1] - 1
        breaks.in[length(breaks.in)] <- breaks.in[length(breaks.in)] + 1
    }
    if (identical(columnPlot, "I")) {
        breaks.in <- c(seq(0, 95, length.out = length(colImage) - 1), 99.9, 101)
        breaks.in[1] <- -1
    }
    matPlot <- rep(0, 18)
    matPlot[7] <- 1
    PhenoCols <- grep("Pheno_", names(aid), value = T)
    if (legPheno) 
        #matPlot[8:10] <- switch(length(PhenoCols) + 1, rep(0, 3), c(2, 0, 0), c(2, 3, 0), 2:4)
        matPlot[8:10] <- switch(length(PhenoCols)+1, rep(0, 3), c(2, 0, 0), c(2, 3, 0), 2:4)
    
    if(legColor==TRUE) matColor='yes'
    if(legColor==FALSE) matColor='no'
    if(legRef==TRUE) matRef='yes'
    if(legRef==FALSE) matRef='no'

    
    matPlot[1] <- switch(matColor, yes = max(matPlot) + 1, 0)
    matPlot[13] <- switch(matRef, yes = max(matPlot) + 1, 0)
    matLayout <- matrix(matPlot, ncol = 6, byrow = T)
    widths.mat <- c(7, rep(0, 4), 0.5)
    if(legPheno==FALSE) widths.mat[1] <- 8.2
    if (matLayout[2, 2] != 0) 
        widths.mat[2] <- 0.2
    if (matLayout[2, 3] != 0) 
        widths.mat[3] <- 0.2
    if (matLayout[2, 4] != 0) 
        widths.mat[4] <- 0.2
    if (legAxisPop) 
        widths.mat[5] <- 0.6
    heights.mat <- c(0, 6, 0)
    if (matLayout[1, 1] != 0) 
        heights.mat[1] <- 0.5
    if (matLayout[3, 1] != 0) 
        heights.mat[3] <- 0.5
    layout(matLayout, widths = widths.mat, heights = heights.mat)
    mar1 <- c(5, 3, 3, 0)
    if (plotIndNames) 
        mar1[2] <- 8
    
    omav <- c(0,0,0,0)
    if(!(missing(bmar))) omav[1] <- bmar
    if(!(missing(lmar))) omav[2] <- lmar
    if(!(missing(tmar))) omav[3] <- tmar
    if(!(missing(rmar))) omav[4] <- rmar
    
    #layout.show()
    par(oma = omav)
    par(mar = mar1)
    if (quantilePlot == TRUE | columnPlot == "I") 
        image(x = 1:(ncol(plotI) + 1), y = 1:(nrow(plotI) + 1), t(plotI), col = colImage, axes = F, ann = F, breaks = breaks.in)
    if (columnPlot == "C" & quantilePlot == FALSE) 
        image(x = 1:(ncol(plotI) + 1), y = 1:(nrow(plotI) + 1), t(plotI), col = colImage, axes = F, ann = F)
    
         
    if(legRef) axis(1, at = seq(ncol(plotI)) + 0.5, labels = gsub("C_|I_", "", names(plotI)), cex.axis = T, las = 2, cex.axis = cexref)
    if (plotIndNames) 
        axis(2, at = seq(nrow(plotI)) + 0.5, labels = dimnames(plotI)[[1]], las = 2, cex.axis = cexind)
    colSepPop <- "black"
    if (identical(grep("Pheno_Pop", names(aid), value = T), "Pheno_Pop")) {
        sep.pop <- which(!duplicated(aid$Pheno_Pop))
        if (sepLinesPop)
            # Controls width of popsepline
            abline(h = sep.pop, col = colSepPop, lwd = 0.3)
    }
    sep.pop <- which(!duplicated(aid$Pheno_Pop))
    #print(sep.pop)
    #if (identical(sepLinesPop, TRUE)) abline(h = sep.pop, col = colSepPop, lwd = 2.0)
            # Controls width of popsepline
           
    
    #sepLinesHGDP <- c(6, 10, 18, 25, 44, 49)
    sepLinesHGDP <- c(5, 9, 16, 24, 38, 40)
    if (sepLineRef) 
        abline(v = sepLinesHGDP + 1, col = "black")
    mar2 <- mar1
    mar2[c(2, 4)] <- 0
    par(mar = mar2)
    cexBarPlot <- 0.7
    colPheno <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
    PhenoCol <- grep("Pheno_", names(aid), value = T)
    for (i in PhenoCol) {
        col.i <- gsub("Pheno_", "Colors_", i)
        if (!identical(grep(col.i, names(aid), value = T), col.i)) {
            aid[, col.i] <- as.factor(aid[, i])
            levels(aid[, col.i]) <- rep(colPheno, length(levels(aid[, col.i])))[1:length(levels(aid[, col.i]))]
            aid[, col.i] <- as.character(aid[, col.i])
        }
    }
    
    
    
    if(legPheno){
      if (identical(grep("Pheno_Data", names(aid), value = T), "Pheno_Data")) {
          barplot(rep(1, nrow(aid)), col = aid[, "Colors_Data"], beside = F, border = NA, space = 0, horiz = T, axes = F, yaxs = "i")
          if (sepLinesPop) abline(h = sep.pop - 1, col = colSepPop, lwd = 0.5)
          axis(1, 0.5, "Data", cex.axis = cexBarPlot, las = 2, xpd = NA)
        
      }
      
      if (identical(grep("Pheno_Region", names(aid), value = T), "Pheno_Region")) {
          barplot(rep(1, nrow(aid)), col = aid[, "Colors_Region"], beside = F, border = NA, space = 0, horiz = T, axes = F, yaxs = "i")
          if (sepLinesPop) abline(h = sep.pop - 1, col = colSepPop, lwd = 0.5)
          axis(1, 0.5, "Region", cex.axis = cexBarPlot, las = 2, , xpd = NA)
        
      }
      if (identical(grep("Pheno_Pop", names(aid), value = T), "Pheno_Pop")) {
        barplot(rep(1, nrow(aid)), col = aid[, "Colors_Pop"], beside = F, border = NA, space = 0, horiz = T, axes = F, yaxs = "i")
        if (sepLinesPop) abline(h = sep.pop - 1, col = colSepPop, lwd = 0.5)
        axis(1, 0.5, "Pop", cex.axis = cexBarPlot, las = 2, xpd = NA)
        
      }
    }
    
    
    
    if(legAxisPop==TRUE && legPheno==TRUE){    
      
      if (identical(grep("Pheno_Pop", names(aid), value = T), "Pheno_Pop")) {
          phenoLeg <- data.frame(Pheno_Pop = aid$Pheno_Pop[order(aid$Order,aid$Pheno_Pop)], Index = seq(from=nrow(aid),to=1))
          phenfrq <- as.data.frame(table(phenoLeg$Pheno_Pop))
          phenoLeg <- phenoLeg[!duplicated(phenoLeg$Pheno_Pop) | !duplicated(phenoLeg$Pheno_Pop, fromLast = T), ]
        
          if(1%in%phenfrq$Freq){
              for(z in phenfrq[phenfrq[,2]==1,1]){
                  tempduo <- rbind(phenoLeg[phenoLeg$Pheno_Pop==z,],phenoLeg[phenoLeg$Pheno_Pop==z,])
                  tempduo$Index[1] <- tempduo$Index[1]-0.75
                  tempduo$Index[2] <- tempduo$Index[2]-0.25
                  phenoLeg <- rbind(phenoLeg[phenoLeg$Pheno_Pop!=z,],tempduo)
              } 
            
          }
          phenoLeg$time <- 1:2
          phenoLeg <- reshape(phenoLeg, idvar = "Pheno_Pop", v.names = "Index", timevar = "time", direction = "wide")
          phenoLeg$loc <- (phenoLeg$Index.1 + phenoLeg$Index.2)/2
          phenfrq <- phenfrq[match(phenoLeg$Pheno_Pop,phenfrq[,1]),]
          phenfrq[,1] <- paste0(phenfrq[,1],' (',phenfrq[,2],')')
          #axis(4, at = phenoLeg$loc, labels = phenoLeg$Pheno_Pop, xpd = NA, las = 2, cex.axis = 0.9)
          axis(4, at = phenoLeg$loc, labels = phenfrq[,1], xpd = NA, las = 2, cex.axis = 0.9)
      }
    }
    
    if(legColor){
      mar3 <- mar1
      mar3[c(1, 3)] <- 0
      par(mar = mar3)
      plot(1, 1, t = "n", axes = F, ann = F, xlim = c(0, length(colImage) * 3), xaxs = "i")
      x1 <- seq(length(colImage))
      #rect(x1, 0.7, x1 + 1, 0.9, col = colImage, xpd = NA)
      rect(x1, 0.5, x1 + 1, 0.8, col = colImage, xpd = NA)
      text(x1[1], 0.3, "low", xpd = NA)
      text(x1[length(x1)]+1, 0.3, "high", xpd = NA)
      popTextDown <- c(sepLinesHGDP, 45)
      popText <- c("Sub-Saharan Africa", "MENA", "Europe", "C S Asia", "East Asia", "Oce", "Amer")
      col.text <- "black"
      par2 <- mar1
      par2[c(1, 3)] <- 0
      #plot(1, 1, xlim = c(0, 51), ylim = c(1, 100), xaxs = "i", axes = F, ann = F, t = "n")
      sepPopLine <- 0.5
      lwd.i <- 2
      #y.seg.pos <- 100
      #y.text.pos <- 70
      y.seg.pos <- 55
      y.text.pos <- 25
      cex.i <- 1
      x1 <- c(sepPopLine, popTextDown[-length(popTextDown)] + sepPopLine)
      x2 <- c(popTextDown - sepPopLine)
    }
    
    #segments(x1, y.seg.pos, x2, y.seg.pos, col = col.text, lwd = lwd.i)
    #textAll <- ((x1 + x2)/2)
    #text(textAll, y.text.pos, popText, cex = cex.i, col = col.text)
}
