#Read metabolomics datafile
read.met <- function(data,headrow=3,metidcol=1,fvalue=8,sep=",",
                     ppkey='PPP',ippkey='BPP',sidkey='none'){
  #Get number of columns in file
  colnum <- count.fields(data,sep=sep)[1]
  #Scan the data and make into a matrix with number of columns equal to colnum
  all <- matrix(scan(data,'character',skip=headrow,sep=",",
                     na.strings=c("NA")),ncol=as.numeric(colnum),byrow=TRUE)
  #Get header of the file. Append X to front of columns with no prefix to denote samples
  p<- read.table(data,nrows=headrow,sep=sep)
  header = c()
  for (i in p[headrow,]){
    if (sidkey!='none'){
      header<-c(header,as.character(i))
    }
    if (sidkey=='none'){
      if((regexpr(paste("^",ppkey,sep=''),i)!=1)&(regexpr(paste("^",ippkey,sep=''),i)!=1)){
        header<-c(header,paste("X",as.character(i),sep=''))}
      else{header<-c(header,as.character(i))}
    }
  }
  #Name columns by edited header
  colnames(all)<-header
  #Name rows by the metaboite column
  rownames(all)<-all[,metidcol]
  #Subset the matrix to include only data (removing extra columns from front)
  all <- all[,fvalue:dim(all)[2]]
  #Make matrix into a numeric matrix
  class(all) <- "numeric"
  #Print the number of metabolites read and the number of samples of each type found
  cat(paste(dim(all)[1]," Metabolites Read\n",sep=''))
  cat(paste(length(which(regexpr(paste("^",ppkey,sep=''),colnames(all))!=-1))," Sample Pooled Plasma Columns Read\n",sep=''))
  cat(paste(length(which(regexpr(paste("^",ippkey,sep=''),colnames(all))!=-1))," Standard Pooled Plasma Columns Read\n",sep=''))
  if (sidkey!='none'){
    cat(paste(length(which(regexpr(paste("^",sidkey,sep=''),colnames(all))!=-1))," Sample Columns Read\n",sep=''))
  }
  if (sidkey=='none') {
    cat(paste(length(which(regexpr("^X",colnames(all))!=-1))," Sample Columns Read\n",sep=''))
  }
  cat(paste(dim(all)[2]," Total Columns\n",sep=""))
  #Return the labeled matrix
  return(all)
}

#Write the retained metabolites to a file that is formatted
#the same as the original file
write.met <- function(res,filename,origfile,headrow=3,metidcol=1,fvalue=8,
                      sep=",",type='keep'
                     ){
  #Get number od columns in the input file
  colnum <- count.fields(origfile,sep=sep)[1]
  #Get the header rows from the input file
  headers <- matrix(scan(origfile,'character',sep=sep,
                         na.strings=c("NA"),nlines=headrow),ncol=colnum,byrow=T)
  
  #Read in just first columns that are not data based on fvalue
  cc <- rep("NULL", colnum)
  cc[1:fvalue-1] <- NA
  col.leads <- read.table(origfile,colClasses=cc,sep=sep,skip=headrow)
  rownames(col.leads) <- col.leads[,metidcol]
  #Merge row labels with the metabolites of interest
  col.data <- merge(col.leads[which(rownames(col.leads) %in% rownames(res[[type]])),],res[[type]],by='row.names',all.x=TRUE)
  col.data <- col.data[,-which(names(col.data) %in% c("Row.names"))]
  #combine the headers with the row labels to get full file
  outdata <- rbind(headers,as.matrix(col.data))
  #Print out some processing notes
  cat(paste(dim(outdata)[2]-fvalue,' Samples Written\n',sep=''))
  cat(paste(dim(outdata)[1]-headrow,' Metabolites Written\n',sep=''))
  #Write the table to an output file
  write.table(outdata,filename,row.names=F,
              col.names=F,quote=F,sep=sep)
}

#QUALITY OF METABOLITE CALCULATORS
#Get the indices of pooled plasma columns versus sample columns
get_group <- function(df,ppkey='PPP',sidkey='X'){
  #Initiate empty list
  res = list() 
  cname = colnames(df) #get the column names
  res[['pp']] = which(regexpr(paste("^",ppkey,sep=''),cname)!=-1) #Get the index of columns that contain pooled plasma
  res[['sid']] = which(regexpr(paste("^",sidkey,sep=''),cname)!=-1) #Get the index of columns that contain samples
  #Error message if no pooled plasma or sample columns by identifier provided
  if( length(res[['pp']])==0 ) stop(paste(ppkey,' is not a column identifier: no pooled plasma columns detected'))
  if( length(res[['sid']])==0 ) stop(paste(sidkey,' is not a column identifier: no sample columns detected'))
  return(res)
}

#Get missing rate of pooled plasma samples and samples
get_missing <- function(df,ppind,sampind){
  ppp = df[,ppind] #Subset to just the pooled plasma
  samps = df[,sampind] #Subest to just the samples
  out=list()
  out[['ppmiss']] = apply(ppp,1,function(x) sum(is.na(x))/length(x)) #Get missing rate of pooled plasma for all metabolites
  out[['sampmiss']] = apply(samps,1,function(x) sum(is.na(x))/length(x)) #Get missing rate of samples for all metabolites
  return(out)
}

subset_met <- function(df,miss,numsplit=5,mincut=0.02,maxcut=0.95){
  if( maxcut<mincut ) stop('Minimum missing rate is larger than maximum missing rate') 
  subsetsout = list()
  bins = seq(mincut,maxcut,((maxcut-mincut)/(numsplit))) #Split data into bins based on pooled plasma missing rate
  bins = c(mincut,bins[-1]) #Make sure minimum value across bins line up to function argument
  bins = c(bins[-length(bins)],maxcut) #Make sure maximum value across bins line up to function argument
  #Loop through all the bins and store that smaller dataframe in a list
  for (i in 1:numsplit){
    subsetsout[[i]]=df[which(miss>bins[i]&miss<=bins[i+1]),,drop=FALSE]
    if (dim(subsetsout[[i]])[1]==0){
      subsetsout[i]<-list(NULL)
      warning(paste('Split ',i,' is empty; removing this split from further analysis. Try changing max and min cutoffs or reducing number of splits'))
    }
  }
  return(subsetsout)
}

corr_metric <- function(df,grps){
  sblocks = list()
  pblocks = list()
  counter = 0
  #For each block consisting of samples and flanking pooled plasma
  for (i in 1:(length(grps[['pp']])-1)){
    counter = counter + 1
    #Get missing rate of samples for each block
    sampid = grps[['sid']][grps[['sid']]>grps[['pp']][i]&grps[['sid']]<grps[['pp']][i+1]]
    df_s = df[,sampid,drop=FALSE]
    sblocks[[counter]] = apply(df_s,1,function(x) sum(is.na(x))/length(x))
    #Get mssing rate of pooled plasma for each block (either 0, .5, or 1)
    ppid = c(grps[['pp']][i],grps[['pp']][i+1])
    df_p = df[,ppid,drop=FALSE]
    pblocks[[counter]] = apply(df_p,1,function(x) sum(is.na(x))/length(x))
  }
  #Make a dataframe
  s_mat = do.call(cbind, sblocks)
  p_mat = do.call(cbind, pblocks)
  #Get correlation between the pooled plasma and sample missing rates
  #Idea borrowed from: http://stackoverflow.com/questions/9136116/r-correlation-between-2-dataframes-by-row
  corrs = sapply(seq.int(dim(s_mat)[1]), function(i) cor(s_mat[i,], p_mat[i,]))
  return(corrs)
}


run_metric <- function(df,grps,scut=.5){
  types = list()
  counter = 0
  #For each block consisting of preceding pooled plasma and subsequent samples
  for (i in 1:(length(grps[['pp']])-1)){
    counter = counter + 1
    #Get missing rate of the samples
    sampid = grps[['sid']][grps[['sid']]>grps[['pp']][i]&grps[['sid']]<grps[['pp']][i+1]]
    df_s = df[,sampid,drop=FALSE]
    bl_sam_missrate = apply(df_s,1,function(x) sum(is.na(x))/length(x))
    #If missing rate is less than cutoff, then mark data as present
    bl_sam_missrate = ifelse(bl_sam_missrate >= scut, 0, 1)
    #Determine if preceding pooled plasma is present
    ppid = c(grps[['pp']][i])
    df_p = df[,ppid,drop=FALSE]
    df_p[!(is.na(df_p))] = 1
    df_p[which(is.na(df_p))] = 0
    #Store 'type' of block based on whether data is present in both the preceding pooled plasma and following samples
    types[[counter]] = apply(cbind(df_p,bl_sam_missrate),1,sum)
  }
  #Find the longest run of data being present
  fullframe = do.call(cbind, types)
  #Define a function to get the longest run
  maxrun <- function(x){
    run = rle(x)$lengths[rle(x)$values==2]
    #If there are no blocks of prsent data return NA
    if (all(is.na(run))){
      return(0)
    }
    #Else return the longest run of present data
    else{
      out = max(run)
      return(out)
    }
  }
  #Apply the function to the data
  longestrun = apply(fullframe,1,maxrun)
  #Return a vector of the longest runs
  return(as.numeric(longestrun))
}

met_proc <- function(df, #Dataframe to of metabolites levels
                     numsplit=5, #Number of splits based on PPP missing rate
                     cor_rates = c(.6,.65,.65,.65,.6), #Correlation cutoffs per subgroup
                     runlengths = c(NA,15,15,15,NA), #Run length
                     mincut=0.02, #Keep metabolites with PPP missing below this
                     maxcut=.95, #Remove metabolites with PPP missing above this
                     scut=.5, #Threshold of missing rate for calling a sample block 'present' (<scut = present, >scut = missing)
                     ppkey='PPP', #Unique prefix for the pooled plasma columns
                     sidkey='X', #Unique prefix to all sample column headers
                     missratecut=0.01, #The minimum missing rate to plot for removed samples
                     histcolors=c('white'), #colors of the histogram plot
                     plot=TRUE, #Whether to include plots at all
                     outfile='MetProc_output' #Name of the resulting image file in plot=TRUE
                     ){
  #Error messages making sure that cutoffs specified are same length as
  #the number of splits
  if( numsplit!= length(cor_rates) ) stop('Length of cor_rates vector not equal to number of splits') 
  if( numsplit!= length(runlengths) ) stop('Length of runlengths vector not equal to number of splits') 
  out = list()
  groups = get_group(df,ppkey,sidkey) #Get the pooled plasma and the sample column indices
  missrate = get_missing(df,groups[['pp']],groups[['sid']]) #Get missing rate of both the pooled plasma and samples for each metabolite
  goods = rownames(df[which(missrate[['ppmiss']]<=mincut),]) #Remove > maxcut missing PPP rate
  bads = rownames(df[which(missrate[['ppmiss']]>maxcut),]) #Keep < mincut missing PPP rate
  
  #Check to see if all metabolites fall below minimum cutpoint or
  #above the maximum cutpoints -- if so, no need to do any splitting
  if (length(goods) + length(bads) == dim(df)[1]){
    cat("All metabolites either below minimum or above maximum cutpoints -- no metrics calculated\n")
    out[['keep']] = df[which(rownames(df) %in% goods),]
    out[['remove']] = df[which(rownames(df) %in% bads),]
    #If plot is TRUE, make the plots specified
    if (plot==TRUE){
      pdf(paste(outfile,'.pdf',sep=''))
      plot_pp_sample_missing(df,ppkey,sidkey)
      heatmap_res(out[['remove']],ppkey=ppkey,sidkey=sidkey,missratecut=missratecut,title='Removed Metabolites')
      heatmap_res(out[['keep']],ppkey=ppkey,sidkey=sidkey,missratecut=missratecut,title='Retained Metabolites')
      dev.off()
    }
    cat(paste(dim(out[['keep']])[1],' Metabolites Retained\n',sep=''))
    cat(paste(dim(out[['remove']])[1],' Metabolites Removed\n',sep=''))
    return(out)
  }
  
  #If there are metabolites with missing rates between the cutoffs
  #use the splitting process to determine which to keep
  subsets = subset_met(df,missrate[['ppmiss']],numsplit=numsplit,mincut=mincut,maxcut=maxcut) #Subset to metabolites in between mincut and maxcut
  #For each group based on the number of splits to apply using pooled plasma missing rate
  for (i in 1:numsplit){
    if (is.null(subsets[[i]])){
      next
    }
    #Get correlation and longest run statistic for all metabolites in that group
    corrstats = corr_metric(subsets[[i]],groups)
    runstats = run_metric(subsets[[i]],groups,scut)
    #Identify good and bad metabolites
    if (is.na(runlengths[i])){
      runcut = max(runstats,na.rm=T)+1
    }
    else {
      runcut = runlengths[i]
    }
    if (is.na(cor_rates[i])){
      corrcut = 1
    }
    else {
      corrcut = cor_rates[i]
    }
    g_keep = rownames(subsets[[i]])[corrstats<corrcut&runstats<runcut]
    b_remove = rownames(subsets[[i]])[corrstats>=corrcut|runstats>=runcut]
    #Get a list of the good and bad metabolites
    goods = c(goods,g_keep)
    bads = c(bads,b_remove)
  }
  #Return a list with one element for good metabolites and one for bad
  out[['keep']] = df[which(rownames(df) %in% goods),]
  out[['remove']] = df[which(rownames(df) %in% bads),]
  if (plot==TRUE){
    pdf(paste(outfile,'.pdf',sep=''))
    plot_pp_sample_missing(df,ppkey,sidkey)
    plot_metric(df,ppkey,sidkey,numsplit,mincut,maxcut,scut,cor_rates,runlengths,histcolors)
    heatmap_res(out[['remove']],ppkey=ppkey,sidkey=sidkey,missratecut=missratecut,title='Removed Metabolites')
    heatmap_res(out[['keep']],ppkey=ppkey,sidkey=sidkey,missratecut=missratecut,title='Retained Metabolites')
    dev.off()
  }
  cat(paste(dim(out[['keep']])[1],' Metabolites Retained\n',sep=''))
  cat(paste(dim(out[['remove']])[1],' Metabolites Removed\n',sep=''))
  return(out)
}

##### PLOT GENERATORS
plot_pp_sample_missing <- function(df, #Dataframe to of metabolites levels
                     ppkey='PPP', #Unique prefix for the pooled plasma columns
                     sidkey='X' #Unique prefix to all sample column headers
){
  #store current par
  oldpar = par(no.readonly = T)
  #set par
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0,oma=c(0,0,0,0))
  #get indicies of pooled plasma versus samples
  groups = get_group(df,ppkey,sidkey)
  #Get missing rate for pooled plasma and samples
  missrate = get_missing(df,groups[['pp']],groups[['sid']])
  #Plot the missing rates against one another
  plot(missrate[['sampmiss']],missrate[['ppmiss']],pch=18,
                 col=adjustcolor('blue',.5),xlab='Sample Missing Rate',
                 ylab='Pooled Plasma Missing Rate',
                 main='Pooled Plamsa Missing Rate v. Sample Missing Rate')
  abline(0,1)
  #set par to original par
  par(oldpar)
}

plot_metric <- function(df,ppkey='PPP',sidkey='X',numsplit=5,
                        mincut=.02,maxcut=0.95,scut=0.5,cor_rates=c(.6,.65,.65,.65,.6),
                        runlengths=c(NA,15,15,15,NA),histcolors=c('white')){
  #Get indicies of pooled plasma versus samples
  groups = get_group(df,ppkey,sidkey)
  #Get missing rate of pooled plasma and missing rate of samples
  missrate = get_missing(df,groups[['pp']],groups[['sid']])
  #Subset the metabolites into numsplit groups based on pooled plasma missing rate
  subsets = subset_met(df,missrate[['ppmiss']],numsplit,mincut,maxcut)
  #Initiate correlation and longest run stats
  corrstats = list()
  runstats = list()
  corrmin = 0
  corrmax = 0
  runmin = 0
  runmax = 0
  #Find the maximum and minimum stats for setting plot limits
  for (i in 1:numsplit){
    if (is.null(subsets[[i]])){
      next
    }
    corrstats[[i]] = corr_metric(subsets[[i]],groups)
    runstats[[i]] = run_metric(subsets[[i]],groups,scut)
    corrmin = min(corrmin,min(corrstats[[i]],na.rm=T))
    corrmax = max(corrmax,max(corrstats[[i]],na.rm=T))
    runmin = min(runmin,min(runstats[[i]]))
    runmax = max(runmax,max(runstats[[i]]))
  }
  #Round to lowest integer
  corrmax = ceiling(corrmax*10)/10
  corrmin = floor(corrmin*10)/10
  runmax = ceiling(runmax*10)/10
  runmin = floor(runmin*10)/10
  
  #store current par
  oldpar = par(no.readonly = T)
  #Intitiate a plot with numsplit*2 graphs
  par(mfrow=c(numsplit,2),mar=c(3,3,1,1),oma = c(0, 0, 4, 0))
  #For each group of metabolite based on pooled plasma missing rate
  #plot the distribution of the correlation and longest run metric
  for (i in 1:numsplit){
    if (is.null(subsets[[i]])){
      hist(0,col='white',xlab='',ylab='',
           main=paste('Group ',i,': Correlation Metric Distribution',sep=''),
           cex.main=.8,cex.axis=.6)
      text(-.5,.5,'No Metabolites')
      title(xlab="Correlation Metric", mgp=c(1.9,.2,0),cex.lab=.6)
      title(ylab="# of Metabolites", mgp=c(1.9,.2,0),cex.lab=.6)
      hist(0,col='white',ylab='',xlab='',
           main=paste('Group ',i,': Longest Run Metric Distribution',sep=''),
           cex.main=.8,cex.axis=.6)
      text(-.5,.5,'No Metabolites')
      title(xlab="Correlation Metric", mgp=c(1.9,.2,0),cex.lab=.6)
      title(ylab="# of Metabolites", mgp=c(1.9,.2,0),cex.lab=.6)
      next
    }
    if (!(is.na(cor_rates[[i]]))){
    one = hist(corrstats[[i]][corrstats[[i]]>=cor_rates[i]],breaks=seq(corrmin,corrmax,0.05),
               right=FALSE,plot=FALSE)
    two = hist(corrstats[[i]][corrstats[[i]]<cor_rates[i]],breaks=seq(corrmin,corrmax,0.05),
               right=FALSE,plot=FALSE)
    plot(one,col=adjustcolor(histcolors[i],.5),
         xlab='',ylab='',main=paste('Group ',i,': Correlation Metric Distribution',sep=''),
         xlim=c(corrmin,corrmax),cex.main=.8,
         cex.axis=.6,ylim=c(0,max(c(one$counts,two$counts))))
    plot(two,col=adjustcolor('black',.4),
         xlim=c(corrmin,corrmax),
         add=T,ylim=c(0,max(c(one$counts,two$counts))))
    title(xlab="Correlation Metric", mgp=c(1.9,.2,0),cex.lab=.6)
    title(ylab="# of Metabolites", mgp=c(1.9,.2,0),cex.lab=.6)
    abline(v=cor_rates[i],col='red',lwd=3)
    }
    else {
      hist(corrstats[[i]],col=adjustcolor('black',.4),
           xlab='',ylab='',main=paste('Group ',i,': Correlation Metric Distribution',sep=''),
           xlim=c(corrmin,corrmax),breaks=seq(corrmin,corrmax,0.05),right=FALSE,cex.main=.8,
           cex.axis=.6)
      title(xlab="Correlation Metric", mgp=c(1.9,.2,0),cex.lab=.6)
      title(ylab="# of Metabolites", mgp=c(1.9,.2,0),cex.lab=.6)
      abline(v=cor_rates[i],col='red',lwd=3)
    }
    if (!(is.na(runlengths[[i]]))){
    one = hist(runstats[[i]][runstats[[i]]>=runlengths[i]],breaks=seq(runmin,runmax+5,5),
               right=FALSE,plot=FALSE)
    two = hist(runstats[[i]][runstats[[i]]<runlengths[i]],breaks=seq(runmin,runmax+5,5),
               right=FALSE,plot=FALSE)
    plot(one,ylim=c(0,max(c(one$counts,two$counts))),col=adjustcolor(histcolors[i],.5),
         xlab='',ylab='',main=paste('Group ',i,': Longest Run Metric Distribution',sep=''),
         xlim=c(runmin,runmax),cex.main=.8,cex.axis=.6)
    plot(two,ylim=c(0,max(c(one$counts,two$counts))),col=adjustcolor('black',.4),
         xlim=c(runmin,runmax),add=T)
    title(xlab="Longest Run Metric", mgp=c(1.9,.2,0),cex.lab=.6)
    title(ylab="# of Metabolites", mgp=c(1.9,.2,0),cex.lab=.6)
    abline(v=runlengths[i],col='red',lwd=3)
    }
    else {
      hist(runstats[[i]],col=adjustcolor('black',.4),
           xlab='',ylab='',main=paste('Group ',i,': Longest Run Metric Distribution',sep=''),
           xlim=c(runmin,runmax),breaks=seq(runmin,runmax+5,5),right=FALSE,cex.main=.8,
           cex.axis=.6)
      title(xlab="Longest Run Metric", mgp=c(1.9,.2,0),cex.lab=.6)
      title(ylab="# of Metabolites", mgp=c(1.9,.2,0),cex.lab=.6)
      abline(v=runlengths[i],col='red',lwd=3)      
    }
  }
  mtext("Distribution of Metrics:\nGrey Metabolites are below threshold", outer = TRUE, cex = 1)
  #Rest to old par
  par(oldpar)
}

heatmap_res <- function(df,ppkey='PPP',sidkey='X',missratecut=0.01,title){
  #check the missratecut value to make sure it's between 0 and 1
  if( missratecut<0 | missratecut>1 ) stop('missratecut must be between 0 and 1 for plotting heatmap') 
  #Get indicies of pooled plasma versus samples
  groups = get_group(df,ppkey,sidkey)
  #Subset the metabolomics data to just the pooled plasma and samples sorted
  #by injection order
  mat_data = df[,sort(c(groups[['pp']],groups[['sid']]))]
  #Replace present data with 1 and missing data with 0
  mat_data[!(is.na(mat_data))] = 1
  mat_data[is.na(mat_data)] = 0
  #Set a color palette of white/grey
  my_palette <- colorRampPalette(c("white", "gray20"))(n = 5)
  #Convert data to matrix
  mat_data = as.matrix(mat_data)
  #Get missing rate of each metabolite
  missrate = apply(mat_data,1,function(x) 1-(sum(x)/length(x)))
  #Subset to metabolites with missing rate greater than provided cutoff
  mat_data = mat_data[missrate>=missratecut,,drop=FALSE]
  #Intiate the structure of the heatmap object to place relevant components
  #in correct spots
  lmat = rbind(4:3,2:1)
  
  #store current par
  oldpar = par(no.readonly = T)
  #Create new par
  par(mar=c(5.1, 6.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0,oma=c(0,0,0,0))
  #Create the heatmap if more than two metabolites
  if (dim(mat_data)[1] >= 2){    
    gplots::heatmap.2(mat_data,main =paste(title,"\n(of metabolites with missing rate >= ",missratecut,")",sep=""),notecol="black",density.info="none",trace="none",         
                      margins =c(1,5),col=my_palette,dendrogram="none",Colv="NA",labRow=FALSE,
                      labCol=FALSE,key=FALSE,lmat=lmat,lhei = c(0.2,1.2),lwid = c(.4,2.3),useRaster=TRUE,
                      hclustfun = fastcluster::hclust)
    #Add a legend
    legend(-.2,.6,c('Missing','Present'),col = c('black'),
         pt.bg=c('White','Black'),pch=22,cex=.8,xpd=TRUE,y.intersp=1)
  }
  else{
    cat('No metabolites to plot. Consider changing missratecut or altering thresholds\n')
  }
  #convert to old par
  par(oldpar)
}



