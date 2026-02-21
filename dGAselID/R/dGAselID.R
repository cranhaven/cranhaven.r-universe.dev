####################################################################
#	InitialPopulation
####################################################################
#
#' InitialPopulation
#'
#' Generates an initial randomly generated population of haploid genotypes.
#' @aliases InitialPopulation
#' @param x Dataset in ExpressionSet format.
#' @param populationSize Number of genotypes in initial population.
#' @param startGenes Genes in the genotypes at initialization.
#' @param EveryGeneInInitialPopulation Request for every gene to be present in the initial population. The default value is TRUE.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' population01<-InitialPopulation(demoALL, 4, 4)
#' population02<-InitialPopulation(demoALL, 20, 4, FALSE)
#'  }
#' @export

InitialPopulation <- function(x, populationSize, startGenes, EveryGeneInInitialPopulation=TRUE) {
  cat(paste("\nGenerating the initial population...\n"))
  genomeLength=length(featureNames(x));
  cat("Generating random chromosomes...\n");
  populationInit = matrix(0, nrow=populationSize, ncol=genomeLength);

  if (EveryGeneInInitialPopulation==TRUE){
    noDGenes=ceiling(genomeLength/populationSize);
    index1<-sample(1:genomeLength, genomeLength, replace=FALSE)

    if (genomeLength%%populationSize==0){
      indexI<-matrix(index1, nrow=populationSize)
      cat(paste("\nNo rest...\n"))
    } else {
      cat(paste("\nWith rest...\n"))
      index2<-sample(setdiff(1:genomeLength,tail(index1, startGenes+1)), abs(populationSize*noDGenes-genomeLength), replace=FALSE)
      indexI<-matrix(c(index1,index2), nrow=populationSize, byrow = TRUE)
    }

    for (i in 1:populationSize) {
      for(j in 1:noDGenes){
        populationInit[i, indexI[i, j]] = 1;
      }
    }

    if (startGenes>noDGenes){
      for (i in 1:populationSize) {
        populationInit[i, sample(setdiff(1:genomeLength,indexI[i,]), startGenes-noDGenes, replace=FALSE)] = 1;
      }
    }
  } else {
    for (i in 1:populationSize) {
      populationInit[i, sample(1:genomeLength, startGenes, replace=FALSE)] = 1;
    }
  }

  colnames(populationInit)=featureNames(x);
  rownames(populationInit)=1:populationSize;
  return(populationInit);
}
####################################################################
#	Individuals
####################################################################
#
#' Individuals
#'
#' Generates individuals with diploid genotypes.
#' @aliases Individuals
#' @param population Population of haploid genotypes.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' population02<-InitialPopulation(demoALL, 20, 4, FALSE)
#' individuals02<-Individuals(population02)
#'  }
#' @export
#
Individuals<-function(population){
  cat(paste("\tGenerating Individuals...\n"))
  if(nrow(population)%%2==1){
    population<-rbind(population, population[1,])
    rownames(population)<-c(1:nrow(population))
  }

  noIndividuals<-nrow(population)
  Id<-rep(1:(nrow(population)/2), each=2)
  population<-cbind(Id, population)
}

####################################################################
#	Split Chromosomes
####################################################################
#
#' splitChromosomes
#'
#' Divides the genotypes into sets with a desired number of chromosomes.
#' @aliases splitChromosomes
#' @param x Dataset in ExpressionSet format.
#' @param noChr Desired number of chromosomes. The default value is 22.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' splitChromosomes(demoALL, 3)
#' splitChromosomes(demoALL)
#'
#'  }
#' @export
#
splitChromosomes <- function(x, noChr=22) {

  noGenes<-matrix(c(1, 3000, 9.174312, 2, 2500, 7.645260, 3, 1900, 5.810398, 4, 1600, 4.892966, 5, 1700, 5.198777, 6, 1900, 5.810398, 7, 1800, 5.504587, 8, 1400, 4.281346, 9, 1400, 4.281346, 10, 1400, 4.281346, 11, 2000, 6.116208, 12, 1600, 4.892966, 13, 800, 2.446483, 14, 1200, 3.669725, 15, 1200, 3.669725, 16, 1300, 3.975535, 17, 1600, 4.892966, 18, 600, 1.834862, 19, 1700, 5.198777, 20, 900, 2.752294, 21, 400, 1.223242, 22, 800, 2.446483), nrow=22, ncol=3, byrow=TRUE)
  colnames(noGenes)<-c("Chromosome", "NoOfGenes",  "Percent")

  toSplit<-dim(exprs(x))[1];

  if (3*noChr>toSplit){
    cat(paste("\nToo many chromosomes for the given genome. Please specify a lower number of chromosomes...\n"));
    noChr<-toSplit%/%3
    cat(paste("\nAutomatically changing the number of chromosomes to "), noChr, paste(" ...\n"));
  }

  noGenes2<-noGenes[1:noChr,3];
  rm(noGenes);

  newPercent<-noGenes2/sum(noGenes2)*100;
  rm(noGenes2);

  chr<-as.integer(newPercent*toSplit/100);
  rm(newPercent);

  chr[noChr]<-chr[noChr]+(toSplit-sum(chr));
  rm(toSplit);
  rm(noChr);

  chrNumber<-0;
  chrConfig<-c();
  for (i in chr) {
    chrNumber<-chrNumber+1
    chrConfig<-c(chrConfig, rep(chrNumber,i));
  }
  rm(chrNumber);
  rm(chr);
  rm(i);
  return(chrConfig);
}

####################################################################
#	RandomizePop
####################################################################
#
#' RandomizePop
#'
#' Generates a random population for the next generation.
#' @aliases RandomizePop
#' @param population Population of chromosome sets in current generation.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' population01<-InitialPopulation(demoALL, 4, 4)
#' population01
#' RandomizePop(population01)
#'  }
#' @export
#
RandomizePop<-function(population){
  cat(paste("\tRandomizing the population...\n"))
  newIndex<-sample(1:dim(population)[1], replace=FALSE)
  newPopulation<-population[newIndex,]
  rownames(newPopulation)<-c(1:dim(newPopulation)[1])
  return(newPopulation)
}

####################################################################
#	Evaluate Population
####################################################################
#
#'  EvaluationFunction
#'
#' Evaluates the individuals' fitnesses.
#' @aliases EvaluationFunction
#' @param x Dataset in ExpressionSet format.
#' @param individuals Population of individuals with diploid genotypes.
#' @param response Response variable.
#' @param method Supervised classifier for fitness evaluation. Most of the supervised classifiers in MLInterfaces are acceptable. The default is knn.cvI(k=3, l=2).
#' @param trainTest Cross-validation method. The default is "LOG".
#' @param nnetSize for nnetI. The default value is NA.
#' @param nnetDecay for nnetI. The default value is NA.
#' @param rdaAlpha for rdaI. The default value is NA.
#' @param rdaDelta for rdaI. The default value is NA.
#' @param ... Additional arguments.
#' @examples
#' \dontrun{
#'  library(genefilter)
#'  library(ALL)
#'  data(ALL)
#'  bALL = ALL[, substr(ALL$BT,1,1) == "B"]
#'  smallALL = bALL[, bALL$mol.biol %in% c("BCR/ABL", "NEG")]
#'  smallALL$mol.biol = factor(smallALL$mol.biol)
#'  smallALL$BT = factor(smallALL$BT)
#'  f1 <- pOverA(0.25, log2(100))
#'  f2 <- function(x) (IQR(x) > 0.5)
#'  f3 <- ttest(smallALL$mol.biol, p=0.1)
#'  ff <- filterfun(f1, f2, f3)
#'  selectedsmallALL <- genefilter(exprs(smallALL), ff)
#'  smallALL = smallALL[selectedsmallALL, ]
#'  rm(f1)
#'  rm(f2)
#'  rm(f3)
#'  rm(ff)
#'  rm(bALL)
#'  sum(selectedsmallALL)
#'  set.seed(1357)
#'
#'  population0<-InitialPopulation(smallALL, 14, 8, FALSE)
#'  individuals0<-Individuals(population0)
#'  results<-EvaluationFunction(smallALL, individuals0, response="mol.biol",
#'              method=knn.cvI(k=3, l=2), trainTest="LOG")
#'  }
#' @export
#
EvaluationFunction <- function(x, individuals, response, method, trainTest, nnetSize=NA, nnetDecay=NA, rdaAlpha=NA, rdaDelta=NA, ...){
  cat(paste("\tEvaluating Fitnesses...\n"))
  if(toString(trainTest)=="LOO"){
    traintest<-xvalSpec("LOO")
  } else if (toString(trainTest)=="LOG"){
    traintest<-xvalSpec("LOG", 5, balKfold.xvspec(5))
  } else {
    traintest<-c(trainTest)
  }
  formula<-as.formula(paste(response, "~."))
  population<-individuals[,2:dim(individuals)[2]]
  populationSize<-nrow(population)
  results<-c()

  for (q in 1:populationSize) {
    if (!is.na(nnetSize)) {
      result = MLearn(formula, x[population[q,]==1,], .method = method, trainInd = traintest, size=nnetSize, decay=nnetDecay)
    } else if (!is.na(rdaAlpha)) {
      result = MLearn(formula, x[population[q,]==1,], .method = method, trainInd = traintest, alpha=rdaAlpha, delta=rdaDelta)
    } else {
      result = MLearn(formula, x[population[q,]==1,], .method = method, trainInd = traintest)
    }
    Accuracy<-(confuMat(result)[1]+confuMat(result)[4])/(confuMat(result)[1]+confuMat(result)[2]+confuMat(result)[3]+confuMat(result)[4])
    results<-rbind(results, Accuracy)
  }
  results<-cbind(individuals[,1], results)
  rownames(results)=1:populationSize

  avgs<-c()
  for (j in results[,1]){
    avgs<-rbind(avgs, mean(results[results[,1]==j,2]))
  }
  results<-cbind(results, avgs)
  colnames(results)=c("Id", "Accuracy", "Average Acc")
  return(results)
}

####################################################################
#	AnalyzeResults
####################################################################
#
#' AnalyzeResults
#'
#' Ranks individuals according to their fitness and records the results.
#' @aliases AnalyzeResults
#' @param individuals Population of individuals with diploid genotypes.
#' @param results Results returned by EvaluationFunction().
#' @param randomAssortment Random Assortment of Chromosomes for recombinations. The default value is TRUE.
#' @param chrConf Configuration of chromosomes returned by splitChromosomes().
#' @examples
#' \dontrun{
#'  library(genefilter)
#'  library(ALL)
#'  data(ALL)
#'  bALL = ALL[, substr(ALL$BT,1,1) == "B"]
#'  smallALL = bALL[, bALL$mol.biol %in% c("BCR/ABL", "NEG")]
#'  smallALL$mol.biol = factor(smallALL$mol.biol)
#'  smallALL$BT = factor(smallALL$BT)
#'  f1 <- pOverA(0.25, log2(100))
#'  f2 <- function(x) (IQR(x) > 0.5)
#'  f3 <- ttest(smallALL$mol.biol, p=0.1)
#'  ff <- filterfun(f1, f2, f3)
#'  selectedsmallALL <- genefilter(exprs(smallALL), ff)
#'  smallALL = smallALL[selectedsmallALL, ]
#'  rm(f1)
#'  rm(f2)
#'  rm(f3)
#'  rm(ff)
#'  rm(bALL)
#'  sum(selectedsmallALL)
#'  set.seed(1357)
#'
#'  population0<-InitialPopulation(smallALL, 14, 10, FALSE)
#'  individuals0<-Individuals(population0)
#'  results0<-EvaluationFunction(smallALL, individuals0, response="mol.biol",
#'              method=knn.cvI(k=3, l=2), trainTest="LOG")
#'  chrConf0<-splitChromosomes(smallALL)
#'  iterRes0<-AnalyzeResults(individuals0, results0, randomAssortment=TRUE, chrConf0)
#'  }
#' @export
#
AnalyzeResults<-function(individuals, results, randomAssortment=TRUE, chrConf){
  cat(paste("\tAnalyzing the results\n"))
  keep<-matrix(0, nrow=nrow(individuals), ncol=1)
  rownames(keep)<-rownames(results)
  colnames(keep)<-"Keep"
  crossOvers<-matrix(c(0), nrow=0, ncol=ncol(individuals))
  colnames(crossOvers)<-colnames(individuals)
  for (i in 1:nrow(results)){
    if (results[i, 2] >= results[i, 3])
      keep[i, 1]=1
  }

  cat(paste("\tApplying crossovers...\n"))

  if (randomAssortment==TRUE){
    for (j in 1:(nrow(individuals)/2)){
      selChr<-individuals[results[,1]==j, -1]
      repeat {
        newChr<-RandomAssortment(Crossover(selChr[1,], selChr[2,], chrConf), chrConf)

        if ((sum(newChr[1,])>3)&&(sum(newChr[2,])>3)){
          break
        }
        cat(paste("\tFailed crossover & random assortment...Redone.\n"))
      }
      newChromosomes<-cbind(c(j,j), newChr)
      crossOvers<-rbind(crossOvers, newChromosomes)
    }

    cat(paste("\tApplying Random Assortment...\n"))

    rownames(crossOvers)<-1:nrow(crossOvers)

  } else {
    for (j in 1:(nrow(individuals)/2)){
      selChr<-individuals[results[,1]==j, -1]
      repeat {
        newChr<-Crossover(selChr[1,], selChr[2,], chrConf)

        if ((sum(newChr[1,])>3)&&(sum(newChr[2,])>3)){
          break
        }
        cat(paste("\tFailed crossover...Redone.\n"))
      }
      newChromosomes<-cbind(c(j,j), newChr)
      crossOvers<-rbind(crossOvers, newChromosomes)
    }

    rownames(crossOvers)<-1:nrow(crossOvers)
  }

  return(list(keep, crossOvers))
}

####################################################################
#	Crossover
####################################################################
#
#' Crossover
#'
#' Two-point crossover operator.
#' @aliases Crossover
#' @param c1 Set of chromosomes.
#' @param c2 Set of chromosomes.
#' @param chrConf Configuration of chromosomes returned by splitChromosomes().
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#' set.seed(1357)
#' population02<-InitialPopulation(demoALL, 2, 4, FALSE)
#' chrConf02<-splitChromosomes(demoALL, 2)
#' chrConf02
#' population02[1:2,]
#' Crossover(population02[1,], population02[2,], chrConf02)
#'  }
#' @export
#
Crossover<-function(c1, c2, chrConf){
  crossVector<-rep(0, length(c1))
  for (i in 1:max(chrConf)) {
    crossSlice<-crossVector[chrConf==i]
    crossIndexes<-sort(sample(1:(length(crossSlice)),2, replace=TRUE))
    crossSlice[crossIndexes[1]:crossIndexes[2]]=1
    rm(crossIndexes)
    crossVector[chrConf==i]<-crossSlice
  }

  c3<-rep(NA, length(c1))
  c4<-rep(NA, length(c1))
  maTemp<-rbind(chrConf, crossVector,c1, c2, c3, c4)
  rm(c3)
  rm(c4)
  rm(crossVector)

  maTemp[5,maTemp[2,]==0]<-maTemp[3,maTemp[2,]==0]
  maTemp[5,maTemp[2,]==1]<-maTemp[4,maTemp[2,]==1]
  maTemp[6,maTemp[2,]==0]<-maTemp[4,maTemp[2,]==0]
  maTemp[6,maTemp[2,]==1]<-maTemp[3,maTemp[2,]==1]
  newChrs<-maTemp[5:6,]
  rm(maTemp)

  return(newChrs)
}
####################################################################
#	RandomAssortment
####################################################################
#
#' RandomAssortment
#'
#' Random assortment of chromosomes operator.
#' @aliases RandomAssortment
#' @param newChrs Set of chromosomes.
#' @param chrConf Configuration of chromosomes returned by splitChromosomes().
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' population02<-InitialPopulation(demoALL, 2, 4, FALSE)
#' chrConf02<-splitChromosomes(demoALL, 4)
#'
#' set.seed(1357)
#' cr1<-Crossover(population02[1,], population02[2,], chrConf02)
#' RandomAssortment(cr1, chrConf02)
#' cr1
#' chrConf02
#'  }
#' @export
#
RandomAssortment<-function(newChrs, chrConf){
  AssortIndex<-sample(c(0,1), size=max(chrConf), replace = TRUE)
  c3<-rep(NA, ncol(newChrs))
  c4<-rep(NA, ncol(newChrs))
  exchange<-c()
  for (i in 1:max(chrConf)) {
    exchange<-c(exchange, rep(AssortIndex[i], sum(as.numeric(chrConf==i))))
  }

  maTemp<-rbind(chrConf, exchange, newChrs[1,], newChrs[2,], c3, c4)
  rm(c3)
  rm(c4)
  rm(AssortIndex)
  rm(exchange)
  maTemp[5,maTemp[2,]==0]<-maTemp[3,maTemp[2,]==0]
  maTemp[5,maTemp[2,]==1]<-maTemp[4,maTemp[2,]==1]
  maTemp[6,maTemp[2,]==0]<-maTemp[4,maTemp[2,]==0]
  maTemp[6,maTemp[2,]==1]<-maTemp[3,maTemp[2,]==1]
  splittedChrs<-maTemp[5:6,]
  rm(maTemp)
  return(splittedChrs)
}

####################################################################
#Point Mutations
####################################################################
#
#'  pointMutation
#'
#' Operator for the point mutation.
#' @aliases pointMutation
#' @param individuals dataset returned by Individuals().
#' @param mutationChance chance for a point mutation to occur.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' set.seed(1234)
#' population<-InitialPopulation(demoALL, 4, 9)
#' individuals<-Individuals(population)
#'
#' individuals
#' set.seed(123)
#' pointMutation(individuals, 4)
#'  }
#' @export
#
pointMutation<-function(individuals, mutationChance){
  noMutations<-floor(mutationChance/100*length(individuals[,-1]))
  cat(paste("\tApplying", noMutations, "Point Mutations...\n"))
  individualsOrig<-individuals
  indexes<-sample(0:length(individuals[,-1]),noMutations)
  individuals[,-1][indexes]=as.numeric(!as.logical(individuals[,-1][indexes]))

  for (q in 1:dim(individuals[,-1])[1]) {
    if (sum(individuals[q,-1])<4){
      individuals[q,]=individualsOrig[q,];
      cat(paste("\tInvalid mutated genotype.Not inherited....\n"))
    }
  }
  rm(q)
  rm(individualsOrig)

  return(individuals)
}

#########################################################################
#Nonsense Mutations
#########################################################################
#
#' nonSenseMutation
#'
#' Operator for the nonsense mutation.
#' @aliases nonSenseMutation
#' @param individuals dataset returned by Individuals().
#' @param chrConf Configuration of chromosomes returned by splitChromosomes().
#' @param mutationChance Chance for a nonsense mutation to occur.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' set.seed(1234)
#' population<-InitialPopulation(demoALL, 4, 9)
#' individuals<-Individuals(population)
#'
#' chrConf<-splitChromosomes(demoALL, 2)
#' chrConf
#' individuals
#'
#' set.seed(123)
#' nonSenseMutation(individuals, chrConf, 20)
#'  }
#' @export
#
nonSenseMutation<-function(individuals, chrConf, mutationChance){
  noChr<-max(chrConf)
  noMutations<-floor(mutationChance/100*dim(individuals)[1]*noChr)
  cat(paste("\tApplying", noMutations, "NonSenseMutation mutations...\n"))
  indexes<-sample(1:length(individuals[,-1]), noMutations)
  indexChr<-rep(chrConf, dim(individuals)[1])
  addIndexes<-c()
  for (i in indexes) {
    p<-i
    repeat {
      p<-p+1
      if (!identical(indexChr[i], indexChr[p])||identical(i, length(individuals[,-1]))){
        break;
      }
      addIndexes<-c(addIndexes, p)
    }
  }
  rm(i)
  allIndexes<-sort(c(indexes, addIndexes), decreasing = FALSE)
  invIndividuals<-t(individuals)
  for (i in allIndexes) {
    invIndividuals[-1,][i]=0
  }

  for (q in 1:dim(invIndividuals[-1,])[2]) {
    if (sum(invIndividuals[-1,q])<4){
      invIndividuals[,q]=t(individuals)[,q];
      cat(paste("\tInvalid mutated genotype.Not inherited....\n"))
    }
  }

  individuals<-t(invIndividuals)
  rm(invIndividuals)
  rm(i)
  rm(q)
  rm(addIndexes)
  rm(indexes)
  rm(allIndexes)
  return(individuals)
}

#########################################################################
#Frameshift Mutations
#########################################################################
#
#' frameShiftMutation
#'
#' Operator for the frameshift mutation.
#' @aliases frameShiftMutation
#' @param individuals dataset returned by Individuals().
#' @param chrConf Configuration of chromosomes returned by splitChromosomes().
#' @param mutationChance Chance for a frameshift mutation to occur.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' set.seed(1234)
#' population<-InitialPopulation(demoALL, 4, 9)
#' individuals<-Individuals(population)
#'
#' chrConf<-splitChromosomes(demoALL, 2)
#' chrConf
#' individuals
#'
#' set.seed(123)
#' frameShiftMutation(individuals, chrConf, 20)
#'  }
#' @export
#
frameShiftMutation<-function(individuals, chrConf, mutationChance){
  noChr<-max(chrConf)
  noMutations<-floor(mutationChance/100*dim(individuals)[1]*noChr)
  cat(paste("\tApplying", noMutations, "FrameShiftMutation mutations...\n"))
  indexes<-sample(1:length(individuals[,-1]),noMutations)
  indexChr<-rep(chrConf, dim(individuals)[1])
  addIndexes<-c()
  for (i in indexes) {
    p<-i
    repeat {
      p<-p+1
      if (!identical(indexChr[i], indexChr[p])||identical(i, length(individuals[,-1]))){
        break;
      }
      addIndexes<-c(addIndexes, p)
    }
  }
  rm(i)
  allIndexes<-sort(c(indexes, addIndexes), decreasing = FALSE)
  invIndividuals<-t(individuals)
  for (j in 1:length(allIndexes)) {
    if (identical(allIndexes[j]+1, allIndexes[j+1])){
      invIndividuals[-1,][allIndexes[j]]=invIndividuals[-1,][allIndexes[j]+1]
    }else{
      invIndividuals[-1,][allIndexes[j]]=0
    }
  }

  for (q in 1:dim(invIndividuals[-1,])[2]) {
    if (sum(invIndividuals[-1,q])<4){
      invIndividuals[,q]=t(individuals)[,q];
      cat(paste("\tInvalid mutated genotype.Not inherited....\n"))
    }
  }

  individuals<-t(invIndividuals)
  rm(invIndividuals)
  rm(j)
  rm(q)
  rm(addIndexes)
  rm(indexes)
  rm(allIndexes)
  return(individuals)
}

#########################################################################
#Large Segment Deletion Mutation
#########################################################################
#
#' largeSegmentDeletion
#'
#' Operator for the large segment deletion.
#' @aliases largeSegmentDeletion
#' @param individuals dataset returned by Individuals().
#' @param chrConf Configuration of chromosomes returned by splitChromosomes().
#' @param mutationChance Chance for a large segment deletion mutation to occur.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' set.seed(1234)
#' population<-InitialPopulation(demoALL, 4, 9)
#' individuals<-Individuals(population)
#'
#' chrConf<-splitChromosomes(demoALL, 2)
#' chrConf
#' individuals
#'
#' set.seed(123)
#' largeSegmentDeletion(individuals, chrConf, 20)
#'  }
#' @export
#
largeSegmentDeletion<-function(individuals, chrConf, mutationChance){
  noChr<-max(chrConf)
  indexesChr<-sample(1:(dim(individuals)[1]*noChr),floor(mutationChance/100*dim(individuals)[1]*noChr))
  indivIndex<-ceiling(indexesChr/noChr)
  chrIndex<-indexesChr%/%indivIndex
  noMutations<-length(indexesChr)
  cat(paste("\tApplying", noMutations, "LargeSegmentDeletion mutations...\n"))

  individualsOrig<-individuals

  for (i in 1:length(indexesChr)) {
    margins<-sort(sample(1:(length(individuals[,-1][indivIndex[i],chrConf==chrIndex[i]])),2, replace=FALSE), decreasing = FALSE)
    individuals[,-1][indivIndex[i],chrConf==chrIndex[i]][margins[1]:margins[2]]=0
  }

  for (q in 1:dim(individuals[,-1])[1]) {
    if (sum(individuals[q,-1])<4){
      individuals[q,]=individualsOrig[q,];
      cat(paste("\tInvalid mutated genotype.Not inherited....\n"))
    }
  }

  rm(i)
  rm(q)
  rm(individualsOrig)
  rm(indexesChr)
  rm(indivIndex)
  rm(chrIndex)
  return(individuals)
}

#########################################################################
#Whole Chromosome Deletion Mutation
#########################################################################
#
#' wholeChromosomeDeletion
#'
#' Operator for the deletion of a whole chromosome.
#' @aliases wholeChromosomeDeletion
#' @param individuals dataset returned by Individuals().
#' @param chrConf Configuration of chromosomes returned by splitChromosomes().
#' @param mutationChance Chance for a deletion of a whole chromosome mutation to occur.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' set.seed(1234)
#' population<-InitialPopulation(demoALL, 4, 9)
#' individuals<-Individuals(population)
#'
#' chrConf<-splitChromosomes(demoALL, 2)
#' chrConf
#' individuals
#'
#' set.seed(123)
#' wholeChromosomeDeletion(individuals, chrConf, 20)
#'  }
#' @export
#
wholeChromosomeDeletion<-function(individuals, chrConf, mutationChance){
  noChr<-max(chrConf)
  indexesChr<-sample(1:(dim(individuals)[1]*noChr),floor(mutationChance/100*dim(individuals)[1]*noChr))
  indivIndex<-ceiling(indexesChr/noChr)
  chrIndex<-indexesChr%/%indivIndex
  noMutations<-length(indexesChr)
  cat(paste("\tApplying", noMutations, "WholeChromosomeDeletion mutations...\n"))

  individualsOrig<-individuals

  for (i in 1:length(indexesChr)) {
    individuals[,-1][indivIndex[i],chrConf==chrIndex[i]]=0
  }

  for (q in 1:dim(individuals[,-1])[1]) {
    if (sum(individuals[q,-1])<4){
      individuals[q,]=individualsOrig[q,];
      cat(paste("\tInvalid mutated genotype.Not inherited....\n"))
    }
  }

  rm(i)
  rm(q)
  rm(individualsOrig)
  rm(indexesChr)
  rm(indivIndex)
  rm(chrIndex)
  return(individuals)
}

#########################################################################
#Transposons
#########################################################################
#
#' transposon
#'
#' Operator for transposons.
#' @aliases transposon
#' @param individuals dataset returned by Individuals().
#' @param chrConf Configuration of chromosomes returned by splitChromosomes().
#' @param mutationChance Chance for a transposon mutation to occur.
#' @examples
#' \dontrun{
#' library(ALL)
#' data(ALL)
#'
#' demoALL<-ALL[1:12,1:8]
#'
#' set.seed(1234)
#' population<-InitialPopulation(demoALL, 4, 9)
#' individuals<-Individuals(population)
#'
#' chrConf<-splitChromosomes(demoALL, 2)
#' chrConf
#' individuals
#'
#' set.seed(123)
#' transposon(individuals, chrConf, 20)
#'  }
#' @export
#
transposon<-function(individuals, chrConf, mutationChance){
  noChr<-max(chrConf)
  indexesChr<-sample(1:(dim(individuals)[1]*noChr),floor(mutationChance/100*dim(individuals)[1]*noChr))
  indivIndex<-ceiling(indexesChr/noChr)
  chrIndex<-indexesChr%/%indivIndex
  noMutations<-length(indexesChr)
  cat(paste("\tApplying", noMutations, "Transposons mutations...\n"))

  individualsOrig<-individuals

  for (i in 1:length(indexesChr)) {
    pickFrom<-as.numeric(which(individuals[,-1][indivIndex[i],chrConf==chrIndex[i]]==1))
    if(length(pickFrom)!=0){
      transposon<-sample(pickFrom, 1)
      positions<-sample(c(-(sum(chrConf==chrIndex[i])-1):-1, 1:(sum(chrConf==chrIndex[i])-1)), 1)
      newIndex<-transposon+positions
      if (newIndex>sum(chrConf==chrIndex[i])){
        newIndex<-newIndex-sum(chrConf==chrIndex[i])
      }

      if (newIndex<1){
        newIndex<-sum(chrConf==chrIndex[i])+newIndex
      }
      individuals[,-1][indivIndex[i],chrConf==chrIndex[i]][transposon]=0
      individuals[,-1][indivIndex[i],chrConf==chrIndex[i]][newIndex]=1
    }
  }

  for (q in 1:dim(individuals[,-1])[1]) {
    if (sum(individuals[q,-1])<4){
      individuals[q,]=individualsOrig[q,];
      cat(paste("\tInvalid mutated genotype.Not inherited....\n"))
    }
  }

  rm(i)
  rm(q)
  rm(individualsOrig)
  rm(indexesChr)
  rm(indivIndex)
  rm(chrIndex)
  return(individuals)
}
#########################################################################

####################################################################
#	Elitism
####################################################################
#
#' Elitism
#'
#' Operator for elitism.
#' @aliases Elitism
#' @param results Results returned by EvaluationFunction().
#' @param elitism Elite population in percentages.
#' @param ID Dominance. The default value is "ID1". Use "ID2" for Incomplete Dominance.
#' @examples
#' \dontrun{
#'  library(genefilter)
#'  library(ALL)
#'  data(ALL)
#'  bALL = ALL[, substr(ALL$BT,1,1) == "B"]
#'  smallALL = bALL[, bALL$mol.biol %in% c("BCR/ABL", "NEG")]
#'  smallALL$mol.biol = factor(smallALL$mol.biol)
#'  smallALL$BT = factor(smallALL$BT)
#'  f1 <- pOverA(0.25, log2(100))
#'  f2 <- function(x) (IQR(x) > 0.5)
#'  f3 <- ttest(smallALL$mol.biol, p=0.1)
#'  ff <- filterfun(f1, f2, f3)
#'  selectedsmallALL <- genefilter(exprs(smallALL), ff)
#'  smallALL = smallALL[selectedsmallALL, ]
#'  rm(f1)
#'  rm(f2)
#'  rm(f3)
#'  rm(ff)
#'  rm(bALL)
#'  sum(selectedsmallALL)
#'  set.seed(1357)
#'
#'  population0<-InitialPopulation(smallALL, 14, 8, FALSE)
#'  individuals0<-Individuals(population0)
#'  results0<-EvaluationFunction(smallALL, individuals0, response="mol.biol",
#'              method=knn.cvI(k=3, l=2), trainTest="LOG")
#'  Elitism(results0, 25, ID="ID1")
#'  Elitism(results0, 25, ID="ID2")
#'  }
#' @export
#
Elitism<-function(results, elitism, ID){
  cat(paste("\tApplying Elitism...Keeping the Best ", elitism, "%\n"))
  if (ID=="ID2"){
    cat("Elitistic individuals...\n")
    elite<-3
  } else {
    cat("Elitistic genotypes...\n")
    elite<-2
  }

  keep<-matrix(0, nrow=nrow(results), ncol=1)
  rownames(keep)<-rownames(results)
  colnames(keep)<-"Keep"
  toKeep<-sort(results[,elite], decreasing = TRUE, index.return=TRUE)
  toKeep<-toKeep$ix
  newIndex<-floor(length(toKeep)*elitism/100)
  rez<-results[toKeep,]
  toKeep<-toKeep[1:newIndex]
  keep[toKeep]<-1
  return(list(keep, toKeep))
}

####################################################################
#	EmbryonicSelection
####################################################################
#
#' EmbryonicSelection
#'
#' Function for deleting individuals with a fitness below a specified threshold.
#' @aliases EmbryonicSelection
#' @param population Population of individuals with diploid genotypes.
#' @param results Results returned by EvaluationFunction().
#' @param embryonicSelection Threshold value. The default value is NA.
#' @examples
#' \dontrun{
#'  library(genefilter)
#'  library(ALL)
#'  data(ALL)
#'  bALL = ALL[, substr(ALL$BT,1,1) == "B"]
#'  smallALL = bALL[, bALL$mol.biol %in% c("BCR/ABL", "NEG")]
#'  smallALL$mol.biol = factor(smallALL$mol.biol)
#'  smallALL$BT = factor(smallALL$BT)
#'  f1 <- pOverA(0.25, log2(100))
#'  f2 <- function(x) (IQR(x) > 0.5)
#'  f3 <- ttest(smallALL$mol.biol, p=0.1)
#'  ff <- filterfun(f1, f2, f3)
#'  selectedsmallALL <- genefilter(exprs(smallALL), ff)
#'  smallALL = smallALL[selectedsmallALL, ]
#'  rm(f1)
#'  rm(f2)
#'  rm(f3)
#'  rm(ff)
#'  rm(bALL)
#'  sum(selectedsmallALL)
#'  set.seed(1357)
#'
#'  population0<-InitialPopulation(smallALL, 14, 8, FALSE)
#'  individuals0<-Individuals(population0)
#'  results0<-EvaluationFunction(smallALL, individuals0, response="mol.biol",
#'              method=knn.cvI(k=3, l=2), trainTest="LOG")
#'  EmbryonicSelection(individuals0, results0, 0.5)
#'  }
#' @export
#
EmbryonicSelection<-function(population, results, embryonicSelection){
  cat(paste("\tApplying Embryonic Selection for Fitness > ", embryonicSelection, "\n"))
  keep<-matrix(0, nrow=nrow(population), ncol=1)
  rownames(keep)<-rownames(results)
  colnames(keep)<-"Keep"

  keep[(results[,3] > embryonicSelection)==TRUE]=1

  return(keep)
}

####################################################################
#	PlotGenAlg
####################################################################
#
#' PlotGenAlg
#'
#' Function for graphically representing the evolution.
#' @aliases PlotGenAlg
#' @param DGenes Occurences of genes as dominant.
#' @param dGenes Occurences of genes as recessive. For future developments.
#' @param maxEval Maximum fitness.
#' @param meanEval Average fitness.
#' @examples
#' \dontrun{
#' 		#Graphical representation of the evolution after each generation.
#' 		#Intended to be used by dGAselID() only.
#' 		#Please refer to the example for dGAselID().
#'  }
#' @export
#
PlotGenAlg <- function(DGenes, dGenes, maxEval, meanEval){
  dev.off()
  dev.new()
  setFr <- layout(matrix(c(1,2,3,3),2,2,byrow = TRUE), TRUE)
  layout.show(setFr)
  par(las=2)
  index<-sort(DGenes, decreasing=TRUE, index.return=TRUE)
  DGenes<-DGenes[index$ix]
  plottedGenes<-length(DGenes)
  plot(maxEval, type="o", col="red", xlab="iteration no.", main="Maximum Accuracy")
  plot(meanEval, type="o", col="blue", xlab="iteration no.", main="Mean Accuracy")
  barplot(DGenes[1:plottedGenes], main="Genes inheritance", xlab="Gene", col=c("darkblue"), beside=FALSE, add=FALSE)
}

####################################################################
#	dGAselID
####################################################################
#
#'  dGAselID
#'
#' Initializes and starts the search with the genetic algorithm.
#' @aliases dGAselID
#' @param x Dataset in ExpressionSet format.
#' @param response Response variable
#' @param method Supervised classifier for fitness evaluation. Most of the supervised classifiers in MLInterfaces are acceptable. The default is knn.cvI(k=3, l=2).
#' @param trainTest Cross-validation method. The default is "LOG".
#' @param startGenes Genes in the genotypes at initialization.
#' @param populationSize Number of genotypes in initial population.
#' @param iterations Number of iterations.
#' @param noChr Number of chromosomes. The default value is 22.
#' @param elitism Elite population in percentages.
#' @param ID Dominance. The default value is "ID1". Use "ID2" for Incomplete Dominance.
#' @param pMutationChance Chance for a Point Mutation to occur. The default value is 0.
#' @param nSMutationChance Chance for a Non-sense Mutation to occur. The default value is 0.
#' @param fSMutationChance Chance for a Frameshift Mutation to occur. The default value is 0.
#' @param lSDeletionChance Chance for a Large Segment Deletion to occur. The default value is 0.
#' @param wChrDeletionChance Chance for a Whole Chromosome Deletion to occur. The default value is 0.
#' @param transposonChance Chance for a Transposon Mutation to occur. The default value is 0.
#' @param randomAssortment Random Assortment of Chromosomes for recombinations. The default value is TRUE.
#' @param embryonicSelection Remove chromosomes with fitness < specified value. The default value is NA.
#' @param EveryGeneInInitialPopulation Request for every gene to be present in the initial population. The default value is TRUE.
#' @param nnetSize for nnetI. The default value is NA.
#' @param nnetDecay for nnetI. The default value is NA.
#' @param rdaAlpha for rdaI. The default value is NA.
#' @param rdaDelta for rdaI. The default value is NA.
#' @param ... Additional arguments.
#' @return The output is a list containing 5 named vectors, records of the evolution:
#'   \item{DGenes}{The occurrences in selected genotypes for every gene,}
#'   \item{dGenes}{The occurrences in discarded genotypes for every gene,}
#'   \item{MaximumAccuracy}{Maximum accuracy in every generation,}
#'   \item{MeanAccuracy}{Average accuracy in every generation,}
#'   \item{MinAccuracy}{Minimum accuracy in every generation,}
#'   \item{BestIndividuals}{Best individual in every generation.}
#'
#' @examples
#' \dontrun{
#'  library(genefilter)
#'  library(ALL)
#'  data(ALL)
#'  bALL = ALL[, substr(ALL$BT,1,1) == "B"]
#'  smallALL = bALL[, bALL$mol.biol %in% c("BCR/ABL", "NEG")]
#'  smallALL$mol.biol = factor(smallALL$mol.biol)
#'  smallALL$BT = factor(smallALL$BT)
#'  f1 <- pOverA(0.25, log2(100))
#'  f2 <- function(x) (IQR(x) > 0.5)
#'  f3 <- ttest(smallALL$mol.biol, p=0.1)
#'  ff <- filterfun(f1, f2, f3)
#'  selectedsmallALL <- genefilter(exprs(smallALL), ff)
#'  smallALL = smallALL[selectedsmallALL, ]
#'  rm(f1)
#'  rm(f2)
#'  rm(f3)
#'  rm(ff)
#'  rm(bALL)
#'  sum(selectedsmallALL)
#'
#'  set.seed(149)
#'  res<-dGAselID(smallALL, "mol.biol", trainTest=1:79, startGenes=12, populationSize=200,
#'                iterations=150, noChr=5, pMutationChance=0.0075, elitism=4)
#'  }
#' @export
#
dGAselID<-function(x, response, method=knn.cvI(k=3, l=2), trainTest="LOG", startGenes, populationSize, iterations, noChr=22, elitism=NA, ID="ID1", pMutationChance=0, nSMutationChance=0, fSMutationChance=0, lSDeletionChance=0, wChrDeletionChance=0, transposonChance=0, randomAssortment=TRUE, embryonicSelection=NA, EveryGeneInInitialPopulation=TRUE, nnetSize=NA, nnetDecay=NA, rdaAlpha=NA, rdaDelta=NA, ...){

  if (typeof(x)!="S4") {
    stop("The supplied data is not an ExpressionSet.");
  }

  if(randomAssortment==TRUE){
    cat("The chromosomes will be randomly assigned...\n")
  }

  if (EveryGeneInInitialPopulation==TRUE){
    cat("Every gene will be present in the initial population...\n")
  }

  if (is.na(embryonicSelection)) {
    embryonicSelection = NA
  }

  if (is.na(elitism)) {
    elitism = 0
  }

  cat("Elitism =", elitism, "%\n")
  cat("Point Mutations rate =", pMutationChance, "%\n")
  cat("Non-sense Mutations rate =", nSMutationChance, "%\n")
  cat("Frameshift Mutation rate =", fSMutationChance, "%\n")
  cat("Large Segment Deletion rate =", lSDeletionChance, "%\n")
  cat("Whole Chromosome Deletion rate =", wChrDeletionChance, "%\n")
  cat("Transposon rate =", transposonChance, "%\n")
  cat("Embryonic Selection for fitness > ", embryonicSelection, "\n")
  cat("Fitness evaluation function =", method@mlFunName, "\n")
  cat("Cross-validation =", trainTest, "\n")

  cat("\nInitial population...\n")
  initialPopulation<-InitialPopulation(x, populationSize, startGenes, EveryGeneInInitialPopulation)
  individuals<-Individuals(initialPopulation)

  cat(paste("Splitting the genotype in", noChr, "chromosomes\n"))
  chrConf<-splitChromosomes(x, noChr)

  kDGenes<-matrix(c(0), nrow=1, ncol=ncol(initialPopulation))
  colnames(kDGenes)<-colnames(initialPopulation)
  rownames(kDGenes)<-"DGenes"

  kdGenes<-matrix(c(0), nrow=1, ncol=ncol(initialPopulation))
  colnames(kdGenes)<-colnames(initialPopulation)
  rownames(kdGenes)<-"dGenes"

  MaxAcc<-c()
  MeanAcc<-c()
  MinAcc<-c()
  bestIndividual<-matrix(0, nrow=1, ncol=ncol(initialPopulation))

  iteration<-0
  dev.new()

  repeat{
    cat(paste("Starting iteration no.", iteration, "\n"))

    results<-EvaluationFunction(x, individuals, response, method, trainTest, nnetSize, nnetDecay, rdaAlpha, rdaDelta)

    iterMinAccuracy <- range(results[,2])[1]
    MinAcc<-c(MinAcc, iterMinAccuracy)
    cat(paste("\tMinimum Fitness in iteration no.", iteration, "equals", iterMinAccuracy*100, "%\n"))

    iterMeanAccuracy <- mean(results[,2])
    MeanAcc<-c(MeanAcc, iterMeanAccuracy)
    cat(paste("\tMean Fitness in iteration no.", iteration, "equals", iterMeanAccuracy*100, "%\n"))

    iterMaxAccuracy <- range(results[,2])[2]
    MaxAcc<-c(MaxAcc, iterMaxAccuracy)
    cat(paste("\tMaximum Fitness in iteration no.", iteration, "equals", iterMaxAccuracy*100, "%\n"))

    lostInEmbryonic<-0
    if (!is.na(embryonicSelection)) {
      keptEmbr<-EmbryonicSelection(individuals, results, embryonicSelection)
      keptIndividuals<-individuals[keptEmbr==1,]
      keptIndividuals[,1]<-rep(1:(nrow(keptIndividuals)/2), each=2)
      rownames(keptIndividuals)<-1:nrow(keptIndividuals)
      keptResults<-results[keptEmbr==1,]
      keptResults[,1]<-rep(1:(nrow(keptIndividuals)/2), each=2)
      rownames(keptResults)<-1:nrow(keptResults)
      discardedIndividuals<-individuals[keptEmbr==0,]
      lostInEmbryonic<-nrow(discardedIndividuals)
    } else {
      keptIndividuals<-individuals
      keptResults<-results
      discardedIndividuals<-individuals[0,]
    }

    iterRes<-AnalyzeResults(keptIndividuals, keptResults, randomAssortment, chrConf)

    keptIndividualsFromChildren<-iterRes[[2]]

    discardedIndividuals<-rbind(discardedIndividuals, keptIndividuals[!(iterRes[[1]]==1),])
    keptIndividualsFromParents<-keptIndividuals[(iterRes[[1]]==1),]
    rownames(keptIndividualsFromParents)<-1:nrow(keptIndividualsFromParents)
    keptResults<-keptResults[iterRes[[1]]==1,]
    rownames(keptResults)<-1:nrow(keptResults)

    keptInElitism<-0
    if (floor(nrow(keptResults)*elitism/100)==0) {
      tempResults<-keptResults
      tempIndividualsFromParents<-keptIndividualsFromParents
      keptIndividualsFromParents<-keptIndividualsFromParents[0,]
      keptResults<-keptResults[0,]
      keptElit<-Elitism(tempResults, 50, ID)
      forBest<-tempIndividualsFromParents[keptElit[[2]],]
      forBest<-forBest[1,]
      rm(tempResults)
      rm(tempIndividualsFromParents)
    } else {
      keptElit<-Elitism(keptResults, elitism, ID)
      keptIndividualsFromParents<-keptIndividualsFromParents[keptElit[[2]],]
      forBest<-rbind(keptIndividualsFromParents)[1,]
      keptResults<-keptResults[keptElit[[2]],]
      keptInElitism<-nrow(rbind(keptResults))
    }

    best<-t(as.matrix(forBest))[,-1]
    bestIndividual<-rbind(bestIndividual, best)
    rm(forBest)

    if(lostInEmbryonic<keptInElitism){
      adjust<-keptInElitism-lostInEmbryonic
      toRemove<-sample(1:nrow(keptIndividualsFromChildren), adjust, replace=FALSE)
      keptIndividualsFromChildren<-keptIndividualsFromChildren[-toRemove,]
    }

    keptIndividuals<-rbind(keptIndividualsFromParents,keptIndividualsFromChildren)
    rownames(keptIndividuals)<-1:nrow(keptIndividuals)

    tempMat<-matrix(0, nrow=1, ncol= ncol(individuals))
    tempMat<-rbind(keptIndividualsFromParents)
    kDGenes[1,]<-kDGenes[1,] + colSums(tempMat)[-1]
    rm(tempMat)

    if((nrow(discardedIndividuals)>0)&&(ncol(discardedIndividuals)>0)){
      tempMat<-matrix(0, nrow=1, ncol= ncol(individuals))
      tempMat<-rbind(keptIndividualsFromParents)
      kdGenes[1,]<-kdGenes[1,] + colSums(tempMat)[-1]
      rm(tempMat)
    }

    if(pMutationChance!=0){
      keptIndividuals<-pointMutation(keptIndividuals, pMutationChance)
    }

    if(nSMutationChance!=0){
      keptIndividuals<-nonSenseMutation(keptIndividuals, chrConf, nSMutationChance)
    }

    if(fSMutationChance!=0){
      keptIndividuals<-frameShiftMutation(keptIndividuals, chrConf, fSMutationChance)
    }

    if(lSDeletionChance!=0){
      keptIndividuals<-largeSegmentDeletion(keptIndividuals, chrConf, lSDeletionChance)
    }

    if(wChrDeletionChance!=0){
      keptIndividuals<-wholeChromosomeDeletion(keptIndividuals, chrConf, wChrDeletionChance)
    }

    if(transposonChance!=0){
      keptIndividuals<-transposon(keptIndividuals, chrConf, transposonChance)
    }

    for (i in 1:dim(keptIndividuals)[1]) {
      if((sum(keptIndividuals[i,2:dim(keptIndividuals)[2]]))<3){
        index<-keptIndividuals[i,]==0
        interval<-1:dim(keptIndividuals)[2]
        interval<-interval[index]
        index<-sample(interval,1, replace=FALSE)
        keptIndividuals[i,index]<-1
        rm(index)
        rm(interval)
      }
    }

    keptIndividuals<-RandomizePop(keptIndividuals)
    keptIndividuals<-Individuals(keptIndividuals[,-1])
    cat(paste("\tPopulation Size in iteration no. ", iteration," = ", nrow(keptIndividuals), "\n"))

    PlotGenAlg(kDGenes, kdGenes, MaxAcc, MeanAcc)

    flush.console()

    individuals<-keptIndividuals

    iteration <- iteration+1
    if(iteration>=iterations) break()

  }
  rezultate<-list(DGenes=kDGenes, dGenes=kdGenes, MaximumAccuracy=MaxAcc, MeanAccuracy=MeanAcc, MinAccuracy=MinAcc, BestIndividuals=bestIndividual[-1,])
  return(rezultate)
}

####################################################################
