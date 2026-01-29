#Class implementing the AssociationRulesAlgorithm
  #Implements the common functions of an association rules algorithm

require(Matrix)
require(pmml)
require(arules)

AssociationRulesAlgorithm <- R6::R6Class("AssociationRulesAlgorithm",

  inherit = KeelAlgorithm,

  public = list(

    #Public properties

    #Algorithm outputs
    output = NULL,
    rules = NULL,


    #Public functions

    #Initialize function
    setParameters = function(dat){
      #super$initialize()

      private$datFilename <- "dat.dat"

      private$datDataset <- dat

      #Test jar file
      #if(! file.exists(paste0(private$exePath, private$jarName))){
      if(! file.exists(system.file("exe", private$jarName, package = "RKEEL"))){
        stop(paste0(private$jarName, " doesn't exist under the defined path. Installation error."))
      }

      private$dataName <- "data"
    },

    run = function(folderPath, expUniqueName, javaOptions){

      super$run(folderPath, expUniqueName, javaOptions)

      #Use tryCatch() to remove experiment folders even it there are errors
      tryCatch({
        #Create dataset folder
        dir.create(paste0(private$mainPath, "/datasets/", private$dataName))
        #Write dataset files
        private$writeDatFromDataframeAR(private$datDataset, paste0(private$mainPath, "/datasets/", private$dataName, "/", private$datFilename))

        #Copy algorithm exe
        file.copy(system.file("exe", private$jarName, package = "RKEEL"), paste0(private$mainPath, "/exe/", private$jarName))

        #Coy KEELToPMML algorithm to exe dir
        file.copy(system.file("exe", "KeelToPMML.jar", package = "RKEEL"), paste0(private$mainPath, "/exe/", "KeelToPMML.jar"))

         #Coy KeelLateXTables algorithm to exe dir
        file.copy(system.file("exe", "KeelLateXTables.jar", package = "RKEEL"), paste0(private$mainPath, "/exe/", "KeelLateXTables.jar"))

        #Create results dir
        dir.create(paste0(private$mainPath, "/results/", private$algorithmName, ".", private$dataName))

        #Create results dir KEELToPMML dir
        dir.create(paste0(private$mainPath, "/results/", "KEELToPMML"))
        dir.create(paste0(private$mainPath, "/results/KEELToPMML/TST", private$algorithmName))

        #Create results dir KeelLateXTables dir
        dir.create(paste0(private$mainPath, "/results/", "KeelLateXTables"))
        dir.create(paste0(private$mainPath, "/results/KeelLateXTables/TST", private$algorithmName))

        #Create .xml experiment file
        private$writeKeelXML()

        #Create config files
        dir.create(paste0(private$mainPath, "/scripts/", private$algorithmName))
        dir.create(paste0(private$mainPath, "/scripts/", private$algorithmName, "/", private$dataName))
        #Create config files KEELToPMML
        dir.create(paste0(private$mainPath, "/scripts/", "KEELToPMML"))
        dir.create(paste0(private$mainPath, "/scripts/", "KEELToPMML", "/TST", private$algorithmName))
        #Create config files KeelLateXTables
        dir.create(paste0(private$mainPath, "/scripts/", "KeelLateXTables"))
        dir.create(paste0(private$mainPath, "/scripts/", "KeelLateXTables", "/TST", private$algorithmName))
        private$writeKeelConfig()
        
        #Change work directory to execute .jar
        wdPath <- getwd()
        #Change to old current working directory after finishing the function
        # even if an error occurs
        on.exit(setwd(wdPath))

        #Manage options to java command line
        if(missing(javaOptions)){
          javaOptions <- ""
        }

        setwd(paste0(private$mainPath, "/scripts/"))
        if(grepl("windows", tolower(Sys.info()[1]))) {
          system(paste0(private$javaPath, "java ", private$javaOpt, " -jar RunKeel.jar"), show.output.on.console = FALSE)
        }
        else {
          system(paste0(private$javaPath, "java ", private$javaOpt, " -jar RunKeel.jar"), ignore.stdout = TRUE)
        }
        setwd(wdPath)

        #read outputs
        private$readOutputs(paste0(private$mainPath, "/results/KEELToPMML/TST", private$algorithmName, "/result0s0file0.pmml"),paste0(private$mainPath, "/results/KeelLateXTables/TST", private$algorithmName, "/result0s0.tex"))

        if(missing(folderPath)){
          cat(paste0("Algorithm ",  class(self)[1], " executed successfully", sep="\n"))
        }
        else{
          cat(paste0("Algorithm ",  class(self)[1], " executed successfully. Stored in: ", private$mainPath, sep="\n"))
        }

      }, error = function(err) {
        #Error
        cat(paste0("Error! ",err))
      }, finally = {
        #Remove data files and Keel experiment folder
        if(missing(folderPath)){
          unlink(paste0(private$dataPath, private$datFilename))
          unlink(private$mainPath, recursive = TRUE)
        }
      })

    },


    #Print object
    print = function() {

      if(class(self)[1] == "AssociationRulesAlgorithm") {
        cat("Keel Association Rules Algorithm Object", sep="\n")
        cat("Need to have an object of an algorithm implementing this class")
      }
      else {
        cat("-----------------------------------", "\n", sep="")
        cat(private$algorithmString, "\n", sep="")
        cat("-----------------------------------", "\n", sep="")
        cat(private$getParametersText(), sep="")
        cat("-----------------------------------", "\n", sep="")
      }

    },

    showRules = function(numRules = -1){
      tryCatch({
        if(numRules > 0){
          arules::inspect(arules::head(self$rules,n = numRules))
        }
        else{
          arules::inspect(self$rules)
        }
      }, error = function(err) {
        #Error
        cat(paste0("Error! ",err))
      })

    },



    addInterestMeasure = function(name, colName = ""){
      tryCatch({
        if(colName == ""){
          colName <- name
        }

        rulestmp <- arules::quality(self$rules)
        inilen <- length(names(rulestmp))
        rulestmp <- cbind(rulestmp, col = arules::interestMeasure(self$rules, measure = name, transactions = self$rules))
        finallen <- length(names(rulestmp))
        if(inilen < finallen){
          colnames(rulestmp)<-c(names(rulestmp)[-finallen],colName)
        }

        arules::quality(self$rules) <- rulestmp

      }, error = function(err) {
        #Error
        cat(paste0("Error! ",err))
      })


    },

    getInterestMeasures = function(){
      return(arules::quality(self$rules))
    },

    sortBy = function(colName = "support"){
      tryCatch({
        self$rules <- arules::sort(self$rules,by = colName)
      }, error = function(err) {
        #Error
        cat(paste0("Error! ",err))
      })
    },

    writeCSV = function(fileName = "rules", sep = ","){
      tryCatch({
        arules::write(self$rules, file=paste0(fileName,".csv"), sep = ",")
      }, error = function(err) {
        #Error
        cat(paste0("Error! ",err))
      })
    },

    writePMML = function(fileName = "rules"){
      tryCatch({
        arules::write.PMML(self$rules,paste0(fileName,".pmml"))
      }, error = function(err) {
        #Error
        cat(paste0("Error! ",err))
      })
    },

    writeKeelLatexTables = function(fileName = paste0(private$algorithmName,"-KeelLatexTables")){
      tryCatch({
        writeLines(private$resultLatex, paste0(fileName,".tex"))
      }, error = function(err) {
        #Error
        cat(paste0("Error! ",err))
      })
    }

  ),

  private = list(

    #data filename
    datFilename = NULL,

    resultLatex = NULL,

    #data dataset
    datDataset = NULL,

    #dataset name
    dataName = NULL,

    #experiment main path
    mainPath = NULL,

    #Create XML function
    writeKeelXML = function(){

      nodeExecution <- XML::xmlNode("execution")

      nodeSentence <- XML::xmlNode("sentence")
      nodeSentence <- XML::addChildren(nodeSentence, kids = list(XML::xmlNode("command", "java")))
      nodeSentence <- XML::addChildren(nodeSentence, kids = list(XML::xmlNode("option", "-Xmx512000000")))
      nodeSentence <- XML::addChildren(nodeSentence, kids = list(XML::xmlNode("option", "-jar")))
      nodeSentence <- XML::addChildren(nodeSentence, kids = list(XML::xmlNode("executableFile", paste0("../exe/", private$jarName))))
      nodeSentence <- XML::addChildren(nodeSentence, kids = list(XML::xmlNode("scriptFile", paste0("./", private$algorithmName, "/", private$dataName, "/config0s0", ".txt"))))
      nodeExecution <- XML::addChildren(nodeExecution, kids = list(nodeSentence))

      nodeSentencePMML <- XML::xmlNode("sentence")
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("command", "java")))
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("option", "-Xmx512000000")))
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("option", "-jar")))
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("executableFile", paste0("../exe/", "KeelToPMML.jar"))))
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("scriptFile", paste0("./", "KEELToPMML/TST", private$algorithmName, "/", "config0s0", ".txt"))))
      nodeExecution <- XML::addChildren(nodeExecution, kids = list(nodeSentencePMML))

      nodeSentencePMML <- XML::xmlNode("sentence")
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("command", "java")))
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("option", "-Xmx512000000")))
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("option", "-jar")))
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("executableFile", paste0("../exe/", "KeelLateXTables.jar"))))
      nodeSentencePMML <- XML::addChildren(nodeSentencePMML, kids = list(XML::xmlNode("scriptFile", paste0("./", "KeelLateXTables/TST", private$algorithmName, "/", "config0s0", ".txt"))))
      nodeExecution <- XML::addChildren(nodeExecution, kids = list(nodeSentencePMML))

      XML::saveXML(nodeExecution, paste0(private$mainPath, "/scripts/", "RunKeel.xml"))

    },


    #Create config files function
    writeKeelConfig = function(){

      #-----ALGORITHM-------
      #Ficheros de entrada
      inputDataString = paste0("\"../datasets/", private$dataName, "/", private$datFilename, "\"")
      #Ficheros de salida
      tmpOutput = ""
      for(i in 0:(private$algorithmOutputNumTxt-1)){
        tmpOutput = paste0(tmpOutput," \"../results/", private$algorithmName, ".", private$dataName, "/result0s0", "e",i,".txt\"")
      }
      outputDataString = paste0("\"../results/", private$algorithmName, ".",private$dataName, "/result0s0", ".tra\" \"../results/", private$algorithmName, ".", private$dataName, "/result0s0", ".tst\"", tmpOutput)

      #Change options to the original after executing the function, even if it 
      # fails
      oldOptions <- options()
      on.exit(options(oldOptions))
            
      options(scipen = 999)
      text <- ""
      text <- paste0(text, "algorithm = ", private$algorithmString)
      text <- paste0(text, "\ninputData = ", inputDataString)
      text <- paste0(text, "\noutputData = ", outputDataString)
      text <- paste0(text, "\n\n")
      text <- paste0(text, private$getParametersText())
      text <- paste0(text, "\n")

      writeLines(text, con = paste0(private$mainPath, "/scripts/", private$algorithmName, "/", private$dataName, "/", "config0s0", ".txt"))


      #-----KeelToPMML------
      #Ficheros de entrada
      inputDataStringPMML = paste0("\"../datasets/", private$dataName, "/", private$datFilename, "\" \"../results/", private$algorithmName, ".", private$dataName, "/result0s0", ".tst\" \"../results/", private$algorithmName, ".",private$dataName, "/result0s0", ".tra\"")
      #Ficheros de salida
      outputDataStringPMML = paste0("\"../results/", "KEELToPMML/TST", private$algorithmName, "/result0s0file0.pmml")

      text <- ""
      text <- paste0(text, "algorithm = ", "KeelToPMML")
      text <- paste0(text, "\ninputData = ", inputDataStringPMML)
      text <- paste0(text, "\noutputData = ", outputDataStringPMML)
      text <- paste0(text, "\n\n")

      writeLines(text, con = paste0(private$mainPath, "/scripts/KEELToPMML/TST", private$algorithmName, "/", "config0s0", ".txt"))

      #------KeelLateXTables-------
      #Ficheros de entrada
      inputDataStringPMML = paste0("\"../datasets/", private$dataName, "/", private$datFilename, "\" \"../results/", private$algorithmName, ".", private$dataName, "/result0s0", ".tst\" \"../results/", private$algorithmName, ".",private$dataName, "/result0s0", ".tra\"")
      #Ficheros de salida
      outputDataStringPMML = paste0("\"../results/", "KeelLateXTables/TST", private$algorithmName, "/result0s0.stat")

      text <- ""
      text <- paste0(text, "algorithm = ", "KeelLateXTables")
      text <- paste0(text, "\ninputData = ", inputDataStringPMML)
      text <- paste0(text, "\noutputData = ", outputDataStringPMML)
      text <- paste0(text, "\n\n")

      writeLines(text, con = paste0(private$mainPath, "/scripts/KeelLateXTables/TST", private$algorithmName, "/", "config0s0", ".txt"))

    },


    #Read algorithm output files
    readOutputs = function(pmmlfile,latexfile){

      #Check prediction files
      if((!file.exists(pmmlfile)) || (!file.exists(latexfile)) ){
        cat(pmmlfile, "\n", sep="")
        stop("Results were not stored. Execution error.")
      }


      #Read rules
      temp <- readLines(pmmlfile,n = -1)
      temp <- gsub("numberOfTransactions=\"Undefined\"", "numberOfTransactions=\"0\"", temp)
      writeLines(temp, pmmlfile)

      tryCatch({
        self$rules <- arules::read.PMML(pmmlfile)
        #add additional interest measures (yule'sQ and netconf)
        rulestmp <- arules::quality(self$rules)
        rulestmp <- cbind(rulestmp, "yulesQ" = arules::interestMeasure(self$rules, measure = "yuleQ", transactions = self$rules))
        arules::quality(self$rules) <- rulestmp
        rulestmp <- cbind(rulestmp, "netconf" = (rulestmp$support - (rulestmp$antecedent_support*rulestmp$consequent_support))/(rulestmp$antecedent_support*(1-rulestmp$antecedent_support)))
        arules::quality(self$rules) <- rulestmp

        private$resultLatex <- readLines(latexfile,n = -1)

      }, error = function(err) {
        #Error
        cat("Error! The number of rules generated is null or too low!")
        cat(pmmlfile, sep="\n")
        cat(latexfile, sep="\n")
        private$resultLatex <- readLines(latexfile,n = -1)
      })
    },


    #Write .dat dataset file from a data frame
    writeDatFromDataframeAR = function(data, fileName){

      dataName <- fileName

      #Check if data is a data.frame
      if(!is.data.frame(data)){
        stop(paste0("Error. Must give a data.frame."))
      }

      #full dataset string
      text <- ""

      #add relationName
      text <- paste0(text, "@relation ", dataName, "\n")

      attributesType <- c()
      #add attributes name and type
      tinputs <- "@inputs"
      toutputs <- "@outputs"

      for(i in 1:length(colnames(data))){

        #add "@attribute" and attribute name
        attribute <- paste0("@attribute ", colnames(data)[i])

        if(i != 1){
          tinputs <- paste0(tinputs, ", ", colnames(data)[i])
        }
        else{
          tinputs <- paste0(tinputs, " ", colnames(data)[i])
        }

        #caterogical
        if((typeof(data[,i]) == "character") || ( !is.na(match(TRUE, is.na(suppressWarnings(as.numeric(as.character(data[,i])))))) )  ){
          #add "{" and first value
          attribute <- paste0(attribute, " {", unique(data[,i])[1])
          #Start in 2 for no comma problems; add all other values
          for(l in 2:length(unique(data[,i]))){
            attribute <- paste0(attribute, ", ", unique(data[,i])[l])
          }
          #finish with "}"
          attribute <- paste0(attribute, "}")
          attributesType <- c(attributesType, "character")
        }
        #real
        else if(typeof(as.numeric(as.character(data[,i]))) == "double"){
          #add type, min and max values
          minValue <- format(min(na.omit(as.numeric(as.character(data[,i])))), nsmall = 1)
          maxValue <- format(max(na.omit(as.numeric(as.character(data[,i])))), nsmall = 1)
          attribute <- paste0(attribute, " real [", minValue, ", ", maxValue, "]")
          attributesType <- c(attributesType, "real")
        }
        #integer
        else if(typeof(as.numeric(as.character(data[,i]))) == "integer"){
          #add type, min and max values
          attribute <- paste0(attribute, " integer [", min(na.omit(as.numeric(as.character(data[,i])))), ", ", max(na.omit(as.numeric(as.character(data[,i])))), "]")
          attributesType <- c(attributesType, "integer")
        }
        #Categorical
        else if(!is.null(levels(data[,i]))){
          #add "{" and first value
          attribute <- paste0(attribute, " {", levels(data[,i])[1])
          #Start in 2 for no comma problems; add all other values
          for(l in 2:length(levels(data[,i]))){
            attribute <- paste0(attribute, ", ", levels(data[,i])[l])
          }
          #finish with "}"
          attribute <- paste0(attribute, "}")
          attributesType <- c(attributesType, "character")
        }

        #Add attribute line to full dataset string
        text <- paste0(text, attribute, "\n")
      }

      text <- paste0(text, tinputs, "\n")
      text <- paste0(text, toutputs, "\n")

      #Add "@data"
      text <- paste0(text, "@data", "\n")

      #Add data lines
      for(i in 1:nrow(data)){

        dataLine <- ""

        for(j in 1:ncol(data)){
          #add values separated with commas
          if(is.na(data[i,j]) || is.nan(data[i,j]) || is.null(data[i,j])) {
            dataLine <- paste0(dataLine, "<null>, ")
          }
          else{
            if(attributesType[j] == "real"){
              dataLine <- paste0(dataLine, format(as.numeric(as.character(data[i,j])), nsmall = 1), ", ")
              #dataLine <- paste0(dataLine, data[i,j], ", ")
            }
            else{
              dataLine <- paste0(dataLine, data[i,j], ", ")
            }

          }
        }

        #Delete last comma
        dataLine <- gsub(", $", "", dataLine)
        #Add data line to full dataset string
        text <- paste0(text, dataLine, "\n")
      }

      #Save dataset file
      fileConn<-file(fileName)
      writeLines(text, fileConn)
      close(fileConn)

    }
  )
)
