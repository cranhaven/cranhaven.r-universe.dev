
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


#Limit size for an input file
MAX_SIZE_MB = 10
options(shiny.maxRequestSize= MAX_SIZE_MB * 1024^2)

# The colors for the graphs (obtened from Material Design: http://www.google.ch/design/spec/style/color.html#color-color-palette)
colors <- c("#E8F5E9", "#A5D6A7", "#4CAF50", "#388E3C", "#FFF9C4", "#FFF176", 
            "#FFEB3B", "#FDD835", "#F9A825")
#Add more colors, this is are for the variable vs variable plots
colorsWithContrast <- grDevices::rainbow(30)[c(1,10,20,30,4,14,24,6,16,28)]


#Starts the server logic
shinyServer(function(input, output, session) {
  
  
  # All this must be session variables 
  dataTra <- NULL   # The SDEFSR_Dataset object for the training file
  dataMatrixTra <- NULL # The representation of the data as a matrix
  
  dataTst <- NULL  #The SDEFSR_Dataset object for the test file
  dataMatrixTst <- NULL  # The representation of the data as a matrix
  
  pathTra <- ""  #Tha path of the training file
  pathTst <- ""
  
  ruleSet <- NULL   # The resulting rule set after the execution of an SD algorithm
  data <- NULL      # SDEFSR_Dataset of the actual selected dataset (it could be train or test)
  dataMatrix <- NULL # The data of the actual selected dataset as a dataframe
  pieChart <- T     # Checks if "pie chart" visualization is selected
  fileBefore <- "Tra" #To change the visualization between tra/tst files
  
  ranges1 <- NULL  # This are the numeric ranges for the visualization filters
  ranges2 <- NULL
  
  lastValue <- 0  #This is a control variable for the buttons
  
  
  
  #on exit clean temporal file
  on.exit({ 
    if(file.exists(paste(getwd(), "/rulesFile.txt",  sep = ""))) file.remove(paste(getwd(), "/rulesFile.txt",  sep = ""))
    if(file.exists(paste(getwd(), "/optionsFile.txt",  sep = ""))) file.remove(paste(getwd(), "/optionsFile.txt",  sep = ""))
    if(file.exists(paste(getwd(), "/testQualityMeasures.txt",  sep = ""))) file.remove(paste(getwd(), "/testQualityMeasures.txt",  sep = ""))
  })
  
  
  
  #Observe "Keep this data" button
  observe({
    
      input$filterData #Observe this variable
    
     if(isolate(input$traTstRadio) == "Training File"){
       # Keep the data on variable vs variable, look at ranges1 and ranges2
       if(isolate(input$visualization) == "Variable vs Variable"){
         pos1 <- which(data[[2]] == isolate(input$Variables1))
         pos2 <- which(data[[2]] == isolate(input$Variables2))
         toKeep <- intersect(ranges1, ranges2)
         dataMatrix <<- dataMatrix[,toKeep]
         #Store data toKeep
         data$data <<- dataTra$data <<- dataTra$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTra$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTra$examplesPerClass <<- examplesPerClass
         #Modify min and max
         data$min[pos1] <<- dataTra$min[pos1] <<- isolate(input$numericRange1)[1]
         data$min[pos2] <<- dataTra$min[pos2] <<- isolate(input$numericRange2)[1]
         data$max[pos1] <<- dataTra$max[pos1] <<- isolate(input$numericRange1)[2]
         data$max[pos2] <<- dataTra$max[pos2] <<- isolate(input$numericRange2)[2]
       } else if(isolate(input$visualization) == "Pie Chart"){
         toKeep <- which((dataMatrix[nrow(dataMatrix),] +1) %in% which(data$class_names %in% isolate(input$classNames)))
         dataMatrix <<- dataMatrix[,toKeep]
         #Store data toKeep
         data$data <<- dataTra$data <<- dataTra$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTra$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTra$examplesPerClass <<- examplesPerClass
         #Modify max value
         pos <- which(data[[2]] == isolate(input$targetClassSelect))
         data$max <<- dataTra$max <<- length(isolate(input$classNames))
       } else {
         toKeep <- ranges1
         dataMatrix <<- dataMatrix[,toKeep]
         #Store data toKeep
         data$data <<- dataTra$data <<- dataTra$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTra$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTra$examplesPerClass <<- examplesPerClass
         #Modify min and max
         pos <- which(data[[2]] == isolate(input$targetClassSelect))
         data$min[pos] <<- dataTra$min[pos] <<- isolate(input$numericRangeVisualization)[1]
         data$max[pos] <<- dataTra$max[pos] <<- isolate(input$numericRangeVisualization)[2]

       } 
     } else {
       #Test File
       if(isolate(input$visualization) == "Variable vs Variable"){
         pos1 <- which(data[[2]] == isolate(input$Variables1))
         pos2 <- which(data[[2]] == isolate(input$Variables2))
         toKeep <- intersect(ranges1, ranges2)
         dataMatrix <<- dataMatrix[,toKeep]
         #Store data toKeep
         data$data <<- dataTst$data <<- dataTst$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTst$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTst$examplesPerClass <<- examplesPerClass
         #Modify min and max
         data$min[pos1] <<- dataTst$min[pos1] <<- isolate(input$numericRange1)[1]
         data$min[pos2] <<- dataTst$min[pos2] <<- isolate(input$numericRange2)[1]
         data$max[pos1] <<- dataTst$max[pos1] <<- isolate(input$numericRange1)[2]
         data$max[pos2] <<- dataTst$max[pos2] <<- isolate(input$numericRange2)[2]
       } else if(isolate(input$visualization) == "Pie Chart"){
         toKeep <- which((dataMatrix[nrow(dataMatrix),] +1) %in% which(data$class_names %in% isolate(input$classNames)))
         dataMatrix <<- dataMatrix[,toKeep]
         #Store data toKeep
         data$data <<- dataTst$data <<- dataTst$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTst$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTst$examplesPerClass <<- examplesPerClass
         #Modify max value
         pos <- which(data[[2]] == isolate(input$targetClassSelect))
         data$max <<- dataTst$max <<- length(isolate(input$classNames))
       } else {
         toKeep <- ranges1
         dataMatrix <<- dataMatrix[,toKeep]
         #Store data toKeep
         data$data <<- dataTst$data <<- dataTst$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTst$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTst$examplesPerClass <<- examplesPerClass
         #Modify min and max
         pos <- which(data[[2]] == isolate(input$targetClassSelect))
         data$min[pos] <<- dataTst$min[pos] <<- isolate(input$numericRangeVisualization)[1]
         data$max[pos] <<- dataTst$max[pos] <<- isolate(input$numericRangeVisualization)[2]
         
       } 
     }
    
  })
  
  #--------- Statistics Table --------------------
  
  output$statistics <- renderTable({
    #----Inputs this function observe ------
    input$traTstRadio
    input$classNames
    input$traFile
    input$tstFile
    input$filterData
    #------------------------------
    #If nothing is select, dont do anything
    if(input$targetClassSelect == "NA" || length(input$targetClassSelect) == 0 || is.null(input$targetClassSelect))
      return(NULL)
    
    #Find the position of the target variable
     pos <- which(data[[2]] == input$targetClassSelect)
     if(length(pos) > 0){
       
    if(data[[3]][pos] != 'c'){
      #If variable selected is numeric, simply make a summary
      as.matrix(summary(dataMatrix[pos, ]))
      
    }else{
      #If it is categoriical, simply count the number of examples
      # but it is neccesary to take into account the selected values on the visualization
      positions <- NULL
      for(i in input$classNames){ # For each selected value
        p <- which(data[[15]][[pos]][dataMatrix[pos,] + 1] == i)
        if(length(p) > 0)
          positions <- c(positions, p)
      }
      # make the summary of the selected data
      as.matrix(summary(data[[2]][dataMatrix[which(data[[2]] == input$targetClassSelect), positions] + 1]))
    }
     }
  })

  
  
  #Observer input target class
  observe({
    # ---- Inputs to obseve --------
    input$targetClassSelect
    input$Variables1
    input$Variables2
    input$filterData
    #-------------------------------
    #Update values if necessary
    posClass <- which(data[[2]] == input$targetClassSelect)
    if(!is.null(ranges2)){
      pos <- which(data[[2]] == input$Variables1)
      updateSliderInput(session, "numericRange1", min = min(dataMatrix[pos,]), max = max(dataMatrix[pos,]), value = c(min(dataMatrix[pos,]), max(dataMatrix[pos,])), step = (max(dataMatrix[pos,]) - min(dataMatrix[pos,])) / 250)
      updateSliderInput(session, "numericRangeVisualization", min = min(dataMatrix[posClass,]), max = max(dataMatrix[posClass,]), value = c(min(dataMatrix[posClass,]), max(dataMatrix[posClass,])), step = (max(dataMatrix[posClass,]) - min(dataMatrix[posClass,])) / 250)
      
    }
    
    if(!is.null(ranges1)){
      pos <- which(data[[2]] == input$Variables2)
      updateSliderInput(session, "numericRange2", min = min(dataMatrix[pos,]), max = max(dataMatrix[pos,]), value = c(min(dataMatrix[pos,]), max(dataMatrix[pos,])), step = (max(dataMatrix[pos,]) - min(dataMatrix[pos,])) / 250)
    
    }
    
  })
  
  
 #---------------- PLOT EXPLORATORY ANALISIS ----------------------------------
  
  output$datasetInfo <- renderPlot({
    
    #----Inputs to observe  ------
    tra <- input$traTstRadio
    pieChart <- input$visualization == "Pie Chart"
    input$traTstRadio
    input$execute
    #------------------------------------
    
    if(input$targetClassSelect == "NA" || length(input$targetClassSelect) == 0 || is.null(input$targetClassSelect))
      return(NULL)
 
    pos <- which(data[[2]] == input$targetClassSelect)
    if(length(pos) == 0)
      return(NULL)
    
    #Check if selected variable is categorical
     categorical <- data[[3]][pos] == 'c'
    
    if(pieChart &  categorical){
     # If "pie chart" and is a categorical variable, then show the pie chart
        positions <- NULL
        for(i in input$classNames){
          #Get only those examples which classes are in the selected variables
          p <- which(data[[15]][[pos]][dataMatrix[pos,] + 1] == i)
          if(length(p) > 0 )
            positions <- c(positions, p)
        }
        #Show the plot.
        if(!is.null(positions)){
          tabla <- table(data[[15]][[pos]][dataMatrix[pos,]+1][positions])
          pie(x = tabla,
              labels = paste(names(tabla), tabla, sep = ": "),
              radius = 1, 
              clockwise = TRUE,
              col = colors, 
              main = "Distribution of examples over variables"
          )
        }
    
    #If visualization is not pie chart or it is not a continuous variableand it is diferent of variable vs variable
    # it is or histogram or a boxplot
    } else if(input$visualization != "Variable vs Variable"){
      #by defaut, choose histogram as visualization
      updateRadioButtons(session, "visualization", selected = "Histogram")
      if( categorical){ 
        #If the variable is categorical, the histogram is visualized for those selected values only, as in the pie chart
        positions <- NULL
        for(i in input$classNames){
          p <- which(data[[15]][[pos]][dataMatrix[pos,] + 1] == i)
          if(length(p) > 0 )
            positions <- c(positions, p)
        } 
        updateSliderInput(session, "numericRangeVisualization", min = 0, max = 0, value = 0)
        updateRadioButtons(session, "visualization", selected = "Histogram")
        barplot(tabla <- table(data[[15]][[pos]][dataMatrix[pos,]+1][positions]),
                main = "Distribution of examples over variables",
                col = colors
                )
      } else if(input$visualization == "Histogram") {
        #If the variable is not categorical
        updateRadioButtons(session, "visualization", selected = "Histogram")
        #Select those instances within the range and show the histogram
        ranges1 <<- which(dataMatrix[pos,] >= input$numericRangeVisualization[1] & dataMatrix[pos,] <= input$numericRangeVisualization[2])
          hist(x = dataMatrix[pos, ranges1],
             col = colors,
             main = "Distribution of examples over variables",
             ylab = "Frequency",
             xlab = "Value")
      } else if(input$visualization == "Box Plot"){
        #If Box plot is selected, then filter instances within the ranges and visualize the box plot
        updateRadioButtons(session, "visualization", selected = "Box Plot")
        ranges1 <<- which(dataMatrix[pos,] >= input$numericRangeVisualization[1] & dataMatrix[pos,] <= input$numericRangeVisualization[2])
        boxplot(dataMatrix[pos, ranges1], 
                main = "Distribution of examples over variables",
                xlab = input$targetClassSelect, 
                ylab = "Value", 
                col = "royalblue2",
                outcol="red"
                ) 
      } 
    } else {
      #Variable vs Variable visualization
      updateRadioButtons(session, "visualization", selected = "Variable vs Variable")
      #Get positions of the variables
      pos1 <- which(data[[2]] == input$Variables1)
      pos2 <- which(data[[2]] == input$Variables2)
      
      #There are 3 plots possibilities:
      # - Categorical vs numeric: Not defined
      # - Categorical vs Categorical:  Bar plots
      # - Numeric vs numeric:  Scatter plots
      if(data$attributeTypes[pos1] == "c" & data$attributeTypes[pos1] == "c"){
      #Categorigcal vs categorical
          mat <- as.data.frame(data)
          plot(x = as.factor(mat[,pos1]), 
               y = as.factor(mat[,pos2]),
               col = colorsWithContrast,   #Change this colors to other with high contrast
               main = "Variable vs Variable Plot")
      } else if((data$attributeTypes[pos1] == "r" | data$attributeTypes[pos1] == "e") & (data$attributeTypes[pos2] == "r" | data$attributeTypes[pos2] == "e")){
        #Numeric vs numeric
        cols <- colorsWithContrast[dataMatrix[nrow(dataMatrix), ] + 1]
        ranges1 <<- which(dataMatrix[pos1,] >= input$numericRange1[1] & dataMatrix[pos1,] <= input$numericRange1[2])
        ranges2 <<- which(dataMatrix[pos2,] >= input$numericRange2[1] & dataMatrix[pos2,] <= input$numericRange2[2])
        #Take the intersection of examples that match both ranges to have the same number of examples on both axis.
        valuesToShow <- intersect(ranges1, ranges2) 
        #Show the plot
        plot(x = dataMatrix[pos1, valuesToShow],
             y = dataMatrix[pos2, valuesToShow],
             main = "Variable vs Variable Plot",
             pch = 19,
             col = cols,
             xlab = data$attributeNames[pos1],
             ylab = data$attributeNames[pos2]
             )
        #Make the legend
        legend("bottomright", legend = data$class_names, pch = 19, title = "Class", col = colorsWithContrast[1:length(data$class_names)])
      } else {
        #Categorical vs numeric
        NULL
      }
    }
    
  })
  

  

  
  # Observe Training File
 observe({
   if(! is.null(input$traFile)){
     tryCatch({
       #if path is different for the actual chosen
    if(pathTra != paste(input$traFile[,1],input$traFile[,4])){
      #get the file
      file <- input$traFile
      file.rename(file$datapath, paste(file$datapath, regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]], sep = ""))
      file$datapath <- paste(file$datapath, regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]], sep = "")
      pathTra <<- paste(input$traFile[,1],input$traFile[,4])
      #Read the file
      dataTra <<- SDEFSR::read.dataset(file$datapath)
      #Update the values, the select variable is the last one.
      updateSelectInput(session = session, 
                        inputId = "targetClassSelect", 
                        label = "Select the target variable", 
                        choices = dataTra[[2]], selected = dataTra[[2]][length(dataTra[[2]])])
      updateSelectInput(session = session, 
                        inputId = "Variables1", 
                        label = "Variable X", 
                        choices = dataTra[[2]], selected = dataTra[[2]][length(dataTra[[2]])])
      updateSelectInput(session = session, 
                        inputId = "Variables2", 
                        label = "Variable Y", 
                        choices = dataTra[[2]], selected = dataTra[[2]][length(dataTra[[2]])])
      updateSelectInput(session = session, 
                        inputId = "targetValueSelect", 
                        label = "Select the target value", 
                        choices = if(dataTra[[3]][length(dataTra[[2]])] == 'c') 
                                      c("All Values", dataTra[[15]][[length(dataTra[[2]])]])
                                  else
                                    "This is not a categorical variable!"
                          )
      updateRadioButtons(session, "traTstRadio", 
                         label = "Visualize file: ", 
                         choices = c("Training File", "Test File"), 
                         selected = "Training File")
      #update global variables
      data <<- dataTra
      dataMatrixTra <<- matrix(unlist(dataTra$data), nrow = dataTra$nVars + 1)
      dataMatrix <<- dataMatrixTra
      .updateAttributes(session, dataTra[[2]][length(dataTra[[2]])], data)
      ranges1 <<- 1:ncol(dataMatrix)
      ranges2 <<- 1:ncol(dataMatrix)
    }
     } , error = function(e) warning(e)) #If an error occur, shows to the user the error.
   }
 })
  
  
  
  # Observe Test File (is the same as before, but for the test file)
  observe({
    if(! is.null(input$tstFile)){
      if(pathTst != paste(input$tstFile[,1], input$tstFile[,4])){
        file <- input$tstFile
        file.rename(file$datapath, paste(file$datapath, regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]], sep = ""))
        file$datapath <- paste(file$datapath, regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]], sep = "")
        pathTst <<- paste(input$tstFile[,1], input$tstFile[,4])
        dataTst <<- SDEFSR::read.dataset(file$datapath)
        updateSelectInput(session = session, 
                          inputId = "targetClassSelect", 
                          label = "Select the target variable", 
                          choices = dataTst[[2]], selected = dataTst[[2]][length(dataTst[[2]])])
        updateSelectInput(session = session, 
                          inputId = "Variables1", 
                          label = "Variable 1", 
                          choices = dataTst[[2]], selected = dataTst[[2]][length(dataTst[[2]])])
        updateSelectInput(session = session, 
                          inputId = "Variables2", 
                          label = "Variable 2", 
                          choices = dataTst[[2]], selected = dataTst[[2]][length(dataTst[[2]])])
        updateSelectInput(session = session, 
                          inputId = "targetValueSelect", 
                          label = "Select the target value", 
                          choices = if(dataTst[[3]][length(dataTst[[2]])] == 'c') 
                            c("All Values", dataTst[[15]][[length(dataTst[[2]])]] )
                          else
                            "This is not a categorical variable!"
        )
        updateRadioButtons(session, "traTstRadio", 
                           label = "Visualize file: ", 
                           choices = c("Training File", "Test File"), 
                           selected = "Test File")
        data <<- dataTst
        dataMatrixTst <<- matrix(unlist(dataTst$data), nrow = dataTst$nVars + 1)
        dataMatrix <<- dataMatrixTst
        .updateAttributes(session, dataTst[[2]][length(dataTst[[2]])], data)
        ranges1 <<- 1:ncol(dataMatrix)
        ranges2 <<- 1:ncol(dataMatrix)
      }
    }
  })
  
  
  
  
  
  
  #Observe Visualize File
  observe({
    file <- input$traTstRadio
    #Changes variables between the training and test file
    if(file == "Training File" & fileBefore == "Tst"){
      fileBefore <<- "Tra"
      data <<- dataTra
      dataMatrix <<- dataMatrixTra
      updateSelectInput(session = session, 
                        inputId = "targetClassSelect", 
                        label = "Select the target variable", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      updateSelectInput(session = session, 
                        inputId = "targetValueSelect", 
                        label = "Select the target value", 
                        choices = if(!is.null(data) ) if(data[[3]][length(data[[2]])] == 'c') 
                          c("All Values", data[[15]][[length(data[[2]])]])
                        else
                          "This is not a categorical variable!"
      )
      updateSelectInput(session = session, 
                        inputId = "Variables1", 
                        label = "Variable 1", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      updateSelectInput(session = session, 
                        inputId = "Variables2", 
                        label = "Variable 2", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      ranges1 <<- 1:ncol(dataMatrix)
      ranges2 <<- 1:ncol(dataMatrix)
      .updateAttributes(session, data[[2]][length(data[[2]])], data)
      
    } else if(file == "Test File" & fileBefore == "Tra") {
      fileBefore <<- "Tst"
      data <<- dataTst
      dataMatrix <<- dataMatrixTst
      updateSelectInput(session = session, 
                        inputId = "targetClassSelect", 
                        label = "Select the target variable", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      updateSelectInput(session = session, 
                        inputId = "targetValueSelect", 
                        label = "Select the target value", 
                        choices = if(!is.null(data) )if(data[[3]][length(data[[2]])] == 'c') 
                          c("All Values", data[[15]][[length(data[[2]])]])
                        else
                          "This is not a categorical variable!"
      )
      updateSelectInput(session = session, 
                        inputId = "Variables1", 
                        label = "Variable 1", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      updateSelectInput(session = session, 
                        inputId = "Variables2", 
                        label = "Variable 2", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      
      
      .updateAttributes(session, data[[2]][length(data[[2]])], data)
      ranges1 <<- 1:ncol(dataMatrix)
      ranges2 <<- 1:ncol(dataMatrix)
      }
  })
  
  #Observe Target Variable
  observe({
    #Change the target variable and update necessary fields
    pos <- which(data[[2]] == input$targetClassSelect)
    if(length(pos) > 0){
      updateSelectInput(session = session, 
                        inputId = "targetValueSelect", 
                        label = "Select the target value", 
                        choices = if(data[[3]][pos] == 'c') 
                          c("All Values", data[[15]][[pos]])
                        else
                          "This is not a categorical variable!"
      )
      .updateAttributes(session, input$targetClassSelect, data)
      
      if(data$attributeTypes[pos] != 'c'){
        ranges1 <<- which(dataMatrix[pos,] >= min(dataMatrix[pos,]) & dataMatrix[pos,] <= max(dataMatrix[pos,]))
        updateNumericInput(session, "numericRangeVisualization", min = min(dataMatrix[pos,]), max = max(dataMatrix[pos,]))
      }
    }
  })
  
  
  #EXECUTE ALGORITHM

  observe({
    input$execute
  
    value <- input$execute

  
    if(input$execute <= lastValue) return(NULL)
   
    tryCatch({
    # Read parameters and check errors
    # ----------------------------------------------
    if(any(is.null(dataTra)) )
      stop("You must supply a training file and a test file. ")
    if(!is.null(dataTst)){
    if(dataTra[[1]] != dataTst[[1]])
      stop("Training and test file must be the same relation.")
    }
      
    targetValue <- isolate(input$targetValueSelect)
    if(targetValue == "This is not a categorical variable!")
      stop("No categorical variable selected as target variable.")
    
    if(targetValue == "All Values")
      targetValue <- "null"
    
    #Set target Variable.

      #dataTst <<- SDEFSR::changeTargetVariable(dataTst, which(input$targetClassSelect == dataTst[[2]]))
      #dataTra <<- SDEFSR::changeTargetVariable(dataTra, which(input$targetClassSelect == dataTra[[2]]))
    
    #Catch all necessary parameters   
    targetClass <- isolate(input$targetClassSelect)
    algorithm <- isolate(input$algorithm)
    
    nLabels <- isolate(input$nLabels)
    rulesRep <- if(isolate(input$rulesRep == "Canonical")) "can" else "dnf"
    nEvals <- isolate(input$nEval)
    popSize <- isolate(input$popSize)
    crossProb <- isolate(input$crossProb)
    mutProb <- isolate(input$mutProb)
    seed <- isolate(input$seed)
    #------------------------------------------------------
    

    
    # Preparation of specific parameters and execution of the algoritm.
    switch(algorithm,
           "SDIGA"= {
             minConf <- isolate(input$minConf)
             Obj1 <- .getObjetives(isolate(input$Obj1))
             w1 <- isolate(input$w1)
             Obj2 <- .getObjetives(isolate(input$Obj2))
             w2 <- isolate(input$w2)
             Obj3 <- .getObjetives(isolate(input$Obj3))
             w3 <- isolate(input$w3)
             lSearch = if(isolate(input$lSearch)) "yes" else "no"
            
             # Execute the algorithm
             #sink("tempFile.txt")
             ruleSet <<- SDEFSR::SDIGA(training = dataTra, 
                   test = dataTst, 
                   seed = seed, 
                   nLabels = nLabels, 
                   nEval = nEvals,
                   popLength = popSize, 
                   mutProb = mutProb,
                   RulesRep = rulesRep, 
                   Obj1 = Obj1, 
                   w1 = w1, 
                   Obj2 = Obj2, 
                   w2 = w2, 
                   Obj3 = Obj3, 
                   w3 = w3, 
                   minConf = minConf,
                   lSearch = lSearch,
                   targetVariable = isolate(input$targetClassSelect),
                   targetClass = targetValue )
             #sink(NULL)
             
            
           },
           "MESDIF" = {
             
             elitePop <- isolate(input$elitePop)
             if(elitePop > popSize) stop("Elite population must be smaller than population size")
             Obj1 <- .getObjetives(isolate(input$Obj1M))
             Obj2 <- .getObjetives(isolate(input$Obj2M))
             Obj3 <- .getObjetives(isolate(input$Obj3M))
             Obj4 <- .getObjetives(isolate(input$Obj4M))
             crossProb <- isolate(input$crossProbM)
             
            
             # Execute the algorithm
             #sink("tempFile.txt")
             ruleSet <<- SDEFSR::MESDIF(training = dataTra,
                    test = dataTst,
                    seed = seed,
                    nLabels = nLabels,
                    nEval = nEvals,
                    popLength = popSize,
                    eliteLength = elitePop,
                    crossProb = crossProb,
                    mutProb = mutProb,
                    RulesRep = rulesRep,
                    Obj1 = Obj1,
                    Obj2 = Obj2,
                    Obj3 = Obj3,
                    Obj4 = Obj4,
                    targetVariable = isolate(input$targetClassSelect),
                    targetClass = targetValue)
             #sink(NULL)
             
           },
           "NMEEF-SD" = {
             minCnf <- isolate(input$minConf)
             Obj1 <- .getObjetives(isolate(input$Obj1N))
             Obj2 <- .getObjetives(isolate(input$Obj2N))
             Obj3 <- .getObjetives(isolate(input$Obj3N))
             strictDominance <- if(isolate(input$strictDominance)) "yes" else "no"
             reInit <- if(isolate(input$reInitPob)) "yes" else "no"
             porcCob <- isolate(input$porcCob)
             
             #Execute the algorithm
             ruleSet <<- SDEFSR::NMEEF_SD(training = dataTra,
                      test = dataTst,
                      seed = seed,
                      nLabels = nLabels,
                      nEval = nEvals,
                      popLength = popSize,
                      mutProb = mutProb,
                      crossProb = crossProb,
                      Obj1 = Obj1,
                      Obj2 = Obj2,
                      Obj3 = Obj3,
                      minCnf = minCnf,
                      reInitCoverage = reInit,
                      porcCob = porcCob,
                      StrictDominance = strictDominance,
                      targetVariable = isolate(input$targetClassSelect),
                      targetClass = targetValue)
           },
           "FuGePSD" = {
             t_norm <- isolate(input$tnorm)
             ruleWeight <- isolate(input$ruleWeight)
             frm <- isolate(input$frm)
             insProb <- isolate(input$insProb)
             dropProb <- isolate(input$dropProb)
             tSize <- isolate(input$tournamentSize)
             gfw <- c(isolate(input$gfw1), isolate(input$gfw2), isolate(input$gfw3), isolate(input$gfw4) )
             allClass <- isolate(input$allClass)
             
             ruleSet <<- SDEFSR::FUGEPSD(paramFile = NULL,
                          training = dataTra,
                          test = dataTst,
                          seed = seed,
                          t_norm = t_norm,
                          ruleWeight = ruleWeight,
                          frm = frm,
                          numGenerations = nEvals,
                          numberOfInitialRules = popSize,
                          crossProb = crossProb,
                          mutProb = mutProb,
                          insProb = insProb,
                          dropProb = dropProb,
                          tournamentSize = tSize,
                          globalFitnessWeights = gfw,
                          ALL_CLASS = allClass,
                          targetVariable = isolate(input$targetClassSelect))
           }
    )
    
    },
    error = function(e){
      message(as.character(e), file = "rulesFile.txt")
      message(as.character(e), file = "optionsFile.txt")
      message(as.character(e), file = "testQualityMeasures.txt")
      return(NULL)
    }
    )

    #After execution of the algorithm, put "Rules Generated" tab as selected.
    if (input$execute > 0){
      updateTabsetPanel(session = session, inputId = "tabSet", selected = "Rules Generated")
    }
    lastValue <<- value
  })

  
  #---- Observe the algorithm choosed to change values -----
  observe({
    algo <- input$algorithm
    if(algo == "FuGePSD"){
      updateNumericInput(session, "nEval", label = "Number of generations", value = 300, min = 1, max = Inf, step = 1)
      updateSelectInput(session, "rulesRep", "Type of rules: ", choices = c("Canonical"))
    } else if(algo %in% c("SDIGA", "MESDIF")){
      updateNumericInput(session, "nEval", label = "Number of evaluations", value = 10000, min = 1, max = Inf, step = 1)
      updateSelectInput(session, "rulesRep", "Type of rules: ", choices = c("Canonical", "DNF (Disyuntive Normal Form)"))
    } else if (algo == "NMEEF-SD"){
      updateNumericInput(session, "nEval", label = "Number of evaluations", value = 10000, min = 1, max = Inf, step = 1)
      updateSelectInput(session, "rulesRep", "Type of rules: ", choices = c("Canonical"))
    }
  })
  
  
  
  
  # ------------ DATA TABLE OF RULES OBTAINED -------------
  
  output$results <- renderDataTable({
    
    input$execute
    #Gets a matrix with number of the rule and the string that represents that rule by rows.
    dataMatrix <- t(sapply(1:length(ruleSet), function(x,b) c(x, b[[x]]$rule), ruleSet))
    
    #Return as data frame
    data.frame(Rule_Number = dataMatrix[,1], Value = dataMatrix[,2])
    
}, escape = FALSE, options = list(pageLength = 10)) #By default, shows the first 10 rules.
  
  
  
  # ------- FOR THE EXECUTION INFO TAB --------------------------
  output$execInfo <- renderUI({
    input$execute
    if(file.exists("optionsFile.txt")){
      #get and process results
      contents <- readChar("optionsFile.txt", file.info("optionsFile.txt")$size)
      contents <- gsub(pattern = "\n", replacement = "<br/>", x = contents, fixed = TRUE)
      file.remove("optionsFile.txt")
      
      #Show results as html
      strong( HTML(contents), style = "font-family: 'consolas'" )
         
    }
  })
  
  # ------- FOR THE QUALITY MEASURES TAB  --------------------------
  output$measures <- renderDataTable({
    input$execute
    if(input$execute > 0){
      #Generate a matrix for each measure of the ruleSet by rows
    dataMatrix <- t(sapply(ruleSet, function(x) as.numeric(x$qualityMeasures)))
    colnames(dataMatrix) <- names(ruleSet[[1]]$qualityMeasures)
    
    #Add means on the last row and return
    #dataMatrix <- rbind(dataMatrix, colMeans(dataMatrix))
    
    dataMatrix <- cbind(Rule_Number = c(1:length(ruleSet)), dataMatrix)
    #colnames(dataMatrix) <- c("Rule_Number", "Num_Variables", "Coverage", "Unusualness", "Significance", "Fuzzy_Support", "Fuzzy_Confidence", "Crisp_Confidence", "TPR","FPR")
    #rownames(dataMatrix) <- c(1:length(ruleSet), "MEAN: ")
    dataMatrix <- as.data.frame(dataMatrix)
    
    dataMatrix
    }
  } )
  
  # UI FOR PLOTTING QUALITY MEASURES
  output$plotResultUI <- renderUI({
    #the button shows the rules plot or hides it (this only create the space for the plot, it does not generate it)
    if(input$displayQMGraph %% 2 == 1 & isolate(input$execute) > 0){ 
      plotOutput("rulesPlot")
    } else {
      NULL
    }
  })
  
  
#PLOT OF THE QUALITY MEASURES 
#Here the plot is generated
  output$rulesPlot <- renderPlot({
    print(plot(ruleSet))
  })
  
  
})

.getObjetives <- function(obj){
  switch(obj,
         "null" = {"null"},
          "Crisp Support" = {"CSUP"},
         "Fuzzy Support" = {"FSUP"},
         "Crisp Confidence" = {"CCNF"},
         "Fuzzy Confidence" = {"FCNF"},
         "Coverage" = {"COVE"},
         "Significance" = {"SIGN"},
         "Unusualness" = {"UNUS"}
         )
}


.updateAttributes <- function(session, attribute, data){
  if(attribute == "NA" || length(attribute) == 0 || is.null(attribute))
    return(NULL)
  
    if( data[[3]][which(data[[2]] == attribute)] == 'c' )
      updateCheckboxGroupInput(session, inputId = "classNames", label = "Select attributes", choices = data[[15]][[which(data[[2]] == attribute)]], selected = data[[15]][[which(data[[2]] == attribute)]], inline = T)
    else
      updateCheckboxGroupInput(session, "classNames", label = "Select attributes", choices = NA, inline = T)
  
}

 