heatmapJI_WINDOW <- function(){
  
  initializeDialog(title = gettextRcmdr("Heatmap of Jaccard Index"),use.tabs=TRUE,tabs=c("tab1","tab2")) 
  
  AllResults <- .makeSuperbiclustResultList()
  

  onOK <- function(){}
  
  onHeatmap1 <- function(){
    sel <- as.integer(tkcurselection(resultBox))+1
    if(length(AllResults)==0){
      justDoIt(paste0("warning('No available results',call.=FALSE)"))
    }
    else if(length(sel)==0){
      justDoIt(paste0("warning('No result selected',call.=FALSE)"))
    }
    else{
      
      SelResult <- AllResults[sel]
      bicres1 <- .tobiclust_transf(SelResult)
      
      
      # Do Data Check
      eval(parse(text=paste0("temp.correct <- .correctdataforresult(",bicres1,")")))
      
      if(temp.correct){
        
        command <- paste0("JI_matrix <- CompareResultJI(BCresult1=",bicres1,",type='",tclvalue(type1_vars),"')")
        doItAndPrint(command)
        
      }
      
    }
  }
  
  onHeatmap2 <- function(){
    sel <- as.integer(tkcurselection(result2Box))+1
    if(length(AllResults)==0){
      justDoIt(paste0("warning('No available results',call.=FALSE)"))
    }
    else if(length(sel)==0){
      justDoIt(paste0("warning('No result selected',call.=FALSE)"))
    }
    else if(length(sel)==1){
      justDoIt(paste0("warning('Please select 2 results',call.=FALSE)"))
    }
    else if(length(sel)>2){
      justDoIt(paste0("warning('Please select no more than 2 results',call.=FALSE)"))
    }
    else{
      
      sel.result1 <- AllResults[sel[1]]
      sel.result2 <- AllResults[sel[2]]
      
      bicres1 <- .tobiclust_transf(sel.result1)
      bicres2 <- .tobiclust_transf(sel.result2)
      
      # Correct data check
      eval(parse(text=paste0("temp.correct1 <- .correctdataforresult(",sel.result1,")")))
      eval(parse(text=paste0("temp.correct2 <- .correctdataforresult(",sel.result2,")")))
      
      if(temp.correct1 & temp.correct2){
        
        command <- paste0("JI_matrix <- CompareResultJI(BCresult1=",bicres1,",BCresult2=",bicres2,",type='",tclvalue(type2_vars),"')")
        doItAndPrint(command)
      }
      
    }
  }
  
  onCancel <- function() {
    if (GrabFocus()) 
      tkgrab.release(top)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  
  onHelp <- function(){
    print(help('CompareResultJI'))
  }
  
  #############
  ### TAB 1 ###
  #############
  
  tab1Frame <- tkframe(tab1)
  
  heat1Frame <- tkframe(tab1Frame)
  resultFrame <- tkframe(heat1Frame)

  resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
                          selectmode="single", background="white")
  for (result in AllResults) tkinsert(resultBox, "end", result)
  resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
  tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
  if(length(AllResults)!=0){tkselection.set(resultBox,0)}
  
  tkgrid(labelRcmdr(heat1Frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results:")),sticky="nw")
  tkgrid(resultBox,resultScroll) #,sticky="ns"
  tkgrid.configure(resultScroll,sticky="ns")
  
  
  radioButtons(heat1Frame,name="radioType1",buttons=c("row","col","both"),values=c("row","col","both"),labels=gettextRcmdr(c("Row","Column","Row & Column")),initialValue="both",title="Jaccard Index Dimension")
  type1_vars <- radioType1Variable
  
  
  tkgrid(resultFrame,radioType1Frame,sticky="nw",padx="6",pady="6")
  tkgrid.configure(radioType1Frame,padx="16")
  tkgrid(heat1Frame,stick="nw")
  
  

  
  heat1Button <- buttonRcmdr(tab1Frame,command=onHeatmap1,text=gettextRcmdr("JI Heatmap"),foreground="darkgreen",default="active",width="12",borderwidth=3)
  
  
  tkgrid(heat1Button,sticky="se",padx="6",pady="10")
  
  tkgrid(tab1Frame)
  
  #############
  ### TAB 2 ###
  #############
  
  tab2Frame <- tkframe(tab2)
  heat2Frame <- tkframe(tab2Frame)
  
  result2Frame <- tkframe(heat2Frame)



  result2Box <- tklistbox( result2Frame , height=5, exportselection="FALSE",
                          selectmode="multiple", background="white")
  for (result in AllResults) tkinsert(result2Box, "end", result)
  result2Scroll <- ttkscrollbar(result2Frame,command=function(...) tkyview(result2Box, ...))
  tkconfigure(result2Box, yscrollcommand=function(...) tkset(result2Scroll, ...))
  if(length(AllResults)!=0){tkselection.set(result2Box,0)}

  tkgrid(labelRcmdr(tab2Frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results (select2):")),sticky="nw")
  tkgrid(result2Box,result2Scroll,sticky="nw") #,sticky="ns"
  tkgrid.configure(result2Scroll,sticky="ns")


  radioButtons(heat2Frame,name="radioType2",buttons=c("row","col","both"),values=c("row","col","both"),labels=gettextRcmdr(c("Row","Column","Row & Column")),initialValue="both",title="Jaccard Index Dimension")
  type2_vars <- radioType2Variable

  tkgrid(result2Frame,radioType2Frame,sticky="nw",padx="6",pady="6")
  tkgrid.configure(radioType2Frame,padx="16")
  tkgrid(heat2Frame,stick="nw")
  

  heat2Button <- buttonRcmdr(tab2Frame,command=onHeatmap2,text=gettextRcmdr("JI Heatmap"),foreground="darkgreen",default="active",width="12",borderwidth=3)

  tkgrid(heat2Button,sticky="se",padx="6",pady="10")

  
  
  tkgrid(tab2Frame,sticky="nw")
  
  
  ####################
  ### GRID BUTTONS ###
  ####################
  
  buttonsFrame <- tkframe(top)
  exitButton <- buttonRcmdr(buttonsFrame,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
  helpButton <- buttonRcmdr(buttonsFrame,command=onHelp,text=gettextRcmdr("Help"),foreground="darkgreen",width="8",borderwidth=3)
  
  tkgrid(exitButton,helpButton,sticky="es")
  
  
  
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE,onOK=onOK,tabs=c("tab1","tab2"),tab.names=c("Single Result","Two Results"),preventGrabFocus=TRUE)
  
}